# Cleaning Eikon Data


# Load libraries
library(readr)
library(tmap)
library(tmaptools)
library(stringr)
library(rspatial)
library(sp)
library(rgdal)
library(dismo)
library(deldir)
library(gstat)
library(tmap)
library(tmaptools)

source("Basis/IDW/cleanCorn.R")

# Load data
allBasis = read_csv("Basis/refinitivData/allBasis.csv")
spotOnly = read_csv("Basis/refinitivData/spotOnly.csv")

# Remove extra columns
spotOnly = subset(spotOnly, select = -c(GEN_TEXT16, Location))

# Change column names
colnames(spotOnly) = c("instrument", "contractName", "basis", "date", "terminalName", "address", "county", "cropType", "phoneNumber")

# Initial Clean
spotOnly = cleanCorn1()

# Get zip codes
spotOnly$zipCode = str_extract(spotOnly$phoneNumber, "\\d{5}")

# Paste zip codes to address to send to geocoder
spotOnly$geoFormatAddress = paste(spotOnly$address, spotOnly$zipCode, sep = ", ")

# min lat = 35
# max lat = 41
# max long = -96
# min long = -89

allGoogle = readRDS("Basis/refinitivData/allGoogle.rds")

badGeo = which(!((allGoogle$lat >= 35 & allGoogle$lat <= 41) | (allGoogle$long >= -96 & allGoogle$long <= -89)))

finalSet = merge(spotOnly[, c("instrument", "basis", "date", "terminalName", "county", "cropType", "geoFormatAddress")],
                 allGoogle,
                 by.x = "geoFormatAddress",
                 by.y = "address")

finalSet = cleanCorn2()

# Remove NA values
finalSet = finalSet[!is.na(finalSet[, c("basis")]), ]

midPoint = median(finalSet$basis)

# reset row names
rownames(finalSet) <- NULL

# Get longitude and laitude (must be in that order)
xy = finalSet[ , c("long", "lat")]

# convert basis data to spatial points data frame
basisSP = SpatialPointsDataFrame(coords = xy, data = data.frame("basis" = finalSet[,"basis"], "City" = finalSet[,"county"], 
                                                                "Terminal" = finalSet[,"terminalName"]),
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))



MO <- readOGR(dsn = "Basis/MissouriCountyBoundariesMap/geo_export_6b1e41b0-3ffc-4779-b905-b6c2702c930a.shp")

# min lat = 35
# max lat = 41
# max long = -96
# min long = -89

# USA Contiguous Albers Equal Area Conic
USAC <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

basisSP <- spTransform(basisSP, USAC)
Missouri <- spTransform(MO, USAC)

RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm = TRUE))
}

null <- RMSE(mean(basisSP$basis), basisSP$basis)
null

## Proximity polygons can be used to interpolate categorical variables
v <- voronoi(basisSP)
# plot(v)

moAgg <- aggregate(Missouri)
vca <- intersect(v, moAgg)
# spplot(vca, 'basis', col.regions = rev(get_col_regions()))

## Rasterize the results
## Rasterize = convert (an image stored as an outline) into pixels that can be displayed on a screen or printed.

r <- raster(Missouri, res = 1000)
vr <- rasterize(vca, r, 'basis')
# plot(vr)

set.seed(54835)
kf <- kfold(nrow(basisSP))
rmse <- rep(NA, 5)
for (k in 1:5) {
  test <- basisSP[kf == k, ]
  train <- basisSP[kf != k, ]
  v <- voronoi(train)
  p <- extract(v, test)
  rmse[k] <- RMSE(test$basis, p$basis)
}

rmse
mean(rmse)
# R^2
1 - (mean(rmse) / null)


## Nearest neighbour interpolation considering multiple (5) neighbours

gs <- gstat(formula = basis~1, locations = basisSP, nmax = 5, set = list(idp = 0))
nn <- interpolate(r, gs)
nnmsk <- mask(nn, vr)
# plot(nnmsk)

rmsenn = 0

for (k in 1:5) {
  test <- basisSP[kf == k, ]
  train <- basisSP[kf != k, ]
  gscv <- gstat(formula = basis ~ 1, locations = train, nmax = 5, set = list(idp = 0))
  p <- predict(gscv, test)$var1.pred
  rmsenn[k] <- RMSE(test$basis, p)
}
rmsenn
mean(rmsenn)
1 - (mean(rmsenn) / null)

## "inverse distance weighted" interpolation
## IDW = points that are further away get less weight in predicting a value a location.

gs <- gstat(formula = basis ~ 1, locations = basisSP)
idw <- interpolate(r, gs, idp = 1)
idwr <- mask(idw, vr)
# plot(idwr)

# tmap_mode("view")
# tmap_mode("plot")
# tmaptools::palette_explorer() 

tm_shape(idwr) + 
  tm_raster(n = 15, palette = "RdYlBu", contrast = c(0.4, 1), midpoint = midPoint,
            title = "", legend.reverse = TRUE) + 
  tm_shape(basisSP) + tm_dots(size = 0.1) +
  tm_legend(legend.outside = TRUE) + 
  tm_layout(title = "Basis (cents)", main.title = "Missouri Basis")

rmse <- rep(NA, 5)
for (k in 1:5) {
  test <- basisSP[kf == k, ]
  train <- basisSP[kf != k, ]
  gs <- gstat(formula = basis ~ 1, locations = train)
  p <- predict(gs, test)
  rmse[k] <- RMSE(test$basis, p$var1.pred)
}
rmse
mean(rmse)
1 - (mean(rmse) / null)

gs2 <- gstat(formula = basis ~ 1, locations = basisSP, nmax = 1, set = list(idp = 1))






# f1 <- function(x, test, train) {
#   nmx <- x[1]
#   idp <- x[2]
#   if (nmx < 1) return(Inf)
#   if (idp < .001) return(Inf)
#   m <- gstat(formula=basis~1, locations=train, nmax=nmx, set=list(idp=idp))
#   p <- predict(m, newdata=test, debug.level=0)$var1.pred
#   RMSE(test$basis, p)
# }
# set.seed(20150518)
# i <- sample(nrow(basisSP), 0.2 * nrow(basisSP))
# tst <- basisSP[i,]
# trn <- basisSP[-i,]
# opt <- optim(c(8, .5), f1, test=tst, train=trn)
# opt
# 
# m <- gstat(formula=basis~1, locations=basisSP, nmax=opt$par[1], set=list(idp=opt$par[2]))
# idw <- interpolate(r, m)
# ## [inverse distance weighted interpolation]
# idw <- mask(idw, Missouri)
# plot(idw)
# 
# 
# 
# tm_shape(idw) + 
#   tm_raster(n = 15, palette = "RdYlBu", contrast = c(0.2, 1), midpoint = midPoint,
#             title = "", legend.reverse = TRUE) + 
#   tm_shape(basisSP) + tm_dots(size = 0.1) +
#   tm_legend(legend.outside = TRUE) + 
#   tm_layout(title = "Basis (cents)", main.title = "Missouri Basis")








