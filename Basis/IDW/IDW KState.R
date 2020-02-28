# Most successful IDW

# Adapted from
# https://rspatial.org/raster/analysis/4-interpolation.html
# https://mgimond.github.io/Spatial/interpolation-in-r.html


library(rspatial)
library(sp)
library(rgdal)
library(dismo)
library(deldir)
library(gstat)
library(tmap)
library(tmaptools)



# Load data
kLocBasisMergeCorn = readRDS("Basis/kLocBasisMergeCorn.rds")
basis2019 = kLocBasisMergeCorn[ , c("Latitude", "Longitude", "avgBasis2019", "County")]

# Remove NA values
basis2019 <- basis2019[!is.na(basis2019[, c("avgBasis2019")]), ]

# Get longitude and laitude (must be in that order)
xy = basis2019[ , c(2,1)]

# convert basis data to spatial points data frame
basisSP = SpatialPointsDataFrame(coords = xy, data = data.frame("avgBasis2019" = basis2019[,3], "county" = basis2019[,4]),
                             proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))



MO <- readOGR(dsn = "Basis/MissouriCountyBoundariesMap/geo_export_6b1e41b0-3ffc-4779-b905-b6c2702c930a.shp")
# set up a palette of interpolated colors
blues <- colorRampPalette(c('yellow', 'orange', 'blue', 'dark blue'))
pols <- list("sp.polygons", MO, fill = "lightgray")
spplot(basisSP, 'avgBasis2019', col.regions = blues(5), sp.layout = pols, pch = 20, cex = 2)

TA <- CRS("+proj=tmerc +lat_0=35.83333333333334 +lon_0=-92.5 +k=0.999933333 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs ")

basisSP <- spTransform(basisSP, TA)
Missouri <- spTransform(MO, TA)

RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm = TRUE))
}

null <- RMSE(mean(basisSP$avgBasis2019), basisSP$avgBasis2019)
null

## Proximity polygons can be used to interpolate categorical variables
v <- voronoi(basisSP)
plot(v)

moAgg <- aggregate(Missouri)
vca <- intersect(v, moAgg)
spplot(vca, 'avgBasis2019', col.regions = rev(get_col_regions()))

## Rasterize the results
## Rasterize = convert (an image stored as an outline) into pixels that can be displayed on a screen or printed.

r <- raster(Missouri, res = 1000)
vr <- rasterize(vca, r, 'avgBasis2019')
plot(vr)



set.seed(5132015)
kf <- kfold(nrow(basisSP))
rmse <- rep(NA, 5)
for (k in 1:5) {
  test <- basisSP[kf == k, ]
  train <- basisSP[kf != k, ]
  v <- voronoi(train)
  p <- extract(v, test)
  rmse[k] <- RMSE(test$avgBasis2019, p$avgBasis2019)
}

rmse
mean(rmse)
# R^2
1 - (mean(rmse) / null)







## Nearest neighbour interpolation considering multiple (5) neighbours

gs <- gstat(formula = avgBasis2019~1, locations = basisSP, nmax = 5, set = list(idp = 0))
nn <- interpolate(r, gs)
nnmsk <- mask(nn, vr)
plot(nnmsk)

rmsenn = 0

for (k in 1:5) {
  test <- basisSP[kf == k, ]
  train <- basisSP[kf != k, ]
  gscv <- gstat(formula = avgBasis2019 ~ 1, locations = train, nmax = 5, set = list(idp = 0))
  p <- predict(gscv, test)$var1.pred
  rmsenn[k] <- RMSE(test$avgBasis2019, p)
}
rmsenn
mean(rmsenn)
1 - (mean(rmsenn) / null)

## "inverse distance weighted" interpolation
## IDW = points that are further away get less weight in predicting a value a location.

gs <- gstat(formula = avgBasis2019 ~ 1, locations = basisSP)
idw <- interpolate(r, gs)
idwr <- mask(idw, vr)
plot(idwr)

# tmap_mode("view")
# tmap_mode("plot")

tm_shape(idwr) + 
  tm_raster(n = 20, palette = "RdBu", midpoint = -0.20,
            title = "", legend.reverse = TRUE) + 
  tm_shape(basisSP) + tm_dots(size = 0.1) +
  tm_legend(legend.outside = TRUE) + 
  tm_layout(title = "Basis (cents)", main.title = "Missouri Basis")
  
rmse <- rep(NA, 5)
for (k in 1:5) {
  test <- basisSP[kf == k, ]
  train <- basisSP[kf != k, ]
  gs <- gstat(formula = avgBasis2019 ~ 1, locations = train)
  p <- predict(gs, test)
  rmse[k] <- RMSE(test$avgBasis2019, p$var1.pred)
}
rmse
mean(rmse)
1 - (mean(rmse) / null)

gs2 <- gstat(formula = avgBasis2019 ~ 1, locations = basisSP, nmax = 1, set = list(idp = 1))







# library(readxl)
# library(osrm)
# County_Centers <- read_excel("Basis/County Centers.xlsx")
# 
# 
# distCom <- osrmTable(loc = County_Centers[1:50, c("County","Longitude","Latitude")])
# distCom$duration[1:5,1:5]







# library(jsonlite)
# 
# o <- data.frame("lat" = 40.19067, "lng" = -92.60359)
# d <- data.frame("lat" = 39.98886, "lng" = -94.80355)
# (url <- paste0("http://router.project-osrm.org/route/v1/driving/", 
#                o$lng,",",o$lat,";",d$lng,",",d$lat,"?overview=full"))
# 
# system.time({
#   route <- fromJSON(url)
# })
# 
# route$routes$distance
# # 251170.5
# route$routes$distance/1609
# # 156.1035












