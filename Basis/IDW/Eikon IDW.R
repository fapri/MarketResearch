# Cleaning Eikon Data


# Load libraries
library(readr)
library(tmap)
library(tmaptools)
library(stringr)

# Load data
allBasis = read_csv("Basis/refinitivData/allBasis.csv")
spotOnly = read_csv("Basis/refinitivData/spotOnly.csv", 
                    col_types = cols(X1 = col_skip()))

# Remove extra columns
spotOnly = subset(spotOnly, select = -c(GEN_TEXT16, Location))

# Cahnge column names
colnames(spotOnly) = c("instrument", "contractName", "basis", "date", "terminalName", "address", "county", "cropType", "phoneNumber")

spotOnly = spotOnly[-which(spotOnly$instrument == "CORNADMMCN-C1"),]

# Fixed row where data was offset
rowToFix = which(spotOnly$instrument == "CORNGFGBTH-C1")
spotOnly$phoneNumber[rowToFix] = '"Bethany, MO - 64424, 660-425-3014"'
spotOnly$address[rowToFix] = "31019 E 260th Ave"
spotOnly$terminalName[rowToFix] = "Gage Fertilizer and Grain"

# ALL OF THESE ADDRESSES WERE WRONG
# Novelty ADM
spotOnly$address[which(spotOnly$instrument == "CORNADMNOV-C1")] = "65031 State Hwy 15"
# Pattonsburg MFA
spotOnly$address[which(spotOnly$instrument == "CORNMFAPBG-C1")] = "204 1st St"
# Unionville MFA
spotOnly$address[which(spotOnly$instrument == "CORNMFAUNV-C1")] = "520 S 23rd St"
# Hopkins Green Plains
spotOnly$address[which(spotOnly$instrument == "CORNGPLHPN-C1")] = "200 N Railroad St"
# Harrisonville Roth Heriford
spotOnly$address[which(spotOnly$instrument == "CORNRHFHRL-C1")] = "32503 MO-2"
# Corder Ray Carroll
spotOnly$address[which(spotOnly$instrument == "CORNRCGDER-C1")] = "26194 MO-20"
# Adrian West Central Ag
spotOnly$address[which(spotOnly$instrument == "CORNWCAADR-C1")] = "438 County Rd 11002"
# Baring ADM
spotOnly$address[which(spotOnly$instrument == "CORNADMBIN-C1")] = "101 2nd St"
# Blackburn Central Missouri AgriService
spotOnly$address[which(spotOnly$instrument == "CORNCAEBCB-C1")] = "215 W Park St"
# Caruthersville Bunge
spotOnly$address[which(spotOnly$instrument == "CORNBNGCRT-C1")] = "100 Ward Ave"
# Caruthersville Consolidated Grain and Barge
spotOnly$address[which(spotOnly$instrument == "CORNCGBCRT-C1")] = "2073 Co Rd 337"
# Centerview West Central Ag
spotOnly$address[which(spotOnly$instrument == "CORNWCACNV-C1")] = "103 N Main St"
# La Plata ADM
spotOnly$address[which(spotOnly$instrument == "CORNADMLAP-C1")] = "131 E Moore St"
# Louisiana Bunge
spotOnly$address[which(spotOnly$instrument == "CORNBNGLOU-C1")] = "1035 MO-79"
# Lucerne	MB Grain
spotOnly$address[which(spotOnly$instrument == "CORNPSFLUC-C1")] = "14680 US Hwy 136"
# Malta Bend Central Missouri AgriService
spotOnly$address[which(spotOnly$instrument == "CORNCAEMBN-C1")] = "1 E Pacific St"
# Newtown Fowler Elevator
spotOnly$address[which(spotOnly$instrument == "CORNFWLNTN-C1")] = "301 E Broadway St"
# Slater Central Missouri AgriService
spotOnly$address[which(spotOnly$instrument == "CORNCAESLT-C1")] = "305 Industrial Blvd,"
# Slater Ray Carroll Grain	
spotOnly$address[which(spotOnly$instrument == "CORNRCGSLT-C1")] = "29261 North Highway 240"
# Baring Nemo Feed
spotOnly$address[which(spotOnly$instrument == "CORNNMFBIN-C1")] = "50726 State Hwy 15"
# Lancaster MFA
spotOnly$address[which(spotOnly$instrument == "CORNMFALNC-C1")] = "13975 US Hwy 63"
# Braymer Consumers
spotOnly$address[which(spotOnly$instrument == "CORNCNMBYM-C1")] = "100 Railroad street"
# Craig Grain
spotOnly$address[which(spotOnly$instrument == "CORNCGICRA-C1")] = "102 Main St"
# Concordia MFA
spotOnly$address[which(spotOnly$instrument == "CORNMFACNC-C1")] = "601 S Main St, 64020"
# Lamar MFA
spotOnly$address[which(spotOnly$instrument == "CORNMFALMR-C1")] = "1901 Hwy Kk, 64759"
# Charleston Consolidated Grain and Barge
spotOnly$address[which(spotOnly$instrument == "CORNCGBCHO-C1")] = "6720 N Hwy K, 63834"


# # Template for fixing addresses
# # 
# spotOnly$address[which(spotOnly$instrument == "-C1")] = ""




# Get zip codes
spotOnly$zipCode = str_extract(spotOnly$phoneNumber, "\\d{5}")

# Paste zip codes to address to send to geocoder
spotOnly$geoFormatAddress = paste(spotOnly$address, spotOnly$zipCode, sep = ", ")

# Get lat/lon from geocoder OSM
x = data.frame(geocode_OSM(spotOnly$geoFormatAddress))

x = x[, c("query", "lat", "lon")]

colnames(x) = c("address", "lat", "long")

# # Get lat/lon from geocoder GOOGLE
# geocode(spotOnly$geoFormatAddress)


# min lat = 35
# max lat = 41
# max long = -96
# min long = -89


noGeo = which(!spotOnly$geoFormatAddress %in% x$address)
badGeo = which(!((x$lat >= 35 & x$lat <= 41) | (x$long >= -96 & x$long <= -89)))

x = x[-badGeo, ]

# Get rows that didn't geocode
notWorking = spotOnly[c(noGeo, badGeo), ]

# Load Google geocoded data
moreMatches = read_csv("Basis/refinitivData/moreMatches.csv", 
         col_types = cols(X1 = col_skip()))


finalSet = rbind(x, moreMatches)


finalSet = merge(finalSet, 
                 spotOnly[, c("basis", "date", "terminalName", "county", "cropType", "geoFormatAddress")], 
                 by.x = "address", 
                 by.y = "geoFormatAddress")



finalSet = merge(spotOnly[, c("basis", "date", "terminalName", "county", "cropType", "geoFormatAddress")],
                 allGoogle,
                 by.x = "geoFormatAddress",
                 by.y = "address")






library(rspatial)
library(sp)
library(rgdal)
library(dismo)
library(deldir)
library(gstat)
library(tmap)
library(tmaptools)



# # Load data
# kLocBasisMergeCorn = readRDS("Basis/kLocBasisMergeCorn.rds")
# finalSet = kLocBasisMergeCorn[ , c("Latitude", "Longitude", "avgfinalSet", "County")]

# Remove NA values
finalSet = finalSet[!is.na(finalSet[, c("basis")]), ]

midPoint = median(finalSet$basis)

# reset row names
rownames(finalSet) <- NULL

# Get longitude and laitude (must be in that order)
xy = finalSet[ , c("long", "lat")]

# convert basis data to spatial points data frame
basisSP = SpatialPointsDataFrame(coords = xy, data = data.frame("basis" = finalSet[,"basis"], "county" = finalSet[,"county"]),
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))



MO <- readOGR(dsn = "Basis/MissouriCountyBoundariesMap/geo_export_6b1e41b0-3ffc-4779-b905-b6c2702c930a.shp")
# set up a palette of interpolated colors
blues <- colorRampPalette(c('yellow', 'orange', 'blue', 'dark blue'))
pols <- list("sp.polygons", MO, fill = "lightgray")
spplot(basisSP, 'basis', col.regions = blues(5), sp.layout = pols, pch = 20, cex = 2)


# min lat = 35
# max lat = 41
# max long = -96
# min long = -89

TA <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

basisSP <- spTransform(basisSP, TA)
Missouri <- spTransform(MO, TA)

RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm = TRUE))
}

null <- RMSE(mean(basisSP$basis), basisSP$basis)
null

## Proximity polygons can be used to interpolate categorical variables
v <- voronoi(basisSP)
plot(v)

moAgg <- aggregate(Missouri)
vca <- intersect(v, moAgg)
spplot(vca, 'basis', col.regions = rev(get_col_regions()))

## Rasterize the results
## Rasterize = convert (an image stored as an outline) into pixels that can be displayed on a screen or printed.

r <- raster(Missouri, res = 1000)
vr <- rasterize(vca, r, 'basis')
plot(vr)



set.seed(5132015)
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
plot(nnmsk)

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
idw <- interpolate(r, gs)
idwr <- mask(idw, vr)
plot(idwr)

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