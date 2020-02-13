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
kLocBasisMergeCorn = readRDS("MarketResearch/Basis/kLocBasisMergeCorn.rds")
basis2019 = kLocBasisMergeCorn[ , c("Latitude", "Longitude", "avgBasis2019", "County")]

# Remove NA values
basis2019 <- basis2019[!is.na(basis2019[, c("avgBasis2019")]), ]

# Get longitude and laitude (must be in that order)
xy = basis2019[ , c(2,1)]

# convert basis data to spatial points data frame
basisSP = SpatialPointsDataFrame(coords = xy, data = data.frame(basis2019[,3]),
                             proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))




MO <- readOGR(dsn = "MissouriCountyBoundariesMap/geo_export_6b1e41b0-3ffc-4779-b905-b6c2702c930a.shp")
# set up a palette of interpolated colors
blues <- colorRampPalette(c('yellow', 'orange', 'blue', 'dark blue'))
pols <- list("sp.polygons", MO, fill = "lightgray")
spplot(basisSP, 'basis2019...3.', col.regions=blues(5), sp.layout=pols, pch=20, cex=2)

TA <- CRS("+proj=tmerc +lat_0=35.83333333333334 +lon_0=-92.5 +k=0.999933333 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs ")

dta <- spTransform(basisSP, TA)
cata <- spTransform(MO, TA)

RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}

null <- RMSE(mean(basisSP$basis2019...3.), basisSP$basis2019...3.)
null # 435.3217

## Proximity polygons can be used to interpolate categorical variables


v <- voronoi(dta)
plot(v)

ca <- aggregate(cata)
vca <- intersect(v, ca)
spplot(vca, 'basis2019...3.', col.regions = rev(get_col_regions()))

## Rasterize the results
## Rasterize = convert (an image stored as an outline) into pixels that can be displayed on a screen or printed.

r <- raster(cata, res = 1000)
vr <- rasterize(vca, r, 'basis2019...3.')
plot(vr)

kf <- kfold(nrow(dta))
rmse <- rep(NA, 5)
for (k in 1:5) {
  test <- dta[kf == k, ]
  train <- dta[kf != k, ]
  v <- voronoi(train)
  p <- extract(v, test)
  rmse[k] <- RMSE(test$basis2019...3., p$basis2019...3.)
}
rmse
mean(rmse)
1 - (mean(rmse) / null)

## Nearest neighbour interpolation considering multiple (5) neighbours

gs <- gstat(formula = basis2019...3.~1, locations = dta, nmax = 5, set = list(idp = 0))
nn <- interpolate(r, gs)
nnmsk <- mask(nn, vr)
plot(nnmsk)

rmsenn = 0

for (k in 1:5) {
  test <- dta[kf == k, ]
  train <- dta[kf != k, ]
  gscv <- gstat(formula=basis2019...3.~1, locations=train, nmax=5, set=list(idp = 0))
  p <- predict(gscv, test)$var1.pred
  rmsenn[k] <- RMSE(test$basis2019...3., p)
}
rmsenn
mean(rmsenn)
1 - (mean(rmsenn) / null)

## "inverse distance weighted" interpolation
## IDW = points that are further away get less weight in predicting a value a location.


gs <- gstat(formula = basis2019...3. ~ 1, locations = dta)
idw <- interpolate(r, gs)
idwr <- mask(idw, vr)
plot(idwr)



tm_shape(idwr) + 
  tm_raster(n = 20,palette = "RdBu", midpoint = -0.20,
            title = "Basis by County (cents)") + 
  tm_shape(dta) + tm_dots(size = 0.1) +
  tm_legend(legend.outside = TRUE)




rmse <- rep(NA, 5)
for (k in 1:5) {
  test <- dta[kf == k, ]
  train <- dta[kf != k, ]
  gs <- gstat(formula = basis2019...3. ~ 1, locations = train)
  p <- predict(gs, test)
  rmse[k] <- RMSE(test$basis2019...3., p$var1.pred)
}
rmse
mean(rmse)
1 - (mean(rmse) / null)

gs2 <- gstat(formula = basis2019...3. ~ 1, locations = dta, nmax = 1, set = list(idp = 1))

















