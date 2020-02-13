# IDW Test 
# California temperatures

# https://rspatial.org/raster/analysis/4-interpolation.html


# interpolate = estimate for unsampled locations




library(rspatial)
d <- sp_data('precipitation')
head(d)

d$prec <- rowSums(d[, c(6:17)])
plot(sort(d$prec), ylab='Annual precipitation (mm)', las=1, xlab='Stations')

library(sp)
dsp <- SpatialPoints(d[,4:3], proj4string=CRS("+proj=longlat +datum=NAD83"))
dsp <- SpatialPointsDataFrame(dsp, d)
CA <- sp_data("counties")
# define groups for mapping
cuts <- c(0,200,300,500,1000,3000)
# set up a palette of interpolated colors
blues <- colorRampPalette(c('yellow', 'orange', 'blue', 'dark blue'))
pols <- list("sp.polygons", CA, fill = "lightgray")
spplot(dsp, 'prec', cuts=cuts, col.regions=blues(5), sp.layout=pols, pch=20, cex=2)

TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0")
library(rgdal)
dta <- spTransform(dsp, TA)
cata <- spTransform(CA, TA)

RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}

null <- RMSE(mean(dsp$prec), dsp$prec)
null # 435.3217

## Proximity polygons can be used to interpolate categorical variables

library(dismo)
library(deldir)
v <- voronoi(dta)
plot(v)

ca <- aggregate(cata)
vca <- intersect(v, ca)
spplot(vca, 'prec', col.regions = rev(get_col_regions()))

## Rasterize the results
## Rasterize = convert (an image stored as an outline) into pixels that can be displayed on a screen or printed.

r <- raster(cata, res = 10000)
vr <- rasterize(vca, r, 'prec')
plot(vr)

kf <- kfold(nrow(dta))
rmse <- rep(NA, 5)
for (k in 1:5) {
  test <- dta[kf == k, ]
  train <- dta[kf != k, ]
  v <- voronoi(train)
  p <- extract(v, test)
  rmse[k] <- RMSE(test$prec, p$prec)
}
rmse
mean(rmse)
1 - (mean(rmse) / null)

## Nearest neighbour interpolation considering multiple (5) neighbours

# library(gstat)
# gs <- gstat(formula = prec~1, locations = dta, nmax = 5, set = list(idp = 0))
# nn <- interpolate(r, gs)
# nnmsk <- mask(nn, vr)
# plot(nnmsk)
# 
# rmsenn = 0
# 
# for (k in 1:5) {
#   test <- dta[kf == k, ]
#   train <- dta[kf != k, ]
#   gscv <- gstat(formula=prec~1, locations=train, nmax=5, set=list(idp = 0))
#   p <- predict(gscv, test)$var1.pred
#   rmsenn[k] <- RMSE(test$prec, p)
# }
# rmsenn
# mean(rmsenn)
# 1 - (mean(rmsenn) / null)

## "inverse distance weighted" interpolation
## IDW = points that are further away get less weight in predicting a value a location.

library(gstat)
gs <- gstat(formula=prec~1, locations=dta)
idw <- interpolate(r, gs)
idwr <- mask(idw, vr)
plot(idwr)

rmse <- rep(NA, 5)
for (k in 1:5) {
  test <- dta[kf == k, ]
  train <- dta[kf != k, ]
  gs <- gstat(formula=prec~1, locations=train)
  p <- predict(gs, test)
  rmse[k] <- RMSE(test$prec, p$var1.pred)
}
rmse
mean(rmse)
1 - (mean(rmse) / null)

gs2 <- gstat(formula=prec~1, locations=dta, nmax=1, set=list(idp=1))















