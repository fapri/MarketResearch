


# From Texas tutorial

# https://mgimond.github.io/Spatial/interpolation-in-r.html



## Install the required package with:
## install.packages("RSocrata")

library(RSocrata)
library(tmaptools)
library(tmap)
library(rgdal)
library(spatstat)  # Used for the dirichlet tessellation function
library(maptools)  # Used for conversion from SPDF to ppp
library(raster)    # Used to clip out thiessen polygons
library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function
library(tigris)


# MODataDf <- read.socrata(
#   "https://data.mo.gov/resource/76ba-6hev.json",
#   app_token = "w22SYmBTM13JSQ2koGlQDAJAh",
#   email     = "enscully@gmail.com",
#   password  = ""
# )

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

# Get shape data from data.mo.gov
Missouri <- readOGR(dsn = "MissouriCountyBoundariesMap/geo_export_6b1e41b0-3ffc-4779-b905-b6c2702c930a.shp")

# Restirct the basis data set to the state of Missouri
basisSP@bbox <- Missouri@bbox

# Transform to another CRS???
Missouri = spTransform(Missouri, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))


# Plot with county centers corresponding to basis value
tm_shape(Missouri) + tm_polygons() +
  tm_shape(basisSP) +
  tm_dots(col = "basis2019...3.", palette = "RdBu", midpoint = -0.20,
          size = 0.7) +
  tm_legend(legend.outside = TRUE)


# # Plot with extra information
# tm_shape(Missouri) + tm_polygons() +
#   tm_shape(basisSP) +
#   tm_dots(col = "basis2019...3.", palette = "RdBu",
#     midpoint = -0.20, title = "Sampled precipitation \n(in inches)",
#     size = 0.7) +
#   tm_text("basis2019...3.", just = "left",
#           xmod = .5, size = 0.7) +
#   tm_legend(legend.outside = TRUE)

# False
sp::is.projected(basisSP)

basisSP = spTransform(basisSP, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))



# Must be True to continure
sp::is.projected(basisSP)

# Create a tessellated surface
th  <-  as(dirichlet(as.ppp(basisSP)), "SpatialPolygons")

# The dirichlet function does not carry over projection information
# requiring that this information be added manually
proj4string(th) <- proj4string(basisSP)

# The tessellated surface does not store attribute information
# from the point data layer. We'll use the over() function (from the sp
# package) to join the point attributes to the tesselated surface via
# a spatial join. The over() function creates a dataframe that will need to
# be added to the `th` object thus creating a SpatialPolygonsDataFrame object
th.z     <- over(th, basisSP, fn = mean)
th.spdf  <-  SpatialPolygonsDataFrame(th, th.z)

# Finally, we'll clip the tessellated  surface to the Texas boundaries
th.clp   <- raster::intersect(Missouri, th.spdf)

# Map the data
tm_shape(th.clp) + 
  tm_polygons(col = "basis2019...3.", palette = "RdBu", midpoint = -0.20,
              title = "Predicted precipitation \n(in inches)") +
  tm_legend(legend.outside = TRUE)





# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(basisSP, "regular", n = 50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(grd) <- proj4string(basisSP)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
basisSP.idw <- gstat::idw(`basis2019...3.` ~ 1, basisSP, newdata = grd, idp = 2.0)

# Convert to raster object then clip to Texas
r       <- raster(basisSP.idw)
r.m     <- mask(r, Missouri)

# Plot final result
tm_shape(r.m) + 
  tm_raster(n = 10,palette = "RdBu", midpoint = -0.20,
            title = "Predicted precipitation \n(in inches)") + 
  tm_shape(basisSP) + tm_dots(size = 0.1) +
  tm_legend(legend.outside = TRUE)






