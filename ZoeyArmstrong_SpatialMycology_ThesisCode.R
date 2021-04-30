library("maptools")
library("raster")
library("rgdal")
library("maps")
library("biomod2")
library("devtools")
library('rMyCoPortal')
library("sf")
library("rgdal")
library ("tidyverse")
library("sp")
library("devtools")
library("ggplot2")
library("rnaturalearth")
library("rnaturalearthdata")
library("GSIF")

####Load in Data####

env.var250 <- stackOpen("C:/Users/Zoey/Desktop/ThesisProjectData/FirstTest.tif")
effort.var <- raster("C:/Users/Zoey/Desktop/ThesisProjectData/EffortVariable.tif")
Herbarium <- st_read("C:/Users/Zoey/Desktop/ThesisProjectData/Observations/Herbarium/Herbarium_Points_Projected.shp")
iNat <- st_read("C:/Users/Zoey/Desktop/ThesisProjectData/Observations/iNatObservations/iNatObservations_Clip_Projected.shp")


# r is a variable to create a whole number from 0-100 based on the effort variable. Higher numbers are more effort observations.
r <- calc(effort.var, fun=function(x){((x + 326)/(1287 + 326)*100)})
r <- round(r)


####Clean up and Combine observation points####

clean.herb <- Herbarium %>% select(decimalLat, decimalLon)
clean.inat <- iNat %>% select(latitude, longitude)
colnames(clean.herb) <- c("latitude", "longitude", "geometry")

clean.obs <- rbind(clean.herb, clean.inat)
class(clean.obs)
st_geometry(clean.obs) <- NULL

clean.obs <- clean.obs[, c("longitude", "latitude")]

# The data arrive as a data.frame, so convert it to a spatial object
colnames(clean.obs) <- c("lon", "lat")
coordinates(clean.obs) <- ~ lon + lat
# If we want to associate data with each of the points in the data.frame
# then we can convert to a SpatialPointsDataFrame
clean.obs <- SpatialPointsDataFrame(coords = coordinates(clean.obs),
                                    data = data.frame(clean.obs))

# Project points, I'm using this one: SR-ORG:7260, NAD83 / US Pacific Northwest Albers
proj4string(clean.obs) <- CRS("+proj=longlat +datum=WGS84
+no_defs +ellps=WGS84 +towgs84=0,0,0")
# Plot a simple map
maps::map(database = "state", regions = c("oregon", "washington"))
points(clean.obs, pch = 21, bg = "dodgerblue")


####Clean Up Herbarium Points####
clean.herb <- Herbarium %>% select(decimalLat, decimalLon)
colnames(clean.herb) <- c("latitude", "longitude", "geometry")

st_geometry(clean.herb) <- NULL

clean.herb <- clean.herb[, c("longitude", "latitude")]

# The data arrive as a data.frame, so convert it to a spatial object
colnames(clean.herb) <- c("lon", "lat")
coordinates(clean.herb) <- ~ lon + lat
# If we want to associate data with each of the points in the data.frame
# then we can convert to a SpatialPointsDataFrame
clean.herb <- SpatialPointsDataFrame(coords = coordinates(clean.herb),
                                     data = data.frame(clean.herb))

# Project points, I'm using this one: SR-ORG:7260, NAD83 / US Pacific Northwest Albers
proj4string(clean.herb) <- CRS("+proj=longlat +datum=WGS84
+no_defs +ellps=WGS84 +towgs84=0,0,0")
# Plot a simple map
maps::map(database = "state", regions = c("oregon", "washington"))
points(clean.herb, pch = 21, bg = "dodgerblue")

####Clean Up iNat Points####
clean.inat <- iNat %>% select(latitude, longitude)
st_geometry(clean.inat) <- NULL

clean.inat <- clean.inat[, c("longitude", "latitude")]

# The data arrive as a data.frame, so convert it to a spatial object
colnames(clean.inat) <- c("lon", "lat")
coordinates(clean.inat) <- ~ lon + lat
# If we want to associate data with each of the points in the data.frame
# then we can convert to a SpatialPointsDataFrame
clean.inat <- SpatialPointsDataFrame(coords = coordinates(clean.inat),
                                     data = data.frame(clean.inat))

# Project points, I'm using this one: SR-ORG:7260, NAD83 / US Pacific Northwest Albers
proj4string(clean.inat) <- CRS("+proj=longlat +datum=WGS84
+no_defs +ellps=WGS84 +towgs84=0,0,0")
# Plot a simple map
maps::map(database = "state", regions = c("oregon", "washington"))
points(clean.inat, pch = 21, bg = "dodgerblue")









####The Modelling Finally####

#Extract climate data using observations
clean.obs <- spTransform(clean.herb,
                         proj4string(env.var250))

env.point250 <- extract(env.var250, clean.obs)

rec <- clean.obs
#Extracting the lat/lon information
rec <- rec[!(is.na(rec$lat) | is.na(rec$lon)), ]
#Turning it into a simple feature
rec <- st_as_sf(x = rec, 
                coords = c("lon", "lat"),
                crs = "+proj=longlat +datum=WGS84")


# Setting the study area boundaries
area = list(min_long = -125, max_long = -116, min_lat = 41, max_lat = 50)

#Turning the data into a data frame which can be used in maxent
rec <- SpatialPointsDataFrame(coords = st_coordinates(rec),
                              data = as.data.frame(rec))
rec <- as.data.frame(rec)

# the name of studied species
myRespName <- 'Cantharellus formosus'

# the XY coordinates of species data
myRespXY <- rec[,c("X","Y")]
myRespXY[] <- apply(myRespXY, 2, function(x) as.numeric(as.character(x)))

clim.coord <- coordinates(env.var250)
colnames(clim.coord) <- colnames(myRespXY)


# some pseudo absence data
samp <- sample(nrow(clim.coord), 0)
myRespXY <- rbind(data.frame(myRespXY), clim.coord[samp,])

# the presence/absences data for our species
myResp <- c(rep(1, nrow(rec)), rep(0, length(samp)))

#Making sure that the sampled points aren't coincident with a study point
d <- duplicated(paste(myRespXY$X, myRespXY$Y))
myRespXY <- myRespXY[!d,]
myResp <- myResp[!d]

myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = env.var250,
                                     resp.xy = as.matrix(myRespXY),
                                     resp.name = myRespName,
                                     eval.expl.var = env.var250,
                                     eval.resp.xy = clean.inat,
                                     PA.nb.rep = 1,
                                     PA.nb.absences = 200,
                                     PA.strategy = "sre",
                                     PA.sre.quant = .15,
                                     na.rm = TRUE)
##  Defining Models Options using default options
myBiomodOption <- BIOMOD_ModelingOptions()


#Creating a weight layer based of the effort variable values

my.weights <- myBiomodData@coord
my.weights$yweights <- extract(r, my.weights)
sapply(my.weights, function(y) sum(length(which(is.na(y)))))

## Computing the models
myBiomodModelOut <- BIOMOD_Modeling(
  myBiomodData,
  models = c("GLM", "ANN", "MAXENT.Phillips","RF"), 
  models.options = myBiomodOption,
  NbRunEval=1,
  DataSplit=100,
  Prevalence=0.5,
  VarImport=3,
  Yweights = my.weights$yweights,
  models.eval.meth = c('KAPPA', 'TSS', 'ROC', 'ETS'),
  SaveObj = TRUE,
  rescal.all.models = FALSE,
  do.full.models = FALSE,
  modeling.id = paste(myRespName,"FirstModeling",sep=""))


myBiomodModelEval <- get_evaluations(myBiomodModelOut)

# print the evaluation metrics
myBiomodModelEval[]

barplot(get_variables_importance(myBiomodModelOut)[,,,], beside = TRUE, las = 2)
get_variables_importance(myBiomodModelOut)

myBiomodProj <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = env.var250, 
  proj.name = 'current', 
  selected.models = 'all', 
  binary.meth = 'TSS', 
  compress = 'xz', 
  clamping.mask = F, 
  output.format = '.grd')

plot(myBiomodProj)