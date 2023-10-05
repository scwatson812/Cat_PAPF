##########################
##### Load Libraries #####
##########################
library(ape)
library(maptools)
library(spatstat)
library(rgeos)
library(rgdal)
library(raster)
library(sf)
library(spdep)

#########################
##### Load the data #####
#########################
###fsa.path should be the file path to the shapefile containing the data (as a shapefile)
###layer in shapefile containing the category assignments should be called 'selected'
###Categories should start a 0 and increase numerically
fsa.path = 'C:/Users/scwatson/Stella Self Dropbox/Stella Watson/Synced Files/Research/Conservation Easements/Experimental Stuff/FSA App/'
fsa.sh <- readOGR(paste0(fsa.path,"ExampleData.shp"))

plot(fsa.sh)

######################
##### Parameters #####
######################
###radius.vec should be the radii at which we wish to evaluate the test statistic
radius.vec <- c(1021.223, 2017.167, 3013.112, 4009.056, 5005.000)
n.rad <- length(radius.vec)
K = length(unique(fsa.sh$selected))
n <- dim(fsa.sh)[1]
###Number of Monte Carlo Simulations
n.MC <- 5

### Make the shapefiles
fsa.sf <- st_as_sf(fsa.sh)
ca.sh = unionSpatialPolygons(fsa.sh,fsa.sh$ID)
ca.sf = st_as_sf(ca.sh)
ca.sf = st_buffer(ca.sf,0)

###########################################
##### Run the Monte Carlo Simulations #####
###########################################
observed.area.mat.00 <- c()
observed.area.mat.01 <- c()
observed.area.mat.02 <- c()
observed.area.mat.03 <- c()
#observed.area.mat.04 <- c()

observed.area.mat.10 <- c()
observed.area.mat.11 <- c()
observed.area.mat.12 <- c()
observed.area.mat.13 <- c()
#observed.area.mat.14 <- c()

observed.area.mat.20 <- c()
observed.area.mat.21 <- c()
observed.area.mat.22 <- c()
observed.area.mat.23 <- c()
#observed.area.mat.24 <- c()

observed.area.mat.30 <- c()
observed.area.mat.31 <- c()
observed.area.mat.32 <- c()
observed.area.mat.33 <- c()
#observed.area.mat.34 <- c()

#observed.area.mat.40 <- c()
#observed.area.mat.41 <- c()
#observed.area.mat.42 <- c()
#observed.area.mat.43 <- c()
#observed.area.mat.44 <- c()

TA0 <- c()
TA1 <- c()
TA2 <- c()
TA3 <- c()
#TA4 <- c()

for(g in 1:n.MC){
  fsa.sh$selected <- sample(fsa.sh$selected,size = n, replace = F)

  ###Obtain the Centriods of the Observed Units
  centers.obs.points =  coordinates(fsa.sh)
  centers.obs.points = SpatialPoints(cbind(centers.obs.points[,1],centers.obs.points[,2]),proj4string = CRS(proj4string(ca.sh)))
  centers.obs.points = st_as_sf(centers.obs.points)
  n.obs.points = dim(centers.obs.points)[1]


  ###Create a list of length n.rad whose ith element is a vector of circles of radius radius.vec[i] centered as each of the observed areal unit centers
  ###Currently implemented for 4 categories
  ###Can extend to 5 categories by uncommenting
  ###Can extend to additional categories by replicating the commented out portions as many times as necessary and adjusting the numbers 
  obs.circles.list.0 = list()
  obs.circles.list.1 = list()
  obs.circles.list.2 = list()
  obs.circles.list.3 = list()
  #obs.circles.list.4 = list()

  ct = 1
  for(rad in 1:n.rad){
    radius = radius.vec[rad]
    circle.obs = st_buffer(centers.obs.points[fsa.sh$selected == 0,],dist = radius)
    for(i in 1:dim(circle.obs)[1]){
      obs.circles.list.0[[ct]] = circle.obs[i,]
      ct = ct + 1
    }
  }

  ct = 1
  for(rad in 1:n.rad){
    radius = radius.vec[rad]
    circle.obs = st_buffer(centers.obs.points[fsa.sh$selected == 1,],dist = radius)
    for(i in 1:dim(circle.obs)[1]){
      obs.circles.list.1[[ct]] = circle.obs[i,]
      ct = ct + 1
    }
  }

  ct = 1
  for(rad in 1:n.rad){
    radius = radius.vec[rad]
    circle.obs = st_buffer(centers.obs.points[fsa.sh$selected == 2,],dist = radius)
    for(i in 1:dim(circle.obs)[1]){
      obs.circles.list.2[[ct]] = circle.obs[i,]
      ct = ct + 1
    }
  }

  ct = 1
  for(rad in 1:n.rad){
    radius = radius.vec[rad]
    circle.obs = st_buffer(centers.obs.points[fsa.sh$selected == 3,],dist = radius)
    for(i in 1:dim(circle.obs)[1]){
      obs.circles.list.3[[ct]] = circle.obs[i,]
      ct = ct + 1
    }
  }

  # ct = 1
  # for(rad in 1:n.rad){
  #   radius = radius.vec[rad]
  #   circle.obs = st_buffer(centers.obs.points[fsa.sh$selected == 4,],dist = radius)
  #   for(i in 1:dim(circle.obs)[1]){
  #     obs.circles.list.4[[ct]] = circle.obs[i,]
  #     ct = ct + 1
  #   }
  # }

  obs.intersection.calc.0<-function(circle){
    obs.intersection = st_intersection(circle,fsa.sf[fsa.sf$selected ==0,])
    area = st_area(obs.intersection)
    if(length(area)==0){
      area = 0
    }
    return(sum(area))
  }
  
  obs.intersection.calc.1<-function(circle){
    obs.intersection = st_intersection(circle,fsa.sf[fsa.sf$selected ==1,])
    area = st_area(obs.intersection)
    if(length(area)==0){
      area = 0
    }
    return(sum(area))
  }

  obs.intersection.calc.2<-function(circle){
    obs.intersection = st_intersection(circle,fsa.sf[fsa.sf$selected ==2,])
    area = st_area(obs.intersection)
    if(length(area)==0){
      area = 0
    }
    return(sum(area))
  }

  obs.intersection.calc.3<-function(circle){
    obs.intersection = st_intersection(circle,fsa.sf[fsa.sf$selected ==3,])
    area = st_area(obs.intersection)
    if(length(area)==0){
      area = 0
    }
    return(sum(area))
  }

  # obs.intersection.calc.4<-function(circle){
  #   obs.intersection = st_intersection(circle,fsa.sf[fsa.sf$selected ==4,])
  #   area = st_area(obs.intersection)
  #   if(length(area)==0){
  #     area = 0
  #   }
  #   return(sum(area))
  # }


  circle.area.calc<-function(circle){
    area = st_area(st_intersection(ca.sf,circle))
    if(length(area)==0){
      area = 0
    }
    return(sum(area))
  }

  observed.area.vec.00 = unlist(lapply(obs.circles.list.0,obs.intersection.calc.0))
  observed.area.vec.01 = unlist(lapply(obs.circles.list.0,obs.intersection.calc.1))
  observed.area.vec.02 = unlist(lapply(obs.circles.list.0,obs.intersection.calc.2))
  observed.area.vec.03 = unlist(lapply(obs.circles.list.0,obs.intersection.calc.3))
  #observed.area.vec.04 = unlist(lapply(obs.circles.list.0,obs.intersection.calc.4))

  observed.area.vec.10 = unlist(lapply(obs.circles.list.1,obs.intersection.calc.0))
  observed.area.vec.11 = unlist(lapply(obs.circles.list.1,obs.intersection.calc.1))
  observed.area.vec.12 = unlist(lapply(obs.circles.list.1,obs.intersection.calc.2))
  observed.area.vec.13 = unlist(lapply(obs.circles.list.1,obs.intersection.calc.3))
  #observed.area.vec.14 = unlist(lapply(obs.circles.list.1,obs.intersection.calc.4))

  observed.area.vec.20 = unlist(lapply(obs.circles.list.2,obs.intersection.calc.0))
  observed.area.vec.21 = unlist(lapply(obs.circles.list.2,obs.intersection.calc.1))
  observed.area.vec.22 = unlist(lapply(obs.circles.list.2,obs.intersection.calc.2))
  observed.area.vec.23 = unlist(lapply(obs.circles.list.2,obs.intersection.calc.3))
  #observed.area.vec.24 = unlist(lapply(obs.circles.list.2,obs.intersection.calc.4))

  observed.area.vec.30 = unlist(lapply(obs.circles.list.3,obs.intersection.calc.0))
  observed.area.vec.31 = unlist(lapply(obs.circles.list.3,obs.intersection.calc.1))
  observed.area.vec.32 = unlist(lapply(obs.circles.list.3,obs.intersection.calc.2))
  observed.area.vec.33 = unlist(lapply(obs.circles.list.3,obs.intersection.calc.3))
  #observed.area.vec.34 = unlist(lapply(obs.circles.list.3,obs.intersection.calc.4))

  #observed.area.vec.40 = unlist(lapply(obs.circles.list.4,obs.intersection.calc.0))
  #observed.area.vec.41 = unlist(lapply(obs.circles.list.4,obs.intersection.calc.1))
  #observed.area.vec.42 = unlist(lapply(obs.circles.list.4,obs.intersection.calc.2))
  #observed.area.vec.43 = unlist(lapply(obs.circles.list.4,obs.intersection.calc.3))
  #observed.area.vec.44 = unlist(lapply(obs.circles.list.4,obs.intersection.calc.4))

  circle.area.vec.0 = unlist(lapply(obs.circles.list.0,circle.area.calc))
  circle.area.vec.1 = unlist(lapply(obs.circles.list.1,circle.area.calc))
  circle.area.vec.2 = unlist(lapply(obs.circles.list.2,circle.area.calc))
  circle.area.vec.3 = unlist(lapply(obs.circles.list.3,circle.area.calc))
  #circle.area.vec.4 = unlist(lapply(obs.circles.list.4,circle.area.calc))

  n.obs.points.0 = sum(fsa.sh$selected == 0)
  n.obs.points.1 = sum(fsa.sh$selected == 1)
  n.obs.points.2 = sum(fsa.sh$selected == 2)
  n.obs.points.3 = sum(fsa.sh$selected == 3)
  #n.obs.points.4 = sum(fsa.sh$selected == 4)

  observed.area.mat.00 = rbind(observed.area.mat.00,matrix(observed.area.vec.00/circle.area.vec.0,n.rad,n.obs.points.0,byrow = TRUE))
  observed.area.mat.01 = rbind(observed.area.mat.01,matrix(observed.area.vec.01/circle.area.vec.0,n.rad,n.obs.points.0,byrow = TRUE))
  observed.area.mat.02 = rbind(observed.area.mat.02,matrix(observed.area.vec.02/circle.area.vec.0,n.rad,n.obs.points.0,byrow = TRUE))
  observed.area.mat.03 = rbind(observed.area.mat.03,matrix(observed.area.vec.03/circle.area.vec.0,n.rad,n.obs.points.0,byrow = TRUE))
  #observed.area.mat.04 = rbind(observed.area.mat.04,matrix(observed.area.vec.04/circle.area.vec.0,n.rad,n.obs.points.0,byrow = TRUE))

  observed.area.mat.10 = rbind(observed.area.mat.10,matrix(observed.area.vec.10/circle.area.vec.1,n.rad,n.obs.points.1,byrow = TRUE))
  observed.area.mat.11 = rbind(observed.area.mat.11,matrix(observed.area.vec.11/circle.area.vec.1,n.rad,n.obs.points.1,byrow = TRUE))
  observed.area.mat.12 = rbind(observed.area.mat.12,matrix(observed.area.vec.12/circle.area.vec.1,n.rad,n.obs.points.1,byrow = TRUE))
  observed.area.mat.13 = rbind(observed.area.mat.13,matrix(observed.area.vec.13/circle.area.vec.1,n.rad,n.obs.points.1,byrow = TRUE))
  #observed.area.mat.14 = rbind(observed.area.mat.14,matrix(observed.area.vec.14/circle.area.vec.1,n.rad,n.obs.points.1,byrow = TRUE))

  observed.area.mat.20 = rbind(observed.area.mat.20,matrix(observed.area.vec.20/circle.area.vec.2,n.rad,n.obs.points.2,byrow = TRUE))
  observed.area.mat.21 = rbind(observed.area.mat.21,matrix(observed.area.vec.21/circle.area.vec.2,n.rad,n.obs.points.2,byrow = TRUE))
  observed.area.mat.22 = rbind(observed.area.mat.22,matrix(observed.area.vec.22/circle.area.vec.2,n.rad,n.obs.points.2,byrow = TRUE))
  observed.area.mat.23 = rbind(observed.area.mat.23,matrix(observed.area.vec.23/circle.area.vec.2,n.rad,n.obs.points.2,byrow = TRUE))
  #observed.area.mat.24 = rbind(observed.area.mat.24,matrix(observed.area.vec.24/circle.area.vec.2,n.rad,n.obs.points.2,byrow = TRUE))

  observed.area.mat.30 = rbind(observed.area.mat.30,matrix(observed.area.vec.30/circle.area.vec.3,n.rad,n.obs.points.3,byrow = TRUE))
  observed.area.mat.31 = rbind(observed.area.mat.31,matrix(observed.area.vec.31/circle.area.vec.3,n.rad,n.obs.points.3,byrow = TRUE))
  observed.area.mat.32 = rbind(observed.area.mat.32,matrix(observed.area.vec.32/circle.area.vec.3,n.rad,n.obs.points.3,byrow = TRUE))
  observed.area.mat.33 = rbind(observed.area.mat.33,matrix(observed.area.vec.33/circle.area.vec.3,n.rad,n.obs.points.3,byrow = TRUE))
  #observed.area.mat.34 = rbind(observed.area.mat.34,matrix(observed.area.vec.34/circle.area.vec.3,n.rad,n.obs.points.3,byrow = TRUE))

  # observed.area.mat.40 = rbind(observed.area.mat.40,matrix(observed.area.vec.40/circle.area.vec.4,n.rad,n.obs.points.4,byrow = TRUE))
  # observed.area.mat.41 = rbind(observed.area.mat.41,matrix(observed.area.vec.41/circle.area.vec.4,n.rad,n.obs.points.4,byrow = TRUE))
  # observed.area.mat.42 = rbind(observed.area.mat.42,matrix(observed.area.vec.42/circle.area.vec.4,n.rad,n.obs.points.4,byrow = TRUE))
  # observed.area.mat.43 = rbind(observed.area.mat.43,matrix(observed.area.vec.43/circle.area.vec.4,n.rad,n.obs.points.4,byrow = TRUE))
  # observed.area.mat.44 = rbind(observed.area.mat.44,matrix(observed.area.vec.44/circle.area.vec.4,n.rad,n.obs.points.4,byrow = TRUE))
  
  TA0 <- c(TA0,sum(area(fsa.sh[fsa.sh$selected == 0,]))/sum(area(fsa.sh)))
  TA1 <- c(TA1,sum(area(fsa.sh[fsa.sh$selected == 1,]))/sum(area(fsa.sh)))
  TA2 <- c(TA2,sum(area(fsa.sh[fsa.sh$selected == 2,]))/sum(area(fsa.sh)))
  TA3 <- c(TA3,sum(area(fsa.sh[fsa.sh$selected == 3,]))/sum(area(fsa.sh)))
  #TA4 <- c(TA4,sum(area(fsa.sh[fsa.sh$selected == 4,]))/sum(area(fsa.sh)))
  
  print(g)
}

write.table(observed.area.mat.00,"Null00.txt",row.names = FALSE, col.names = FALSE)
write.table(observed.area.mat.01,"Null01.txt",row.names = FALSE, col.names = FALSE)
write.table(observed.area.mat.02,"Null02.txt",row.names = FALSE, col.names = FALSE)
write.table(observed.area.mat.03,"Null03.txt",row.names = FALSE, col.names = FALSE)
#write.table(observed.area.mat.04,"Null04.txt",row.names = FALSE, col.names = FALSE)

write.table(observed.area.mat.10,"Null10.txt",row.names = FALSE, col.names = FALSE)
write.table(observed.area.mat.11,"Null11.txt",row.names = FALSE, col.names = FALSE)
write.table(observed.area.mat.12,"Null12.txt",row.names = FALSE, col.names = FALSE)
write.table(observed.area.mat.13,"Null13.txt",row.names = FALSE, col.names = FALSE)
#write.table(observed.area.mat.14,"Null14.txt",row.names = FALSE, col.names = FALSE)

write.table(observed.area.mat.20,"Null20.txt",row.names = FALSE, col.names = FALSE)
write.table(observed.area.mat.21,"Null21.txt",row.names = FALSE, col.names = FALSE)
write.table(observed.area.mat.22,"Null22.txt",row.names = FALSE, col.names = FALSE)
write.table(observed.area.mat.23,"Null23.txt",row.names = FALSE, col.names = FALSE)
#write.table(observed.area.mat.24,"Null24.txt",row.names = FALSE, col.names = FALSE)

write.table(observed.area.mat.30,"Null30.txt",row.names = FALSE, col.names = FALSE)
write.table(observed.area.mat.31,"Null31.txt",row.names = FALSE, col.names = FALSE)
write.table(observed.area.mat.32,"Null32.txt",row.names = FALSE, col.names = FALSE)
write.table(observed.area.mat.33,"Null33.txt",row.names = FALSE, col.names = FALSE)
#write.table(observed.area.mat.34,"Null34.txt",row.names = FALSE, col.names = FALSE)

# write.table(observed.area.mat.40,"Null40.txt",row.names = FALSE, col.names = FALSE)
# write.table(observed.area.mat.41,"Null41.txt",row.names = FALSE, col.names = FALSE)
# write.table(observed.area.mat.42,"Null42.txt",row.names = FALSE, col.names = FALSE)
# write.table(observed.area.mat.43,"Null43.txt",row.names = FALSE, col.names = FALSE)
# write.table(observed.area.mat.44,"Null44.txt",row.names = FALSE, col.names = FALSE)

write.table(TA0,"NullTotalAreaProp0.txt",row.names = FALSE, col.names = FALSE)
write.table(TA1,"NullTotalAreaProp1.txt",row.names = FALSE, col.names = FALSE)
write.table(TA2,"NullTotalAreaProp2.txt",row.names = FALSE, col.names = FALSE)
write.table(TA3,"NullTotalAreaProp3.txt",row.names = FALSE, col.names = FALSE)
#write.table(TA4,"TotalAreaProp4.txt",row.names = FALSE, col.names = FALSE)


