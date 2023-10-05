###path should be the file path to where the null distribution is stored
path = 'C:/Users/scwatson/Documents/'
###n.rad should be the number of radii
n.rad = 5
###n0 should be the number of units in the 0 category
n.0 <- 270
###n.vec should be a vector of length K-1 with the number of units in categories 1, 2, 3, etc.
n.vec <-  c(10,80,40)
###sims should be the number of Monte Carlo simulations
sims <- 5
###ub should be probability associated with the critical value (e.g. for an upper tailed test with alpha = 0.05, ub = 0.95)
ub = 0.95

OTS.00 = read.table(paste0(path,"Null00.txt"),fill = T)
OTS.01 = read.table(paste0(path,"Null01.txt"),fill = T)
OTS.02 = read.table(paste0(path,"Null02.txt"),fill = T)
OTS.03 = read.table(paste0(path,"Null03.txt"),fill = T)
#OTS.04 = read.table(paste0(path,"Null04.txt"),fill = T)

OTS.10 = read.table(paste0(path,"Null10.txt"),fill = T)
OTS.11 = read.table(paste0(path,"Null11.txt"),fill = T)
OTS.12 = read.table(paste0(path,"Null12.txt"),fill = T)
OTS.13 = read.table(paste0(path,"Null13.txt"),fill = T)
#OTS.14 = read.table(paste0(path,"Null14.txt"),fill = T)

OTS.20 = read.table(paste0(path,"Null20.txt"),fill = T)
OTS.21 = read.table(paste0(path,"Null21.txt"),fill = T)
OTS.22 = read.table(paste0(path,"Null22.txt"),fill = T)
OTS.23 = read.table(paste0(path,"Null23.txt"),fill = T)
#OTS.24 = read.table(paste0(path,"Null24.txt"),fill = T)

OTS.30 = read.table(paste0(path,"Null30.txt"),fill = T)
OTS.31 = read.table(paste0(path,"Null31.txt"),fill = T)
OTS.32 = read.table(paste0(path,"Null32.txt"),fill = T)
OTS.33 = read.table(paste0(path,"Null33.txt"),fill = T)
#OTS.34 = read.table(paste0(path,"Null34.txt"),fill = T)

#OTS.40 = read.table(paste0(path,"Null40.txt"),fill = T)
#OTS.41 = read.table(paste0(path,"Null41.txt"),fill = T)
#OTS.42 = read.table(paste0(path,"Null42.txt"),fill = T)
#OTS.43 = read.table(paste0(path,"Null43.txt"),fill = T)
#OTS.44 = read.table(paste0(path,"Null44.txt"),fill = T)

TAP.0 = read.table(paste0(path,"NullTotalAreaProp0.txt"),fill = T)
TAP.1 = read.table(paste0(path,"NullTotalAreaProp1.txt"),fill = T)
TAP.2 = read.table(paste0(path,"NullTotalAreaProp2.txt"),fill = T)
TAP.3 = read.table(paste0(path,"NullTotalAreaProp3.txt"),fill = T)
#TAP.4 = read.table(paste0(path,"NullTotalAreaProp4.txt"),fill = T)

TAP.0 <- unlist(TAP.0)
TAP.1 <- unlist(TAP.1)
TAP.2 <- unlist(TAP.2)
TAP.3 <- unlist(TAP.3)
#TAP.4 <- unlist(TAP.4)


OTS.00.mat <- matrix(unlist(t(OTS.00)),sims,n.0*n.rad,byrow = T)
OTS.01.mat <- matrix(unlist(t(OTS.01)),sims,n.0*n.rad,byrow = T)
OTS.02.mat <- matrix(unlist(t(OTS.02)),sims,n.0*n.rad,byrow = T)
OTS.03.mat <- matrix(unlist(t(OTS.03)),sims,n.0*n.rad,byrow = T)
#OTS.04.mat <- matrix(unlist(t(OTS.04)),sims,n.0*n.rad,byrow = T)

OTS.10.mat <- matrix(unlist(t(OTS.10)),sims,n.vec[1]*n.rad,byrow = T)
OTS.11.mat <- matrix(unlist(t(OTS.11)),sims,n.vec[1]*n.rad,byrow = T)
OTS.12.mat <- matrix(unlist(t(OTS.12)),sims,n.vec[1]*n.rad,byrow = T)
OTS.13.mat <- matrix(unlist(t(OTS.13)),sims,n.vec[1]*n.rad,byrow = T)
#OTS.14.mat <- matrix(unlist(t(OTS.14)),sims,n.vec[1]*n.rad,byrow = T)

OTS.20.mat <- matrix(unlist(t(OTS.20)),sims,n.vec[2]*n.rad,byrow = T)
OTS.21.mat <- matrix(unlist(t(OTS.21)),sims,n.vec[2]*n.rad,byrow = T)
OTS.22.mat <- matrix(unlist(t(OTS.22)),sims,n.vec[2]*n.rad,byrow = T)
OTS.23.mat <- matrix(unlist(t(OTS.23)),sims,n.vec[2]*n.rad,byrow = T)
#OTS.24.mat <- matrix(unlist(t(OTS.24)),sims,n.vec[2]*n.rad,byrow = T)

OTS.30.mat <- matrix(unlist(t(OTS.30)),sims,n.vec[3]*n.rad,byrow = T)
OTS.31.mat <- matrix(unlist(t(OTS.31)),sims,n.vec[3]*n.rad,byrow = T)
OTS.32.mat <- matrix(unlist(t(OTS.32)),sims,n.vec[3]*n.rad,byrow = T)
OTS.33.mat <- matrix(unlist(t(OTS.33)),sims,n.vec[3]*n.rad,byrow = T)
#OTS.34.mat <- matrix(unlist(t(OTS.34)),sims,n.vec[3]*n.rad,byrow = T)

# OTS.40.mat <- matrix(unlist(t(OTS.40)),sims,n.vec[4]*n.rad,byrow = T)
# OTS.41.mat <- matrix(unlist(t(OTS.41)),sims,n.vec[4]*n.rad,byrow = T)
# OTS.42.mat <- matrix(unlist(t(OTS.42)),sims,n.vec[4]*n.rad,byrow = T)
# OTS.43.mat <- matrix(unlist(t(OTS.43)),sims,n.vec[4]*n.rad,byrow = T)
# OTS.44.mat <- matrix(unlist(t(OTS.44)),sims,n.vec[4]*n.rad,byrow = T)

OTS.00.TS <- matrix(NA,sims,n.rad)
for(i in 1:sims){
  for(r in 1:n.rad){
    OTS.00.TS[i,r] = mean(OTS.00.mat[i,(n.0*(r-1)+1):(n.0*r)]/as.numeric(TAP.0[i]))
  }
}

OTS.01.TS <- matrix(NA,sims,n.rad)
for(i in 1:sims){
  for(r in 1:n.rad){
    OTS.01.TS[i,r] = mean(OTS.01.mat[i,(n.0*(r-1)+1):(n.0*r)]/as.numeric(TAP.1[i]))
  }
}

OTS.02.TS <- matrix(NA,sims,n.rad)
for(i in 1:sims){
  for(r in 1:n.rad){
    OTS.02.TS[i,r] = mean(OTS.02.mat[i,(n.0*(r-1)+1):(n.0*r)]/as.numeric(TAP.2[i]))
  }
}

OTS.03.TS <- matrix(NA,sims,n.rad)
for(i in 1:sims){
  for(r in 1:n.rad){
    OTS.03.TS[i,r] = mean(OTS.03.mat[i,(n.0*(r-1)+1):(n.0*r)]/as.numeric(TAP.3[i]))
  }
}

# OTS.04.TS <- matrix(NA,sims,n.rad)
# for(i in 1:sims){
#   for(r in 1:n.rad){
#     OTS.04.TS[i,r] = mean(OTS.04.mat[i,(n.0*(r-1)+1):(n.0*r)]/as.numeric(TAP.4[i]))
#   }
# }


OTS.10.TS <- matrix(NA,sims,n.rad)
for(i in 1:sims){
  for(r in 1:n.rad){
    OTS.10.TS[i,r] = mean(OTS.10.mat[i,(n.vec[1]*(r-1)+1):(n.vec[1]*r)]/as.numeric(TAP.0[i]))
  }
}

OTS.11.TS <- matrix(NA,sims,n.rad)
for(i in 1:sims){
  for(r in 1:n.rad){
    OTS.11.TS[i,r] = mean(OTS.11.mat[i,(n.vec[1]*(r-1)+1):(n.vec[1]*r)]/as.numeric(TAP.1[i]))
  }
}

OTS.12.TS <- matrix(NA,sims,n.rad)
for(i in 1:sims){
  for(r in 1:n.rad){
    OTS.12.TS[i,r] = mean(OTS.12.mat[i,(n.vec[1]*(r-1)+1):(n.vec[1]*r)]/as.numeric(TAP.2[i]))
  }
}

OTS.13.TS <- matrix(NA,sims,n.rad)
for(i in 1:sims){
  for(r in 1:n.rad){
    OTS.13.TS[i,r] = mean(OTS.13.mat[i,(n.vec[1]*(r-1)+1):(n.vec[1]*r)]/as.numeric(TAP.3[i]))
  }
}

# OTS.14.TS <- matrix(NA,sims,n.rad)
# for(i in 1:sims){
#   for(r in 1:n.rad){
#     OTS.14.TS[i,r] = mean(OTS.14.mat[i,(n.vec[1]*(r-1)+1):(n.vec[1]*r)]/as.numeric(TAP.4[i]))
#   }
# }


OTS.20.TS <- matrix(NA,sims,n.rad)
for(i in 1:sims){
  for(r in 1:n.rad){
    OTS.20.TS[i,r] = mean(OTS.20.mat[i,(n.vec[2]*(r-1)+1):(n.vec[2]*r)]/as.numeric(TAP.0[i]))
  }
}

OTS.21.TS <- matrix(NA,sims,n.rad)
for(i in 1:sims){
  for(r in 1:n.rad){
    OTS.21.TS[i,r] = mean(OTS.21.mat[i,(n.vec[2]*(r-1)+1):(n.vec[2]*r)]/as.numeric(TAP.1[i]))
  }
}

OTS.22.TS <- matrix(NA,sims,n.rad)
for(i in 1:sims){
  for(r in 1:n.rad){
    OTS.22.TS[i,r] = mean(OTS.22.mat[i,(n.vec[2]*(r-1)+1):(n.vec[2]*r)]/as.numeric(TAP.2[i]))
  }
}

OTS.23.TS <- matrix(NA,sims,n.rad)
for(i in 1:sims){
  for(r in 1:n.rad){
    OTS.23.TS[i,r] = mean(OTS.23.mat[i,(n.vec[2]*(r-1)+1):(n.vec[2]*r)]/as.numeric(TAP.3[i]))
  }
}

# OTS.24.TS <- matrix(NA,sims,n.rad)
# for(i in 1:sims){
#   for(r in 1:n.rad){
#     OTS.24.TS[i,r] = mean(OTS.24.mat[i,(n.vec[2]*(r-1)+1):(n.vec[2]*r)]/as.numeric(TAP.4[i]))
#   }
# }

OTS.30.TS <- matrix(NA,sims,n.rad)
for(i in 1:sims){
  for(r in 1:n.rad){
    OTS.30.TS[i,r] = mean(OTS.30.mat[i,(n.vec[3]*(r-1)+1):(n.vec[3]*r)]/as.numeric(TAP.0[i]))
  }
}

OTS.31.TS <- matrix(NA,sims,n.rad)
for(i in 1:sims){
  for(r in 1:n.rad){
    OTS.31.TS[i,r] = mean(OTS.31.mat[i,(n.vec[3]*(r-1)+1):(n.vec[3]*r)]/as.numeric(TAP.1[i]))
  }
}

OTS.32.TS <- matrix(NA,sims,n.rad)
for(i in 1:sims){
  for(r in 1:n.rad){
    OTS.32.TS[i,r] = mean(OTS.32.mat[i,(n.vec[3]*(r-1)+1):(n.vec[3]*r)]/as.numeric(TAP.2[i]))
  }
}

OTS.33.TS <- matrix(NA,sims,n.rad)
for(i in 1:sims){
  for(r in 1:n.rad){
    OTS.33.TS[i,r] = mean(OTS.33.mat[i,(n.vec[3]*(r-1)+1):(n.vec[3]*r)]/as.numeric(TAP.3[i]))
  }
}

# OTS.34.TS <- matrix(NA,sims,n.rad)
# for(i in 1:sims){
#   for(r in 1:n.rad){
#     OTS.34.TS[i,r] = mean(OTS.34.mat[i,(n.vec[3]*(r-1)+1):(n.vec[3]*r)]/as.numeric(TAP.4[i]))
#   }
# }

# OTS.40.TS <- matrix(NA,sims,n.rad)
# for(i in 1:sims){
#   for(r in 1:n.rad){
#     OTS.40.TS[i,r] = mean(OTS.40.mat[i,(n.vec[4]*(r-1)+1):(n.vec[4]*r)]/as.numeric(TAP.0[i]))
#   }
# }

# OTS.41.TS <- matrix(NA,sims,n.rad)
# for(i in 1:sims){
#   for(r in 1:n.rad){
#     OTS.41.TS[i,r] = mean(OTS.41.mat[i,(n.vec[4]*(r-1)+1):(n.vec[4]*r)]/as.numeric(TAP.1[i]))
#   }
# }

# OTS.42.TS <- matrix(NA,sims,n.rad)
# for(i in 1:sims){
#   for(r in 1:n.rad){
#     OTS.42.TS[i,r] = mean(OTS.42.mat[i,(n.vec[4]*(r-1)+1):(n.vec[4]*r)]/as.numeric(TAP.2[i]))
#   }
# }

# OTS.43.TS <- matrix(NA,sims,n.rad)
# for(i in 1:sims){
#   for(r in 1:n.rad){
#     OTS.43.TS[i,r] = mean(OTS.43.mat[i,(n.vec[4]*(r-1)+1):(n.vec[4]*r)]/as.numeric(TAP.3[i]))
#   }
# }

# OTS.44.TS <- matrix(NA,sims,n.rad)
# for(i in 1:sims){
#   for(r in 1:n.rad){
#     OTS.44.TS[i,r] = mean(OTS.44.mat[i,(n.vec[4]*(r-1)+1):(n.vec[4]*r)]/as.numeric(TAP.4[i]))
#   }
# }

mean.00 <- apply(OTS.00.TS,2,mean)
mean.01 <- apply(OTS.01.TS,2,mean)
mean.02 <- apply(OTS.02.TS,2,mean)
mean.03 <- apply(OTS.03.TS,2,mean)
#mean.04 <- apply(OTS.04.TS,2,mean)

mean.10 <- apply(OTS.10.TS,2,mean)
mean.11 <- apply(OTS.11.TS,2,mean)
mean.12 <- apply(OTS.12.TS,2,mean)
mean.13 <- apply(OTS.13.TS,2,mean)
#mean.14 <- apply(OTS.14.TS,2,mean)

mean.20 <- apply(OTS.20.TS,2,mean)
mean.21 <- apply(OTS.21.TS,2,mean)
mean.22 <- apply(OTS.22.TS,2,mean)
mean.23 <- apply(OTS.23.TS,2,mean)
#mean.24 <- apply(OTS.24.TS,2,mean)

mean.30 <- apply(OTS.30.TS,2,mean)
mean.31 <- apply(OTS.31.TS,2,mean)
mean.32 <- apply(OTS.32.TS,2,mean)
mean.33 <- apply(OTS.33.TS,2,mean)
#mean.34 <- apply(OTS.34.TS,2,mean)

#mean.40 <- apply(OTS.40.TS,2,mean)
#mean.41 <- apply(OTS.41.TS,2,mean)
#mean.42 <- apply(OTS.42.TS,2,mean)
#mean.43 <- apply(OTS.43.TS,2,mean)
#mean.44 <- apply(OTS.44.TS,2,mean)

sd.00 <- apply(OTS.00.TS,2,sd)
sd.01 <- apply(OTS.01.TS,2,sd)
sd.02 <- apply(OTS.02.TS,2,sd)
sd.03 <- apply(OTS.03.TS,2,sd)
#sd.04 <- apply(OTS.04.TS,2,sd)

sd.10 <- apply(OTS.10.TS,2,sd)
sd.11 <- apply(OTS.11.TS,2,sd)
sd.12 <- apply(OTS.12.TS,2,sd)
sd.13 <- apply(OTS.13.TS,2,sd)
#sd.14 <- apply(OTS.14.TS,2,sd)

sd.20 <- apply(OTS.20.TS,2,sd)
sd.21 <- apply(OTS.21.TS,2,sd)
sd.22 <- apply(OTS.22.TS,2,sd)
sd.23 <- apply(OTS.23.TS,2,sd)
#sd.24 <- apply(OTS.24.TS,2,sd)

sd.30 <- apply(OTS.30.TS,2,sd)
sd.31 <- apply(OTS.31.TS,2,sd)
sd.32 <- apply(OTS.32.TS,2,sd)
sd.33 <- apply(OTS.33.TS,2,sd)
#sd.34 <- apply(OTS.34.TS,2,sd)

#sd.40 <- apply(OTS.40.TS,2,sd)
#sd.41 <- apply(OTS.41.TS,2,sd)
#sd.42 <- apply(OTS.42.TS,2,sd)
#sd.43 <- apply(OTS.43.TS,2,sd)
#sd.44 <- apply(OTS.44.TS,2,sd)

global.TS.00 <- rep(NA,sims)
global.TS.01 <- rep(NA,sims)
global.TS.02 <- rep(NA,sims)
global.TS.03 <- rep(NA,sims)
#global.TS.04 <- rep(NA,sims)

global.TS.10 <- rep(NA,sims)
global.TS.11 <- rep(NA,sims)
global.TS.12 <- rep(NA,sims)
global.TS.13 <- rep(NA,sims)
#global.TS.13 <- rep(NA,sims)

global.TS.20 <- rep(NA,sims)
global.TS.21 <- rep(NA,sims)
global.TS.22 <- rep(NA,sims)
global.TS.23 <- rep(NA,sims)
#global.TS.23 <- rep(NA,sims)

global.TS.30 <- rep(NA,sims)
global.TS.31 <- rep(NA,sims)
global.TS.32 <- rep(NA,sims)
global.TS.33 <- rep(NA,sims)
#global.TS.33 <- rep(NA,sims)

#global.TS.40 <- rep(NA,sims)
#global.TS.41 <- rep(NA,sims)
#global.TS.42 <- rep(NA,sims)
#global.TS.43 <- rep(NA,sims)
#global.TS.43 <- rep(NA,sims)

for(i in 1:sims){
  global.TS.00[i] <- max((OTS.00.TS[i,]-mean.00)/sd.00)
  global.TS.01[i] <- max((OTS.01.TS[i,]-mean.01)/sd.01)
  global.TS.02[i] <- max((OTS.02.TS[i,]-mean.02)/sd.02)
  global.TS.03[i] <- max((OTS.03.TS[i,]-mean.03)/sd.03)
  #global.TS.04[i] <- max((OTS.04.TS[i,]-mean.04)/sd.04)
  
  global.TS.10[i] <- max((OTS.10.TS[i,]-mean.10)/sd.10)
  global.TS.11[i] <- max((OTS.11.TS[i,]-mean.11)/sd.11)
  global.TS.12[i] <- max((OTS.12.TS[i,]-mean.12)/sd.12)
  global.TS.13[i] <- max((OTS.13.TS[i,]-mean.13)/sd.13)
  #global.TS.14[i] <- max((OTS.14.TS[i,]-mean.14)/sd.14)
  
  global.TS.20[i] <- max((OTS.20.TS[i,]-mean.20)/sd.20)
  global.TS.21[i] <- max((OTS.21.TS[i,]-mean.21)/sd.21)
  global.TS.22[i] <- max((OTS.22.TS[i,]-mean.22)/sd.22)
  global.TS.23[i] <- max((OTS.23.TS[i,]-mean.23)/sd.23)
  #global.TS.24[i] <- max((OTS.24.TS[i,]-mean.24)/sd.24)
  
  global.TS.30[i] <- max((OTS.30.TS[i,]-mean.30)/sd.30)
  global.TS.31[i] <- max((OTS.31.TS[i,]-mean.31)/sd.31)
  global.TS.32[i] <- max((OTS.32.TS[i,]-mean.32)/sd.32)
  global.TS.33[i] <- max((OTS.33.TS[i,]-mean.33)/sd.33)
  #global.TS.34[i] <- max((OTS.34.TS[i,]-mean.34)/sd.34)
  
  #global.TS.40[i] <- max((OTS.40.TS[i,]-mean.40)/sd.40)
  #global.TS.41[i] <- max((OTS.41.TS[i,]-mean.41)/sd.41)
  #global.TS.42[i] <- max((OTS.42.TS[i,]-mean.42)/sd.42)
  #global.TS.43[i] <- max((OTS.43.TS[i,]-mean.43)/sd.43)
  #global.TS.44[i] <- max((OTS.44.TS[i,]-mean.44)/sd.44)
}

global.TS <- rep(NA,sims)
for(i in 1:sims){
  ###Use this for 4 categories
  global.TS[i] = max(c((OTS.00.TS[i,]-mean.00)/sd.00,(OTS.01.TS[i,]-mean.01)/sd.01,(OTS.02.TS[i,]-mean.02)/sd.02,(OTS.03.TS[i,]-mean.03)/sd.03,
                       (OTS.10.TS[i,]-mean.10)/sd.10,(OTS.11.TS[i,]-mean.11)/sd.11,(OTS.12.TS[i,]-mean.12)/sd.12,(OTS.13.TS[i,]-mean.13)/sd.13,
                       (OTS.20.TS[i,]-mean.20)/sd.20,(OTS.21.TS[i,]-mean.21)/sd.21,(OTS.22.TS[i,]-mean.22)/sd.22,(OTS.23.TS[i,]-mean.23)/sd.23,
                       (OTS.30.TS[i,]-mean.30)/sd.30,(OTS.31.TS[i,]-mean.31)/sd.31,(OTS.32.TS[i,]-mean.32)/sd.32,(OTS.33.TS[i,]-mean.33)/sd.33))
  ###Use this for 5 categories
  #global.TS[i] = max(c((OTS.00.TS[i,]-mean.00)/sd.00,(OTS.01.TS[i,]-mean.01)/sd.01,(OTS.02.TS[i,]-mean.02)/sd.02,(OTS.03.TS[i,]-mean.03)/sd.03,(OTS.04.TS[i,]-mean.04)/sd.04,
                       #(OTS.10.TS[i,]-mean.10)/sd.10,(OTS.11.TS[i,]-mean.11)/sd.11,(OTS.12.TS[i,]-mean.12)/sd.12,(OTS.13.TS[i,]-mean.13)/sd.13,(OTS.14.TS[i,]-mean.14)/sd.14,
                       #(OTS.20.TS[i,]-mean.20)/sd.20,(OTS.21.TS[i,]-mean.21)/sd.21,(OTS.22.TS[i,]-mean.22)/sd.22,(OTS.23.TS[i,]-mean.23)/sd.23,(OTS.24.TS[i,]-mean.24)/sd.24,
                       #(OTS.30.TS[i,]-mean.30)/sd.30,(OTS.31.TS[i,]-mean.31)/sd.31,(OTS.32.TS[i,]-mean.32)/sd.32,(OTS.33.TS[i,]-mean.33)/sd.33)(OTS.34.TS[i,]-mean.34)/sd.34,
                       #(OTS.40.TS[i,]-mean.30)/sd.40,(OTS.41.TS[i,]-mean.41)/sd.41,(OTS.42.TS[i,]-mean.42)/sd.42,(OTS.43.TS[i,]-mean.43)/sd.43)(OTS.44.TS[i,]-mean.44)/sd.44)
}



#UB.ij is an n.rad dimensional vector containing the critical value for the test statistic detecting clustering of category j surrounding category i at each individual radius
UB.00 <- apply(OTS.00.TS,2,quantile,prob = ub)
UB.01 <- apply(OTS.01.TS,2,quantile,prob = ub)
UB.02 <- apply(OTS.02.TS,2,quantile,prob = ub)
UB.03 <- apply(OTS.03.TS,2,quantile,prob = ub)
#UB.04 <- apply(OTS.04.TS,2,quantile,prob = ub)

UB.10 <- apply(OTS.10.TS,2,quantile,prob = ub)
UB.11 <- apply(OTS.11.TS,2,quantile,prob = ub)
UB.12 <- apply(OTS.12.TS,2,quantile,prob = ub)
UB.13 <- apply(OTS.13.TS,2,quantile,prob = ub)
#UB.14 <- apply(OTS.14.TS,2,quantile,prob = ub)

UB.20 <- apply(OTS.20.TS,2,quantile,prob = ub)
UB.21 <- apply(OTS.21.TS,2,quantile,prob = ub)
UB.22 <- apply(OTS.22.TS,2,quantile,prob = ub)
UB.23 <- apply(OTS.23.TS,2,quantile,prob = ub)
#UB.24 <- apply(OTS.24.TS,2,quantile,prob = ub)

UB.30 <- apply(OTS.30.TS,2,quantile,prob = ub)
UB.31 <- apply(OTS.31.TS,2,quantile,prob = ub)
UB.32 <- apply(OTS.32.TS,2,quantile,prob = ub)
UB.33 <- apply(OTS.33.TS,2,quantile,prob = ub)
#UB.34 <- apply(OTS.34.TS,2,quantile,prob = ub)

#UB.40 <- apply(OTS.40.TS,2,quantile,prob = ub)
#UB.41 <- apply(OTS.41.TS,2,quantile,prob = ub)
#UB.42 <- apply(OTS.42.TS,2,quantile,prob = ub)
#UB.43 <- apply(OTS.43.TS,2,quantile,prob = ub)
#UB.44 <- apply(OTS.44.TS,2,quantile,prob = ub)

UB.global.00 <- quantile(global.TS.00,ub)
UB.global.01 <- quantile(global.TS.01,ub)
UB.global.02 <- quantile(global.TS.02,ub)
UB.global.03 <- quantile(global.TS.03,ub)
#UB.global.04 <- quantile(global.TS.04,ub)

UB.global.10 <- quantile(global.TS.10,ub)
UB.global.11 <- quantile(global.TS.11,ub)
UB.global.12 <- quantile(global.TS.12,ub)
UB.global.13 <- quantile(global.TS.13,ub)
#UB.global.14 <- quantile(global.TS.14,ub)

UB.global.20 <- quantile(global.TS.20,ub)
UB.global.21 <- quantile(global.TS.21,ub)
UB.global.22 <- quantile(global.TS.22,ub)
UB.global.23 <- quantile(global.TS.23,ub)
#UB.global.24 <- quantile(global.TS.24,ub)

UB.global.30 <- quantile(global.TS.30,ub)
UB.global.31 <- quantile(global.TS.31,ub)
UB.global.32 <- quantile(global.TS.32,ub)
UB.global.33 <- quantile(global.TS.33,ub)
#UB.global.34 <- quantile(global.TS.34,ub)

#UB.global.40 <- quantile(global.TS.40,ub)
#UB.global.41 <- quantile(global.TS.41,ub)
#UB.global.42 <- quantile(global.TS.42,ub)
#UB.global.43 <- quantile(global.TS.43,ub)
#UB.global.44 <- quantile(global.TS.44,ub)

#results is a K by K matrix with entry in row i and column j equal to the critical value for the global test over all radii of the test statistic testing for clustering of category j surrounding category i
results <- rbind(c(UB.global.00,UB.global.01,UB.global.02,UB.global.03),
                 c(UB.global.10,UB.global.11,UB.global.12,UB.global.13),
                 c(UB.global.20,UB.global.21,UB.global.22,UB.global.23),
                 c(UB.global.30,UB.global.31,UB.global.32,UB.global.33))
#For K = 5
#results <- rbind(c(UB.global.00,UB.global.01,UB.global.02,UB.global.03,UB.global.04),
#c(UB.global.10,UB.global.11,UB.global.12,UB.global.13,UB.global.14),
#c(UB.global.20,UB.global.21,UB.global.22,UB.global.23,UB.global.24),
#c(UB.global.30,UB.global.31,UB.global.32,UB.global.33,UB.global.34),
#c(UB.global.40,UB.global.41,UB.global.42,UB.global.43,UB.global.44))

#UB global is the critical value for the global test statistic over all radii and all category pairs
UB.global <- quantile(global.TS,ub)

