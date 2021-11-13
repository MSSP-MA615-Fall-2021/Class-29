
## https://cran.r-project.org/web/packages/gstat/index.html
## 

library(gstat)

library(moments)
library(raster)

# dataFolder<-"Data_GWR/"
# COUNTY<-shapefile(paste0(dataFolder,"COUNTY_ATLANTIC.shp"))
# state<-shapefile(paste0(dataFolder,"STATE_ATLANTIC.shp"))
# df<-read.csv(paste0(dataFolder,"data_atlantic_1998_2012.csv"), header=T)

dataFolder<-"DATA_08/"
train<-read.csv(paste0(dataFolder,"train_data.csv"), header= TRUE)

coordinates(train) = ~x+y


v.cloud<-variogram(SOC~ 1, data = train, cloud=T)
head(v.cloud)

plot(v.cloud, main = "Variogram cloud", xlab = "Separation distance (m)")

## assuming isotropic

v<-variogram(SOC~ 1, data = train, cloud=F)

v

plot(v, main = "Variogram - default", xlab = "Separation distance (m)")

## cut-off

v.cut<-variogram(SOC ~ 1, train, cutoff=500000, width=500000/20)

plot(v.cut, main = "Variogram with cutoff and fixed width", xlab = "Separation distance (m)")

## anisotropic variogram

v.map<-variogram(SOC ~ 1, train, map = TRUE, cutoff=600000, width=600000/17)


plot(v.map, col.regions = bpy.colors(64),
     main="Variogram Map",
     xlab="x",
     ylab="y")


## Directional Variograms

plot(variogram(SOC ~ 1, train, 
               alpha = c(30, 120),
               cutoff = 600000),  
     main = "Directional Variograms, SOC",
     sub = "Azimuth 30N (left), 120N (right)", 
     pch = 20, col = "blue")



show.vgms()



## Exponential (Exp) model

# Intial parameter set by eye esitmation
m.exp<-vgm(25,"Exp",25000,10)
# least square fit
m.exp.f<-fit.variogram(v, m.exp)
m.exp.f



plot(v, pl=F, model=m.exp.f,col="black", cex=1, lwd=0.5,lty=1,pch=20,
     main="Variogram models - Exponential",xlab="Distance (m)",ylab="Semivariance")


## Spherical (Sph) model

# Intial parameter set by eye esitmation
m.sph<-vgm(25,"Sph",40000,10)
# least square fit
m.sph.f<-fit.variogram(v, m.sph)
m.sph.f
##   model     psill    range
## 1   Nug  9.170219     0.00
## 2   Sph 13.482728 55366.02

plot(v, pl=F, model=m.sph.f,col="black", cex=1, lwd=0.5,lty=1,pch=20,
     main="Variogram models - Spherical",xlab="Distance (m)",ylab="Semivariance")



## Goodness-of-fit


attributes(m.exp.f)$SSErr
## [1] 2.561876e-07
attributes(m.sph.f)$SSErr
## [1] 6.223063e-06


fit.variogram(v, vgm(c("Exp", "Sph", "Mat")))
##   model    psill    range
## 1   Nug 15.12586      0.0
## 2   Exp 12.20305 148458.6



## Variogram Modeling with Transformed Data

hist(train$SOC,
     main= "Distribution of SOC")


skewness(train$SOC) 

## [1] 1.412073



shapiro.test(train$SOC)
## 
##  Shapiro-Wilk normality test
## 
## data:  train$SOC
## W = 0.88057, p-value = 2.7e-16













