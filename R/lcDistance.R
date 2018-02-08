#### Least cost path calculation using passive telemetry datasets
## Last Edited: 2018-02-08 (VU)
##
## Function to use short term center of activity positions (using COA.R) to calculate
## least cost paths around landmasses. Uses 'rworldmap' to construct
## cost raster if none is provided

lcDistance<-function(coa.data, cost=NULL, utm, res=0.005, directions=16, ...){
  ####################################################################################
  ## coa.data      data frame containing center of activity positions (output from COA.R)
  ## cost          cost raster, with land pixel values of 1000 and sea as 1, if none provided
  ##                this is extracted and calculated using 'rworldmap'
  ## utm           CRS object with local projected coordinate reference system (with "+units=m")
  ## res           resolution of cost layer (in degrees) calculated if not provided
  ## directions    number of directional axes to consider when calculating 
  ##                least cost path (options: 4, 8, 16 [default])
  ####################################################################################
  
  ## load required libraries and set up CRS for geographic projection
  sapply(c("lubridate","plyr","raster","gdistance","maps"), require, character.only=TRUE)
  ll<-CRS("+proj=longlat +datum=WGS84")
  
  ## Setup input data; convert to spatial object and transform to projection
  data<-coa.data
  data[,grep("Date",colnames(data))]<-ymd_hms(data[,grep("Date",colnames(data))])
  data$step<-0:(nrow(data)-1)
  coordinates(data)<-~Longitude.coa+Latitude.coa; projection(data)<-ll
  dat<-spTransform(data, utm)
  
  ## Extract landmass to calculate cost raster if not provided
  if(is.null(cost)){
    require("rworldmap")
    require("rworldxtra")
    tryCatch({
      poly<- suppressWarnings(crop(getMap(resolution="high"), extent(data)+0.5))
      coast.poly<-spTransform(poly, ll)
      cost.r<-rasterize(coast.poly, raster(extent(data)+0.5, res=res), 1000)
      cost.r[is.na(values(cost.r))]<-1
      cost.ras<-projectRaster(cost.r, crs=utm)
    }, error=function(e){message("Error: no land in sight!\nConsider Direct or Circuitous distances")})
    }else{
      cost.ras<-projectRaster(cost, crs=utm)
      }
  
  ## Produce transition matrices, and correct for distortion
  tryCatch({
    trCost <- transition(1/cost.ras, mean, directions=directions)
    trCost <- geoCorrection(trCost, type="c")
  }, error=function(e){message("Error in calculating Transition layer")})
  
  ## Calculate shortest path between sequence of detection steps
  tryCatch({
    traj<-list()
    for(i in 1:max(data$step)){
      origin<-dat[dat$step%in%(i-1),]; goal<-dat[dat$step%in%i,]
      traj[[i]]<-shortestPath(trCost, origin, goal, output="SpatialLines")
      data@data[i+1,"Distance_m"]<-as.numeric(costDistance(trCost, origin, goal))
      setTxtProgressBar(txtProgressBar(min=1, max=max(data$step), style=3), i)
    }
    trajectory<-spTransform(do.call(rbind, traj),ll) 
  }, error=function(e){message("Error in calculating least cost path")})
  
  ## return list output with step distances and spatial trajectory file
  out<-list(data=as.data.frame(data), lc.traj=trajectory)
  return(out)
}

