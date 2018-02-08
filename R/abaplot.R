##### Create Abacus plot from raw passive acoustic telemetry detection data
## Last Edited: 2018-02-07 (VU)
##
## Function to create abacus plot to visualise daily detection patterns in multiple
## tagged animals

abaplot<-function(data, id, startwindow=NULL, endwindow=NULL, tagdate=NULL, taglife=NULL, mar=c(5,6,1,1), xlab="Date", ylab="Tag", ggplot=TRUE, RI=TRUE, pch=20, plotbat=FALSE, ...){
  ####################################################################################
  ## data            data frame containing passive telemetry dataset; VEMCO field naming accepted 
  ## id              field in 'data' with unique individual tag/animal identifier
  ## startwindow     start date for x axis (format "yyyy-mm-dd"); if NULL (default) all data is plotted
  ## endwindow       end date for x axis (format "yyyy-mm-dd"); if NULL (default) all data is plotted
  ## plotbat         plot points indicating battery life of transmitter; if tagdate and taglife values 
  ##                  provided, else first and last date of detection assumed (default = FALSE)
  ## tagdate         vector of dates each tag was released; if NULL (default) first date of detection
  ##                  is presumed to be date tagged
  ## taglife         battery life of tag (in days); if NULL (default) last date of detection
  ##                  is presumed to be end of battery life
  ## ggplot          use ggplot2 for plotting function
  ## RI              calculate and print detection summary and Residency Index (RI) table
  ## ...             additional characteristics of plotting function
  ####################################################################################
  sapply(c("lubridate","plyr"), require, character.only=TRUE)
  
  data$dt<-ymd_hms(data[,grep("Date",colnames(data))])
  data[,id]<-droplevels(as.factor(data[,id]))
  
  ## Define ploting window
  if(is.null(startwindow)){x1=min(as.Date(data$dt))}else{x1=as.Date(startwindow)}
  if(is.null(endwindow)){x2=max(as.Date(data$dt))}else{x2=as.Date(endwindow)}
  xlim<-c(x1,x2)
  ### Define start and end of tag battery life
  if(is.null(tagdate)){startdate=ddply(data, id, function(x) startdate=as.Date(min(x$dt)))$V1}else{startdate<-tagdate}
  if(is.null(taglife)){enddate=ddply(data, id, function(x) enddate=as.Date(max(x$dt)))$V1}else{enddate=startdate+taglife}
  
  ## Plot abacus
  if(ggplot){
    require(ggplot2)
    p<-ggplot(data, aes(as.Date(data$dt), data[,id]), environment = environment()) + geom_point(...) +
      xlab(xlab) + ylab(ylab) + scale_x_date(date_labels = "%b\n%Y", date_minor_breaks = "1 month")
      
    if(plotbat){
      print(p + geom_point(data=data.frame(tag=1:length(levels(data[,id])), startdate=startdate), aes(x = startdate, y = tag), pch="|", colour=8, size=5)+
              geom_point(data=data.frame(tag=1:length(levels(data[,id])), enddate=enddate), aes(x = enddate, y = tag), pch="|", colour=8, size=5))
    }else{
      print(p)
    }
  }else{
    par(mar=mar)
    plot(as.Date(data$dt),as.integer(data[,id]), xaxt="n", yaxt="n", xlab=xlab, ylab=ylab, pch=NA, xlim=xlim, ylim=extendrange(1:length(levels(data[,id])), f=0.1))
    axis(2, at=1:length(levels(data[,id])) , labels=levels(data[,id]), las=1, cex.axis=0.8); axis(2, at=1:length(levels(data[,id])) , labels=F , tck=1, col=8)
    points(as.Date(data$dt),as.integer(data[,id]), pch=pch, ...)
    axis.Date(1, at=seq(as.Date(format(min(data$dt), "%Y-%m-01"))-31,as.Date(format(max(data$dt), "%Y-%m-01"))+31, "months"), format='%b\n%Y', cex.axis=0.8, padj=0.2)
    axis.Date(1, at=seq(as.Date(format(min(data$dt), "%Y-%m-01"))-31,as.Date(format(max(data$dt), "%Y-%m-01"))+31, "weeks"),labels = FALSE, tcl = -0.3)
    axis.Date(1, at=seq(as.Date(format(min(data$dt), "%Y-%m-01"))-31,as.Date(format(max(data$dt), "%Y-%m-01"))+31, "days"), labels = FALSE, tcl = -0.15)
    box() 
    if(plotbat){
      points(startdate, 1:length(levels(data[,id])), pch="|", col=8)
      points(enddate, 1:length(levels(data[,id])), pch="|", col=8) 
    }
  }
  
  if(RI){
    dd<-ddply(data, id, function(x) c(Days.Detected=length(unique(as.Date(x$dt)))))
    dd$Days.at.Liberty<-as.numeric(difftime(enddate, startdate, "days"))
    dd$Residency.Index<-dd$Days.Detected/dd$Days.at.Liberty
    cat("Detection Summary Table:\n"); print(dd[rev(rownames(dd)),], row.names=F)
  }
}



