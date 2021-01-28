if(!file.exists('drigrass_vwc_2019050120201101.rds')){
  source('r/get_vwc.R')
}
library(lubridate)
# make figure for all plots
dg.swc.df <- readRDS('drigrass_vwc_2019050120201101.rds')

dg.swc.df <- dg.swc.df[order(dg.swc.df$treatment),]

# 
pdf('drigrass vwc.pdf',width = 8,height = 12*0.618)
par(mar=c(5,5,1,1),
    mfrow = c(3,2))
plot.vec <- unique(dg.swc.df$plot.nm)
for (i in seq_along(plot.vec)){
  plot.df <- dg.swc.df[dg.swc.df$plot.nm == plot.vec[i],]
  treat.in <- unique(dg.swc.df$treatment[dg.swc.df$plot.nm == plot.vec[i]])
  
  if(!is.na(mean(plot.df$vwc,na.rm = T)) ){
    plot(vwc~date,data = plot.df,
         xlab='',ylab='VWC',
         xaxt='n',
         main = paste0(plot.vec[i],' ',treat.in),type='s',col='navy')
    
    # add proper date axis
    date.range = range(plot.df$date,na.rm=T)
    mons.vec =  seq(date.range[1],date.range[2],by='mon')
    
    mon.c <- format(mons.vec,'%m')
    axis(1,at = mons.vec,labels = mon.c)
    yr.vec <- unique(year(plot.df$date))
    where.c <-which(mon.c =='01') / length(mon.c)
    num.yr <- length(where.c)
    mtext(yr.vec[(length(yr.vec) - num.yr + 1):length(yr.vec)],side = 1,adj = where.c,line = 3)
  }

}
dev.off()


# get vwc by each treatment
library(doBy)
dg.swc.df <- dg.swc.df[!is.na(dg.swc.df$date),]
dg.swc.df <- dg.swc.df[!is.na(dg.swc.df$vwc),]
dg.swc.df$vwc[dg.swc.df$vwc<0] <- 0
dg.swc.df$treatment <- as.factor(dg.swc.df$treatment)
dg.swc.treat.df <- summaryBy(vwc~date + treatment, data = dg.swc.df,FUN=c(mean,sd,length)) 

dg.swc.treat.df$se <- with(dg.swc.treat.df,vwc.sd/sqrt(vwc.length))

# make a figure
pdf('drigrass vwc by treatment.pdf',width = 4*2,height = 4*5*0.618)
par(mar=c(5,5,1,1),
    mfrow = c(5,2))
plot.vec <- levels(dg.swc.treat.df$treatment)
for (i in seq_along(plot.vec)){
  plot.df <- dg.swc.treat.df[dg.swc.treat.df$treatment == plot.vec[i],]
  treat.in <- plot.vec[i]
  
 
    plot(vwc.mean~date,data = plot.df,
         xlab='',ylab='VWC',
         xaxt='n',
         main = treat.in,type='s',col='navy')
    points(c(vwc.mean + vwc.sd)~date,data = plot.df,type='l',lty='dashed',col='grey')
    points(c(vwc.mean - vwc.sd)~date,data = plot.df,type='l',lty='dashed',col='grey')
    # add proper date axis
    date.range = range(plot.df$date,na.rm=T)
    mons.vec =  seq(date.range[1],date.range[2],by='mon')
    
    mon.c <- format(mons.vec,'%m')
    axis(1,at = mons.vec,labels = mon.c)
    yr.vec <- unique(year(plot.df$date))
    where.c <-which(mon.c =='01') / length(mon.c)
    num.yr <- length(where.c)
    mtext(yr.vec[(length(yr.vec) - num.yr + 1):length(yr.vec)],side = 1,adj = where.c,line = 3)

  
}
dev.off()
