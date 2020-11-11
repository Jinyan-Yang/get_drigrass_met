source('r/load.R')

# read probe info
probe.info.df <- read.csv('probe_info.csv',stringsAsFactors = F)
tmp.1 <- probe.info.df[,c( "Plot..", "Treatment","Sensors", "Sensor.1...South...East.Corner" )]
names(tmp.1) <- c('plot.nm','treatment','sensor','probe.nm')

tmp.2 <- probe.info.df[,c( "Plot..", "Treatment","Sensors","Sensor.2...North..West.Corner" )]
names(tmp.2) <- c('plot.nm','treatment','sensor','probe.nm')

probe.long.infor.df <- rbind(tmp.1,tmp.2)

# download vwc data####

s.date = as.Date('2019-5-1')
e.date = as.Date('2020-11-1')

get.vwc.dg.func(s.date,e.date,probe.long.infor.df)

# save a csv copy
dg.swc.df <- readRDS('drigrass_vwc_2019050120201101.rds')
write.csv(dg.swc.df,'drigrass_vwc_2019050120201101.csv',row.names = F)
# make plots
dg.swc.df <- readRDS('drigrass_vwc_2019050120201101.rds')

dg.swc.df <- dg.swc.df[order(dg.swc.df$treatment),]

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