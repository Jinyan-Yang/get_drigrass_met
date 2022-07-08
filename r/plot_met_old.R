library(doBy)
library(tidyr)
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
# make colot transprent##############
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
cal.ci.func <- function(sd.in,size.in){
  1.96  *sd.in/ sqrt(size.in)
}

# read shelter info
# shelter.info.df <- read.csv('TreatmentMaster.csv')
# # 
# sl.con.plot.vec <- shelter.info.df$Plot[shelter.info.df$DRIGrassCode =='OC-']
# amb.con.plot.vec <- shelter.info.df$Plot[shelter.info.df$DRIGrassCode =='C-']
# drt.con.plot.vec <- shelter.info.df$Plot[shelter.info.df$DRIGrassCode =='R-']
# frq.con.plot.vec <- shelter.info.df$Plot[shelter.info.df$DRIGrassCode =='V-']

# read climate inputs
dg.tair.df <- readRDS('drigrass_AIRVARS_2019080920200630.rds')
dg.irrig.df <- readRDS('drigrass_IRRIG_2019080920200630.rds')
dg.swc.df <- readRDS('drigrass_vwc_2019050120201101.rds')
dg.swc.df$vwc[dg.swc.df$vwc<0.01] <- NA

# get real treatment name
treat.info.df.raw <- read.csv('TreatmentMaster.csv')
treat.info.df <- treat.info.df.raw[,c('DRIGrassCode','Treatment')]
treat.info.df <- treat.info.df[!duplicated(treat.info.df),]


# plot swc#### plot diff
# give the swc data the actual treatment names
dg.swc.df <- merge(dg.swc.df,treat.info.df,all.x = T,
                   by.x = 'treatment',by.y='DRIGrassCode')

dg.swc.df <- dg.swc.df[dg.swc.df$Treatment %in% 
                         c('Ambient','Drought','Frequency','ShelterControl'),]

dg.swc.df$Treatment <- as.factor(dg.swc.df$Treatment)

# subset by date
dg.swc.df <- dg.swc.df[dg.swc.df$date>as.Date('2019-8-9')&
                         dg.swc.df$date<as.Date('2020-6-30'),]

dg.swc.df.summary <- summaryBy(vwc~date + Treatment,data= dg.swc.df,
                               FUN=c(mean,sd,length))
dg.swc.df.summary$vwc.ci <- cal.ci.func(sd.in = dg.swc.df.summary$vwc.sd,
                                        size.in = dg.swc.df.summary$vwc.length)
dg.swc.df.summary <- dg.swc.df.summary[!is.na(dg.swc.df.summary$date),]

swc.sub.df <- dg.swc.df.summary#[dg.swc.df.summary$Treatment == 'Ambient',]

met.sub.df <- spread(data = swc.sub.df[,c('date','Treatment','vwc.mean')],
                     key = Treatment,value = vwc.mean)


pdf('dg_met_plot_new.pdf',width = 4,height = 3*.618*4)
par(mar=c(5,5,1,1),
    mfrow=c(3,1))
# plot together#####
# plot swc

# a.	Ambient- "green3"
# b.	Drought- "goldenrod4"
# c.	Frequency- "blue"


# palette(col.df$auLandscape[c(2,4,5,1)])
palette(c("green3","goldenrod4","blue"))

# 
par(mar=c(0,5,4,1))
# 
plot((Drought - Ambient)~date,data= met.sub.df,
     ylim=c(-0.1,0.1),type='l',xlab=' ',xaxt='n',
     ylab='Difference in soil moisture content',
     col = 2,lwd=3)

points((Frequency - Ambient)~date,
       data= met.sub.df,type='l',
       col = 3,lwd=3)

# points((ShelterControl - Ambient)~date,
#        data= met.sub.df,type='l',
#        col = 4,lwd=3)
abline(h=0,lty='dashed',col='grey',lwd=3)
legend('topright',
       legend = c('Ambient','Drought','Frequency'),
       col=palette(),
       pch=16,bty='n',horiz = T)
legend('topleft',legend = '(a)',bty='n')
# plot rainfall
dg.irrig.df.daily <- summaryBy(irrigsum~Date + currentplot,
                               data = dg.irrig.df,
                               FUN=sum,na.rm=T)
dg.rain.df <- merge(dg.irrig.df.daily,treat.info.df.raw,all.x = T,
                   by.x = 'currentplot',by.y='Plot')

dg.rain.df <- dg.rain.df[dg.rain.df$Treatment %in% 
                         c('Ambient','Drought','Frequency','ShelterControl'),]
dg.rain.df <- dg.rain.df[dg.rain.df$Comments=='Burned',]
dg.rain.df.sum <- summaryBy(irrigsum.sum ~ Date + Treatment,
                            data = dg.rain.df,
                            FUN=mean,na.rm=T)
dg.rain.df.sum.spread <- spread(data = dg.rain.df.sum,
                                key = Treatment,value = irrigsum.sum.mean)
# plot
# palette(col.df$auLandscape[c(4,5,1)])
par(mar=c(2,5,2,1))

plot(( Frequency- Ambient)~Date,data= dg.rain.df.sum.spread,
     ylim=c(-20,40),type='l',xlab=' ',xaxt='n',
     ylab=expression('Difference in rainfall'~(mm~d^-1)),
     col = 3,lwd=3)

points((Drought - Ambient)~Date,
       data= dg.rain.df.sum.spread,type='l',
       col = 2,lwd=3)

abline(h=0,lty='dashed',col='grey',lwd=3)
# legend('topright',
#        legend = c('Drought','Frequency'),
#        col=palette()[2:3],
#        pch=16,bty='n',horiz = F)
legend('topleft',legend = '(b)',bty='n')

# cum rainfall
dg.rain.df.sum.spread$cum.rain.amb <- cumsum(dg.rain.df.sum.spread$Ambient)
dg.rain.df.sum.spread$cum.rain.drt <- cumsum(dg.rain.df.sum.spread$Drought)
dg.rain.df.sum.spread$cum.rain.frq <- cumsum(dg.rain.df.sum.spread$Frequency)
# 
par(mar=c(4,5,0,1))

plot(cum.rain.amb~Date,data= dg.rain.df.sum.spread,
     type='l',xlab='2020',
     ylab='Cumulative rainfall (mm)',
     col = 1,lwd=3)

points(cum.rain.drt~Date,
       data= dg.rain.df.sum.spread,type='l',
       col = 2,lwd=3)

points(cum.rain.frq~Date,
       data= dg.rain.df.sum.spread,type='l',
       col = 3,lwd=3)

# legend('topright',
#        legend = c('Drought','Frequency'),
#        col=palette()[2:3],
#        pch=16,bty='n',horiz = T)
legend('topleft',legend = '(c)',bty='n')


# tair
dg.tair.df$t.amb.mean <- (dg.tair.df$Tair_Avg.1. + 
                            dg.tair.df$Tair_Avg.5. + 
                            dg.tair.df$Tair_Avg.6.)/3
dg.tair.df$t.os.mean <- (dg.tair.df$Tair_Avg.2. + 
                            dg.tair.df$Tair_Avg.3. + 
                            dg.tair.df$Tair_Avg.4.)/3
# plot
par(mar=c(0,5,4,1))

plot((t.amb.mean)~DateTime, 
     data = dg.tair.df,type='l',
     xlab=' ',xaxt='n',
     ylab=expression('Air temperature'~(degree*C)),col=1)

points((t.amb.mean - t.os.mean)~DateTime, 
     data = dg.tair.df,type='l',
     col=4)
legend('topleft',legend = '(a)',bty='n')
# par
dg.par.daily.df <- summaryBy(PPFD_Avg.1. + PPFD_Avg.2. + PPFD_Avg.3.+
                               PPFD_Avg.4. + PPFD_Avg.5. + PPFD_Avg.6.
                              ~Date,
                              data = dg.tair.df,
                              FUN=mean,na.rm=T,keep.names = T)

dg.par.daily.df$par.amb.mean <- (dg.par.daily.df$PPFD_Avg.1. + 
                                   dg.par.daily.df$PPFD_Avg.5. + 
                                   dg.par.daily.df$PPFD_Avg.6.)/3
dg.par.daily.df$par.os.mean <- (dg.par.daily.df$PPFD_Avg.2. + 
                                  dg.par.daily.df$PPFD_Avg.3. + 
                                  dg.par.daily.df$PPFD_Avg.4.)/3

dg.par.daily.df$par.amb.mean <- dg.par.daily.df$par.amb.mean  * 10e-6 * 3600 #* 24/ 4.57
dg.par.daily.df$par.os.mean <- dg.par.daily.df$par.os.mean  * 10e-6 * 3600
# plot
par(mar=c(4,5,0,1))
plot((par.os.mean)~Date, 
     data = dg.par.daily.df,type='l',
     xlab=2020,ylab=expression('PAR'~(MJ~m^-2~d^-1)),col=1)

points((par.os.mean - par.amb.mean)~Date, 
       data = dg.par.daily.df,
       type='l',col=4)
abline(h=0,lty='dashed',col='grey',lwd=3)
legend('topleft',legend = '(b)',bty='n')
#####
dev.off()

















#######plot separaterly######
# amb#
palette(col.df$auLandscape[c(2,4,5,1)])
plot(vwc.mean~date,data= swc.sub.df,
     ylim=c(0.03,0.3),type='l',xlab='2020',
     ylab='Soil moisture content',
     col = 1,lwd=3)
polygon(x = c(swc.sub.df$date,rev(swc.sub.df$date)),
        y = c(swc.sub.df$vwc.mean + swc.sub.df$vwc.ci,
              rev(c(swc.sub.df$vwc.mean - swc.sub.df$vwc.ci))),
        col = t_col(palette()[1],percent = 80),border = NA)
# drt
swc.sub.df <- dg.swc.df.summary[dg.swc.df.summary$Treatment == 'Drought',]
swc.sub.df <- swc.sub.df[complete.cases(swc.sub.df),]
points(vwc.mean~date,data= swc.sub.df,
       type='l',col=2,lwd=3)
polygon(x = c(swc.sub.df$date,rev(swc.sub.df$date)),
        y = c(swc.sub.df$vwc.mean + swc.sub.df$vwc.ci,
              rev(c(swc.sub.df$vwc.mean - swc.sub.df$vwc.ci))),
        col = t_col(palette()[2],percent = 80),border = NA)


# frq
swc.sub.df <- dg.swc.df.summary[dg.swc.df.summary$Treatment == 'Frequency',]
swc.sub.df <- swc.sub.df[!is.na(swc.sub.df$date),]
swc.sub.df <- swc.sub.df[complete.cases(swc.sub.df),]
points(vwc.mean~date,data= swc.sub.df,
       type='l',col=3,lwd=3)
polygon(x = c(swc.sub.df$date,rev(swc.sub.df$date)),
        y = c(swc.sub.df$vwc.mean + swc.sub.df$vwc.ci,
              rev(c(swc.sub.df$vwc.mean - swc.sub.df$vwc.ci))),
        col = t_col(palette()[3],percent = 80),border = NA)
# # shelter
# swc.sub.df <- dg.swc.df.summary[dg.swc.df.summary$Treatment == 'ShelterControl',]
# swc.sub.df <- swc.sub.df[!is.na(swc.sub.df$date),]
# swc.sub.df <- swc.sub.df[complete.cases(swc.sub.df),]
# points(vwc.mean~date,data= swc.sub.df,
#        type='l',col=4,lwd=3)
# polygon(x = c(swc.sub.df$date,rev(swc.sub.df$date)),
#         y = c(swc.sub.df$vwc.mean + swc.sub.df$vwc.ci,
#               rev(c(swc.sub.df$vwc.mean - swc.sub.df$vwc.ci))),
#         col = t_col(palette()[4],percent = 80),border = NA)
legend('topleft',
       legend = c('Ambient','Drought','Frequency'),
       col=palette(),
       pch=16,bty='n',horiz = T)

# plot rain####
# give the swc data the actual treatment names
dg.irrig.df.daily <- summaryBy(irrigsum~Date + currentplot,
                               data = dg.irrig.df,
                               FUN=sum,na.rm=T)
dg.swc.df <- merge(dg.irrig.df.daily,treat.info.df.raw,all.x = T,
                   by.x = 'currentplot',by.y='Plot')

dg.swc.df <- dg.swc.df[dg.swc.df$Treatment %in% 
                         c('Ambient','Drought','Frequency','ShelterControl'),]

dg.swc.df$Treatment <- as.factor(dg.swc.df$Treatment)

# subset by date
dg.swc.df <- dg.swc.df[dg.swc.df$date>as.Date('2019-8-9')&
                         dg.swc.df$date<as.Date('2020-6-30'),]

dg.swc.df.summary <- summaryBy(vwc~date + Treatment,data= dg.swc.df,
                               FUN=c(mean,sd,length))
dg.swc.df.summary$vwc.ci <- cal.ci.func(sd.in = dg.swc.df.summary$vwc.sd,
                                        size.in = dg.swc.df.summary$vwc.length)
dg.swc.df.summary <- dg.swc.df.summary[!is.na(dg.swc.df.summary$date),]

swc.sub.df <- dg.swc.df.summary[dg.swc.df.summary$Treatment == 'Ambient',]

# amb
palette(col.df$auLandscape[c(2,4,5,1)])
plot(vwc.mean~date,data= swc.sub.df,
     ylim=c(0.03,0.3),type='l',xlab='2020',
     ylab='Soil moisture content',
     col = 1,lwd=3)
polygon(x = c(swc.sub.df$date,rev(swc.sub.df$date)),
        y = c(swc.sub.df$vwc.mean + swc.sub.df$vwc.ci,
              rev(c(swc.sub.df$vwc.mean - swc.sub.df$vwc.ci))),
        col = t_col(palette()[1],percent = 80),border = NA)
# drt
swc.sub.df <- dg.swc.df.summary[dg.swc.df.summary$Treatment == 'Drought',]
swc.sub.df <- swc.sub.df[complete.cases(swc.sub.df),]
points(vwc.mean~date,data= swc.sub.df,
       type='l',col=2,lwd=3)
polygon(x = c(swc.sub.df$date,rev(swc.sub.df$date)),
        y = c(swc.sub.df$vwc.mean + swc.sub.df$vwc.ci,
              rev(c(swc.sub.df$vwc.mean - swc.sub.df$vwc.ci))),
        col = t_col(palette()[2],percent = 80),border = NA)


# frq
swc.sub.df <- dg.swc.df.summary[dg.swc.df.summary$Treatment == 'Frequency',]
swc.sub.df <- swc.sub.df[!is.na(swc.sub.df$date),]
swc.sub.df <- swc.sub.df[complete.cases(swc.sub.df),]
points(vwc.mean~date,data= swc.sub.df,
       type='l',col=3,lwd=3)
polygon(x = c(swc.sub.df$date,rev(swc.sub.df$date)),
        y = c(swc.sub.df$vwc.mean + swc.sub.df$vwc.ci,
              rev(c(swc.sub.df$vwc.mean - swc.sub.df$vwc.ci))),
        col = t_col(palette()[3],percent = 80),border = NA)
legend('topleft',legend = c('Ambient','Drought','Frequency'),col=palette(),
       pch=16,bty='n',horiz = T)
