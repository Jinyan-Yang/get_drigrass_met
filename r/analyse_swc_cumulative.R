library(doBy)
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
# read in swc  data
dg.swc.df <- readRDS('drigrass_vwc_2019050120201101.rds')
# quality control
dg.swc.df$vwc[dg.swc.df$vwc<0.01] <- NA
# get real treatment name
treat.info.df <- read.csv('TreatmentMaster.csv')
treat.info.df <- treat.info.df[,c('DRIGrassCode','Treatment')]
treat.info.df <- treat.info.df[!duplicated(treat.info.df),]
# give the swc data the actual treatment names
dg.swc.df <- merge(dg.swc.df,treat.info.df,all.x = T,
                   by.x = 'treatment',by.y='DRIGrassCode')

dg.swc.df <- dg.swc.df[dg.swc.df$Treatment %in% 
                         c('Ambient','Drought','Frequency','ShelterControl'),]

dg.swc.df$Treatment <- droplevels(dg.swc.df$Treatment)

# subset by date
dg.swc.df <- dg.swc.df[dg.swc.df$date>as.Date('2019-8-9')&
                         dg.swc.df$date<as.Date('2020-6-30'),]

# get treatment sum
drigrass.vwc.sum.df.treat <- summaryBy(vwc~Treatment,
                                       data = dg.swc.df,
                                       FUN=c(mean),na.rm=T)
drigrass.vwc.sum.df.treat <- drigrass.vwc.sum.df.treat[!is.na(drigrass.vwc.sum.df.treat$Treatment),]

drigrass.vwc.sum.df.treat$Treatment <- droplevels(drigrass.vwc.sum.df.treat$Treatment)

drigrass.vwc.sum.df<- summaryBy(vwc~Treatment+date,
                                data = dg.swc.df,
                                FUN=c(mean),na.rm=T)
drigrass.vwc.sum.df$Treatment <- droplevels(drigrass.vwc.sum.df$Treatment)
# do anova
library(agricolae)
hsd=HSD.test(aov(vwc.mean~Treatment,data = drigrass.vwc.sum.df), "Treatment", group=T)
summary(aov(vwc~Treatment,data = dg.swc.df))


# make plot in pdf
pdf('swc_cumu_treat_newDate.pdf',width = 8,height = 8*.618)
palette(col.df$auLandscape[c(2,4,5,1)])
# box plot
daysInyear <- 365.25#convert daily mean vwc to annual sum
boxplot((vwc.mean*daysInyear)~Treatment,data = drigrass.vwc.sum.df,ylim=c(20,100),
        xlab = 'Treatment',ylab='Annual cumulative soil moisture',
        col=palette())
# add significance
text(x = seq_along(drigrass.vwc.sum.df.treat$Treatment),
       y = rep(100,nrow(drigrass.vwc.sum.df.treat)),
       labels = as.character(hsd$groups$groups),bty='n')
# points(x = seq_along(drigrass.vwc.sum.df.treat$Treatment),
#        y = rep(100,nrow(drigrass.vwc.sum.df.treat)),
#        pch =as.character(hsd$groups$groups))
dev.off()
