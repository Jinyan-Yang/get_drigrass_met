source('r/load.R')

# read probe info
probe.info.df <- read.csv('probe_info.csv',stringsAsFactors = F)
tmp.1 <- probe.info.df[,c( "Plot..", "Treatment","Sensors", "Sensor.1...South...East.Corner" )]
names(tmp.1) <- c('plot.nm','treatment','sensor','probe.nm')

tmp.2 <- probe.info.df[,c( "Plot..", "Treatment","Sensors","Sensor.2...North..West.Corner" )]
names(tmp.2) <- c('plot.nm','treatment','sensor','probe.nm')

probe.long.infor.df <- rbind(tmp.1,tmp.2)

# download vwc data####

s.date = as.Date('2019-8-9')
e.date = as.Date('2020-6-30')

get.vwc.dg.func(s.date,e.date,probe.long.infor.df)
get.vwc.dg.func(s.date = s.date,
                e.date = e.date,
                probe.long.infor.df = probe.long.infor.df,var.in = 'AIRVARS')

get.vwc.dg.func(s.date = s.date,
                e.date = e.date,
                probe.long.infor.df = probe.long.infor.df,var.in = 'IRRIG')

dg.tair.df <- readRDS('drigrass_AIRVARS_2019080920200630.rds')
dg.irrig.df <- readRDS('drigrass_IRRIG_2019080920200630.rds')

write.csv(dg.tair.df,'cache/driGrass_tair_Par.csv',row.names = F)

# write.csv(dg.irrig.df,'cache/driGrass_tair_Par.csv',row.names = F)
# save a csv copy
dg.swc.df <- readRDS('drigrass_vwc_2019050120201101.rds')
write.csv(dg.swc.df,'drigrass_vwc_2019050120201101.csv',row.names = F)