library(HIEv)
library(tidyr)
library(doBy)
# set download path
setToPath('download')


if(!file.exists('c:/hiev/token.txt')){
  stop('token needs to be in c:/hiev/token.txt')
}else{
  setToken()
}


# function to download vwv for drigrass
get.vwc.dg.func <- function(s.date,e.date,var.in = 'VWSOIL',probe.long.infor.df){
  # creat a vector of end day of months
  date.vec <- seq(s.date,e.date,by='month')-1
  
  tmp.ls <- list()
  
  # read vwc
  for (i in seq_along(date.vec)){
    
    date.date <- ceiling_date(date.vec[i], "month") - 1

    # creat file name
    fn <- sprintf('MROS_AUTO_%s_R_%s.dat',
                  var.in,
                  format(date.date,'%Y%m%d'))
    
    vwc.tmp.df <- try(downloadTOA5(fn))
    
    if(class(vwc.tmp.df) != 'try-error' & var.in == 'VWSOIL'){
      # get daily mean
      vwc.tmp.df$date <- as.Date(vwc.tmp.df$DateTime)
      vwc.sum.df <- summaryBy(.~date,data = vwc.tmp.df,FUN=mean,na.rm=T,keep.names = T)
      #conver to long format 
      vwc.sum.df.long <- gather(vwc.sum.df,'probe.nm','vwc','VW_Avg.1.':'VW_Avg.48.')
      vwc.sum.df.long <- vwc.sum.df.long[,c('date','probe.nm','vwc')]
      vwc.sum.df.long$probe.nm <- as.numeric(gsub("VW_Avg.",'', vwc.sum.df.long$probe.nm))
      
      # 
      dg.vwc.df <- merge(vwc.sum.df.long,probe.long.infor.df,all = T)
      
      dg.vwc.df.mean <- summaryBy(vwc ~ date + plot.nm + treatment,
                                  data = dg.vwc.df,
                                  FUN=mean,na.rm=T,keep.names = T) 
      
      
      tmp.ls[[i]] <- dg.vwc.df.mean
      # plot(vwc~date,data = dg.vwc.df.mean[dg.vwc.df.mean$plot.nm == 2,])
    }else if (class(vwc.tmp.df) == 'try-error'){
      
      tmp.ls[[i]] <- NA
    }else{
      tmp.ls[[i]] <- vwc.tmp.df
    }

  }
  
  out.df <- do.call(rbind,tmp.ls)
  
  out.nm <- paste0('drigrass_',var.in,'_',
                   format(s.date,'%Y%m%d'),format(e.date,'%Y%m%d'),'.rds')
  saveRDS(out.df,out.nm)
}
