#processFutureWarming
library(plyr)

processData.processFutureWarming <- function(futureGDD, outfile, ...){
  
  genmom = futureGDD
  gdds = list()
  periods = list(1989:2014, 2040:2064, 2065:2089)
  starts = c(1989, 2040, 2065)
  
  for(i in 1:length(periods)){
    gdds[[i]] = ddply(genmom[genmom$year %in% periods[[i]], ], 'lakeid', function(df){
      
      data.frame(med_gdd=median(df$GDD_wtr_5c))
    })
    
    gdds[[i]]$period = paste0(range(periods[[i]]), collapse='-')
    gdds[[i]]$periodi = i
    
  }
  
  all_period_gdd = do.call(rbind, gdds)
  
  write.table(all_period_gdd, file=outfile, sep='\t', row.names=FALSE)
}

