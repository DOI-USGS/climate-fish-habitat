
library(RColorBrewer)
library(plyr)

visualizeData.visualizeFutureWarming <- function(processedFutureWarming, outfile, ...){

  all_period_gdd = processedFutureWarming
  lakeids = c('2331600', '805400','146100') #Exemplar lakes: Trout Lake, Mendota, Green Lake
  periods = list(1989:2014, 2040:2064, 2065:2089)
  starts = sapply(periods, min)
  cols = RColorBrewer::brewer.pal(3, 'Set1')
  
  # 
  # 
  # 
  # genmom = read.table('d:/test/newGretchenFig/GENMOM_all_habitat.tsv', sep='\t', header=TRUE)
  # 
  # gdds = list()
  # 
  # for(i in 1:length(periods)){
  #   gdds[[i]] = ddply(genmom[genmom$year %in% periods[[i]], ], 'lakeid', function(df){
  # 
  #     data.frame(med_gdd=median(df$GDD_wtr_5c))
  #   })
  # 
  #   gdds[[i]]$period = paste0(range(periods[[i]]), collapse='-')
  #   gdds[[i]]$periodi = i
  # 
  # }
  # 
  # all_period_gdd = do.call(rbind, gdds)
  threelake = subset(all_period_gdd, lakeid %in% lakeids)
  
  png(filename = outfile, width=2600, height=1900, res=300)
  
  plot(1, NA, ylim=c(2200,3300), xlim=c(0.75,3.25), xaxt='n', xlab='', ylab='Degree Days')
  
  for(i in 1:length(lakeids)){
    points(med_gdd~periodi, subset(all_period_gdd, lakeid==lakeids[i]), pch=16, col=cols[i])
    lines(med_gdd~periodi, subset(all_period_gdd, lakeid==lakeids[i]), lty=3, col=cols[i])
  }
  
  axis(1, at=1:3, labels = unlist(lapply(lapply(periods, range), paste, collapse="-")))
  
  dev.off()
  
}