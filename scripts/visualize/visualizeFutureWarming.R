
library(RColorBrewer)
library(plyr)

visualizeData.visualizeFutureWarming <- function(processedFutureWarming, outfile, ...){

  all_period_gdd = processedFutureWarming
  lakeids = c('2331600', '805400','146100') #Exemplar lakes: Trout Lake, Mendota, Green Lake
  lakenames = c('Trout Lake', 'Lake Mendota', 'Green Lake')
  periods = list(1989:2014, 2040:2064, 2065:2089)
  starts = sapply(periods, min)
  cols = RColorBrewer::brewer.pal(3, 'Set1')
  

  threelake = subset(all_period_gdd, lakeid %in% lakeids)
  
  png(filename = outfile, width=2600, height=1900, res=300)
  
  plot(1, NA, ylim=c(2200,3300), xlim=c(0.75,3.25), xaxt='n', xlab='Years', ylab='Degree Days')
  
  #points(med_gdd~periodi, all_period_gdd, pch=16, col=rgb(0.5,0.5,0.5,0.2), cex=0.5)
  
  for(i in 1:length(lakeids)){
    lakedata = subset(all_period_gdd, lakeid==lakeids[i])
    points(med_gdd~periodi, lakedata, pch=16, col=cols[i])
    lines(med_gdd~periodi, lakedata, lty=3, col=cols[i])
    
    text(min(lakedata$periodi), min(lakedata$med_gdd), labels = lakenames[i], pos=3, 
         cex=1.5, col=cols[i])
    
  }
  
  axis(1, at=1:3, labels = unlist(lapply(lapply(periods, range), paste, collapse="-")))
  
  dev.off()
  
}