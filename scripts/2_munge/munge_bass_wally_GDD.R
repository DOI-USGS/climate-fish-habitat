wally <- read.csv('data/partial_plot_wae_GDD_median_quartiles.csv', stringsAsFactors = FALSE, header = TRUE)
bass <- read.csv('data/partial_plot_LMB_GDD_median_quartiles.csv', stringsAsFactors = FALSE, header = TRUE)

gdd.bins <- seq(2200,3100, by = 50)

get_bin <- function(val){
  difs <- abs(val-gdd.bins)
  gdd.bins[which.min(difs)][1]
}


wally$bin = NA
for (j in 1:nrow(wally)){
  wally$bin[j] <- get_bin(wally$DD[j])
}
bass$bin = NA
for (j in 1:nrow(bass)){
  bass$bin[j] <- get_bin(bass$GDD[j])
}


library(dplyr)
binned.wally = wally %>% rename(GDD=DD) %>% group_by(bin) %>% summarize(prob=mean(median))
binned.bass = bass %>% group_by(bin) %>% summarize(prob=mean(median))

plot(binned.wally, type='l', ylim=c(0,1), lwd=3, las=1, xlab='Growing Degree Days', ylab='Probability of fish')
lines(binned.bass, lwd=3, col='red')