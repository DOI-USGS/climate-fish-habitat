#visualizeClimateScenarios

rcps = c(26, 45, 60, 85)
rcp_descriptions=c("Low warming scenario (RCP 26)", "Medium warming scenario (RCP 45)", 
                   "Medium-high warming scenario (RCP 60)", "High warming scenario (RCP 85)")

read.rcp.temp = function(rcp){
  temps = read.fwf(paste0('data/icmip5_tas_Amon_modmean_rcp', rcp, '_0-360E_-90-90N_n_000.dat.txt'), 
                   skip=3, widths=c(5, 11, rep(15,11)))
  names(temps) = c('year', paste0('month_', 1:12))
  
  temps$ann_avg = apply(temps[,-1], 1, mean)

  return(temps)
}

rcp26 = read.rcp.temp(26)

#use 1990-2010 as baseline temp for anomaly
baseline = mean(subset(rcp26, year %in% 2000)$ann_avg)

jpeg('images/futureClimateScenarios.jpg', res=300, width=3000, height=1500)
plot(1, NA, ylim=c(-1, 4), xlim=c(1995,2100), xlab='Year', ylab='Temperature increase since year 2000')
polygon(c(1990, 1990, 2016, 2016), c(-2, 6, 6, -2), col=rgb(0.5,0.5,0.5,0.5))

cols = rev(paste0(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"), 'FF'))

for(i in 1:length(rcps)){
  rcp_temp = read.rcp.temp(rcps[i])
  rel_baseline = mean(subset(rcp_temp, year %in% 2000)$ann_avg)
  
  lines(rcp_temp$year, rcp_temp$ann_avg-baseline-(rel_baseline-baseline), col=cols[i], lwd=2)
}
abline(v=2016)
text(2017.3, 2, labels = 'Future', srt=90)
text(2014.4, 2, labels = 'Past', srt=90)
abline(h=0)

legend('top', legend=rcp_descriptions, lwd=2, col=cols, horiz=FALSE, bty='n')

for(l in -1:5){
  abline(h=l, lty=3, col=rgb(0.5,0.5,0.5,0.4))
}

dev.off()
