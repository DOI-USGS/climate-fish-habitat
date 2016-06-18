#visualizeClimateScenarios
library(httr)
library(plyr)

scenarios = c('a2', 'b1', 'a1b')

rcp_descriptions=c("Low warming scenario (RCP 26)", "Medium warming scenario (RCP 45)", 
                   "Medium-high warming scenario (RCP 60)", "High warming scenario (RCP 85)")

read.rcp.temp = function(scenario){
  
  rcpURL = paste0('http://www.ipcc-data.org/data/ar4_tas-gm_', scenario, '.zip')
  dl_dest = tempfile()
  GET(rcpURL, write_disk(dl_dest, overwrite = TRUE))
  mod_files = unzip(dl_dest, exdir=tempdir())
  mod_names = sapply(basename(mod_files), function(x) strsplit(x, '\\.')[[1]][1])
  all_mods = data.frame()
  for(i in 1:length(mod_files)){
  
    temps = read.table(mod_files[i], sep='')
    
    names(temps) = c('year', 'temp')
    temps$model = mod_names[i]
    all_mods = rbind(all_mods, temps)
  }
  
  all_mods$scenario = scenario
  
  return(all_mods)
}


temps = lapply(scenarios, read.rcp.temp)
temps = do.call(rbind, temps)

temps = ddply(temps, c('scenario', 'model'), function(df){
  df$temp = df$temp - subset(df, year==2000)$temp
  return(df)
})

meanrange = ddply(temps, c('scenario', 'year'), function(df){
  
  data.frame(mean=mean(df$temp), q25=quantile(df$temp,0.30), q75=quantile(df$temp, 0.70))
})

ggplot(meanrange, aes(year, mean, color=scenario)) + 
  geom_ribbon(aes(ymin = q25, ymax = q75, fill=scenario, alpha=0.5)) + 
  geom_line(aes(y=mean, color=scenario)) + 
  xlab('Year') + 
  ylab('Change from year 2000 (°C)') +
  theme_minimal()

ggsave('sandbox/futureClimateScenarios.jpg')
