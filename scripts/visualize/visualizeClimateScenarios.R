#visualizeClimateScenarios
library(httr)
library(plyr)
library(xml2)
library(ggplot2)

visualizeData.visualizeClimateScenarios <- function(outfile, ...){
  
  #outfile = 'figures/futureClimate-desktop.svg'
  
  scenarios = c('a2', 'b1')#, 'a1b')
  
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
  
  #convert to Fahrenheit
  meanrange$mean = meanrange$mean * 9/5
  meanrange$q25 = meanrange$q25 * 9/5
  meanrange$q75 = meanrange$q75 * 9/5
  
  theme_future <- function(base_size = 10, base_family = "Helvetica")
    {
      theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(
          panel.border = element_blank()
          , panel.grid.major.x = element_blank()
          , panel.grid.minor.x = element_blank()
          #, panel.grid.major.y = element_line(color="black")
          , panel.grid.major.y = element_line(size=0.25)
          , axis.line.x = element_line(colour = "black")
          , axis.line.y = element_line(colour = "black")
          , axis.title.x = element_text(size=16)
          , axis.title.y = element_text(size=16, angle = 90)
          , axis.text = element_text(size=12)
          , legend.key = element_rect(color = "white")
          , legend.position = c(0.35,0.9)
          , legend.title = element_text(size=16)
          , legend.text = element_text(size=12)
        )
    }
  
  ggplot(meanrange, aes(year, mean, color=scenario)) + 
    geom_ribbon(aes(ymin = q25, ymax = q75, fill=scenario), alpha=0.5) +
    geom_line(aes(y=mean, color=scenario), size=1.5) + 
    xlab('Year') + 
    ylab(expression(Change~from~year~2000~"("*degree*F*")")) +
    scale_x_continuous(limits= c(1990, 2100), breaks = seq(1990, 2100, by = 20))+
    scale_fill_manual(values = c("a2" = "chocolate","b1" = "dodgerblue")
                       , labels = c("High greenhouse gas emissions (A2)", "Low greenhouse gas emissions (B1)"), name = "Warming Scenario") +
    scale_color_manual(values = c( "a2" = "chocolate","b1" = "dodgerblue")
                       , labels = c("High", "Low"), name = "Warming Scenario")+
    #geom_hline(yintercept = 0, linetype = "dotted")+
    geom_vline(xintercept = 2016)+
    theme_future() + guides(linetype=FALSE, color=FALSE)
  
  ggsave(outfile, device = 'svg')
  
  svg <- read_xml(outfile)
  xml_attr(svg, 'width') <- NULL
  xml_attr(svg, 'height') <- NULL
  write_xml(svg, outfile)


}
