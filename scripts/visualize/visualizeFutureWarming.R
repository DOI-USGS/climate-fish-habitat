
library(RColorBrewer)
library(plyr)
library(xml2)
library(grid)
library(maps)
library(ggplot2)

visualizeData.visualizeFutureWarming <- function(processedFutureWarming, outfile, ...){

  all_period_gdd = processedFutureWarming
  
  #should theoreticaly pull such data import sections out in vizlab
  wbic_latlon = read.table('data/WI_Lakes_WbicLatLon.tsv', sep='\t', header=TRUE, as.is=TRUE)
  wisco = map_data('state', region = 'wisconsin')
  
  lakeids = c('2331600', '805400','146100') #Exemplar lakes: Trout Lake, Mendota, Green Lake
  lakenames = c('Trout Lake', 'Lake Mendota', 'Green Lake')
  cols = c("#377EB8", "#E41A1C" ,"#4DAF4A")
  periods = list(1989:2014, 2040:2064, 2065:2089)
  starts = sapply(periods, min)
  #cols = RColorBrewer::brewer.pal(length(lakeids), 'Set1')
  names(cols) = lakeids

  threelake = subset(all_period_gdd, lakeid %in% lakeids)
  
  grDevices::svg(filename = outfile, width=9, height=6)
  par(mai = c(0.8, 0.8, 0.2, 0.8))
  plot(1, NA, ylim=c(2200,3300), xlim=c(0.75,3.25), xaxt='n', xlab='Years', ylab='Degree Days')
  
  #points(med_gdd~periodi, all_period_gdd, pch=16, col=rgb(0.5,0.5,0.5,0.2), cex=0.5)
  
  theme_future <- function(base_size = 10, base_family = "")
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
        , axis.title.x = element_text(size=16, vjust=1)
        , axis.title.y = element_text(size=16, angle = 90)
        , axis.text = element_text(size=12)
        , legend.key = element_rect(color = "white")
        , legend.position = c(0.2,0.9)
        , legend.title = element_text(size=16)
        , legend.text = element_text(size=12)
      )
  }
  
  g = ggplot(threelake, aes(x=factor(periodi), y=med_gdd, color=factor(lakeid), group = lakeid)) + 
    geom_point(data=all_period_gdd, aes(x=factor(periodi), y=med_gdd, color='X'), color=rgb(0.5,0.5,0.5,0.1), shape=16) +
    geom_line() + geom_point(size=3, aes(shape=factor(lakeid))) +
    scale_x_discrete(breaks = 1:3, labels=c("1989-2014","2040-2064","2065-2089")) + xlab('Years') + ylab('Degree Days') + 
    guides(color=FALSE, shape=FALSE) + theme_future() + scale_colour_manual(values = cols) + theme(plot.margin = unit(c(0,4,0,0), "cm"), 
                                                                                      axis.title.y=element_text(margin=margin(0,10,0,0)), 
                                                                                      axis.title.x=element_text(margin=margin(10,0,0,0)))
  
  for(i in 1:length(lakeids)){
    
    lakedata = subset(all_period_gdd, lakeid==lakeids[i])
    
    g = g + annotate("text", x=min(lakedata$periodi), y=min(lakedata$med_gdd), 
                     label=lakenames[i], vjust=-1.5, size=7, color=cols[i])
    
  }
  
  ## build inset map
  wbic_cols = cols
  names(wbic_cols) = paste0('WBIC_', lakeids)
  minset = ggplot(wisco, aes(long, lat)) + geom_polygon(fill=rgb(0.5,0.5,0.5,0.8)) + 
    geom_point(data=subset(wbic_latlon, site_id %in% paste0('WBIC_', lakeids)), aes(LON, LAT, color=site_id, shape=site_id), size=3) + 
    guides(color=FALSE, shape=FALSE) + coord_map("conic", lat0 = 30) + theme_bw() + 
      theme(plot.background=element_blank(), axis.title=element_blank(),
            axis.line=element_blank(),axis.text=element_blank(), 
            panel.grid.major=element_blank(), panel.background=element_blank(),
            panel.border=element_blank(), axis.ticks=element_blank()) + 
    scale_colour_manual(values = wbic_cols) + scale_shape_manual(values = c(16, 15, 17))
  
  
  ## output to SVG
  grDevices::svg(filename = outfile, width=9, height=6)
  
  print(g)
  print(minset, vp=viewport(width = 0.45, height = 0.45, x = 0.84, y = 0.6))
  
  dev.off()
  
  
  # svg will auto-scale if you remove the width and height attributes
  svg <- read_xml(outfile)
  xml_attr(svg, 'width') <- NULL
  xml_attr(svg, 'height') <- NULL
  write_xml(svg, outfile)
}
