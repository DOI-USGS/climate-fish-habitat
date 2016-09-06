
library(plyr)
library(maps)
library(RColorBrewer)
library(ggplot2)

nldas       = read.table('data/NLDAS_best_key_habitat.tsv', sep='\t', header=TRUE, as.is=TRUE)
wbic_latlon = read.table('data/WI_Lakes_WbicLatLon.tsv', sep='\t', header=TRUE, as.is=TRUE)

#wi_JAS = ddply(nldas, 'year', function(df){mean(df$mean_surf_JAS, na.rm=TRUE)})

# png('sandbox/wi_avg_jas.png', width=900, height=900, res=150)
# plot(wi_JAS, pch=16)
# abline(lm(V1~year, wi_JAS))
# dev.off()

temp_change_30yr = ddply(nldas, 'site_id', function(df){
  
  early = subset(df, year %in% 1980:1985)
  late  = subset(df, year %in% 2005:2010)
  
  jas_diff = (9/5)*(mean(late$mean_surf_JAS) - mean(early$mean_surf_JAS)) 
  
  return(data.frame(anom_30yr_F=jas_diff))
})

jas_diff_latlon = merge(temp_change_30yr, wbic_latlon, by='site_id')


#create nice tick labels
lonbrk = seq(-92, -88, 2)
latbrk = seq(42,47,1)
lonlab = paste0(lonbrk, ' °E') 
latlab = paste0(latbrk, ' °N')



jas_diff_latlon$anom_30yr_F[jas_diff_latlon$anom_30yr_F > 1.8] = 1.8

wisco = map_data('state', region = 'wisconsin')

ggplot(wisco, aes(long, lat)) + geom_polygon(fill=rgb(0.5,0.5,0.5,0.8)) + 
  geom_point(data=jas_diff_latlon, aes(LON, LAT, color=anom_30yr_F)) + 
  scale_color_gradientn("Lake warming since 1980 (°F)\n", colors=c( "#f9f3c2","#FF0000"), limits=c(0,1.8)) + 
  coord_map("conic", lat0 = 30) + theme_bw() + theme(plot.background=element_blank()) + 
  xlab('Longitude') + ylab('Latitude') + 
  scale_x_continuous(breaks = lonbrk, labels = lonlab) + 
  scale_y_continuous(breaks = latbrk, labels = latlab)

ggsave('sandbox/contemporaryLakeTrends-desktop.png', height=5.75, width=8.7, units='in')

# ggplot(wisco, aes(long, lat)) + geom_polygon() + 
#   geom_point(data=jas_latlon_80s, aes(LON, LAT, color=V1)) + 
#   scale_colour_gradientn("Average Summer Temp", colours=c( "#f9f3c2","#660000"), limits=c(22,27)) + 
#   ggtitle('2000-2010')
# ggsave('sandbox/2000s_JAS_map.png')

