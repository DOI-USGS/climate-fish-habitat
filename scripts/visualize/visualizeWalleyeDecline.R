# // document.getElementById('data-line').getTotalLength(); then set that value to 'stroke-dasharray', 
# // animate 'stroke-dashoffset' from that value down to 0. 
svgWallyDecline <- function(object, filename){
  line.i <- which(names(object$view.1.2) == 'lines')
  ids <- c('walleye-line','bass-line')
  for (i in line.i){
    object$view.1.2[[i]]$class = 'data-line'
    object$view.1.2[[i]]$id = ids[1]
    ids <- head(ids,-1)
  }
  object$css <- '#tick-labels, #y-title, text {
  \tfont-family: Arial;
}
.data-line {
\tstroke-linejoin: round;
\tstroke-width:3;
\tstroke-linecap: round;
}
#tick-labels {
\tfont-size: 10.00pt; 
}
#y-title, axis-label {
\tfont-size: 14.00pt; 
}
#walleye-line, #bass-line {
  stroke-dasharray: {{wally-dash-max}};
  stroke-dashoffset: {{wally-dash-max}};
  animation: dash 2s linear alternate infinite;
}

@keyframes dash {
  from {
    stroke-dashoffset: {{wally-dash-max}};
  }
  to {
    stroke-dashoffset: 0;
  }
}
'
  # start.i <- 4 #where to start animation path
  # 
  # datapoints <- strsplit(gsub('M','', path),'[ ]')[[1]]
  # t.length <- 0
  # for (i in 2:length(datapoints)){
  #   pt.1 <- as.numeric(strsplit(datapoints[i-1],'[,]')[[1]])
  #   pt.2 <- as.numeric(strsplit(datapoints[i],'[,]')[[1]])
  #   t.length <- t.length + sqrt((pt.2[1]-pt.1[1])^2+(pt.2[2]-pt.1[2])^2)
  # }
  
  svg <- dinosvg::svg(object, width=10, height=7, as.xml=TRUE)
  
  
  
  XML::newXMLNode('text', parent = svg, attrs = c(x='10', y='25', 'text-anchor'="begin", id='y-title'), XML::newXMLTextNode('Relative abundance'), at=1)
  dinosvg:::write_svg(svg, file = filename)
  
}

library(gsplot)

visualizeData.visualizeWallyDecline <- function(processedWallyTrends, processedBassTrends, ..., outfile){
  
  wally <- processedWallyTrends
  bass <- processedBassTrends

  x.tcks = seq(1995,2010, by=5)
  y.tcks = seq(0, 1.5, by=0.25)
  par(mai=c(.5,.5,0.5,0.5))
  
  gs.trends <- gsplot() %>% 
    lines(wally$Year, wally$rel.abun, col='#01b29F', ylim=c(0,1.3), xlab='Year') %>% 
    lines(bass$Year, bass$rel.abun, col='#990000', ylim=c(0,1.3)) %>% 
    axis(1, at=x.tcks, labels=x.tcks) %>% 
    axis(2, at=y.tcks, labels=y.tcks) 
  
  svgWallyDecline(gs.trends, outfile)
}

