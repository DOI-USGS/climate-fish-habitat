# // document.getElementById('data-line').getTotalLength(); then set that value to 'stroke-dasharray', 
# // animate 'stroke-dashoffset' from that value down to 0. 
svgWallyDecline <- function(object, filename){
  object$view.1.2$lines$id = 'data-line'
  object$css <- '#tick-labels, #y-title {
  \tfont-family: Arial;
}
#data-line {
\tstroke-linejoin: round;
}
#tick-labels {
\tfont-size: 12.00pt; 
}
#y-title {
\tfont-size: 14.00pt; 
}'
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
    lines(wally$Year, wally$rel.abun, col='#01b29F', ylim=c(0,1.3), lwd=3) %>% 
    lines(bass$Year, bass$rel.abun, col='#990000', ylim=c(0,1.3), lwd=3) %>% 
    axis(1, at=x.tcks, labels=x.tcks) %>% 
    axis(2, at=y.tcks, labels=y.tcks) 
  
  svgWallyDecline(gs.trends, outfile)
}

