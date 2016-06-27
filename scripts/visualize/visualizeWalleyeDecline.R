# // document.getElementById('data-line').getTotalLength(); then set that value to 'stroke-dasharray', 
# // animate 'stroke-dashoffset' from that value down to 0. 
svgWallyDecline <- function(object, filename){
  line.i <- which(names(object$view.1.2) == 'lines')
  ids <- c('walleye-line','bass-line')
  for (i in line.i){
    object$view.1.2[[i]]$class = 'data-line'
    object$view.1.2[[i]]$id = ids[1]
    ids <- tail(ids,-1)
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
#walleye-line {
  stroke-dasharray: 1125.464;
  stroke-dashoffset: 1125.464;
  animation: dash-walleye 12s linear forwards;
}

#bass-line {
  stroke-dasharray: 1570.7915;
  stroke-dashoffset: 1570.7915;
  animation: dash-bass 12s linear forwards;
}
@keyframes dash-walleye {
0%  {stroke-dashoffset: 1002.88890337959}
4.76190476190476%  {stroke-dashoffset: 971.257314089763}
9.52380952380952%  {stroke-dashoffset: 934.575185457417}
14.2857142857143%  {stroke-dashoffset: 903.565622400192}
19.047619047619%  {stroke-dashoffset: 875.795170189254}
23.8095238095238%  {stroke-dashoffset: 824.988975252022}
28.5714285714286%  {stroke-dashoffset: 778.895373049547}
33.3333333333333%  {stroke-dashoffset: 727.104578346241}
38.0952380952381%  {stroke-dashoffset: 620.567460490538}
42.8571428571429%  {stroke-dashoffset: 585.183327040643}
47.6190476190476%  {stroke-dashoffset: 556.597104834831}
52.3809523809524%  {stroke-dashoffset: 515.092448662301}
57.1428571428571%  {stroke-dashoffset: 457.379226052573}
61.9047619047619%  {stroke-dashoffset: 423.253621296027}
66.6666666666667%  {stroke-dashoffset: 386.178475119646}
71.4285714285714%  {stroke-dashoffset: 356.117053180556}
76.1904761904762%  {stroke-dashoffset: 314.149534299879}
80.9523809523809%  {stroke-dashoffset: 273.628598661619}
85.7142857142857%  {stroke-dashoffset: 209.518742874852}
90.4761904761905%  {stroke-dashoffset: 86.0816305301447}
95.2380952380952%  {stroke-dashoffset: 52.9225593579903}
100%  {stroke-dashoffset: 0}
}
@keyframes dash-bass {
  0%  {stroke-dashoffset: 1570.79145873685}
4.76190476190476%  {stroke-dashoffset: 1374.35306769274}
9.52380952380952%  {stroke-dashoffset: 1201.8993436709}
14.2857142857143%  {stroke-dashoffset: 1148.19816677505}
19.047619047619%  {stroke-dashoffset: 1098.15460350263}
23.8095238095238%  {stroke-dashoffset: 998.472338600596}
28.5714285714286%  {stroke-dashoffset: 955.852125919288}
33.3333333333333%  {stroke-dashoffset: 922.419206677917}
38.0952380952381%  {stroke-dashoffset: 873.260622483496}
42.8571428571429%  {stroke-dashoffset: 754.701282336884}
47.6190476190476%  {stroke-dashoffset: 706.732918762545}
52.3809523809524%  {stroke-dashoffset: 665.052551573663}
57.1428571428571%  {stroke-dashoffset: 591.372241958407}
61.9047619047619%  {stroke-dashoffset: 529.278771641473}
66.6666666666667%  {stroke-dashoffset: 444.843470943246}
71.4285714285714%  {stroke-dashoffset: 376.279452857956}
76.1904761904762%  {stroke-dashoffset: 306.72508563346}
80.9523809523809%  {stroke-dashoffset: 251.362804033078}
85.7142857142857%  {stroke-dashoffset: 208.788998835515}
90.4761904761905%  {stroke-dashoffset: 150.429589619863}
95.2380952380952%  {stroke-dashoffset: 35.939371168678}
100%  {stroke-dashoffset: 0}
}
'
  # start.i <- 4 #where to start animation path
  # 
  # datapoints <- strsplit(gsub('M','', path),'[ ]')[[1]]
  # t.length <- c(0)
  # for (i in 2:length(datapoints)){
  #   pt.1 <- as.numeric(strsplit(datapoints[i-1],'[,]')[[1]])
  #   pt.2 <- as.numeric(strsplit(datapoints[i],'[,]')[[1]])
  #   t.length[i] <- t.length[i-1] + sqrt((pt.2[1]-pt.1[1])^2+(pt.2[2]-pt.1[2])^2)
  # }
  # 
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

