# // document.getElementById('data-line').getTotalLength(); then set that value to 'stroke-dasharray', 
# // animate 'stroke-dashoffset' from that value down to 0. 
library(XML)
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
#y-title, #x-title {
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
.background-walleye {
       fill:#b2e7e2;
       stroke:white;
       stroke-width:2;
       fill-opacity:0.75;
    }
.background-bass {
       fill:#e0b2b2;
       stroke:white;
       stroke-width:2;
       fill-opacity:0.75;
}
@keyframes shift-bass {
	  0%   {transform: translateX(820px)}
	  50% {transform: translateX(700px)}
    100% {transform: translateX(720px)}
}
@keyframes background-largemouth-bass {
	  0%   {transform: translate(0)}
	  50% {transform: translate(-10px,5px)}
    100% {transform: translateY(0)}
}
#all-bass {
  animation: shift-bass 12s linear forwards;
}
#bass-1,#bass-19 {
	  animation: background-largemouth-bass 2s ease infinite;
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
  xlabel <- XML::xpathApply(dinosvg:::g_side(svg,'1'), "//*[local-name()='g'][@id='axis-label']/*[local-name()='text']")[[1]]
  XML::xmlAttrs(xlabel)[['id']] <- 'x-title'

  vb <- strsplit(XML::xmlAttrs(svg)[['viewBox']],'[ ]')[[1]]
  XML::xmlAttrs(svg)[['viewBox']] <- paste(-200, vb[2], as.numeric(vb[3])+400, vb[4])
  all.bass <- newXMLNode('g',parent = svg, attrs = c('id'='all-bass','transform'=sprintf("translate(%s,0)", as.numeric(vb[3])+100), class='background-bass'), at=1)
  all.wally <- newXMLNode('g',parent = svg, attrs = c('id'='all-walleye','transform'="translate(-100,0)", class='background-walleye'), at=0)
  n = 30
  set.seed(211)
  wally.y = runif(n=n, min = as.numeric(vb[2]), max = as.numeric(vb[4]))
  wally.x = runif(n=n, min = -200, max = 100)
  wally.s = rnorm(n=n, mean = 1, sd=0.1)
  bass.y = runif(n=n, min = as.numeric(vb[2]), max = as.numeric(vb[4]))
  bass.x = runif(n=n, min = -100, max = 200)
  bass.s = rnorm(n=n, mean = 1, sd=0.1)
  for (i in 1:n){
    newXMLNode('use', parent=all.wally, attrs = c(x=wally.x[i], y=wally.y[i], 'xlink:href'="#walleye", id=sprintf('walleye-%s',i), transform=sprintf('scale(%s)',wally.s[i])))
    newXMLNode('use', parent=all.bass, attrs = c(x=bass.x[i], y=bass.y[i], 'xlink:href'="#bass", id=sprintf('bass-%s',i), transform=sprintf('scale(%s)',bass.s[i])))
  }
  defs <- newXMLNode('defs',parent = svg)
  newXMLNode('path',parent=defs,
             attrs = c(d="m -77.09283,4.1298534 c 0,0 -1.150639,-2.6848248 -0.383546,-3.83546413 0.767093,-1.1506392 6.136743,-2.68482487 6.136743,-2.68482487 l 9.205114,-1.9177321 10.355753,-3.0683713 10.7393,-4.986103 18.410228,-4.219011 -1.15064,-9.588661 1.917732,0 1.534186,-8.821567 3.451918,1.150639 0.767093,-6.520289 4.986103,1.150639 1.917732,-3.068371 1.9177321,2.301278 2.6848249,1.15064 0.3835464,-3.451918 1.5341857,0 L 0,-39.210892 l 3.0683713,-2.301278 1.1506393,0.383546 0.3835464,3.068371 3.8354641,-1.150639 1.9177319,3.451918 3.451918,-1.150639 1.150639,3.451917 3.068371,-2.301278 1.15064,4.21901 2.684825,-0.383546 0.383546,2.301279 3.068371,0.767092 0,1.917732 2.301279,0.383547 0,1.917732 2.684825,1.917732 0,0.767093 c 0,0 1.150639,1.534186 1.534185,3.451918 0.383547,1.917732 5.36965,1.917732 5.36965,1.917732 0,0 3.835464,-8.821568 6.903836,-13.424125 3.068371,-4.602557 6.520289,-6.136742 6.520289,-6.136742 l 3.451917,-0.767093 c 0,0 0.767093,0.383546 3.451918,2.301278 2.684825,1.917732 25.314064,21.862146 25.314064,21.862146 0,0 1.534185,0.767093 0.383546,1.917732 -1.150639,1.1506395 -4.986103,3.0683716 -4.986103,3.0683716 l 6.903835,1.1506392 8.821568,0 26.081158,-18.7937748 c 0,0 0.76709,-1.917732 3.45192,-1.534186 2.68482,0.383547 3.83546,4.219011 3.83546,4.219011 l 0,11.122846 c 0,0 0.38355,-0.767092 0,2.3012789 -0.38355,3.0683713 -4.21901,7.6709283 -4.21901,7.6709283 0,0 -0.38355,1.15063927 -0.38355,2.6848249 0,1.5341857 3.45192,5.7531963 3.45192,5.7531963 l 0.76709,1.917732 1.15064,8.4380206 -0.38354,9.205114 -3.06838,2.301279 c 0,0 -12.65703,-4.602557 -14.57476,-6.520289 -1.91773,-1.917733 -12.273484,-10.355754 -12.273484,-10.355754 0,0 -2.301279,-3.4519166 -3.451918,-3.4519166 -1.150639,0 -21.862146,3.4519166 -21.862146,3.4519166 l 0,1.534186 2.301279,3.835464 -5.36965,5.753196 -4.602557,7.670929 c 0,0 0,1.534185 -1.150639,3.451917 -1.150639,1.917732 -4.986104,2.684825 -7.287382,2.301279 -2.301279,-0.383547 -6.520289,-4.219011 -6.520289,-4.219011 L 42.957198,18.704617 c 0,0 -11.889938,1.917732 -21.095052,2.301278 -9.205114,0.383547 -19.9444139,0.383547 -25.3140637,0 -5.3696498,-0.383546 -4.9861034,0 -4.9861034,0 0,0 4.602557,2.301279 5.7531962,4.219011 1.1506392,1.917732 1.91773207,4.21901 1.91773207,4.21901 0,0 0.76709283,2.684825 -1.15063927,3.835465 -1.917732,1.150639 -5.3696498,2.684824 -6.1367426,4.986103 -0.7670928,2.301278 -0.3835464,5.753196 -2.6848253,5.753196 -2.301278,0 -7.670928,-6.136742 -7.670928,-8.054475 0,-1.917732 -4.602557,-12.657031 -4.21901,-13.80767 0.383546,-1.15064 -16.492496,-2.684825 -21.862146,-4.219011 -5.36965,-1.534186 -20.32796,-5.753196 -20.32796,-5.753196 L -75.94219,10.266596 c 0,0 0,-0.3835456 -1.150639,-1.9177316 -1.150639,-1.534186 -1e-6,-4.219011 -1e-6,-4.219011 z",
                       id="walleye", transform="scale(-0.5,0.5)"))
  newXMLNode('path',parent=defs,
             attrs = c(d="m -84.380212,4.5133995 4.602557,-4.21901102 7.670929,-6.13674198 9.58866,-3.835464 19.177321,-10.7392995 c 0,0 15.341852,-4.986104 23.012782,-5.36965 7.67093,-0.383547 1.91773,-4.986104 1.91773,-4.986104 l 2.68483,0.383547 0.38355,-3.835465 2.68482,2.684825 4.2190103,-4.986103 0.76709,3.451918 4.60256,-6.136743 1.53419,-1.150639 1.5341800262923,0 -1.5341800262923,4.602557 5.36965,-2.684825 1.53418,1.917732 -1.53418,3.068371 4.60255,-0.767093 -0.38354,3.068372 2.6848197,0 0.7671,2.301278 3.06837,-0.767093 1.53418,1.917733 0.38355,0.767092 2.68482,0 1.53419,1.15064 c 0,0 8.43802,-8.054475 18.02668,-10.7393 9.58866,-2.684825 19.17732,1.534186 19.17732,1.534186 0,0 5.7532,2.684825 6.13674,8.054474 0.38355,5.36965 0.7671,8.054475 -0.76709,9.588661 -1.53418,1.534185 -8.82157,2.684825 -8.82157,2.684825 l -0.38354,2.684825 c 0,0 6.13674,2.6848245 15.34185,1.917732 9.20512,-0.767093 9.58866,-0.767093 9.58866,-0.767093 l 9.97221,-5.753196 c 0,0 12.657033,-7.670929 17.259593,-8.054475 4.60256,-0.383547 5.7532,0 5.7532,0 0,0 4.9861,5.753196 4.60255,9.205114 -0.38354,3.451918 -5.75319,8.8215675 -6.90383,11.8899395 -1.15064,3.06837098 0.76709,6.520289 0.76709,6.520289 0,0 4.60256,6.520286 4.60256,9.9722055 0,3.45192 -1.15064,7.67093 -3.06837,8.82157 -1.91774,1.15064 -6.52029,1.53418 -8.05448,1.15064 -1.53418,-0.38355 -22.245693,-11.5064 -22.245693,-11.5064 0,0 -11.50639,-2.6848195 -19.17732,-1.9177295 -7.67093,0.7670995 -7.28738,2.3012795 -7.28738,2.3012795 l 8.43802,2.68483 c 0,0 2.68482,1.15064 2.68482,2.68482 0,1.53419 -0.76709,9.20511 -3.06837,11.88994 -2.30128,2.68482 -5.7532,5.7532 -10.35575,5.7532 -4.60256,0 -11.88994,-3.45192 -11.88994,-3.45192 L 27.615337,25.992 26.081157,23.30717 c 0,0 -2.30128,1.53419 -12.27349,2.68483 -9.9721997,1.15064 -30.30016,-0.76709 -30.30016,-0.76709 0,0 7.2873803,1.91773 8.4380203,3.45191 1.15064,1.53419 1.15064,4.98611 1.15064,4.98611 0,0 0.38354,2.68482 1.15064,4.21901 0.76709,1.53418 0.76709,5.36965 -1.15064,4.9861 -1.91773,-0.38355 -10.3557603,-4.9861 -13.0405803,-8.43802 -2.68483,-3.45192 -5.36965,-7.28738 -5.36965,-7.28738 0,0 -17.25959,0.76709 -19.560868,-0.38355 -2.301278,-1.15064 -14.191217,-4.60256 -14.191217,-4.60256 0,0 -5.753196,-0.38354 -9.205114,-1.15063 -3.451918,-0.7671 -9.972207,-4.60256 -9.972207,-4.60256 l -6.520289,-5.7532 c 0,0 -1.917732,-0.38354 -2.301278,-1.9177295 -0.383547,-1.534186 2.684824,-4.219011 2.684824,-4.219011 z",
                       id="bass", transform="scale(0.5,0.5)"))
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

