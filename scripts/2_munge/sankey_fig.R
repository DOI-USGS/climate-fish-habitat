fish.change = read.csv('data/categories_by_time_period_median_probs.csv', stringsAsFactors = FALSE)

library(dplyr)
fish.change <- filter(fish.change, !is.na(X1989.2014) & !is.na(X2040.2064) & !is.na(X2065.2089))
arrows.1 = group_by(fish.change, X1989.2014) %>% 
  summarize(toCoexistence=sum(X2040.2064 == 'Coexistence'),toWally=sum(X2040.2064 == 'Walleye dominant'), 
            toBass = sum(X2040.2064 == 'Bass dominant'), toNeither=sum(X2040.2064 == 'Neither'))


js.funs <- '\nvar svg = document.querySelector("svg")
    var xmax = Number(svg.getAttribute("viewBox").split(" ")[2]);
var pt = svg.createSVGPoint();
function init(evt){
if ( window.svgDocument == null ) {
svgDocument = evt.target.ownerDocument;
}
}
function hovertext(text, evt){
var tooltip = document.getElementById("tooltip");
var tooltip_bg = document.getElementById("tooltip_bg");
tooltip.setAttribute("text-anchor","begin");
tooltip.setAttribute("dx","7");
if (evt === undefined){
tooltip.setAttribute("class","hidden");
tooltip.firstChild.data = text;
tooltip_bg.setAttribute("x",0);
tooltip_bg.setAttribute("y",0);
tooltip_bg.setAttribute("class","hidden");
} else {
evt.target.setAttribute("opacity","1.0");
pt = cursorPoint(evt)
tooltip.setAttribute("x",pt.x);
tooltip.setAttribute("y",pt.y);
tooltip.firstChild.data = text;
tooltip_bg.setAttribute("x",pt.x+5);
tooltip_bg.setAttribute("y",pt.y-20);
tooltip.setAttribute("class","shown");
tooltip_bg.setAttribute("class","shown");
var length = tooltip.getComputedTextLength();
tooltip_bg.setAttribute("width", length+8);
if (pt.x+length+8 > xmax){
tooltip.setAttribute("text-anchor","end");
tooltip.setAttribute("dx","-8");
tooltip_bg.setAttribute("x",pt.x-12-length);
}
}}
function cursorPoint(evt){
pt.x = evt.clientX; pt.y = evt.clientY;
return pt.matrixTransform(svg.getScreenCTM().inverse());
};'

types = c('Walleye dominant', 'Coexistence', 'Bass dominant', 'Neither')
colors = c('Walleye dominant'='cyan','Coexistence'='#9932CD','Bass dominant'='#cc0000','Neither'='grey')
to.types = c('toWally', 'toCoexistence', 'toBass', 'toNeither')
periods = c('early'='X1989.2014', 'mid'='X2040.2064', 'late'='X2065.2089')
arrow.names <- c('toWally'='Walleye dominant', 'toCoexistence'='Coexistence', 'toBass'='Bass dominant', 'toNeither'="Neither")
period.txt = c('1989-2014','2040-2064','2065-2089')
library(dinosvg)
svg_node = dinosvg:::svg_node

scale = 0.33
box.w = 170
gap.s = 200
box.s = 12
l.m = 20
t.m = 20
y = list()
h = list()

n.threshold = c(40,100)

svg <- dinosvg:::init_svg(14,12)
dinosvg:::add_css(svg, '
.hidden {
opacity:0;
}
path, .bin {
        -webkit-transition: 0.25s ease-in-out;
        -moz-transition: 0.25s ease-in-out;
        -o-transition: 0.25s ease-in-out;
        transition: 0.25s ease-in-out;
    }
  text {
		  font-size: 14px;
		  cursor: default;
		  font-family: Tahoma, Geneva, sans-serif;}
                  .big-text{
font-size: 22px;
                  }
.medium-text{
font-size: 18px;
                  }
                  ')

dinosvg:::add_ecmascript(svg, js.funs)

for (i in 1:length(periods)){
  period.id = names(periods)[i]
  period.name = unname(periods[i])
  
  g <- svg_node('g',svg, c(id=period.id, opacity="0.9",transform=sprintf("translate(%s,%s)",l.m+(i-1)*(box.w+gap.s), t.m)))
  y[[period.name]][1] = 0
  t = 1
  period.data = group_by_(fish.change, period.name) %>% tally %>% data.frame %>% tidyr::spread_(key=period.name, 'n')
  for (type in types){
    h[[period.name]][t] = period.data[[type]]*scale
    svg_node('rect',g,c(width=box.w, class='bin', height=h[[period.name]][t], y=y[[period.name]][t], fill=colors[[type]], opacity="0.6",
                        onmousemove=sprintf("hovertext('%s %s lakes',evt)",formatC(period.data[[type]], format="d", big.mark=','), type), onmouseout="hovertext(' ');evt.target.setAttribute('opacity','0.6');"))
    if (period.data[[type]] < n.threshold[1]){
      svg_node('text',g, c(x=box.w/2, y=y[[period.name]][t], dy="-3", fill='black', stroke='none', 'text-anchor'='middle'), XML::newXMLTextNode(sprintf("%s",type)))
    } else if (period.data[[type]] > n.threshold[2]){
      svg_node('text',g, c(class='medium-text', x=box.w/2, y=y[[period.name]][t]+h[[period.name]][t]/2, dy="0.33em", fill='black', stroke='none', 'text-anchor'='middle'), XML::newXMLTextNode(sprintf("%s",type)))
    } else {
      svg_node('text',g, c(x=box.w/2, y=y[[period.name]][t]+h[[period.name]][t]/2, dy="0.33em", fill='black', stroke='none', 'text-anchor'='middle'), XML::newXMLTextNode(sprintf("%s",type)))
      
    }
    
    y[[period.name]][t+1] = y[[period.name]][t]+box.s+h[[period.name]][t]
    t=t+1
  }
  svg_node('text',g, c(x=box.w/2, dy='1em', class='big-text', y=y[[period.name]][t], fill='black', stroke='none', 'text-anchor'='middle'), XML::newXMLTextNode(period.txt[i]))
  y[[period.name]] <- y[[period.name]][1:4] # get rid of the last one
}

arrows = list()

# couldn't figure out how to do this w/ summarize_...
arrows[[1]] = group_by(fish.change, X1989.2014) %>% 
  summarize(toCoexistence=sum(X2040.2064 == 'Coexistence'),toWally=sum(X2040.2064 == 'Walleye dominant'), 
            toBass = sum(X2040.2064 == 'Bass dominant'), toNeither=sum(X2040.2064 == 'Neither')) %>% data.frame

arrows[[2]]  = group_by(fish.change, X2040.2064) %>% 
  summarize(toCoexistence=sum(X2065.2089 == 'Coexistence'),toWally=sum(X2065.2089 == 'Walleye dominant'), 
            toBass = sum(X2065.2089 == 'Bass dominant'), toNeither=sum(X2065.2089 == 'Neither')) %>% data.frame

start.arrows <- list()
# // arrows!
for (i in 1:(length(periods)-1)){
  
  period.from = unname(periods[i])
  from.i = 1
  for (from.type in types){
    
    where.type <- which(unname(unlist(from.type == arrows[[i]][1]))) # this is the "FROM" type column
    to.i = 1
    y.0 = 0
    for (to.type in to.types){
      h = arrows[[i]][[to.type]][where.type]*scale
      from.y.bot = y[[period.from]][from.i]+h+y.0
      start.arrows[[period.from]][[from.type]][[to.type]] <- c(y1=from.y.bot, h = h)
      to.i = to.i+1
      y.0  = y.0 + h
    }
    from.i = from.i+1
  }
  
}

# // arrows!
for (i in 2:length(periods)){
  
  period.to = unname(periods[i])
  period.from = unname(periods[i-1])
  to.i = 1
 
  for (to.type in to.types){
    
    
    from.i = 1
    y.0 = 0
    for (from.type in types){
      
      where.type <- which(unname(unlist(from.type == arrows[[i-1]][1]))) # this is the "FROM" type column
      h = arrows[[i-1]][[to.type]][where.type]*scale
      to.y.bot = y[[period.to]][to.i]+h+y.0
      start.arrows[[period.from]][[from.type]][[to.type]]= c(start.arrows[[period.from]][[from.type]][[to.type]], y2= to.y.bot)
      
      from.i = from.i+1
      y.0  = y.0 + h
    }
    to.i = to.i+1
    
  }
}

for (i in 1:2){
  period.from = unname(periods[i])
  g <- svg_node('g',svg, c(opacity='0.4', id=paste0(period.from,'-arrow'), transform=sprintf("translate(%s,%s)",l.m+(i-1)*(box.w+gap.s), t.m)))
  for (from.type in c('Bass dominant', 'Neither', 'Walleye dominant', 'Coexistence')){
    for (to.type in names(start.arrows[[period.from]][[from.type]])){
      stc = start.arrows[[period.from]][[from.type]][[to.type]]
      arr.txt <- arrow.names[[to.type]]
      if (from.type == arr.txt){
        mouse.text <- sprintf("hovertext('%s lakes remain as %s',evt)",formatC(stc[['h']]/scale, format="d", big.mark=','), from.type)
      } else {
        mouse.text <- sprintf("hovertext('%s lakes shift from %s to %s',evt)",formatC(stc[['h']]/scale, format="d", big.mark=','), from.type, arr.txt)  
      }
      
      svg_node('path', g, c(d = sprintf("M%s,%s L%s,%s v-%s L%s,%s", box.w, stc[['y1']], box.w+gap.s, stc[['y2']], stc[['h']], box.w, stc[['y1']]-stc[['h']]), 
                            fill=colors[[from.type]], stroke='none', opacity="0.5",
                            onmousemove=mouse.text, onmouseout="hovertext(' ');evt.target.setAttribute('opacity','0.6');"))
    }
  }
  
}

svg_node('rect',svg, c(id="tooltip_bg", rx="2.5", ry="2.5", width="55", height="20", fill="white", 'stroke-width'="0.5", stroke="#696969", class="hidden"))
svg_node('text',svg, c(id="tooltip", dy="-5", stroke="none", fill="#000000", 'text-anchor'="begin", class="sub-label"), XML::newXMLTextNode(' '))


dinosvg:::write_svg(svg, file='sandbox/futureSuitability-desktop.svg')
