wally <- read.csv('data/walleye_age0_annual_means.csv', stringsAsFactors = FALSE, header = TRUE)
bass <- read.csv('data/fall_lmb8_cpue_annual_mean.csv', stringsAsFactors = FALSE, header = TRUE)

library(gsplot)
library(dinosvg)
library(dplyr)
bass.munged <- bass %>% rename(year = Survey.Year, cpue = mean.cpue.fall, n=count) %>% filter(year>1990) %>% select(-sd.cpue, -X)
wally.munged <- wally %>% rename(cpue = mean.cpue) %>% filter(year>1990) %>% select(year, cpue)
x.tcks = seq(1995,2010, by=5)
par(mai=c(.5,.5,0.8,0.5))
gs.bass <- gsplot() %>% lines(bass.munged$year, bass.munged$cpue, col='#990000', ylim=c(0,18)) %>% 
  axis(1, at=x.tcks, labels=x.tcks) %>% 
  axis(2, at=c(0, 5, 10, 15), labels=c(0, 5, 10, 15))
gs.wally <- gsplot() %>% lines(wally.munged$year, wally.munged$cpue, col='#01b29F', ylim=c(0,65)) %>% 
  axis(1, at=x.tcks, labels=x.tcks) %>% 
  axis(2, at=c(0, 20, 40, 60), labels=c(0, 20, 40, 60))

to_svg <- function(object, filename){
  object$view.1.2$lines$id = 'data-line'
  object$css <- '#tick-labels, #y-title {
    \tfont-family: Arial;
}
  #data-line {
    \tstroke-linejoin: round;
    \tstroke-width:3;
  }
  #tick-labels {
    \tfont-size: 12.00pt; 
  }
  #y-title {
    \tfont-size: 14.00pt; 
  }'
  svg = svg(object, width=10, height=7, as.xml=TRUE)
  XML::newXMLNode('text', parent = svg, attrs = c(x='10', y='50', 'text-anchor'="begin", id='y-title'), XML::newXMLTextNode('Catch per unit effort'), at=1)
  dinosvg:::write_svg(svg, file = filename)
}

to_svg(gs.bass, 'sandbox/bass_decline.svg' )
to_svg(gs.wally, 'sandbox/wally_decline.svg')
