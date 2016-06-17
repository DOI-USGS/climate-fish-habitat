
visualizeFutureSuitability <- function(item){
  
  fish.change.list <- readData(item$filename)
  fish.sum <- fish.change.list$fish.change.summary
  arrows <- fish.change.list[c('arrows.1', 'arrows.2')]

  types <- c('Walleye dominant', 'Coexistence', 'Bass dominant', 'Neither')
  colors <- c('cyan','#9932CD','#cc0000','grey')
  to.types <- c('toWally', 'toCoexistence', 'toBass', 'toNeither')
  periods <- c('early'='X1989.2014', 'mid'='X2040.2064', 'late'='X2065.2089')
  period.txt <- c('1989-2014','2040-2064','2065-2089')
  
  svg_node <- dinosvg:::svg_node
  
  scale <- 0.33
  box.w <- 170
  gap.s <- 200
  box.s <- 12
  l.m <- 20
  t.m <- 20
  y <- list()
  h <- list()
  
  n.threshold <- c(40,100)
  
  svg <- dinosvg:::init_svg(14,12)
  dinosvg:::add_css(svg, 'text {
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
  
  for (i in 1:length(periods)){
    period.id <- names(periods)[i]
    period.name <- unname(periods[i])
    
    g <- svg_node('g',svg, c(id=period.id, transform=sprintf("translate(%s,%s)",l.m+(i-1)*(box.w+gap.s), t.m)))
    y[[period.name]][1] <- 0
    t <- 1
    fish.sum.period <- fish.sum %>% filter(time.period == period.name) %>% select(-time.period)
    period.data <- unlist(fish.sum.period)
    for (type in types){
      h[[period.name]][t] <- period.data[[type]]*scale
      svg_node('rect',g,c(width=box.w, height=h[[period.name]][t], y=y[[period.name]][t], fill=colors[t], opacity="0.6"))
      if (period.data[[type]] < n.threshold[1]){
        svg_node('text',g, c(x=box.w/2, y=y[[period.name]][t], dy="-3", fill='black', stroke='none', 'text-anchor'='middle'), XML::newXMLTextNode(sprintf("%s (n=%s)",type, period.data[[type]])))
      } else if (period.data[[type]] > n.threshold[2]){
        if (type != 'Neither'){
          svg_node('text',g, c(class='medium-text', x=box.w/2, y=y[[period.name]][t]+h[[period.name]][t]/2, dy="-0.3em", fill='black', stroke='none', 'text-anchor'='middle'), XML::newXMLTextNode(sprintf("%s",type)))
          svg_node('text',g, c(class='medium-text', x=box.w/2, y=y[[period.name]][t]+h[[period.name]][t]/2, dy="0.9em", fill='black', stroke='none', 'text-anchor'='middle'), XML::newXMLTextNode(sprintf("(n=%s)",period.data[[type]])))
        } else {
          svg_node('text',g, c(class='medium-text', x=box.w/2, y=y[[period.name]][t]+h[[period.name]][t]/2, dy="0.33em", fill='black', stroke='none', 'text-anchor'='middle'), XML::newXMLTextNode(sprintf("%s (n=%s)",type, period.data[[type]])))
        }
        
      } else {
        svg_node('text',g, c(x=box.w/2, y=y[[period.name]][t]+h[[period.name]][t]/2, dy="0.33em", fill='black', stroke='none', 'text-anchor'='middle'), XML::newXMLTextNode(sprintf("%s (n=%s)",type, period.data[[type]])))
        
      }
      
      y[[period.name]][t+1] <- y[[period.name]][t]+box.s+h[[period.name]][t]
      t<-t+1
    }
    svg_node('text',g, c(x=box.w/2, dy='1em', class='big-text', y=y[[period.name]][t], fill='black', stroke='none', 'text-anchor'='middle'), XML::newXMLTextNode(period.txt[i]))
    y[[period.name]] <- y[[period.name]][1:4] # get rid of the last one
  }
  
  
  
  start.arrows <- list()
  # // arrows!
  for (i in 1:(length(periods)-1)){
    
    period.from <- unname(periods[i])
    from.i <- 1
    for (from.type in types){
      
      where.type <- which(unname(unlist(from.type == arrows[[i]][1]))) # this is the "FROM" type column
      to.i <- 1
      y.0 <- 0
      for (to.type in to.types){
        h <- arrows[[i]][[to.type]][where.type]*scale
        from.y.bot <- y[[period.from]][from.i]+h+y.0
        start.arrows[[period.from]][[from.type]][[to.type]] <- c(y1=from.y.bot, h = h)
        to.i <- to.i+1
        y.0  <- y.0 + h
      }
      from.i <- from.i+1
    }
    
  }
  
  # // arrows!
  for (i in 2:length(periods)){
    
    period.to <- unname(periods[i])
    period.from <- unname(periods[i-1])
    to.i <- 1
    
    for (to.type in to.types){
      
      
      from.i <- 1
      y.0 <- 0
      for (from.type in types){
        
        where.type <- which(unname(unlist(from.type == arrows[[i-1]][1]))) # this is the "FROM" type column
        h <- arrows[[i-1]][[to.type]][where.type]*scale
        to.y.bot <- y[[period.to]][to.i]+h+y.0
        start.arrows[[period.from]][[from.type]][[to.type]]<- c(start.arrows[[period.from]][[from.type]][[to.type]], y2= to.y.bot)
        
        from.i <- from.i+1
        y.0  <- y.0 + h
      }
      to.i <- to.i+1
      
    }
  }
  
  for (i in 1:2){
    period.from <- unname(periods[i])
    g <- svg_node('g',svg, c(id=paste0(period.from,'-arrow'), transform=sprintf("translate(%s,%s)",l.m+(i-1)*(box.w+gap.s), t.m)))
    from.i <- 1
    for (from.type in names(start.arrows[[period.from]])){
      for (to.type in names(start.arrows[[period.from]][[from.type]])){
        stc <- start.arrows[[period.from]][[from.type]][[to.type]]
        svg_node('path', g, c(d = sprintf("M%s,%s L%s,%s v-%s L%s,%s", box.w, stc[['y1']], box.w+gap.s, stc[['y2']], stc[['h']], box.w, stc[['y1']]-stc[['h']]), 
                              fill=colors[from.i], stroke='none', opacity="0.2"))
      }
      from.i <- from.i+1
    }
    
  }
  
  
  dinosvg:::write_svg(svg, file='sandbox/fish_change_GH_paper.svg')
  
}
