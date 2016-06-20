
processData.processFutureSuitability <- function(futureSuitability, outfile){

  fish.change <- futureSuitability %>% 
    filter(!is.na(X1989.2014) & !is.na(X2040.2064) & !is.na(X2065.2089)) %>% 
    mutate(X1989.2014 = as.factor(X1989.2014), X2040.2064 = as.factor(X2040.2064),
           X2065.2089 = as.factor(X2065.2089)) 

  fish.change.summary <- lapply(names(fish.change)[-1], 
         FUN=function(period.name, fish.change){
           fish.change %>% group_by_(period.name) %>% tally %>% ungroup() %>% 
             tidyr::spread_(key=period.name, 'n') %>% mutate(time.period = period.name)
         }, fish.change=fish.change)
  fish.change.summary <- rbind_all(fish.change.summary)
  
  arrows.1 <- group_by(fish.change, X1989.2014) %>% 
    summarize(toCoexistence=sum(X2040.2064 == 'Coexistence'),toWally=sum(X2040.2064 == 'Walleye dominant'), 
              toBass = sum(X2040.2064 == 'Bass dominant'), toNeither=sum(X2040.2064 == 'Neither')) %>% 
    ungroup() %>% 
    mutate(changeFrom = 'X1989.2014') %>% 
    rename(fish.dominant = X1989.2014) 
  
  arrows.2  <- group_by(fish.change, X2040.2064) %>% 
    summarize(toCoexistence=sum(X2065.2089 == 'Coexistence'),toWally=sum(X2065.2089 == 'Walleye dominant'), 
              toBass = sum(X2065.2089 == 'Bass dominant'), toNeither=sum(X2065.2089 == 'Neither')) %>% 
    ungroup() %>% 
    mutate(changeFrom = 'X2040.2064') %>% 
    rename(fish.dominant = X2040.2064)
  
  fish.change.list <- list(fish.change.summary = fish.change.summary, arrows.1 = arrows.1, arrows.2 = arrows.2)

  saveRDS(fish.change.list, file = outfile)
}
