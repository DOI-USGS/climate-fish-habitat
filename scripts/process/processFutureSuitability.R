
library(dplyr)

processData.processFutureSuitability <- function(futureSuitability, outfile, ...){

  area.factor = 0.000247105 # in acres
  fish.change <- futureSuitability %>% 
    rename(X1989.2014 = `1989-2014`, X2040.2064 = `2040-2064`, X2065.2089 = `2065-2089`) %>% 
    filter(!is.na(X1989.2014) & !is.na(X2040.2064) & !is.na(X2065.2089)) %>% 
    mutate(X1989.2014 = as.factor(X1989.2014), X2040.2064 = as.factor(X2040.2064),
           X2065.2089 = as.factor(X2065.2089), area = mda.lakes::getArea(WBIC)) %>% select(WBIC, area, everything())

  summarize_lakes <- function(period.name, fish.change){
    fish.change %>% group_by_(period.name) %>% tally %>% ungroup() %>% 
      tidyr::spread_(key=period.name, 'n') %>% mutate(time.period = period.name)
  }
  summarize_area <- function(period.name, fish.change){
    fish.change %>% group_by_(period.name) %>% summarize(acreage=sum(area)*area.factor) %>% 
      tidyr::spread_(key=period.name, 'acreage') %>% mutate(time.period = period.name)
  }
  
  fish.change.summary.area <- lapply(names(fish.change)[c(-1,-2)], 
         FUN=summarize_area, fish.change=fish.change)
  fish.change.summary.area <- bind_rows(fish.change.summary.area)
  fish.change.summary.count <- lapply(names(fish.change)[c(-1,-2)], 
                                     FUN=summarize_lakes, fish.change=fish.change)
  fish.change.summary.count <- bind_rows(fish.change.summary.count)
  
  area.arrows.1 <- group_by(fish.change, X1989.2014) %>% 
    summarize(toCoexistence = sum(area[X2040.2064 == 'Coexistence']*area.factor), toWally=sum(area[X2040.2064 == 'Walleye dominant']*area.factor), 
              toBass = sum(area[X2040.2064 == 'Bass dominant']*area.factor), toNeither=sum(area[X2040.2064 == 'Neither']*area.factor)) %>% 
    ungroup() %>% 
    mutate(changeFrom = 'X1989.2014') %>% 
    rename(fish.dominant = X1989.2014)
  
  count.arrows.1 <- group_by(fish.change, X1989.2014) %>%
    summarize(toCoexistence=sum(X2040.2064 == 'Coexistence'),toWally=sum(X2040.2064 == 'Walleye dominant'),
              toBass = sum(X2040.2064 == 'Bass dominant'), toNeither=sum(X2040.2064 == 'Neither')) %>%
    ungroup() %>%
    mutate(changeFrom = 'X1989.2014') %>%
    rename(fish.dominant = X1989.2014)
  
  area.arrows.2 <- group_by(fish.change, X2040.2064) %>% 
    summarize(toCoexistence = sum(area[X2065.2089 == 'Coexistence']*area.factor), toWally=sum(area[X2065.2089 == 'Walleye dominant']*area.factor), 
              toBass = sum(area[X2065.2089 == 'Bass dominant']*area.factor), toNeither=sum(area[X2065.2089 == 'Neither']*area.factor)) %>% 
    ungroup() %>% 
    mutate(changeFrom = 'X2040.2064') %>% 
    rename(fish.dominant = X2040.2064)
  
  count.arrows.2  <- group_by(fish.change, X2040.2064) %>%
    summarize(toCoexistence=sum(X2065.2089 == 'Coexistence'),toWally=sum(X2065.2089 == 'Walleye dominant'),
              toBass = sum(X2065.2089 == 'Bass dominant'), toNeither=sum(X2065.2089 == 'Neither')) %>%
    ungroup() %>%
    mutate(changeFrom = 'X2040.2064') %>%
    rename(fish.dominant = X2040.2064)
  
  fish.change.list <- list(area.fish.change.summary = fish.change.summary.area, count.fish.change.summary = fish.change.summary.count,
                           count.arrows.1 = count.arrows.1, count.arrows.2 = count.arrows.2,
                           area.arrows.1 = area.arrows.1, area.arrows.2 = area.arrows.2)

  saveRDS(fish.change.list, file = outfile)
}
