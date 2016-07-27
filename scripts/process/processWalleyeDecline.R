
library(dplyr)

processData.processWallyTrends <- function(wallyTrends, outfile, ...){
  wally.processed <- wallyTrends %>% 
    rename(recruitment = mean.cpue, Year = year) %>% 
    filter(Year>1990) %>% select(Year, recruitment)
  write.table(wally.processed, file = outfile, sep = '\t', row.names = FALSE)
}

processData.processBassTrends <- function(bassTrends, outfile, ...){
  bass.processed <- bassTrends %>% 
    rename(rel.abun = relative.abundance) %>% 
    filter(Year>1990, Year < 2015) %>% select(Year, rel.abun)
  write.table(bass.processed, file = outfile, sep = '\t', row.names = FALSE)
}
