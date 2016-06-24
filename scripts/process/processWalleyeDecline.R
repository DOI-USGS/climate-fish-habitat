
library(dplyr)

processData.processWallyTrends <- function(wallyTrends, outfile, ...){
  wally.processed <- wallyTrends %>% 
    rename(rel.abun = relative.density) %>% 
    filter(Year>1990) 
  write.table(wally.processed, file = outfile, sep = '\t', row.names = FALSE)
}

processData.processBassTrends <- function(bassTrends, outfile, ...){
  bass.processed <- bassTrends %>% 
    rename(rel.abun = relative.abundance) %>% 
    filter(Year>1990) 
  write.table(bass.processed, file = outfile, sep = '\t', row.names = FALSE)
}
