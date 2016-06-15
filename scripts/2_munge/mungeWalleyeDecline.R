
munge.mungeWallyTrends <- function(item){
  fname <- getData(item)
  wally <- readData(fname)
  wally.munged <- wally %>% 
    rename(rel.abun = relative.density) %>% 
    filter(Year>1990) 
  write.table(wally.munged, file = 'mungedWallyTrends.tsv', sep = '\t')
}

munge.mungeBassTrends <- function(item){
  fname <- getData(item)
  bass <- readData(fname)
  bass.munged <- bass %>% 
    rename(rel.abun = relative.abundance) %>% 
    filter(Year>1990) 
  write.table(bass.munged, file = 'mungedBassTrends.tsv', sep = '\t')
}
