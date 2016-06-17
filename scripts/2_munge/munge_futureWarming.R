#mungeFutureWarming
munge.mungeFutureWarming <- function(item){
  fname <- getData(item)
  
  futures <- unzip(fname, exdir=tempdir())
  futures.munged <- #TODO:load and munge data here
    
  write.table(futures.munged, file = 'mungedFutureWarming.tsv', sep = '\t')
  
}
  
  