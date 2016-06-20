#processFutureWarming
processData.processFutureWarming <- function(futureTempsA2, futureTempsB1, futureTempsA1B, outfile, ...){

  futures.munged <- #TODO:load and munge data here
    
  write.table(futures.munged, file = outfile, sep = '\t')
  
}
  
  