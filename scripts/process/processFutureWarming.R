#processFutureWarming
processData.processFutureWarming <- function(folderFutureTemps, outfile, ...){
  # folderFutureTemps is a list of file names
  # create a readData.dat function??? or just do read.csv??
  futures.munged <- data.frame()
  #TODO:load and munge data here
  write.table(futures.munged, file = outfile, sep = '\t')
  
}
  
  