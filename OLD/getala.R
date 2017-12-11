 download.file("http://biocache.ala.org.au/ws/occurrences/download?q=Banksia+ericifolia", "data.zip")
 
 
   library(RCurl)
  z = getURLContent("http://biocache.ala.org.au/ws/occurrences/download?q=Banksia+ericifolia")
  attributes(z)

  library(Rcompression)
  ar = zipArchive(z)
  names(ar)
  getZipInfo(ar)
  ar[["data.csv"]]
  dd = read.csv(textConnection(ar[["data.csv"]]))