morloc_plotPDF <- function(x, filename){
  pdf(filename)
  plot(x)
  dev.off()
  NULL
}

morloc_plotVectorPDF <- function(...){
  plotPDF(...)
}
