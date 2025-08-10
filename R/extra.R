mlc_plotPDF <- function(x, filename){
  pdf(filename)
  plot(x)
  dev.off()
  NULL
}

mlc_plotVectorPDF <- function(...){
  plotPDF(...)
}
