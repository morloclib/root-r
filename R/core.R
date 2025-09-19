morloc_idr <- function(x) x

morloc_fst <- function(x) x[[1]]
morloc_snd <- function(x) x[[2]]
morloc_thr <- function(x) x[[3]]

morloc_toFst <- function(f, x) list(f(x), x)
morloc_toSnd <- function(f, x) list(x, f(x))

morloc_elem <- function(x, xs){
    x %in% xs
}

morloc_run <- function(f){
    f()
}

morloc_id <- function(x){
  x
}

morloc_seq <- function(x, y){
  y
}

morloc_const <- function(x, y){
  x
}

morloc_ifelse <- function(cond, x, y){
  if(cond){
    x
  } else {
    y
  }
}

morloc_at <- function(i, xs){
  if (is.list(xs)){
    xs[[i]]
  } else {
    xs[i]
  }
}

morloc_map <- function(f, xs){
  sapply(xs, f)  
}

morloc_at <- function(i, xs){
  xs[i]
}

morloc_slice <- function(i, j, xs){
  xs[i-1:min(j, length(xs))]
}

morloc_zipWith <- function(f, xs, ys){
  N <- min(length(xs), length(ys))
  zs <- as.list(rep(NA, N))
  for(i in seq_along(xs)){
    zs[[i]] <- f(xs[[i]], ys[[i]])
  }
  return(zs)
}

morloc_enumerateWith <- function(f, xs){
  ys <- list()
  for(i in seq_along(xs)){
    ys[[i]] <- f(xs[[i]], i)
  }
  return(ys)
}

morloc_fold <- function(f, init, xs){
  Reduce(f=f, x=xs, init=init, accumulate=FALSE)
}

morloc_scan <- function(f, init, xs){
  Reduce(f=f, x=xs, init=init, accumulate=TRUE)
}

morloc_join <- function(xs, ys){
  c(xs, ys)
}

morloc_neg <- function(x) (-1) * x
morloc_add <- function(x,y) x + y
morloc_sub <- function(x,y) x - y

morloc_inv <- function(x) 1 * x
morloc_mul <- function(x,y) x * y
morloc_div <- function(x,y) x / y
morloc_exp <- function(x,y) x ^ y
morloc_log <- function(x,y) log(x, base=y)

plotPDF <- function(x, filename){
  pdf(filename)
  plot(x)
  dev.off()
  NULL
}

morloc_plotVectorPDF <- function(...){
  plotPDF(...)
}


morloc_le <- function(x, y){
	x <= y
}

morloc_eq <- function(x, y){
	x == y
}

morloc_not <- function(x){
	!x
}

morloc_and <- function(x, y){
	x && y
}

morloc_or <- function(x, y){
	x || y
}
