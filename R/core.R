mlc_fst <- function(x) x[[1]]
mlc_snd <- function(x) x[[2]]
mlc_thr <- function(x) x[[3]]

mlc_elem <- function(x, xs){
    x %in% xs
}

mlc_run <- function(f){
    f()
}

mlc_id <- function(x){
  x
}

mlc_seq <- function(x, y){
  y
}

mlc_const <- function(x, y){
  x
}

mlc_at <- function(i, xs){
  if (is.list(xs)){
    xs[[i]]
  } else {
    xs[i]
  }
}

mlc_map <- function(f, xs){
  sapply(xs, f)  
}

# [a] -> a
mlc_head <- function(xs) {
  if(length(xs) == 0){
    stop("Empty list in head operation")
  }
  return(xs[[1]])
}

# [a]_{n} -> [a_{n-1}
mlc_tail <- function(xs) {
  if(length(xs) == 0){
    stop("Empty list in tail operation")
  }
  return(xs[-1])
}

# [a] -> a
mlc_last <- function(xs) {
  if(length(xs) == 0){
    stop("Empty list in last operation")
  }
  return(tail(xs, n = 1)[[1]])
}

# i:Int -> [a]_{n>i} -> [a]_{n=i}
mlc_take <- function(i, xs) {
  if(i < 1){
    # this operation returns an empty sequence while preserving the type
    return(tail(xs, n=0))
  }
  return(xs[1:i])
}

# i:Int -> [a]_{n; n>i} -> [a]_{m; m <= n-i}
mlc_drop <- function(i, xs) {
  if(i < 1){
    # this operation returns an empty sequence while preserving the type
    return(xs)
  }
  return(xs[-(1:i)])
}

#  [a]_{n>i} -> [a]_{n-i}
mlc_init <- function(xs) {
  if(length(xs) == 0){
    stop("Empty list in init operation")
  }
  return(xs[-length(xs)])
}

mlc_zipWith <- function(f, xs, ys){
  N <- min(length(xs), length(ys))
  zs <- as.list(rep(NA, N))
  for(i in seq_along(xs)){
    zs[[i]] <- f(xs[[i]], ys[[i]])
  }
  return(zs)
}

mlc_enumerateWith <- function(f, xs){
  ys <- list()
  for(i in seq_along(xs)){
    ys[[i]] <- f(xs[[i]], i)
  }
  return(ys)
}

mlc_fold <- function(f, init, xs){
  Reduce(f=f, x=xs, init=init, accumulate=FALSE)
}

mlc_scan <- function(f, init, xs){
  Reduce(f=f, x=xs, init=init, accumulate=TRUE)
}

mlc_join <- function(xs, ys){
  c(xs, ys)
}

mlc_add <- function(x,y) x + y
mlc_sub <- function(x,y) x - y
mlc_mul <- function(x,y) x * y
mlc_div <- function(x,y) x / y
mlc_neg <- function(x) (-1) * x

plotPDF <- function(x, filename){
  pdf(filename)
  plot(x)
  dev.off()
  NULL
}

mlc_plotVectorPDF <- function(...){
  plotPDF(...)
}



mlc_gt <- function(x, y){
	x > y
}

mlc_lt <- function(x, y){
	x < y
}

mlc_ge <- function(x, y){
	x >= y
}

mlc_le <- function(x, y){
	x <= y
}

mlc_eq <- function(x, y){
	x == y
}

mlc_ne <- function(x, y){
	x != y
}

mlc_not <- function(x){
	!x
}

mlc_and <- function(x, y){
	x && y
}

mlc_or <- function(x, y){
	x || y
}
