morloc_idr <- function(x) x

morloc_toFst <- function(f, x) list(f(x), x)
morloc_toSnd <- function(f, x) list(x, f(x))

morloc_elem <- function(x, xs){
    x %in% xs
}

morloc_run <- function(f){
    f()
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

# morloc is 0-based and R is 1-based, so indices need to be adjusted
morloc_at <- function(i, xs){
  if (is.list(xs)){
    xs[[i+1]]
  } else {
    xs[i+1]
  }
}

# NOTE: i and j are expected to be 0-based
morloc_slice <- function(i, j, xs){
  if((i+1) > j || (i+1) > length(xs)){
    xs[0]
  } else {
    xs[(i+1):min(j, length(xs))]
  }
}


# TODO: could I pass type info from Morloc to run the right mapping function?
# Alternatively, perhaps typeclasses could do it?
morloc_map <- function(f, xs) {

  if (length(xs) == 0) {
    return(list())
  }

  # Use vapply if we can determine output type from first element
  first_result <- f(xs[[1]])
  
  if (length(first_result) == 1 && is.atomic(first_result)) {
    # Use vapply for type-safe vectorized operation
    if (is.logical(first_result)) {
      return(vapply(xs, f, FUN.VALUE = logical(1)))
    } else if (is.integer(first_result)) {
      return(vapply(xs, f, FUN.VALUE = integer(1)))
    } else if (is.double(first_result) || is.numeric(first_result)) {
      return(vapply(xs, f, FUN.VALUE = numeric(1)))
    } else if (is.character(first_result)) {
      return(vapply(xs, f, FUN.VALUE = character(1)))
    } else if (is.raw(first_result)) {
      return(vapply(xs, f, FUN.VALUE = raw(1)))
    } else if (is.complex(first_result)) {
      return(vapply(xs, f, FUN.VALUE = complex(1)))
    }
  }
  
  # Fallback to lapply for complex outputs
  return(lapply(xs, f))
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

morloc_not <- function(x){
	!x
}

morloc_and <- function(x, y){
	x && y
}

morloc_or <- function(x, y){
	x || y
}

### Claude Sonnet 4.5
#' Deep Equality Comparison for R Objects
#'
#' Recursively compares two R objects for equality, ignoring attributes.
#' Uses vectorized operations where possible for efficiency.
#'
#' @param x First object to compare
#' @param y Second object to compare
#' @return Logical scalar: TRUE if objects are equal, FALSE otherwise
#'
#' @details
#' Design choices:
#' - Ignores all attributes (class, names, dim, etc.) - compares data only
#' - Uses identical() for atomic vectors (fast, vectorized)
#' - Recursively handles lists element-by-element
#' - Short-circuits on length/type mismatches for efficiency
#' - Handles NULL, NA, NaN, Inf consistently
#'
#' Edge cases handled:
#' - NULL values: NULL == NULL is TRUE
#' - Empty vectors/lists: c() == c() is TRUE, list() == list() is TRUE
#' - NA values: NA == NA is TRUE (unlike base ==)
#' - NaN values: NaN == NaN is TRUE
#' - Mixed types: c(1,2,3) != list(1,2,3) (different types)
#' - Named vs unnamed: c(a=1) == c(1) is TRUE (ignores names)
#' - Nested structures: Recursively compared
#' - Different length: Always FALSE, no recycling
morloc_eq <- function(x, y) {
  # Strip all attributes to compare data only
  x <- unclass(x)
  y <- unclass(y)
  attributes(x) <- NULL
  attributes(y) <- NULL

  # Handle NULL cases first
  if (is.null(x) && is.null(y)) return(TRUE)
  if (is.null(x) || is.null(y)) return(FALSE)

  # Check type compatibility
  # typeof() gives the internal storage type
  type_x <- typeof(x)
  type_y <- typeof(y)

  if (type_x != type_y) return(FALSE)

  # Check lengths match (required for element-wise comparison)
  len_x <- length(x)
  len_y <- length(y)

  if (len_x != len_y) return(FALSE)

  # Handle empty objects
  if (len_x == 0) return(TRUE)

  # For atomic vectors (numeric, character, logical, etc.):
  # Use identical() which is vectorized and handles NA/NaN correctly
  # identical() is much faster than element-wise comparison
  if (is.atomic(x)) {
    return(identical(x, y))
  }

  # For lists: must compare element-by-element recursively
  # This is necessary because lists can contain heterogeneous types
  if (is.list(x)) {
    # Use vectorized all() over sapply for efficiency
    # sapply is still needed because list elements may differ in type
    return(all(sapply(seq_along(x), function(i) {
      morloc_eq(x[[i]], y[[i]])
    })))
  }

  # For other types (functions, environments, etc.):
  # Fall back to identical()
  return(identical(x, y))
}
