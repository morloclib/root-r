morloc_idr <- function(x) x

morloc_toFst <- function(f, x) list(f(x), x)
morloc_toSnd <- function(f, x) list(x, f(x))

# --- Boolean operations ---

morloc_not <- function(x) !x
morloc_and <- function(x, y) x && y
morloc_or <- function(x, y) x || y

# --- Comparison operations ---

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
  type_x <- typeof(x)
  type_y <- typeof(y)

  if (type_x != type_y) return(FALSE)

  # Check lengths match
  len_x <- length(x)
  len_y <- length(y)

  if (len_x != len_y) return(FALSE)

  # Handle empty objects
  if (len_x == 0) return(TRUE)

  # For atomic vectors: use identical() which handles NA/NaN correctly
  if (is.atomic(x)) {
    return(identical(x, y))
  }

  # For lists: compare element-by-element recursively
  if (is.list(x)) {
    return(all(sapply(seq_along(x), function(i) {
      morloc_eq(x[[i]], y[[i]])
    })))
  }

  # Fallback
  return(identical(x, y))
}

morloc_le <- function(x, y) x <= y

# --- Control flow ---

morloc_ifelse <- function(cond, x, y) {
  if (cond) {
    x
  } else {
    y
  }
}

morloc_branch <- function(cond, fa, fb, x) {
  if (cond(x)) {
    fa(x)
  } else {
    fb(x)
  }
}

# --- Arithmetic operations ---

morloc_neg <- function(x) (-1) * x
morloc_abs <- function(x) abs(x)
morloc_add <- function(x, y) x + y
morloc_sub <- function(x, y) x - y
morloc_mul <- function(x, y) x * y
morloc_intdiv <- function(x, y) x %/% y
morloc_mod <- function(x, y) x %% y

morloc_inv <- function(x) 1 / x
morloc_div <- function(x, y) x / y
morloc_pow <- function(x, y) x ^ y
morloc_ln <- function(x) log(x)

morloc_show <- function(x) as.character(x)

# --- Sequence operations ---

# morloc is 0-based and R is 1-based, so indices need to be adjusted
morloc_at <- function(i, xs) {
  if (is.list(xs)) {
    xs[[i + 1]]
  } else {
    xs[i + 1]
  }
}

# NOTE: i and j are expected to be 0-based
morloc_slice <- function(i, j, xs) {
  if ((i + 1) > j || (i + 1) > length(xs)) {
    xs[0]
  } else {
    xs[(i + 1):min(j, length(xs))]
  }
}

morloc_map <- function(f, xs) {
  if (length(xs) == 0) {
    return(list())
  }

  first_result <- f(xs[[1]])

  if (length(first_result) == 1 && is.atomic(first_result)) {
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

  return(lapply(xs, f))
}

morloc_sort <- function(xs) {
  if (length(xs) <= 1) return(xs)
  if (is.list(xs)) {
    if (length(xs) > 0 && is.list(xs[[1]])) {
      keys <- sapply(xs, function(x) x[[1]])
      idx <- order(keys)
      result <- vector("list", length(xs))
      for (i in seq_along(idx)) {
        result[[i]] <- xs[[idx[i]]]
      }
      return(result)
    }
    return(xs[order(unlist(xs))])
  } else {
    return(sort(xs))
  }
}

morloc_sortBy <- function(cmp, xs) {
  if (length(xs) <= 1) return(xs)
  n <- length(xs)
  result <- xs
  # Bubble sort with custom comparator
  for (i in 1:(n - 1)) {
    for (j in 1:(n - i)) {
      if (!cmp(result[[j]], result[[j + 1]])) {
        temp <- result[[j]]
        result[[j]] <- result[[j + 1]]
        result[[j + 1]] <- temp
      }
    }
  }
  result
}

morloc_zipWith <- function(f, xs, ys) {
  N <- min(length(xs), length(ys))
  zs <- as.list(rep(NA, N))
  for (i in seq_len(N)) {
    zs[[i]] <- f(xs[[i]], ys[[i]])
  }
  return(zs)
}

morloc_fold <- function(f, init, xs) {
  Reduce(f = f, x = xs, init = init, accumulate = FALSE)
}

morloc_unzip <- function(xs) {
  if (length(xs) == 0) return(list(list(), list()))
  a <- lapply(xs, function(x) x[[1]])
  b <- lapply(xs, function(x) x[[2]])
  list(a, b)
}

morloc_replicate <- function(n, x) {
  rep(list(x), n)
}

morloc_takeWhile <- function(f, xs) {
  result <- list()
  for (x in xs) {
    if (!f(x)) break
    result <- c(result, list(x))
  }
  result
}

morloc_dropWhile <- function(f, xs) {
  dropping <- TRUE
  result <- list()
  for (x in xs) {
    if (dropping && f(x)) next
    dropping <- FALSE
    result <- c(result, list(x))
  }
  result
}

morloc_partition <- function(f, xs) {
  yes <- list()
  no <- list()
  for (x in xs) {
    if (f(x)) {
      yes <- c(yes, list(x))
    } else {
      no <- c(no, list(x))
    }
  }
  list(yes, no)
}

morloc_scanl <- function(f, init, xs) {
  result <- list(init)
  acc <- init
  for (x in xs) {
    acc <- f(acc, x)
    result <- c(result, list(acc))
  }
  result
}

morloc_intersperse <- function(sep, xs) {
  if (length(xs) <= 1) return(xs)
  result <- list()
  for (i in seq_along(xs)) {
    if (i > 1) result <- c(result, list(sep))
    result <- c(result, list(xs[[i]]))
  }
  result
}

morloc_enumerate <- function(xs) {
  lapply(seq_along(xs), function(i) list(i - 1L, xs[[i]]))
}

morloc_str_add <- function(x, y) paste0(x, y)

# --- Map operations ---

# WARNING: The Map implementation in R is limited. R doesn't have a general
# map type. The named `list` type works properly only for string keys.

morloc_keys <- function(m) {
  as.list(names(m))
}

morloc_vals <- function(m) {
  unname(as.list(m))
}

morloc_lookup <- function(key, m) {
  m[[key]]
}

morloc_insert <- function(key, val, m) {
  result <- m
  result[[key]] <- val
  result
}

morloc_delete <- function(key, m) {
  result <- m
  result[[key]] <- NULL
  result
}

morloc_from_list <- function(xs) {
  result <- list()
  for (pair in xs) {
    result[[pair[[1]]]] <- pair[[2]]
  }
  result
}

morloc_to_list <- function(m) {
  lapply(names(m), function(k) list(k, m[[k]]))
}

morloc_map_key <- function(f, m) {
  result <- list()
  for (k in names(m)) {
    result[[f(k)]] <- m[[k]]
  }
  result
}

morloc_map_val <- function(f, m) {
  result <- list()
  for (k in names(m)) {
    result[[k]] <- f(m[[k]])
  }
  result
}

morloc_filter_map <- function(f, m) {
  result <- list()
  for (k in names(m)) {
    if (f(k, m[[k]])) {
      result[[k]] <- m[[k]]
    }
  }
  result
}
