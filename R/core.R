morloc_idr <- function(x) x

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

morloc_inv <- function(x) 1 / x
morloc_ln <- function(x) log(x)

morloc_float_mod <- function(x, y){
  x - y * floor(x / y)
}

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

morloc_fold1 <- function(f, xs) {
  acc <- xs[[1]]
  if (length(xs) > 1) {
    for (i in 2:length(xs)) {
      acc <- f(acc, xs[[i]])
    }
  }
  acc
}

morloc_safeFold1 <- function(f, xs) {
  if (length(xs) == 0) return(NULL)
  acc <- xs[[1]]
  if (length(xs) > 1) {
    for (i in 2:length(xs)) {
      acc <- f(acc, xs[[i]])
    }
  }
  acc
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

# --- Stack operations ---

morloc_cons <- function(x, xs) {
  c(list(x), xs)
}

morloc_uncons <- function(xs) {
  list(xs[[1]], xs[-1])
}

# --- Queue operations ---

morloc_snoc <- function(xs, x) {
  c(xs, list(x))
}

morloc_unsnoc <- function(xs) {
  n <- length(xs)
  list(xs[-n], xs[[n]])
}

# --- New list operations ---

morloc_iterate <- function(n, f, x) {
  result <- list()
  for (i in seq_len(n)) {
    result <- c(result, list(x))
    x <- f(x)
  }
  result
}

morloc_groupBy <- function(eq, xs) {
  if (length(xs) == 0) return(list())
  result <- list()
  group <- list(xs[[1]])
  if (length(xs) > 1) {
    for (i in 2:length(xs)) {
      if (eq(xs[[i-1]], xs[[i]])) {
        group <- c(group, list(xs[[i]]))
      } else {
        result <- c(result, list(group))
        group <- list(xs[[i]])
      }
    }
  }
  result <- c(result, list(group))
  result
}

morloc_find <- function(f, xs) {
  for (x in xs) {
    if (f(x)) return(x)
  }
  return(NULL)
}

morloc_unique <- function(xs) {
  result <- list()
  seen <- list()
  for (x in xs) {
    found <- FALSE
    for (s in seen) {
      if (morloc_eq(s, x)) {
        found <- TRUE
        break
      }
    }
    if (!found) {
      seen <- c(seen, list(x))
      result <- c(result, list(x))
    }
  }
  result
}

morloc_groupSort <- function(xs) {
  if (length(xs) == 0) return(list())
  keys <- sapply(xs, function(x) x[[1]])
  vals <- lapply(xs, function(x) x[[2]])
  unique_keys <- sort(unique(keys))
  result <- list()
  for (k in unique_keys) {
    group_vals <- vals[keys == k]
    result <- c(result, list(list(k, group_vals)))
  }
  result
}

morloc_range <- function(a, b) {
  if (a > b) return(list())
  as.list(a:b)
}

morloc_rangeStep <- function(a, b, step) {
  if (a > b) return(list())
  as.list(seq(a, b, by = step))
}

# --- Readable operations ---

morloc_read_int <- function(s) {
  result <- suppressWarnings(as.integer(s))
  if (is.na(result)) return(NULL)
  result
}

morloc_read_real <- function(s) {
  result <- suppressWarnings(as.numeric(s))
  if (is.na(result)) return(NULL)
  result
}

morloc_read_str <- function(s) {
  s
}

morloc_read_bool <- function(s) {
  if (s %in% c("true", "True", "TRUE")) return(TRUE)
  if (s %in% c("false", "False", "FALSE")) return(FALSE)
  return(NULL)
}

# --- Sequence conversions ---
# In R, all sequence types map to list, so these are identity functions

morloc_toDeque <- function(xs) xs
morloc_toVector <- function(xs) xs
morloc_toArray <- function(xs) xs

