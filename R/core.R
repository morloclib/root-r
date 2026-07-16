morloc_idr <- function(x) x

# --- Boolean operations ---

morloc_not <- function(x) !x

morloc_size_str <- function(x){
  nchar(x)
}

morloc_size_vec <- function(x){
  length(x)
}

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

  # Empty values of any representation -- NULL, integer(0), numeric(0),
  # list(), character(0) -- all have length 0 and represent the same
  # morloc value @[]@. Treat any two length-zero values as equal,
  # regardless of type. This matters because the R codegen emits the
  # morloc literal @[]@ as @c()@ (which is @NULL@) while a length-zero
  # subscript on a typed vector keeps that vector's type
  # (@numeric(0)@, @integer(0)@, ...); the slicer-vs-literal comparison
  # @.[a:b] xs == []@ depends on these comparing equal.
  len_x <- length(x)
  len_y <- length(y)
  if (len_x == 0 && len_y == 0) return(TRUE)
  if (is.null(x) || is.null(y)) return(FALSE)

  # Check type compatibility — allow integer/double comparison since
  # morloc Int may arrive as R double (from 64-bit serialization) or
  # as R integer (from literals like 0L).
  type_x <- typeof(x)
  type_y <- typeof(y)

  if (type_x != type_y) {
    numeric_types <- c("integer", "double")
    if (!(type_x %in% numeric_types && type_y %in% numeric_types)) return(FALSE)
  }

  # Length mismatch (the both-zero case is already handled above).
  if (len_x != len_y) return(FALSE)

  # For atomic vectors: use identical() which handles NA/NaN correctly.
  # When types differ (integer vs double from 64-bit Int serialization),
  # fall back to == which compares values across numeric types.
  if (is.atomic(x)) {
    if (type_x == type_y) {
      return(identical(x, y))
    } else {
      return(all(x == y))
    }
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

# --- Arithmetic operations ---

morloc_neg <- function(x) (-1) * x
morloc_abs <- function(x) abs(x)

morloc_inv <- function(x) 1 / x
morloc_ln <- function(x) log(x)

morloc_to_real <- function(x) as.numeric(x)

# --- Numeric conversions ---
#
# TotalInto: one function per R runtime type. as.integer/as.numeric/as.integer64
# subsume the entire lossless-widening lattice because R has only three numeric
# runtimes (integer = 32-bit, numeric = double, integer64 via bit64).

morloc_into_integer <- function(x) as.integer(x)
morloc_into_numeric <- function(x) as.numeric(x)
morloc_into_bit64   <- function(x) bit64::as.integer64(x)

# PartialInto: bounds-checked conversions to each morloc target. NULL for the
# ?a error sentinel. Float sources must be finite and integer-valued.

.morloc_try_int <- function(x, lo, hi, cast){
  if (is.null(x) || length(x) == 0) return(NULL)
  if (is.na(x)) return(NULL)
  if (is.double(x)){
    if (!is.finite(x)) return(NULL)
    if (x != floor(x)) return(NULL)
  }
  if (x < lo || x > hi) return(NULL)
  cast(x)
}

morloc_try_u8  <- function(x) .morloc_try_int(x, 0, 255, as.integer)
morloc_try_u16 <- function(x) .morloc_try_int(x, 0, 65535, as.integer)
morloc_try_u32 <- function(x) .morloc_try_int(x, 0, 4294967295, as.numeric)
morloc_try_u64 <- function(x) .morloc_try_int(x, 0, 2^64 - 1, as.numeric)
morloc_try_i8  <- function(x) .morloc_try_int(x, -128, 127, as.integer)
morloc_try_i16 <- function(x) .morloc_try_int(x, -32768, 32767, as.integer)
morloc_try_i32 <- function(x) .morloc_try_int(x, -2147483648, 2147483647, as.numeric)
morloc_try_i64 <- function(x) .morloc_try_int(x, -2^63, 2^63 - 1, function(v) bit64::as.integer64(v))
morloc_try_int <- function(x) .morloc_try_int(x, -2147483648, 2147483647, as.integer)

morloc_float_mod <- function(x, y){
  x - y * floor(x / y)
}

# --- Sequence operations ---

# One helper for every IndexLike instance. R's native integer is 32-bit, so
# we deliberately use numeric (double) here to preserve bound values above
# 2^31 - 1; downstream code converts back to integer for seq/[ as needed.
morloc_to_index <- function(x) {
  # Returns NULL or numeric. NULL passes through so slicer bounds can be
  # left blank (the desugar emits `(Null :: ?Int64)` for an empty
  # position, and a user expression evaluating to NULL composes the
  # same way).
  if (is.null(x)) NULL else as.numeric(x)
}

# morloc is 0-based and R is 1-based, so indices need to be adjusted.
# Negative indices wrap from the end (Python semantics): .[-1] is the last
# element, .[-2] the second-to-last, etc.
morloc_at <- function(i, xs) {
  # __access_index__ takes ?Int64 to match __to_index__'s return shape,
  # but a NULL index has no semantic meaning at runtime.
  if (is.null(i)) stop("morloc_at: index is NULL")
  if (i < 0) i <- i + length(xs)
  if (is.list(xs)) {
    xs[[i + 1]]
  } else {
    xs[i + 1]
  }
}

# Python-style slice in 0-based indexing. NULL bounds default per step sign.
# Bound arithmetic stays in `numeric` (double) to keep precision above 2^31 - 1
# where R's primitive `integer` would silently truncate; the conversion back to
# integer happens only at the final subscript step.
morloc_slice <- function(start, stop, step, xs) {
  n <- as.numeric(length(xs))
  k <- if (is.null(step)) 1 else as.numeric(step)
  if (k == 0) stop("slice step cannot be zero")
  if (k > 0) {
    i <- if (is.null(start)) 0 else as.numeric(start)
    j <- if (is.null(stop))  n else as.numeric(stop)
  } else {
    i <- if (is.null(start)) n - 1 else as.numeric(start)
    j <- if (is.null(stop))  -1    else as.numeric(stop)
  }
  # Negative indices wrap from the end (only when the bound was supplied).
  if (i < 0 && !is.null(start)) i <- i + n
  if (j < 0 && !is.null(stop))  j <- j + n
  # Clamp positive-step bounds to valid range.
  if (k > 0) {
    i <- max(0, min(i, n))
    j <- max(0, min(j, n))
  } else {
    i <- max(-1, min(i, n - 1))
    j <- max(-1, min(j, n - 1))
  }
  # 0-based positions iterating from i toward j by k.
  idx <- if (k > 0) {
    if (i >= j) numeric(0) else seq(i, j - 1, by = k)
  } else {
    if (i <= j) numeric(0) else seq(i, j + 1, by = k)
  }
  if (length(idx) == 0) {
    xs[0]
  } else {
    xs[idx + 1]
  }
}

morloc_map <- function(f, xs) {
  n <- length(xs)
  if (n == 0) {
    return(list())
  }

  # Probe the first element to choose the output container. The previous
  # implementation then called `vapply(xs, f, ...)` which re-evaluated the
  # first element, doubling its cost for any non-trivial `f`. Skip index 1
  # in the vapply/lapply pass and prepend `first_result` instead.
  first_result <- f(xs[[1]])

  if (length(first_result) == 1 && is.atomic(first_result)) {
    if (n == 1L) {
      return(first_result)
    }
    fun_value <- vector(typeof(first_result), 1L)
    rest <- vapply(xs[-1L], f, FUN.VALUE = fun_value)
    return(c(first_result, rest))
  }

  if (n == 1L) {
    return(list(first_result))
  }
  rest <- lapply(xs[-1L], f)
  return(c(list(first_result), rest))
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

# half-open interval [a, b), like Python's range and root's slice
morloc_range <- function(a, b) {
  if (a >= b) return(list())
  as.list(a:(b - 1))
}

morloc_rangeStep <- function(a, b, step) {
  if (a >= b) return(list())
  as.list(seq(a, b - 1, by = step))
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

