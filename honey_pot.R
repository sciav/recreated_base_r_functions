# For assignment in Stats 14 - R Programming for Data Science at UC San Jose with Prof McCollum.

# Write a function called my_min() that computes the minimum of a numeric vector without the min() function.
# Include a logical argument called na.rm that specifies whether to remove NAs.
my_min <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  if (length(x) == 0) {
    minimum <- Inf
  } else {
    if (any(is.na(x))) {
      minimum <- NA
    } else {
      minimum <- x[1]
      if (length(x) > 1) {
        for (i in 2:length(x)) {
          if (x[i] < minimum) {
            minimum <- x[i]
          }
        }
      }
    }
  } 
  minimum
}

# Write a function called my_max() that computes the maximum of a numeric vector without the min() function.
# Include a logical argument called na.rm that specifies whether to remove NAs.
my_max <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  if (length(x) == 0) {
    maximum <- -Inf
  } else {
    if (any(is.na(x))) {
      maximum <- NA
    } else {
      maximum <- x[1]
      if (length(x) > 1) {
        for (i in 2:length(x)) {
          if (x[i] > maximum) {
            maximum <- x[i]
          }
        }
      }
    }
  } 
  maximum
}

# The pmin() function (the p stands for "parallel") takes any number of numeric or integer vector arguments (not necessarily of the same length) and returns the minimum value among all the input vectors at each index position.
# The function also takes an optional logical na.rm argument that specifies whether to remove NAs. If na.rm = TRUE but all the input vectors contain NA at the same index position, the pmin() function returns NA for that index.
my_pmin <- function(..., na.rm = FALSE) {
  lengths <- sapply(list(...), length)
  # find lcm lengths for recycling
  mult <- outer(sort(lengths, dec = TRUE), sort(lengths, dec = TRUE), "/")
  if (!identical(mult[col(mult) > row(mult)] %/% 1, mult[col(mult) > row(mult)])) {
    warning("an argument will be fractionallу recуcled")
  }
  big_length <- prod(lengths)
  new_args <- lapply(seq_along(list(...)),
                     function(i) {
                       rep(list(...)[[i]], (big_length / lengths)[i])
                     })
  par_min <- apply(as.data.frame(new_args), 1,
                   function(x, na.rm) {
                     ifelse(all(is.na(x)), NA, min(x, na.rm = na.rm))
                   }, na.rm = ifelse(na.rm == TRUE, TRUE, FALSE))
  par_min[(1:length(par_min)) <= max(lengths)]  # keep only as many as longest
}

# The pmax() function (the p stands for "parallel") takes any number of numeric or integer vector arguments (not necessarily of the same length) and returns the maximum value among all the input vectors at each index position.
# The function also takes an optional logical na.rm argument that specifies whether to remove NAs. If na.rm = TRUE but all the input vectors contain NA at the same index position, the pmin() function returns NA for that index.
my_pmax <- function(..., na.rm = FALSE) {
  lengths <- sapply(list(...), length)
  # find lcm lengths for recycling
  mult <- outer(sort(lengths, dec = TRUE), sort(lengths, dec = TRUE), "/")
  if (!identical(mult[col(mult) > row(mult)] %/% 1, mult[col(mult) > row(mult)])) {
    warning("an argument will be fractionallу recуcled")
  }
  big_length <- prod(lengths)
  new_args <- lapply(seq_along(list(...)),
                     function(i) {
                       rep(list(...)[[i]], (big_length / lengths)[i])
                     })
  par_max <- apply(as.data.frame(new_args), 1,
                   function(x, na.rm) {
                     ifelse(all(is.na(x)), NA, max(x, na.rm = na.rm))
                   }, na.rm = ifelse(na.rm == TRUE, TRUE, FALSE))
  par_max[(1:length(par_max)) <= max(lengths)]  # keep only as many as longest
}

# Write a function called my_quantile() that computes a specified quantile of a numeric vector without the quantile() function.
# Include a logical argument called na.rm that specifies whether to remove NAs.
my_quantile <- function(x, probs = seq(0, 1, 0.25), na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  } else {
    if (any(is.na(x))) {
      stop("No NA's or NaN's allowed if na.rm is FALSE")
    }
  }
  x <- sort(x)
  quantiles <- numeric(length = length(probs))
  for (i in 1:length(probs)) {
    percent <- numeric(0)
    for (j in 1:length(x)) {
      if (mean(x[j] >= x) > probs[i]) {
        percent <- c(percent, x[j])
      }
    }
    quantiles[i] <- percent[1]
  }
  if (length(probs) == 5 & probs[1] == 0 & probs[5] == 1) {
    quantiles[2] <- median(x[x < median(x)])
    quantiles[4] <- median(x[x > median(x)])
  }
  if (length(probs) == 5 & probs[5] == 0 & probs[1] == 1) {
    quantiles[4] <- median(x[x < median(x)])
    quantiles[2] <- median(x[x > median(x)])
  }
  if (max(probs) == 1){
    quantiles[which(probs == 1)] <- my_max(x)
  }
  if (min(probs) == 0) {
    quantiles[which(probs == 0)] <- my_min(x)
  }
  if (any(probs == 0.5)) {
    quantiles[which(probs == 0.5)] <- median(x)
  }
  names(quantiles) <- paste((probs * 100), "%")
  quantiles
}
