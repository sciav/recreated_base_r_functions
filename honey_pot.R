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
  par_min <- apply(as.data.frame(new_args), 1, min, na.rm = ifelse(na.rm == TRUE, TRUE, FALSE))
  par_min[(1:length(par_min)) <= max(lengths)]  
}

my_pmax <- function(..., na.rm = FALSE) {
  lengths <- sapply(list(...), length)
  # find lcm of lengths for recycling
  mult <- outer(sort(lengths, dec = TRUE), sort(lengths, dec = TRUE), "/")
  if (!identical(mult[col(mult) > row(mult)] %/% 1, mult[col(mult) > row(mult)])) {
    warning("an argument will be fractionallу recуcled")
  }
  big_length <- prod(lengths)
  new_args <- lapply(seq_along(list(...)),
                     function(i) {
                       rep(list(...)[[i]], (big_length / lengths)[i])
                     })
  par_max <- apply(as.data.frame(new_args), 1, max, na.rm = ifelse(na.rm == TRUE, TRUE, FALSE))
  par_max[(1:length(par_max)) <= max(lengths)]  # keep only as many as longest
}

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