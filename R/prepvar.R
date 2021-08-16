#' Prepare a variable for fitting transformation and dealing with NA and zero
#'
#' @param x the variable to transform
#' @param na_val the value to convert NAs to
#'   "auto" will select a value well below the range of the rest
#' @param zero_val the value to conver zeros to.
#'   "zero" treat zeros as any other value. "auto" will select a
#'   value well below the range of the rest
#' @param trans_fn function to transform other values with, e.g., `log10`
#'
#' @return A vector of length x
#' @export
#'
#' @examples
#' set.seed(0)
#' z <- round(abs(rnorm(1000, sd = 4)))
#' z[sample(1:1000, 100)] <- NA
#' p <- prepvar(z, na_val = "auto", zero_val = "auto", trans_fn = log)
#' hist(p)
prepvar <- function(x, na_val = "auto", zero_val = "zero", trans_fn = identity) {
  nlocs <- is.na(x) | is.infinite(x)
  zlocs <- !nlocs & x == 0
  
  if(zero_val == "zero")
    rlocs <- !nlocs
  else
    rlocs <- !(nlocs | zlocs)
  
  x[rlocs] <- trans_fn(x[rlocs])
  
  if (na_val == "auto")
    x[nlocs] <- min(x[rlocs]) - 6*sd(x[rlocs])
  else
    x[nlocs] <- na_val
  
  if (zero_val == "auto")
    x[zlocs] <- min(x[rlocs]) - 3*sd(x[rlocs])
  else if (zero_val != "zero")
    x[zlocs] <- zero_val
  
  return(x)
}