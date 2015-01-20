# compute cumulative returns
cumret <- function(x) {
  # takes a vector x of returns as a proportion
  N <- length(x)
  out <- rep(NA, N)
  out[1] <- 1 + 1*x[1]
  for (i in 2:N) {
    out[i] <- out[i-1] + out[i-1]*x[i]
  }
  return(out)
}