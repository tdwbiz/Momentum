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

# compute rolling betas
getBetas <- function(df, mkt, k = 126) {
  # takes data frame df and returns vector of estimated k-rolling day betas 
  # with regard to mkt - a vector
  # 10 daily lags are used for mkt
  # uses close prices
  j <- grep("close", names(df), ignore.case=T)[1]
  n <- nrow(df)
  stopifnot(k > 0 & n == nrow(mkt))
  betas <- rep(NA, n-k-9)
  Date <- index(df)[(k+10):n]
  
  for (i in (k+10):n) {
    reg <- lm(df[(i-k+1):i,j]~mkt[(i-k+1):i]+mkt[(i-k):(i-1)]+mkt[(i-k-1):(i-2)]+
                mkt[(i-k-2):(i-3)]+mkt[(i-k-3):(i-4)]+mkt[(i-k-4):(i-5)]+
                mkt[(i-k-5):(i-6)]+mkt[(i-k-6):(i-7)]+mkt[(i-k-7):(i-8)]+
                mkt[(i-k-8):(i-9)]+mkt[(i-k-9):(i-10)])
    betas[i-k-9] <- sum(reg$coeff[2:11])
  }
  
  return(data.frame(Date, betas))
}