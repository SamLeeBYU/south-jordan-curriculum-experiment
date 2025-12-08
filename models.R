#Fixed effects model
fit.lm <- function(dat) {
  X <- model.matrix(
    ~ 0 +
      as.factor(id) +
      as.factor(week) +
      as.factor(time) * as.factor(instruction),
    data = dat
  )
  m <- lm(
    cbind(spelling, phonics) ~ 0 + X,
    data = dat
  )
  Uhat <- residuals(m)
  Sigma.u.hat <- crossprod(Uhat) / df.residual(m)
  Var.B <- Sigma.u.hat * solve(crossprod(X))[ncol(X), ncol(X)]
  list(
    est = coef(m)[ncol(X), ],
    var = Var.B,
    n = nrow(X),
    p = ncol(X)
  )
}

wald.test <- function(estimates, alpha = 0.05) {
  v.i <- solve(estimates$var)
  w <- t(estimates$est) %*% v.i %*% estimates$est
  (1 - pchisq(w, df = 2)) < alpha
}

f.test <- function(estimates, alpha = 0.05) {
  v.i <- solve(estimates$var)
  w <- t(estimates$est) %*% v.i %*% estimates$est

  n = estimates$n
  p = estimates$p
  nu <- (n - p - 1)
  f <- nu / (2 * (n - p)) * w

  (1 - pf(f, df1 = 2, df2 = nu)) < alpha
}
