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
    var = Var.B
  )
}

wald.test <- function(estimates, alpha = 0.05) {
  v.i <- solve(estimates$var)
  w <- t(estimates$est) %*% v.i %*% estimates$est
  (1 - pchisq(w, df = 2)) < alpha
}
