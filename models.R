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
    cbind(spelling, phonic) ~ 0 + X,
    data = dat
  )
  Uhat <- residuals(m)
  Sigma.u.hat <- crossprod(Uhat) / (nrow(Uhat) - sum(!is.na(coef(m)[, 1])))
  Var.B <- Sigma.u.hat * solve(crossprod(X))[ncol(X), ncol(X)]
  list(
    est = coef(m)[ncol(X), ],
    var = Var.B
  )
}

#Random effects model
fit.re <- function(dat) {
  X <- model.matrix(
    ~ 0 +
      as.factor(week) +
      as.factor(classroom) +
      as.factor(time) * as.factor(instruction),
    data = dat
  )

  beta3_col <- ncol(X)

  m.spell <- nlme::lme(
    fixed = spelling ~ 0 +
      as.factor(week) +
      as.factor(time) * as.factor(instruction),
    random = ~ 1 | id,
    data = dat,
    method = "REML"
  )

  m.phon <- nlme::lme(
    fixed = phonic ~ 0 +
      as.factor(week) +
      as.factor(time) * as.factor(instruction),
    random = ~ 1 | id,
    data = dat,
    method = "REML"
  )

  est <- c(
    spelling = last(nlme::fixed.effects(m.spell)),
    phonics = last(nlme::fixed.effects(m.phon))
  )

  V.spell <- vcov(m.spell)
  V.phon <- vcov(m.phon)

  ## Covariance between the two outcome equations:
  ## Approximate using residual correlation times SEs:
  Sigma.u <- cor(residuals(m.spell), residuals(m.phon), use = "complete.obs") *
    sqrt(m.spell$sigma^2) *
    sqrt(m.phon$sigma^2)

  var.mat <- matrix(0, 2, 2)
  var.mat[1, 1] <- V.spell[5, 5]
  var.mat[2, 2] <- V.phon[5, 5]
  var.mat[1, 2] <- var.mat[2, 1] <- Sigma.u

  list(
    est = est,
    var = var.mat
  )
}

wald.test <- function(estimates, alpha = 0.05) {
  v.i <- solve(estimates$var)
  w <- t(estimates$est) %*% v.i %*% estimates$est
  (1 - pchisq(w, df = 2)) < alpha
}
