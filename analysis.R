#Frequentist analysis
source("data.R")
source("models.R")

X <- model.matrix(
  ~ 0 +
    as.factor(id) +
    as.factor(classroom) +
    Age +
    as.factor(time) * as.factor(instruction),
  data = ellas.class
)
m <- lm(
  cbind(spelling, phonics) ~ 0 + X,
  data = ellas.class
)
summary(m)

#Spelling | Coef | SE
#Age | 22.917 | 9.186
#Class Red | -19.025 | 6.108
#\beta_1 | 13.091 | 7.834
#\beta_2 | -1.586 | 7.835
#\beta_3 | 9.636 | 11.078

#Phonics | Coef | SE
#Age | 16.542 | 4.715
#Class Red | -7.738 | 5.403
#\beta_1 | 1.000 | 4.021
#\beta_2 | -3.410 | 4.022
#\beta_3 | 6.818 | 5.687

estimates <- fit.lm(ellas.class)
#F-test
v.i <- solve(estimates$var)
w <- t(estimates$est) %*% v.i %*% estimates$est

#Chisq test
w
# 1.44
1 - pchisq(w, 2)
#0.48

n = estimates$n
p = estimates$p
nu <- (n - p - 1)
f <- nu / (2 * (n - p)) * w
f
#0.7169982
(1 - pf(f, df1 = 2, df2 = nu))
#0.4905246

#Bayesian analysis
allsamps <- readRDS("mcmc-samples.rds")
relevant.coefs <- allsamps[, c(1:14)]

#Posterior Estimates
cbind(
  colMeans(relevant.coefs),
  apply(relevant.coefs, 2, median),
  apply(relevant.coefs, 2, \(x) HDInterval::hdi(x)) |> t()
)

#Spelling | Mean | Median | 95% HPD
#Age | 17.37 | 17.38 | 2.34, 32.398
#Class Red | -17.40 | -17.42 | -28.79, -5.898
#\beta_1 | 13.124 | 13.139 | -1.8146, 28.0868
#\beta_2 | -1.5608 | -1.5621 | -16.706, 13.2158
#\beta_3 | 9.599 | 9.625 | -11.36, 30.83

#Phonics | Mean | Median | 95% HPD
#Age | 13.687 | 13.69 | 4.373, 22.65
#Class Red | -8.00 | -7.99 | -14.11697, -1.88
#\beta_1 | 0.996 | 0.982 | -6.911, 8.735574
#\beta_2 | -3.39778 | -3.3988075 | -11.3178, 4.25631
#\beta_3 | 6.810 | 6.8184 | -4.1028, 17.918

#Bayesian hypothesis testing
beta3 <- relevant.coefs[, c(7, 14)]
#Pr(beta_3,spelling > 0, beta_3,phonics > 0)
mean(beta3[, 1] > 0 & beta3[, 2] > 0)
# = 0.72305
#1-Pr(beta_3,spelling < 0, beta_3,phonics < 0)
1 - mean(beta3[, 1] < 0 & beta3[, 2] < 0)
# = 0.9785
