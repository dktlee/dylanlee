# The following data are claim amounts (in thousands of dollars) associated with 30 automobileaccidents reported to an 
# insurance company limited to motor comprehensive covers:
# 0.78    9.41    9.02    4.18    6.11    5.79    4.95    1.54    0.47    0.79
# 1.55    2.34    0.34    6.25    5.04    6.80    6.03    2.77    6.04    7.81
# 0.65    1.10    1.74    2.15    0.73    4.21    1.21    7.90    5.93    1.95
# The  Gamma  distribution  is  often  used  for  modelling  claim  data. Suppose  the  claim  sizes Y1,...,Yn are
# independently distributed Gamma random variables.

# The maximum likelihood estimates for the two Gamma parameters using Newton Raphson algorithm:

y = c(0.78, 9.41, 9.02, 4.18, 6.11, 5.79, 4.95, 1.54, 0.47, 0.79, 1.55, 2.34, 0.34, 6.25, 5.04, 6.80, 6.03, 2.77, 6.04, 7.81, 0.65, 1.10, 1.74, 2.15, 0.73, 4.21, 1.21, 7.90, 5.93, 1.95)
n = length(y)

# Score Vector
score = function(theta) {
  alpha = theta[1]
  beta  = theta[2]
  S1    = n*log(beta) - n*digamma(alpha) + sum(log(y))
  S2    = n*alpha/beta - sum(y)
  return (c(S1,S2))
}

# Information Matrix
info = function(theta) {
  alpha = theta[1]
  beta  = theta[2]
  I11   = n*trigamma(alpha)
  I12   = -n/beta # = I21
  I22   = n*alpha/beta^2
  return (rbind(c(I11,I12),c(I12,I22)))
}

# Newton Raphson Algorithm
# alpha0 = 1.920488
# beta0  = 0.5
newton_raph = function(theta0){
  flag = TRUE
  theta_old = theta0
  while (flag){
    theta_new = theta_old + solve(info(theta_old)) %*% score(theta_old)
    
    if ((abs(theta_new[1]-theta_old[1])<0.00001) && (abs(theta_new[2]-theta_old[2])<0.00001))
    {flag=FALSE}
    theta_old = theta_new
  }

  return(theta_new)
}

# Wald based and LR (likelihood ratio) based 95% confidence intervals for theexpected claim size in

LRCI <- function(beta, y, n)
{
  -2*(n*(log(beta)) - (beta * n *mean(y))  - n*(log(1/mean(y))) + n )- qchisq(0.95,1)
}
mle <- 1/mean(y)

UpperCI <- 1/ uniroot(LRCI, c(0,mle), y=y, n=n)$root
LowerCI <- 1/ uniroot(LRCI, c(mle,1), y=y, n=n)$root