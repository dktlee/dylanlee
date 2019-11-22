library(rgl) # for 'dynamic' 3d plot below

n <- 1000 # sample size
set.seed(271) # set seed (for reproducibility)
E <- matrix(rexp(n * 4), ncol = 4) # (n, 4) matrix of iid Exp(1)
th <- 2 # Clayton parameter
V <- rgamma(n, shape = 1/th) # sample of V
psi <- function(t) (1+t)^(-1/th) # Clayton generator
U <- psi(E/V) # Clayton sample in 4d
## 2d plot
plot(U[, 1:2], xlab = expression(U[1]), ylab = expression(U[2]))
## 3d plot
plot3d(U[, 1:3], xlab = expression(U[1]), ylab = expression(U[2]), zlab = expression(U[3])) # dynamic plot
## Pairs plot (d >= 4)
pairs(U, gap = 0, pch = ".",
      labels = as.expression( lapply(seq_len(ncol(U)), function(j) bquote(U[.(j)])) ))
## => The copula is exchangeable (which we see from the symmetry with respect
##    to the diagonal) but not radially symmetric (which we see from the missing
##    point-symmetry with respect to (1/2, ..., 1/2)).
