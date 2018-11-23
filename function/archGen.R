
#----------------------------------------------------------------------------------
# ARCH(p) MODEL
#----------------------------------------------------------------------------------

# name:         SVMgen.R
# description:  generate a sample belongs ARCH model
# inputs:  
#               alpha <- SVM model parameters
#               error <- vector of noise in eq x = error*sigma
#               n     <- sample size, we use 100 iteration for chain burning
#                      this value must be grater than 100. Real size n - 100   
# output:       a sample of size n
#
# creadet by:   Carolina Gamboa
# V2: we modify the algorithm so that it also returns the volatility of the process


library(tseries)


genARCH <- function(alpha, error, n){
  
  p <- length(alpha)-1 # (p +1) is the number of parameters
  
  if (n <= 100){
    cat("error sample size!! \n")  # set 100 itera for burning the chain
  } 
  
  else {
    x     <- double(n)
    for(j in 1:p){
      x[j]  <- rnorm(1, sd = sqrt(alpha[1]/(1-sum(alpha[-1]))))
    }
    sigma2 <- NULL
    for(i in (p+1):n)  # Generate ARCH(1) process
    {
      s <- NULL
      for(j in 1:p){
        s <- sum(s, alpha[j+1]*x[i-j]^2)
      }
      sigma2[i] <- alpha[1] + s
      
      x[i] <- error[i]*sqrt(sigma2[i])
    }
    
    x <- ts(x[101:n])
    sigma2 <- ts(sigma2[101:n])
    y <- list(x, sigma2)
    names(y) <- c("x", "sigma2")
    return(y)
     # return(x)
    
  }
  
}

# 
# 
# #
# correlation <- NULL
# for(i in 1:1000){
# n = 460
# a1 <- c(0.01, 0.9) # model 1
# x1 <- genARCH(a1, rnorm(n), n)
# mod <- garchFit( ~garch(1, 0), data = x1$x)
# correlation = c(correlation, cor(mod@h.t, x1$sigma2))
# 
# }
# 
# plot(ts(mod@h.t[1:100]))
# plot(ts(x1$sigma2[1:100]))
# # a2 <- c(0.0022, 0.322, 0.074, 0.093)
# # a1 <- c(0.0126, 0.3526)
# 
#  corre <- NULL
#  for(i in 1:1000){
#    n = 460
#    a1 <- c(0.01, 0.01) # model 1
#    x1 <- genARCH(a1, rnorm(n), n)
#    x2 <- genARCH(a1, rnorm(n), n)
#    corre <- c(corre, 1-GCC_d(x1$sigma2, x2$sigma2, 10))
#  }
# 


