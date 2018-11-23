
#----------------------------------------------------------------------------------
# Stochastic model
#
# x_t = sigma_t * error
# log(sigma_t^2) = a[1] + a[2]*log(sigma_{t-1}^2) + .. + phi
#
#----------------------------------------------------------------------------------
#
# name:         SVMgen.R
# description:  generate a sample belongs stochastic volatility model
# inputs:  
#               a   <- SVM model parameters
#               phi <- vector of noise in eq for ln(sigma^2) from SVM
#               n   <- sample size, we use 100 iteration for chain burning
#                      this value must be grater than 100. Real size n - 100   
# output:       a sample of size n
#
# creadet by:   Carolina Gamboa
# version2: we modify the inputs. Now the user is asked to give the value of the 
#           variance of phi, instead of assigning it in 1 as in the previous version. 
# version3: we generate as a seed the log of volatility by normal
#----------------------------------------------------------------------------------

genSVM <- function(a, phi, sigma2_phi, n){
  # ARCH(2) coefficients
  if (n <= 100){
    cat("error sample size!! \n")
  } 
  else {
  error <- rnorm(n)
  lsigma2 <- NULL
  lsigma2[1] <- rnorm(1, a[1]/(1-a[2]), sqrt( sigma2_phi/( 1-(a[2]^2) ) ) )
  x <- NULL
  for(i in 1:n){
    lsigma2[i+1] <- a[1] + a[2]*lsigma2[i] + phi[i]
    x[i] <- error[i]*sqrt(exp(lsigma2[i+1]))
  }
  
  sigma2 <- exp(lsigma2)
  x <- ts(x[101:n])
  sigma2 <- ts(sigma2[102:(n+1)]) 
  z <- list(x, sigma2)
  names(z) <- c("x", "sigma2")
  return(z)
  }
}


# corre <- NULL
# lcorre1 <- NULL
# lcorre2 <- NULL
# lcorre3 <- NULL
# 
# a1 <- c(0.1, 0.85)
# sigma2_phi1 <- 0.2
# n = 1100
# for(i in 1:1000){
# # x1 <- svsim(1000, mu = -10, phi = 0.99, sigma = 0.2)
# # x2 <- svsim(1000, mu = -10, phi = 0.99, sigma = 0.2)
# #   lcorre <- c(lcorre, 1-GCC_d(log(x1$vol^2), log(x2$vol^2), 0))
# 
# 
#     x1 <- genSVM_mod(a1, phi = rnorm(n, 0, sqrt(0.2)), sigma2_phi= 0.2, n)
#   x2 <- genSVM_mod(a1, phi = rnorm(n, sqrt(0.2)), sigma2_phi = 0.2, n)
#   x3 <- genSVM_mod(a2, phi = rnorm(n, sqrt(0.2)), sigma2_phi = 0.2, n)
#   x4 <- genSVM_mod(a2, phi = rnorm(n, sqrt(0.2)), sigma2_phi = 0.2, n)
# 
#   k = 1  
#    lcorre1 <- c(lcorre, 1-GCC_d(log(x1$sigma2), log(x2$sigma2), k))
#    lcorre2 <- c(lcorre, 1-GCC_d(log(x1$sigma2), log(x3$sigma2), k))
#    lcorre3 <- c(lcorre, 1-GCC_d(log(x3$sigma2), log(x4$sigma2), k))
# 
#   }
# 
# hist(lcorre1)
# hist(lcorre2)
# hist(lcorre3)

# 
# 
# n <- 580
# a <- c(0.831, 0.685)
# mu <- a[1] / (1-a[2]) #2.638095
# 
# sigma2_phi = 1
# phi <- rnorm(n, 0, sqrt(sigma2_phi))
# x <- genSVM(a, phi, sigma2_phi , n)
# 
# #library(stochvol)
# mod1 <- svsample(x$x, draws = 10000, burnin = 1000, thinpara = 1, thinlatent = 1)
# plot(mod1$latent[, 1])
# suma = summary(mod1)
# suma$para
# lt <- suma$latent[,1]
# plot(ts(exp(lt)))
# plot(x$sigma2)
# 
# cor(lt, log(x$sigma2))
# mean(lt)
# var(lt)
