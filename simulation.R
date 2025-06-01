library(tidyverse)

# first we choose a distribution (Normal, Exponential, Poisson) and sample from it

# choose Poisson, Fisher information is 


n <- 100

param <- 500
fisher_info <- 1/param

estimate_variance <- function(param, distribution, n) {
  if (distribution == "poisson") {
    
    estimates <- numeric(100)
    
    for (i in 1:100) {
      sample <- rpois(n, param)
      
      estimates[i] <- mean(sample)
    }
    
    return (var(estimates))
  }
}



crlb <- function(n){
  return (1/(fisher_info * n))
}

variances <- numeric(n)
crlb_vals <- numeric(n)
x_vals <- 1:n

for (sample_size in 1:n) {
  crlb_vals[sample_size] <- crlb(sample_size)
  variances[sample_size] <- estimate_variance(param=param, distribution="poisson", n = sample_size)
}

df <- tibble(variances, crlb_vals, x_vals)

ggplot(df) +
  geom_smooth(aes(x = x_vals, y = variances, color = "red"), se = FALSE) +
  geom_smooth(aes(x = x_vals, y = crlb_vals), color = "blue", se = FALSE)



