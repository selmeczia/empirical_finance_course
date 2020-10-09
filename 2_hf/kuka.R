# kuka

#parameters
T <- length(data$loss[!is.na(data$loss)])
n <- 40
m = ceiling(T/n)
mu <- 0.1
sigma <- 0.1
xi <- 0.1



likelihood <- data.table(window_number = 1:m)
likelihood[,max_value := as.numeric()]
likelihood[,arg := as.numeric()]

for (i in 1:m){
  
  lower_bound <- (n*(i-1))+1 
  
  if((n*i) > T){
    upper_bound <- T
  } else {
    upper_bound <- n*i
  }
  
  likelihood[i]$max_value <- max(data$loss[(lower_bound+1):upper_bound])
}

likelihood[,arg := 1 + (xi * (max_value - mu) / sigma)]
likelihood[,loglh := ( -log(sigma) - ((1 + (1 / xi) ) * log(arg) )- arg^(-1 / xi) )]


sum_of_likelihood <- sum(likelihood$loglh)




logl <- function(parameters)
{sum(( -log(parameters[1]) - ((1 + (1 / parameters[2]) ) * log(likelihood$arg) )- likelihood$arg^(-1 / parameters[2]) ))}


pars <- c(0.000001, 0.0000005)
result <- optim(pars, logl, control = c("fnscale" = -1))

result

optimize(logl, maximum = T, interval = c(0,1))

