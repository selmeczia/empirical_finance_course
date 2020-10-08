# Packages
library(quantmod)
library(data.table)
library(e1071)
library(stats)
library(ggpubr)
library(ggthemes)
library(tsoutliers)
library(stats)

##feladatok:
## GPD eloszlás (generalized pareto distributoin)
## küszöbérék megválasztása
## illeszkedés jóságának vizsgálata (qq vagy egyéb)
## a gdp eloszlással 1 napos 95 és 99%os VaR és CVaR (külnböző küszöbértékekkel)
## összehasonlítás historikus értékekel

########### Data load ########### 

getSymbols("SHOP", from = '2016-09-22',
           to = "2020-09-22",warnings = FALSE,
           auto.assign = TRUE)



########### Data manipulation ########### 

data <- setnames(data.table(SHOP$SHOP.Adjusted), "price")
data$date <- time(SHOP)

#calculating logreturns
z <- with(data, diff(log(data$price)))
data <- cbind(data, logreturn = c(NA,z))

#loss
data[,loss := -logreturn]

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

##### EVT #####
evt_table <- data.table(sorted_loss = as.numeric(data[order(-loss),]$loss))

u <- 0.055
evt_table[,Y := sorted_loss-u]

Nu <- nrow(evt_table[evt_table$Y > 0])
eps <- 0.303
beta <- 0.02

likelihood <- evt_table[1:Nu]
likelihood[, lnL := -log(beta) - (1 + 1/eps) * log(1 + eps/beta*Y)]


