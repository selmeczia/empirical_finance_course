# Packages
library(quantmod)
library(data.table)
library(e1071)
library(stats)
library(ggpubr)
library(ggthemes)
library(tsoutliers)
library(stats)
library(openxlsx)

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



########### EVT ###########

evt_table <- data.table(sorted_loss = as.numeric(data[order(-loss),]$loss))

u <- 0.055
evt_table[,Y := sorted_loss-u]

Nu <- nrow(evt_table[evt_table$Y > 0])
xi <- 0.30256155567236
beta <- 0.0198265125479346

likelihood <- evt_table[1:Nu]
likelihood[, lnL := -log(beta) - (1 + 1/xi) * log(1 + xi/beta*Y)]

logl <- function(parameters)
{sum( -log(parameters[1]) - (1 + (1 / parameters[2]) ) * log(1 + parameters[2]/parameters[1]*likelihood$Y))}


pars <- c(0.5, 0.5)
result <- optim(pars, logl, control = c("fnscale" = -1))

beta_hat <- result$par[1]
xi_hat <- result$par[2]

writeLines(paste0("\n xi : ", format(result$par[2], digit = 4),
                  "\n Beta : ", format(result$par[1],digit = 4), "\n"))

#write.xlsx(likelihood, "loss_sorted.xlsx")


########### VaR and CVaR ###########

n <- length(data$logreturn)-1
q <- 0.99


## EVT VaR

EVT_VaR <- (u + beta_hat/xi_hat * ((n/Nu * (1-q))^(-xi_hat) - 1 ))
writeLines(paste0("\n VaR at ", q*100, "% confidence level: ", format(EVT_VaR*100, digit = 3), "% \n"))


## Historical VaR

n_hist <- n*(1-q)
VaR_hist <- likelihood$sorted_loss[ceiling(n_hist)]
writeLines(paste0("\n Historical VaR at ", q*100, "% confidence level: ",
                  format(VaR_hist*100, digit = 3), "% \n"))


## EVT CVaR
EVT_CVaR <- EVT_VaR + (beta_hat + xi_hat * (EVT_VaR-u)) / (1 - xi_hat)
writeLines(paste0("\n EVT CVaR at ", q*100, "% confidence level: ",
                  format(EVT_CVaR*100, digit = 3), "% \n"))


## Historical CVaR
CVaR_hist <- mean(likelihood[1:ceiling(n_hist)]$sorted_loss)
writeLines(paste0("\n Historical CVaR at ", q*100, "% confidence level: ",
                  format(CVaR_hist*100, digit = 3), "% \n"))



########### Hill-method ########### 

hill_data <- na.omit(data.table(sorted_loss = data[order(-loss),]$loss))
hill_data[,excess := sorted_loss-u]
A <- tail(hill_data[hill_data$sorted_loss > u]$sorted_loss, n = 1)
hill_data[,lnL := log(sorted_loss/A)]

alpha <- length(hill_data[hill_data$lnL >= 0]$lnL) / sum(hill_data[hill_data$lnL >= 0]$lnL)
hill_xi <- 1/alpha
hill_beta <-   mean(hill_data[hill_data$excess >= 0]$excess) * (1 - hill_xi)


########### Peaks over Treshold ###########

temp <- data.table(sorted_loss = as.numeric(data[order(-loss),]$loss))

pot <- data.table(sorted_loss = as.numeric(temp[temp$sorted_loss > 0]$sorted_loss))
pot[,peaks := as.numeric()]

for (i in 2:nrow(pot)){
  pot[i,]$peaks <- sum(pot[1:i]$sorted_loss - pot[i,]$sorted_loss) / (i-1)
}


plot_data <- pot$sorted_loss[3:336]
plot_data_2 <- pot$peaks[3:336]

  
  