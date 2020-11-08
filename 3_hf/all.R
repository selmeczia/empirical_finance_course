

test_dt <- data.table(read_excel(paste0(getwd(), "/3_hf/data/test.xlsx")))
window <- test_dt[2:(nrow(test_dt)-7)]
event <- test_dt[(nrow(test_dt)-6):nrow(test_dt)]
result <- lm(window$CS_return ~ window$MSCI_return)

alpha <- result$coefficients[1]
beta <- result$coefficients[2]


window$e_hat <- window$CS_return-alpha-beta*window$MSCI_return
event$e_star <- event$CS_return-alpha-beta*event$MSCI_return
event$CAR <- cumsum(event$e_star)

sigma_sq_e_hat <- sum(window$e_hat[2:nrow(window)]^2) / (length(window$e_hat[2:nrow(window)])-2)


X_star <- matrix(1, nrow = 7, ncol = 2)
X_star[,2] <- event$e_star
X <- matrix(1, nrow = nrow(window)-1, ncol = 2)
X[,2] <- window$MSCI_return[2:nrow(window)]
Vi <- diag(rep(sigma_sq_e_hat, 7)) + X_star %*% solve(crossprod(X)) %*% t(X_star) * sigma_sq_e_hat

var <- c()
for (i in 1:7) {
  var[i] <- sum(Vi[1:i,1:i])
}


event$VAR <- var
event$SCAR <- event$CAR/sqrt(event$VAR)

event$p_value <- 1-pt(abs(event$SCAR), df = nrow(window))
