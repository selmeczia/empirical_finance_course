
### functions

process_data <- function(data, index = F){
  
  rest <- names(data[2:ncol(data)])
  names(data) <- c("Date", rest)
  n <- nrow(data)
  
  data$Return <- c(data[1:nrow(data)-1 , "Close"] /data[2:nrow(data), "Close"] - 1, NA)
  
  data$Date <- as.Date(data$Date, "%d-%b-%y")
  processed_data <- data.table(data)
  processed_data <- processed_data[Date >= "2015-06-22" & Date <= "2016-06-30" ,c("Date", "Return")]
  
  
  
  return(processed_data)
}



eventstudy <- function(data, index_data, y_col, x_col, event_start){
  
  merged <- merge(data, index_data, by = "Date")
  names(merged) <- c("Date", y_col, x_col)
  
  
  event_length <- nrow(merged[Date > event_start])
  window <- merged[2:(nrow(merged)-event_length)]
  event <- merged[(nrow(merged)+1-event_length):nrow(merged)]
  
  formula <- as.formula(paste0(y_col, "~", x_col) )
  
  result <- lm(formula, d = merged )
  alpha <- result$coefficients[1]
  beta <- result$coefficients[2]
  
  window$e_hat <- window[,..y_col]-alpha-beta*window[, ..x_col]
  event$e_star <- event[,..y_col]-alpha-beta*event[,..x_col]
  event$CAR <- cumsum(event$e_star)
  
  
  sigma_sq_e_hat <- sum(window$e_hat[1:nrow(window)]^2) / (length(window$e_hat[1:nrow(window)])-2)
  
  
  X_star <- matrix(1, nrow = event_length, ncol = 2)
  X_star[,2] <- event$e_star
  X <- matrix(1, nrow = nrow(window), ncol = 2)
  
  X[,2] <- window[1:nrow(window), ..x_col]$MSCI
  
  Vi <- diag(rep(sigma_sq_e_hat, event_length)) + X_star %*% solve(crossprod(X)) %*% t(X_star) * sigma_sq_e_hat
  
  var <- c()
  for (i in 1:event_length) {
    var[i] <- sum(Vi[1:i,1:i])
  }
  
  event$VAR <- var
  event$SCAR <- event$CAR/sqrt(event$VAR)
  event$p_value <- 1-pt(abs(event$SCAR), df = nrow(window))
  
  output <- list()
  output$e_hat <- window$e_hat
  output$event <- event
  output$e_star <- event$e_star
  output$alpha <- alpha
  output$beta <- beta
  output$sigma_sq_e_hat <- sigma_sq_e_hat
  
  return(output)
}



