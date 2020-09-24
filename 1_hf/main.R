# Packages
library(quantmod)
library(data.table)
library(e1071)
library(stats)
library(ggpubr)
library(ggthemes)


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

#creating empty columns
data[,mean := as.numeric()]
data[,variance := as.numeric()]
data[,skewness := as.numeric()]
data[,kurtosis := as.numeric()]
data[,window_size := as.numeric()]

#ordering table by date
data <- data[order(-date)]



########### Calculating moments ########### 

for (n in 1:dim(data)[1]){
  window <- data[1:n]
  data[n]$mean <- mean(window$logreturn, na.rm = T)
  data[n]$variance <- var(window$logreturn, na.rm = T)
  data[n]$skewness <- skewness(window$logreturn, na.rm = T)
  data[n]$kurtosis <- kurtosis(window$logreturn, na.rm = T)
  data[n]$window_size <- n
}



########### Visualising moments ########### 

ggplot(data = data, aes(x = window_size, y = mean))+
  geom_line(color = "steelblue3", size = 1.3)+
  labs(x = "Window size", y = "Mean of logreturns")+
  theme_economist()+
  theme(axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"))+
  scale_color_economist()


ggplot(data = data, aes(x = window_size, y = variance))+
  geom_line(color = "steelblue3", size = 1.3)+
  labs(x = "Window size", y = "Variance of logreturns")+
  theme_economist()+
  theme(axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"))+
  scale_color_economist()


ggplot(data = data, aes(x = window_size, y = skewness))+
  geom_line(color = "steelblue3", size = 1.3)+
  labs(x = "Window size", y = "Skewness of logreturns")+
  theme_economist()+
  theme(axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"))+
  scale_color_economist()


ggplot(data = data, aes(x = window_size, y = kurtosis))+
  geom_line(color = "steelblue3", size = 1.3)+
  labs(x = "Window size", y = "Kurtosis of logreturns")+
  theme_economist()+
  theme(axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"))+
  scale_color_economist()



########### Normality tests########### 

# Test 1
shapiro.test(data$logreturn)
# p value is too small, we reject the null-hypothesis that the distribution is normal

# Test 2
ggqqplot(data$logreturn, color = "steelblue3")+
  theme_economist()+
  theme(axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"))+
  scale_color_economist()
# The tails are much fatter than the normal distribution: at the two ends of the distribution, the 
# logreturns are much higher.

# Test 3
ggdensity(data$logreturn, fill = "steelblue3", add = "mean", rug = T)+
  stat_overlay_normal_density(color = "red", linetype = "dashed", size = 1.3)+
  theme_economist()+
  theme(axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"))+
  scale_color_economist()
# There are to many values around 0, while there are empty space under the normal distribution curve 
# around 0.05. There are even returns at -0.2.


