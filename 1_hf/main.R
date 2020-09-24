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
#data <- data[order(-date)]



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

p1 <- ggplot(data = data, aes(x = window_size, y = mean))+
  geom_line(color = "steelblue3", size = 1.3)+
  labs(x = "Window size", y = "Mean of logreturns")+
  theme_economist()+
  theme(axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"))+
  scale_color_economist()


p2 <- ggplot(data = data, aes(x = window_size, y = variance))+
  geom_line(color = "steelblue3", size = 1.3)+
  labs(x = "Window size", y = "Variance of logreturns")+
  theme_economist()+
  theme(axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"))+
  scale_color_economist()


p3 <- ggplot(data = data, aes(x = window_size, y = skewness))+
  geom_line(color = "steelblue3", size = 1.3)+
  labs(x = "Window size", y = "Skewness of logreturns")+
  theme_economist()+
  theme(axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"))+
  scale_color_economist()


p4 <- ggplot(data = data, aes(x = window_size, y = kurtosis))+
  geom_line(color = "steelblue3", size = 1.3)+
  labs(x = "Window size", y = "Kurtosis of logreturns")+
  theme_economist()+
  theme(axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"))+
  scale_color_economist()



########### Normality tests ########### 

# Test 1
shapiro.test(data$logreturn)
# p value is too small, we reject the null-hypothesis that the distribution is normal

# Test 2
p_qq <- ggqqplot(data$logreturn, color = "steelblue3")+
  theme_economist()+
  theme(axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"))+
  scale_color_economist()
# The tails are much fatter than the normal distribution: at the two ends of the distribution, the 
# logreturns are much higher.

# Test 3
p_dens <- ggdensity(data$logreturn, fill = "steelblue3", add = "mean", rug = T)+
  stat_overlay_normal_density(color = "red", linetype = "dashed", size = 1.3)+
  theme_economist()+
  theme(axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"))+
  scale_color_economist()
# There are to many values around 0, while there are empty space under the normal distribution curve 
# around 0.05. There are even returns at -0.2.

########### Saving plots ########### 
path <- paste0(getwd(), "/", "1_hf/plots")

ggsave("mean.png", p1, path = path, width = 10, height = 7)
ggsave("variance.png", p2, path = path, width = 10, height = 7)
ggsave("skewness.png", p3, path = path, width = 10, height = 7)
ggsave("kurtosis.png", p4, path = path, width = 10, height = 7)

ggsave("qq.png", p_qq, path = path, width = 10, height = 7)
ggsave("density.png", p_dens, path = path, width = 10, height = 7)


########### Alpha estimation ########### 

data[,losses := (-logreturn)]
n <- nrow(data[!is.na(data$losses)])

#table(data$losses > probability$x)[2]
probability <- data.table(x = seq(0.03, 0.15, by = 0.001))
probability[,prob_x := as.numeric()]

for (i in 1:nrow(probability)){
  probability$prob_x[i] <- table(probability$x[i] < data$losses)[2]/n
  
}

probability[, log_x := log(x)]
probability[, log_prob_x := log(prob_x)]

lm(d = probability, log_x ~ log_prob_x)
#lm(d = probability, log_prob_x ~ log_x)



########### Hill estimation ########### 

hill_plotting <- function(sample_size, log_loss){
  
  alpha_data <- data.table(values = as.numeric())
  
  for (i in 1:sample_size){
  
  hill_data <- data.table(x_i = sort(log_loss))
  
  hill_data <- na.omit(hill_data)
  
  limit <- as.numeric(hill_data[sample_size])
  hill_data[,ln_x_i_a := as.numeric()]
  hill_data$ln_x_i_a <- log(hill_data$x_i/limit)
  
  alpha_data[i,] <- sample_size / sum(hill_data[ln_x_i_a > 0,]$ln_x_i_a)
  
  }
  
}


hill_plotting(50, data$losses)

