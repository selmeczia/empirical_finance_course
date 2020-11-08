#packages
library(readxl)
library(estudy2)
library(data.table)
library(ggplot2)
library(ggthemes)

# load functions
funs_path <- paste0(getwd(), "/3_hf/functions.R")
source(funs_path)

#import files
barc <- read.csv(paste0(getwd(), "/3_hf/data/barc.csv"))
bat <- read.csv(paste0(getwd(), "/3_hf/data/bat.csv"))
bp <- read.csv(paste0(getwd(), "/3_hf/data/bp.csv"))
ezj <- read.csv(paste0(getwd(), "/3_hf/data/ezj.csv"))
hsbc <- read.csv(paste0(getwd(), "/3_hf/data/hsbc.csv"))
rbs <- read.csv(paste0(getwd(), "/3_hf/data/rbs.csv"))
rds <- read.csv(paste0(getwd(), "/3_hf/data/rds.csv"))
index <- read_excel(paste0(getwd(), "/3_hf/data/historyIndex.xls"))
names(index) <- c("Date", "standard_index")

# create index data table
index_dt <- data.table(index)
m <- nrow(index_dt)
index_dt$Return <- c(NA, index_dt[2:m, standard_index] / index_dt[1:m-1, standard_index] - 1)
proc_index <-  index_dt[Date >= "2015-06-22" & Date <= "2016-07-01" ,c("Date", "Return")]
proc_index$Date <- as.Date(proc_index$Date)

# process all table
Sys.setlocale("LC_TIME", "C")
proc_barc <- process_data(barc)
proc_bat <- process_data(bat)
proc_bp <- process_data(bp)
proc_ezj <- process_data(ezj)
proc_hsbc <- process_data(hsbc)
proc_rbs <- process_data(rbs)
proc_rds <- process_data(rds)

# event study

e_barc <- eventstudy(proc_barc, proc_index, "Barclays", "MSCI", "2016-06-22")
e_bat <- eventstudy(proc_bat, proc_index, "Bat", "MSCI", "2016-06-22")
e_bp <- eventstudy(proc_bp, proc_index, "Bp", "MSCI", "2016-06-22")
e_ezj <- eventstudy(proc_ezj, proc_index, "Easy_jet", "MSCI", "2016-06-22")
e_hsbc <- eventstudy(proc_hsbc, proc_index, "Hsbc", "MSCI", "2016-06-22")
e_rbs <- eventstudy(proc_rbs, proc_index, "Rbs", "MSCI", "2016-06-22")
e_rds <- eventstudy(proc_rds, proc_index, "Rds", "MSCI", "2016-06-22")


# abnormal returns

data <- data.table( days = 1:6, e_ezj$event)

ggplot(data = data, aes(x = days, y = e_star))+
  geom_bar(stat = "identity", fill = "steelblue3")+
  labs(x = "Days after event", y = "Abnormal return")+
  scale_y_continuous(labels = function(x) paste0(x*100, "%"))+
  theme_economist()

ggplot(data = data, aes(x = days, y = CAR))+
  geom_line(color = "steelblue3", size = 3)+
  scale_y_continuous(labels = function(x) paste0(x*100, "%"))+
  theme_economist()

# mean abnormal returns

all_ret <- merge(proc_barc, proc_bat, by ="Date")
all_ret <- merge(all_ret, proc_bp, by = "Date")
names(all_ret) <- c("Date", "Barclays", "Bat", "Bp")
all_ret <- merge(all_ret, proc_ezj, by = "Date")
all_ret <- merge(all_ret, proc_hsbc, by = "Date")
names(all_ret) <- c("Date", "Barclays", "Bat", "Bp", "Easy_jet", "Hsbc")
all_ret <- merge(all_ret, proc_rbs, by = "Date")
all_ret <- merge(all_ret, proc_rds, by = "Date")
names(all_ret) <- c("Date", "Barclays", "Bat", "Bp", "Easy_jet", "Hsbc", "Rbs", "Rds")
all_ret[,mean := rowMeans(all_ret[,2:7])]

mean_ret <- all_ret[, c("Date", "mean")]
names(mean_ret) <- c("Date", "return")
eventstudy(mean_ret, proc_index, "All", "MSCI", "2016-06-22")






