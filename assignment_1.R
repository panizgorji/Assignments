#start the assignment
#add libraries
library(tidyverse)
library(ggplot2)
data<-read.csv("~/Documents/course/programming/forbes.csv")

# Part2-Q1 ----------------------------------------------------------------
data$age<-parse_number(data$age)
data$X...rank<-parse_number(data$X...rank)
data$net_worth<-parse_number(data$net_worth)


# Q2 ----------------------------------------------------------------------
#In order to remove the people witj net_worth less than 1 bilion we remove the people
# with rank higher than 1571
data_new<-filter(data,data$X...rank <= 1571)


