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

# Q3 ----------------------------------------------------------------------
#First we plot age against net_worth
ggplot(data_new)+
  geom_point(aes(x=age,y=net_worth))
#As we can see 39 missing values exist for age which are removes.

#then we plot age against log(net_worth)
ggplot(data_new)+
  geom_point(aes(x=age,y=log(net_worth))) 
#when we use log(net_worth) the plot is more informative since the observations for
#lower values of net_worth (which contains the bigger portion of the data) is less dense
#and easier to comprehend.





