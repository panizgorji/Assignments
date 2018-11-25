#start the assignment
# I made some changes to the responses after the commit for each question
# so the final version might be different from the individual commit for each questtion
#add libraries

#add libraries
library(tidyverse)
library(ggplot2)
data<-read.csv("~/Documents/course/programming/forbes.csv")
# It is surprising that the label of rank variable is X..Rank in my data

# Part2-Q1 ----------------------------------------------------------------

# In order to recode the age, net_worth and rank into numeric variables
#I use function parse_number()
data$age<-parse_number(data$age)
data$X...rank<-parse_number(data$X...rank)
data$net_worth<-parse_number(data$net_worth)

# Q2 ----------------------------------------------------------------------

#In order to remove the people with net_worth less than 1 bilion I remove the people
# with rank higher than 1571
data<-filter(data,data$X...rank <= 1571)

# Another possible solution is to remove net_worth higher than 82.1
data<-filter(data,data$net_worth <= 82.1)

# Q3 ----------------------------------------------------------------------

#First we plot age against net_worth
ggplot(data)+
  geom_point(aes(x=age,y=net_worth))
#As we can see 39 missing values exist for age which are removed.  

#then we plot age against log(net_worth)
ggplot(data)+
  geom_point(aes(x=age,y=log(net_worth))) 
#when we use log(net_worth) the plot is more informative since the observations for
#lower values of net_worth (which consists of the bigger portion of the data) is less dense
#and easier to comprehend.

# Q4 ----------------------------------------------------------------------

data_byCountry <- data %>% 
  group_by(country) %>% 
  summarise(count=n(),range_worth=(max(net_worth)-min(net_worth))) %>% 
  filter(count>=6) %>% 
  arrange(range_worth) 

  
# Q5 ----------------------------------------------------------------------

ggplot(data_byCountry, aes(x=country,y=range_worth))+
  geom_bar(stat="identity",color = "violetred", fill = "violetred2")+
  coord_flip()+
  ylab("Difference between the Highest and lowest Net Worth") +
  xlab("Country")+
    theme_bw ()

# Q6 ----------------------------------------------------------------------

ggplot(data_byCountry, aes(x=reorder(country, range_worth),y=range_worth))+
  geom_bar(stat="identity",color = "violetred4", fill = "violetred2")+
  coord_flip()+
  ylab("Difference between the highest and lowest net worth") +
  xlab("Country")+
  theme_bw ()

# Q7 ----------------------------------------------------------------------

by_rank<-
  data %>% 
  group_by(X...rank) %>% 
  summarise(count=n()) %>% 
  filter(count>1) %>% 
  View()
#As we can see in thsi table there are 19 ranks which more than one person


# Q8 ----------------------------------------------------------------------

#First we group by rank and count the number of people in each rank
by_rank<-data %>% 
  group_by(X...rank) %>% 
  summarise(count=n()) 

#Then we add a column with the average rank
mutate(by_rank,avg_rank=X...rank+(count-1)/2) %>% 
  View()


# Q9 ----------------------------------------------------------------------

install.packages("rworldmap")
library(rworldmap)

#First we add the log form of net worth
net_worth_country<-group_by(data, country) %>% 
  summarise(sum_worth=log(sum(net_worth))) 

#Then we join our data and the world map by the nam eof countries
mapped_data <- joinCountryData2Map(net_worth_country, joinCode = "NAME", 
                                   nameJoinColumn = "country",verbose = T)

#As we can see one country couldn't match because the non-matching name
#which should be replaced
net_worth_country$country<-recode(net_worth_country$country,
                                  "Polynesia"="French Polynesia")
# I join the data again
mapped_data <- joinCountryData2Map(net_worth_country, joinCode = "NAME", 
                                   nameJoinColumn = "country",verbose = T)
mapCountryData(mapped_data
               ,nameColumnToPlot="sum_worth", colourPalette = "terrain",
               mapTitle = "Net Worth Per Country")

