
library(tidyverse)
library(xml2)
library(RCurl)
library(dplyr)
base_url <- "https://www.cia.gov/library/publications/the-world-factbook/"

# Q1 ----------------------------------------------------------------------
#' Question 1: Get Population Ranking
#'
#' @return A dataframe including 4 columns countries, the country links
#'  the countries' population and the rankings of population.
#' @export
#'
#' @examples
get_population_ranking <- function(){
  xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
                         "country" = "//td[@class='region']/a",
                         "value" = "//tr/td[3]",
                         "rank" = "//tr/td[1]")
  url = str_c(base_url, "fields/335rank.html")
  
  #download url and execute all XPath queries which will each return a column for a data_frame
  url_data <- read_html(getURL(url,
                               .encoding = "UTF-8",
                               .opts = list(followlocation = FALSE)))
  
  countries <- xml_find_all(url_data, xpath_expressions['country'])
  country_links <- xml_find_all(url_data, xpath_expressions['country_link'])
  value <- xml_find_all(url_data, xpath_expressions['value'])
  rank <- xml_find_all(url_data, xpath_expressions['rank'])
  
  #make the necessary adjustments to the data frame as given by the assignment
  data_all <- data.frame(country = c(sapply(countries,xml_text)), 
                         country_link = c(gsub("^...","",sapply(country_links, xml_text))),
                         population=c(sapply(value,xml_text)),
                         rank.population=c(sapply(rank,xml_text)))
  return(data_all)
  View(data_all)
}

#Test the funtion:
example_1<-get_population_ranking()
View(example_1)

# Q-2 ---------------------------------------------------------------------
#' Question 2: Retrieve Land Area
#'
#' @param country_link A character vector of one or more country_link urls
#'
#' @return A character vector 
#' @export
#'
#' @examples
get_land_area <- function(country_link){
  xpath <- str_c("//div[@id='","field-area","']/div[",2,"]/span[2]")
  
  #download the file from country_link and execute the xpath query
  area_url = str_c(base_url, country_link)
  area_data_all <- vector(length = length(country_link))
  
  for (i in 1:length(country_link)) {
    data_html <- read_html(download_html(area_url[i]))
    area_data <- xml_find_all(data_html, xpath)
    area_data_all[i] <- xml_text(area_data)
  }
  return(area_data_all)
}

#Test the funtion:
country_link <- example_1$country_link
data_land<-get_land_area(country_link)
View(data_land)

# Q-3 ---------------------------------------------------------------------

#' Question 3: Get Population Density
#'
#' @return A data frame of 6 variables including population density of each country
#' @export
#'
#' @examples
get_population_density <- function(){
  data_bind<-cbind(example_1,data_land)
  data_bind$data_land<-parse_number(data_bind$data_land)
  data_bind[12, "data_land"] <- 1000000
  data_bind$data_land<-parse_number(data_bind$data_land)
  data_bind$population<-parse_number(data_bind$population)
  data_bind <- mutate(data_bind, population_density = population / data_land)
  return(data_bind)
}

# Test the function
full_data<-get_population_density()
View(full_data)

# Q-4 ---------------------------------------------------------------------
#' Question 4: Get All Provided Rankings
#'
#' @return A dataframe of the characteristics and the link 
#' @export
#'
#' @examples
get_rankings <- function(){
  url2 <- "https://www.cia.gov/library/publications/the-world-factbook/docs/rankorderguide.html"
  xpath <- c("characteristic" = "//div[@class='field_label']/strong/a",
             "characteristic_link" = "//div[@class='field_label']/strong/a/@href")
  #...
  url_data2 <- read_html(getURL(url2,
                               .encoding = "UTF-8",
                               .opts = list(followlocation = FALSE)))

  characteristic <- xml_find_all(url_data2, xpath['characteristic'])
  characteristic_link <- xml_find_all(url_data2, xpath['characteristic_link'])
  data_all <- data.frame(characteristic = c(gsub(":","", sapply(characteristic,xml_text))), 
                         characteristic_link = c(gsub("../","", sapply(characteristic_link, xml_text))))
  data_all$characteristic<-tolower(data_all$characteristic)
  return(data_all)
  View(data-all)

}
example2<-get_rankings()
View(example2)
str(example2)


# Q-5 ---------------------------------------------------------------------
#' Question 5 - Part 1: Get Ranking
#'
#' @param url The url of the ranking
#' @param characteristic What this ranking is about
#'
#' @return
#' @export
#'
#' @examples

base_url <- "https://www.cia.gov/library/publications/the-world-factbook/"
get_ranking <- function(url = "fields/335rank.html", characteristic = "population"){
  xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
                         "country" = "//td[@class='region']/a",
                         "value" = "//tr/td[3]",
                         "rank" = "//tr/td[1]")
  #...
  url = str_c(base_url, url)
  
  url_data <- read_html(getURL(url,
                               .encoding = "UTF-8",
                               .opts = list(followlocation = FALSE)))
  country_link<-xml_find_all(url_data, xpath_expressions['country_link'])
  country <- xml_find_all(url_data, xpath_expressions['country'])
  value <- xml_find_all(url_data, xpath_expressions['value'])
  rank <- xml_find_all(url_data, xpath_expressions['rank'])
  
  all_data2 <- data.frame(country_link = c(gsub("^../","",sapply(country_link,xml_text))), 
                         country = sapply(country, xml_text),
                         value=sapply(value,xml_text),
                         rank=sapply(rank,xml_text))
  all_data2<-all_data2 %>% 
   rename(!!characteristic:=value)
  return(all_data2)
  View(all_data2)
}

# Test the function
example3<-get_ranking(url = "fields/279rank.html", characteristic = "area")
View(example3)


#' Question 5 - Part 2: Get Country Characteristic
#'
#' @param country_link 
#' @param xpath_field_id 
#' @param item 
#'
#' @return
#' @export
#'
#' @examples
base_url <- "https://www.cia.gov/library/publications/the-world-factbook/"
get_country_characteristic <- function(country_link, xpath_field_id = "field-area", item = 2){
  xpath <- str_c("//div[@id='",xpath_field_id,"']/div[",item,"]/span[2]")
  #download the file from country_link and execute the xpath query
  characteristics_url = str_c(base_url, country_link)
  characteristics_data_all <- vector(length = length(country_link))
  
  for (i in 1:length(country_link)) {
    data_html <- read_html(download_html(characteristics_url[i]))
    characteristics_data <- xml_find_all(data_html, xpath)
    characteristics_data_all[i] <- xml_text(characteristics_data)
  }
  return(characteristics_data_all)
}

# Testing the funtion:
country_link <- example$country_link
data<-get_country_characteristic("geos/ch.html","field-area",2)
View(data)

#' Question 6: Combine Rankings
#'
#' @param rankings Rankings from get_rankings (or a selection thereof)
#'
#' @return
#' @export
#'
#' @examples
combine_rankings <- function(rankings){
  
}



