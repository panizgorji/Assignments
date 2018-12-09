library(tidyverse)

# Question 1 ------------------------------------------------------------------------------------------------------

#' Tidy_df
#'
#' @param data 
#' @param column_prefix 
#'
#' @return a tidy data frame with columns that start with the given column prefix.
#' @export
#'
#' @examples
tidy_df <- function(data, column_prefix = "var") {
  gather(data, grep("var" , names( data) ),
         key = "variable", value = "value")
}

# An example to check the function
x= tibble( var1=1:4,
           var2=c("a","b","c","f"),
           var3=c(1,3,"d","v"),
           nn=c(4,5,6,8)
)
x<-tidy_df(x,"var")
x

# Question 2 ------------------------------------------------------------------------------------------------------

#' Get the Jane Austen data
#'
#' It will attempt to install the right package for you. If it does not work,
#'   try to install it manually.
#'
#' @return A data frame with Jane Austen texts, one line per row
get_jane_austen_data <- function(){
  
  tryCatch({library(gutenbergr)}, error = function(e){install.packages("gutenbergr")})
  library(gutenbergr)
  
  austen_text <- gutenberg_works(author == "Austen, Jane") %>% 
    gutenberg_download(meta_fields = "title") %>% mutate(id = row_number(gutenberg_id))
  assign("austen_text", austen_text, envir=.GlobalEnv)
  invisible()
}
View(austen_text)
# -----------------------------------------------------------------------
# Extract_possible_names 
#' Title
#'
#' @param data the input which is in a dataframe format
#' @param text_column the column containing text
#' @param id_column the column containing text
#'
#' @return The output is a table with three columns, one column is the "text_id" 
#' which contains the id from original data, "id" which is the new unique id for each word
#' on capital and "name" which is contains the words with first letter in capital
#' @export
#'
#' @examples
#' first I counted the number of words in each row and calculated the maximum number
#' (in order to know how many columns is needed) then I put each word in a different 
#' column and used the code from previous question to put all words in one column and 
#' then filter the words with first letters in capital (also the rows that contain no 
#' word) and after generated new column for id and renamed the columns as needed.
extract_possible_names<-function(data,text_column, id_column){
  data<-mutate(data, n =str_count(data[[text_column]], '\\w+'))
  max_words<-max(data$n)
  x <- c(paste("word", 1:max_words, sep = ""))
  data_new<- separate(data,text, into=x,
                      remove = TRUE) 
  data_new<-gather(data_new,grep("word" , names(data_new)),
                   key = "variable", value = "value") 
  data_new<-filter(data_new, data_new$value > 0 & str_detect(data_new$value,"^[A-Z]."))
  data_new<-rename(data_new,text_id=id, name=value)
  data_new<-mutate(data_new,id=rownames(data_new))
  data_new<-select(data_new,text_id, name, id)
}
austen_CapWords <- extract_possible_names(austen_text, "text", "id")
View(austen_CapWords)


# Question 3 ------------------------------------------------------------------------------------------------------

# filter_names
austen_word_freqs <- readRDS("austen_word_freqs (1).Rds")
#' Title
#'
#' @param data the data from previous section which contains words in capital
#' @param Ref_data the data that contains frequency of the words
#' @param name is the column label of the words 
#'
#' @return the words with first letter in capital that were appeared capitalized 
#' more tha 75% of the time
#' @export
#'
#' @examples
filter_names<-function(data,Ref_data, name){
  data_new<-data
  data_new$lowercase_name <- tolower(data_new[[name]])
  summarised_data<-data_new %>%
    group_by(lowercase_name) %>% 
    rename(word = lowercase_name) %>% 
    summarise(n_present=n()) %>% 
    inner_join(Ref_data, by="word") %>% 
    mutate(percentage = (n_present/ count) * 100)
  
  data_new %>%
    rename(word = lowercase_name) %>%
    inner_join(summarised_data, by = "word") %>%
    filter(percentage >= 75) %>% 
    select(id, text_id, name, percentage)
  
}

filtered_names <- filter_names(austen_CapWords, austen_word_freqs, "name")
View(filtered_names)


# Question 4 ------------------------------------------------------------------------------------------------------

# count_names_per_book
#' Title
#'
#' @param ref_data is the original data with titles of books and text_id
#' @param data is the table from ref_data with id, text_id and names (derived
#' from previous section)
#'
#' @return returns a data frame with a column containing the book
#' tiles, a column with the number of unique names per book and a 
#' column with total number of name occurrences per book
#' @export
#'
#' @examples
count_names_per_book <- function(ref_data, data) {
  ref_data <- ref_data %>%
    select(title, id) %>% 
    rename(text_id = id)
  
  joined_data <- inner_join(data, ref_data, by = "text_id") %>% 
  
  names_per_book <- joined_data %>% 
    group_by(title) %>% 
    summarise(unique_names = length(unique(name)), name_occurrences = n())
}

filtered_names <- count_names_per_book(austen_text, filtered_names)
View(filtered_names)
