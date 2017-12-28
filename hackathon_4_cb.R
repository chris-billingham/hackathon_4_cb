# hackathon 4 attempt

library(tidyverse)
library(tidytext)
library(sentimentr)
library(magrittr)
library(caret)
library(lubridate)
library(pbapply)

# import the data
train <- read_csv("data/hackathon_4_train.csv")
test <- read_csv("data/hackathon_4_test.csv")

# import the gendered names list
names <- read_csv("data/names.csv")

# split to a boys and girls list
boy <- names %>% filter(gender == "boy") %>% filter(nchar(name)>=3)
girl <- names %>% filter(gender == "girl") %>% filter(nchar(name)>=3)

# let's sort out the dates
train$created <- mdy_hm(train$created)
test$created <- mdy_hm(test$created)

# let's scrub some of the text types
train$description <- train$description %>% 
  iconv("UTF-8", "UTF-8", sub='')
test$description <- test$description %>% 
  iconv("UTF-8", "UTF-8", sub='')
train$text <- train$text %>% 
  iconv("UTF-8", "UTF-8", sub='')
test$text <- test$text %>% 
  iconv("UTF-8", "UTF-8", sub='')
train$tweet_location <- train$tweet_location %>%
  iconv("UTF-8", "UTF-8", sub='')
test$tweet_location <- test$tweet_location %>%
  iconv("UTF-8", "UTF-8", sub='')

# let's to lower everything worth tolowering
train$description %<>% tolower()
train$name %<>% tolower()
train$text %<>% tolower()
test$description %<>% tolower()
test$name %<>% tolower()
test$text %<>% tolower()

# these are some terrible functions to check a list against a list for grepl
boy_count <- function(name) {
  boy_sum <- boy$name %>%
    map_int(~grepl(.x, name)) %>%
    sum()
  boy_sum <- boy_sum/nrow(boy)
  return(boy_sum)
}

girl_count <- function(name) {
  girl_sum <- girl$name %>%
    map_int(~grepl(.x, name)) %>%
    sum()
  girl_sum <- girl_sum/nrow(girl)
  return(girl_sum)
}

# run these functions to get girl and boy counts 
# this is crap and takes ages
train$boy_name <- pblapply(train$name, boy_count2)
train$boy_name <- unlist(train$boy_name)
train$girl_name <- pblapply(train$name, girl_count)
train$girl_name <- unlist(train$girl_name)
test$boy_name <- pblapply(test$name, boy_count)
test$boy_name <- unlist(test$boy_name)
test$girl_name <- pblapply(test$name, girl_count)
test$girl_name <- unlist(test$girl_name)
