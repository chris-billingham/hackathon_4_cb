# hackathon 4 attempt

library(tidyverse)
library(tidytext)
library(sentimentr)
library(magrittr)
library(caret)
library(lubridate)

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

# these are some terrible functions to check a list against a list for grepl
boy_count <- function(name) {
  boy_sum <- 0
  for (i in 1:nrow(boy)) {
    value <- grepl(boy$name[i], name)
    boy_sum <- boy_sum + value
  }
  boy_sum <- boy_sum/nrow(boy)
  return(boy_sum)
}

girl_count <- function(name) {
  girl_sum <- 0
  for (i in 1:nrow(girl)) {
    value <- grepl(girl$name[i], name)
    girl_sum <- girl_sum + value
  }
  girl_sum <- girl_sum/nrow(girl)
  return(girl_sum)
}

