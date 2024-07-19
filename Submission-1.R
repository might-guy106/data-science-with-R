library(tidyverse)
library(rvest)
library(dplyr)
library(imager)
library(stringr)

# QUESTION NO 1
url <- "https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/"
page <- read_html(url)

M_cap <- page %>% html_elements(".company-ellipses") %>% html_text()
CMP <- page %>% html_elements(".span") 
CMP


# QUESTION NO 3
# 1)
tennis <- function(p) {
  x <- 0
  while(T) {
    result <- rbinom(1, 1, p) # a random result for a set
    # if player A wins
    if (result == 1) {
      x <- x + 1
      if (x == 5) {break}
    }
    # if player B wins
    else {
      if (x < 4) {
        x <- x + 1
      } 
      else {break}
    }
  }
  
  return(x)
}
# 2)
matches <- vector(length = 1000)
for(i in 1:1000)
{
  matches[i] <- tennis(0.70)
}
ans <- mean(matches)
ans




# QUESTION NO 4
# 1)
MontyHall <- function() {
  doors <- c("car", "goat", "goat")
  
  my_door <- sample(1:3, 1)
  monty_door <- sample(which(doors[-my_door] == "goat"), 1)
  # switching from my door to left over door
  left_door <- setdiff(1:3, c(my_door, monty_door))
  
  if(doors[left_door] == "car"){
    return(1)
  } else{
    return(0)
  }
}
# 2)
wins <- 0
for (i in 1:1000) {
  result <- MontyHall()
  if(result == 1){
    wins <- wins+1
  }
}
probability <- wins/1000
probability

# QUESTION NO 5
library(rvest)
html <- read_html('https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/')

movies <- html %>% html_elements(".countdown-item")

for (movie in movies) {
  ranking <- movie %>% html_elements(".countdown-index") %>% html_text() %>% trimws()
  name <- movie %>% html_elements(".article_movie_title") %>% html_text() %>% trimws()
  score <- movie %>% html_elements(".tMeterScore") %>% html_text() %>% trimws()
  year <- movie %>% html_elements(".start-year") %>% html_text() %>% trimws()
  
  cat("Ranking:", ranking, "\n")
  cat("Name of Movie:", name, "\n")
  cat("Tomato % score:", score, "\n")
  cat("Year of movie:",year,"\n\n")
}






