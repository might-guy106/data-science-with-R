



# QUESTION NO 3
# 1)
tennis <- function(p) {
  no.sets.played <- 0  
  
  for (no.sets.played in 1:5) {
    if (no.sets.played < 4) {
      result <- rbinom(1, 1, p)
      
      if (result == 1) {
        no.sets.played <- no.sets.played + 1
      } else {
        no.sets.played <- no.sets.played + 1
      }
    } else {
      break  
    }
  }
  
  return(x)
}

# QUESTION NO 3
# 2)

matches <- numeric(1000)
for (i in 1:1000) {
  matches[i] <- tennis(0.7)
}

ans <- mean(matches)

for (i in 1:1000) {
  
}

ans <- mean(matches)




# QUESTION 4
# 1)
MontyHall <- function() {
  doors <- c("car", "goat", "goat")  
  contestant_option <- sample(1:3, 1) 
  
  if (doors[contestant_option] == "car") {
    monty_opens <- sample(which(doors == "goat"))
  } else {
    monty_opens <- which(doors == "goat" & seq_along(doors) != contestant_option)
  }
  
  remaining_door <- which(seq_along(doors) != contestant_choice & seq_along(doors) != monty_open)
  
  # after switching his option
  if (doors[remaining_door] == "car") {
    return(1)  # wins
  } else {
    return(0)  # loss
  }
}

#QUESTION 5

library(rvest)
html <- read_html('https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/')

movies <- html %>% html_nodes(".countdown-item")

for (movie in movies) {
  ranking <- movie %>% html_nodes(".countdown-index") %>% html_text() %>% trimws()
  name <- movie %>% html_nodes(".article_movie_title") %>% html_text() %>% trimws()
  score <- movie %>% html_nodes(".tMeterScore") %>% html_text() %>% trimws()
  year <- movie %>% html_nodes(".start-year") %>% html_text() %>% trimws()
  
  cat("Ranking:", ranking, "\n")
  cat("Name of Movie:", name, "\n")
  cat("Tomato % score:", score, "\n")
  cat("Year of movie:",year,"\n\n")
}

