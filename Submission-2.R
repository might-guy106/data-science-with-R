# Question no 1 (a)
data(iris)
par(mfrow = c(2, 2))
boxplot(iris$Sepal.Length ~ iris$Species, main = "Sepal Length")
boxplot(iris$Petal.Length ~ iris$Species, main = "Petal Length")
plot(iris$Sepal.Length, iris$Petal.Length, col = iris$Species,
     xlab = "Sepal Length", ylab = "Petal Length", main = "Scatterplot of Sepal vs. Petal Length")

# Question no 2 (b)
library(imager)

flip <- function(image) {
  width <- dim(image)[2]
  height <- dim(image)[1]
  flipped_image <- imager::imager(height = height, width = width)
  
  for (y in 1:height) {
    for (x in 1:width) {
      pixel <- imager::get(image, x, y)
      imager::set(flipped_image, width - x + 1, y, pixel)
    }
  }
  return(flipped_image)
}

image_path <- imager::file.choose()
original_image <- imager::load.image(image_path)
imager::plot(original_image, main = "Original Image")
flipped_image <- flip(original_image)
imager::plot(flipped_image, main = "Flipped Image")




# Question no 3 (c)
library(MASS)
data(ships)
incident_counts <- tapply(ships$incidents, ships$type, sum)
barplot(incident_counts, 
        main = "Total Number of Incidents by Ship Type",
        xlab = "Ship Type",
        ylab = "Total Number of Incidents",
        col="steelblue")
# from the graph, clearly ship B is the Least trustworthy

# Question no 4 (d)
library(rvest)

df <- data.frame()

for (page in 1:13855) {
  link <- paste0("https://stats.stackexchange.com/questions?tab=votes&page=", page)
  page_link <- read_html(link)
  
  title <- page_link %>% html_nodes("#questions .s-link") %>% html_text()
  votes <- page_link %>% html_nodes(".s-post-summary--stats-item__emphasized .s-post-summary--stats-item-number") %>% html_text()
  views <- page_link %>% html_nodes(".is-supernova .s-post-summary--stats-item-number") %>% html_text()
  ans <- page_link %>% html_nodes(".has-answers .s-post-summary--stats-item-number") %>% html_text()
  
  df <- rbind(df, data.frame(title, votes, views, ans, stringsAsFactors = FALSE))
}

# Optional: Convert numeric columns to appropriate data types
df$votes <- as.integer(df$votes)
df$views <- as.integer(df$views)
df$answers <- as.integer(df$ans)

# Question no 5 (e)
expected_value <- 0
full_tablets <- 100
half_tablets <- 0
while (half_tablets == 0) {
  expected_value <- expected_value + 1
  probability <- full_tablets / (2*full_tablets - 1)
  if (runif(1) <= probability) {
    half_tablets <- half_tablets + 1
  }
  full_tablets <- full_tablets - 1
}
expected_value
