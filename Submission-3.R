# QUESTION NO 1
library(ggplot2)
data(iris)
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  labs(x = "SEPAL LENGTH", y = "PETAL LENGTH", color = "SPECIES") +
  ggtitle("PETAL LENGTH Vs SEPAL LENGTH - for the species")

# CONCLUSION : 
#  3 species - setosa, versicolor, virginica
#  in general, sepal & petal lengths order - setosa < versicolor < virginica
#  slight overlap b/w versicolor & virginica. 



# QUESTION NO 2
data(txhousing, package = "ggplot2")
str(txhousing)
summary(txhousing)
missing_values <- sum(!complete.cases(txhousing))
cat("No of missing values:", missing_values, "\n")

# Scatterplot of sales by date
ggplot(txhousing, aes(x = date, y = sales)) +
  geom_point() +
  labs(x = "Date", y = "Sales") +
  ggtitle("Sales by Date")

# Scatterplot of median housing price by date
ggplot(txhousing, aes(x = date, y = median)) +
  geom_point() +
  labs(x = "Date", y = "Median Housing Price") +
  ggtitle("Median Housing Price by Date")

# Bar plot of sales by month
txhousing$month <- format(txhousing$date, "%b")
ggplot(txhousing, aes(x = month)) +
  geom_bar() +
  labs(x = "Month", y = "Sales") +
  ggtitle("Sales by Month")

# Boxplot of sales by year
txhousing$year <- format(txhousing$date, "%Y")
ggplot(txhousing, aes(x = year, y = sales)) +
  geom_boxplot() +
  labs(x = "Year", y = "Sales") +
  ggtitle("Sales by Year")




# QUESTION NO 3
titanic <- read.csv("C:/Users/kkart/Downloads/titanic.csv")
titanic$Survived <- ifelse(titanic$Survived == 0, "dead", "survived")

final_plot <- ggplot(data = titanic, aes(y = Survived, x = Fare, fill = Sex)) +
  geom_boxplot(position = "dodge") +
  scale_fill_manual(values = c("red", "blue")) +
  facet_grid(Survived ~ ., scales = "free_y", space = "free_y") +
  labs(x = "Fair", y = "", title = "Fare vs Survival") +
  theme_minimal()
print(final_plot)


