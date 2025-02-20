R Code 
#Question 6. Construct a web graph (or spider graph) of the categorical variables. Fine-tune the graph so that interesting results emerge. Discuss your findings.



install.packages("fmsb")   
install.packages("readxl") 
install.packages("dplyr")  


library(fmsb)
library(readxl)
library(dplyr)


df <- read_excel("C:/Users/Kadeem Green/Downloads/spreadsheet_full.xlsx")


categorical_vars <- c("workclass", "education", "marital-status", "occupation", 
                      "relationship", "race", "sex", "native-country", "income")

df[categorical_vars] <- lapply(df[categorical_vars], as.factor)


plot_radar_chart <- function(var_name) {
  
 
  category_freq <- prop.table(table(df[[var_name]]))  # Correct frequency calculation
  category_freq <- as.data.frame(category_freq)
  colnames(category_freq) <- c("Category", "Frequency")
  
  
  max_val <- max(category_freq$Frequency, na.rm = TRUE)
  min_val <- min(category_freq$Frequency, na.rm = TRUE)
  
 
  max_values <- rep(max_val, nrow(category_freq))  # Ensure proper max scaling
  min_values <- rep(0, nrow(category_freq))  # Min is always 0
  
  
  radar_data <- rbind(max_values, min_values, t(category_freq$Frequency))
  colnames(radar_data) <- category_freq$Category
  radar_data <- as.data.frame(radar_data)
  
  
  dev.new()  
  
 
  radarchart(radar_data,
             axistype = 1, 
             pcol = "blue",  # Line color
             pfcol = rgb(0.2, 0.5, 0.8, 0.5),  # Fill color
             plwd = 3,        # Line width
             cglcol = "grey", # Grid color
             cglty = 1,       # Grid line type
             axislabcol = "black",
             caxislabels = round(seq(0, max_val, length.out = 5), 2), # Properly formatted axis labels
             title = paste("Radar Chart of", var_name))
  
  legend("topright", legend = paste("Categories of", var_name), col = "blue", lwd = 3)
}

***************************************************************************************************
  
  #Question 9. Construct a histogram of each numerical variables, with an overlay of the target variable income normalize if necessary.


library(tidyverse)

age <- read.csv("C:/Users/Kadeem Green/Downloads/Adult.txt", stringsAsFactors = TRUE)


head(age)


str(age)

num_vars <- age %>% select_if(is.numeric) %>% names()


if(!is.factor(age$income)) {
  age$income <- as.factor(age$income)
}

normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}


age_normalized <- age %>%
  mutate(across(all_of(num_vars), normalize))


for (var in num_vars) {
  print(
    ggplot(age, aes_string(x = var, fill = "income")) +
      geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
      labs(title = paste("Histogram of", var, "by Income"),
           x = var,
           y = "Count") +
      theme_minimal()
  )
}

*****************************************************************************************************
  
#Question 10 For each pair of numerical variables, construct a scatter plot of the variables. Discuss your salient results.
 library(ggplot2)


df <- read.csv("C:/Users/Kadeem Green/Downloads/spreadsheet_full.xlsx", stringsAsFactors = FALSE)


df$age <- as.numeric(df$age)
df$demogweight <- as.numeric(df$demogweight)
df$education_num <- as.numeric(df$education.num)  # Ensure correct column name
df$capital_gain <- as.numeric(df$capital.gain)
df$capital_loss <- as.numeric(df$capital.loss)
df$hours_per_week <- as.numeric(df$hours.per.week)


df <- as.data.frame(df)


plot(df$age, df$hours_per_week, 
     main="Scatterplot of Age vs Hours per Week",
     xlab="Age", 
     ylab="Hours Per Week", 
     pch=19, col="blue")


plot(df$education_num, df$capital_gain, 
     main="Scatterplot of Education Number vs Capital Gain",
     xlab="Education Number", 
     ylab="Capital Gain", 
     pch=19, col="red")

plot(df$capital_gain, df$capital_loss, 
     main="Scatterplot of Capital Gain vs Capital Loss",
     xlab="Capital Gain", 
     ylab="Capital Loss", 
     pch=19, col="green")


plot(df$hours_per_week, df$demogweight, 
     main="Scatterplot of Hours per Week vs Demographic Weight",
     xlab="Hours Per Week", 
     ylab="Demographic Weight", 
     pch=19, col="purple")


plot(df$capital_gain, df$age, 
     main="Scatterplot of Capital Gain vs Age",
     xlab="Capital Gain", 
     ylab="Age", 
     pch=19, col="orange")



