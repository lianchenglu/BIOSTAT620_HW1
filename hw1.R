setwd("/home/chenggg/BIOSTAT620")
rm(list = ls())
gc()
library(readxl)
library(dplyr)
library(ggplot2)
library(GGally)
library(lubridate)
library(reshape2)
library(tidyr)
df <- read_excel("ScreenTime.xlsx")
df <- df[c(1:11), ]
df$Pickup.1st_EST <- format(as.POSIXct(df$Pickup.1st_PST, format = "%H:%M", tz = "America/Los_Angeles"), "%H:%M", tz = "America/New_York")

convert_to_minutes <- function(time) {
  if (!grepl("h", time)) {
    return(as.numeric(sub("m", "", time)))
  }
  parts <- strsplit(time, "h|m")[[1]]
  as.numeric(parts[1]) * 60 + as.numeric(parts[2])
}
df$Total.ST.min <- sapply(df$Total.ST, convert_to_minutes)
df$Social.ST.min <- sapply(df$Social.ST, convert_to_minutes)
df$prop_ST <- df$Social.ST.min / df$Total.ST.min
df$duration_per_use <- df$Total.ST.min / df$Pickups

# Daily total screen time
ggplot(data = df, aes(x = Date, y = Total.ST.min)) +
  geom_line() + 
  geom_point() +  
  labs(title = "Daily total screen time",
       x = "Date",
       y = "Minutes") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Daily total social screen time
ggplot(data = df, aes(x = Date, y = Social.ST.min)) +
  geom_line() + 
  geom_point() +  
  labs(title = "Daily total social screen time",
       x = "Date",
       y = "Minutes") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Daily number of pickups
ggplot(data = df, aes(x = Date, y = Pickups)) +
  geom_line() + 
  geom_point() +  
  labs(title = "Daily number of pickups",
       x = "Date",
       y = "Number of pickups") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Daily proportion of social screen time
ggplot(data = df, aes(x = Date, y = prop_ST)) +
  geom_line() + 
  geom_point() +  
  labs(title = "Daily proportion of social screen time",
       x = "Date",
       y = "Poportion of social screen time") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Daily duration per use
ggplot(data = df, aes(x = Date, y = duration_per_use)) +
  geom_line() + 
  geom_point() +  
  labs(title = "Daily duration per use",
       x = "Date",
       y = "Duration per use") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) 

df2 <- df[,c(1,3,5,6,9,10)]
ggpairs(df2[,-1]) 

df2_long <- pivot_longer(df2, cols = -Date, names_to = "series", values_to = "occupation_time")
ggplot(data = df2_long, aes(x = Date, y = occupation_time, color = series)) +
  geom_line() +
  labs(title = "Melted plot", x = "Time", y = "Count") +
  theme_minimal()


# Calculate the P(x >= c) for Total.ST.min
df2 <- df2[order(df2$Total.ST.min, decreasing = TRUE),]  # Sorting by Total.ST.min
df2$Total.ST.prob <- seq_along(df2$Total.ST.min) / nrow(df2)

# Plot
ggplot(df2, aes(x = Total.ST.min, y = Total.ST.prob)) +
  geom_point() + 
  labs(x = "Vector Magnitude: c", 
       y = "P(x >= c)",
       title = "Occupation-Time Curves for total screen time") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Calculate the P(x >= c) for Social.ST.min
df2 <- df2[order(df2$Social.ST.min, decreasing = TRUE),]  # Sorting by Social.ST.min
df2$Social.ST.prob <- seq_along(df2$Social.ST.min) / nrow(df)

# Plot
ggplot(df2, aes(x = Social.ST.min, y = Social.ST.prob)) +
  geom_point() + 
  labs(x = "Vector Magnitude: c", 
       y = "P(x >= c)",
       title = "Occupation-Time Curves for social screen time") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Calculate the P(x >= c) for Pickups
df2 <- df2[order(df2$Pickups, decreasing = TRUE),]  # Sorting by Pickups
df2$Pickups.prob <- seq_along(df2$Pickups) / nrow(df)

# Plot
ggplot(df2, aes(x = Pickups, y = Pickups.prob)) +
  geom_point() + 
  labs(x = "Vector Magnitude: c", 
       y = "P(x >= c)",
       title = "Occupation-Time Curves for Number of Pickups") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Calculate the P(x >= c) for prop_ST
df2 <- df2[order(df2$prop_ST, decreasing = TRUE),]  # Sorting by prop_ST
df2$prop_ST.prob <- seq_along(df2$prop_ST) / nrow(df)

# Plot
ggplot(df2, aes(x = prop_ST, y = prop_ST.prob)) +
  geom_point() + 
  labs(x = "Vector Magnitude: c", 
       y = "P(x >= c)",
       title = "Occupation-Time Curves for Number of proportion of social screen time") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# Calculate the P(x >= c) for prop_ST
df2 <- df2[order(df2$duration_per_use, decreasing = TRUE),]  # Sorting by duration_per_use
df2$duration_per_use.prob <- seq_along(df2$duration_per_use) / nrow(df)

# Plot
ggplot(df2, aes(x = duration_per_use, y = duration_per_use.prob)) +
  geom_point() + 
  labs(x = "Vector Magnitude: c", 
       y = "P(x >= c)",
       title = "Occupation-Time Curves for Duration per use") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

par(mfrow=c(3,2))
for(i in 2:6) {
  acf_result <- acf(df2[[i]], main = paste("ACF for series", names(df2)[i]), plot = T)
  print(paste("Autocorrelations for series:", names(df2)[i]))
  print(acf_result$acf)
  acf_result
}

