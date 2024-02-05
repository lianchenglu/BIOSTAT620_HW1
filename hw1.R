setwd("/home/chenggg/BIOSTAT620")
rm(list = ls())
gc()
library(readxl)
library(dplyr)
library(ggplot2)
library(GGally)
library(lubridate)
library(reshape2)
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
p1 <- ggplot(data = df, aes(x = Date, y = Total.ST.min)) +
  geom_line() + 
  geom_point() +  
  labs(title = "Daily total screen time",
       x = "Date",
       y = "Minutes") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Daily total social screen time
p2 <- ggplot(data = df, aes(x = Date, y = Social.ST.min)) +
  geom_line() + 
  geom_point() +  
  labs(title = "Daily total social screen time",
       x = "Date",
       y = "Minutes") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Daily number of pickups
p3 <- ggplot(data = df, aes(x = Date, y = Pickups)) +
  geom_line() + 
  geom_point() +  
  labs(title = "Daily number of pickups",
       x = "Date",
       y = "Number of pickups") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Daily proportion of social screen time
p4 <- ggplot(data = df, aes(x = Date, y = prop_ST)) +
  geom_line() + 
  geom_point() +  
  labs(title = "Daily proportion of social screen time",
       x = "Date",
       y = "Poportion of social screen time") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Daily duration per use
p5 <- ggplot(data = df, aes(x = Date, y = duration_per_use)) +
  geom_line() + 
  geom_point() +  
  labs(title = "Daily duration per use",
       x = "Date",
       y = "Duration per use") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) 

(combined_plot1 <- p1 + p2 + p3 + p4 + p5 +
  plot_layout(nrow = 3, ncol = 2))

df2 <- df[,c(1,3,5,6,9,10)]
ggpairs(df2[,-1]) 

# df2_long <- pivot_longer(df2, cols = -Date, names_to = "series", values_to = "occupation_time")
# ggplot(data = df2_long, aes(x = Date, y = occupation_time, color = series)) +
#   geom_line() +
#   labs(title = "Melted plot", x = "Time", y = "Count") +
#   theme_minimal()


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
par(mfrow=c(1,1))

# P3
library(circular)
time_to_degrees <- function(time) {
  # Convert time to hours and minutes
  hours <- as.numeric(substr(time, 1, 2))
  minutes <- as.numeric(substr(time, 4, 5))
  # Calculate the total hours since midnight
  total_hours <- hours + minutes / 60
  # Calculate the angle
  angle <- total_hours / 24 * 360
  return(angle)
}
angles <- sapply(df$Pickup.1st_EST, time_to_degrees)
pickup_angles <- circular(angles, units="degrees", template="clock24")

# Plot the circular data
plot(pickup_angles, pch=19, col='blue', main="First Pickup Times on 24-hour Clock")

plot(pickup_angles, stack=TRUE, bins=144, col=" blue " ) # 360/2.5=144

# P4
glm_model <- glm(Pickups ~ offset(log(Total.ST.min / 60)), family = poisson, data = df2)
summary(glm_model)

mark_weekdays <- function(date) {
  if (weekdays(date) %in% c("Saturday", "Sunday")) {
    return(0)
  } else {
    return(1)
  }
}
df2$IsWeekday <- sapply(df2$Date, mark_weekdays)
df2$IsAfterJan10 <- 1
glm_model2 <- glm(Pickups ~ IsWeekday + IsAfterJan10 + offset(log(Total.ST.min / 60)), family=poisson, data = df2)
summary(glm_model2)


# P5
estimates <- mle.vonmises(pickup_angles)
mu <- estimates$mu
kappa <- estimates$kappa
angle_830AM <- (time_to_degrees("08:30") * 2 * pi)/360- pi
angle_830AM_circular <- circular(angle_830AM, type = 'angles', units = 'radians')
cdf <- pvonmises(angle_830AM_circular, mu, kappa)

# The probability that the first pickup is at 8:30 AM or later
probability <- 1 - cdf
probability


# Calculate the frequencies of each Pickup value
pickup_freq <- table(df2$Pickups)
# Sort the unique Pickup values in descending order
sorted_pickups <- sort(unique(df2$Pickups), decreasing = F)
# Calculate the cumulative sum of frequencies for each unique Pickup value
cum_freq <- cumsum(rev(pickup_freq[as.character(sorted_pickups)]))
# Calculate the cumulative probabilities
cum_prob <- cum_freq / sum(pickup_freq)
# Create a new dataframe for plotting
df_plot <- data.frame(
  Pickups = sorted_pickups,
  CumulativeProbability = rev(cum_prob)
)
# Plot
ggplot(df_plot, aes(x = Pickups, y = CumulativeProbability)) +
  geom_point() + 
  labs(x = "Vector Magnitude: c", 
       y = "P(x >= c)",
       title = "Occupation-Time Curves for Number of Pickups") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) 


library(ggplot2)
library(dplyr)
library(cowplot)
# Function to calculate cumulative probabilities
calculate_cum_prob <- function(df, variable_name) {
  # Calculate frequencies
  variable_freq <- table(df[[variable_name]])
  # Sort unique values
  sorted_variable <- sort(unique(df[[variable_name]]), decreasing = F)
  # Calculate cumulative frequencies and probabilities
  cum_freq <- cumsum(variable_freq[as.character(sorted_variable)])
  cum_prob <- cum_freq / sum(variable_freq)
  # Return the data frame for plotting
  data.frame(
    VectorMagnitude = sorted_variable,
    CumulativeProbability = rev(cum_prob)
  )
}

# Apply the function to each variable and plot
df_total_st <- calculate_cum_prob(df2, "Total.ST.min")
df_social_st <- calculate_cum_prob(df2, "Social.ST.min")
df_pickups <- calculate_cum_prob(df2, "Pickups")
df_prop_st <- calculate_cum_prob(df2, "prop_ST")
df_duration_per_use <- calculate_cum_prob(df2, "duration_per_use")

# Create each plot
p1 <- ggplot(df_total_st, aes(x = VectorMagnitude, y = CumulativeProbability)) +
  geom_point() + 
  labs(title = "Total Screen Time", x = "Vector Magnitude: c", y = "P(x >= c)") +
  theme_minimal() 

p2 <- ggplot(df_social_st, aes(x = VectorMagnitude, y = CumulativeProbability)) +
  geom_point() + 
  labs(title = "Social Screen Time", x = "Vector Magnitude: c", y = "P(x >= c)") +
  theme_minimal() 

p3 <- ggplot(df_pickups, aes(x = VectorMagnitude, y = CumulativeProbability)) +
  geom_point() + 
  labs(title = "Number of Pickups", x = "Vector Magnitude: c", y = "P(x >= c)") +
  theme_minimal() 

p4 <- ggplot(df_prop_st, aes(x = VectorMagnitude, y = CumulativeProbability)) +
  geom_point() + 
  labs(title = "Proportion of Social Screen Time", x = "Vector Magnitude: c", y = "P(x >= c)") +
  theme_minimal() 

p5 <- ggplot(df_duration_per_use, aes(x = VectorMagnitude, y = CumulativeProbability)) +
  geom_point() + 
  labs(title = "Duration per Use", x = "Vector Magnitude: c", y = "P(x >= c)") +
  theme_minimal() 
# Combine the plots
(combined_plot <- plot_grid(
  p1, p2, p3, p4, p5,
  labels = c("A", "B", "C", "D", "E"),
  ncol = 2,
  align = 'v'
))


