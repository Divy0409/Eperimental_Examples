# For Question 1
# Install and load the required packages if not already installed
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
if (!require(readxl)) {
  install.packages("readxl")
}
if (!require("plotly")) install.packages("plotly")

# Load the library
library(plotly)
library(ggplot2)
library(readxl)

#Length vs Time Scatter Plot with increasing Width:
# Read the text file into a dataframe called helicopter_data
# Replace "ROS_EX.txt" with the actual path of your data file
file_path <- "ROS_EX.txt"
helicopter_data <- read.table(file_path, header = TRUE, sep = "\t")

# Load the dplyr package for data manipulation
library(dplyr)

# Filter the data to select the highest Time value for each Helicopter_ID
filtered_data <- helicopter_data %>%
  group_by(Helicopter_ID) %>%  # Group the data by each Helicopter_ID
  filter(Time == max(Time)) %>%  # Within each group, keep only the row with the highest Time value
  ungroup()  # Remove grouping information from the dataframe

# Load the ggplot2 package for plotting
library(ggplot2)

# Create a scatter plot using ggplot2
ggplot(filtered_data, aes(x = Length, y = Time, color = Width)) +  # Define the aesthetics: Length on the x-axis, Time on the y-axis, and Width for the color gradient
  geom_point(size = 5) +  # Add points to the plot, with a specified size
  scale_color_gradient(low = "green", high = "red") +  # Define a color gradient from green to red based on Width values
  labs(title = "Scatter Plot: Length vs Time by Increasing Width",  # Set the title of the plot
       x = "Length",  # Label for the x-axis
       y = "Time") +  # Label for the y-axis
  theme_minimal()  # Apply a minimal theme for a clean look



# Create the boxplot of Time by Width
# 'aes(x = factor(Width), y = Time)' maps the Width (as a factor) to the x-axis and Time to the y-axis
ggplot(helicopter_data, aes(x = factor(Width), y = Time)) +
  # Add a boxplot with a blue fill color and black outline
  # Outliers are colored red, with a specific shape (16) and size (2)
  geom_boxplot(fill = "skyblue", color = "black", outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  # Add titles and labels for the axes
  labs(title = "Boxplot of Flight Time by Width",
       x = "Width",
       y = "Time (seconds)") +
  # Apply a minimalistic theme to the plot
  theme_minimal() +
  # Adjust the x-axis text to be angled at 45 degrees and horizontally justified to 1 for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Create the boxplot of Time by Length
# 'aes(x = factor(Length), y = Time)' maps the Length (as a factor) to the x-axis and Time to the y-axis
ggplot(helicopter_data, aes(x = factor(Length), y = Time)) +
  # Add a boxplot with a blue fill color and black outline
  # Outliers are colored red, with a specific shape (16) and size (2)
  geom_boxplot(fill = "skyblue", color = "black", outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  # Add titles and labels for the axes
  labs(title = "Boxplot of Flight Time by Length",
       x = "Length",
       y = "Time (seconds)") +
  # Apply a minimalistic theme to the plot
  theme_minimal() +
  # Adjust the x-axis text to be angled at 45 degrees and horizontally justified to 1 for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Bubble plot with Time affecting the size of bubbles
ggplot(helicopter_data, aes(x = Width, y = Length, size = Time)) +
  geom_point(alpha = 0.7) +  # Set alpha for transparency
  scale_size_continuous(range = c(2, 10)) +  # Adjust size range
  labs(
    title = "Bubble Plot of Width vs Length with Time",
    x = "Width",
    y = "Length",
    size = "Time"
  ) +
  theme_minimal()


if (!require("dplyr")) install.packages("dplyr")

# Load the libraries
library(dplyr)

# Create bins for Width and Length
data_binned <- helicopter_data %>%
  mutate(
    Width_bin = cut(Width, breaks = 10),
    Length_bin = cut(Length, breaks = 10)
  ) %>%
  group_by(Width_bin, Length_bin) %>%
  summarize(Mean_Time = mean(Time), .groups = 'drop')

# Create a heatmap
ggplot(data_binned, aes(x = Width_bin, y = Length_bin, fill = Mean_Time)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title = "Heatmap of Average Time by Width and Length Bins",
    x = "Width",
    y = "Length",
    fill = "Mean Time"
  ) +
  theme_minimal()



# For Question 1 (c)

# 1. Width vs Time:
gplot(helicopter_data, aes(x = Width, y = Time)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Width vs Time", x = "Width", y = "Time") +
  theme_minimal() 

#2. Length vs Time:
ggplot(helicopter_data, aes(x = Length, y = Time)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Length vs Time", x = "Length", y = "Time") +
  theme_minimal()

# Fit the linear model
model <- lm(Time ~ Width + Length, data = helicopter_data)

# Display the summary of the model
summary(model)

# Fit the linear model
model <- lm(Time ~ Width, data = helicopter_data)

# Display the summary of the model
summary(model)

# Fit the linear model
model <- lm(Time ~ Length, data = helicopter_data)

# Display the summary of the model
summary(model)



# For Question 2 (a):

# Set parameters
n <- 100  # Number of data points
x_min <- 0
x_max <- 4
residual_sd <- 3.9

# Generate x values
x <- runif(n, min=x_min, max=x_max)

# Compute true y values based on the line equation
true_y <- 30 + 10 * x

# Add normally distributed residuals to generate y values
y <- true_y + rnorm(n, mean=0, sd=residual_sd)

# Create the plot
plot(x, y, pch=19, xlim=c(x_min, x_max), ylim=c(min(y) - 5, max(y) + 5),
     xlab="x", ylab="y", main="Hypothetical Data with y = 30 + 10x")

# Add the true line
abline(a = 30, b = 10, col="blue", lwd=2)  # True line y = 30 + 10x

# Add legend
legend("topright", legend=c("Data Points", "True Line"), col=c("black", "blue"), pch=c(19, NA), lty=c(NA, 1), lwd=c(NA, 2))


# For Question 2 (b):

# Set parameters
n <- 100  # Number of data points
x_min <- 0
x_max <- 4
residual_sd <- 10

# Generate x values
x <- runif(n, min=x_min, max=x_max)

# Compute true y values based on the line equation
true_y <- 30 + 10 * x

# Add normally distributed residuals to generate y values
y <- true_y + rnorm(n, mean=0, sd=residual_sd)

# Create the plot
plot(x, y, pch=19, xlim=c(x_min, x_max), ylim=c(min(y) - 5, max(y) + 5),
     xlab="x", ylab="y", main="Hypothetical Data with y = 30 + 10x")

# Add the true line
abline(a = 30, b = 10, col="blue", lwd=2)  # True line y = 30 + 10x

# Add legend
legend("topright", legend=c("Data Points", "True Line"), col=c("black", "blue"), pch=c(19, NA), lty=c(NA, 1), lwd=c(NA, 2))

