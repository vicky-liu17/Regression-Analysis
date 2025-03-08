# Basic R Script Example
library(car)
# Print a welcome message
print("Welcome to R Programming!")

# Create some basic variables
x <- 10
y <- 20

# Perform basic arithmetic
sum <- x + y
print(paste("The sum of", x, "and", y, "is:", sum))

# Create a vector
numbers <- c(1, 2, 3, 4, 5)
print("A vector of numbers:")
print(numbers)

# Calculate mean and standard deviation
mean_value <- mean(numbers)
sd_value <- sd(numbers)
print(paste("Mean of the vector:", mean_value))
print(paste("Standard deviation of the vector:", sd_value))

# Create a simple plot
png("plot.png")  # Create a PNG device
plot(numbers, type = "o", col = "blue", 
     main = "Simple Line Plot",
     xlab = "Index", ylab = "Value")
dev.off()  # Close the PNG device

# Create a simple data frame
df <- data.frame(
  name = c("Alice", "Bob", "Charlie"),
  age = c(25, 30, 35),
  score = c(88, 92, 85)
)
print("A sample data frame:")
print(df) 