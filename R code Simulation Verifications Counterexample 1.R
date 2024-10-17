# Create a directory to save outputs
output_dir <- "PolyaUrnCounterexamples"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}
#Counterexample 1: Non-Convergence in a Two-Color Urn

# Load necessary libraries
library(ggplot2)

# Parameters
a <- 5   # Positive constant greater than b
b <- 3   # Positive constant less than a
n_steps <- 1000  # Number of steps to simulate
initial_balls <- c(50, 50)  # Initial number of balls of each color

# Initialize vectors to store the composition and proportions
X1 <- numeric(n_steps + 1)
X2 <- numeric(n_steps + 1)
P1 <- numeric(n_steps + 1)

# Set initial values
X1[1] <- initial_balls[1]
X2[1] <- initial_balls[2]
P1[1] <- X1[1] / (X1[1] + X2[1])

# Simulation loop
set.seed(42)  # For reproducibility
for (n in 1:n_steps) {
  # Total number of balls
  S_n <- X1[n] + X2[n]
  
  # Probability of drawing each color
  p1 <- X1[n] / S_n
  p2 <- X2[n] / S_n
  
  # Draw a ball
  drawn_color <- sample(c(1, 2), size = 1, prob = c(p1, p2))
  
  # Update composition based on the drawn color
  if (drawn_color == 1) {
    X1[n + 1] <- X1[n] + a
    X2[n + 1] <- X2[n] - b
  } else {
    X1[n + 1] <- X1[n] - b
    X2[n + 1] <- X2[n] + a
  }
  
  # Ensure non-negativity
  X1[n + 1] <- max(X1[n + 1], 0)
  X2[n + 1] <- max(X2[n + 1], 0)
  
  # Update proportion
  total_balls <- X1[n + 1] + X2[n + 1]
  if (total_balls == 0) {
    P1[n + 1] <- NA
  } else {
    P1[n + 1] <- X1[n + 1] / total_balls
  }
}

######################

# Create a data frame for plotting
df1 <- data.frame(
  Step = 0:n_steps,
  Proportion = P1
)

# Plot the proportion over time
plot1 <- ggplot(df1, aes(x = Step, y = Proportion)) +
  geom_line(color = "blue") +
  labs(
    title = "Proportion of Color 1 Over Time in Two-Color Urn",
    x = "Step",
    y = "Proportion of Color 1"
  ) +
  theme_minimal()

# Display the plot
print(plot1)

# Save the plot
ggsave(filename = file.path(output_dir, "TwoColorUrn_Proportion.png"), plot = plot1)

