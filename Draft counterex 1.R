# R Code for Adjusted Counterexample 1

# Load necessary libraries
library(ggplot2)

# Parameters
a <- 1   # Number of balls added when a color is drawn
p <- 0.5 # Probability of transferring a ball from the other color
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
    X2[n + 1] <- X2[n]
    # Possible transfer from color 2 to color 1
    if (X2[n] > 0 && runif(1) < p) {
      X1[n + 1] <- X1[n + 1] + 1
      X2[n + 1] <- X2[n + 1] - 1
    }
  } else {
    X1[n + 1] <- X1[n]
    X2[n + 1] <- X2[n] + a
    # Possible transfer from color 1 to color 2
    if (X1[n] > 0 && runif(1) < p) {
      X1[n + 1] <- X1[n + 1] - 1
      X2[n + 1] <- X2[n + 1] + 1
    }
  }
  
  # Ensure non-negativity
  X1[n + 1] <- max(X1[n + 1], 0)
  X2[n + 1] <- max(X2[n + 1], 0)
  
  # Update proportion
  total_balls <- X1[n + 1] + X2[n + 1]
  P1[n + 1] <- X1[n + 1] / total_balls
}

# Create a data frame for plotting
df1 <- data.frame(
  Step = 0:n_steps,
  Proportion = P1
)

# Plot the proportion over time
plot1 <- ggplot(df1, aes(x = Step, y = Proportion)) +
  geom_line(color = "blue") +
  labs(
    title = "Proportion of Color 1 Over Time in Two-Color Urn with Transfers",
    x = "Step",
    y = "Proportion of Color 1"
  ) +
  theme_minimal()

# Display the plot
print(plot1)

# Save the plot
ggsave(filename = "TwoColorUrn_Proportion.png", plot = plot1, width = 10, height = 8)
