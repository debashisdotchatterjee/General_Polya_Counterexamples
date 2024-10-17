#Counterexample 3: Non-Standard Growth Rates in a Multi-Color Urn
#Simulation of the Urn Process
#We will simulate a multi-color urn where each color has a different reinforcement factor 


# Parameters
k_values <- c(1, 2, 5)  # Different k_i values for each color
d <- length(k_values)  # Number of colors
n_steps <- 1000  # Number of steps to simulate
initial_balls <- rep(10, d)  # Initial number of balls of each color

# Initialize vectors to store the composition and proportions
X <- matrix(0, nrow = n_steps + 1, ncol = d)
P <- matrix(0, nrow = n_steps + 1, ncol = d)

# Set initial values
X[1, ] <- initial_balls
P[1, ] <- X[1, ] / sum(X[1, ])

# Simulation loop
set.seed(42)  # For reproducibility
for (n in 1:n_steps) {
  # Total number of balls
  S_n <- sum(X[n, ])
  
  # Probability of drawing each color
  p <- X[n, ] / S_n
  
  # Draw a ball
  drawn_color <- sample(1:d, size = 1, prob = p)
  
  # Update composition
  delta_X <- rep(0, d)
  delta_X[drawn_color] <- k_values[drawn_color]
  
  X[n + 1, ] <- X[n, ] + delta_X
  
  # Update proportions
  total_balls <- sum(X[n + 1, ])
  P[n + 1, ] <- X[n + 1, ] / total_balls
}



####################

# Create a data frame for plotting
df3 <- data.frame(
  Step = rep(0:n_steps, each = d),
  Proportion = as.vector(t(P)),
  Color = factor(rep(1:d, times = n_steps + 1))
)

# Plot the proportions over time
plot3 <- ggplot(df3, aes(x = Step, y = Proportion, color = Color)) +
  geom_line() +
  labs(
    title = "Proportions Over Time in Multi-Color Urn with Different Reinforcement Rates",
    x = "Step",
    y = "Proportion",
    color = "Color"
  ) +
  theme_minimal()

# Display the plot
print(plot3)

# Save the plot
ggsave(filename = file.path(output_dir, "MultiColorUrn_Proportions.png"), plot = plot3)


