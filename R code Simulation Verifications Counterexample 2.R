#Counterexample 2: Cyclic Dynamics in a Three-Color Urn
#Simulation of the Urn Process
#We will simulate the three-color urn model with the specified skew-symmetric replacement matrix.


# Parameters
c_value <- 1  # Positive constant
n_steps <- 1000  # Number of steps to simulate
initial_balls <- c(30, 30, 30)  # Initial number of balls of each color

# Initialize vectors to store the composition and proportions
X <- matrix(0, nrow = n_steps + 1, ncol = 3)
P <- matrix(0, nrow = n_steps + 1, ncol = 3)

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
  drawn_color <- sample(1:3, size = 1, prob = p)
  
  # Update composition based on the drawn color
  delta_X <- rep(0, 3)
  if (drawn_color == 1) {
    delta_X <- c(0, c_value, -c_value)
  } else if (drawn_color == 2) {
    delta_X <- c(-c_value, 0, c_value)
  } else if (drawn_color == 3) {
    delta_X <- c(c_value, -c_value, 0)
  }
  
  X[n + 1, ] <- X[n, ] + delta_X
  
  # Ensure non-negativity
  X[n + 1, ] <- pmax(X[n + 1, ], 0)
  
  # Update proportions
  total_balls <- sum(X[n + 1, ])
  if (total_balls == 0) {
    P[n + 1, ] <- NA
  } else {
    P[n + 1, ] <- X[n + 1, ] / total_balls
  }
}





################


# Create a data frame for plotting
df2 <- data.frame(
  Step = rep(0:n_steps, each = 3),
  Proportion = as.vector(t(P)),
  Color = factor(rep(1:3, times = n_steps + 1))
)

# Plot the proportions over time
plot2 <- ggplot(df2, aes(x = Step, y = Proportion, color = Color)) +
  geom_line() +
  labs(
    title = "Proportions Over Time in Three-Color Urn with Cyclic Dynamics",
    x = "Step",
    y = "Proportion",
    color = "Color"
  ) +
  theme_minimal()

# Display the plot
print(plot2)

# Save the plot
ggsave(filename = file.path(output_dir, "ThreeColorUrn_Proportions.png"), plot = plot2)

