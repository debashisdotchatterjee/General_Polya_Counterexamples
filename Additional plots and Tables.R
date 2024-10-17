#Additional Tables and Summaries
#Final Proportions
#We can create tables showing the final proportions of each color in each simulation.

# Final proportions for Counterexample 1
cat("Final Proportion of Color 1 in Two-Color Urn:\n")
print(P1[n_steps + 1])

# Final proportions for Counterexample 2
cat("\nFinal Proportions in Three-Color Urn:\n")
print(P[n_steps + 1, ])

# Final proportions for Counterexample 3
cat("\nFinal Proportions in Multi-Color Urn:\n")
print(P[n_steps + 1, ])


#################

# Save final proportions for Counterexample 1
write.csv(
  data.frame(Step = 0:n_steps, Proportion = P1),
  file = file.path(output_dir, "TwoColorUrn_Proportions.csv"),
  row.names = FALSE
)

# Save final proportions for Counterexample 2
write.csv(
  data.frame(Step = rep(0:n_steps, each = 3), Proportion = as.vector(t(P)), Color = rep(1:3, times = n_steps + 1)),
  file = file.path(output_dir, "ThreeColorUrn_Proportions.csv"),
  row.names = FALSE
)

# Save final proportions for Counterexample 3
write.csv(
  data.frame(Step = rep(0:n_steps, each = d), Proportion = as.vector(t(P)), Color = rep(1:d, times = n_steps + 1)),
  file = file.path(output_dir, "MultiColorUrn_Proportions.csv"),
  row.names = FALSE
)
