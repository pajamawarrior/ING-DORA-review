library(ggplot2)

# Risk register scores from the doc (draft)
risks <- data.frame(
  id = c("R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8"),
  likelihood = c(4, 3, 3, 3, 3, 2, 2, 2),
  impact = c(4, 5, 5, 3, 3, 3, 3, 2)
)

# Background 5x5 grid, coloured by likelihood x impact
grid <- expand.grid(likelihood = 1:5, impact = 1:5)
grid$score <- grid$likelihood * grid$impact

# Group risk IDs that land in the same cell
labels <- aggregate(id ~ likelihood + impact, data = risks,
                    FUN = paste, collapse = ", ")

ggplot(grid, aes(likelihood, impact, fill = score)) +
  geom_tile(colour = "white") +
  scale_fill_gradient2(low = "#4caf50", mid = "#ffeb3b",
                       high = "#e53935", midpoint = 12.5) +
  geom_text(data = labels,
            aes(x = likelihood, y = impact, label = id),
            inherit.aes = FALSE, size = 5, fontface = "bold") +
  labs(title = "ING DORA Review – Risk Heatmap (draft)",
       x = "Likelihood", y = "Impact", fill = "Score") +
  theme_minimal()

ggsave("ing_dora_heatmap.png", width = 7, height = 6, dpi = 300)