library(ggplot2)

risks <- data.frame(
  id = c("R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8"),
  likelihood = c(4, 3, 3, 3, 3, 2, 2, 2),
  impact = c(4, 5, 5, 3, 3, 3, 3, 2)
)

grid <- expand.grid(likelihood = 1:5, impact = 1:5)
grid$score <- grid$likelihood * grid$impact
grid$band <- cut(grid$score, breaks = c(0, 4, 9, 14, 25),
                 labels = c("Low", "Medium", "High", "Critical"))

labels <- aggregate(id ~ likelihood + impact, data = risks,
                    FUN = paste, collapse = "  ")

bg   <- "#0d1117"
text <- "#e6edf3"
dim  <- "#8b949e"

ggplot(grid, aes(likelihood, impact)) +
  geom_tile(aes(fill = band), width = 0.92, height = 0.92, alpha = 0.9) +
  geom_text(aes(x = likelihood + 0.38, y = impact + 0.36, label = score),
            colour = text, alpha = 0.55, size = 3, hjust = 1, vjust = 1) +
  geom_text(data = labels, aes(label = id),
            colour = "white", fontface = "bold", size = 5) +
  scale_fill_manual(values = c(Low = "#238636", Medium = "#bf8700",
                               High = "#db6d28", Critical = "#da3633"),
                    name = NULL, drop = FALSE) +
  scale_x_continuous(breaks = 1:5, expand = c(0.01, 0.01),
                     labels = c("Rare", "Unlikely", "Possible",
                                "Likely", "Almost\ncertain")) +
  scale_y_continuous(breaks = 1:5, expand = c(0.01, 0.01),
                     labels = c("Minimal", "Minor", "Moderate",
                                "Major", "Severe")) +
  coord_fixed() +
  labs(title = "ING Group · DORA Risk Heatmap",
       subtitle = "Outside-in review of 2025 public disclosures · draft scores",
       x = "LIKELIHOOD", y = "IMPACT") +
  theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = bg, colour = NA),
    panel.background = element_rect(fill = bg, colour = NA),
    panel.grid       = element_blank(),
    plot.title       = element_text(colour = text, face = "bold", size = 18),
    plot.subtitle    = element_text(colour = dim, margin = margin(b = 15)),
    plot.title.position = "plot",
    axis.text        = element_text(colour = dim),
    axis.title       = element_text(colour = dim, face = "bold", size = 10),
    axis.title.x     = element_text(margin = margin(t = 10)),
    axis.title.y     = element_text(margin = margin(r = 10)),
    legend.position  = "bottom",
    legend.text      = element_text(colour = text),
    legend.key.size  = unit(0.5, "cm"),
    plot.margin      = margin(20, 20, 20, 20)
  )

ggsave("ing_dora_heatmap.png", width = 8, height = 8.5, dpi = 300, bg = bg)
