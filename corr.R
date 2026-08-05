# theta_state generated from BayesPrism 

colnames(theta_state)

df <- data.frame(
  AM = theta_state[, "AM"],
  Aerocytes = theta_state[, "aerocytes"]
)

df$sample <- rownames(theta_state)

library(ggplot2)

p <- ggplot(df, aes(x = AM, y = Aerocytes)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(
    method = "lm",          # 线性趋势
    se = TRUE,              # 显示置信区间
    color = "#b883d3",
    fill = "#f6cae5",
    alpha = 0.2
  ) +
  # theme_classic(base_size = 14) +  
  theme_classic() +  
  theme(
    # text = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 18, face = "bold"), 
  ) +
  labs(
    x = "AM",
    y = "Aerocytes",
    # title = "Correlation between AM and Aerocytes"
  )

p2 <- p +
  geom_text_repel(
    aes(label = rownames(theta_state)),
    size = 5,
    box.padding = 0.3,
    point.padding = 0.3,
    max.overlaps = Inf
  )

p2

ggsave(
  "cor_am_aero.svg",
  p2,
  width=4.2,
  height=4
)
