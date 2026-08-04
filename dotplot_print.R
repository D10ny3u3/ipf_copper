features = c(
    "LYZ", "CD68", "EPCAM", "KRT8", "CLDN5", 
    "VWF", "CCL5", "CD3E", "DCN", "COL1A2"
)

dot_plot_anything <- DotPlot(
  hab_copper,
  features = features, 
  assay = "RNA",
  # scale.max = 80
) + 
  # NoLegend() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 14),
    axis.text = element_text(size = 16, face = "bold")
  ) +
  labs(y = NULL, x = NULL) +
  scale_color_gradient(
    low  = "#f0ecf5",   # 极浅的紫色（接近白色但有底色）
    high = "#a1a9d0"    # 目标蓝色
  )

ggsave(
  "dot_plot_anything.svg",
  dot_plot_anything,
  width=5,
  height=4
)