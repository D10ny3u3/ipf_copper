## plot ----

library(ggplot2)

num_clusters <- length(unique(hab_fib$Classification))
sci_colors <- c(
  "#a1a9d0", "#f0988c", "#b883d3", "#9e9e9e",
  "#cfeaf1", "#c4a5de", "#f6cae5", "#96cccb"
)
sci_palette <- rep(sci_colors, length.out = num_clusters)

p <- DimPlot(
  hab_fib,
  reduction   = "umap.harmony",
  group.by    = "Classification",
  # label       = TRUE, # No label showed
  # label.size  = 5,
  cols        = sci_palette,
  repel       = TRUE,
  combine     = TRUE
) +
  ggtitle("Fibroblasts") + # Title descripting Cell Type
  labs(
    x = "UMAP_1",
    y = "UMAP_2"
  ) +
  theme(
    axis.title = element_text(size = 16, face = "bold"), # axis title bold and large
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)  # plot title bold and large
  ) 
p

ggsave(
  "UMAP_fib.svg",
  p,
  width=5,
  height=4
)