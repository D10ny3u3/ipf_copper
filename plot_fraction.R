# fig 1B (fib constitution) ----

library(tidyverse)
library(patchwork)

cols =c("#a1a9d0", "#f0988c", "#b883d3", "#9e9e9e",
         "#cfeaf1", "#c4a5de", "#f6cae5", "#96cccb")

table(seuobj$sample_type)

seuobj$sample_type <- factor(
  seuobj$sample_type,
  levels = c("control", "ipf"),
  labels = c("Control", "IPF")
)

seuobj$Classification <- factor(seuobj$Classification)

sample_table <- as.data.frame(
    table(seuobj$sample_type, seuobj$Classification
))

names(sample_table) <- c("sample_type","Cell_Type", "Cell_Number")

plot_sample <- ggplot(sample_table, aes(
    x=sample_type, weight=Cell_Number, fill=Cell_Type)) +
  geom_bar(position="fill") +
  scale_fill_manual(values=cols) + 
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        axis.line.x = element_line(colour = "black") ,
        axis.text.x = element_text(size=18, face="bold", colour = "black"), # obvious x text
        axis.text.y = element_blank(),
        axis.title.y = element_text(size=16, face="bold"),
        axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.line.y = element_line(colour = "black") ,
        legend.position = "none" # same with umap
  ) +
  labs(y="Percentage") +
  RotatedAxis() + 
  labs(x = NULL) +
  scale_y_continuous(labels = NULL, expand = c(0,0))

plot_sample

ggsave(
  "fraction_fib.svg",
  plot_sample,
  width=1.5,
  height=4
)
