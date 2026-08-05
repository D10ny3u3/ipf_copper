# qlf data generated from edgeR

res_topTags <- topTags(qlf, n=Inf)$table
res_topTags$gene <- rownames(res_topTags)

str(res_topTags)
# 'data.frame':	16667 obs. of  6 variables:
#   $ logFC : num  -4.45 -2.35 -2.61 -3.46 -3.27 ...
# $ logCPM: num  6.05 9.2 4.82 7.36 3.01 ...
# $ F     : num  866 433 349 273 254 ...
# $ PValue: num  2.38e-08 2.38e-07 4.87e-07 1.09e-06 1.39e-06 ...
# $ FDR   : num  0.000396 0.001981 0.002705 0.003926 0.003926 ...
# $ gene  : chr  "Mmp12" "Ctss" "Ccr5" "Gpnmb" ...

library(ggplot2)

res_topTags$threshold <- "NS"

res_topTags$threshold[
  res_topTags$FDR < 0.05 & res_topTags$logFC > 0
] <- "Up"

res_topTags$threshold[
  res_topTags$FDR < 0.05 & res_topTags$logFC < 0
] <- "Down"

p <- ggplot(res_topTags,
            aes(x = logFC,
                y = -log10(FDR),
                color = threshold)) +
  geom_point(
    size = 1.5,
    alpha = 0.7
  ) +
  scale_color_manual(
    values = c(
      "Down" = "#a1a9d0",
      "NS" = "grey70",
      "Up" = "#f0988c"
    )
  ) +
  geom_vline(
    xintercept = c(-1, 1),
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_hline(
    yintercept = -log10(0.05),
    linetype = "dashed",
    color = "grey50"
  ) +
  theme_classic() + 
  labs(
    x = "log2 Fold Change",
    y = "-log10(FDR)",
    color = NULL
  ) +
  theme(
    # text = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16, face = "bold"), 
    legend.position = "top",
    legend.text = element_text(size = 12)
  )

p

library(ggrepel)

label_gene <- res_topTags[
  order(res_topTags$FDR),
][1:50, ]


p2 <- p +
  geom_text_repel(
    data = label_gene,
    aes(label = gene),
    size = 4,
    color = "black",
    max.overlaps = 20,
    show.legend = FALSE
  )

p2

ggsave(
  "deg_lcc6.svg",
  p2,
  width=4.5,
  height=4
)
