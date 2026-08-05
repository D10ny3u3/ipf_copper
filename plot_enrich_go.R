# down_genes_go generated from clusterProfiler

library(org.Mm.eg.db)
library(clusterProfiler)

barplot(
  down_genes_go, showCategory = 10)

dotplot(
  down_genes_go, showCategory = 20,
  font.size = 8)

dotplot(
  down_genes_go, showCategory = 20,
  font.size = 8)+
  theme_classic()

selected_pathways <- c(
  "leukocyte migration", 
  "leukocyte chemotaxis", 
  "regulation of inflammatory response",
  "positive regulation of MAPK cascade", 
  "cell killing",
  "adaptive immune response",
  "regulation of interleukin-6 production",
  "interleukin-1 production"
)

res <- down_genes_go@result
new_results <- subset(res, res$Description %in% selected_pathways)

down_genes_go_selected <- down_genes_go
down_genes_go_selected@result <- new_results

p <- dotplot(
  down_genes_go_selected,
  # showCategory = 20,
  font.size = 8
) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

p

ggsave(
  "go_lcc6.svg",
  p,
  width=8,
  height=5
)

