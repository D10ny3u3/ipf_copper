# Suppl Fig 2A (gsea) ----

library(enrichplot)

il6_gsea <- gseaplot2(
  up_gene_gsea,
  geneSetID = "GO:0032675",
  title = "regulation of interleukin-6 production",
  base_size = 14
)

il6_gsea[[1]] <- il6_gsea[[1]] +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    )
  )

ggsave(
  "il6_gsea.svg",
  il6_gsea,
  width=6.5,
  height=5.5
)
