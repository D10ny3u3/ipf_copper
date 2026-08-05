# qlf generated from edgeR
res <- topTags(qlf, n=Inf)$table

library(ComplexHeatmap)

logCPM <- cpm(dge, log=TRUE)
label_gene <- rownames(res)[order(res$FDR)][1:200]

col_fun <- circlize::colorRamp2(
  c(-2,0,2),
  c("#a1a9d0","white","#f0988c")
)

group <- factor(
  c(
    rep("LCC6", 3),
    rep("LPS", 3),
    rep("MTT", 3),
    rep("PBS", 3),
    rep("TM", 3)
  ),
  levels = c("PBS", "LPS", "LCC6", "MTT", "TM")
)

ha <- HeatmapAnnotation(
  Group = group,
  col = list(
    Group=c(
      "PBS"="#f6cae5",
      "LPS"="#96cccb",
      "LCC6"="#cfeaf1",
      "MTT"="#9e9e9e",
      "TM"="#b883d3"
    )
  )
)

heatmap_mat <- logCPM[label_gene, ]
heatmap_scaled <- t(scale(t(heatmap_mat)))  

p <- Heatmap(
  heatmap_scaled,
  name="Z-score",
  col=col_fun,
  top_annotation=ha,
  show_row_names=FALSE,
  show_column_names=TRUE,
  cluster_columns=TRUE,
  cluster_rows=TRUE,
  # column_title="LPS-induced transcriptional response",
  heatmap_legend_param=list(
    title="Expression"
  )
)

p

genes <- c(
  "Il1a", "Il1b", "Il6", "Tnf", "Nos2", "Clec4e", 
  "Cxcl2", "Lcn2", "Irak2", "Malt1", "Ripk2", 
  "Hmox1", "Gdf15", "Ddit3", "Hsph1", 
  "Slc31a1", "Atp7a", "Cd44")

genes <- as.data.frame(genes)

svg("heatmap.svg", 
    width = 5,
    height = 5,
    # onefile = TRUE,
    # family = "Helvetica",
    # paper = "special",
    # useDingbats = FALSE
)
p + rowAnnotation(
  link = anno_mark(
    at = which(rownames(heatmap_scaled) %in% genes$genes), 
    labels = genes$genes, labels_gp = gpar(fontsize = 10)))

dev.off()
