data <- read.table(
  "featureCounts_merged_count.annot.tsv", 
  header = TRUE, 
  sep = "\t", 
  row.names = 1,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

data <- data[, 1:16]

data_unique <- data %>%
  rowwise() %>%
  mutate(total = sum(c_across(1:15))) %>%
  ungroup() %>%
  group_by(gene_name) %>%
  slice_max(order_by = total, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(-total)

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

data_unique <- as.data.frame(data_unique)
rownames(data_unique) <- data_unique$gene_name
data_unique$gene_name <- NULL

library(edgeR)

dge <- DGEList(counts=data_unique, group=group)
keep <- filterByExpr(dge, group=group)
dge <- dge[keep, , keep.lib.sizes=FALSE]
dge <- normLibSizes(dge)

design <- model.matrix(~ group)
dge <- estimateDisp(dge, design, robust=TRUE)
fit <- glmQLFit(dge, design)

# To compare LPS with PBS
qlf_LPS_PBS <- glmQLFTest(
  fit,
  coef = 2
)

# To compare LCC6 with LPS
contrast <- c(0, -1, 1, 0, 0)
qlf_LCC6_vs_LPS <- glmQLFTest(
  fit,
  contrast = contrast
)
