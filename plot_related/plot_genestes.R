# human is list of genesets.

copper_genesets <- human %>%
  filter(grepl("copper", gs_name, ignore.case = TRUE)) %>%
  dplyr::select(gs_name, gene_symbol) %>% 
  split(x = .$gene_symbol, f = .$gs_name)

gobp_copper_genesets <- copper_genesets[grep("^GOBP_", names(copper_genesets))]

# hab_epi is a seurat obj.

table(hab_epi$sample_type)

hab_epi$sample_type <- factor(
  hab_epi$sample_type,
  levels = c("control", "ipf"),
  labels = c("Control", "IPF")
)

library(Seurat)
hab_epi <- JoinLayers(hab_epi)
hab_epi <- AddModuleScore(
  object = hab_epi,
  features = gobp_copper_genesets,
  ctrl = 100,
  name = names(gobp_copper_genesets)
)

original_names <- grep("COPPER", names(hab_epi@meta.data), value = TRUE)
clean_names <- sub("\\d+$", "", original_names)
for(i in seq_along(original_names)){
  old_name <- original_names[i]
  new_name <- clean_names[i]
  hab_epi[[new_name]] <- hab_epi[[old_name]]
  hab_epi[[old_name]] <- NULL
}


