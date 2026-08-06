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

library(ggplot2)

sci_colors <- c(
  "#a1a9d0", "#f0988c", "#b883d3", "#9e9e9e",
  "#cfeaf1", "#c4a5de", "#f6cae5", "#96cccb"
)

plots <- lapply(
  clean_names,
  function(x){
    plot_geneset_pseudobulk_box(
      seu_obj = hab_epi,
      geneset_name = x
    )
  }
)

library(patchwork)
combined_plot <- wrap_plots(plots, ncol = 4) 
print(combined_plot)
ggsave(
  filename = "hab_epi_copper.pdf", 
  plot = combined_plot,          
  width = 20,                      
  height = 10,                   
  dpi = 300                      
)

epi_response_copper <- 
  plot_geneset_pseudobulk_box(
    seu_obj = hab_epi, 
    "GOBP_CELLULAR_RESPONSE_TO_COPPER_ION",
    y_lab = "CELL_RESP_COPPER_ION")

ggsave(
  "epi_response_copper.svg",
  epi_response_copper,
  width=2,
  height=4
)

epi_transport <- 
  plot_geneset_pseudobulk_box(
    seu_obj = hab_epi, 
    "GOBP_COPPER_ION_TRANSPORT",
    y_lab = "COPPER_ION_TRANSPORT")

ggsave(
  "epi_transport.svg",
  epi_transport,
  width=2,
  height=4
)


