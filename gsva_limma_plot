library(limma)
library(ggprism)
library(ggplot2)
library(stringr)
library(dplyr)

gsva_limma_plot <- function(
    gsva_es,
    group_vector,
    group1 = "IPF",
    group2 = "Control",
    t_cutoff = 2,
    y_limit = 20,
    out_png = NULL
){
  group <- factor(group_vector)
  design <- model.matrix(~0 + group)
  colnames(design) <- levels(group)
  rownames(design) <- colnames(gsva_es)
  contrast_str <- paste0(group1, " - ", group2)
  contrast <- makeContrasts(contrasts = contrast_str, levels = design)
  fit <- lmFit(gsva_es, design)
  fit2 <- contrasts.fit(fit, contrast)
  fit3 <- eBayes(fit2)
  Diff <- topTable(fit3, coef = 1, number = Inf)
  dat_plot <- data.frame(
    id = rownames(Diff),
    t = Diff$t
  )
  dat_plot$id <- str_replace(dat_plot$id , "HALLMARK_","")
  dat_plot$threshold <- factor(
    ifelse(dat_plot$t >= t_cutoff, "Up",
           ifelse(dat_plot$t <= -t_cutoff, "Down", "NoSignifi")),
    levels = c("Up","Down","NoSignifi")
  )
  dat_plot <- dat_plot %>% arrange(t)
  dat_plot$id <- factor(dat_plot$id, levels = dat_plot$id)
  p <- ggplot(dat_plot, aes(x = id, y = t, fill = threshold)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c(
      'Up'= '#f0988c',
      'NoSignifi'='#9e9e9e',
      'Down'='#a1a9d0'
    )) +
    geom_hline(yintercept = c(-t_cutoff, t_cutoff),
               color = 'white', linewidth = 0.5, lty='dashed') +
    xlab('') +
    ylab(paste0("t value of GSVA score, ", group1, " vs ", group2)) +
    guides(fill = "none") +
    theme_prism(border = TRUE) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 16)
    ) +
    scale_y_continuous(
      limits = c(-y_limit, y_limit),
      breaks = seq(-y_limit, y_limit, by = y_limit/2),
      expand = c(0, 0)
    ) +
    geom_text(
      data = subset(dat_plot, t < -t_cutoff),
      aes(x = id, y = 0.1, label = id),
      hjust = 0, color = "black", size = 3
    ) +
    geom_text(
      data = subset(dat_plot, t >= -t_cutoff & t < 0),
      aes(x = id, y = 0.1, label = id),
      hjust = 0, color = "grey", size = 3
    ) +
    geom_text(
      data = subset(dat_plot, t >= 0 & t < t_cutoff),
      aes(x = id, y = -0.1, label = id),
      hjust = 1, color = "grey", size = 3
    ) +
    geom_text(
      data = subset(dat_plot, t >= t_cutoff),
      aes(x = id, y = -0.1, label = id),
      hjust = 1, color = "black", size = 3
    )
  if (!is.null(out_png)) {
    ggsave(out_png, p, width = 7, height = 7)
  }
  return(list(
    diff = Diff,
    plot = p
  ))
}
