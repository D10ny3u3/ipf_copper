

plot_geneset_pseudobulk_box <- function(
    seu_obj = hab_alv_fib, 
    geneset_name,
    sample_col = "orig.ident",
    group_col = "sample_type",
    y_lab = "y_lab"
){
  
  #-----data------------------------
  # 1. Extract expression
  #-----------------------------
  
  df <- FetchData(
    seu_obj,
    vars = c(
      geneset_name,
      sample_col,
      group_col
    )
  )
  
  df <- na.omit(df)
  
  
  #------average----------------
  # 2. Pseudo-bulk
  #-----------------------------
  
  pb <- aggregate(
    df[[geneset_name]],
    by = list(
      sample = df[[sample_col]],
      group = df[[group_col]]
    ),
    FUN = mean
  )
  
  colnames(pb)[3] <- "expression"
  
  
  if(length(unique(pb$group)) < 2){
    warning("Only one group detected")
    return(NULL)
  }
  
  
  #--------statistic---------------------
  # 3. Wilcoxon
  #-----------------------------
  
  w_res <- wilcox.test(
    expression ~ group,
    data = pb
  )
  
  p_val <- w_res$p.value
  
  
  p_label <- ifelse(
    p_val < 0.001,
    "***",
    ifelse(
      p_val < 0.01,
      "**",
      ifelse(
        p_val < 0.05,
        "*",
        "ns"
      )
    )
  )
  
  
  # #-----------------------------
  # # 4. Trend
  # #-----------------------------
  # 
  # mean_group <- aggregate(
  #   expression ~ group,
  #   data = pb,
  #   mean
  # )
  # 
  # 
  # ipf_mean <- mean_group$expression[
  #   mean_group$group == "IPF"
  # ]
  # 
  # control_mean <- mean_group$expression[
  #   mean_group$group == "Control"
  # ]
  # 
  # 
  # trend <- if(
  #   ipf_mean > control_mean
  # ){
  #   "↑"
  # }else{
  #   "↓"
  # }
  
  
  #------plot-----------------------
  # 5. Plot
  #-----------------------------
  
  p <- ggplot(
    pb,
    aes(
      x = group,
      y = expression,
      fill = group
    )
  ) +
    
    geom_boxplot(
      width = 0.55,
      outlier.shape = NA,
      color = "black",
      size = 0.8
    ) +
    
    geom_jitter(
      width = 0.12,
      size = 2.5,
      shape = 21,
      color = "black"
    ) +
    
    scale_fill_manual(
      values = c(
        "IPF" = sci_colors[2],
        "Control" = sci_colors[1]
      )
    ) +
    
    theme_bw() +
    labs(
      y = y_lab
    ) +
    
    # ggtitle(geneset_name) +
    
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(
        color = "black",
        face = "bold",
        size = 18,
        angle = 45, hjust = 1
      ),
      
      axis.text.y = element_text(
        color = "black",
        face = "bold"
      ),
      axis.title.y = element_text(
        color = "black",
        face = "bold",
        size = 16
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # panel.border = element_rect(
      #   color = "black",
      #   size = 1.2
      # ),
      axis.line = element_line(
        color = "black",
        size = 0.8
      ),
      panel.border = element_blank(),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold.italic"
      ),
      legend.position = "none"
    )
  
  
  #-----annotation------------------------
  # 6. Annotation
  #-----------------------------
  
  max_y <- max(
    pb$expression,
    na.rm = TRUE
  )
  
  line_y  <- max_y * 1.12
  p_y     <- max_y * 1.20
  # trend_y <- max_y * 1.05
  
  
  p <- p +
    annotate(
      "segment",
      x = 1,
      xend = 2,
      y = line_y,
      yend = line_y,
      size = 0.8
    ) +
    annotate(
      "text",
      x = 1.5,
      y = p_y,
      label = p_label,
      size = 6,
      fontface = "bold"
    ) +
    # annotate(
    #   "text",
    #   x = 1.5,
    #   y = trend_y,
    #   label = trend,
    #   color = "red",
    #   size = 6,
    #   fontface = "bold"
    # ) +
    scale_y_continuous(
      expand = expansion(
        mult = c(0.05,0.3)
      )
    )
  
  
  return(p)
}


plot_geneset_pseudobulk_box <- function(
    seu_obj = hab_alv_fib, 
    geneset_name,
    sample_col = "orig.ident",
    group_col = "sample_type"
){
  
  #-----------------------------
  # 1. Extract expression
  #-----------------------------
  
  df <- FetchData(
    seu_obj,
    vars = c(
      geneset_name,
      sample_col,
      group_col
    )
  )
  
  df <- na.omit(df)
  
  
  #-----------------------------
  # 2. Pseudo-bulk
  #-----------------------------
  
  pb <- aggregate(
    df[[geneset_name]],
    by = list(
      sample = df[[sample_col]],
      group = df[[group_col]]
    ),
    FUN = mean
  )
  
  colnames(pb)[3] <- "expression"
  
  
  if(length(unique(pb$group)) < 2){
    warning("Only one group detected")
    return(NULL)
  }
  
  
  #-----------------------------
  # 3. Wilcoxon
  #-----------------------------
  
  w_res <- wilcox.test(
    expression ~ group,
    data = pb
  )
  
  p_val <- w_res$p.value
  
  
  p_label <- ifelse(
    p_val < 0.001,
    "***",
    ifelse(
      p_val < 0.01,
      "**",
      ifelse(
        p_val < 0.05,
        "*",
        "ns"
      )
    )
  )
  
  
  # #-----------------------------
  # # 4. Trend
  # #-----------------------------
  # 
  # mean_group <- aggregate(
  #   expression ~ group,
  #   data = pb,
  #   mean
  # )
  # 
  # 
  # ipf_mean <- mean_group$expression[
  #   mean_group$group == "IPF"
  # ]
  # 
  # control_mean <- mean_group$expression[
  #   mean_group$group == "Control"
  # ]
  # 
  # 
  # trend <- if(
  #   ipf_mean > control_mean
  # ){
  #   "↑"
  # }else{
  #   "↓"
  # }
  
  
  #-----------------------------
  # 5. Plot
  #-----------------------------
  
  p <- ggplot(
    pb,
    aes(
      x = group,
      y = expression,
      fill = group
    )
  ) +
    
    geom_boxplot(
      width = 0.55,
      outlier.shape = NA,
      color = "black",
      size = 0.8
    ) +
    
    geom_jitter(
      width = 0.12,
      size = 2.5,
      shape = 21,
      color = "black"
    ) +
    
    scale_fill_manual(
      values = c(
        "IPF" = sci_colors[2],
        "Control" = sci_colors[1]
      )
    ) +
    
    theme_bw() +
    labs(
      y = geneset_name
    ) +
    
    # ggtitle(geneset_name) +
    
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(
        color = "black",
        face = "bold",
        size = 12
      ),
      axis.text.y = element_text(
        color = "black",
        face = "bold"
      ),
      axis.title.y = element_text(
        color = "black",
        face = "bold",
        size = 6
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # panel.border = element_rect(
      #   color = "black",
      #   size = 1.2
      # ),
      axis.line = element_line(
        color = "black",
        size = 0.8
      ),
      panel.border = element_blank(),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold.italic"
      ),
      legend.position = "none"
    )
  
  
  #-----------------------------
  # 6. Annotation
  #-----------------------------
  
  max_y <- max(
    pb$expression,
    na.rm = TRUE
  )
  
  line_y  <- max_y * 1.12
  p_y     <- max_y * 1.20
  # trend_y <- max_y * 1.05
  
  
  p <- p +
    annotate(
      "segment",
      x = 1,
      xend = 2,
      y = line_y,
      yend = line_y,
      size = 0.8
    ) +
    annotate(
      "text",
      x = 1.5,
      y = p_y,
      label = p_label,
      size = 6,
      fontface = "bold"
    ) +
    # annotate(
    #   "text",
    #   x = 1.5,
    #   y = trend_y,
    #   label = trend,
    #   color = "red",
    #   size = 6,
    #   fontface = "bold"
    # ) +
    scale_y_continuous(
      expand = expansion(
        mult = c(0.05,0.3)
      )
    )
  
  
  return(p)
}
