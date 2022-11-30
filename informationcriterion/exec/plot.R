library(ggplot2)
library(latex2exp)

rotate <- function(x) t(apply(x, 2, rev))

plot_corrmat2 <- function(corrmat, xlabel = "", title = "", legend = FALSE, betas = NULL) {
  
  if (!is.null(betas)) { 
    betas[which(betas == 1)] <- NA
    betamat <- diag(betas)
    corrmat <- corrmat + betamat # set the values to NA
  }
  
  melted_corrmat <- reshape2::melt(rotate(corrmat))
  
  p <- ggplot(data = melted_corrmat, aes(x=Var1, y=Var2, fill=value)) +
    geom_tile() +
    theme_minimal() +
    xlab(xlabel) +
    ggtitle(title) + 
    ylab("") +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    scale_fill_gradient(low = "#f9f9f9", high = "#1763AA", name = TeX("R^2"), na.value = "#fa8537") +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
          panel.grid.minor = element_line(colour = "black", size = 5)) + 
    coord_fixed()
  
  if (!legend) {
    p <- p + theme(legend.position = "none")
  }
  
  p
}

plot_results <- function(results, n = 1000, p = 100, s = 10, dimensionality = "low",
                         corr_type = "independent", rho = 0, beta_type = "first", 
                         snr = 0.05, 
                         title = "", 
                         ylim = c(0, 1), 
                         score = c("F1", "AIC", "BIC", "AICc")) { 
  
  
  
  ### get the data 
  n_ = n; p_ = p; s_ = s; dimensionality_ = dimensionality;
  corr_type_ = corr_type; rho_ = rho; beta_type_ = beta_type; 
  snr_ = snr 
  
  results <- results %>% dplyr::filter(
    n == n_,
    p == p_,
    s == s_,
    dimensionality == dimensionality_, 
    corr_type == corr_type_,
    rho == rho_,
    beta_type == beta_type_,
    snr == snr_
  ) 
  
  p <- ggplot(results) +
    geom_boxplot(aes(x = algorithm_label, y = score), fill = "grey") +
    scale_x_discrete(limits = c("e-net", "lasso", "forward stepwise")) + 
    #scale_y_continuous(limits = ylim, expand = c(0, 0)) +
    coord_flip() +
    xlab("") +
    ylab(sprintf("Best %s score", score[1])) +
    ggtitle(title) +
    theme_bw() +
    theme(legend.position = "none", 
          plot.margin=unit(c(0,.4,0,0),"cm"))
  
  return(p)
}



plot_comparison <- function(results, n = 100, p = 100, s = 10, dimensionality = "low",
                            corr_type = "block", rho = 0.7, beta_type = "spread", 
                            snr = 0.05,
                            ylim = c(NA, NA), 
                            title = "",
                            rotate_angle = 90, 
                            vjust = 0.5,
                            hjust = 1,
                            dotted_lines = c(4.5, 8.5)) { 
  
  ### get the data 
  n_ = n; p_ = p; s_ = s; dimensionality_ = dimensionality;
  corr_type_ = corr_type; rho_ = rho; beta_type_ = beta_type; 
  snr_ = snr 
  
  results <- results %>% dplyr::filter(
    n == n_,
    p == p_,
    s == s_,
    dimensionality == dimensionality_, 
    corr_type == corr_type_,
    rho == rho_,
    beta_type == beta_type_,
    snr == snr_
  ) 
  
  
  p <- ggplot(data = results) + 
    geom_boxplot(mapping = aes(x = algorithm_label, y = score)) + 
    ylab("F1 score") + 
    xlab("") + 
    ggtitle(title) + 
    theme(axis.text.x = element_text(angle = rotate_angle, vjust = vjust, hjust=hjust)) + 
    geom_vline(xintercept = dotted_lines[1], linetype = "dotted") +
    geom_vline(xintercept = dotted_lines[2], linetype = "dotted") 
  
  if (!is.na(ylim[1])) { 
    p <- p + ylim(ylim) 
  } 
  
  return(p)
}




plotSettings <-
  function(data, 
           ylim = NULL,
           title = "") {
    
    
    labels = c(
      '1' = 'no bystanders',
      '2' = parse(text = TeX("$\\gamma = .5$")),
      '3' = parse(text = TeX("$\\gamma = .75$")),
      '4' = parse(text = TeX("$\\gamma = .9$")),
      '5' = parse(text = TeX("$\\gamma = .5$")),
      '6' = parse(text = TeX("$\\gamma = .75$")),
      '7' = parse(text = TeX("$\\gamma = .9$"))
    )
    
    
    p <- ggplot(data = data) +
      geom_boxplot(aes(x = id, y = PRCAUC), fill = 'grey90') +
      #scale_x_discrete(breaks = unique(data$method), labels = ml) +
      scale_y_continuous(expand = c(0, 0), limits = ylim) +
      #coord_flip() +
      xlab("") +
      ylab("Area under the PRC") +
      ggtitle(title) +
      theme_bw() +
      theme(legend.position = "none") +
      geom_vline(xintercept = 1.5, linetype = "dotted") +
      geom_vline(xintercept = 4.5, linetype = "dotted") +
      theme(
        panel.grid.major.x = element_line(size = .1, color = "grey"),
        panel.grid.major.y = element_blank()
      ) + theme(axis.text.x = element_text(angle = 0)) +
      scale_x_discrete(labels = labels)
    
    
    g <- ggplotGrob(p)
    g <-
      gtable_add_grob(
        g,
        grobTree(
          textGrob(
            "125 innocent bystanders",
            x = unit(0.4, "npc"),
            y = unit(0.03, "npc"),
            gp = gpar(fontsize = 9, col = "grey20")
          ),
          textGrob(
            "250 innocent bystanders",
            x = unit(0.8, "npc"),
            y = unit(0.03, "npc"),
            gp = gpar(fontsize = 9, col = "grey20")
          )
        ),
        t = 1,
        l = 1,
        b = 10,
        r = 7
      )
    
    p <- grid.draw(g)
    
    # ## Add the second title and plot
    # g2 <- gtable_add_grob(g, textGrob("Right", x=1, hjust=1, gp=title_style),
    #                       t=2, l=4, b=2, r=4, name="right-title")
    
    return(p)
  }


