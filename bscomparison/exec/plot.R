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
                         ylim = c(0, 1)) { 
  
  
  
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
    geom_boxplot(aes(x = algorithm_label, y = F1), fill = "grey") +
    scale_x_discrete(limits = c("hybrid", "e-net", "lasso", "forward stepwise", "best subset")) + 
    scale_y_continuous(limits = ylim, expand = c(0, 0)) +
    coord_flip() +
    xlab("") +
    ylab(TeX("Best $F_1$ score")) +
    ggtitle(title) +
    theme_bw() +
    theme(legend.position = "none", 
          plot.margin=unit(c(0,.4,0,0),"cm"))
  
  return(p)
}
