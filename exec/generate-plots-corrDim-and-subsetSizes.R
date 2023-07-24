# Generate Figure 8 (correlation and dimensionality) and Figure 10 (performance
# based on subset size)

## Effect of k
Corr <- c("block")
Beta <- c("first")
Dim <- c("low")
Rho <- 0.7

raw_results <- 
  readRDS(paste("./results/raw_results_",
                Dim, "_",
                Beta, "_",
                Corr, ".RDS",
                sep="")
  )

# name the list
names(raw_results) <- c("job.id",         "problem"    ,    "algorithm"    ,  "n"           ,
                        "p"           ,   "s"           ,   "dimensionality", "corr_type" ,    
                        "rho"         ,   "beta_type"    ,  "snr"        ,    "k"      ,       
                        "alpha"      ,    "result" )

# get unique SNRs
SNR <- unique(raw_results$snr)

out_snr <- lapply(SNR, function(Snr){
  
  # find results for above parameter setting
  indices <- which(raw_results$dimensionality == Dim & 
                     raw_results$rho == Rho & 
                     raw_results$snr == Snr & 
                     raw_results$corr_type == Corr & 
                     raw_results$beta_type == Beta)
  
  out <- lapply(indices, function(x){
    data_tmp <- raw_results$result[[x]]
    
    data_tmp$TP <- as.numeric(data_tmp$TP)
    data_tmp$TN <- as.numeric(data_tmp$TN)
    data_tmp$FP <- as.numeric(data_tmp$FP)
    data_tmp$FN <- as.numeric(data_tmp$FN)
    
    data_tmp$MCC <- 
      (data_tmp$TP*data_tmp$TN - data_tmp$FP*data_tmp$FN)/
      sqrt(
        (data_tmp$TP + data_tmp$FP) * 
          (data_tmp$TP + data_tmp$FN) * 
          (data_tmp$TN + data_tmp$FP) * 
          (data_tmp$TN + data_tmp$FN))
    
    data_tmp$F2 <- 
      ((1+2^2)*data_tmp$TP)/
      ((1+2^2)*data_tmp$TP +
         2^2*data_tmp$FN + 
         data_tmp$FP)
    
    data_tmp <- data_tmp[data_tmp$k <= 50,]
    data_tmp
  })
  
  out <- do.call(rbind, out)
  
  
  no_enets <- as_tibble(out[is.na(out$alpha) | out$alpha==1,]) %>% 
    mutate(method = replace(method, alpha == 1, "lasso"))
  
  enets <- as_tibble(out[out$alpha>0 & out$alpha<1 & out$method=="enet",])
  enets$alpha <- round(enets$alpha,2)
  enets <- enets %>% 
    mutate(method = replace(method, alpha == 0.1, "enet_0.1")) %>% 
    mutate(method = replace(method, alpha == 0.2, "enet_0.2")) %>%
    mutate(method = replace(method, alpha == 0.3, "enet_0.3")) %>%
    mutate(method = replace(method, alpha == 0.4, "enet_0.4")) %>%
    mutate(method = replace(method, alpha == 0.5, "enet_0.5")) %>%
    mutate(method = replace(method, alpha == 0.6, "enet_0.6")) %>%
    mutate(method = replace(method, alpha == 0.7, "enet_0.7")) %>%
    mutate(method = replace(method, alpha == 0.8, "enet_0.8")) %>%
    mutate(method = replace(method, alpha == 0.9, "enet_0.9"))
  
  enets <- enets[enets$method != "enet",]
  
  one_setting <- bind_rows(enets, no_enets)
  
  # dismiss hybrid (we implemented our idea of a two step procedure by a
  # pre-selection via Enet/Lasso followed by BSS; these results are not
  # part of the paper "Variable selection in linear regression models: 
  # choosing the best subset is not always the best choice" and are 
  # therefore dismissed from the following analysis.)
  one_setting <- one_setting[one_setting$method != "enet_bs_hybrid", ]
  
  one_setting$method <- factor(one_setting$method,
                               levels = 
                                 c("enet_0.1",
                                   "enet_0.2",
                                   "enet_0.3",
                                   "enet_0.4",
                                   "enet_0.5", 
                                   "enet_0.6",
                                   "enet_0.7",
                                   "enet_0.8",
                                   "enet_0.9", 
                                   "lasso",
                                   "fs",
                                   "bs" ))
  levels(one_setting$method)[levels(one_setting$method) == "enet_0.1"] <- 
    "Enet 0.1"
  levels(one_setting$method)[levels(one_setting$method) == "enet_0.2"] <-
    "Enet 0.2"
  levels(one_setting$method)[levels(one_setting$method) == "enet_0.3"] <-
    "Enet 0.3"
  levels(one_setting$method)[levels(one_setting$method) == "enet_0.4"] <-
    "Enet 0.4"
  levels(one_setting$method)[levels(one_setting$method) == "enet_0.5"] <- 
    "Enet 0.5"
  levels(one_setting$method)[levels(one_setting$method) == "enet_0.6"] <-
    "Enet 0.6"
  levels(one_setting$method)[levels(one_setting$method) == "enet_0.7"] <-
    "Enet 0.7"
  levels(one_setting$method)[levels(one_setting$method) == "enet_0.8"] <-
    "Enet 0.8"
  levels(one_setting$method)[levels(one_setting$method) == "enet_0.9"] <- 
    "Enet 0.9"
  levels(one_setting$method)[levels(one_setting$method) == "lasso"] <- 
    "Lasso"
  levels(one_setting$method)[levels(one_setting$method) == "fs"] <- 
    "FSS"
  levels(one_setting$method)[levels(one_setting$method) == "bs"] <- 
    "BSS"
  
  one_setting$snr <- Snr
  one_setting$rho <- Rho
  one_setting$dim <- Dim
  one_setting$beta_position <- Beta
  one_setting$corr_struc <- Corr
  
  
  names(one_setting)[c(1,12,13)] <- c("Method", 
                                      "Recall", 
                                      "Precision"
  )
  
  
  one_setting <- one_setting %>% 
    pivot_longer(c("F1",
                   "F2",
                   "MCC",
                   "Recall",
                   "Precision"), 
                 names_to = 'Metric', 
                 values_to = 'Value')
  
})

out_snr <- do.call(rbind, out_snr)

out_snr[is.na(out_snr$Value) & out_snr$k <=15, ]$Value  <- 0

p <- unique(out_snr$p)
s <- unique(out_snr$s)

out_snr.summarised <- 
  out_snr%>% 
  group_by(snr, rho, dim, corr_struc, beta_position, Method, k, Metric) %>%
  summarise(mean.Value=mean(Value, na.rm=TRUE),
            sd.Value=sd(Value, na.rm=TRUE))



ggplot(out_snr.summarised %>% 
         filter(Method %in% 
                  c("Enet 0.1", "Enet 0.5", "Enet 0.9", "Lasso", "FSS", "BSS") & 
                  Metric %in% 
                  c("F1", "Recall", "Precision") & 
                  rho == 0.7 & 
                  snr==0.71,
                k<=15), 
       aes(x=k, y=mean.Value, color=Method)) + 
  geom_vline(xintercept = 10, alpha=0.5, linetype="dashed") +
  geom_line() + 
  facet_wrap(~Metric, ncol=3) + 
  scale_color_manual(name = "Method",
                     values = c(
                       "#FF99CC",
                       "#FF66FF",
                       "#B266FF",
                       "#FF3333",
                       "#0080FF",
                       "#00CC00")
  ) +
  ylim(0,1) + 
  xlab("Subset size k") + 
  ylab("Value")+
  scale_x_continuous(breaks=c(0,5,10,15), minor_breaks = 1:15)


ggsave(file="./plots/Figure_10.png",
       height = 1000, width = 3000, units = "px", dpi=300)



# effect of Correlation and Dimension

# plot for different correlation

Corr <- c("block")
Beta <- c("first")
DIM <- c("low", "medium", "high")

Snr <-   2.07


out <- lapply(DIM, function(Dim){
  
  # load results
  
  raw_results <- NULL
  
  tryCatch(raw_results <- 
    readRDS(paste("./results/raw_results_",
                  Dim, "_",
                  Beta, "_",
                  Corr, ".RDS",
                  sep="")
    ), error = function(e) cat(paste("No data for ", Dim, 
                                     "-dimensional setting found ! \n", sep=""))
  )
  
  if(is.null(raw_results)){
    out_dim <- NULL
  }else{
    # rename list elements
    names(raw_results) <- c("job.id",         "problem"    ,    "algorithm"    ,  "n"           ,
                            "p"           ,   "s"           ,   "dimensionality", "corr_type" ,    
                            "rho"         ,   "beta_type"    ,  "snr"        ,    "k"      ,       
                            "alpha"      ,    "result" )
    
    
    SNR <- unique(raw_results$snr)
    
    
    RHO <- unique(raw_results$rho)
    
    # generate tibble for rho =c(0.35, 0.7)
    out_rho_block <- lapply(RHO, function(Rho){
      
      indices <- which(raw_results$dimensionality == Dim & 
                         raw_results$rho == Rho & 
                         raw_results$snr == Snr & 
                         raw_results$corr_type == Corr & 
                         raw_results$beta_type == Beta)
      
      out <- lapply(indices, function(x){
        data_tmp <- raw_results$result[[x]]
        index_maxF1 <- which(data_tmp$F1 == max(data_tmp$F1, na.rm = T))
        if(length(index_maxF1)==0){
          out <- raw_results$result[[x]][1,]
          out$F1 <- 0
        }else{
          out <- data_tmp[index_maxF1,]
          
        }
        out
      })
      
      out <- do.call(rbind, out)
      
      no_enets <- as_tibble(out[is.na(out$alpha) | out$alpha==1,]) %>% 
        mutate(method = replace(method, alpha == 1, "lasso"))
      
      enets <- as_tibble(out[out$alpha>0 & out$alpha<1 & out$method=="enet",])
      enets$alpha <- round(enets$alpha,2)
      enets <- enets %>% 
        mutate(method = replace(method, alpha == 0.1, "enet_0.1")) %>% 
        mutate(method = replace(method, alpha == 0.5, "enet_0.5")) %>%
        mutate(method = replace(method, alpha == 0.9, "enet_0.9"))
      
      enets <- enets[enets$method != "enet",]
      
      
      one_setting <- bind_rows(enets, no_enets)
      
      # dismiss hybrid (Enet followeb by FSS/BSS)
      one_setting <- one_setting[one_setting$method != "enet_bs_hybrid", ]
      
      one_setting$method <- factor(one_setting$method,
                                   levels = c(
                                     "enet_0.1", 
                                     "enet_0.5", 
                                     "enet_0.9", 
                                     "lasso",
                                     "fs",
                                     "bs" ))
      
      names(one_setting)[c(1,12,13,14)] <- c("Method", "Recall", "Precision", "Best_F1")
      
      one_setting <- one_setting %>% 
        pivot_longer(c(Best_F1,Recall,Precision), names_to = 'Metric', values_to = 'Value')
      
      p <- unique(one_setting$p)
      s <- unique(one_setting$s)
      one_setting$rho <- Rho 
      one_setting$dim <- Dim
      
      one_setting
      
    })
    
    out_rho_block <- do.call(rbind, out_rho_block)
    
    # generate tibble for rho=0 (independent)
    Corr <- "independent"
    
    raw_results <- 
      readRDS(paste("./results/raw_results_",
                    Dim, "_",
                    Beta, "_",
                    Corr, ".RDS",
                    sep="")
      )
    
    # name list elements
    names(raw_results) <- c("job.id",         "problem"    ,    "algorithm"    ,  "n"           ,
                            "p"           ,   "s"           ,   "dimensionality", "corr_type" ,    
                            "rho"         ,   "beta_type"    ,  "snr"        ,    "k"      ,       
                            "alpha"      ,    "result" )
    
    
    
    
    RHO <- unique(raw_results$rho)
    
    Rho <- RHO
    
    indices <- which(raw_results$dimensionality == Dim & 
                       raw_results$rho == Rho & 
                       raw_results$snr == Snr & 
                       raw_results$corr_type == Corr & 
                       raw_results$beta_type == Beta)
    
    out_independent <- lapply(RHO, function(Rho){
      
      indices <- which(raw_results$dimensionality == Dim & 
                         raw_results$rho == Rho & 
                         raw_results$snr == Snr & 
                         raw_results$corr_type == Corr & 
                         raw_results$beta_type == Beta)
      
      out <- lapply(indices, function(x){
        data_tmp <- raw_results$result[[x]]
        index_maxF1 <- which(data_tmp$F1 == max(data_tmp$F1, na.rm = T))
        if(length(index_maxF1)==0){
          out <- raw_results$result[[x]][1,]
          out$F1 <- 0
        }else{
          out <- data_tmp[index_maxF1,]
          
        }
        out
      })
      
      out <- do.call(rbind, out)
      
      no_enets <- as_tibble(out[is.na(out$alpha) | out$alpha==1,]) %>% 
        mutate(method = replace(method, alpha == 1, "lasso"))
      
      enets <- as_tibble(out[out$alpha>0 & out$alpha<1 & out$method=="enet",])
      enets$alpha <- round(enets$alpha,2)
      enets <- enets %>% 
        mutate(method = replace(method, alpha == 0.1, "enet_0.1")) %>% 
        mutate(method = replace(method, alpha == 0.5, "enet_0.5")) %>%
        mutate(method = replace(method, alpha == 0.9, "enet_0.9"))
      
      enets <- enets[enets$method != "enet",]
      
      
      one_setting <- bind_rows(enets, no_enets)
      
      # dismiss hybrid 
      one_setting <- one_setting[one_setting$method != "enet_bs_hybrid", ]
      
      one_setting$method <- factor(one_setting$method,
                                   levels = c( 
                                     "enet_0.1",  
                                     "enet_0.5",  
                                     "enet_0.9", 
                                     "lasso",
                                     "fs",
                                     "bs" ))
      
      names(one_setting)[c(1,12,13,14)] <- c("Method", "Recall", "Precision", "Best_F1")
      
      one_setting <- one_setting %>% 
        pivot_longer(c(Best_F1,Recall,Precision), names_to = 'Metric', values_to = 'Value')
      
      p <- unique(one_setting$p)
      s <- unique(one_setting$s)
      one_setting$rho <- Rho 
      one_setting$dim <- Dim
      
      one_setting
      
    })
    
    out_independent <- do.call(rbind, out_independent)
    
    
    out_dim <- bind_rows(out_independent, out_rho_block)
  }
  
  out_dim
  
})

out <- do.call(rbind, out)

# rename levels
levels(out$Method)[levels(out$Method) == "enet_0.1"] <- 
  "Enet 0.1"
levels(out$Method)[levels(out$Method) == "enet_0.5"] <- 
  "Enet 0.5"
levels(out$Method)[levels(out$Method) == "enet_0.9"] <- 
  "Enet 0.9"
levels(out$Method)[levels(out$Method) == "lasso"] <- 
  "Lasso"
levels(out$Method)[levels(out$Method) == "fs"] <- 
  "FSS"
levels(out$Method)[levels(out$Method) == "bs"] <- 
  "BSS"

if(!all(c("high", "medium", "low") %in% out$dim)){
  cat(paste("! ! ! NOTE: Could not produce Figure 08 ! ! !\n",
            "Raw results for high- and medium-dimensional settings are needed.\n",
            "Please follow instructions in the README to download the data or generate\n",
            "all raw results by applying Chapter II in the master script.\n",
            sep=""))
  
}else{
  
  # rename settings
  out$dim[out$dim == "low"] <- "low (n = 1000, p = 100)"
  out$dim[out$dim == "medium"] <- "medium (n = 500, p = 500)"
  out$dim[out$dim == "high"] <- "high (n = 100, p = 1000)"
  
  # generate plot
  ggplot(out, aes(x = as.factor(rho), y = Value, fill=Method))+
    geom_boxplot()+
    facet_wrap( ~ factor(dim, 
                         levels = c("low (n = 1000, p = 100)", 
                                    "medium (n = 500, p = 500)", 
                                    "high (n = 100, p = 1000)")), 
                nrow=1)+
    ylim(0,1) +
    scale_fill_manual(values=c(
      "#FF99CC",
      "#FF66FF",
      "#B266FF",
      "#FF3333",
      "#0080FF",
      "#00CC00"
    )) + 
    ylab("Best possible F1-score") + 
    xlab(TeX(r'(Correlation strength $\rho$)'))
  
  # save plot
  ggsave(filename = "./plots/Figure_08.png",
         width = 2800, height = 1200, units = "px", dpi = 400)
}

