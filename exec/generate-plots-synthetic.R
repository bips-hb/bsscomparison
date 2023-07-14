#' Script to plot results of simulation results from synthetic data
#' 
#' @param CORR vector of correlation structures
#' @param BETA vector of non-zero positions
#' @param DIM dimensionality of problem
#' @param SNR vecotr of signal-to-noise values
#' 
#' load neccessary packages
library(dplyr)
library(tibble)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(reshape2)


#### Plots for the Appendix ####
DIM <- c("high", "medium", "low")
BETA <- c("spread", "first")
CORR <- c("block", "toeplitz", "independent")



for(Dim in DIM){
  
  for(Beta in BETA){
    if(Beta == "first"){
      Beta_title <- "adjacent"
    }else{
      Beta_title <- "equally distributed"
    }
    
    for(Corr in CORR){
      
      
      if(Corr == "independent"){
        Beta <- "first"
      }
      
      raw_results <- 
        readRDS(paste("./results/raw_results_",
                      Dim, "_",
                      Beta, "_",
                      Corr, ".RDS",
                      sep="")
        )
      
      names(raw_results) <- c("job.id",         "problem"    ,    "algorithm"    ,  "n"           ,
                              "p"           ,   "s"           ,   "dimensionality", "corr_type" ,    
                              "rho"         ,   "beta_type"    ,  "snr"        ,    "k"      ,       
                              "alpha"      ,    "result" )
      
      
      
      
      
      SNR <- unique(raw_results$snr)
      
      RHO <- unique(raw_results$rho)
      
      
      for(Rho in RHO){
        
        out_snr <- lapply(SNR, function(Snr){
          
          
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
              out$snr <- Snr
            }else{
              out <- data_tmp[index_maxF1,]
              out$snr <- Snr
            }
            out
          })
          
          out <- do.call(rbind, out)
          
          no_enets <- as_tibble(out[is.na(out$alpha) | out$alpha==1,]) %>% 
            mutate(method = replace(method, alpha == 1, "lasso"))
          
          enets <- as_tibble(out[out$alpha>0 & out$alpha<1 & out$method=="enet",])
          enets$alpha <- round(enets$alpha,2)
          enets <- enets %>% 
            # uncomment if more Enet versions are desired
            mutate(method = replace(method, alpha == 0.1, "enet_0.1")) %>% 
            # mutate(method = replace(method, alpha == 0.2, "enet_0.2")) %>%
            # mutate(method = replace(method, alpha == 0.3, "enet_0.3")) %>%
            # mutate(method = replace(method, alpha == 0.4, "enet_0.4")) %>%
            mutate(method = replace(method, alpha == 0.5, "enet_0.5")) %>%
            # mutate(method = replace(method, alpha == 0.6, "enet_0.6")) %>%
            # mutate(method = replace(method, alpha == 0.7, "enet_0.7")) %>%
            # mutate(method = replace(method, alpha == 0.8, "enet_0.8")) %>%
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
                                           # "enet_0.2", 
                                           # "enet_0.3", 
                                           # "enet_0.4", 
                                           "enet_0.5", 
                                           # "enet_0.6", 
                                           # "enet_0.7", 
                                           # "enet_0.8", 
                                         "enet_0.9", 
                                         "lasso",
                                         "fs",
                                         "bs" ))
          levels(one_setting$method)[levels(one_setting$method) == "enet_0.1"] <- 
            "Enet 0.1"
          # levels(one_setting$method)[levels(one_setting$method) == "enet_0.2"] <- 
          #   "Enet 0.2"
          # levels(one_setting$method)[levels(one_setting$method) == "enet_0.3"] <- 
          #   "Enet 0.3"
          # levels(one_setting$method)[levels(one_setting$method) == "enet_0.4"] <- 
          #   "Enet 0.4"
          levels(one_setting$method)[levels(one_setting$method) == "enet_0.5"] <- 
            "Enet 0.5"
          # levels(one_setting$method)[levels(one_setting$method) == "enet_0.6"] <- 
          #   "Enet 0.6"
          # levels(one_setting$method)[levels(one_setting$method) == "enet_0.7"] <- 
          #   "Enet 0.7"
          # levels(one_setting$method)[levels(one_setting$method) == "enet_0.8"] <- 
          #   "Enet 0.8"
          levels(one_setting$method)[levels(one_setting$method) == "enet_0.9"] <- 
            "Enet 0.9"
          levels(one_setting$method)[levels(one_setting$method) == "lasso"] <- 
            "Lasso"
          levels(one_setting$method)[levels(one_setting$method) == "fs"] <- 
            "FSS"
          levels(one_setting$method)[levels(one_setting$method) == "bs"] <- 
            "BSS"
          
          names(one_setting)[c(1,12,13,14)] <- c("Method", 
                                                 "Corresponding Recall", 
                                                 "Corresponding Precision", 
                                                 "Best possible F1")
          
          one_setting <- one_setting %>% 
            pivot_longer(c("Best possible F1",
                           "Corresponding Recall",
                           "Corresponding Precision"), 
                         names_to = 'Metric', 
                         values_to = 'Value')
          
          p <- unique(one_setting$p)
          s <- unique(one_setting$s)
          
          one_setting
        })
        out_snr <- do.call(bind_rows, out_snr)
        
        p <- unique(out_snr$p)
        s <- unique(out_snr$s)
        
        pic <- ggplot(out_snr , 
                      aes(x = as.factor(snr), y = Value, fill = Method))+
          geom_boxplot()+
          facet_wrap( ~ Metric, ncol = 1)+
          ylim(0,1)+
          scale_fill_manual(values=c(
            "#FF99CC",
            "#FF66FF",
            "#B266FF",
            "#FF3333",
            "#0080FF",
            "#00CC00"
          )) + 
          xlab("Signal-to-noise ratrio τ")+
          theme(plot.title = element_text(size=9))
  
        
        ggsave(paste("./plots/Appendix_Figures",
                     Dim, "_",
                     Beta, "_",
                     Corr, "_",
                     100*Rho,
                     ".png", sep=""), 
               width = 18, 
               height = 18, units = "cm", dpi = 300)
        
        
        
        
      }
      
      
    }
    
  }
  
}

# Rename the figures of the Appendix:

fig_counter <- 0

lapply(DIM, function(Dim){
  lapply(BETA, function(Beta){
    lapply(CORR, function(Corr){
      lapply()
      
      
      
      
    })
    
  })
  
})





lapply(DIM, function(Dim){
  
  lapply(BETA, function(Beta){
    if(Beta == "first"){
      Beta_title <- "adjacent"
    }else{
      Beta_title <- "equally distributed"
    }
    
    lapply(CORR, function(Corr){
      
      
      if(Corr == "independent"){
        Beta <- "first"
      }
      
      raw_results <- 
        readRDS(paste("./results/raw_results_",
                      Dim, "_",
                      Beta, "_",
                      Corr, ".RDS",
                      sep="")
        )
      
      names(raw_results) <- c("job.id",         "problem"    ,    "algorithm"    ,  "n"           ,
                              "p"           ,   "s"           ,   "dimensionality", "corr_type" ,    
                              "rho"         ,   "beta_type"    ,  "snr"        ,    "k"      ,       
                              "alpha"      ,    "result" )
      
      
      
      
      RHO <- unique(raw_results$rho)
      
      
      lapply(RHO, function(Rho){
        


        
        
        raw_results <- 
          readRDS(paste("./results/raw_results_",
                        Dim, "_",
                        Beta, "_",
                        Corr, ".RDS",
                        sep="")
          )
        
        names(raw_results) <- c("job.id",         "problem"    ,    "algorithm"    ,  "n"           ,
                                "p"           ,   "s"           ,   "dimensionality", "corr_type" ,    
                                "rho"         ,   "beta_type"    ,  "snr"        ,    "k"      ,       
                                "alpha"      ,    "result" )
        
        bs_per_k <- 
          lapply(which(raw_results$algorithm == "bs"),
                 function(x){
                   bind_cols(
                     raw_results$result[[x]],
                     "dimensionality" = raw_results$dimensionality[x],
                     "corr_type" = raw_results$corr_type[x],
                     "rho" = raw_results$rho[x],
                     "snr" = raw_results$snr[x]
                   )[1:max(raw_results$k, na.rm = TRUE),]
                 })
        
        bs_per_k <- do.call(rbind, bs_per_k)
        
        p1 <- ggplot(bs_per_k %>% filter(rho %in% Rho & snr %in% SNR_BSS_k) ,
                     aes(x = as.factor(k), y= TP, fill=as.factor(snr)))+
          geom_boxplot()+
          scale_fill_manual(
            values=c(
              "0.05" = "#A50026",
              "0.09" = "#D73027",
              "0.14" = "#F46D43",
              "0.25" = "#FDAE61",
              "0.42" = "#FEE08B",
              "0.71" = "#D9EF8B",
              "1.22" = "#A6D96A",
              "2.07" = "#66BD63",
              "3.52" = "#1A9850",
              "6.00" = "#006837")
          ) +
          labs(fill='Signal-to-noise ratio τ') +
          xlab("Subset size k")+
          ylab("Number of true positives") +
          geom_hline(yintercept = 10, alpha = 0)+
          scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (10+1 ) * 1.1)))))
        
        p2 <- ggplot(bs_per_k %>% filter(rho %in% Rho & snr %in% SNR_BSS_k) ,
                     aes(x = as.factor(k), y= F1, fill=as.factor(snr)))+
          geom_boxplot()+
          scale_fill_manual(
            values=c(
              "0.05" = "#A50026",
              "0.09" = "#D73027",
              "0.14" = "#F46D43",
              "0.25" = "#FDAE61",
              "0.42" = "#FEE08B",
              "0.71" = "#D9EF8B",
              "1.22" = "#A6D96A",
              "2.07" = "#66BD63",
              "3.52" = "#1A9850",
              "6.00" = "#006837")
          ) +
          labs(fill='Signal-to-noise ratio τ') +
          xlab("Subset size k")+
          ylim(0,1)
        
        p3 <- ggplot(bs_per_k %>% filter(rho %in% Rho & snr %in% SNR_BSS_k) ,
                     aes(x = as.factor(k), y= precision, fill=as.factor(snr)))+
          geom_boxplot()+
          scale_fill_manual(
            values=c(
              "0.05" = "#A50026",
              "0.09" = "#D73027",
              "0.14" = "#F46D43",
              "0.25" = "#FDAE61",
              "0.42" = "#FEE08B",
              "0.71" = "#D9EF8B",
              "1.22" = "#A6D96A",
              "2.07" = "#66BD63",
              "3.52" = "#1A9850",
              "6.00" = "#006837")
          ) +
          labs(fill='Signal-to-noise ratio τ') +
          xlab("Subset size k")+
          ylab("Precision")+
          ylim(0,1)
        
        p4 <- ggplot(bs_per_k %>% filter(rho %in% Rho & snr %in% SNR_BSS_k)  ,
                     aes(x = as.factor(k), y= recall, fill=as.factor(snr)))+
          geom_boxplot()+
          scale_fill_manual(
            values=c(
              "0.05" = "#A50026",
              "0.09" = "#D73027",
              "0.14" = "#F46D43",
              "0.25" = "#FDAE61",
              "0.42" = "#FEE08B",
              "0.71" = "#D9EF8B",
              "1.22" = "#A6D96A",
              "2.07" = "#66BD63",
              "3.52" = "#1A9850",
              "6.00" = "#006837")
          ) +
          labs(fill='Signal-to-noise ratio τ') +
          xlab("Subset size k")+
          ylim(0,1)+
          ylab("Recall")
        
        
        ggarrange(p1, p2, p3, p4, ncol=2, nrow = 2,
                  common.legend = TRUE, legend="bottom")
        
        
        ggsave(paste("./plots/Results_BSS_vs_k_",
                     Dim, "_",
                     Corr, "_",
                     Beta, "_",
                     100*Rho,
                     #Snr, "_",
                     ".png", sep=""), 
               width = 36, 
               height = 18, units = "cm", dpi = 300)
        
        
        
        
      })
      
      
    })
    
  })
  
})

