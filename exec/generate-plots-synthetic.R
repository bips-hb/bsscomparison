#' Script to plot results of simulation results from synthetic data
#' 
#' Inout:
#' @param CORR vector of correlation structures
#' @param BETA vector of non-zero positions
#' @param DIM dimensionality of problem
#' @param SNR vecotr of signal-to-noise values
#' 
#' Output:
#' Figures 2,3,4 and 5 of the paper and figures 1-27 of the appendix
#' (if only low dimensional data is available, only figures 3 & 4 of the
#' paper and figures 19-27 of the appendix are generated)
#' 
#' 
#' load neccessary packages
library(dplyr)
library(tibble)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(reshape2)


#### Plots for the Appendix ####
# This script uses 


# counter is needed for labaling the figures according to the appendix.
# If only low dimensional data is available, set counter to 18

if(all(c("high", "medium") %in% DIM)){
  counter <- 0
}else{
  counter <- 18
  cat("There is no high/medium-dimensional setting data available.\nWill only plot results for low-dimensional settings.\n")
}


### Generate figures for the Appendix
# loop over dimensionality, correlation structure, position of the non-zero betas
# and correlation strength
for(Dim in DIM){
  
  for(Corr in CORR){
    
    for(Beta in BETA){
      if(Beta == "first"){
        Beta_title <- "adjacent"
      }else{
        Beta_title <- "equally distributed"
      }
      
      # we only need one beta positioning if there is no correlation between
      #. any predictor
      if(Corr == "independent" & Beta == "spread"){
        next
      }
      
      raw_results <- NULL
      
      # load the raws results 
      raw_results <- 
        readRDS(paste("./results/raw_results_",
                      Dim, "_",
                      Beta, "_",
                      Corr, ".RDS",
                      sep="")
        )
      
      # rename list elements of the raw-resuslt
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
            
            # calculate F1
            data_tmp$F1 <- 
              2*data_tmp$TP/(2*data_tmp$TP + data_tmp$FP + data_tmp$FN)
            # Find max F1
            index_maxF1 <- 
              which(data_tmp$F1 == max(data_tmp$F1, na.rm = T))
            
            if(max(data_tmp$F1, na.rm = T)==0){
              # if all F1 are zero we use the smallest subset (k=1)
              out <- data_tmp[1,]
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
            # uncomment if more Enet versions are desired (please uncomment
            # also the the levels etc below)
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
          
          
          one_setting
        })
        out_snr <- do.call(bind_rows, out_snr)
        
        # please add more colors in "scale_fill_manual" if you have more than
        # six methods 
        pic_appendix <- 
          ggplot(out_snr , 
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
        
        # raise counter for naming the pltos
        counter <- counter +1
        
        # save plot
        ggsave(plot = pic_appendix, filename = paste("./plots/Appendix_Figure_",
                     counter,
                     ".png", sep=""), 
               width = 18, 
               height = 18, units = "cm", dpi = 300)
        
        
        ### Additionaly, save figures if the setting corresponds to of the four 
          # settings shown the paper (figures 2, 3, 4 and 5)
        
        if(
           (Dim == "high" & Beta == "spread" & Corr == "block" & Rho == 0.35) |
           (Dim == "high" & Beta == "first" & Corr == "toeplitz" & Rho == 0.7) |
           (Dim == "low" & Beta == "spread" & Corr == "block" & Rho == 0.7) |
           (Dim == "low" & Beta == "first" & Corr == "block" & Rho == 0.7)){
          
          pic_paper <- 
            ggplot(out_snr %>% filter(snr %in% c(0.05, 0.25, 0.42, 1.22, 2.07, 6) ), 
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
          
        }
        
        # save figure based on setting of the paper
        if(Dim == "high" & Beta == "spread" & Corr == "block" & Rho == 0.35){
          ggsave(plot = pic_paper, filename =  "./plots/Figure_02.png",width = 18, 
                 height = 18, units = "cm", dpi = 300)
        }
        if(Dim == "high" & Beta == "first" & Corr == "toeplitz" & Rho == 0.7){
          ggsave(plot = pic_paper, filename = "./plots/Figure_03.png",width = 18, 
                 height = 18, units = "cm", dpi = 300)
        }
        if(Dim == "low" & Beta == "spread" & Corr == "block" & Rho == 0.7){
          ggsave(plot = pic_paper, filename = "./plots/Figure_04.png",width = 18, 
                 height = 18, units = "cm", dpi = 300)
        }
        if(Dim == "low" & Beta == "first" & Corr == "block" & Rho == 0.7){
          ggsave(plot = pic_paper, filename = "./plots/Figure_05.png",width = 18, 
                 height = 18, units = "cm", dpi = 300)
        }
        
        
        
      }
      
      
    }
    
  }
  
}

