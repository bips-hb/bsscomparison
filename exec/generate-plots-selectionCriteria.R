# Script for generating the plots for the selection criteria

# für paper plot
methods <- c("Enet 0.1", "Enet 0.5", "Enet 0.9", "Lasso", "FSS", "BSS")
corr_struc <- "block"
Dim <- "high"
beta_position <- "adjacent"
rho <- 0.7

BETA <- c("adjacent", "spread")
DIM <- c("low", "high")
CORR <- c("block", "toeplitz")

# für Abgleich
if(runCriteriaSimu == TRUE){
  path2data <- "./data/" 
}else{
  path2data <- "" 
}


counter <- 57

for(Corr in CORR){
  for(Beta in BETA){
    for(Dim in DIM){
      
      
      # load results of BIC, mBIC2 and HQC
      sc_data <- 
        readRDS(
          paste("./results/Results_Selection_Criteria_",
                Corr, "_",
                Dim, "_",
                Beta,
                ".RDS",
                sep="")
        )
      
      # load results of Stability Selection
      ss_data <-
        readRDS(
          paste("./results/Results_Stability_Selection_",
                Corr, "_",
                Dim, "_",
                Beta,
                ".RDS",
                sep="")
        )
      
      # one dataset
      results <- bind_rows(
        sc_data,
        ss_data)
      
      #rename
      results$beta_position[results$beta_position == "spread"] <- "equally spaced"
      results$beta_position[results$beta_position == "adjacent"] <- "consecutive"
      
      RHO <- unique(results$rho)
      
      for(Rho in RHO){
        
        print(Rho)
        
        # Generarte pltos for F1-score, Recall and Precision for BIC, mBIC2, HQC and
        # Stability Selection and save in ./plots
        
        # F1
        pic_f1 <- ggplot(results %>% filter(rho == Rho),
               aes(x=as.factor(snr), y=F1, fill=method)) +
          geom_boxplot(outlier.size = 0.5) +
          facet_wrap(~ criterion, ncol=2) +
          scale_fill_manual(values=c(
            colorRampPalette(c("#FF99CC", "#B266FF"))(9),
            "#FF3333",
            "#0080FF",
            "#00CC00"
          )) +
          xlab("Signal-to-noise ratio") +
          ylab("F1-score") +
          ylim(0,1)
        
        counter <- counter + 1
        ggsave(plot = pic_f1, filename = paste("./plots/Appendix_Figure_", counter, ".png", sep=""),
               dpi=600, width = 21, height = 18, units = "cm")
        
        # Accuracy/Recall
        pic_accuracy <- ggplot(results %>% filter(rho == Rho),
               aes(x=as.factor(snr), y=Accuracy, fill=method)) +
          geom_boxplot(outlier.size = 0.5) +
          facet_wrap(~ criterion, ncol=2) +
          scale_fill_manual(values=c(
            colorRampPalette(c("#FF99CC", "#B266FF"))(9),
            "#FF3333",
            "#0080FF",
            "#00CC00"
          )) +
          xlab("Signal-to-noise ratio") +
          ylab("Recall") +
          ylim(0,1)
        
        counter <- counter + 1
        ggsave(plot = pic_accuracy, filename = paste("./plots/Appendix_Figure_", counter, ".png", sep=""),
               dpi=600, width = 21, height = 18, units = "cm")
        
        # Precision
        pic_precision <- ggplot(results %>% filter(rho == Rho),
               aes(x=as.factor(snr), y=Precision, fill=method)) +
          geom_boxplot(outlier.size = 0.5) +
          facet_wrap(~ criterion, ncol=2) +
          scale_fill_manual(values=c(
            colorRampPalette(c("#FF99CC", "#B266FF"))(9),
            "#FF3333",
            "#0080FF",
            "#00CC00"
          )) +
          xlab("Signal-to-noise ratio") +
          ylab("Precision") +
          ylim(0,1)
        
        counter <- counter+1
        ggsave(plot = pic_precision, filename = paste("./plots/Appendix_Figure_", counter, ".png", sep=""),
               dpi=600, width = 21, height = 18, units = "cm")
        
        
      }
    }
  }
}



# generate plot for the results if for each method its best selection criteria is
# used:

# get the best criterion of each method in terms of mean F1 for each parameter setting
criterion_mean_best_results <- 
  results %>% 
  group_by(method, snr, rho, criterion) %>%
  summarise(meanF1 = mean(F1)) %>%
  group_by(method, snr, rho) %>%
  mutate(max_meanF1 = max(meanF1)) %>% 
  filter(meanF1 == max_meanF1) %>%
  distinct(method, snr, rho, .keep_all= TRUE)

# filter the results for the best selection criteria
best_criterion_results <- 
  lapply(1:nrow(criterion_mean_best_results), function(j){
    results %>% 
      filter(
        method == criterion_mean_best_results$method[j] &
          snr == criterion_mean_best_results$snr[j] &
          rho == criterion_mean_best_results$rho[j] &
          criterion == criterion_mean_best_results$criterion[j])
  })

best_criterion_results <- do.call(rbind, best_criterion_results)  

# rename 
names(best_criterion_results)[c(1,11, 12)] <- 
  c("Method", 
    "Precision",
    "Recall")

best_criterion_results <- best_criterion_results %>% 
  pivot_longer(c("F1",
                 "Recall",
                 "Precision"), 
               names_to = 'Metric', 
               values_to = 'Value')

best_criterion_results$Metric <- 
  factor(best_criterion_results$Metric, 
         levels=c("F1",
                  "Precision",
                  "Recall"))

# set NaN to 0 (e.g. Precision becomes NaN if TP and FP are both 0)
best_criterion_results$Value[which(is.na(best_criterion_results$Value))] <- 0

# generate plot
ggplot(best_criterion_results %>%
         filter(rho == rho,
                Method %in% methods),
       aes(x = as.factor(snr), y = Value, fill = Method)) + 
  geom_boxplot() +
  facet_wrap( ~ Metric, ncol=1) +
  scale_fill_manual(values=c(
    colorRampPalette(c("#FF99CC", "#B266FF"))(sum(methods %in% paste("Enet", seq(0.1,0.9,0.1)))), #Enet colors
    "#FF3333", # Lasso color
    "#0080FF", # FSS color
    "#00CC00" # BSS color
  )) + 
  xlab("Signal-to-noise ratio") +
  ylab("Value") + 
  ylim(0,1)

# save plot
ggsave(paste("./plots/BestCriterion_", corr_struc, "_", Dim, "_", rho, ".png", sep=""),
       dpi=600, width = 21, height = 18, units = "cm")
