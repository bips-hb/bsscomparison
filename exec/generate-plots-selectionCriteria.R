# Script for generating the plots for the selection criteria

library(tidyverse)

if(runCriteriaSimu == TRUE){
  path2data <- "./data/" 
}else{
  path2data <- "./results/" 
}

sc_data <- 
  readRDS(
    paste(path2data, "Results_Selection_Criteria_",
          corr_struc, "_",
          Dim, "_",
          beta_position,
          ".RDS",
          sep="")
  )

ss_data <-
  readRDS(
    paste(path2data, "Results_Stability_Selection_",
          corr_struc, "_",
          Dim, "_",
          beta_position,
          ".RDS",
          sep="")
  )

results <- bind_rows(
  sc_data,
  ss_data)

results$beta_position[results$beta_position == "spread"] <- "equally spaced"
results$beta_position[results$beta_position == "adjacent"] <- "consecutive"

# Generarte pltos for F1-score, Recall and Precision for BIC, mBIC2, HQC and
# Stability Selection and save in ./plots
ggplot(results %>% filter(rho == rho),
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

ggsave(paste("./plots/F1_multipleCriteria_", corr_struc, "_", Dim, "_", rho, ".png", sep=""),
       dpi=600, width = 21, height = 18, units = "cm")

ggplot(results %>% filter(rho == rho),
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

ggsave(paste("./plots/Accuracy_multipleCriteria_", corr_struc, "_", Dim, "_", rho, ".png", sep=""),
       dpi=600, width = 21, height = 18, units = "cm")


ggplot(results %>% filter(rho == rho),
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

ggsave(paste("./plots/Precision_multipleCriteria_", corr_struc, "_", Dim, "_", rho, ".png", sep=""),
       dpi=600, width = 21, height = 18, units = "cm")
