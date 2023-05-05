# Script for generating the plots for the selection criteria

library(tidyverse)

corr_struc <- "toeplitz"
Dim <- "high"
beta_position <- "spread"
rho <- 0.7

sc_data <- 
  readRDS(
    paste("./data/Results_Selection_Criteria_",
          corr_struc, "_",
          Dim, "_",
          beta_position,
          ".RDS",
          sep="")
  )

ss_data <-
  readRDS(
    paste("./data/Results_Stability_Selection_",
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
