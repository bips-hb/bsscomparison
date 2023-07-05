#' Script to plot results of simulation results from synthetic data
#' 
#' loadd neccessary packages
library(dplyr)
library(tibble)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(reshape2)



### Semi Synthetic data p=100, n =594
#' read data
out_sim_TCGA <-
  readRDS("./results/raw_results_SemiSyntheticSimulation_100_594.RDS")

#' alpha=1 is Lasso; rename
out_sim_TCGA <-
  out_sim_TCGA %>%
  mutate(method = case_when(method == "Enet 1" ~ "Lasso",
                            TRUE ~ method ))

#' set levels of all methods
out_sim_TCGA$method <-
  factor(out_sim_TCGA$method,
         levels=c(
           "Enet 0.1",
           "Enet 0.2",
           "Enet 0.3",
           "Enet 0.4",
           "Enet 0.5",
           "Enet 0.6",
           "Enet 0.7",
           "Enet 0.8",
           "Enet 0.9",
           "Lasso",
           "FSS",
           "BSS"))

#' get best possible F1 score for each method in each sim run and for each 
#' signal-to-noise value
F1max_out_sim_TCGA <-
  out_sim_TCGA %>%
  group_by(sim.n, snr, method) %>%
  mutate(maxF1 = max(F1)) %>%
  ungroup() %>%
  filter(F1 == maxF1) %>%
  group_by(method, snr, sim.n) %>%
  distinct(maxF1, .keep_all = T)

#' melt data
melted_out_sim_TCGA <-
  tibble(melt(F1max_out_sim_TCGA,
              measure = c("maxF1", "Precision", "Accuracy")))

melted_out_sim_TCGA$variable <-  as.character(melted_out_sim_TCGA$variable)

#' rename
melted_out_sim_TCGA <-
  melted_out_sim_TCGA %>%
  mutate(variable = case_when(variable == "maxF1" ~ "Best possible F1",
                              variable == "Precision" ~ "Corresponding Precision",
                              variable == "Accuracy" ~ "Corresponding Recall",
                              TRUE ~ variable ))

#' generate plot
pic_out <- 
  ggplot(melted_out_sim_TCGA %>% filter(snr %in% c(0.05, 0.25, 0.42, 1.22, 2.07, 6)),
         aes(x = as.factor(snr), y = value, fill = method)) +
  geom_boxplot(width=0.5)+
  ylim(0,1) +
  scale_fill_manual(values=c(
    colorRampPalette(c("#FF99CC", "#B266FF"))(9),
    "#FF3333",
    "#0080FF",
    "#00CC00"
  )) +
  facet_wrap(.~variable, ncol=1)+
  labs(fill="Method") +
  xlab("Signal-to-noise ratio") +
  ylab("Value")

#' save plot
ggsave("./plots/Results_low_semisyn.png",
       width = 18,
       height = 18, units = "cm", dpi = 300)


### generate plots for BSS performance for different subset sizes

#' True positives
p1 <- ggplot(out_sim_TCGA %>% filter(method %in% c("BSS")) ,
             aes(x = as.factor(k), y= TP, fill=as.factor(snr)))+
  geom_boxplot()+
  scale_fill_manual(
    values=c(
      "0.42" = "#FEE08B",
      "1.22" = "#A6D96A",
      "3.52" = "#1A9850")
  ) +
  labs(fill='Signal-to-noise ratio τ') +
  xlab("Subset size k")+
  ylab("Number of true positives") +
  geom_hline(yintercept = 10, alpha = 0)+
  scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (10+1 ) * 1.1)))))

#' F1 score
p2 <- ggplot(out_sim_TCGA %>% filter(method %in% c("BSS")) ,
             aes(x = as.factor(k), y= F1, fill=as.factor(snr)))+
  geom_boxplot()+
  scale_fill_manual(
    values=c(
      "0.42" = "#FEE08B",
      "1.22" = "#A6D96A",
      "3.52" = "#1A9850")
  ) +
  labs(fill='Signal-to-noise ratio τ') +
  xlab("Subset size k")+
  ylab("F1")+
  ylim(0,1)

#' Precision
p3 <- ggplot(out_sim_TCGA %>% filter(method %in% c("BSS")) ,
             aes(x = as.factor(k), y= Precision, fill=as.factor(snr)))+
  geom_boxplot()+
  scale_fill_manual(
    values=c(
      "0.42" = "#FEE08B",
      "1.22" = "#A6D96A",
      "3.52" = "#1A9850")
  ) +
  labs(fill='Signal-to-noise ratio τ') +
  xlab("Subset size k")+
  ylab("Precision")+
  ylim(0,1)

#' Accuracy (recall)
p4 <- ggplot(out_sim_TCGA %>% filter(method %in% c("BSS")) ,
             aes(x = as.factor(k), y= Accuracy, fill=as.factor(snr)))+
  geom_boxplot()+
  scale_fill_manual(
    values=c(
      "0.42" = "#FEE08B",
      "1.22" = "#A6D96A",
      "3.52" = "#1A9850")
  ) +
  labs(fill='Signal-to-noise ratio τ') +
  xlab("Subset size k")+
  ylab("Recall")+
  ylim(0,1)

#' combine to one plot
multiple_pic_out <- ggarrange(p1, p2, p3, p4, ncol=2, nrow = 2,
          common.legend = TRUE, legend="bottom")


#' save the plot
ggsave("./plots/BSS_vs_k_low_semisyn.png", 
       width = 36, 
       height = 18, units = "cm", dpi = 300)






### Semi Synthetic data p=1000, n =100

out_sim_TCGA <-
  readRDS("./results/raw_results_SemiSyntheticSimulation_1000_100.RDS")

out_sim_TCGA <-
  out_sim_TCGA %>%
  mutate(method = case_when(method == "Enet 1" ~ "Lasso",
                            TRUE ~ method ))

out_sim_TCGA$method <-
  factor(out_sim_TCGA$method,
         levels=c(
           "Enet 0.1",
           "Enet 0.2",
           "Enet 0.3",
           "Enet 0.4",
           "Enet 0.5",
           "Enet 0.6",
           "Enet 0.7",
           "Enet 0.8",
           "Enet 0.9",
           "Lasso",
           "FSS",
           "BSS"))

F1max_out_sim_TCGA <-
  out_sim_TCGA %>%
  group_by(sim.n, snr, method) %>%
  mutate(maxF1 = max(F1)) %>%
  ungroup() %>%
  filter(F1 == maxF1) %>%
  group_by(method, snr, sim.n) %>%
  distinct(maxF1, .keep_all = T)


melted_out_sim_TCGA <-
  tibble(melt(F1max_out_sim_TCGA,
              measure = c("maxF1", "Precision", "Accuracy")))

melted_out_sim_TCGA$variable <-  as.character(melted_out_sim_TCGA$variable)

melted_out_sim_TCGA <-  melted_out_sim_TCGA %>%
  mutate(variable = case_when(variable == "maxF1" ~ "Best possible F1",
                              variable == "Precision" ~ "Corresponding Precision",
                              variable == "Accuracy" ~ "Corresponding Recall",
                              TRUE ~ variable ))


pic_out <- ggplot(melted_out_sim_TCGA %>% filter(snr %in% c(0.05, 0.25, 0.42, 1.22, 2.07, 6)),
                  aes(x = as.factor(snr), y = value, fill = method)) +
  geom_boxplot(width=0.5)+
  ylim(0,1) +
  scale_fill_manual(values=c(
    colorRampPalette(c("#FF99CC", "#B266FF"))(9),
    "#FF3333",
    "#0080FF",
    "#00CC00"
  )) +
  facet_wrap(.~variable, ncol=1)+
  labs(fill="Method") +
  xlab("Signal-to-noise ratio") +
  ylab("Value")

ggsave("./plots/Results_high_semisyn.png",
       width = 18,
       height = 18, units = "cm", dpi = 300)

p1 <- ggplot(out_sim_TCGA %>% filter(method %in% c("BSS")) ,
             aes(x = as.factor(k), y= TP, fill=as.factor(snr)))+
  geom_boxplot()+
  scale_fill_manual(
    values=c(
      "0.42" = "#FEE08B",
      "1.22" = "#A6D96A",
      "3.52" = "#1A9850")
  ) +
  labs(fill='Signal-to-noise ratio τ') +
  xlab("Subset size k")+
  ylab("Number of true positives") +
  geom_hline(yintercept = 10, alpha = 0)+
  scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (10+1 ) * 1.1)))))

p2 <- ggplot(out_sim_TCGA %>% filter(method %in% c("BSS")) ,
             aes(x = as.factor(k), y= F1, fill=as.factor(snr)))+
  geom_boxplot()+
  scale_fill_manual(
    values=c(
      "0.42" = "#FEE08B",
      "1.22" = "#A6D96A",
      "3.52" = "#1A9850")
  ) +
  labs(fill='Signal-to-noise ratio τ') +
  xlab("Subset size k")+
  ylab("F1")+
  ylim(0,1)

p3 <- ggplot(out_sim_TCGA %>% filter(method %in% c("BSS")) ,
             aes(x = as.factor(k), y= Precision, fill=as.factor(snr)))+
  geom_boxplot()+
  scale_fill_manual(
    values=c(
      "0.42" = "#FEE08B",
      "1.22" = "#A6D96A",
      "3.52" = "#1A9850")
  ) +
  labs(fill='Signal-to-noise ratio τ') +
  xlab("Subset size k")+
  ylab("Precision")+
  ylim(0,1)

p4 <- ggplot(out_sim_TCGA %>% filter(method %in% c("BSS")) ,
             aes(x = as.factor(k), y= Accuracy, fill=as.factor(snr)))+
  geom_boxplot()+
  scale_fill_manual(
    values=c(
      "0.42" = "#FEE08B",
      "1.22" = "#A6D96A",
      "3.52" = "#1A9850")
  ) +
  labs(fill='Signal-to-noise ratio τ') +
  xlab("Subset size k")+
  ylab("Recall")+
  ylim(0,1)


multiple_pic_out <- ggarrange(p1, p2, p3, p4, ncol=2, nrow = 2,
                              common.legend = TRUE, legend="bottom")



ggsave("./plots/BSS_vs_k_high_semisyn.png", 
       width = 36, 
       height = 18, units = "cm", dpi = 300)



