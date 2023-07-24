#' Script to plot results of simulation results from synthetic data
#' 
#' loadd neccessary packages
library(dplyr)
library(tibble)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(reshape2)


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


pic_out <- ggplot(melted_out_sim_TCGA %>% filter(snr %in% c(0.42, 1.22, 3.52)),
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

ggsave("./plots/Appendix_Figure_28.png",
       width = 18,
       height = 18, units = "cm", dpi = 300)

### Generate Figure 06 of the apper (show only a selection of Enet versions)
pic_out <- ggplot(melted_out_sim_TCGA %>% 
                    filter(method %in% c("Enet 0.1", "Enet 0.5", "Enet 0.9",
                                         "Lasso", "FSS", "BSS"),
                           snr %in% c(0.42, 1.22, 3.52)),
                  aes(x = as.factor(snr), y = value, fill = method)) +
  geom_boxplot(width=0.5)+
  ylim(0,1) +
  scale_fill_manual(values=c(
    colorRampPalette(c("#FF99CC", "#B266FF"))(3),
    "#FF3333",
    "#0080FF",
    "#00CC00"
  )) +
  facet_wrap(.~variable, ncol=1)+
  labs(fill="Method") +
  xlab("Signal-to-noise ratio") +
  ylab("Value")

ggsave("./plots/Figure_06.png",
       width = 18,
       height = 18, units = "cm", dpi = 300)



### Semi-Synthetic data p=100, n =594
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
  ggplot(melted_out_sim_TCGA %>% filter(snr %in% c(0.42, 1.22, 3.52)),
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
ggsave("./plots/Appendix_Figure_29.png",
       width = 18,
       height = 18, units = "cm", dpi = 300)

### Generate Figure 07 of the apper (show only a selection of Enet versions)
pic_out <- ggplot(melted_out_sim_TCGA %>% 
                    filter(method %in% c("Enet 0.1", "Enet 0.5", "Enet 0.9",
                                         "Lasso", "FSS", "BSS"),
                           snr %in% c(0.42, 1.22, 3.52)),
                  aes(x = as.factor(snr), y = value, fill = method)) +
  geom_boxplot(width=0.5)+
  ylim(0,1) +
  scale_fill_manual(values=c(
    colorRampPalette(c("#FF99CC", "#B266FF"))(3),
    "#FF3333",
    "#0080FF",
    "#00CC00"
  )) +
  facet_wrap(.~variable, ncol=1)+
  labs(fill="Method") +
  xlab("Signal-to-noise ratio") +
  ylab("Value")

ggsave("./plots/Figure_07.png",
       width = 18,
       height = 18, units = "cm", dpi = 300)



