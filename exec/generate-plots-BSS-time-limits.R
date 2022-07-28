#' Plotting results of Best Subset Selection based
#' on MIO Formulation by Bertsimas et al. (2016) and the Gurbi Solver
#' with respect to different time limits and certified solutions
#' 

# Load necessary packages 
library(ggpubr)
library(tidyverse)



################################################################################
# Certified and non-certified solutions for a block setting with consecutive   #
# non-zero direct predictors, p=100 variables, n = 1000 observations,          #
# signal-to-noise ratio (snr) of 0.42 & correlation rho = 0.7 between variables#
################################################################################

# load data
time_data <- readRDS("./results/time_comparison_block_100_1000.RDS")

# The Gurobi solver uses a lower and upperbound criterion to find a solution, 
# where the convergence rate of the lower bound criterion is much faster (see 
# Hastie et al., 2020, Best Subset, Forward Stepwise or Lasso? Analysis and 
# Recommendations Based on Extensive Comparisons. Statistical Science, 
# 35(4):579 – 592)). "OPTIMAL" means that the soultion has been certified to be 
# optimal by the lower and upper bound, while "'TIME_LIMIT" means that the 
# solver could not certify the solution for the given time limit.
# Change "OPTIMAL" to "certified" and "TIME_LIMIT"
time_data <-  time_data %>%
  mutate(status = case_when(status == "OPTIMAL" ~ "certified",
                            status == "TIME_LIMIT" ~ "non-certified",
                            TRUE ~ status ))

# create a plot which shows boxplots based on the best possible F1 scores in 
# in each simualtion run for different time limits (in seconds)
g1 <- ggplot(time_data %>% 
               group_by(sim.n, time) %>%
               summarise(maxF1 = max(F1)),
             aes(x = as.factor(time), y = maxF1)) + 
  geom_boxplot( fill="grey")+
  labs(tags = "A") +
  ylim(0,1) +
  xlab("Time limit per subset size (in seconds)") +
  ylab("Best possible F1") 


# function for plotting the number of runs which have been 
# certified/non-certified
give.n <- function(x){
  return(c(y = 0.1, label = length(x)))
  # experiment with y to find the perfect position
}

# plot best bossible F1 score for different time limits and certification
g2 <- ggplot(time_data %>% 
               group_by(sim.n, time) %>%
               mutate(maxF1 = max(F1)) %>%
               ungroup() %>%
               filter(F1 == maxF1) %>%
               group_by(time, sim.n) %>%
               distinct(maxF1, .keep_all = T)
             ,
             aes(x= as.factor(time), y = maxF1, fill=as.factor(status))) + 
  geom_boxplot()+
  labs(fill='BSS results', tags = "B") +
  #theme(legend.position="bottom") +
  stat_summary(fun.data = give.n, geom = "text", fun = median,
               position = position_dodge(width = 0.75), size = 3) +
  scale_fill_manual(values=c("certified" = "green3", 
                             "non-certified" = "red3")) +
  ylim(0,1) +
  xlab("Time limit per subset size (in seconds)") +
  ylab("Best possible F1") 

# plot boxplot of F1 scores for each subset size k für 180sec and 3600sec
g3 <- ggplot(time_data %>% filter(time %in% c(180,3600)),
             aes(x = as.factor(k), y= F1, fill=as.factor(status)))+
  geom_boxplot()+ 
  scale_fill_manual(values=c("certified" = "green3", 
                             "non-certified" = "red3")) +
  facet_wrap(.~time, labeller = labeller(time = c(
    "10" = "10 sec time limit",
    "60" = "60 sec time limit",
    "180" = "180 sec time limit",
    "600" = "600 sec time limit",
    "3600" = "3600 sec time limit"
  )))+
  stat_summary(fun.data = give.n, geom = "text", fun = median,
               position = position_dodge(width = 0.75), size =1.5) +
  labs(fill='BSS results', tags = "C") +
  #theme(legend.position="bottom") +
  xlab("Subset size k")+
  ylab("F1") +
  ylim(0,1)

# Calculate the theoretical best possible F1 score for a given subset size k and
# a true subset size s
# In our Simulation k=1,2,...,15  while s=10 is the number of true non-zero 
# direct predictors. Hence, for k=10 we have F1 score =1 and for smaller/bigger 
# k the F1-scores decrease
best_pss_F1_per_k <- sapply(1:15, function(x){
  if(x>10){
    TP <-10
    FP <- x-10
    FN <- 0
  }else{
    TP <- x
    FP <- 0
    FN <- 10-x
  }
  
  TP/(TP+0.5*(FP+FN))
})

# Add the theoretical best possible F1 scores as a orange line
for(i in 1:15){
  g3 <- g3 + 
    geom_segment(y=best_pss_F1_per_k[i], 
                 yend=best_pss_F1_per_k[i], 
                 x= i -0.4,
                 xend=i +0.4, color="orange")
}


# change the 
give.n <- function(x){
  return(c(y = 1, label = length(x)))
  # experiment with the multiplier to find the perfect position
}

# plot the corresponding number of true positives (TP) for each subse size k
g4 <- ggplot(time_data %>% filter(time %in% c(180,3600)),
             aes(x = as.factor(k), y= TP, fill=as.factor(status)))+
  geom_boxplot()+ 
  scale_fill_manual(values=c("certified" = "green3", 
                             "non-certified" = "red3")) +
  facet_wrap(.~time, labeller = labeller(time = c(
    "10" = "10 sec time limit",
    "60" = "60 sec time limit",
    "180" = "180 sec time limit",
    "600" = "600 sec time limit",
    "3600" = "3600 sec time limit"
  )))+
  labs(fill='BSS results', tags = "D") +
  #theme(legend.position="bottom") +
  xlab("Subset size k")+
  ylab("Selected true positives") +
  stat_summary(fun.data = give.n, geom = "text", fun = median,
               position = position_dodge(width = 0.75), size =1.5) +
  geom_hline(yintercept = 10, alpha = 0)+
  geom_hline(yintercept = 0, alpha = 0)+
  scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (10+1 ) * 1.1)))))

# calculate the possible best TP for each subset size and add it as an orange
# line to plot g4. For k>= 10 it is 1
for(i in 1:15){
  if(i>10){
    TP <- 10
  }else{
    TP <- i
  }
  g4 <- g4 + 
    geom_segment(y=TP, 
                 yend=TP, 
                 x= i -0.4,
                 xend=i +0.4, color="orange")
  rm(TP)
}

# arrange the 4 plots
ggarrange(ggarrange(g1, g2, ncol=2, legend = FALSE), g3, g4, ncol=1,
          common.legend = TRUE, 
          legend="bottom")

# save the plots
ggsave("./plots/Time_comparison_blocklowfirst70_snr042.png",
       width = 18, 
       height = 24, units = "cm", dpi = 300)




################################################################################
# Certified and non-certified solutions for semi-synthetic data with the 10    #
# highest correlated variables with each other being the non-zero direct       #
# predictors. p=100 variables, n = 594 observations and signal-to-noise ratio  #
# (snr) of 0.42                                                                #
################################################################################

time_data <- readRDS("./results/time_comparison_semisyn_100_594.RDS")

# The Gurobi solver uses a lower and upperbound criterion to find a solution, 
# where the convergence rate of the lower bound criterion is much faster (see 
# Hastie et al., 2020, Best Subset, Forward Stepwise or Lasso? Analysis and 
# Recommendations Based on Extensive Comparisons. Statistical Science, 
# 35(4):579 – 592)). "OPTIMAL" means that the soultion has been certified to be 
# optimal by the lower and upper bound, while "'TIME_LIMIT" means that the 
# solver could not certify the solution for the given time limit.
# Change "OPTIMAL" to "certified" and "TIME_LIMIT"
time_data <-  time_data %>%
  mutate(status = case_when(status == "OPTIMAL" ~ "certified",
                            status == "TIME_LIMIT" ~ "non-certified",
                            TRUE ~ status ))

# create a plot which shows boxplots based on the best possible F1 scores in 
# in each simualtion run for different time limits (in seconds)
g1 <- ggplot(time_data %>% 
               group_by(sim.n, time) %>%
               summarise(maxF1 = max(F1)),
             aes(x = as.factor(time), y = maxF1)) + 
  geom_boxplot( fill="grey")+
  labs(tags = "A") +
  ylim(0,1) +
  xlab("Time limit per subset size (in seconds)") +
  ylab("Best possible F1") 


give.n <- function(x){
  return(c(y = 0.1, label = length(x)))
  # experiment with y to find the perfect position
}

# plot best bossible F1 score for different time limits and certification
g2 <- ggplot(time_data %>% 
               group_by(sim.n, time) %>%
               mutate(maxF1 = max(F1)) %>%
               ungroup() %>%
               filter(F1 == maxF1) %>%
               group_by(time, sim.n) %>%
               distinct(maxF1, .keep_all = T)
             ,
             aes(x= as.factor(time), y = maxF1, fill=as.factor(status))) + 
  geom_boxplot()+
  labs(fill='BSS results', tags = "B") +
  #theme(legend.position="bottom") +
  stat_summary(fun.data = give.n, geom = "text", fun = median,
               position = position_dodge(width = 0.75)) +
  scale_fill_manual(values=c("certified" = "green3", 
                             "non-certified" = "red3")) +
  ylim(0,1) +
  xlab("Time limit per subset size (in seconds)") +
  ylab("Best possible F1") 


# plot boxplot of F1 scores for each subset size k für 180sec and 3600sec
g3 <- ggplot(time_data %>% filter(time %in% c(180,3600)),
             aes(x = as.factor(k), y= F1, fill=as.factor(status)))+
  geom_boxplot()+ 
  scale_fill_manual(values=c("certified" = "green3", 
                             "non-certified" = "red3")) +
  facet_wrap(.~time, labeller = labeller(time = c(
    "10" = "10 sec time limit",
    "60" = "60 sec time limit",
    "180" = "180 sec time limit",
    "600" = "600 sec time limit",
    "3600" = "3600 sec time limit"
  )))+
  stat_summary(fun.data = give.n, geom = "text", fun = median,
               position = position_dodge(width = 0.75), size =1.5) +
  labs(fill='BSS results', tags = "C") +
  #theme(legend.position="bottom") +
  xlab("Subset size k")+
  ylab("F1") +
  ylim(0,1)

# Calculate the theoretical best possible F1 score for a given subset size k and
# a true subset size s
# In our Simulation k=1,2,...,15  while s=10 is the number of true non-zero 
# direct predictors. Hence, for k=10 we have F1 score =1 and for smaller/bigger 
# k the F1-scores decrease
best_pss_F1_per_k <- sapply(1:15, function(x){
  if(x>10){
    TP <-10
    FP <- x-10
    FN <- 0
  }else{
    TP <- x
    FP <- 0
    FN <- 10-x
  }
  
  TP/(TP+0.5*(FP+FN))
})

# Add the theoretical best possible F1 scores as a orange line
for(i in 1:15){
  g3 <- g3 + 
    geom_segment(y=best_pss_F1_per_k[i], 
                 yend=best_pss_F1_per_k[i], 
                 x= i -0.4,
                 xend=i +0.4, color="orange")
}



give.n <- function(x){
  return(c(y = 1, label = length(x)))
  # experiment with y to find the perfect position
}

# plot the corresponding number of true positives (TP) for each subse size k
g4 <- ggplot(time_data %>% filter(time %in% c(180,3600)),
             aes(x = as.factor(k), y= TP, fill=as.factor(status)))+
  geom_boxplot()+ 
  scale_fill_manual(values=c("certified" = "green3", 
                             "non-certified" = "red3")) +
  facet_wrap(.~time, labeller = labeller(time = c(
    "10" = "10 sec time limit",
    "60" = "60 sec time limit",
    "180" = "180 sec time limit",
    "600" = "600 sec time limit",
    "3600" = "3600 sec time limit"
  )))+
  labs(fill='BSS results', tags = "D") +
  #theme(legend.position="bottom") +
  xlab("Subset size k")+
  ylab("Selected true positives") +
  stat_summary(fun.data = give.n, geom = "text", fun = median,
               position = position_dodge(width = 0.75), size =1.5) +
  geom_hline(yintercept = 10, alpha = 0)+
  geom_hline(yintercept = 0, alpha = 0)+
  scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (10+1 ) * 1.1)))))

# calculate the possible best TP for each subset size and add it as an orange
# line to plot g4. For k>= 10 it is 1
for(i in 1:15){
  if(i>10){
    TP <- 10
  }else{
    TP <- i
  }
  g4 <- g4 + 
    geom_segment(y=TP, 
                 yend=TP, 
                 x= i -0.4,
                 xend=i +0.4, color="orange")
  rm(TP)
}


# arrange the 4 plots
ggarrange(ggarrange(g1, g2, ncol=2, legend = FALSE), g3, g4, ncol=1,
          common.legend = TRUE, 
          legend="bottom")

# save the plots
ggsave("./plots/Time_comparison_semisynlowfirst_snr042.png",
       width = 18, 
       height = 24, units = "cm", dpi = 300)
