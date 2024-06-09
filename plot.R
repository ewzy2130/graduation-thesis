library(ggplot2)
library(gridExtra)
library(dplyr)
methods <- c("FairBoosting", "FairBART", "FairRF","quantBoosting","quantBART","quantRF")


new_ex_RF <- read.csv("./results/new_ex_RF.csv")
new_in_RF <- read.csv("./results/new_in_RF.csv")
new_na_RF <- read.csv("./results/new_na_RF.csv")
old_ex_RF <- read.csv("./results/old_ex_RF.csv")
old_in_RF <- read.csv("./results/old_in_RF.csv")
old_na_RF <- read.csv("./results/old_na_RF.csv")

resnew <- rbind(new_ex_RF,new_in_RF,new_na_RF,old_ex_RF,old_in_RF,old_na_RF)
resnew$score <- 'rf'
#write.csv(resnew, "resnew.csv", row.names = FALSE)
#resnew <- read.csv("resnew.csv")

method_levels <- c("FairBoosting", "FairBART", "FairRF", "quantBoosting","quantBART","quantRF")
method_labels <- c("FairBoosting", "FairBART", "FairRF", "quantBoosting","quantBART","quantRF")

#覆盖率
resnew %>%   
  mutate(exprid = factor(exprid,  
                         levels = 1:3,  
                         labels = c("inexact",  
                                    "exact",  
                                    "naive"  
                         ))) %>%  
  mutate(method = factor(method,  
                         levels = method_levels,  
                         labels = method_labels)) %>%  
  ggplot(aes(x = method, y = coverage)) +  
  geom_boxplot() +  
  facet_wrap(~ exprid, ncol = 3) +  
  geom_hline(yintercept = 0.95, color = "red") +  
  ylim(c(0, 1)) +  
  xlab("Method") + ylab("Coverage of ITE (alpha = 0.05)") +   
  coord_flip() +  
  theme_bw() +  
  theme(panel.grid = element_blank(),  
        strip.text = element_text(size = 15))

resnew %>%   
  mutate(exprid = factor(exprid,  
                         levels = 1:3,  
                         labels = c("inexact",  
                                    "exact",  
                                    "naive"  
                         ))) %>%  
  mutate(method = factor(method,  
                         levels = method_levels,  
                         labels = method_labels)) %>%  
  ggplot(aes(x = method, y = length)) +  
  geom_boxplot() +   
  facet_wrap(~ exprid, ncol = 3) +  
  geom_hline(yintercept = 2.1, color = "red") +  
  ylim(c(0, 3)) +  
  xlab("Method") + ylab("Interval lengths for ITE (alpha = 0.05)") +   
  coord_flip() +  
  theme_bw() +  
  theme(panel.grid = element_blank(),  
        strip.text = element_text(size = 15))

#ks_low
resnew %>%   
  mutate(exprid = factor(exprid,  
                         levels = 1:3,  
                         labels = c("inexact",  
                                    "exact",  
                                    "naive"  
                         ))) %>%  
  mutate(method = factor(method,  
                         levels = method_levels,  
                         labels = method_labels)) %>%  
  ggplot(aes(x = method, y = KS_lo)) +  
  geom_boxplot() +  
  facet_wrap(~ exprid, ncol = 3) +  
  geom_hline(yintercept = 0.05, color = "red") +  
  ylim(c(0, 0.3)) +  
  xlab("Method") + ylab("KS distance(low)  (alpha = 0.05)") +   
  coord_flip() +  
  theme_bw() +  
  theme(panel.grid = element_blank(),  
        strip.text = element_text(size = 15))


#ks_up
resnew %>%   
  mutate(exprid = factor(exprid,  
                         levels = 1:3,  
                         labels = c("inexact",  
                                    "exact",  
                                    "naive"  
                         ))) %>%  
  mutate(method = factor(method,  
                         levels = method_levels,  
                         labels = method_labels)) %>%  
  ggplot(aes(x = method, y = KS_up)) +  
  geom_boxplot() +  
  
  facet_wrap(~ exprid, ncol = 3) +  
  geom_hline(yintercept = 0.05, color = "red") +  
  ylim(c(0, 0.4)) +  
  xlab("Method") + ylab("KS distance(up) (alpha = 0.05)") +   
  coord_flip() +  
  theme_bw() +  
  theme(panel.grid = element_blank(),  
        strip.text = element_text(size = 15))

