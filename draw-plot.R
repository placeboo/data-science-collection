rm(list = ls())
library(rms)
library(ggplot2)
library(dplyr)
library(tidyr)

getHdata(titanic3)

dat = titanic3 %>% select(survived, pclass, sex, age)

dat$dead = 1 - dat$survived
# dead vs class
dat %>%
      group_by(pclass, dead) %>%
      summarise(n = n()) %>%
      mutate(N = sum(n), prop = n / sum(n)) %>%
      filter(dead == 1) %>% ggplot(aes(x = pclass, y = prop)) +
      geom_point(aes(size = N), alpha = 0.7, color = "red") +
      scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
      scale_size_continuous(breaks = c(300, 500, 700), range = c(10,25), name = "Size") +
      xlab("Passenger Class") +
      ylab("Probability of Death") +
      theme_bw() +
      theme(strip.text.x = element_text(size=24, face="bold"),
            axis.text.x = element_text(color = "grey20", size = 14, face = "plain"),
            axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),
            axis.title.x = element_text(color = "grey20", size = 20, face = "bold"),
            axis.title.y = element_text(color = "grey20", size = 20, face = "bold"),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 14, face = "bold"),
            legend.position="bottom")

ggsave(filename = "r-figure/dead-prop-pclass.png", width = 180, height = 120, units = "mm")

dat_nm = dat %>% filter(!is.na(age))


# age < 20
tmp_dat1 = dat_nm %>%
      filter(age < 20) %>%
      group_by(pclass, sex, dead) %>%
      summarise(n = n()) %>%
      mutate(N = sum(n), prop = n / sum(n)) %>%
      filter(dead == 1) %>%
      mutate(age = "<20 yrs")


# age > 45
tmp_dat2 = dat_nm %>%
      filter(age > 45) %>%
      group_by(pclass, sex, dead) %>%
      summarise(n = n()) %>%
      mutate(N = sum(n), prop = n / sum(n)) %>%
      filter(dead == 1) %>%
      mutate(age = ">45 yrs")

dat1 = rbind(tmp_dat1, tmp_dat2)

dat1 %>% ggplot(aes(x = age, y = prop)) +
      geom_point(aes(shape = pclass, color = sex, size = N), alpha = 0.7) +
      scale_size(range = c(0, 15), name = "Number") +
      scale_shape_discrete(name = "Passenger Class") +
      scale_color_discrete(name = "Sex") +
      xlab("Age") +
      ylab("Probability of Death") +
      theme_bw() +
      guides(colour = guide_legend(override.aes = list(size=5)),
             shape = guide_legend(override.aes = list(size=5))) + # change the legend symbol
      theme(strip.text.x = element_text(size=24, face="bold"),
            axis.text.x = element_text(color = "grey20", size = 14, face = "plain"),
            axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),
            axis.title.x = element_text(color = "grey20", size = 24, face = "bold"),
            axis.title.y = element_text(color = "grey20", size = 24, face = "bold"),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 14, face = "bold"))

ggsave(filename = "r-figure/prop_emp.png", width = 150, height = 200, units = "mm")
