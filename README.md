Collection of plots via ggplots
================
Jiaqi Yin
07 May, 2019

-   [`titanic data`](#titanic-data)

``` r
knitr::opts_chunk$set(echo = TRUE,nclude=FALSE, cache=TRUE, include=TRUE, message=FALSE, warning=FALSE)
rm(list = ls()) # clean all the data
```

``` r
library(ggplot2)
library(dplyr)
library(tidyr)

library(rms)
library(rmarkdown)
```

> `dplyr`, `tidyr`, and `ggplots` are excellent tools dealing with data.
> There are so many usefull functions in those packages and sometimes we (I) may forget how to implement those functions. Therefore, I try to make a notebook where I collect some representative work from my previous/current projects. There are no certain orders here. However, once it (this notebook) grows large enough, I will organize it and maybe have blogs for it.

`titanic data`
--------------

``` r
getHdata(titanic3)

dat = titanic3 %>% select(survived, pclass, sex, age) # only select those variables

dat$dead = 1 - dat$survived

# dead vs. passeneger class
dat %>%
      group_by(pclass, dead) %>%
      summarise(n = n()) %>% # count number
      mutate(N = sum(n), prop = n / sum(n)) %>% # add prop column
      filter(dead == 1) %>% ggplot(aes(x = pclass, y = prop)) + 
      geom_point(aes(size = N), alpha = 0.7, color = "red") +
      scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
      scale_size_continuous(breaks = c(300, 500, 700), range = c(10,25), name = "Size") + 
      xlab("Passenger Class") +
      ylab("Probability of Death") +
      theme_bw() + # white backgroud
      theme(axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),
            axis.text.y = element_text(color = "grey20", size = 10, face = "plain"),
            axis.title.x = element_text(color = "grey20", size = 15, face = "bold"),
            axis.title.y = element_text(color = "grey20", size = 15, face = "bold"),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10, face = "bold"),
            legend.position="bottom")
```

![](README_files/figure-markdown_github/load%20titanic%20data-1.png)
