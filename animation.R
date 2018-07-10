library(tidyverse)
library(fasster)
library(lubridate)
library(tsibble)
library(ggplot2)

anim_points <- c("month", "day", "30 minutes")

make_frame <- function(unit, pos){
  agg_elec <- tsibbledata::elecdemand %>% 
    index_by(Time = !!expr(floor_date(index, unit = !!unit))) %>%
    summarise(Demand = sum(Demand)) %>%
    mutate(unit = pos)
}

plot_data <- anim_points %>%
  imap_dfr(make_frame)

plot_data %>% 
  ggplot(aes(x=Time, y=Demand)) + 
  geom_line() + 
  transition_states(unit, 5, 3) + 
  ease_aes('elastic-in') + 
  view_follow(fixed_x = TRUE)
