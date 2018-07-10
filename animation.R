library(tidyverse)
library(fasster)
library(lubridate)
library(tsibble)
library(ggplot2)

anim_points <- c("month", "day", "30 minutes")

make_frame <- function(unit){
  agg_elec <- tsibbledata::elecdemand %>% 
    index_by(Time = !!expr(floor_date(index, unit = !!unit))) %>%
    summarise(Demand = sum(Demand), x = median(index)) %>%
    mutate(unit = unit)
}

plot_data <- anim_points %>%
  map_dfr(make_frame)

plot_data %>% 
  as_tibble %>%
  mutate(unit = factor(unit, levels = anim_points)) %>% 
  ggplot(aes(x=Time, y=Demand)) + 
  geom_line() + 
  transition_states(unit, 8, 8, wrap = FALSE) + 
  ease_aes('cubic-out') + 
  view_follow(fixed_x = TRUE)


anim <- split_animation(by = frame_vars()$previous_state)
