library(tidyverse)
library(fasster)
library(lubridate)
library(tsibble)
library(ggplot2)
library(gganimate)

anim_points <- c("month", "day", "30 minutes")

make_frame <- function(unit){
  agg_elec <- tsibbledata::elecdemand %>% 
    index_by(Time = !!expr(floor_date(index, unit = !!unit))) %>%
    summarise(Demand = sum(Demand), x = median(index)) %>%
    mutate(unit = unit)
}

plot_data <- anim_points %>%
  map_dfr(make_frame)

p <- plot_data %>% 
  as_tibble %>%
  mutate(unit = factor(unit, levels = anim_points)) %>% 
  ggplot(aes(x=x, y=Demand)) + 
  geom_line() + 
  xlab("Time") + ylab("Electiricty Demand (GW)") +
  transition_states(unit, 8, 8, wrap = FALSE) + 
  ease_aes('cubic-out') + 
  view_follow(fixed_x = TRUE)

animate(p, device = "png", width = 1000, height = 600)

frame_vars()$frame_source %>% dirname %>% unique %>% list.files

tsibbledata::elecdemand %>%
  mutate(state = 1) %>%
  bind_rows(
    tsibbledata::elecdemand %>%
      filter(month(index) == 6) %>% 
      mutate(state = 2)
  ) %>% 
  as_tibble %>% 
  mutate(index = as.numeric(index)) %>% 
  ggplot(aes(x=index, y=Demand)) + 
  geom_line() + 
  transition_states(state, 8, 8, wrap = FALSE) + 
  ease_aes('cubic-out') + 
  view_follow(fixed_x = FALSE)
  
yrange <- c(tsibbledata::elecdemand %>%
  .$Demand %>% range,
  tsibbledata::elecdemand %>%
  filter(month(index) == 6) %>%
  .$Demand %>% range
)

xrange <- c(tsibbledata::elecdemand %>%
              .$index %>% range,
            tsibbledata::elecdemand %>%
              filter(month(index) == 6) %>%
              .$index %>% range
)

p2 <- tsibbledata::elecdemand %>% 
  autoplot(Demand) + 
  view_step_manual(1,1, xrange[c(1,3)], xrange[c(2,4)], yrange[c(1,3)], yrange[c(2,4)])

animate(p2, device = "png", width = 1000, height = 600)
