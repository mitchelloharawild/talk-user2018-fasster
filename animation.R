library(tidyverse)
library(fasster)
library(lubridate)
library(tsibble)
library(ggplot2)
library(gganimate)


gif2mp4 <- function(gif, path){
  tmp_gif <- tempfile()
  magick::image_write(gif, tmp_gif)
  message("Converting gif to mp4...")
  system(paste0('ffmpeg -i ', tmp_gif, ' -y -movflags faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" ', path))
  message("Done!")
}

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
  transition_states(unit, 10, 1, wrap = FALSE) + 
  ease_aes('cubic-out') + 
  view_follow(fixed_x = TRUE)

gif1 <- animate(p, device = "png", width = 1000, height = 600)

gif1_split <- split_animation(gif1, cut(gganimate::frame_vars(gif1)$frame, c(0, 3, 51, 100)))

gif1 %>% 
  gif2mp4("/home/mitchell/Desktop/test.mp4") %>%

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
  xlab("Time") + ylab("Electiricty Demand (GW)") +
  view_step_manual(1,10, 
                   c(xrange[1],xrange[c(1,3)]), 
                   c(xrange[2],xrange[c(2,4)]),
                   c(1000, yrange[c(1,3)]),
                   c(1001, yrange[c(2,4)]),
                   wrap = FALSE)

gif2 <- animate(p2, device = "png", width = 1000, height = 600)
gif2_split <- split_animation(gif2, cut(gganimate::frame_vars(gif2)$frame, c(0, 31, 70, 100)))
gif2_split[[2]]
