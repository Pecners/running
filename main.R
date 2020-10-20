library(tidyverse)
library(googlesheets4)
library(lubridate)

gs4_auth(email = "spencer.schien@gmail.com")
raw <- read_sheet("https://docs.google.com/spreadsheets/d/1U1z2AYvGHG_yaOGyAlPbT9SpcVC-47hhQEQwXM46Kqw/edit#gid=0",
                  col_types = "c")

raw_type <- raw %>%
  modify_at(1, mdy) %>%
  modify_at(2, hms) %>%
  modify_at(c(3:4), as.numeric) %>%
  mutate_at(5, function(x) as.duration(ms(x)))

full_times <- raw_type %>%
  group_by(date, distance) %>%
  summarise(time_5k =  dseconds(sum(pace))) %>%
  ungroup()

pr_5k <- full_times %>%
  filter(distance == 5) %>%
  summarise(fastest = min(time_5k)) %>%
  .[[1]]
  
full_times %>%
  filter(distance == 5 & date > ymd("2020-01-01")) %>%
  ggplot(aes(date, time_5k)) +
  geom_point() +
  geom_hline(linetype = 3, yintercept = pr_5k) +
  scale_y_time(limits = c(60 * 25, 60* 35), breaks = 60 * seq(25, 35, 5)) +
  theme_minimal()
