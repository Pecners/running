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
  filter(completed == 1) %>%
  group_by(date, distance) %>%
  summarise(time_5k =  dseconds(sum(pace))) %>%
  ungroup()

pr_5k <- full_times %>%
  filter(distance == 5) %>%
  summarise(fastest = min(time_5k)) %>%
  .[[1]]

pr_5k_recent <- full_times %>%
  filter(distance == 5) %>%
  filter(time_5k > min(time_5k)) %>%
  summarise(min(time_5k)) %>%
  .[[1]]

gap <- dseconds(pr_5k_recent - pr_5k)

mod_df <- full_times %>%
  filter(distance == 5 & year(date) > 2019) %>%
  mutate(adj_date = as.numeric(date) - min(as.numeric(date)))

mod <- lm(adj_date ~ time_5k, data = mod_df)

summary(mod)

goal_date <- min(mod_df$date) + predict(mod, data.frame(time_5k = pr_5k))

regression_line <- function(t) {
  return(-.20342 * t + 382.54824)
}

times <- c(1862, 1606)
dates <- min(mod_df$date) + regression_line(times)

reg_df <- tibble(times, dates)
  
full_times %>%
  filter(distance == 5 & date > ymd("2020-01-01")) %>%
  ggplot(aes(date, time_5k)) +
  geom_point() +
  geom_segment(aes(x = ymd(dates[1]), xend = ymd(dates[2]), y = times[1], yend = times[2])) +
  annotate(geom = "text", label = paste0("Predicted completion date:\n", goal_date),
           x = ymd(goal_date), y = 30 * 60) +
  scale_x_date(limits = c(min(mod_df$date) - 7, goal_date + 14)) +
  geom_hline(linetype = 3, yintercept = pr_5k) +
  scale_y_time(limits = c(60 * 25, 60 * 35), breaks = 60 * seq(25, 35, 5)) +
  theme_minimal() +
  geom_smooth(method = stats::lm, se = FALSE, fullrange = TRUE)


