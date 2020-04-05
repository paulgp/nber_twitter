

library(rtweet)
library(tidyverse)
library(lubridate)
library(scales)
tmls <- get_timelines(c("nberpubs"), n = 3200)

nber_tweets = tmls %>% select(status_id, created_at, text,favorite_count, retweet_count) %>%
  mutate(created_at = with_tz(created_at, tzone = "America/New_York")) %>%
  filter(status_id != 1173272766151180288) 

counts_day = nber_tweets %>% mutate(wday = wday(created_at, label = TRUE)) %>% 
  group_by(wday)  %>% 
  summarize(num_posts = n(),
            num_likes_med = median(favorite_count),
            num_rt_med = median(retweet_count),
            num_likes = mean(favorite_count),
            num_rt = mean(retweet_count)) %>%
  mutate(weekend = (wday == "Sat" | wday == "Sun"))

counts_hour = nber_tweets %>% mutate(hour = hour(created_at)) %>% 
  group_by(hour) %>% 
  summarize(num_posts = n(),
            num_likes = median(favorite_count),
            num_rt = median(retweet_count))

counts_day_hour = nber_tweets %>% 
  mutate(post_time = paste(hour(round_date(created_at, "1 hour")), 
                           minute(round_date(created_at, "1 hour")), sep = ":"),
         wday = wday(created_at, label = TRUE)) %>%
  group_by(wday, post_time) %>% 
  summarize(num_posts = n(),
            num_likes = mean(favorite_count),
            num_rt = mean(retweet_count)) %>%
  filter(!is.na(post_time)) %>%
  mutate(post_time = parse_date_time(post_time, "%H! %M!")) %>%
  mutate(weekend = (wday == "Sat" | wday == "Sun"))

ggplot(data = counts_day_hour %>% filter(hour(post_time) < 19 & hour(post_time) > 8)) +
  geom_line(aes(y = num_posts, x = post_time, group = as.factor(wday), color = weekend)) +
  scale_x_datetime(breaks = date_breaks("1 hours"),
                   labels = date_format("%H:%M")) +
  labs(color = "Weekend", x = "Time of Posting (rounded to hour)", y = "Number of Posts",
       title = "Number of posts across time of day and days of the week by @nberpubs") + 
    theme_classic(base_size = 16)

ggplot(data = counts_day_hour %>% filter(hour(post_time) < 19 & hour(post_time) > 8)) +
  geom_line(aes(y = num_posts, x = post_time, color = weekend)) +
  scale_x_datetime(breaks = date_breaks("1 hours"),
                   labels = date_format("%H:%M")) +
  facet_wrap(~wday) +
  labs(color = "Weekend", x = "Time of Posting (rounded to hour)", y = "Number of Posts",
       title = "Number of posts across time of day and days of the week by @nberpubs") + 
    theme_classic(base_size = 16)

ggplot(data = counts_day_hour %>% filter(hour(post_time) < 19 & hour(post_time) > 8)) +
  geom_line(aes(y = num_likes, x = post_time, group = as.factor(wday), color = weekend)) +
  scale_x_datetime(breaks = date_breaks("1 hours"),
                   labels = date_format("%H:%M")) +
  labs(color = "Weekend", x = "Time of Posting (rounded to hour)", y = "Average number of likes",
       title = "Average number of likes across time of day and days of the week on posts by @nberpubs") + 
    theme_classic(base_size = 16)
ggplot(data = counts_day_hour %>% filter(hour(post_time) < 19 & hour(post_time) > 8)) +
  geom_line(aes(y = num_rt, x = post_time, group = as.factor(wday), color = weekend)) +
  scale_x_datetime(breaks = date_breaks("1 hours"),
                   labels = date_format("%H:%M")) +
  labs(color = "Weekend", x = "Time of Posting (rounded to hour)", y = "Average number of likes",
       title = "Average number of retweets across time of day and days of the week on posts by @nberpubs") + 
  theme_classic(base_size = 16)

ggplot(data = counts_day) +
  geom_col(aes(y = num_posts, x = wday, fill = as.factor(weekend))) +
  scale_fill_discrete(guide=FALSE) +
  labs(x = "Day of Posting", y = "Total Number of Posts",
       title = "Number of posts across days of the week by @nberpubs") + 
  theme_classic(base_size = 16)

ggplot(data = counts_day) +
  geom_col(aes(y = num_likes, x = wday, fill = as.factor(weekend))) +
  scale_fill_discrete(guide=FALSE) +
  labs(x = "Day of Posting", y = "Average Number of Likes",
       title = "Average number of likes across days of the week on posts by @nberpubs") + 
  theme_classic(base_size = 16)

ggplot(data = counts_day) +
  geom_col(aes(y = num_rt, x = wday, fill = as.factor(weekend))) +
  scale_fill_discrete(guide=FALSE) +
  labs(x = "Day of Posting", y = "Average Number of Retweets",
       title = "Average number of retweets across days of the week on posts by @nberpubs") + 
  theme_classic(base_size = 16)

reg_data = nber_tweets %>% mutate(hour = hour(created_at), wday = wday(created_at, label = TRUE),
                                  weekend = (wday == "Sat" | wday == "Sun")) %>%
  filter(!is.na(created_at))

summary(lm(retweet_count ~ weekend, data = reg_data ), robust = TRUE)
summary(lm(favorite_count ~ weekend, data = reg_data ), robust = TRUE)
summary(lm(num_posts ~ weekend, data = counts_day ), robust = TRUE)
