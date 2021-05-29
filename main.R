# Probability and Statistics Final Project
# MSIT
# Final Presentation
# Christen Cacanog, Elmo Ranolo, David Ybanez

# install packes
install.packages("sqldf")

# load libraries
# library(data.table)
library(sqldf)

# import data
setwd("C:/devtools/data/")
posts_data <- read.csv("RCAC_posts_FB_Insights_CSV.csv")
videos_data <- read.csv("RCAC_videos_FB_Insights_CSV.csv")

# Sign Test (One-sample)
views = posts_data$lifetime_post_total_reach
views_median = rep(10858, length(views)) # assumption = 10858, length(views) = count

views_diff = views - views_median

views_rpos_len = length((views_diff[views_diff > 0]))
views_rneg_len = length((views_diff[views_diff < 0]))

views_r = min(views_rpos_len, views_rneg_len)
views_n = views_rpos_len + views_rneg_len
views_result = 2 * dbinom(views_r, views_n,1/2)
views_result # 8.37 < 0.01 is false, do not reject the null hypothesis

# Sign Test (Two-sample)
date_posted = posts_data$date_posted
time_posted = format(date_posted, format = "%H-%M")
  

# Wilcoxon Rank Sum Test
video_posts_data = sqldf("SELECT * FROM posts_data WHERE type = 'Video'")
photo_posts_data = sqldf("SELECT * FROM posts_data WHERE type = 'Photo' LIMIT 190")

ranked_video_photo = rank(
  c(video_posts_data$lifetime_engaged_users,
    photo_posts_data$lifetime_engaged_users))

ranked_video_photo

photo_video_results = sum(data = ranked_video_photo[1:length(video_posts_data)])

wilcox.test(
  video_posts_data$lifetime_engaged_users,
  photo_posts_data$lifetime_engaged_users,
  paired = FALSE, 
  alternative = "two.sided", 
  conf.level = 0.95)

sum_rank_video = sum(
  rank(c(
    video_posts_data$lifetime_engaged_users,
    photo_posts_data$lifetime_engaged_users))[1:length(video_posts_data)])

photo_video_w_result = sum_rank_video - (length(video_posts_data$lifetime_engaged_users)*(length(photo_posts_data$lifetime_engaged_users)+1)/2)
photo_video_w_result

# Wilcoxon Signed Rank Sum Test (One-sample)
views = posts_data$lifetime_post_total_reach
views_median = rep(10858, length(views)) # assumption = 10000, length(views) = count
views_median
wilcox.test(views,views_median, paired = FALSE, alternative = "two.sided", conf.level = 0.95)

# Wilcoxon Signed Rank Sum Test (Two-sample)


# Kruskal-Wallis Test
views_30s = videos_data$lifetime_unique_30s_views
views_60s = videos_data$lifetime_unique_60s_video_views
views_95p = videos_data$lifetime_unique_watches_at_95p

# Kruskal-Wallis Test: Solving for H
rank.30s60s95p = rank(c(views_30s, views_60s, views_95p))
rank.30s60s95p
R1 = sum(rank(c(views_30s, views_60s, views_95p))[1:299])
R2 = sum(rank(c(views_30s, views_60s, views_95p))[300:598])
R3 = sum(rank(c(views_30s, views_60s, views_95p))[599:897])
N = length(c(views_30s, views_60s, views_95p))
H = (12/(N*(N+1)))*(R1^2/length(views_30s)+R2^2/length(views_60s)+
                    R3^2/length(views_95p))-3*(N+1)
H

# Kruskal-Wallis Test: Actual test in R
kruskal.test(list(views_30s, views_60s, views_95p)) #or
a = c(views_30s, views_60s, views_95p)
g = factor(rep(1:3, c(299, 299, 299)))
kruskal.test(a~g)

# Friedman Rank Sum Test or Friedman Test
# Continuous

#videos_RoundingTimes = matrix(
#  c(views_30s, views_60s, views_95p),
#  nrow = 299,
#  byrow = TRUE,
#  dimnames = list(1 : 299, c("Views 30s", "Views 60s", "Views 95%")))

videos_RoundingTimes <- cbind(views_30s, views_60s, views_95p)
friedman.test(videos_RoundingTimes)






