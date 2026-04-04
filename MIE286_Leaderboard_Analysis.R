library(readxl)
library(tidyverse)

dataset <- read_excel("~kimmytran/fake_leaderboard_dataset.xlsx")

dataset$mean_time <- rowMeans(dataset[, c("race1_time_sec", "race2_time_sec", "race3_time_sec")])
dataset$mean_crashes <- rowMeans(dataset[, c("race1_crashes", "race2_crashes", "race3_crashes")])

dataset$time_improvement <- dataset$race1_time_sec - dataset$race3_time_sec
dataset$crash_improvement <- dataset$race1_crashes - dataset$race3_crashes

# compute mean by game_version
mean_by_game_version <- aggregate(cbind(mean_time, mean_crashes, time_improvement, crash_improvement) ~ game_version,
          data = dataset, mean)

# compute variance by game_version
var_by_game_version <- aggregate(cbind(mean_time, mean_crashes, time_improvement, crash_improvement) ~ game_version,
          data = dataset, var)

# compute sd by game_version for each variable (mean_time, mean_crashes, time_ip)
sd_by_game_version <- aggregate(cbind(mean_time, mean_crashes, time_improvement, crash_improvement) ~ game_version,
          data = dataset, sd)

mean_by_game_version
var_by_game_version
sd_by_game_version

# normality tests; shapiro-wilk test n < 5000
# if p < 0.05 --> reject H0 --> data IS NOT normally distributed
# if p > 0.05 --> fail to reject H0 --> data IS normally distributed
normal_time_live <- shapiro.test(dataset$mean_time[dataset$game_version == "live_bots"])
normal_time_post <- shapiro.test(dataset$mean_time[dataset$game_version == "post_no_bots"])
normal_crash_live <- shapiro.test(dataset$mean_crashes[dataset$game_version == "live_bots"])
normal_crash_post <- shapiro.test(dataset$mean_crashes[dataset$game_version == "post_no_bots"])


normal_time_live
normal_time_post
normal_crash_live
normal_crash_post

# colours of plots (we can change)
live_bots_col <- "lightblue" 
post_no_bots_col <- "lightgreen"

# histograms of mean_time by game_version
# live_bots
hist(dataset$mean_time[dataset$game_version == "live_bots"],
     col = c(live_bots_col),
     main = "Live Bots: Mean Time",
     xlab = "Mean Time")

# post_no_live bots
hist(dataset$mean_time[dataset$game_version == "post_no_bots"],
     col = c(post_no_bots_col),
     main = "Post No Bots: Mean Time",
     xlab = "Mean Time")

# plot two histograms comparing mean_time together
ggplot(dataset, aes(x = mean_time, fill = game_version)) +
  scale_fill_manual(values = c(live_bots_col,
                            post_no_bots_col)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 1) +  
  labs(title = "Mean Time Distribution by Game Version", x = "Mean Time of Completion by Participant", y = "Frequency")

# histograms of mean_crashes by game_version
# live_bots
hist(dataset$mean_crashes[dataset$game_version == "live_bots"],
     col = c(live_bots_col),
     main = "Live Bots: Mean Crashes",
     xlab = "Mean Crashes")

# post_no_live bots
hist(dataset$mean_crashes[dataset$game_version == "post_no_bots"],
     col = c(post_no_bots_col),
     main = "Post No Bots: Mean Crashes",
     xlab = "Mean Crashes")

# plot two histograms comparing mean_crashes together
ggplot(dataset, aes(x = mean_crashes, fill = game_version)) +
  scale_fill_manual(values = c(live_bots_col,
                               post_no_bots_col)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 1) +  
  labs(title = "Mean Crash Distribution by Game Version", x = "Mean Crashes by Participant", y = "Frequency")

# boxplot comparing mean_time across two game_versions
boxplot(mean_time ~ game_version, data = dataset,
        col = c(live_bots_col, post_no_bots_col),
        main = "Mean Completion Time by Condition",
        xlab = "Game Version", ylab = "Mean Time (sec)")

# boxplot comparing mean_crashes across two game_versions
boxplot(mean_crashes ~ game_version, data = dataset,
        col = c(live_bots_col, post_no_bots_col),
        main = "Mean Crashes by Condition",
        xlab = "Game Version", ylab = "Mean # of Crashes")

# ALL tests @ confidence level of 95% (alpha = 0.05)
# check if vaariances are equal (null) or unequal (alt)
# if p < 0.05 --> reject H0 --> variances are unequal
# if p > 0.05 --> fail to reject H0 --> variances are equal
var_time <- var.test(mean_time ~ game_version, data = dataset, conf.level = 0.95) 
var_crashes <- var.test(mean_crashes ~ game_version, data = dataset, conf.level = 0.95)

var_time
var_crashes

# print unpaired t-test values; specifically look at p-value for hypotheses
# if p < 0.05 --> reject H0 --> IS significant difference between groups
# if p > 0.05 --> fail to reject H0 --> NO significant evidence of difference
# if variances are unequal, welch's t-test
t_time <- t.test(mean_time ~ game_version, data = dataset, conf.level = 0.95)
t_crashes <- t.test(mean_crashes ~ game_version, data = dataset, conf.level = 0.95)

t_time
t_crashes

# if equal variances (can delete if not equal)
t_time <- t.test(mean_time ~ game_version, data = dataset, var.equal = TRUE, conf.level = 0.95)
t_crashes <- t.test(mean_crashes ~ game_version, data = dataset, var.equal = TRUE, conf.level = 0.95)

# pearson correlation test
# if p < 0.05 --> statistically significant
# if cor = negative --> faster (smaller time), more crashes
# if cor = positive --> slower (larger time), more crashes
# if cor = 0 --> no relationship
cor_speed_accuracy <- cor.test(dataset$mean_time, dataset$mean_crashes, conf.level = 0.95)

cor_speed_accuracy

# plot the correlation between crashes and time
ggplot(dataset, aes(x = mean_time, y = mean_crashes, color = game_version)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c(live_bots_col, post_no_bots_col)) +
  labs(
    title = "Speed–Accuracy Relationship",
    x = "Mean Time (Seconds)",
    y = "Mean # of Crashes")
