library(readxl)
library(tidyverse)

# import excel file
dataset <- read_excel("~kimmytran/MIE286_data.xlsx")

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
normal_time_live <- shapiro.test(dataset$mean_time[dataset$game_version == "A"]) # live
normal_time_post <- shapiro.test(dataset$mean_time[dataset$game_version == "B"]) # post
normal_crash_live <- shapiro.test(dataset$mean_crashes[dataset$game_version == "A"]) # live
normal_crash_post <- shapiro.test(dataset$mean_crashes[dataset$game_version == "B"]) # post

normal_time_imp_live <- shapiro.test(dataset$time_improvement[dataset$game_version == "A"]) # live
normal_time_imp_post <- shapiro.test(dataset$time_improvement[dataset$game_version == "B"]) # post
normal_crash_imp_live <- shapiro.test(dataset$crash_improvement[dataset$game_version == "A"]) # live
normal_crash_imp_post <- shapiro.test(dataset$crash_improvement[dataset$game_version == "B"]) # post

normal_time_live
normal_time_post
normal_crash_live
normal_crash_post

normal_time_imp_live
normal_time_imp_post
normal_crash_imp_live 
normal_crash_imp_post

# colours of plots (we can change)
live_bots_col <- "#0074D9" 
post_no_bots_col <- "#FF851B"

# -----------------------------------------------
# histograms of mean_time by game_version

# live_bots
png("hist_mean_time_A.png", width = 800, height = 600)
hist(dataset$mean_time[dataset$game_version == "A"],
     col = c(live_bots_col),
     breaks = 12,
     main = "Ver. A - Live with Bots: Speed",
     xlab = "Mean Time (Seconds)")
dev.off()

# post_no_live bots
png("hist_mean_time_B.png", width = 800, height = 600)
hist(dataset$mean_time[dataset$game_version == "B"],
     col = c(post_no_bots_col),
     breaks = 12,
     main = "Ver. B - Post with No Bots: Speed",
     xlab = "Mean Time (Seconds)")
dev.off()
# -----------------------------------------------
# histograms of mean_crashes by game_version
# live_bots
png("hist_mean_crashes_A.png", width = 800, height = 600)
hist(dataset$mean_crashes[dataset$game_version == "A"],
     col = c(live_bots_col),
     breaks = 12,
     main = "Ver. A - Live with Bots: Accuracy",
     xlab = "Mean Crashes (# of Crashes)")
dev.off()

# post_no_live bots
png("hist_mean_crashes_B.png", width = 800, height = 600)
hist(dataset$mean_crashes[dataset$game_version == "B"],
     col = c(post_no_bots_col),
     breaks = 12,
     main = "Ver. B - Post with No Bots: Accuracy",
     xlab = "Mean Crashes (# of Crashes)")
dev.off()
# -----------------------------------------------
#histograms of time improvement by game_version
# live_bots
png("hist_time_imp_A.png", width = 800, height = 600)
hist(dataset$time_improvement[dataset$game_version == "A"],
     col = c(live_bots_col),
     breaks = 12,
     main = "Ver. A - Live with Bots: Speed Improvement from Trial 1 to 3",
     xlab = "Time Improved from Trial 1 to 3 (Seconds)")
dev.off()

# post_no_live bots
png("hist_time_imp_B.png", width = 800, height = 600)
hist(dataset$time_improvement[dataset$game_version == "B"],
     col = c(post_no_bots_col),
     breaks = 12,
     main = "Ver. B - Post with No Bots: Speed Improvement from Trial 1 to 3",
     xlab = "Time Improved from Trial 1 to 3 (Seconds)")
dev.off()

# -----------------------------------------------
#histograms of crash improvement by game_version
# live_bots
png("hist_crashes_imp_A.png", width = 800, height = 600)
hist(dataset$crash_improvement[dataset$game_version == "A"],
     col = c(live_bots_col),
     breaks = 12,
     main = "Ver. A - Live with Bots: Accuracy Improvement from Trial 1 to 3",
     xlab = "Crash Improved from Trial 1 to 3 (# of Crashes)")
dev.off()

# post_no_live bots
png("hist_crashes_imp_B.png", width = 800, height = 600)
hist(dataset$crash_improvement[dataset$game_version == "B"],
     col = c(post_no_bots_col),
     breaks = 12,
     main = "Ver. B - Post with No Bots: Accuracy Improvement from Trial 1 to 3",
     xlab = "Crash Improved from Trial 1 to 3 (# of Crashes)")
dev.off()
# -----------------------------------------------
# boxplot comparing mean_time across two game_versions
boxplot(mean_time ~ game_version, data = dataset,
        col = c(live_bots_col, post_no_bots_col),
        main = "Speed by Condition",
        xlab = "Game Version", ylab = "Mean Time (Seconds)")

# boxplot comparing mean_crashes across two game_versions
boxplot(mean_crashes ~ game_version, data = dataset,
        col = c(live_bots_col, post_no_bots_col),
        main = "Accuracy by Condition",
        xlab = "Game Version", ylab = "Mean Crashes (# of Crashes)")

# boxplot comparing time_improvement across two game_versions
boxplot(time_improvement ~ game_version, data = dataset,
        col = c(live_bots_col, post_no_bots_col),
        main = "Speed Improvement from Trial 1 to 3 by Condition",
        xlab = "Game Version", ylab = "Time Improved from Trial 1 to 3 (Seconds)")

# boxplot comparing crash_improvement across two game_versions
boxplot(crash_improvement ~ game_version, data = dataset,
        col = c(live_bots_col, post_no_bots_col),
        main = "Accuracy Improvement from Trial 1 to 3 by Condition",
        xlab = "Game Version", ylab = "Crash Improvement from Trial 1 to 3 (# of Crashes)")
# -----------------------------------------------

# ALL tests @ confidence level of 95% (alpha = 0.05)
# check if variances are equal (null) or unequal (alt)
# if p < 0.05 --> reject H0 --> variances are unequal
# if p > 0.05 --> fail to reject H0 --> variances are equal
var_time <- var.test(mean_time ~ game_version, data = dataset, conf.level = 0.95) 
var_crashes <- var.test(mean_crashes ~ game_version, data = dataset, conf.level = 0.95)
var_time_imp <- var.test(time_improvement ~ game_version, data = dataset, conf.level = 0.95) 
var_crashes_imp <- var.test(crash_improvement ~ game_version, data = dataset, conf.level = 0.95)

var_time
var_crashes
var_time_imp
var_crashes_imp

# print unpaired t-test values; specifically look at p-value for hypotheses
# if p < 0.05 --> reject H0 --> IS significant difference between groups
# if p > 0.05 --> fail to reject H0 --> NO significant evidence of difference
# welch's test
t_time <- t.test(mean_time ~ game_version, data = dataset, conf.level = 0.95)
t_crashes <- t.test(mean_crashes ~ game_version, data = dataset, conf.level = 0.95)
t_time_imp <- t.test(time_improvement ~ game_version, data = dataset, conf.level = 0.95)
t_crashes_imp <- t.test(crash_improvement ~ game_version, data = dataset, conf.level = 0.95)

t_time
t_crashes
t_time_imp
t_crashes_imp

# pearson correlation test
# if p < 0.05 --> statistically significant
# if cor = negative --> faster (smaller time), more crashes
# if cor = positive --> slower (larger time), more crashes
# if cor = 0 --> no relationship
cor_speed_accuracy <- cor.test(dataset$mean_time, dataset$mean_crashes, conf.level = 0.95)
cor_speed_accuracy_imp <- cor.test(dataset$time_improvement, dataset$crash_improvement, conf.level = 0.95)

cor_speed_accuracy
cor_speed_accuracy_imp

# plot the correlation between crashes and time
ggplot(dataset, aes(x = mean_time, y = mean_crashes, color = game_version)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c(live_bots_col, post_no_bots_col)) +
  labs(
    title = "Speed–Accuracy Relationship",
    x = "Mean Time (Seconds)",
    y = "Mean Crashes (# of Crashes)")

# plot the correlation between crashes and time
ggplot(dataset, aes(x = time_improvement, y = crash_improvement, color = game_version)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c(live_bots_col, post_no_bots_col)) +
  labs(
    title = "Speed–Accuracy Improvement Relationship",
    x = "Time Improvement from Trial 1 to 3 (Seconds)",
    y = "Crash Improvement from Trial 1 to 3 (# of Crashes)")

