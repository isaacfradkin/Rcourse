### Final ex soultion for lesson 11 - internal file
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
# import data
stroop_data <- read_csv("csv files/stroop_data.csv")
# calculate new columns
stroop_data = stroop_data %>% mutate(correct = stroop_data$response == stroop_data$color,
                                      congruent = stroop_data$text == stroop_data$color)
stroop_data$congruent = as.factor(stroop_data$congruent)
levels(stroop_data$congruent) = c("Incongruent", "Congruent")
stroop_data$correct = as.factor(stroop_data$correct)

#q1
length(unique(stroop_data$session_id))
#q2
summary(stroop_data)
# q3
sum(stroop_data$correct==0)

# step 1
new_stroop_data = filter(stroop_data, stroop_data$correct==TRUE & stroop_data$response_time<=1500)

# step 2
sum_stroop_data =new_stroop_data %>% group_by(session_id, congruent) %>% summarise(mean_rt =  mean(response_time))

# step 3
wide_stroop_data <- sum_stroop_data %>%
  pivot_wider(
    names_from = congruent, 
    values_from = mean_rt, 
  )
scatter_plot <- ggplot(wide_stroop_data, aes(x = Congruent, y = Incongruent))+ geom_point(alpha = 0.8) + geom_smooth(method = "lm", color = "blue", se = FALSE) + 
  labs(title = "Scatter Plot: Congurent vs Incongurent Trials", x = "Congurent", y = "Incongurent") + theme_minimal()
scatter_plot 

# step 4
wide_stroop_data <- wide_stroop_data %>%
  mutate(Difference = Incongruent - Congruent)

long_stroop_data <- wide_stroop_data %>%
  pivot_longer(
    cols = c(Congruent, Incongruent, Difference),
    names_to = "Condition",
    values_to = "RT")

box_plot <- ggplot(long_stroop_data, aes(x = Condition, y = RT)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Reaction Times and Differences",
    x = "Measure",
    y = "Reaction Time (RT)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
box_plot
# step 5
t_test_result <- t.test(x = wide_stroop_data$Congruent, y = wide_stroop_data$Incongruent, alternative = "less", paired = TRUE)

print(t_test_result)

t_test_result <- t.test(x = wide_stroop_data$Congruent, y = wide_stroop_data$Incongruent, conf.level = 0.99, paired = TRUE)

print(t_test_result)

# step 6
new_stroop_data$is_red = new_stroop_data$color=="red"
red_stroop_data = new_stroop_data %>% group_by(session_id, is_red, congruent) %>% summarise(mean_rt =  mean(response_time))
wide_red_stroop_data <- red_stroop_data %>%
  pivot_wider(
    names_from = c(congruent, is_red), 
    values_from = mean_rt, 
  )

wide_red_stroop_data$red_stroop_effect = wide_red_stroop_data$Incongruent_TRUE - wide_red_stroop_data$Congruent_TRUE
wide_red_stroop_data$no_red_stroop_effect = wide_red_stroop_data$Incongruent_FALSE - wide_red_stroop_data$Congruent_FALSE


t_test_red_result <- t.test(x = wide_red_stroop_data$red_stroop_effect, y = wide_red_stroop_data$no_red_stroop_effect, alternative = "less", paired = TRUE)

print(t_test_red_result)

long_red_stroop_data <- wide_red_stroop_data %>%
  pivot_longer(
    cols = c(no_red_stroop_effect, red_stroop_effect),
    names_to = "Color",
    values_to = "RT")
long_red_stroop_data$Color = as.factor(long_red_stroop_data$Color)
levels(long_red_stroop_data$Color) = c("Not Red", "Red")
box_plot <- ggplot(long_red_stroop_data, aes(x = Color, y = RT)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Reaction Times and Differences",
    x = "Color",
    y = "Reaction Time Differnce(RT)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



