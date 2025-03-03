library(dplyr)
library(ggplot2)
library(readr)

set.seed(123)
n_iter <- 10000

stroop_data <- read_csv("csv files/stroop_data.csv")

# Q1
sub_stroop <- stroop_data_full %>% group_by(session_id) %>% 
  summarise(mean_acc=mean(correct), mean_rt = mean(response_time))
acc_mean <- mean(sub_stroop$mean_acc)

# ניצור את הוקטור שישמור את התוצאות
sampled_means <- numeric(n_iter)

# הלולאה 
for (i in 1:n_iter){
  sampled_data <- sample(sub_stroop$mean_acc, replace = TRUE)
  sampled_means[i] <- mean(sampled_data)
} 

conf_level = 0.95
alpha <- 1-conf_level

# נחשב הסף העליון והתחתון של רווח הסמך על סמך ההתפלגות 
lower <- quantile(sampled_means, alpha / 2)
upper <- quantile(sampled_means, 1 - alpha / 2)
# נציג
paste0("the bootastaped CI is: ", round(lower,2), " - ", round(upper,2), " ILS")


# עכשיו נציג את האיור
hist_plot <- ggplot(data.frame(sampled_means = sampled_means), aes(x = sampled_means)) + geom_histogram(binwidth = 0.001) +
  geom_vline(xintercept = acc_mean, color = "red", linetype = "dashed") +
  geom_vline(xintercept = c(lower, upper), color = "green") +
  annotate("text", x = acc_mean, y = 10, label = "Sample Median", color = "red") +
  annotate("text",x = lower, y = 10, label = "Lower CI", color = "green") +
  annotate("text",x = upper, y = 10, label = "Upper CI", color = "green")

hist_plot

# Q2
cor.test(sub_stroop$mean_acc, sub_stroop$mean_rt)

x <- sub_stroop$mean_acc
y <- sub_stroop$session_id

corr_xy = cor(x, y)
#  וקטור התפלגות האפס
H0_dist <- numeric(n_iter)

# לולאת הפרמוטציות
for(i in 1:n_iter){
  # ניצור העתק של שני הוקטורים המקוריים
  # אחד ישמור על צורתו המקורית
  temp_x <- x
  # ולשני נשנה את הסדר
  temp_y <- sample(y,  replace = FALSE)
  # נאכסן את הקורלציה בין שני הוקטורים
  H0_dist[i] <- cor(temp_x, temp_y)
}
# נחשב את ההסתברות לקבל את ההקורלציה במדגם המקורי תחת השערת האפס
p <- sum(H0_dist >= corr_xy)/length(H0_dist)
p

# עכשיו נציג את התוצאה
hist_plot<-ggplot(data.frame(H0_dist = H0_dist), aes(x = H0_dist)) + geom_histogram(bins = 15) +
  geom_vline(xintercept = corr_xy, color = "red", linetype = "dashed") +
  annotate("text", x = corr_xy, y = 700, label = "Sample mean", color = "red")

hist_plot

# Q3

# calculate new columns
stroop_data = stroop_data %>% mutate(correct = stroop_data$response == stroop_data$color,
                                     congruent = stroop_data$text == stroop_data$color)
stroop_data$congruent = as.factor(stroop_data$congruent)
levels(stroop_data$congruent) = c("Incongruent", "Congruent")
stroop_data$correct = as.factor(stroop_data$correct)

new_stroop_data = filter(stroop_data, stroop_data$correct==TRUE & stroop_data$response_time<=1500)

# step 2
sum_stroop_data =new_stroop_data %>% group_by(session_id, congruent) %>% summarise(mean_rt =  mean(response_time))

# step 3
wide_stroop_data <- sum_stroop_data %>%
  pivot_wider(
    names_from = congruent, 
    values_from = mean_rt, 
  )

diff <- wide_stroop_data$Incongruent - wide_stroop_data$Congruent

H0_dist <- numeric(n_iter)

for(i in 1:n_iter){
  # ניצור העתק של וקטור ההפרשים
  temp_diff <- diff
  # נדגום מקרית וקטור שמכיל 1 ו-1
  # את הדגימה נבצע מתוך וקטור שמכיל רק 1 ו-1
  # נדגום כמות ערכים כאורך הוקטור המקורי
  # נצטרך דגימה עם החזרה כיוון שאנחנו דוגמים מוקטור שיש בו שני ערכים
  temp <- sample(c(-1,1), length(diff), replace = TRUE)
  # נכפול בין הוקטורים
  temp_diff = temp_diff*temp
  # נאכסן את ממוצע ההפרשים
  H0_dist[i] <- mean(temp_diff)
}
# נחשב את ההסתברות לקבל את המדגם המקורי תחת השערת האפס
p <- sum(H0_dist >= mean(diff))/length(H0_dist)
p

# עכשיו נציג את התוצאה
hist_plot<-ggplot(data.frame(H0_dist = H0_dist), aes(x = H0_dist)) + geom_histogram(bins = 100) +
  geom_vline(xintercept = mean(diff), color = "red", linetype = "dashed") +
  annotate("text", x = mean(diff), y = 10, label = "Sample mean", color = "red")

hist_plot
