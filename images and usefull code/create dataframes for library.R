library(tidyverse)

wide_df = data.frame(subject = c(1,2,3,4),
                     first = c(40,34,56,71),
                     second = c(43,44,50,75))


long_df = wide_df %>% gather(key = "iteration", value = "score", - subject)


long_df2 = data.frame(family_id = c(1,1,1,2,2,2,3,3),
                      familial_role = c("mother","father","child_1","father","child_1","child_2","mother","child_1"),
                      IQ = round(rnorm(8, 100, 15)))



mood_scores = rep(runif(20,2,8), each = 3) + rnorm(60,0,2) + rep(c(-0.5,0,0.5),20)
mood = data.frame(subject = rep(1:20, each = 3),
                                 measurement_time = rep(c("T1","T2","T3"),20),
                                 mood_score = ifelse(mood_scores>10,10,
                                                      ifelse(mood_scores<0,0,mood_scores)) %>% round() )


save(wide_df, file = "data/wide_df.rda")
save(long_df, file = "data/long_df.rda")
save(long_df2, file = "data/long_df2.rda")
save(mood, file = "data/mood.rda")

write.csv(mood,"mood.csv")


write.csv(stroop_data,"stroop_data_full.csv")
write.csv(stroop_data %>% select(-correct,-congruent),"stroop_data.csv")

# stroop subj data


subj1 <- read_csv("csv files/stroop_subj_data_part_1.csv")
subj2 <- read_csv("csv files/stroop_subj_data_part_2.csv")


subj1_wide = subj1 %>% select(-age) %>%
  distinct() %>%
  drop_na() %>% 
  mutate(dummy = 1) %>%
  spread(key = "Site",value = "dummy")%>%
  mutate(across(everything(), ~replace_na(., 0)))


subj2_wide = subj2 %>% select(-age) %>%
  distinct() %>%
  drop_na() %>% 
  mutate(dummy = 1) %>%
  spread(key = "Site",value = "dummy")%>%
  mutate(across(everything(), ~replace_na(., 0)))

write.csv(subj1_wide,"subj1_wide.csv")
write.csv(subj2_wide,"subj2_wide.csv")



