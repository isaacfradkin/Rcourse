library(tidyverse)
library(readxl)


#### prepare stroop data #####

stroop_data <- read_excel("inst/tutorials/datasets/stroop + Qs/stroop_data.xlsx")

stroop_sessions = stroop_data %>% select(session_id) %>% distinct() %>% pull(session_id)

stroop_summ = stroop_data %>% group_by(session_id) %>%
  summarise(correct_percent = mean(correct,na.rm = T),
            RT_congruent = mean(RT,Congruent))



Qs <- read_csv("inst/tutorials/datasets/stroop + Qs/Qs edited.csv")

nfc = Qs %>% filter(!is.na(session_id)) %>%
   mutate(attention_check = ifelse(grepl("read",attentioncorrect),1,0)  ,
          age = as.numeric(age),
          gender = ifelse(gender ==1, "female","male")
          ) %>%
  select(session_id,age,gender,attention_check,contains("nfc"))%>%
  filter(session_id %in% stroop_sessions)
  

write.csv(nfc, "datasets/stroop + Qs/need_for_cognition.csv")

usethis::use_data(stroop_data, overwrite = TRUE)

joined = left_join(nfc,stroop_summ, by ="session_id")
