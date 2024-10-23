# יבוא חבילות
library(tidyverse)
library(ggplot2)
library(readr)



#### עיבוד נתוני מטלת הסטרופ ####


## ייבוא 

stroop_data <- read_csv("csv files/stroop_data.csv")


# תחילה -הדפיסו את צפו בשורות הראשונות בטבלה כדי להבין אילו עמודות קיימות בטבלה ואילו ערכים נמצאים בהן.

head(stroop_data)


# צרו 2 עמודות חדשות:

# correct
# עמודה זו תקודד האם הנבדק השיב נכונה, כלומר - האם תגובת הנבדק תואמת ל*צבע* הגירוי

# condition
# עמודה זו תקודד את תנאי הניסוי - האם הטקסט תאם את צבע הגירוי
# אם הטקסט תואם את הצבע העמודה תכיל את הערך - "congruent"
# ואם היא לא תואמת, העמודה תכיל את הערך - "incogruent"

stroop_data = stroop_data %>% 
  mutate(condition = ifelse(text == color,
                            "congruent","incogruent"),
         correct = response == color)


# סיכום ראשוני

first_summary = stroop_data %>% group_by(session_id) %>%
  summarise(trials = n(),
            מ_congruent = sum(ifelse(condition == "congruent",1,0)),
            correct_percent = mean(ifelse(correct,1,0)))



### סינון תצפיות בעייתיות

# היסטוגרמת זמני תגובה 

ggplot(stroop_data, aes(x = response_time))+
  geom_histogram()+
  xlim(lims = c(0,5000)) # תוספת לצמצום הטווח בציר האיקס

# מהו לדעתכם הרף העליון הסביר לזמני התגובה?
# סננו את הטבלה כך שתכיל רק זמני תגובה הקטנים מ3000 מילי שניות
# כמה תצפיות סיננתם?

stroop_data_filtered = stroop_data %>% filter(response_time<3000)

nrow(stroop_data) - nrow(stroop_data_filtered)

# בנוסף, סננו מהטבלה גם את כל הסבבים בהם הנבדקים טעו

stroop_data_filtered = stroop_data_filtered %>% filter(correct)


# האם הקובץ מכיל ערכים חסרים?

stroop_data_filtered$response_time %>% summary() # לא


# סינון נבדקים עם אחוזי הצלחה נמוכים מדי
rejected_subjects = first_summary  %>% filter(correct_percent<0.5) %>%
  pull(session_id)

stroop_data_filtered = stroop_data_filtered %>%
  filter(!session_id %in% rejected_subjects)

nrow(stroop_data_filtered)

### סיכום הנתונים


# כעת סכמו את ממוצע זמן התגובה עבור כל תנאי, עבור כל נבדק

stroop_summary = stroop_data_filtered%>%
  filter( correct == 1) %>%
  group_by(session_id,condition) %>%
  summarise(mean_RT = mean(response_time))


stroop_summary_all = stroop_data_filtered%>%
  filter( correct == 1) %>%
  group_by(session_id) %>%
  summarise(mean_RT = mean(response_time))


# פרסו את הטבלה כך שעבור כל נבדק תוקדש שורה אחת
# הטבלה צריכה להכיל עמודה אחת עבור זמן התגובה הממוצע בתצפיות תואמות
# ועמודה נוספת עבור זמן התגובה הממוצע בתצפיות שאינן תואמות

stroop_subj_summary = stroop_summary %>%
  spread(key = "condition", value = "mean_RT")

# הוסיפו לטבלה עמודה ובה ההפרש בממוצע זמני התגובה בין תצפיות תואמות לבלתי תואמות

stroop_subj_summary  = stroop_subj_summary  %>%
  mutate(diff = congruent - incogruent,
         summ = mean(congruent ,incogruent))

# צרו היסטוגרמה עבור ההפרשים והוסיפו קו אנכי בערך 0
# מהתרשמותכם - האם זה נראה שנבדקים נוטים להגיב לאט יותר באחד התנאים?

ggplot(stroop_subj_summary,aes(x = diff))+
  geom_histogram(fill = "lightblue")+
  geom_vline(xintercept =0, linetype = 2)+
  theme_minimal()+
  xlab("Response time difference (congruent - incogruent)")+
  ggtitle("Stroop effect")+ # center
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Frequency")

  

# חשבו את ממוצע וס"ת ההפרשים

סstroop_subj_summary %>% group_by() %>% summarise(mean(diff),sd(diff))


### סיכום המטלה

#צרו טבלה אחת המסכמת את תוצאות המטלה עבור כל נבדק.
# הטבלה צריכה לכלול את המספר המזהה של הנבדק, את אחוז הטעויות שלו ואת הפרש זמני התגובה שלו בין התנאים.

stroop_subj_summary = stroop_subj_summary %>%
  left_join(first_summary) %>%
  select(session_id,correct_percent,diff)


# צרו גרף המציג את הקשר בין אחוז הטעויות לבין ההפרש בזמני התגובה
# האם נראה קשר בין המשתנים?

ggplot(stroop_subj_summary, aes(x = correct_percent, y = diff))+
  geom_point(alpha = 0.1) +
  geom_smooth(method  = "lm")




# צרו גם תרשים עמודות המציג עמודה אחת עבור זמן התגובה בתנאי התואם ועמודה נוספת עבור 
# זמן התגובה בתנאי חוסר ההתאמה. הוסיפו לכל עמודה קווי טעות
# המייצגים את *טעות* התקן

sum_by_condition = stroop_summary  %>% group_by(condition) %>%
  summarise(mean = mean(mean_RT),
            sd = sd(mean_RT))

ggplot(sum_by_condition, aes(x = condition, y = mean, col = gender))+
  geom_point()+
  geom_errorbar(aes(x = condition, ymin = mean - sd, ymax = mean + sd), width = 0.2)
  # ***

## ייבוא נתוני הנבדקים

subj1 <- read_csv("csv files/stroop_subj_data_part_1.csv")
subj2 <- read_csv("csv files/stroop_subj_data_part_2.csv")

subj1 %>% pull(session_id) %>% unique() %>% length()

x = subj1 %>% select(-age) %>%
  distinct() %>%
  drop_na() %>% 
  mutate(dummy = 1) %>%
  spread(key = "Site",value = "dummy")%>%
  mutate(across(everything(), ~replace_na(., 0)))


y = x %>% gather(key = "site", value = "dummy", -session_id) %>% drop_na()

subj = bind_rows(subj1,subj2)

stroop_subj_summary = stroop_subj_summary %>% 
  left_join(subj %>% mutate(session_id = as.numeric(session_id)), by = "session_id")


stroop_subj_summary_by_uni = stroop_subj_summary %>% group_by(Site) %>%
  summarise(mean_diff = mean(diff),
            sd_diff = sd(diff),
            mean_correct = mean(correct_percent),
            sd_correct = sd(correct_percent))

### ייבוא הטבלה


### חישוב ממוצע השאלון

# הפכו את ציוני העמודות ההפוכות

# חשבו את הציון הממוצע עבור כל נבדק

Qs = Qs %>% mutate(attention_check = ifelse(grepl("read",attentioncorrect),1,0) )
nrow(Qs)

intr = Qs %>% #filter(attention_check == 1) %>%
  mutate( nfc = nfc_01 - nfc_02r - nfc_03r + nfc_04+ nfc_05- nfc_06r,
          intr = intrinsic_01 + intrinsic_02 + intrinsic_03 + intrinsic_04 + intrinsic_05 - intrinsic_06r-
            intrinsic_07r + intrinsic_08 + intrinsic_09 + intrinsic_10 + intrinsic_11 + intrinsic_12+
            intrinsic_13 + intrinsic_14 +intrinsic_15,
          age = as.numeric(age),
          gender = ifelse(gender=="1","1",ifelse(gender=="2","2",NA))
          ) %>%
  mutate(age = ifelse(age<17,NA,
                      ifelse(age>100,NA,age))) %>%
  select(nfc,intr, session_id, age, gender)

nrow(intr)


int_subj = intr 

ggplot(intr,aes(x = intr))+ geom_histogram()

names(Qs)


dff = full_join(stroop_subj_summary,int_subj )
dff = full_join(dff,first_summary) %>%
  drop_na()


dfff = gather(dff, key = "measure", value = "value", correct_percent,diff)
ggplot(dfff , aes(x = gender, y = value))+
  geom_point(alpha = 0.2)+
  geom_smooth(method  = "lm")+
  facet_wrap(~measure, scales = "free")


cor(dff$intr, dff$diff)


ggplot(dff,aes(x= gender, y = correct_percent))+
  #  geom_violin()+
  geom_point(alpha = 0.1)

psych::alpha (Qs %>% filter(attention_check == 1) %>%
                select(contains("intr")),check.keys=TRUE )
### סינון תצפיות חריגות ובעייתיות

# סננו נבדקים בעלי 

### סיכומים תיאוריים

## חלק שני - עיבוד תוצאות המטלה

by_gender = dff %>% group_by(gender) %>%
  summarise(mean_diff = mean(diff),
            sd_diff = sd(diff),
            mean_correct = mean(correct_percent),
            sd_correct = sd(correct_percent))

ggplot(by_gender, aes(x = gender, y = mean_diff))+
  geom_point()+
  geom_errorbar(aes(ymin = mean_diff - sd_diff, ymax = mean_diff + sd_diff), width =0.2)


ggplot(by_gender,aes(x = gender, y = mean_correct))+
  geom_point()+
  geom_errorbar(aes(ymin = mean_correct - sd_correct, ymax = mean_correct + sd_correct), width =0.2)


### עריכת נתונים


## שילוב הנתונים

### הפקת תרשימים

### שמירה

## שאלות לסיכום

subj_data <- read_csv("csv files/stroop_subject_info.csv")


stroop_summary_all = stroop_summary_all %>%
  left_join(subj_data %>% mutate(session_id = as.numeric(session_id),
                                 age = as.numeric(age)))

ggplot(stroop_summary_all, aes(x = Site, y = mean_RT))+
  # geom_point()+
  stat_summary()+
  geom_smooth(method = "lm")

names(stroop_summary_all)


dff  = dff %>%
  left_join(subj_data %>% mutate(session_id = as.numeric(session_id),
                                 age = as.numeric(age)))

ggplot(dff %>% group_by(Site) %>% summarise(diff = mean(diff)), aes(x = Site, y = diff))+
  geom_col()




library(ggplot2)
library(dplyr)

# Step 1: Summarize data to calculate mean and SD for each condition
stroop_summary <- stroop_data %>%
  group_by(condition) %>%
  summarise(
    mean_response_time = mean(response_time, na.rm = TRUE),
    sd_response_time = sd(response_time, na.rm = TRUE)
  )

# Step 2: Create the bar plot with error bars
ggplot(stroop_summary, aes(x = condition, y = mean_response_time)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.6) +  # Bar plot
  geom_errorbar(aes(ymin = mean_response_time - sd_response_time, 
                    ymax = mean_response_time + sd_response_time), 
                width = 0.2) +  # Error bars
  labs(x = "Condition", y = "Mean Response Time (ms)", title = "Mean Response Time by Condition") +
  theme_minimal()
