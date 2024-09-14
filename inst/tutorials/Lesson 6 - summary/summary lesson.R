

## מבוא


## חלק ראשון - חישוב ציוני השאלון

### ייבוא הטבלה

Qs <- read_csv("inst/tutorials/datasets/stroop + Qs/Qs edited.csv")

### חישוב ממוצע השאלון

# הפכו את ציוני העמודות ההפוכות

# חשבו את הציון הממוצע עבור כל נבדק

Qs = Qs %>% mutate()

### סינון תצפיות חריגות ובעייתיות

# סננו נבדקים בעלי 

### סיכומים תיאוריים

## חלק שני - עיבוד תוצאות המטלה

## ייבוא 

stroop_data <- read_excel("inst/tutorials/datasets/stroop + Qs/stroop_data.xlsx")


### עריכת נתונים

# ייצרו 2 עמודות חדשות

# correct
# עמודה זו תקודד האם הנבדק השיב נכונה, כלומר - האם תגובת הנבדק תואמת ל*צבע* הגירוי

# condition
# עמודה זו תקודד את תנאי הניסוי - האם הטקסט תאם את צבע הגירוי
# אם הטקסט תואם את הצבע העמודה תכיל את הערך - "congruent"
# ואם היא לא תואמת, העמודה תכיל את הערך - "incogruent"

stroop_data = stroop_data %>% 
  mutate(condition = ifelse(text == color,
                            "congruent","incogruent"))

### סינון תצפיות בעייתיות

# היסטוגרמת זמני תגובה 

ggplot(stroop_data, aes(x = response_time))+
  geom_histogram()+
  xlim(lims = c(0,5000))

# מהו לדעתכם הרף העליון הסביר לזמני התגובה?
# סננו את הטבלה כך שתכיל רק זמני תגובה סבירים
# כמה תצפיות סיננתם?

stroop_data_filtered = stroop_data %>% filter(response_time<5000)

nrow(stroop_data) - nrow(stroop_data_filtered)

# האם הקובץ מכיל ערכים חסרים?

stroop_data_filtered$response_time %>% summary() # לא


### סיכום הנתונים

# סכמו את הנתונים עבור כל נבדק
# חשבו את אחוז הסבבים בהם הוא ענה נכונה, 
# ואת מספר הסבבים בה הוצג לו גירוי בו הצבע תואם את הטקסט

stroop_general_summary = stroop_data_filtered %>%
  group_by(session_id) %>%
  summarise(correct_percent = mean(correct),
          n_congruent     = sum(condition == "congruent"))

# כעת סכמו את הנתונים עבור כל תנאי, עבור כל נבדק
# הסירו את הסבבים בהם הנבדק טעה
# ואז סכמו את ממוצע זמן התגובה

stroop_summary = stroop_data_filtered%>%
  filter( correct == 1) %>%
  group_by(session_id,condition) %>%
  summarise(mean_RT = mean(response_time))


# פרסו את הטבלה כך שעבור כל נבדק תוקדש שורה אחת
# הטבלה צריכה להכיל עמודה אחת עבור זמן התגובה הממוצע בתצפיות תואמות
# ועמודה נוספת עבור זמן התגובה הממוצע בתצפיות שאינן תואמות

stroop_subj_summary = stroop_summary %>%
  spread(key = "condition", value = "mean_RT")

# הוסיפו לטבלה עמודה ובה ההפרש בממוצע זמני התגובה בין תצפיות תואמות לבלתי תואמות

stroop_subj_summary  = stroop_subj_summary  %>%
  mutate(diff = congruent - incogruent)

# צרו היסטוגרמה עבור ההפרשים והוסיפו קו אנכי בערך 0
# מהתרשמותכם - האם זה נראה שנבדקים נוטים להגיב לאט יותר באחד התנאים?

ggplot(stroop_subj_summary,aes(x = diff))+
  geom_histogram()+
  geom_vline(xintercept =0, linetype = 2)

# חשבו את ממוצע ההפרשים עבור כל הנבדקים יחד ואת 
# שימו לב לבטל את החלוקה לקבוצות

stroop_mean_effect = stroop_subj_summary %>%
  group_by() %>%
  summarise(mean_diff = mean(diff),
            CI_interval = t.test(diff)$conf.int %>% diff())

# צרו תרשים עם עמודה + קוי טעות עבור הסיכום שיצרתם

ggplot(stroop_mean_effect, aes(y = mean_diff))+
  geom_col(x = "1")+
  geom_errorbar(aes()) # ***

## שילוב הנתונים

### הפקת תרשימים

### שמירה

## שאלות לסיכום

