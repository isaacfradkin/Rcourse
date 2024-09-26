
#### עיבוד נתוני מטלת הסטרופ ####


## ייבוא 

stroop_data <- read_csv("csv files/stroop_data.csv")


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
  xlim(lims = c(0,5000))

# מהו לדעתכם הרף העליון הסביר לזמני התגובה?
# סננו את הטבלה כך שתכיל רק זמני תגובה הקטנים מ3000 מילי שניות
# כמה תצפיות סיננתם?

stroop_data_filtered = stroop_data %>% filter(response_time<3000)

nrow(stroop_data) - nrow(stroop_data_filtered)

# בנוסף, סננו מהטבלה גם את כל הסבבים בהם הנבדקים טעו

stroop_data_filtered = stroop_data_filtered %>% filter(correct)


# האם הקובץ מכיל ערכים חסרים?

stroop_data_filtered$response_time %>% summary() # לא



### סיכום הנתונים


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
  mutate(diff = congruent - incogruent,
         summ = mean(congruent ,incogruent))

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



## חלק ראשון - חישוב ציוני השאלון

### ייבוא הטבלה

Qs <- read_csv("inst/tutorials/datasets/stroop + Qs/Qs edited.csv")

### חישוב ממוצע השאלון

# הפכו את ציוני העמודות ההפוכות

# חשבו את הציון הממוצע עבור כל נבדק

Qs = Qs %>% mutate(attention_check = ifelse(grepl("read",attentioncorrect),1,0) )

intr = Qs %>% filter(attention_check == 1) %>%
  select(contains("nfc"), session_id, age)

intr = intr %>% mutate(intr = nfc_01 - nfc_02r - nfc_03r + nfc_04+ nfc_05- nfc_06r) %>%
  filter(intr<50)
  
int_subj = intr %>% select(session_id,intr, age) 

ggplot(intr,aes(x = intr))+ geom_histogram()

names(Qs)


dff = full_join(stroop_subj_summary,int_subj )
dff = full_join(dff,first_summary) %>%
  drop_na() %>% filter(as.numeric(age)<50)

ggplot(dff, aes(x = correct_percent, y = summ))+
  geom_point()+
  geom_smooth(method  = "lm")


cor(dff$intr, dff$diff)


psych::alpha (Qs %>% filter(attention_check == 1) %>%
  select(contains("intr")),check.keys=TRUE )
### סינון תצפיות חריגות ובעייתיות

# סננו נבדקים בעלי 

### סיכומים תיאוריים

## חלק שני - עיבוד תוצאות המטלה



### עריכת נתונים


## שילוב הנתונים

### הפקת תרשימים

### שמירה

## שאלות לסיכום

