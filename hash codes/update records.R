library(readr)
library(dplyr)
source("checker functions.R")

##### Load records ####
if (FALSE){
IDs <- read_csv("hash codes/IDs.csv")

records = IDs 
names(records) = c("name","family name","id","email","group")
records  = records %>% mutate(group = group %>% substr(55, 56) %>%as.numeric() )

write_csv(records, "hash codes/records.csv")
} # create new records df

records = read_csv("hash codes/records.csv")

##### Update by lesson #####

records = check_lesson(records, 2, L2_checker)

records$L2 %>% table()





