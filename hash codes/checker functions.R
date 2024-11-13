library(stringr) 

check_lesson = function(records, lesson_number, checker){
  # lesson_number: number, 1-8
  # checker:       checker function
  lesson = read_csv(paste0("hash codes/L",lesson_number,".csv")) %>%
    rename(id = `מספר זיהוי`,
           hash = `תגובה 1`) %>%
    select(id, hash)
  
  lesson$pass = NA
  for (i in 1:nrow(L2)){
    hash = lesson$hash[i]
    pass = ifelse(hash == "-", NA, checker(hash))
    lesson$pass[i] = pass
  }
  
  column_name = paste0("L",lesson_number)
  records = records %>% left_join(lesson %>%
                                    select(id,pass) %>%
                                    mutate(id = as.character(id)) %>%
                                    rename(!!column_name := pass),
                                  by = "id")
}


L2_checker =  function(hash){
  
  exercises = c("ari_op_ex", "comp_op_ex", "final_ex1","final_ex2","final_ex3",
                "int_ex","log_ex","logi_op_ex","str_ex","str_question","var_question")
  
  response_table = learnrhash::decode_obj(hash)
  
  all_exist = exercises %in% response_table$label %>% all()
  all_correct = response_table %>% filter(label %in% exercises) %>%
    pull(correct) %>% all() %>% ifelse(is.na(.), F, .)
  
  pass = all_exist & all_correct
   
   return(pass)
 }

 