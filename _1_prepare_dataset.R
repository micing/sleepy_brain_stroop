library(tidyverse)
library(readxl)

path1 = "./data/design/"
path2 = "./data/task-stroop_beh/"
path3 = "./data/datasets/"

condition_file = "Stroop decoding GN 2019-02-14.xlsx"
randomization_file = "randomizationlist_sleepybrain2pilots.csv"
randomization_file2 = "randomization_list.csv"
dataset_file = "sleepy_brain_stroop_data"

min_trial=43 # remove initial training sessions
early_response = 4000
late_response = 20000

c=read_excel(paste0(path1, condition_file)) %>% rename_all(tolower) %>% 
  rename(facit="correct response", code="acronym", condition_txt = "condition") %>% 
  mutate(congruent = (type == "Congruent")*1)
r=read_csv2(paste0(path1, randomization_file)) %>% 
  rename(randomization="order_1fullsleepfirst_2sleepdeprivedfirst") %>%
  bind_rows(read_csv(paste0(path1, randomization_file2)) %>% 
              separate(id, c("stub", "id"), sep="_", convert=TRUE) %>%
              mutate(randomization = case_when(first_condition == "fullsleep" ~ 1, 
                                               first_condition == "sleepdeprived" ~ 2)) %>%
              select(id, randomization)) %>% arrange(id)

data=tibble()
for (f in list.files(path2)) {
  d=read_tsv(paste0(path2, f)) %>% rename_all(tolower) %>% rename(event="event type") %>% 
    left_join(c, by="code") %>%
    mutate(id=as.numeric(substr(f,6,9)), 
           session=as.numeric(substr(f,15,15)),
           code_lag=lag(code),
           valid=lead(event)=="Response",
           response_time=replace(lead(ttime), valid==FALSE, NA),
           response=replace(lead(code), valid==FALSE, NA),
           correct=replace((response==facit)*1, valid==FALSE, NA)) %>% 
    left_join(r, by="id") %>% 
    mutate(sleepy=(session==1 & randomization==2 | session==2 & randomization==1)*1) %>%
    filter(event=="Picture") %>% 
    mutate(condition=code) %>%
    filter(condition != "fix1") %>%
    mutate(emotion=factor(substr(condition,1,1), levels=c("N", "F", "H"), labels=c("Neutral","Fear", "Happy")), 
           emotional=(emotion != "Neutral")*1, 
           woman=(substr(condition, 2, 2)=="W")*1, 
           b=(substr(condition, 3, 3)=="B")*1) %>%
    arrange(time) %>%
    mutate(early = (!is.na(response_time) & response_time < early_response)*1,
           late  = (is.na(response_time) | response_time > late_response)*1,
           error = (is.na(response_time) | is.na(response) | !correct | early | late )*1,
           response_time_clean = replace(response_time, error==TRUE, NA)) %>%
    mutate(condition_prev=lag(condition),
           emotional_prev=lag(emotional),
           congruent_prev=lag(congruent),
           response_time_prev=lag(response_time),
           correct_prev=lag(correct), 
           early_prev=lag(early),
           late_prev=lag(late),
           error_prev=lag(error),
           response_time_clean_prev=lag(response_time_clean)) %>%
    mutate(response_time_delta = response_time_clean-response_time_clean_prev) %>%
    mutate(update=(congruent==congruent_prev)*1) %>%
    filter(condition %in% c$code & code_lag=="fix1" & trial >= min_trial) %>%
    mutate(rt=response_time_clean, 
           rt_prev=response_time_clean_prev,
           rt_delta=response_time_delta, 
           rt_log=log(rt), 
           rt_log_prev=log(rt_prev),
           rt_log_delta=log(rt)-log(rt_prev)) %>%
    group_by(id, congruent, sleepy, update, emotional) %>% 
           mutate(mean_rt_log = mean(rt_log), rt_log_abs_dev = abs(rt_log - mean_rt_log)) %>%
           ungroup() %>%
    select(id, session, sleepy, trial, time, condition, emotional, congruent, update,
             response_time, correct, early, late, error, response_time_clean, response_time_delta, rt, starts_with("rt_"), ends_with("_prev"))
  data = rbind(data, d)  
}

save(data, file=paste0(path3, dataset_file, ".RDta"))






