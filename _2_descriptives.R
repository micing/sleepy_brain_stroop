library(tidyverse)
library(clipr)
load("./data/datasets/sleepy_brain_stroop_data.RDta")

##### Functions #####

tableit=function(d) {
  group_by(d, id, sleepy, emotional, congruent, update) %>% 
  summarize(subj_mean_rt=mean(rt, na.rm=T), 
            subj_mean_rt_delta=mean(rt_delta, na.rm=T), 
            subj_mean_rt_log=mean(rt_log, na.rm=T),
            subj_mean_rt_log_delta=mean(rt_log_delta, na.rm=T),
            subj_sd_rt_delta=sd(rt_delta, na.rm=T),
            subj_sd_rt_log_delta=sd(rt_log_delta, na.rm=T)) %>% 
  group_by(sleepy, emotional, congruent, update) %>% 
  summarize(mean_rt=mean(subj_mean_rt, na.rm=T), 
            mean_rt_delta=mean(subj_mean_rt_delta, na.rm=T), 
            mean_rt_log=mean(subj_mean_rt_log, na.rm=T), 
            mean_rt_log_delta=mean(subj_mean_rt_log_delta, na.rm=T),
            mean_sd_rt_delta=mean(subj_sd_rt_delta, na.rm=T),
            mean_sd_rt_log_delta=mean(subj_sd_rt_log_delta, na.rm=T),
            sd_rt=sd(subj_mean_rt, na.rm=T), 
            sd_rt_delta=sd(subj_mean_rt_delta, na.rm=T), 
            sd_rt_log=sd(subj_mean_rt_log, na.rm=T), 
            sd_rt_log_delta=sd(subj_mean_rt_log_delta, na.rm=T),
            sd_sd_rt_delta=sd(subj_sd_rt_delta, na.rm=T),
            sd_sd_rt_log_delta=sd(subj_sd_rt_log_delta, na.rm=T))
}

t=tableit(data)
write_clip(t)
print(t)
