library(tidyverse)
library(lme4)

random_formulas = list(
  null = " + (1 | id)",
  cu = " + (1 + congruent + update | id)",
  cus = " + (1 + congruent + update + sleepy | id)"
)

fixed_formulas = list(
  null = " ~ 1",
  c =  " ~ congruent",
  u =  " ~ update",
  s =  " ~ sleepy",
  c_u =  " ~ congruent + update",
  c_s =  " ~ congruent + sleepy",
  u_s =  " ~ update + sleepy",
  c_u_s = " ~ congruent + update + sleepy",
  cu =  " ~ congruent*update",
  cs =  " ~ congruent*sleepy",
  us =  " ~ update*sleepy",
  cu_s =  " ~ congruent*update + sleepy",
  cs_u =  " ~ congruent*sleepy + update",
  us_c =  " ~ update*sleepy + congruent",
  cu_cs =  " ~ congruent*update + congruent*sleepy",
  cu_us =  " ~ congruent*update + update*sleepy",
  cu_cs_us =  " ~ congruent*update + congruent*sleepy + update*sleepy",
  cus =  " ~ congruent*update*sleepy"
)

fixed_formulas = fixed_formulas[names(fixed_formulas) %in% 
                c("null", "c" , "u", "s", "cu", "cu_s", "cu_cs", "cu_us", "cu_cs_us", "cus")] 

#### functions ####

options(dplyr.summarise.inform = FALSE)

subj_summary = function(d) {
  d %>% group_by(id, sleepy, emotional, congruent, update) %>% 
    summarize(subj_mean_rt=mean(rt, na.rm=T), 
              subj_mean_rt_log=mean(rt_log, na.rm=T),
              subj_mean_error=mean(error, na.rm=T),
              subj_mean_late=mean(late, na.rm=T),
              subj_mean_early=mean(early, na.rm=T),
              subj_sd_rt=sd(rt, na.rm=T),
              subj_sd_rt_log=sd(rt_log, na.rm=T),
              subj_sd_error=sd(error, na.rm=T),
              subj_sd_late=sd(late, na.rm=T),
              subj_mean_early=mean(early, na.rm=T)) %>% ungroup()
}

compare <- function(s1, s2) {
  c1 <- unique(strsplit(gsub('_', '', s1), "")[[1]])
  c2 <- unique(strsplit(gsub('_', '', s2), "")[[1]])
  length(intersect(c1,c2))/length(c1)
}

#### Read data ####

load("./data/datasets/sleepy_brain_stroop_data.RDta")
data_sd=group_by(data, id, sleepy, emotional, congruent, update) %>% 
  summarize(sd_rt_delta = sd(rt_delta, na.rm=T), sd_rt_log_delta=sd(rt_log_delta, na.rm=T))

reduced_data = data %>% filter(update == 0 & congruent == 0 | update == 1 & congruent == 1) %>% 
  mutate(conflict = (update == 0)*1 )

#### fit models ####

models = list()

for (test in c("neutral", "emotional")) {
  if (test == "neutral") { d = data %>% filter(emotional == 0) }
  else { d = data %>% filter(emotional == 1) }
  
  for (re in names(random_formulas)) {
    re_formula = random_formulas[[re]]

    for (fe in names(fixed_formulas)) {
      fe_formula = fixed_formulas[[fe]]
      
      if (re != "null" && compare(re, fe) != 1) { next } # dont fit models with random effects that are not also included as fixed effects
      
      print(paste0("Fitting: ", test,": ", fe_formula, re_formula))

      models$rt[[test]][[fe]][[re]] = glmer(paste0("rt", fe_formula, re_formula), data=d, family=Gamma(link="log")) %>% tryCatch(error=function(e){return(NA)})
      models$rt_log[[test]][[fe]][[re]] = lmer(paste0("rt_log", fe_formula, re_formula), data=d, REML=F) %>% tryCatch(error=function(e){return(NA)})
      models$sd_rt[[test]][[fe]][[re]] = lmer(paste0("subj_sd_rt", fe_formula, re_formula), data=d %>% subj_summary, REML=F) %>% tryCatch(error=function(e){return(NA)})
      models$sd_rt_log[[test]][[fe]][[re]] = lmer(paste0("subj_sd_rt_log", fe_formula, re_formula), data=d %>% subj_summary, REML=F) %>% tryCatch(error=function(e){return(NA)})
      models$rt_log_abs_dev[[test]][[fe]][[re]] = lmer(paste0("rt_log_abs_dev", fe_formula, re_formula), data=d, REML=F) %>% tryCatch(error=function(e){return(NA)})
      models$error[[test]][[fe]][[re]] = glmer(paste0("error", fe_formula, re_formula), data=d, family=binomial) %>% tryCatch(error=function(e){return(NA)})
      models$late[[test]][[fe]][[re]] = glmer(paste0("late", fe_formula, re_formula), data=d, family=binomial) %>% tryCatch(error=function(e){return(NA)})
      models$early[[test]][[fe]][[re]] = glmer(paste0("early", fe_formula, re_formula), data=d, family=binomial) %>% tryCatch(error=function(e){return(NA)})
    }
  }
}

dvs = c("rt", "rt_log", "sd_rt", "sd_rt_log", "rt_log_abs_dev", "error", "late", "early")
m=models

for (v in dvs) {
  models = list()
  models[[v]]=m[[v]]
  save(models, file=paste0("sleepy_brain_stroop_main_models_", v, ".RDta"))
}


