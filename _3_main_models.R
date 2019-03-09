library(tidyverse)
library(lme4)
# library(nlme)
# library(lmerTest)
# library(pbkrtest)
#library(car)
source("functions.R")

random_formula = re_formula[["rc"]][["3"]]

#### Read data ####
load("./data/datasets/sleepy_brain_stroop_data.RDta")
data_sd=group_by(data, id, sleepy, emotional, congruent, update) %>% 
  summarize(sd_rt_delta = sd(rt_delta, na.rm=T), sd_rt_log_delta=sd(rt_log_delta, na.rm=T))

#### Fit main models (takes some time to complete) ####
models = list(rt=list(), error=list(), rt_delta=list(), rt_log=list(), rt_log_delta=list(), sd_rt_delta=list(), sd_rt_log_delta=list())

for (n in names(fe_formula)) {
    print(paste0("Fitting: ", fe_formula[[n]]))
    models$rt[n] = glmer(paste0("rt", fe_formula[[n]], random_formula), data=data, family=Gamma(link="log"))
    models$error[n] = glmer(paste0("error", fe_formula[[n]], random_formula), data=data, family=binomial)
    models$rt_delta[n] = lmer(paste0("rt_delta", fe_formula[[n]], random_formula), data=data, REML=F)
    models$rt_log[n] = lmer(paste0("rt_log", fe_formula[[n]], random_formula), data=data, REML=F)
    models$rt_log_delta[n] = lmer(paste0("rt_log_delta", fe_formula[[n]], random_formula), data=data, REML=F)
    models$sd_rt_delta[n] = lmer(paste0("sd_rt_delta", fe_formula[[n]], random_formula), data=data_sd, REML=F)
    models$sd_rt_log_delta[n] = lmer(paste0("sd_rt_log_delta", fe_formula[[n]], random_formula), data=data_sd, REML=F)
}

save(models, file="sleepy_brain_stroop_main_models.RDta")

