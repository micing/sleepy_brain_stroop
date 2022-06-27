library("tidyverse")

#### functions ####

star = function(p) {
  if (p <.001) return("***")
  if (p <.01) return("**")
  if (p <.05) return("*")
  return("")
}

tidy_model = function(m, d, t, l) {
  m %>% broom::tidy() %>% mutate(model=row_number()) %>%
    select("model", "term", "AIC", "logLik", "df", "p.value") %>% 
    pivot_wider(names_from=model, values_from=term:p.value) %>%
    mutate(dv=d, label = l, test=t) %>% select(dv, test, label, everything()) %>% 
    select(-p.value_1, -df_1) %>% rename(df=df_2, p.value=p.value_2) %>%
    mutate(star = star(p.value))
}

#### Test hypotheses (likelihood ratio tests) ####

dvs = c("rt", "rt_log", "sd_rt", "sd_rt_log", "rt_log_abs_dev", "error", "late", "early")
#dvs = "rt"

lr_test= tibble()

for (dv in dvs) {
  load(paste0("sleepy_brain_stroop_main_models_", dv, ".RDta"))
  for (test in c("neutral", "emotional")) {
    m=models[[dv]][[test]]
    lr_test = lr_test %>% bind_rows(
      anova(m$cu$cu, m$cu_s$cu) %>% tidy_model(dv, test, "sleepy"),
      anova(m$cu_s$cus, m$cu_cs$cus) %>% tidy_model(dv, test, "sleepy*congruent"),
      anova(m$cu_s$cus, m$cu_us$cus) %>% tidy_model(dv, test, "sleepy*update"),
      anova(m$cu_cs_us$cus, m$cus$cus) %>% tidy_model(dv, test, "sleepy*congruent*update"))
  }
}

lr_test %>% print()
lr_test %>% write_tsv("tables/main_hypotheses_lr_test.tsv")

t = lr_test

