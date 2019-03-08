#### Test hypotheses ####

load("sleepy_brain_stroop_main_models.RDta")
m=models

# Any effect of sleep deprivation

# reduced model (only main effect)
anova(m$rt$s, m$rt$null)
anova(m$error$s, m$error$null)
anova(m$rt_delta$s, m$rt_delta$null)
anova(m$rt_log$s, m$rt_log$null)
anova(m$rt_log_delta$s, m$rt_log_delta$null)
anova(m$sd_rt_delta$s, m$sd_rt_delta$null)
anova(m$sd_rt_log_delta$s, m$sd_rt_log_delta$null)

# congreunce model (incl interaction)
anova(m$rt$cs, m$rt$c)
anova(m$error$cs, m$error$c)
anova(m$rt_delta$cs, m$rt_delta$c)
anova(m$rt_log$cs, m$rt_log$c)
anova(m$rt_log_delta$cs, m$rt_log_delta$c)
anova(m$sd_rt_delta$cs, m$sd_rt_delta$c)
anova(m$sd_rt_log_delta$cs, m$sd_rt_log_delta$c)

# emotional model (incl interaction)
anova(m$rt$es, m$rt$e)
anova(m$error$es, m$error$e)
anova(m$rt_delta$es, m$rt_delta$e)
anova(m$rt_log$es, m$rt_log$e)
anova(m$rt_log_delta$es, m$rt_log_delta$e)
anova(m$sd_rt_delta$es, m$sd_rt_delta$e)
anova(m$sd_rt_log_delta$es, m$sd_rt_log_delta$e)

# update/conflict models (incl interaction)
anova(m$rt$su, m$rt$u)
anova(m$error$su, m$error$u)
anova(m$rt_delta$su, m$rt_delta$u)
anova(m$rt_log$su, m$rt_log$u)
anova(m$rt_log_delta$su, m$rt_log_delta$u)
anova(m$sd_rt_delta$su, m$sd_rt_delta$u)
anova(m$sd_rt_log_delta$su, m$sd_rt_log_delta$u)

# full model (incl all interactions)
anova(m$rt$ceu, m$rt$cesu)
anova(m$error$ceu, m$error$cesu)
anova(m$rt_delta$ceu, m$rt_delta$cesu)
anova(m$rt_log$ceu, m$rt_log$cesu)
anova(m$rt_log_delta$ceu, m$rt_log_delta$cesu)
anova(m$sd_rt_delta$ceu, m$sd_rt_delta$cesu)
anova(m$sd_rt_log_delta$ceu, m$sd_rt_log_delta$cesu)




