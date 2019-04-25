#### Test hypotheses ####

load("sleepy_brain_stroop_main_models.RDta")
m=models

# Any effect of sleep deprivation

# reduced model (only main effect)
anova(m$rt$s, m$rt$null)
anova(m$error$s, m$error$null)
anova(m$rt_log$s, m$rt_log$null)

# full model but only main effect of sleepiness
anova(m$rt$cu__e, m$rt$cu__es)
anova(m$error$cu__e, m$error$cu__es)
anova(m$rt_log$cu__e, m$rt_log$cu__es)

# full model (three-way interaction with sleepiness)
anova(m$rt$csu__e, m$rt$cu__es)
anova(m$error$csu__e, m$error$cu__es)
anova(m$rt_log$csu__e, m$rt_log$cu__es)





