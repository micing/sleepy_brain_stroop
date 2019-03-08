library(tidyverse)
library(lme4)
library(MASS)
source("functions.R")

skip_re=TRUE

#### This is code for trying out random effect structures and error distributions on data ####
#### it may have problems converging without tweaking when data is updated ####

load("./data/datasets/sleepy_brain_stroop_data.RDta")
data_rt=data
data_var= group_by(data, id, sleepy, emotional, congruent) %>% 
  summarize(rt_var_lin=var(rt, na.rm=T), rt_var_gamma=var_gamma(rt), rt_sd_lin=sqrt(rt_var_lin), rt_sd_gamma=sqrt(rt_var_gamma))

#### functions ####

# plot function with starting vaules for estimating gamma distribution parameters on data
plotit = function(m, title="", gamma=FALSE, offset=.8, shape=9, scale=12) {
  den=density(residuals(m))
  df=tibble(x=den$x, y=den$y)
  
  df$gaussy=dnorm(df$x,m=mean(residuals(m)), sd=sd(residuals(m)))
  p=ggplot(data = df, aes(x = x,y = y)) + 
    scale_colour_manual(name="Residuals",
                        values=c(gamma="red", gaussian="blue", observed="black")) +
    geom_line(aes(color="observed", size=2)) +     
    geom_line(aes(x=df$x, y=gaussy, color="gaussian", size = 1)) + 
    guides(size=FALSE) +
    theme_minimal() +
    labs(title=title, y="Density")
  if (gamma) p = p + geom_line(aes(x=df$x, y=gammay, color="gamma", size = 1)) 
  plot(p)
}

#### fitting random effect structures (takes some time to complete) ####
if (!skip_re) {
  modelfit=tibble()
  for (t in names(re_formula)){
    for (i in names(re_formula[[t]])){
      print(paste0("Fitting model: ", t, i))
      #m=lmer(paste0("rt ~ sleepy*emotional*congruent", re_formula[[t]][[i]]), data=data_rt, REML=FALSE)
      #m=lmer(paste0("rt_log ~ sleepy*emotional*congruent", re_formula[[t]][[i]]), data=data_rt, REML=FALSE)
      m=glmer(paste0("rt ~ sleepy*emotional*congruent", re_formula[[t]][[i]]), data=data_rt, family=Gamma(link="log"))
      modelfit = rbind(modelfit, tibble(type=t, index=i, ll=logLik(m), AIC=AIC(m)))
    }
  }
  save(modelfit, file="modelfit.RDta")
}

#### fitting error distributions
models = list(
  rt=list(
    lmm=lmer(paste0("rt ~ sleepy*emotional*congruent", re_formula[["rc"]][["3"]]), data=data_rt, REML=F),
    lmm_log=lmer(paste0("rt_log ~ sleepy*emotional*congruent", re_formula[["rc"]][["3"]]), data=data_rt, REML=F),
    glmm_log_gamma=glmer(paste0("rt ~ sleepy*emotional*congruent", re_formula[["rc"]][["3"]]), data=data_rt, family=Gamma(link="log")),
    glmm_ident_gamma=glmer(paste0("rt ~ sleepy*emotional*congruent", re_formula[["rc"]][["3"]]), data=data_rt, family=Gamma(link="identity")),
    glmm_log_normal=glmer(paste0("rt ~ sleepy*emotional*congruent", re_formula[["rc"]][["3"]]), data=data_rt, family=gaussian(link="log")),
    glmm_ident_normal=glmer(paste0("rt ~ sleepy*emotional*congruent", re_formula[["rc"]][["3"]]), data=data_rt, family=gaussian(link="identity"))
  ),
  var_lin=list(
    lmm=lmer(paste0("rt_var_lin ~ sleepy*emotional*congruent", re_formula[["rc"]][["3"]]), data=data_var, REML=F),
    glmm_gamma=glmer(paste0("rt_var_lin ~ sleepy*emotional*congruent", re_formula[["rc"]][["3"]]), data=data_var, family=Gamma(link="log"))
    ),
  var_gamma=list(
    lmm=lmer(paste0("rt_var_gamma ~ sleepy*emotional*congruent", re_formula[["rc"]][["3"]]), data=data_var, REML=F),
    glmm_gamma=glmer(paste0("rt_var_gamma ~ sleepy*emotional*congruent", re_formula[["rc"]][["3"]]), data=data_var, family=Gamma(link="log"))
    ),
  sd_lin=list(
    lmm=lmer(paste0("rt_sd_lin ~ sleepy*emotional*congruent", re_formula[["rc"]][["3"]]), data=data_var, REML=F),
    glmm_gamma=glmer(paste0("rt_sd_lin ~ sleepy*emotional*congruent", re_formula[["rc"]][["3"]]), data=data_var, family=Gamma(link="log"))
  ),
  sd_gamma=list(
    lmm=lmer(paste0("rt_sd_gamma ~ sleepy*emotional*congruent", re_formula[["rc"]][["3"]]), data=data_var, REML=F),
    glmm_gamma=glmer(paste0("rt_sd_gamma ~ sleepy*emotional*congruent", re_formula[["rc"]][["3"]]), data=data_var, family=Gamma(link="log"))
  )
)

#### Plotting residuals ####

## RT
plotit(models$rt$lmm, "RT Residuals (LMM)")
plotit(models$rt$lmm_log, "RT Residuals (LMM log transform)")
plotit(models$rt$glmm_ident_normal, "RT Residuals (GLMM normal)")
plotit(models$rt$glmm_log_normal, "RT Residuals (GLMM log-normal)")
plotit(models$rt$glmm_log_gamma, "RT Residuals (GLMM log-gamma)")
plotit(models$rt$glmm_ident_gamma, "RT Residuals (GLMM Gamma)")

## sd of RT
plotit(models$sd_lin$lmm, "RT sd (linear) Residuals (LMM)")
plotit(models$sd_gamma$lmm, "RT sd (gamma) Residuals (LMM)")

## variance of RT
plotit(models$var_lin$lmm, "RT variance (linear) Residuals (LMM)")
plotit(models$var_gamma$lmm, "RT variance (gamma) Residuals (LMM)")






