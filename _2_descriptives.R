library(tidyverse)

load("./data/datasets/sleepy_brain_stroop_data.RDta")

height=150
width= 180

##### Functions #####

subj_summary = function(d) {
  d %>% group_by(id, sleepy, emotional, congruent, update) %>% 
  summarize(subj_mean_rt=mean(rt, na.rm=T), 
            subj_mean_rt_log=mean(rt_log, na.rm=T),
            subj_mean_rt_log_abs_dev=mean(rt_log_abs_dev, na.rm=T),
            subj_mean_error=mean(error, na.rm=T),
            subj_mean_late=mean(late, na.rm=T),
            subj_mean_early=mean(early, na.rm=T),
            subj_sd_rt=sd(rt, na.rm=T),
            subj_sd_rt_log=sd(rt_log, na.rm=T),
            subj_sd_rt_log_abs_dev=sd(rt_log_abs_dev, na.rm=T),
            subj_sd_error=sd(error, na.rm=T),
            subj_sd_late=sd(late, na.rm=T),
            subj_mean_early=mean(early, na.rm=T)) 
}

cond_summary = function(d) {
  d %>% group_by(sleepy, emotional, congruent, update) %>% 
  summarize(mean_rt=mean(subj_mean_rt, na.rm=T), 
            mean_rt_log=mean(subj_mean_rt_log, na.rm=T), 
            mean_rt_log_abs_dev=mean(subj_mean_rt_log_abs_dev, na.rm=T),
            mean_sd_rt=mean(subj_sd_rt, na.rm=T),
            mean_sd_rt_log=mean(subj_sd_rt_log, na.rm=T),
            mean_error=mean(subj_mean_error, na.rm=T), 
            mean_late=mean(subj_mean_late, na.rm=T), 
            mean_early=mean(subj_mean_early, na.rm=T),
            sd_rt=sd(subj_mean_rt, na.rm=T), 
            sd_rt_log=sd(subj_mean_rt_log, na.rm=T), 
            sd_sd_rt=sd(subj_sd_rt, na.rm=T),
            sd_sd_rt_log=sd(subj_sd_rt_log, na.rm=T),
            sd_sd_rt_log_abs_dev=sd(subj_sd_rt_log_abs_dev, na.rm=T),
            sd_error=sd(subj_mean_error, na.rm=T),
            sd_late=sd(subj_mean_late, na.rm=T),
            sd_early=sd(subj_mean_early, na.rm=T),
            se_rt=sd(subj_mean_rt, na.rm=T)/sqrt(n()), 
            se_rt_log=sd(subj_mean_rt_log, na.rm=T)/sqrt(n()), 
            se_rt_log_abs_dev=sd(subj_mean_rt_log_abs_dev, na.rm=T)/sqrt(n()),
            se_sd_rt=sd(subj_sd_rt, na.rm=T)/sqrt(n()),
            se_sd_rt_log=sd(subj_sd_rt_log, na.rm=T)/sqrt(n()),
            se_error=sd(subj_mean_error, na.rm=T)/sqrt(n()),
            se_late=sd(subj_mean_late, na.rm=T)/sqrt(n()),
            se_early=sd(subj_mean_early, na.rm=T)/sqrt(n()))
}


#### print data ####

s = data %>% subj_summary
c = s  %>% cond_summary

print(c)

#### plot data ####

dodge <- position_dodge(width = 0.9)
d = data %>% subj_summary %>% cond_summary %>% 
  mutate(emotional = factor(emotional, c(0,1), c("Neutral", "Emotional")),
         congruent = factor(congruent, c(0,1), c("I", "C")),
         update = factor(update, c(0,1), c("-", "+")),
         sleepy = factor(sleepy, c(0,1), c("Full sleep", "Sleep deprived"))
         )

p = d %>% ggplot(aes(x = interaction(congruent, update), y = mean_rt, fill = factor(sleepy))) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = mean_rt + se_rt, ymin = mean_rt - se_rt), position = dodge, width = 0.2) +
  theme_minimal() +
  facet_wrap(~emotional) +
  ggtitle("Mean reaction time (RT)")
plot(p)
ggsave("figures/rt.png", width=width, height=height, units="mm")

c=8.6
p = d %>% ggplot(aes(x = interaction(congruent, update), y = mean_rt_log - c, fill = factor(sleepy))) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = mean_rt_log - c + se_rt_log, ymin = mean_rt_log - c - se_rt_log), position = dodge, width = 0.2) +
  theme_minimal() +
  facet_wrap(~emotional) +
  ggtitle(paste0("Mean (log) reaction time (RT) + constant = ", c))
plot(p)
ggsave("figures/rt_log.png", width=width, height=height, units="mm")


p = d %>% ggplot(aes(x = interaction(congruent, update), y = mean_sd_rt - c, fill = factor(sleepy))) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = mean_sd_rt + se_sd_rt, ymin = mean_sd_rt - se_sd_rt), position = dodge, width = 0.2) +
  theme_minimal() +
  facet_wrap(~emotional) +
  ggtitle("Reaction time variability (SD of RT)")
plot(p)
ggsave("figures/sd_rt.png", width=width, height=height, units="mm")

c=0.2
p = d %>% ggplot(aes(x = interaction(congruent, update), y = mean_sd_rt_log - c, fill = factor(sleepy))) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = mean_sd_rt_log - c + se_sd_rt_log, ymin = mean_sd_rt_log - c - se_sd_rt_log), position = dodge, width = 0.2) +
  theme_minimal() +
  facet_wrap(~emotional) +
  ggtitle(paste0("Reaction time variability (SD of log RT) + constant = ", c))
plot(p)
ggsave("figures/sd_rt_log.png", width=width, height=height, units="mm")

c=0.15
p = d %>% ggplot(aes(x = interaction(congruent, update), y = mean_rt_log_abs_dev - c, fill = factor(sleepy))) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = mean_rt_log_abs_dev - c + se_rt_log_abs_dev, ymin = mean_rt_log_abs_dev - c - se_rt_log_abs_dev), position = dodge, width = 0.2) +
  theme_minimal() +
  facet_wrap(~emotional) +
  ggtitle(paste0("Reaction time variability + constant = ", c, "\n(absolute deviation from mean log RT)"))
plot(p)
ggsave("figures/rt_log_abs_dev.png", width=width, height=height, units="mm")

p = d %>% ggplot(aes(x = interaction(congruent, update), y = mean_error, fill = factor(sleepy))) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax =  mean_error + se_error, ymin = mean_error - se_error), position = dodge, width = 0.2) +
  theme_minimal() +
  facet_wrap(~emotional) +
  ggtitle("Proportion wrong responses")
plot(p)
ggsave("figures/error.png", width=width, height=height, units="mm")

p = d %>% ggplot(aes(x = interaction(congruent, update), y = mean_late, fill = factor(sleepy))) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax =  mean_late + se_late, ymin = mean_late - se_late), position = dodge, width = 0.2) +
  theme_minimal() +
  facet_wrap(~emotional) +
  ggtitle("Proportion late (>20000) responses")
plot(p)
ggsave("figures/late.png", width=width, height=height, units="mm")

p = d %>% ggplot(aes(x = interaction(congruent, update), y = mean_early, fill = factor(sleepy))) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax =  mean_early + se_early, ymin = mean_early - se_early), position = dodge, width = 0.2) +
  theme_minimal() +
  facet_wrap(~emotional) +
  ggtitle("Proportion early (<4000) responses")
plot(p)
ggsave("figures/early.png", width=width, height=height, units="mm")


