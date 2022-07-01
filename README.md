# Sleepy Brain 2/3 Stroop test preliminary analysis

Michael Ingre
2022-06-28

### Introduction
This document describes the analytical process and summarises early results on stroop data from the sleepy brain project, waves 2/3.

This study has four design factors and in data they have been 0/1 coded and given names to interpret the condition indicated by 1. *Sleepy* refers to the sleep deprived condition (vs full sleep condition), *emotional* refers to stimuli with emotional content (vs neutral), *congruent* refers to stimuli that is congruent in colour and text (vs incongruent) and *update* refers to the previous stimulus and whether the current stimulus is an update of the congruent/incongruent status or if the congruence has changed. 

The congruence status of previous and current stimulus is sometimes indicated by the letters *C* and *I* in this text, for congruent stimuli with or without update (cC, iC) and similarly for incongruent stimuli (iI, cI).

In order to reduce the number of factors and interactions, *emotional* and *neutral* conditions have been analysed separately as two different tests. This leaves three factors in the main design of the study: sleepy, congruent and update.

The main hypothesis was that sleep deprivation would increase reaction time (RT) and reaction time variability (RTV) on the Stroop test. This hypothesis was modelled as a main effect of sleepy.

A three-way interaction (sleepy*congruent*update) was also hypothesised, where primarily RTV would increase in the sleep deprived condition after an incongruent stimuli without update (cI), but not (or to a lesser degree) after a congruent stimuli with update (cC). This was based on a previously published report <sup>1</sup>, but careful analysis of the methods used there suggests that the results did not describe a proper three-way interaction on RTV, but rather the combined variance of both cI and cC conditions. The combining of variances occurred when a difference score was calculated in raw data and subsequently used as a dependent variable in the analysis. Thus, these results are suggestive of a more general effect on RTV, in two of the four possible conditions, and since they combined the conditions with highest (cI) and lowest expected variance (cC), the observations are probably more consistent with the hypothesis of a main effect (across all conditions) of sleep deprivation on RTV presented above.

For completeness, any two-way interaction with sleep deprivation and congruence/update was also tested, and results were also reported for response errors, late responses (> 2 sec) and early responses (< .4 sec).

### Statistical analysis
Data were analysed in R (4.1.2) applying the lmer and glmer functions in the lme4 package for mixed effect models <sup>2</sup> . [Code and data can be found here](https://github.com/micing/sleepy_brain_stroop)

Reaction time (RT) was analysed using two different models. Raw RT data was fitted with a generalised linear mixed model (GMM) applying a log link and gamma error distribution, to account for the skewed distribution that is typical for reaction times. A linear mixed model (LMM) was also fitted on the logarithm of RT. 

Reaction time variability (RTV) was modelled in three different ways. Following the previous report <sup>1</sup> the standard deviation (SD) was calculated for each individual in all different conditions and used as a dependent variable in a LMM. An alternative SD was also calculated on the logarithm of RT. A third variable was calculated as the absolute deviation of each observation from the subject specific mean in each condition.

Errors, early and late responses were binary variables and modelled using binary logistic regression, applying a logit link and binomial error distribution.

A model fitting procedure was developed for likelihood ratio testing that started with fitting a baseline model with just an intercept and then adding fixed effect terms one at the time until the full three-way interaction (sleepy*congruent*update) with all main effects and two-way interactions was fitted. Random coefficients were included for all fixed effects in the model. All LMMs were fitted using Maximum Likelihood (ML) to enable accurate likelihood ratio testing.

Finally, one degree of freedom likelihood ratio tests were applied to test for the main (fixed) effect of *sleepy* and its two- and three-way interactions with *congruent* and *update*. 

### Results
For the neutral test, we observed main effects of sleep deprivation on RT and RTV but not on errors, early or late responses. No interactions were observed on the neutral test.

On the emotional test there was a main effect of sleep deprivation on errors, early and late responses. Furthermore, there were two-way interactions (sleepy*update) on early and late responses. No three way interaction was observed.

[A table summarising the likelihood ratio tests of all hypotheses can be found here.](https://github.com/micing/sleepy_brain_stroop/blob/master/tables/main_hypotheses_lr_test.tsv)

[Descriptive data can be found here.](https://github.com/micing/sleepy_brain_stroop/tree/master/figures)

### References
1.	Floros, O. et al. Vulnerability in Executive Functions to Sleep Deprivation Is Predicted by Subclinical Attention-Deficit/Hyperactivity Disorder Symptoms. Biol Psychiatry Cogn Neurosci Neuroimaging 6, 290–298 (2021).
2.	Bates, D., Mächler, M., Bolker, B. & Walker, S. Fitting Linear Mixed-Effects Models Using lme4. Journal of Statistical Software vol. 67 1–48 (2015).


