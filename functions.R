#### functions ####

var_gamma = function(d) {sigma(glm(d ~ 1 , family=Gamma(link="log")))^2}

#### formulas for random effect structures ####

re_formula=list(
  rc=list(
    "0" = " + (1 | id)",
    "1" = " + (1 + sleepy + emotional + congruent || id)",
    "2"  = "+ (1 + sleepy + emotional + congruent | id)",
    "3" = " + (1 + sleepy + emotional + congruent + I(sleepy*emotional) + I(sleepy*congruent) + I(emotional*congruent) || id)",
    "4" = "+ (1 + sleepy + emotional + congruent | id) + 
    (0 + I(sleepy*emotional) + I(sleepy*congruent) + I(emotional*congruent) || id)",
    "5" = " +  (1 + sleepy + emotional + congruent + I(sleepy*emotional) + I(sleepy*congruent) + I(emotional*congruent) | id)",
    "6" = " + (1 + sleepy*emotional*congruent || id)",
    "7" = " + (1 + sleepy*emotional*congruent | id)"),
  vc=list(
    "0" = " + (1 | id)",
    "1" = " + (1 | id) + (1 | id:sleepy) + (1 | id:emotional) + (1 | id:congruent)",
    "2" = " + (1 | id) + (1 | id:sleepy) + (1 | id:emotional) + (1 | id:congruent) +
    (1 | id:sleepy:emotional) + (1 | id:sleepy:congruent) + (1| id:emotional:congruent)",
    "3" = " +  (1 | id) + (1 | id:sleepy) + (1 | id:emotional) + (1 | id:congruent) +
    (1 | id:sleepy:emotional) + (1 | id:sleepy:congruent) + (1| id:emotional:congruent) + (1 | id:sleepy:emotional:congruent)"),
  hyb = list(
    "0" = " + (1 | id)",
    "1" = " + (1 | id/sleepy)",
    "2" = " + (1 + emotional + congruent || id/sleepy)",
    "3" = " + (1 + emotional + congruent |  id/sleepy)",
    "4" = " + (1 + emotional + congruent || id) + (1 | id:sleepy)",
    "5" = " + (1 + emotional + congruent |  id) + (1 | id:sleepy)",
    "6" = " + (1 + emotional + congruent || id:sleepy)",
    "7" = " + (1 | id) + (1 + emotional + congruent | id:sleepy)")
  )

#### formlulas for fixed effects ####

# # fully specified up to a four-way interaction
# fe_formula=list(
#   null = " ~ 1",
#   c =  " ~ congruent",
#   e =  " ~ emotional",
#   s =  " ~ sleepy",
#   u =  " ~ update",
#   ce =  " ~ congruent*emotional",
#   cs =  " ~ congruent*sleepy",
#   cu =  " ~ congruent*update",
#   es =  " ~ emotional*sleepy",
#   eu =  " ~ emotional*update",
#   su =  " ~ sleepy*update",
#   ces =  " ~ congruent*emotional*sleepy",
#   ceu =  " ~ congruent*emotional*update",
#   csu =  " ~ congruent*sleepy*update",
#   esu =  " ~ emotional*sleepy*update",
#   cesu = " ~ congruent*emotional*sleepy*update"
# )

## Note on naming: Effects specified before any underscore are fully specified (all main effects + interactions)
## effects after a single underscore describe interactions, after a double underscore main effects

fe_formula=list(
  null = " ~ 1",
  c =  " ~ congruent",
  e =  " ~ emotional",
  s =  " ~ sleepy",
  u =  " ~ update",
  c__e =  " ~ congruent + emotional",
  s__e =  " ~ sleepy + emotional",
  u__e =  " ~ update + emotional",
  cs =  " ~ congruent*sleepy",
  cu =  " ~ congruent*update",
  su =  " ~ sleepy*update",
  cs__e =  " ~ congruent*sleepy + emotional",
  cu__e =  " ~ congruent*update + emotional",
  su__e =  " ~ sleepy*update + emotional",
  cs__ue =  " ~ congruent*sleepy + emotional + update",
  cu__es =  " ~ congruent*update + emotional + sleepy",
  su__ce =  " ~ sleepy*update + congruent + emotional",
  csu =  " ~ congruent*sleepy*update",
  csu__e =  " ~ congruent*sleepy*update + emotional",
  cesu = " ~ congruent*emotional*sleepy*update"
)