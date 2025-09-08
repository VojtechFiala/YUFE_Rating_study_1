# Models for YUFE4postdocs study one: 
# "Now-final" 

# Final models: 

# DATA in Long Format...
d1 <- read.csv("Yufe_rating_study_long_data_SM_factor_25_08_25.csv",T, dec=",")

names(d1)[names(d1)=="Split"]<-"Above_Below"

# The "Czech control group" will be used only later, let's leave it out for now...
d2 <- d1[d1$Above_Below!=0,]

summary(as.factor(d2$Sample))

summary(as.data.frame(d2))

summary(as.factor(is.na(d2$Atr)))


stanATTR <- function(x){(x-mean(d$Atr))/sd(d$Atr)}
unstanATTR <- function(x){x*sd(d$Atr)+mean(d$Atr)}

stanTRU <- function(x){(x-mean(d$Tru))/sd(d$Tru)}
unstanTRU <- function(x){x*sd(d$Tru)+mean(d$Tru)}

stanDOM <- function(x){(x-mean(d$Dom))/sd(d$Dom)}
unstanDOM <- function(x){x*sd(d$Dom)+mean(d$Dom)}

stanL <- function(x){(x-mean(as.numeric(d$L)))/sd(as.numeric(d$L))}
unstanL <- function(x){x*sd(as.numeric(d$L))+mean(as.numeric(d$L))}

stanSShD <- function(x){(x-mean(as.numeric(d$SShD)))/sd(as.numeric(d$SShD))}
unstanSShD <- function(x){x*sd(as.numeric(d$SShD))+mean(as.numeric(d$SShD))}

stanDIST <- function(x){(x-mean(as.numeric(d$DIST)))/sd(as.numeric(d$DIST))}
unstanDIST <- function(x){x*sd(as.numeric(d$DIST))+mean(as.numeric(d$DIST))}

stanFA <- function(x){(x-mean(as.numeric(d$Asym)))/sd(as.numeric(d$Asym))}
unstanFA <- function(x){x*sd(as.numeric(d$Asym))+mean(as.numeric(d$Asym))}

stanAge <- function(x){(x-mean(d$Age))/sd(d$Age)}
unstanAge <- function(x){x*sd(d$Age)+mean(d$Age)}

summary(d2$Atr)

d2$FSMUi <- paste(d2$Sample,d2$Above_Below)
summary(as.factor(d2$FSMUi))

# Getting only females, only males: 

summary(nchar(d2$Face_ID)) # Most have 13, some has 14 characters. Nevermind, always they end with "_F" or "_M" (?)
summary(as.factor(substr(d2$Face_ID,nchar(d2$Face_ID)-1,nchar(d2$Face_ID)))) # See...

d2$StimSex <- as.factor(substr(d2$Face_ID,nchar(d2$Face_ID)-1,nchar(d2$Face_ID)))

d_F <- d2[d2$StimSex == "_F",]
d_M <- d2[d2$StimSex == "_M",]


###########
#         #
#  F_GMM  #
#         #
###########

d <- d_F

D_AT_F <- list(
  TrustwRating = stanTRU(d$Tru),
  AtrRating = stanATTR(d$Atr),
  DomRating = stanDOM(d$Dom),
  FSMUi = as.numeric(as.factor(d$FSMUi)), # Sample complete is: CZ Orig, CZ New Below, CZ New Above... 
  face = as.numeric(as.factor(d$Face_ID)),
  rater= as.numeric(as.factor(d$Particip_ID)),
  Age = stanAge(as.numeric(d$Age)), # Age of the stimuli
  dist = stanDIST(as.numeric(d$DIST)), # facial distinctiveness of the stimuli (morphometric measure)
  sshd = stanSShD(as.numeric(d$SShD)), # facial sextypicality of the stimuli (morphometric measure)
  FA = stanFA(as.numeric(d$Asym)), # facial asymmetry (morphometric measure)
  N_raters = length(unique(d$Particip_ID)),
  L = stanL(as.numeric(d$L))# Add the total number of raters
)

summary.data.frame(D_AT_F)

table(d_F$FSMUi,D_AT_F$FSMUi)

library(rethinking)


###########
#         #
#  A_T_D  # 
#         #
###########

ATD_long_F <- ulam(alist(
  
  # Multivariate normal for Trustworthiness and Attractiveness
  c(AtrRating, TrustwRating, DomRating) ~ multi_normal(c(muA, muT, muD), Rho_Scales, sigma_Scales),
  
  
  # ATTRACTIVENESS
  muA <- z_rNV_A[rater] * sigma_rater_A
  + f_per_group_A[face, FSMUi] # per face - per culture
  + (aA + f_per_group_pr_A[FSMUi,1])
  + (b_age_A + f_per_group_pr_A[FSMUi, 2]) * Age
  + (b_dist_A + f_per_group_pr_A[FSMUi, 3]) * dist
  + (b_FA_A + f_per_group_pr_A[FSMUi, 4]) * FA
  + (b_sshd_A + f_per_group_pr_A[FSMUi, 5]) * sshd
  + (b_L_A + f_per_group_pr_A[FSMUi,6]) * L, 
  
  aA ~ dnorm(0, 0.5),  # 
  
  b_age_A ~ dnorm(0, 0.3),
  b_dist_A ~ dnorm(0, 0.3),
  b_FA_A ~ dnorm(0, 0.3),
  b_sshd_A ~ dnorm(0, 0.3),
  b_L_A ~ dnorm(0, 0.3),
  
  # Non-centered parameterization for rater effects
  z_rNV_A[rater] ~ dnorm(0, 1),  #  latent variable
  
  sigma_rater_A ~ dexp(1),  # Attractiveness
  
  gq> vector[rater]:a_rNV_A <<- aA + z_rNV_A * sigma_rater_A,  # Generate rater effects for Attractiveness
  
  # Varying effects for Attractiveness morpho-predictors
  transpars> matrix[FSMUi, 6]:f_per_group_pr_A <- compose_noncentered(sigma_pr_A, L_Rho_pr_A, z_pr_A),
  cholesky_factor_corr[6]:L_Rho_pr_A ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_A ~ normal(0, 1),
  vector[6]:sigma_pr_A ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_A <<- Chol_to_Corr(L_Rho_pr_A),
  
  # Priors for the multivariate normal distribution for face intercepts across groups
  transpars> matrix[face, 12]:f_per_group_A <- compose_noncentered(sigma_FSMUi_A, L_Rho_FSMUi_A, z_FSMUi_A), 
  cholesky_factor_corr[12]:L_Rho_FSMUi_A ~ lkj_corr_cholesky(2),
  matrix[12, face]:z_FSMUi_A ~ normal(0, 1),
  vector[12]:sigma_FSMUi_A ~ dexp(1),
  
  gq> matrix[12, 12]:Rho_FSMUi_A <<- Chol_to_Corr(L_Rho_FSMUi_A),
  
  
  # TRUSTWORTHINESS
  muT <- z_rNV_T[rater] * sigma_rater_T
  + f_per_group_T[face, FSMUi] # per face - per culture
  + (aT + f_per_group_pr_T[FSMUi,1])
  + (b_age_T + f_per_group_pr_T[FSMUi, 2]) * Age
  + (b_dist_T + f_per_group_pr_T[FSMUi, 3]) * dist
  + (b_FA_T + f_per_group_pr_T[FSMUi, 4]) * FA
  + (b_sshd_T + f_per_group_pr_T[FSMUi, 5]) * sshd
  + (b_L_T + f_per_group_pr_T[FSMUi,6]) * L, 
  
  aT ~ dnorm(0, 0.5),  
  
  b_age_T ~ dnorm(0, 0.3),
  b_dist_T ~ dnorm(0, 0.3),
  b_FA_T ~ dnorm(0, 0.3),
  b_sshd_T ~ dnorm(0, 0.3),
  b_L_T ~ dnorm(0, 0.3),
  
  z_rNV_T[rater] ~ dnorm(0, 1),  # Trustworthiness latent variable
  
  sigma_rater_T ~ dexp(1),  
  
  gq> vector[rater]:a_rNV_T <<- aT + z_rNV_T * sigma_rater_T,  # Generate rater effects for Trustworthiness
  
  # Varying effects for Trustworthiness morpho-predictors
  transpars> matrix[FSMUi, 6]:f_per_group_pr_T <- compose_noncentered(sigma_pr_T, L_Rho_pr_T, z_pr_T),
  cholesky_factor_corr[6]:L_Rho_pr_T ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_T ~ normal(0, 1),
  vector[6]:sigma_pr_T ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_T <<- Chol_to_Corr(L_Rho_pr_T),
  
  transpars> matrix[face, 12]:f_per_group_T <- compose_noncentered(sigma_FSMUi_T, L_Rho_FSMUi_T, z_FSMUi_T), 
  cholesky_factor_corr[12]:L_Rho_FSMUi_T ~ lkj_corr_cholesky(2),
  matrix[12, face]:z_FSMUi_T ~ normal(0, 1),
  vector[12]:sigma_FSMUi_T ~ dexp(1),
  
  gq> matrix[12, 12]:Rho_FSMUi_T <<- Chol_to_Corr(L_Rho_FSMUi_T),
  
  
  # DOMINANCE
  muD <- z_rNV_D[rater] * sigma_rater_D
  + f_per_group_D[face, FSMUi] # per face - per culture
  + (aD + f_per_group_pr_D[FSMUi,1])
  + (b_age_D + f_per_group_pr_D[FSMUi, 2]) * Age
  + (b_dist_D + f_per_group_pr_D[FSMUi, 3]) * dist
  + (b_FA_D + f_per_group_pr_D[FSMUi, 4]) * FA
  + (b_sshd_D + f_per_group_pr_D[FSMUi, 5]) * sshd
  + (b_L_D + f_per_group_pr_D[FSMUi,6]) * L, 
  
  aD ~ dnorm(0, 0.5),  
  
  b_age_D ~ dnorm(0, 0.3),
  b_dist_D ~ dnorm(0, 0.3),
  b_FA_D ~ dnorm(0, 0.3),
  b_sshd_D ~ dnorm(0, 0.3),
  b_L_D ~ dnorm(0, 0.3),
  
  # Non-centered parameterization for rater effects
  z_rNV_D[rater] ~ dnorm(0, 1),  #  latent variable
  
  sigma_rater_D ~ dexp(1),  # Dominance
  
  gq> vector[rater]:a_rNV_D <<- aD + z_rNV_D * sigma_rater_D,  
  
  transpars> matrix[FSMUi, 6]:f_per_group_pr_D <- compose_noncentered(sigma_pr_D, L_Rho_pr_D, z_pr_D),
  cholesky_factor_corr[6]:L_Rho_pr_D ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_D ~ normal(0, 1),
  vector[6]:sigma_pr_D ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_D <<- Chol_to_Corr(L_Rho_pr_D),
  
  # Priors for the multivariate normal distribution for face intercepts across groups
  transpars> matrix[face, 12]:f_per_group_D <- compose_noncentered(sigma_FSMUi_D, L_Rho_FSMUi_D, z_FSMUi_D), 
  cholesky_factor_corr[12]:L_Rho_FSMUi_D ~ lkj_corr_cholesky(2),
  matrix[12, face]:z_FSMUi_D ~ normal(0, 1),
  vector[12]:sigma_FSMUi_D ~ dexp(1),
  
  gq> matrix[12, 12]:Rho_FSMUi_D <<- Chol_to_Corr(L_Rho_FSMUi_D),
  
  
  # Common terms
  Rho_Scales ~ lkj_corr(2),
  sigma_Scales ~ dexp(1)
  
), data=D_AT_F, iter=700, sample=T, cores=14, chains=14)


post_F_ATD_Long <- extract.samples(ATD_long_F)
saveRDS(post_F_ATD_Long, file="post_F_ATD_22_08_25_Long.RDS")
save(ATD_long_F,file="(ATD_long_F_22_08.Rdata")

#              1    2    3    4    5    6    7    8    9   10   11   12
# AUS Above 1428    0    0    0    0    0    0    0    0    0    0    0
# AUS Below    0 1372    0    0    0    0    0    0    0    0    0    0
# COL Above    0    0  854    0    0    0    0    0    0    0    0    0
# COL Below    0    0    0  848    0    0    0    0    0    0    0    0
# CZ Above     0    0    0    0 4004    0    0    0    0    0    0    0
# CZ Below     0    0    0    0    0 4046    0    0    0    0    0    0
# RSA Above    0    0    0    0    0    0 1266    0    0    0    0    0
# RSA Below    0    0    0    0    0    0    0 1228    0    0    0    0
# TUR Above    0    0    0    0    0    0    0    0 2150    0    0    0
# TUR Below    0    0    0    0    0    0    0    0    0 2040    0    0
# VN Above     0    0    0    0    0    0    0    0    0    0 1878    0
# VN Below     0    0    0    0    0    0    0    0    0    0    0 1932


# Run it all with GMM already - there's no need to avoid them...

# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T

# I.A - WOMEN
Attr_short_F <- ulam(alist(
  
  AtrRating ~  normal(muA, sigma_A),
  
  muA <- z_rNV_A[rater] * sigma_rater_A
  + (aA + f_per_group_pr_A[FSMUi,1])
  + (b_age_A + f_per_group_pr_A[FSMUi, 2]) * Age
  + (b_dist_A + f_per_group_pr_A[FSMUi, 3]) * dist
  + (b_FA_A + f_per_group_pr_A[FSMUi, 4]) * FA
  + (b_sshd_A + f_per_group_pr_A[FSMUi, 5]) * sshd
  + (b_L_A + f_per_group_pr_A[FSMUi,6]) * L, 
  
  # Prior for Attractiveness intercept
  aA ~ dnorm(0, 0.5),  
  
  # Priors for Attractiveness slopes
  b_age_A ~ dnorm(0, 0.3),
  b_dist_A ~ dnorm(0, 0.3),
  b_FA_A ~ dnorm(0, 0.3),
  b_sshd_A ~ dnorm(0, 0.3),
  b_L_A ~ dnorm(0, 0.3),
  
  # Non-centered parameterization for rater effects
  z_rNV_A[rater] ~ dnorm(0, 1),  # Attractiveness latent variable
  
  sigma_rater_A ~ dexp(1),  # Attractiveness
  
  gq> vector[rater]:a_rNV_A <<- aA + z_rNV_A * sigma_rater_A,  # Generate rater effects for Attractiveness
  
  # Varying effects for Attractiveness morpho-predictors
  transpars> matrix[FSMUi, 6]:f_per_group_pr_A <- compose_noncentered(sigma_pr_A, L_Rho_pr_A, z_pr_A),
  cholesky_factor_corr[6]:L_Rho_pr_A ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_A ~ normal(0, 1),
  vector[6]:sigma_pr_A ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_A <<- Chol_to_Corr(L_Rho_pr_A),
  
  # Covariance between Attractiveness and Trustworthiness
  sigma_A ~ dexp(1)
), data=D_AT_F, iter=1000, sample=T, cores=8, chains=8, log_lik=T)

precis(Attr_short_F)

# Extract the posterior samples using precis
precis_data <- precis(Attr_short_F, depth = 3, pars = "f_per_group_pr_A")

# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 

# I.C - WOMEN
Trustw_short_F <- ulam(alist(
  
  # Multivariate normal for Trustworthiness and Attractiveness
  TrustwRating ~ normal(muT, sigma_T),
  
  # Mean structure for Trustworthiness and Attractiveness
  muT <- z_rNV_T[rater] * sigma_rater_T
  + (aT + f_per_group_pr_T[FSMUi,1])
  + (b_age_T + f_per_group_pr_T[FSMUi, 2]) * Age
  + (b_dist_T + f_per_group_pr_T[FSMUi, 3]) * dist
  + (b_FA_T + f_per_group_pr_T[FSMUi, 4]) * FA
  + (b_sshd_T + f_per_group_pr_T[FSMUi, 5]) * sshd
  + (b_L_T + f_per_group_pr_T[FSMUi,6]) * L, 
  
  # Priors for Trustworthiness intercepts
  aT ~ dnorm(0, 0.5),  # Trustworthiness
  
  # Priors for Attractiveness and Trustworthiness slopes
  b_age_T ~ dnorm(0, 0.3),
  b_dist_T ~ dnorm(0, 0.3),
  b_FA_T ~ dnorm(0, 0.3),
  b_sshd_T ~ dnorm(0, 0.3),
  b_L_T ~ dnorm(0, 0.3),
  
  # Non-centered parameterization for rater effects
  z_rNV_T[rater] ~ dnorm(0, 1),  # Trustworthiness latent variable
  
  sigma_rater_T ~ dexp(1),  # Trustworthiness
  
  gq> vector[rater]:a_rNV_T <<- aT + z_rNV_T * sigma_rater_T,  # Generate rater effects for Trustworthiness
  
  # Varying effects for Trustworthiness morpho-predictors
  transpars> matrix[FSMUi, 6]:f_per_group_pr_T <- compose_noncentered(sigma_pr_T, L_Rho_pr_T, z_pr_T),
  cholesky_factor_corr[6]:L_Rho_pr_T ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_T ~ normal(0, 1),
  vector[6]:sigma_pr_T ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_T <<- Chol_to_Corr(L_Rho_pr_T),
  
  # Covariance between Attractiveness and Trustworthiness
  sigma_T ~ dexp(1)
  
), data=D_AT_F, iter=1000, sample=T, cores=8, chains=8, log_lik=T)


# DOM - type I - without per-face varying terms - mind to add Log_lik = T 
# DOM - type I - without per-face varying terms - mind to add Log_lik = T 
# DOM - type I - without per-face varying terms - mind to add Log_lik = T 
# DOM - type I - without per-face varying terms - mind to add Log_lik = T 
# DOM - type I - without per-face varying terms - mind to add Log_lik = T 
# DOM - type I - without per-face varying terms - mind to add Log_lik = T 
# DOM - type I - without per-face varying terms - mind to add Log_lik = T 
# DOM - type I - without per-face varying terms - mind to add Log_lik = T 
# DOM - type I - without per-face varying terms - mind to add Log_lik = T 
# DOM - type I - without per-face varying terms - mind to add Log_lik = T 
# DOM - type I - without per-face varying terms - mind to add Log_lik = T 


Dom_short_F <- ulam(alist(
  
  # Multivariate normal for Trustworthiness and Attractiveness
  DomRating ~ normal(muD, sigma_D),
  
  # Mean structure for Trustworthiness and Attractiveness
  muD <- z_rNV_D[rater] * sigma_rater_D
  + (aD + f_per_group_pr_D[FSMUi,1])
  + (b_age_D + f_per_group_pr_D[FSMUi, 2]) * Age
  + (b_dist_D + f_per_group_pr_D[FSMUi, 3]) * dist
  + (b_FA_D + f_per_group_pr_D[FSMUi, 4]) * FA
  + (b_sshd_D + f_per_group_pr_D[FSMUi, 5]) * sshd
  + (b_L_D + f_per_group_pr_D[FSMUi,6]) * L, 
  
  # Priors for Trustworthiness intercepts
  aD ~ dnorm(0, 0.5),  # Trustworthiness
  
  # Priors for Attractiveness and Trustworthiness slopes
  b_age_D ~ dnorm(0, 0.3),
  b_dist_D ~ dnorm(0, 0.3),
  b_FA_D ~ dnorm(0, 0.3),
  b_sshd_D ~ dnorm(0, 0.3),
  b_L_D ~ dnorm(0, 0.3),
  
  # Non-centered parameterization for rater effects
  z_rNV_D[rater] ~ dnorm(0, 1),  # Trustworthiness latent variable
  
  sigma_rater_D ~ dexp(1),  # Trustworthiness
  
  gq> vector[rater]:a_rNV_D <<- aD + z_rNV_D * sigma_rater_D,  # Generate rater effects for Trustworthiness
  
  # Varying effects for Trustworthiness morpho-predictors
  transpars> matrix[FSMUi, 6]:f_per_group_pr_D <- compose_noncentered(sigma_pr_D, L_Rho_pr_D, z_pr_D),
  cholesky_factor_corr[6]:L_Rho_pr_D ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_D ~ normal(0, 1),
  vector[6]:sigma_pr_D ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_D <<- Chol_to_Corr(L_Rho_pr_D),
  
  # Covariance between Attractiveness and Trustworthiness
  sigma_D ~ dexp(1)
  
), data=D_AT_F, iter=1000, sample=T, cores=8, chains=8, log_lik=T)

post_F_Atr <- extract.samples(Attr_short_F)
post_F_Tru <- extract.samples(Trustw_short_F)
post_F_Dom <- extract.samples(Dom_short_F)

saveRDS(post_F_Atr, file="post_F_Atr_22_08_25.RDS")
saveRDS(post_F_Tru, file="post_F_Tru_22_08_25.RDS")
saveRDS(post_F_Dom, file="post_F_Dom_22_08_25.RDS")

WAIC1_attr <- WAIC(Attr_short_F)
write.csv(WAIC1_attr, file="WAIC1_attr_22_08.csv")

WAIC1_tru <- WAIC(Trustw_short_F)
write.csv(WAIC1_tru, file="WAIC1_tru_22_08.csv")

WAIC1_dom <- WAIC(Dom_short_F)
write.csv(WAIC1_dom, file="WAIC1_dom_22_08.csv")


save(Attr_short_F,file="(m1_f_attr_22_08.Rdata")
save(Trustw_short_F,file="(m1_f_tru_22_08.Rdata")
save(Dom_short_F,file="(m1_f_dom_22_08.Rdata")



# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T
# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T
# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T
# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T
# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T
# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T
# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T
# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T
# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T
# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T
# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T

# II.A - WOMEN

Attr_long_F <- ulam(alist(
  
  # Multivariate normal for Attractiveness
  AtrRating ~ normal(muA, sigma_A),
  
  # Mean structure for Attractiveness
  muA <- z_rNV_A[rater] * sigma_rater_A
  + f_per_group_A[face, FSMUi] # per face - per culture
  + (aA + f_per_group_pr_A[FSMUi,1])
  + (b_age_A + f_per_group_pr_A[FSMUi, 2]) * Age
  + (b_dist_A + f_per_group_pr_A[FSMUi, 3]) * dist
  + (b_FA_A + f_per_group_pr_A[FSMUi, 4]) * FA
  + (b_sshd_A + f_per_group_pr_A[FSMUi, 5]) * sshd
  + (b_L_A + f_per_group_pr_A[FSMUi,6]) * L, 
  
  # Priors for Attractiveness  intercept
  aA ~ dnorm(0, 0.5),  # Attractiveness
  
  # Priors for Attractiveness and slopes
  b_age_A ~ dnorm(0, 0.3),
  b_dist_A ~ dnorm(0, 0.3),
  b_FA_A ~ dnorm(0, 0.3),
  b_sshd_A ~ dnorm(0, 0.3),
  b_L_A ~ dnorm(0, 0.3),
  
  # Non-centered parameterization for rater effects
  z_rNV_A[rater] ~ dnorm(0, 1),  # Attractiveness latent variable
  
  sigma_rater_A ~ dexp(1),  # Attractiveness
  
  gq> vector[rater]:a_rNV_A <<- aA + z_rNV_A * sigma_rater_A,  # Generate rater effects for Attractiveness
  
  # Varying effects for Attractiveness morpho-predictors
  transpars> matrix[FSMUi, 6]:f_per_group_pr_A <- compose_noncentered(sigma_pr_A, L_Rho_pr_A, z_pr_A),
  cholesky_factor_corr[6]:L_Rho_pr_A ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_A ~ normal(0, 1),
  vector[6]:sigma_pr_A ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_A <<- Chol_to_Corr(L_Rho_pr_A),
  
  # Priors for the multivariate normal distribution for face intercepts across groups
  transpars> matrix[face, 12]:f_per_group_A <- compose_noncentered(sigma_FSMUi_A, L_Rho_FSMUi_A, z_FSMUi_A), 
  cholesky_factor_corr[12]:L_Rho_FSMUi_A ~ lkj_corr_cholesky(2),
  matrix[12, face]:z_FSMUi_A ~ normal(0, 1),
  vector[12]:sigma_FSMUi_A ~ dexp(1),
  
  gq> matrix[12, 12]:Rho_FSMUi_A <<- Chol_to_Corr(L_Rho_FSMUi_A),
  
  # Covariance between Attractiveness and Trustworthiness
  sigma_A ~ dexp(1)
  
), data=D_AT_F, iter=1000, sample=T, cores=8, chains=8, log_lik=T)



# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 

Trustw_long_F <- ulam(alist(
  
  # Multivariate normal for Trustworthiness and Attractiveness
  TrustwRating ~ normal(muT, sigma_T),
  
  # Mean structure for Trustworthiness and Attractiveness
  muT <- z_rNV_T[rater] * sigma_rater_T
  + f_per_group_T[face, FSMUi] # per face - per culture
  + (aT + f_per_group_pr_T[FSMUi,1])
  + (b_age_T + f_per_group_pr_T[FSMUi, 2]) * Age
  + (b_dist_T + f_per_group_pr_T[FSMUi, 3]) * dist
  + (b_FA_T + f_per_group_pr_T[FSMUi, 4]) * FA
  + (b_sshd_T + f_per_group_pr_T[FSMUi, 5]) * sshd
  + (b_L_T + f_per_group_pr_T[FSMUi,6]) * L, 
  
  # Priors for Trustworthiness intercepts
  aT ~ dnorm(0, 0.5),  # Trustworthiness
  
  # Priors for Attractiveness and Trustworthiness slopes
  b_age_T ~ dnorm(0, 0.3),
  b_dist_T ~ dnorm(0, 0.3),
  b_FA_T ~ dnorm(0, 0.3),
  b_sshd_T ~ dnorm(0, 0.3),
  b_L_T ~ dnorm(0, 0.3),
  
  # Non-centered parameterization for rater effects
  z_rNV_T[rater] ~ dnorm(0, 1),  # Trustworthiness latent variable
  
  sigma_rater_T ~ dexp(1),  # Trustworthiness
  
  gq> vector[rater]:a_rNV_T <<- aT + z_rNV_T * sigma_rater_T,  # Generate rater effects for Trustworthiness
  
  # Varying effects for Trustworthiness morpho-predictors
  transpars> matrix[FSMUi, 6]:f_per_group_pr_T <- compose_noncentered(sigma_pr_T, L_Rho_pr_T, z_pr_T),
  cholesky_factor_corr[6]:L_Rho_pr_T ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_T ~ normal(0, 1),
  vector[6]:sigma_pr_T ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_T <<- Chol_to_Corr(L_Rho_pr_T),
  
  # Priors for the multivariate normal distribution for face intercepts across groups
  transpars> matrix[face, 12]:f_per_group_T <- compose_noncentered(sigma_FSMUi_T, L_Rho_FSMUi_T, z_FSMUi_T), 
  cholesky_factor_corr[12]:L_Rho_FSMUi_T ~ lkj_corr_cholesky(2),
  matrix[12, face]:z_FSMUi_T ~ normal(0, 1),
  vector[12]:sigma_FSMUi_T ~ dexp(1),
  
  gq> matrix[12, 12]:Rho_FSMUi_T <<- Chol_to_Corr(L_Rho_FSMUi_T),
  
  # Covariance between Attractiveness and Trustworthiness
  sigma_T ~ dexp(1)
  
), data=D_AT_F, iter=1000, sample=T, cores=8, chains=8, log_lik=T)


# DOM - type II - with per-face varying terms - mind to add Log_lik = T 
# DOM - type II - with per-face varying terms - mind to add Log_lik = T 
# DOM - type II - with per-face varying terms - mind to add Log_lik = T 
# DOM - type II - with per-face varying terms - mind to add Log_lik = T 
# DOM - type II - with per-face varying terms - mind to add Log_lik = T 
# DOM - type II - with per-face varying terms - mind to add Log_lik = T 
# DOM - type II - with per-face varying terms - mind to add Log_lik = T 
# DOM - type II - with per-face varying terms - mind to add Log_lik = T 
# DOM - type II - with per-face varying terms - mind to add Log_lik = T 
# DOM - type II - with per-face varying terms - mind to add Log_lik = T 
# DOM - type II - with per-face varying terms - mind to add Log_lik = T 

Dom_long_F <- ulam(alist(
  
  # Multivariate normal for Trustworthiness and Attractiveness
  DomRating ~ normal(muD, sigma_D),
  
  # Mean structure for Trustworthiness and Attractiveness
  muD <- z_rNV_D[rater] * sigma_rater_D
  + f_per_group_D[face, FSMUi] # per face - per culture
  + (aD + f_per_group_pr_D[FSMUi,1])
  + (b_age_D + f_per_group_pr_D[FSMUi, 2]) * Age
  + (b_dist_D + f_per_group_pr_D[FSMUi, 3]) * dist
  + (b_FA_D + f_per_group_pr_D[FSMUi, 4]) * FA
  + (b_sshd_D + f_per_group_pr_D[FSMUi, 5]) * sshd
  + (b_L_D + f_per_group_pr_D[FSMUi,6]) * L, 
  
  # Priors for Trustworthiness intercepts
  aD ~ dnorm(0, 0.5),  # Trustworthiness
  
  # Priors for Attractiveness and Trustworthiness slopes
  b_age_D ~ dnorm(0, 0.3),
  b_dist_D ~ dnorm(0, 0.3),
  b_FA_D ~ dnorm(0, 0.3),
  b_sshd_D ~ dnorm(0, 0.3),
  b_L_D ~ dnorm(0, 0.3),
  
  # Non-centered parameterization for rater effects
  z_rNV_D[rater] ~ dnorm(0, 1),  # Trustworthiness latent variable
  
  sigma_rater_D ~ dexp(1),  # Trustworthiness
  
  gq> vector[rater]:a_rNV_D <<- aD + z_rNV_D * sigma_rater_D,  # Generate rater effects for Trustworthiness
  
  # Varying effects for Trustworthiness morpho-predictors
  transpars> matrix[FSMUi, 6]:f_per_group_pr_D <- compose_noncentered(sigma_pr_D, L_Rho_pr_D, z_pr_D),
  cholesky_factor_corr[6]:L_Rho_pr_D ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_D ~ normal(0, 1),
  vector[6]:sigma_pr_D ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_D <<- Chol_to_Corr(L_Rho_pr_D),
  
  # Priors for the multivariate normal distribution for face intercepts across groups
  transpars> matrix[face, 12]:f_per_group_D <- compose_noncentered(sigma_FSMUi_D, L_Rho_FSMUi_D, z_FSMUi_D), 
  cholesky_factor_corr[12]:L_Rho_FSMUi_D ~ lkj_corr_cholesky(2),
  matrix[12, face]:z_FSMUi_D ~ normal(0, 1),
  vector[12]:sigma_FSMUi_D ~ dexp(1),
  
  gq> matrix[12, 12]:Rho_FSMUi_D <<- Chol_to_Corr(L_Rho_FSMUi_D),
  
  # Covariance between Attractiveness and Trustworthiness
  sigma_D ~ dexp(1)
  
), data=D_AT_F, iter=1000, sample=T, cores=8, chains=8, log_lik=T)



compare(Attr_short_F, Attr_long_F)
compare(Trustw_short_F, Trustw_long_F)
compare(Dom_short_F, Dom_long_F)


post_F_Atr_Long <- extract.samples(Attr_long_F)
post_F_Tru_Long <- extract.samples(Trustw_long_F)
post_F_Dom_Long <- extract.samples(Dom_long_F)

saveRDS(post_F_Atr_Long, file="post_F_Atr_22_08_25_Long.RDS")
saveRDS(post_F_Tru_Long, file="post_F_Tru_22_08_25_Long.RDS")
saveRDS(post_F_Dom_Long, file="post_F_Dom_22_08_25_Long.RDS")


WAIC2_attr <- WAIC(Attr_long_F)
write.csv(WAIC1_attr, file="WAIC2F_attr_22_08.csv")

WAIC2_tru <- WAIC(Trustw_long_F)
write.csv(WAIC1_tru, file="WAIC2F_tru_22_08.csv")

WAIC2_dom <- WAIC(Dom_long_F)
write.csv(WAIC1_dom, file="WAIC2F_dom_22_08.csv")


save(Attr_long_F,file="(m2_f_attr_22_08.Rdata")
save(Trustw_long_F,file="(m2_f_tru_22_08.Rdata")
save(Dom_long_F,file="(m2_f_dom_22_08.Rdata")






#########
#       #
#   M   #
#       #
#########


d <- d_M

D_AT_M <- list(
  TrustwRating = stanTRU(d$Tru),
  AtrRating = stanATTR(d$Atr),
  DomRating = stanDOM(d$Dom),
  FSMUi = as.numeric(as.factor(d$FSMUi)), # Sample complete is: CZ Orig, CZ New Below, CZ New Above... 
  face = as.numeric(as.factor(d$Face_ID)),
  rater= as.numeric(as.factor(d$Particip_ID)),
  Age = stanAge(as.numeric(d$Age)), # Age of the stimuli
  dist = stanDIST(as.numeric(d$DIST)), # facial distinctiveness of the stimuli (morphometric measure)
  sshd = stanSShD(as.numeric(d$SShD)), # facial sextypicality of the stimuli (morphometric measure)
  FA = stanFA(as.numeric(d$Asym)), # facial asymmetry (morphometric measure)
  N_raters = length(unique(d$Particip_ID)),
  L = stanL(as.numeric(d$L))# Add the total number of raters
)

summary.data.frame(D_AT_M)

table(d_M$FSMUi,D_AT_M$FSMUi)

# Remember that there are only 89 men (as compared to 106 women) 
# In the old script, you accidentaly copied the female layout again (I mean, not data,
# the table outline below was just women repeated).

#              1    2    3    4    5    6    7    8    9   10   11   12
# AUS Above 1207    0    0    0    0    0    0    0    0    0    0    0
# AUS Below    0 1168    0    0    0    0    0    0    0    0    0    0
# COL Above    0    0  701    0    0    0    0    0    0    0    0    0
# COL Below    0    0    0  712    0    0    0    0    0    0    0    0
# CZ Above     0    0    0    0 3426    0    0    0    0    0    0    0
# CZ Below     0    0    0    0    0 3349    0    0    0    0    0    0
# RSA Above    0    0    0    0    0    0 1079    0    0    0    0    0
# RSA Below    0    0    0    0    0    0    0 1007    0    0    0    0
# TUR Above    0    0    0    0    0    0    0    0 1725    0    0    0
# TUR Below    0    0    0    0    0    0    0    0    0 1785    0    0
# VN Above     0    0    0    0    0    0    0    0    0    0 1657    0
# VN Below     0    0    0    0    0    0    0    0    0    0    0 1558

summary.data.frame(D_AT_M)
summary.data.frame(D_AT_F)

###########
#         #
#  A_T_D  # 
#         #
###########

ATD_long_M <- ulam(alist(
  
  # Multivariate normal for Trustworthiness and Attractiveness
  c(AtrRating, TrustwRating, DomRating) ~ multi_normal(c(muA, muT, muD), Rho_Scales, sigma_Scales),
  
  
  # ATTRACTIVENESS
  muA <- z_rNV_A[rater] * sigma_rater_A
  + f_per_group_A[face, FSMUi] # per face - per culture
  + (aA + f_per_group_pr_A[FSMUi,1])
  + (b_age_A + f_per_group_pr_A[FSMUi, 2]) * Age
  + (b_dist_A + f_per_group_pr_A[FSMUi, 3]) * dist
  + (b_FA_A + f_per_group_pr_A[FSMUi, 4]) * FA
  + (b_sshd_A + f_per_group_pr_A[FSMUi, 5]) * sshd
  + (b_L_A + f_per_group_pr_A[FSMUi,6]) * L, 
  
  aA ~ dnorm(0, 0.5),  # 
  
  b_age_A ~ dnorm(0, 0.3),
  b_dist_A ~ dnorm(0, 0.3),
  b_FA_A ~ dnorm(0, 0.3),
  b_sshd_A ~ dnorm(0, 0.3),
  b_L_A ~ dnorm(0, 0.3),
  
  # Non-centered parameterization for rater effects
  z_rNV_A[rater] ~ dnorm(0, 1),  #  latent variable
  
  sigma_rater_A ~ dexp(1),  # Attractiveness
  
  gq> vector[rater]:a_rNV_A <<- aA + z_rNV_A * sigma_rater_A,  # Generate rater effects for Attractiveness
  
  # Varying effects for Attractiveness morpho-predictors
  transpars> matrix[FSMUi, 6]:f_per_group_pr_A <- compose_noncentered(sigma_pr_A, L_Rho_pr_A, z_pr_A),
  cholesky_factor_corr[6]:L_Rho_pr_A ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_A ~ normal(0, 1),
  vector[6]:sigma_pr_A ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_A <<- Chol_to_Corr(L_Rho_pr_A),
  
  # Priors for the multivariate normal distribution for face intercepts across groups
  transpars> matrix[face, 12]:f_per_group_A <- compose_noncentered(sigma_FSMUi_A, L_Rho_FSMUi_A, z_FSMUi_A), 
  cholesky_factor_corr[12]:L_Rho_FSMUi_A ~ lkj_corr_cholesky(2),
  matrix[12, face]:z_FSMUi_A ~ normal(0, 1),
  vector[12]:sigma_FSMUi_A ~ dexp(1),
  
  gq> matrix[12, 12]:Rho_FSMUi_A <<- Chol_to_Corr(L_Rho_FSMUi_A),
  
  
  # TRUSTWORTHINESS
  muT <- z_rNV_T[rater] * sigma_rater_T
  + f_per_group_T[face, FSMUi] # per face - per culture
  + (aT + f_per_group_pr_T[FSMUi, 1])
  + (b_age_T + f_per_group_pr_T[FSMUi, 2]) * Age
  + (b_dist_T + f_per_group_pr_T[FSMUi, 3]) * dist
  + (b_FA_T + f_per_group_pr_T[FSMUi, 4]) * FA
  + (b_sshd_T + f_per_group_pr_T[FSMUi, 5]) * sshd
  + (b_L_T + f_per_group_pr_T[FSMUi, 6]) * L, 
  
  aT ~ dnorm(0, 0.5),  
  
  b_age_T ~ dnorm(0, 0.3),
  b_dist_T ~ dnorm(0, 0.3),
  b_FA_T ~ dnorm(0, 0.3),
  b_sshd_T ~ dnorm(0, 0.3),
  b_L_T ~ dnorm(0, 0.3),
  
  z_rNV_T[rater] ~ dnorm(0, 1),  # Trustworthiness latent variable
  
  sigma_rater_T ~ dexp(1),  
  
  gq> vector[rater]:a_rNV_T <<- aT + z_rNV_T * sigma_rater_T,  # Generate rater effects for Trustworthiness
  
  # Varying effects for Trustworthiness morpho-predictors
  transpars> matrix[FSMUi, 6]:f_per_group_pr_T <- compose_noncentered(sigma_pr_T, L_Rho_pr_T, z_pr_T),
  cholesky_factor_corr[6]:L_Rho_pr_T ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_T ~ normal(0, 1),
  vector[6]:sigma_pr_T ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_T <<- Chol_to_Corr(L_Rho_pr_T),
  
  transpars> matrix[face, 12]:f_per_group_T <- compose_noncentered(sigma_FSMUi_T, L_Rho_FSMUi_T, z_FSMUi_T), 
  cholesky_factor_corr[12]:L_Rho_FSMUi_T ~ lkj_corr_cholesky(2),
  matrix[12, face]:z_FSMUi_T ~ normal(0, 1),
  vector[12]:sigma_FSMUi_T ~ dexp(1),
  
  gq> matrix[12, 12]:Rho_FSMUi_T <<- Chol_to_Corr(L_Rho_FSMUi_T),
  
  
  # DOMINANCE
  muD <- z_rNV_D[rater] * sigma_rater_D
  + f_per_group_D[face, FSMUi] # per face - per culture
  + (aD + f_per_group_pr_D[FSMUi,1])
  + (b_age_D + f_per_group_pr_D[FSMUi, 2]) * Age
  + (b_dist_D + f_per_group_pr_D[FSMUi, 3]) * dist
  + (b_FA_D + f_per_group_pr_D[FSMUi, 4]) * FA
  + (b_sshd_D + f_per_group_pr_D[FSMUi, 5]) * sshd
  + (b_L_D + f_per_group_pr_D[FSMUi, 6]) * L, 
  
  aD ~ dnorm(0, 0.5),  
  
  b_age_D ~ dnorm(0, 0.3),
  b_dist_D ~ dnorm(0, 0.3),
  b_FA_D ~ dnorm(0, 0.3),
  b_sshd_D ~ dnorm(0, 0.3),
  b_L_D ~ dnorm(0, 0.3),
  
  # Non-centered parameterization for rater effects
  z_rNV_D[rater] ~ dnorm(0, 1),  #  latent variable
  
  sigma_rater_D ~ dexp(1),  # Dominance
  
  gq> vector[rater]:a_rNV_D <<- aD + z_rNV_D * sigma_rater_D,  
  
  transpars> matrix[FSMUi, 6]:f_per_group_pr_D <- compose_noncentered(sigma_pr_D, L_Rho_pr_D, z_pr_D),
  cholesky_factor_corr[6]:L_Rho_pr_D ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_D ~ normal(0, 1),
  vector[6]:sigma_pr_D ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_D <<- Chol_to_Corr(L_Rho_pr_D),
  
  # Priors for the multivariate normal distribution for face intercepts across groups
  transpars> matrix[face, 12]:f_per_group_D <- compose_noncentered(sigma_FSMUi_D, L_Rho_FSMUi_D, z_FSMUi_D), 
  cholesky_factor_corr[12]:L_Rho_FSMUi_D ~ lkj_corr_cholesky(2),
  matrix[12, face]:z_FSMUi_D ~ normal(0, 1),
  vector[12]:sigma_FSMUi_D ~ dexp(1),
  
  gq> matrix[12, 12]:Rho_FSMUi_D <<- Chol_to_Corr(L_Rho_FSMUi_D),
  
  
  # Common terms
  Rho_Scales ~ lkj_corr(2),
  sigma_Scales ~ dexp(1)
  
), data=D_AT_M, iter=700, sample=T, cores=14, chains=14)

post_M_ATD_Long <- extract.samples(ATD_long_M)
saveRDS(post_M_ATD_Long, file="post_M_ATD_22_08_25_Long.RDS")
save(ATD_long_M, file="Model_ATD_long_M.Rdata")



# Run it all with GMM already - there's no need to avoid them...

# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T

# I.A - MEN
Attr_short_M <- ulam(alist(
  
  AtrRating ~  normal(muA, sigma_A),
  
  muA <- z_rNV_A[rater] * sigma_rater_A
  + (aA + f_per_group_pr_A[FSMUi,1])
  + (b_age_A + f_per_group_pr_A[FSMUi, 2]) * Age
  + (b_dist_A + f_per_group_pr_A[FSMUi, 3]) * dist
  + (b_FA_A + f_per_group_pr_A[FSMUi, 4]) * FA
  + (b_sshd_A + f_per_group_pr_A[FSMUi, 5]) * sshd
  + (b_L_A + f_per_group_pr_A[FSMUi,6]) * L, 
  
  # Prior for Attractiveness intercept
  aA ~ dnorm(0, 0.5),  
  
  # Priors for Attractiveness slopes
  b_age_A ~ dnorm(0, 0.3),
  b_dist_A ~ dnorm(0, 0.3),
  b_FA_A ~ dnorm(0, 0.3),
  b_sshd_A ~ dnorm(0, 0.3),
  b_L_A ~ dnorm(0, 0.3),
  
  # Non-centered parameterization for rater effects
  z_rNV_A[rater] ~ dnorm(0, 1),  # Attractiveness latent variable
  
  sigma_rater_A ~ dexp(1),  # Attractiveness
  
  gq> vector[rater]:a_rNV_A <<- aA + z_rNV_A * sigma_rater_A,  # Generate rater effects for Attractiveness
  
  # Varying effects for Attractiveness morpho-predictors
  transpars> matrix[FSMUi, 6]:f_per_group_pr_A <- compose_noncentered(sigma_pr_A, L_Rho_pr_A, z_pr_A),
  cholesky_factor_corr[6]:L_Rho_pr_A ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_A ~ normal(0, 1),
  vector[6]:sigma_pr_A ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_A <<- Chol_to_Corr(L_Rho_pr_A),
  
  # Covariance between Attractiveness and Trustworthiness
  sigma_A ~ dexp(1)
), data=D_AT_M, iter=1000, sample=T, cores=8, chains=8, log_lik=T)

precis(Attr_short_M)

# Extract the posterior samples using precis
precis_data <- precis(Attr_short_M, depth = 3, pars = "f_per_group_pr_A")

# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type I - without per-face varying terms - mind to add Log_lik = T 

# I.C - MEN
Trustw_short_M <- ulam(alist(
  
  # Multivariate normal for Trustworthiness and Attractiveness
  TrustwRating ~ normal(muT, sigma_T),
  
  # Mean structure for Trustworthiness and Attractiveness
  muT <- z_rNV_T[rater] * sigma_rater_T
  + (aT + f_per_group_pr_T[FSMUi,1])
  + (b_age_T + f_per_group_pr_T[FSMUi, 2]) * Age
  + (b_dist_T + f_per_group_pr_T[FSMUi, 3]) * dist
  + (b_FA_T + f_per_group_pr_T[FSMUi, 4]) * FA
  + (b_sshd_T + f_per_group_pr_T[FSMUi, 5]) * sshd
  + (b_L_T + f_per_group_pr_T[FSMUi,6]) * L, 
  
  # Priors for Trustworthiness intercepts
  aT ~ dnorm(0, 0.5),  # Trustworthiness
  
  # Priors for Attractiveness and Trustworthiness slopes
  b_age_T ~ dnorm(0, 0.3),
  b_dist_T ~ dnorm(0, 0.3),
  b_FA_T ~ dnorm(0, 0.3),
  b_sshd_T ~ dnorm(0, 0.3),
  b_L_T ~ dnorm(0, 0.3),
  
  # Non-centered parameterization for rater effects
  z_rNV_T[rater] ~ dnorm(0, 1),  # Trustworthiness latent variable
  
  sigma_rater_T ~ dexp(1),  # Trustworthiness
  
  gq> vector[rater]:a_rNV_T <<- aT + z_rNV_T * sigma_rater_T,  # Generate rater effects for Trustworthiness
  
  # Varying effects for Trustworthiness morpho-predictors
  transpars> matrix[FSMUi, 6]:f_per_group_pr_T <- compose_noncentered(sigma_pr_T, L_Rho_pr_T, z_pr_T),
  cholesky_factor_corr[6]:L_Rho_pr_T ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_T ~ normal(0, 1),
  vector[6]:sigma_pr_T ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_T <<- Chol_to_Corr(L_Rho_pr_T),
  
  # Covariance between Attractiveness and Trustworthiness
  sigma_T ~ dexp(1)
  
), data=D_AT_M, iter=1000, sample=T, cores=8, chains=8, log_lik=T)


# DOM - type I - without per-face varying terms - mind to add Log_lik = T 
# DOM - type I - without per-face varying terms - mind to add Log_lik = T 
# DOM - type I - without per-face varying terms - mind to add Log_lik = T 
# DOM - type I - without per-face varying terms - mind to add Log_lik = T 
# DOM - type I - without per-face varying terms - mind to add Log_lik = T 
# DOM - type I - without per-face varying terms - mind to add Log_lik = T 
# DOM - type I - without per-face varying terms - mind to add Log_lik = T 
# DOM - type I - without per-face varying terms - mind to add Log_lik = T 
# DOM - type I - without per-face varying terms - mind to add Log_lik = T 
# DOM - type I - without per-face varying terms - mind to add Log_lik = T 
# DOM - type I - without per-face varying terms - mind to add Log_lik = T 


Dom_short_M <- ulam(alist(
  
  # Multivariate normal for Trustworthiness and Attractiveness
  DomRating ~ normal(muD, sigma_D),
  
  # Mean structure for Trustworthiness and Attractiveness
  muD <- z_rNV_D[rater] * sigma_rater_D
  + (aD + f_per_group_pr_D[FSMUi,1])
  + (b_age_D + f_per_group_pr_D[FSMUi, 2]) * Age
  + (b_dist_D + f_per_group_pr_D[FSMUi, 3]) * dist
  + (b_FA_D + f_per_group_pr_D[FSMUi, 4]) * FA
  + (b_sshd_D + f_per_group_pr_D[FSMUi, 5]) * sshd
  + (b_L_D + f_per_group_pr_D[FSMUi,6]) * L, 
  
  # Priors for Trustworthiness intercepts
  aD ~ dnorm(0, 0.5),  # Trustworthiness
  
  # Priors for Attractiveness and Trustworthiness slopes
  b_age_D ~ dnorm(0, 0.3),
  b_dist_D ~ dnorm(0, 0.3),
  b_FA_D ~ dnorm(0, 0.3),
  b_sshd_D ~ dnorm(0, 0.3),
  b_L_D ~ dnorm(0, 0.3),
  
  # Non-centered parameterization for rater effects
  z_rNV_D[rater] ~ dnorm(0, 1),  # Trustworthiness latent variable
  
  sigma_rater_D ~ dexp(1),  # Trustworthiness
  
  gq> vector[rater]:a_rNV_D <<- aD + z_rNV_D * sigma_rater_D,  # Generate rater effects for Trustworthiness
  
  # Varying effects for Trustworthiness morpho-predictors
  transpars> matrix[FSMUi, 6]:f_per_group_pr_D <- compose_noncentered(sigma_pr_D, L_Rho_pr_D, z_pr_D),
  cholesky_factor_corr[6]:L_Rho_pr_D ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_D ~ normal(0, 1),
  vector[6]:sigma_pr_D ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_D <<- Chol_to_Corr(L_Rho_pr_D),
  
  # Covariance between Attractiveness and Trustworthiness
  sigma_D ~ dexp(1)
  
), data=D_AT_M, iter=1000, sample=T, cores=8, chains=8, log_lik=T)


post_M_Atr <- extract.samples(Attr_short_M)
post_M_Tru <- extract.samples(Trustw_short_M)
post_M_Dom <- extract.samples(Dom_short_M)

saveRDS(post_M_Atr, file="post_M_Atr_23_08_25.RDS")
saveRDS(post_M_Tru, file="post_M_Tru_23_08_25.RDS")
saveRDS(post_M_Dom, file="post_M_Dom_23_08_25.RDS")


WAIC_Attr_short_M <- WAIC(Attr_short_M)
write.table(WAIC_Attr_short_M, file="WAIC_Attr_short_M.txt")

WAIC_Tru_short_M <- WAIC(Trustw_short_M)
write.table(WAIC_Tru_short_M, file="WAIC_Trustw_short_M.txt")

WAIC_Dom_short_M <- WAIC(Dom_short_M)
write.table(WAIC_Dom_short_M, file="WAIC_Dom_short_M.txt")


save(Attr_short_M, file="mod_M_short_Attr_23_08_25.Rdata")
save(Trustw_short_M, file="mod_M_short_Trustw_23_08_25.Rdata")
save(Dom_short_M, file="mod_M_short_Dom_23_08_25.Rdata")



# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T
# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T
# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T
# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T
# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T
# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T
# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T
# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T
# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T
# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T
# ATTR - type II - with per-face Varying terms - mind to add Log_lik = T

# II.A - MEN

Attr_long_M <- ulam(alist(
  
  # Multivariate normal for Attractiveness
  AtrRating ~ normal(muA, sigma_A),
  
  # Mean structure for Attractiveness
  muA <- z_rNV_A[rater] * sigma_rater_A
  + f_per_group_A[face, FSMUi] # per face - per culture
  + (aA + f_per_group_pr_A[FSMUi,1])
  + (b_age_A + f_per_group_pr_A[FSMUi, 2]) * Age
  + (b_dist_A + f_per_group_pr_A[FSMUi, 3]) * dist
  + (b_FA_A + f_per_group_pr_A[FSMUi, 4]) * FA
  + (b_sshd_A + f_per_group_pr_A[FSMUi, 5]) * sshd
  + (b_L_A + f_per_group_pr_A[FSMUi,6]) * L, 
  
  # Priors for Attractiveness  intercept
  aA ~ dnorm(0, 0.5),  # Attractiveness
  
  # Priors for Attractiveness and slopes
  b_age_A ~ dnorm(0, 0.3),
  b_dist_A ~ dnorm(0, 0.3),
  b_FA_A ~ dnorm(0, 0.3),
  b_sshd_A ~ dnorm(0, 0.3),
  b_L_A ~ dnorm(0, 0.3),
  
  # Non-centered parameterization for rater effects
  z_rNV_A[rater] ~ dnorm(0, 1),  # Attractiveness latent variable
  
  sigma_rater_A ~ dexp(1),  # Attractiveness
  
  gq> vector[rater]:a_rNV_A <<- aA + z_rNV_A * sigma_rater_A,  # Generate rater effects for Attractiveness
  
  # Varying effects for Attractiveness morpho-predictors
  transpars> matrix[FSMUi, 6]:f_per_group_pr_A <- compose_noncentered(sigma_pr_A, L_Rho_pr_A, z_pr_A),
  cholesky_factor_corr[6]:L_Rho_pr_A ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_A ~ normal(0, 1),
  vector[6]:sigma_pr_A ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_A <<- Chol_to_Corr(L_Rho_pr_A),
  
  # Priors for the multivariate normal distribution for face intercepts across groups
  transpars> matrix[face, 12]:f_per_group_A <- compose_noncentered(sigma_FSMUi_A, L_Rho_FSMUi_A, z_FSMUi_A), 
  cholesky_factor_corr[12]:L_Rho_FSMUi_A ~ lkj_corr_cholesky(2),
  matrix[12, face]:z_FSMUi_A ~ normal(0, 1),
  vector[12]:sigma_FSMUi_A ~ dexp(1),
  
  gq> matrix[12, 12]:Rho_FSMUi_A <<- Chol_to_Corr(L_Rho_FSMUi_A),
  
  # Covariance between Attractiveness and Trustworthiness
  sigma_A ~ dexp(1)
  
), data=D_AT_M, iter=1000, sample=T, cores=8, chains=8, log_lik=T)



# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 
# TRUSTW - type II - with per-face varying terms - mind to add Log_lik = T 

Trustw_long_M <- ulam(alist(
  
  # Multivariate normal for Trustworthiness and Attractiveness
  TrustwRating ~ normal(muT, sigma_T),
  
  # Mean structure for Trustworthiness and Attractiveness
  muT <- z_rNV_T[rater] * sigma_rater_T
  + f_per_group_T[face, FSMUi] # per face - per culture
  + (aT + f_per_group_pr_T[FSMUi,1])
  + (b_age_T + f_per_group_pr_T[FSMUi, 2]) * Age
  + (b_dist_T + f_per_group_pr_T[FSMUi, 3]) * dist
  + (b_FA_T + f_per_group_pr_T[FSMUi, 4]) * FA
  + (b_sshd_T + f_per_group_pr_T[FSMUi, 5]) * sshd
  + (b_L_T + f_per_group_pr_T[FSMUi, 6]) * L, 
  
  # Priors for Trustworthiness intercepts
  aT ~ dnorm(0, 0.5),  # Trustworthiness
  
  # Priors for Attractiveness and Trustworthiness slopes
  b_age_T ~ dnorm(0, 0.3),
  b_dist_T ~ dnorm(0, 0.3),
  b_FA_T ~ dnorm(0, 0.3),
  b_sshd_T ~ dnorm(0, 0.3),
  b_L_T ~ dnorm(0, 0.3),
  
  # Non-centered parameterization for rater effects
  z_rNV_T[rater] ~ dnorm(0, 1),  # Trustworthiness latent variable
  
  sigma_rater_T ~ dexp(1),  # Trustworthiness
  
  gq> vector[rater]:a_rNV_T <<- aT + z_rNV_T * sigma_rater_T,  # Generate rater effects for Trustworthiness
  
  # Varying effects for Trustworthiness morpho-predictors
  transpars> matrix[FSMUi, 6]:f_per_group_pr_T <- compose_noncentered(sigma_pr_T, L_Rho_pr_T, z_pr_T),
  cholesky_factor_corr[6]:L_Rho_pr_T ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_T ~ normal(0, 1),
  vector[6]:sigma_pr_T ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_T <<- Chol_to_Corr(L_Rho_pr_T),
  
  # Priors for the multivariate normal distribution for face intercepts across groups
  transpars> matrix[face, 12]:f_per_group_T <- compose_noncentered(sigma_FSMUi_T, L_Rho_FSMUi_T, z_FSMUi_T), 
  cholesky_factor_corr[12]:L_Rho_FSMUi_T ~ lkj_corr_cholesky(2),
  matrix[12, face]:z_FSMUi_T ~ normal(0, 1),
  vector[12]:sigma_FSMUi_T ~ dexp(1),
  
  gq> matrix[12, 12]:Rho_FSMUi_T <<- Chol_to_Corr(L_Rho_FSMUi_T),
  
  # Covariance between Attractiveness and Trustworthiness
  sigma_T ~ dexp(1)
  
), data=D_AT_M, iter=1000, sample=T, cores=8, chains=8, log_lik=T)


# DOM - type II - with per-face varying terms - mind to add Log_lik = T 
# DOM - type II - with per-face varying terms - mind to add Log_lik = T 
# DOM - type II - with per-face varying terms - mind to add Log_lik = T 
# DOM - type II - with per-face varying terms - mind to add Log_lik = T 
# DOM - type II - with per-face varying terms - mind to add Log_lik = T 
# DOM - type II - with per-face varying terms - mind to add Log_lik = T 
# DOM - type II - with per-face varying terms - mind to add Log_lik = T 
# DOM - type II - with per-face varying terms - mind to add Log_lik = T 
# DOM - type II - with per-face varying terms - mind to add Log_lik = T 
# DOM - type II - with per-face varying terms - mind to add Log_lik = T 
# DOM - type II - with per-face varying terms - mind to add Log_lik = T 

Dom_long_M <- ulam(alist(
  
  # Multivariate normal for Trustworthiness and Attractiveness
  DomRating ~ normal(muD, sigma_D),
  
  # Mean structure for Trustworthiness and Attractiveness
  muD <- z_rNV_D[rater] * sigma_rater_D
  + f_per_group_D[face, FSMUi] # per face - per culture
  + (aD + f_per_group_pr_D[FSMUi,1])
  + (b_age_D + f_per_group_pr_D[FSMUi, 2]) * Age
  + (b_dist_D + f_per_group_pr_D[FSMUi, 3]) * dist
  + (b_FA_D + f_per_group_pr_D[FSMUi, 4]) * FA
  + (b_sshd_D + f_per_group_pr_D[FSMUi, 5]) * sshd
  + (b_L_D + f_per_group_pr_D[FSMUi, 6]) * L, 
  
  # Priors for Trustworthiness intercepts
  aD ~ dnorm(0, 0.5),  # Trustworthiness
  
  # Priors for Attractiveness and Trustworthiness slopes
  b_age_D ~ dnorm(0, 0.3),
  b_dist_D ~ dnorm(0, 0.3),
  b_FA_D ~ dnorm(0, 0.3),
  b_sshd_D ~ dnorm(0, 0.3),
  b_L_D ~ dnorm(0, 0.3),
  
  # Non-centered parameterization for rater effects
  z_rNV_D[rater] ~ dnorm(0, 1),  # Trustworthiness latent variable
  
  sigma_rater_D ~ dexp(1),  # Trustworthiness
  
  gq> vector[rater]:a_rNV_D <<- aD + z_rNV_D * sigma_rater_D,  # Generate rater effects for Trustworthiness
  
  # Varying effects for Trustworthiness morpho-predictors
  transpars> matrix[FSMUi, 6]:f_per_group_pr_D <- compose_noncentered(sigma_pr_D, L_Rho_pr_D, z_pr_D),
  cholesky_factor_corr[6]:L_Rho_pr_D ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_D ~ normal(0, 1),
  vector[6]:sigma_pr_D ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_D <<- Chol_to_Corr(L_Rho_pr_D),
  
  # Priors for the multivariate normal distribution for face intercepts across groups
  transpars> matrix[face, 12]:f_per_group_D <- compose_noncentered(sigma_FSMUi_D, L_Rho_FSMUi_D, z_FSMUi_D), 
  cholesky_factor_corr[12]:L_Rho_FSMUi_D ~ lkj_corr_cholesky(2),
  matrix[12, face]:z_FSMUi_D ~ normal(0, 1),
  vector[12]:sigma_FSMUi_D ~ dexp(1),
  
  gq> matrix[12, 12]:Rho_FSMUi_D <<- Chol_to_Corr(L_Rho_FSMUi_D),
  
  # Covariance between Attractiveness and Trustworthiness
  sigma_D ~ dexp(1)
  
), data=D_AT_M, iter=1000, sample=T, cores=8, chains=8, log_lik=T)



compare(Attr_short_M, Attr_long_M)
compare(Trustw_short_M, Trustw_long_M)
compare(Dom_short_M, Dom_long_M)


post_M_Atr_Long <- extract.samples(Attr_long_M)
post_M_Tru_Long <- extract.samples(Trustw_long_M)
post_M_Dom_Long <- extract.samples(Dom_long_M)

saveRDS(post_M_Atr_Long, file="post_M_Atr_23_08_25_Long.RDS")
saveRDS(post_M_Tru_Long, file="post_M_Tru_23_08_25_Long.RDS")
saveRDS(post_M_Dom_Long, file="post_M_Dom_23_08_25_Long.RDS")



WAIC_Attr_long_M <- WAIC(Attr_long_M)
write.table(WAIC_Attr_long_M, file="WAIC_Attr_long_M.txt")

WAIC_Tru_long_M <- WAIC(Trustw_long_M)
write.table(WAIC_Tru_long_M, file="WAIC_Tru_long_M.txt")

WAIC_Dom_long_M <- WAIC(Dom_long_M)
write.table(WAIC_Dom_long_M, file="WAIC_Dom_long_M.txt")


save(Attr_long_M, file="mod_M_long_Attr_23_08_25.Rdata")
save(Trustw_long_M, file="mod_M_long_Trustw_23_08_25.Rdata")
save(Dom_long_M, file="mod_M_long_Dom_23_08_25.Rdata")




#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 25 % excluded...
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.


d <- d2

d$Atr <- as.numeric(d$Atr)
d$Tru <- as.numeric(d$Tru)
d$DIST <- as.numeric(d$DIST)
d$SShD <- as.numeric(d$SShD)
d$Asym <- as.numeric(d$Asym)
d$L <- as.numeric(d$L)

summary(as.data.frame(d))

# Exclude those, who scored "in the middle" 
summary(as.factor(d$NearMedian25_verbal))

d <- d[d$NearMedian25_verbal=="Outside",]

# Females (beware, wrong object was subsetted here, perhaps): 
d_F <- d[d$StimSex == "_F",]
d_M <- d[d$StimSex == "_M",]


###########
#         #
#  F_25   #
#         #
###########

d <- d_F

D_AT_F <- list(
  TrustwRating = stanTRU(d$Tru),
  AtrRating = stanATTR(d$Atr),
  DomRating = stanDOM(d$Dom),
  FSMUi = as.numeric(as.factor(d$FSMUi)), # Sample complete is: CZ Orig, CZ New Below, CZ New Above... 
  face = as.numeric(as.factor(d$Face_ID)),
  rater= as.numeric(as.factor(d$Particip_ID)),
  Age = stanAge(as.numeric(d$Age)), # Age of the stimuli
  dist = stanDIST(as.numeric(d$DIST)), # facial distinctiveness of the stimuli (morphometric measure)
  sshd = stanSShD(as.numeric(d$SShD)), # facial sextypicality of the stimuli (morphometric measure)
  FA = stanFA(as.numeric(d$Asym)), # facial asymmetry (morphometric measure)
  N_raters = length(unique(d$Particip_ID)),
  L = stanL(as.numeric(d$L))# Add the total number of raters
)

summary.data.frame(D_AT_F)

table(d_F$FSMUi,D_AT_F$FSMUi)

#              1    2    3    4    5    6    7    8    9   10   11   12
# AUS Above 1048    0    0    0    0    0    0    0    0    0    0    0
# AUS Below    0 1048    0    0    0    0    0    0    0    0    0    0
# COL Above    0    0  636    0    0    0    0    0    0    0    0    0
# COL Below    0    0    0  636    0    0    0    0    0    0    0    0
# CZ Above     0    0    0    0 2994    0    0    0    0    0    0    0
# CZ Below     0    0    0    0    0 3054    0    0    0    0    0    0
# RSA Above    0    0    0    0    0    0  948    0    0    0    0    0
# RSA Below    0    0    0    0    0    0    0  954    0    0    0    0
# TUR Above    0    0    0    0    0    0    0    0 1602    0    0    0
# TUR Below    0    0    0    0    0    0    0    0    0 1572    0    0
# VN Above     0    0    0    0    0    0    0    0    0    0 1404    0
# VN Below     0    0    0    0    0    0    0    0    0    0    0 1446

summary.data.frame(D_AT_F)

library(rethinking)

# MODEL for females
ATD_25_F <- ulam(alist(
  
  # Multivariate normal for Trustworthiness and Attractiveness
  c(AtrRating, TrustwRating, DomRating) ~ multi_normal(c(muA, muT, muD), Rho_Scales, sigma_Scales),
  
  
  # ATTRACTIVENESS
  muA <- z_rNV_A[rater] * sigma_rater_A
  + f_per_group_A[face, FSMUi] # per face - per culture
  + (aA + f_per_group_pr_A[FSMUi,1])
  + (b_age_A + f_per_group_pr_A[FSMUi, 2]) * Age
  + (b_dist_A + f_per_group_pr_A[FSMUi, 3]) * dist
  + (b_FA_A + f_per_group_pr_A[FSMUi, 4]) * FA
  + (b_sshd_A + f_per_group_pr_A[FSMUi, 5]) * sshd
  + (b_L_A + f_per_group_pr_A[FSMUi,6]) * L, 
  
  aA ~ dnorm(0, 0.5),  # 
  
  b_age_A ~ dnorm(0, 0.3),
  b_dist_A ~ dnorm(0, 0.3),
  b_FA_A ~ dnorm(0, 0.3),
  b_sshd_A ~ dnorm(0, 0.3),
  b_L_A ~ dnorm(0, 0.3),
  
  # Non-centered parameterization for rater effects
  z_rNV_A[rater] ~ dnorm(0, 1),  #  latent variable
  
  sigma_rater_A ~ dexp(1),  # Attractiveness
  
  gq> vector[rater]:a_rNV_A <<- aA + z_rNV_A * sigma_rater_A,  # Generate rater effects for Attractiveness
  
  # Varying effects for Attractiveness morpho-predictors
  transpars> matrix[FSMUi, 6]:f_per_group_pr_A <- compose_noncentered(sigma_pr_A, L_Rho_pr_A, z_pr_A),
  cholesky_factor_corr[6]:L_Rho_pr_A ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_A ~ normal(0, 1),
  vector[6]:sigma_pr_A ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_A <<- Chol_to_Corr(L_Rho_pr_A),
  
  # Priors for the multivariate normal distribution for face intercepts across groups
  transpars> matrix[face, 12]:f_per_group_A <- compose_noncentered(sigma_FSMUi_A, L_Rho_FSMUi_A, z_FSMUi_A), 
  cholesky_factor_corr[12]:L_Rho_FSMUi_A ~ lkj_corr_cholesky(2),
  matrix[12, face]:z_FSMUi_A ~ normal(0, 1),
  vector[12]:sigma_FSMUi_A ~ dexp(1),
  
  gq> matrix[12, 12]:Rho_FSMUi_A <<- Chol_to_Corr(L_Rho_FSMUi_A),
  
  
  # TRUSTWORTHINESS
  muT <- z_rNV_T[rater] * sigma_rater_T
  + f_per_group_T[face, FSMUi] # per face - per culture
  + (aT + f_per_group_pr_T[FSMUi,1])
  + (b_age_T + f_per_group_pr_T[FSMUi, 2]) * Age
  + (b_dist_T + f_per_group_pr_T[FSMUi, 3]) * dist
  + (b_FA_T + f_per_group_pr_T[FSMUi, 4]) * FA
  + (b_sshd_T + f_per_group_pr_T[FSMUi, 5]) * sshd
  + (b_L_T + f_per_group_pr_T[FSMUi,6]) * L, 
  
  aT ~ dnorm(0, 0.5),  
  
  b_age_T ~ dnorm(0, 0.3),
  b_dist_T ~ dnorm(0, 0.3),
  b_FA_T ~ dnorm(0, 0.3),
  b_sshd_T ~ dnorm(0, 0.3),
  b_L_T ~ dnorm(0, 0.3),
  
  z_rNV_T[rater] ~ dnorm(0, 1),  # Trustworthiness latent variable
  
  sigma_rater_T ~ dexp(1),  
  
  gq> vector[rater]:a_rNV_T <<- aT + z_rNV_T * sigma_rater_T,  # Generate rater effects for Trustworthiness
  
  # Varying effects for Trustworthiness morpho-predictors
  transpars> matrix[FSMUi, 6]:f_per_group_pr_T <- compose_noncentered(sigma_pr_T, L_Rho_pr_T, z_pr_T),
  cholesky_factor_corr[6]:L_Rho_pr_T ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_T ~ normal(0, 1),
  vector[6]:sigma_pr_T ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_T <<- Chol_to_Corr(L_Rho_pr_T),
  
  transpars> matrix[face, 12]:f_per_group_T <- compose_noncentered(sigma_FSMUi_T, L_Rho_FSMUi_T, z_FSMUi_T), 
  cholesky_factor_corr[12]:L_Rho_FSMUi_T ~ lkj_corr_cholesky(2),
  matrix[12, face]:z_FSMUi_T ~ normal(0, 1),
  vector[12]:sigma_FSMUi_T ~ dexp(1),
  
  gq> matrix[12, 12]:Rho_FSMUi_T <<- Chol_to_Corr(L_Rho_FSMUi_T),
  
  
  # DOMINANCE
  muD <- z_rNV_D[rater] * sigma_rater_D
  + f_per_group_D[face, FSMUi] # per face - per culture
  + (aD + f_per_group_pr_D[FSMUi,1])
  + (b_age_D + f_per_group_pr_D[FSMUi, 2]) * Age
  + (b_dist_D + f_per_group_pr_D[FSMUi, 3]) * dist
  + (b_FA_D + f_per_group_pr_D[FSMUi, 4]) * FA
  + (b_sshd_D + f_per_group_pr_D[FSMUi, 5]) * sshd
  + (b_L_D + f_per_group_pr_D[FSMUi,6]) * L, 
  
  aD ~ dnorm(0, 0.5),  
  
  b_age_D ~ dnorm(0, 0.3),
  b_dist_D ~ dnorm(0, 0.3),
  b_FA_D ~ dnorm(0, 0.3),
  b_sshd_D ~ dnorm(0, 0.3),
  b_L_D ~ dnorm(0, 0.3),
  
  # Non-centered parameterization for rater effects
  z_rNV_D[rater] ~ dnorm(0, 1),  #  latent variable
  
  sigma_rater_D ~ dexp(1),  # Dominance
  
  gq> vector[rater]:a_rNV_D <<- aD + z_rNV_D * sigma_rater_D,  
  
  transpars> matrix[FSMUi, 6]:f_per_group_pr_D <- compose_noncentered(sigma_pr_D, L_Rho_pr_D, z_pr_D),
  cholesky_factor_corr[6]:L_Rho_pr_D ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_D ~ normal(0, 1),
  vector[6]:sigma_pr_D ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_D <<- Chol_to_Corr(L_Rho_pr_D),
  
  # Priors for the multivariate normal distribution for face intercepts across groups
  transpars> matrix[face, 12]:f_per_group_D <- compose_noncentered(sigma_FSMUi_D, L_Rho_FSMUi_D, z_FSMUi_D), 
  cholesky_factor_corr[12]:L_Rho_FSMUi_D ~ lkj_corr_cholesky(2),
  matrix[12, face]:z_FSMUi_D ~ normal(0, 1),
  vector[12]:sigma_FSMUi_D ~ dexp(1),
  
  gq> matrix[12, 12]:Rho_FSMUi_D <<- Chol_to_Corr(L_Rho_FSMUi_D),
  
  
  # Common terms
  Rho_Scales ~ lkj_corr(2),
  sigma_Scales ~ dexp(1)
  
), data=D_AT_F, iter=1250, sample=T, cores=8, chains=8)


post_F_ATD_25 <- extract.samples(ATD_25_F)
saveRDS(post_F_ATD_25, file="post_F_ATD_out25_25_08_25_Long.RDS")
save(ATD_25_F, file="mod_F_ATD_out25_25_08_25_Long.RDS")
save(ATD_25_F, file="mod_F_ATD_out25_25_08_25_Long.Rdata")




###########
#         #
#  M_25   #
#         #
###########

d <- d_M

D_AT_M <- list(
  TrustwRating = stanTRU(d$Tru),
  AtrRating = stanATTR(d$Atr),
  DomRating = stanDOM(d$Dom),
  FSMUi = as.numeric(as.factor(d$FSMUi)), # Sample complete is: CZ Orig, CZ New Below, CZ New Above... 
  face = as.numeric(as.factor(d$Face_ID)),
  rater= as.numeric(as.factor(d$Particip_ID)),
  Age = stanAge(as.numeric(d$Age)), # Age of the stimuli
  dist = stanDIST(as.numeric(d$DIST)), # facial distinctiveness of the stimuli (morphometric measure)
  sshd = stanSShD(as.numeric(d$SShD)), # facial sextypicality of the stimuli (morphometric measure)
  FA = stanFA(as.numeric(d$Asym)), # facial asymmetry (morphometric measure)
  N_raters = length(unique(d$Particip_ID)),
  L = stanL(as.numeric(d$L))# Add the total number of raters
)

summary.data.frame(D_AT_M)

table(d_M$FSMUi,D_AT_M$FSMUi)

#              1    2    3    4    5    6    7    8    9   10   11   12
# AUS Above  912    0    0    0    0    0    0    0    0    0    0    0
# AUS Below    0  912    0    0    0    0    0    0    0    0    0    0
# COL Above    0    0  534    0    0    0    0    0    0    0    0    0
# COL Below    0    0    0  534    0    0    0    0    0    0    0    0
# CZ Above     0    0    0    0 2586    0    0    0    0    0    0    0
# CZ Below     0    0    0    0    0 2476    0    0    0    0    0    0
# RSA Above    0    0    0    0    0    0  812    0    0    0    0    0
# RSA Below    0    0    0    0    0    0    0  801    0    0    0    0
# TUR Above    0    0    0    0    0    0    0    0 1313    0    0    0
# TUR Below    0    0    0    0    0    0    0    0    0 1368    0    0
# VN Above     0    0    0    0    0    0    0    0    0    0 1251    0
# VN Below     0    0    0    0    0    0    0    0    0    0    0 1174

summary.data.frame(D_AT_M)

library(rethinking)


ATD_25_M <- ulam(alist(
  
  # Multivariate normal for Trustworthiness and Attractiveness
  c(AtrRating, TrustwRating, DomRating) ~ multi_normal(c(muA, muT, muD), Rho_Scales, sigma_Scales),
  
  
  # ATTRACTIVENESS
  muA <- z_rNV_A[rater] * sigma_rater_A
  + f_per_group_A[face, FSMUi] # per face - per culture
  + (aA + f_per_group_pr_A[FSMUi,1])
  + (b_age_A + f_per_group_pr_A[FSMUi, 2]) * Age
  + (b_dist_A + f_per_group_pr_A[FSMUi, 3]) * dist
  + (b_FA_A + f_per_group_pr_A[FSMUi, 4]) * FA
  + (b_sshd_A + f_per_group_pr_A[FSMUi, 5]) * sshd
  + (b_L_A + f_per_group_pr_A[FSMUi,6]) * L, 
  
  aA ~ dnorm(0, 0.5),  # 
  
  b_age_A ~ dnorm(0, 0.3),
  b_dist_A ~ dnorm(0, 0.3),
  b_FA_A ~ dnorm(0, 0.3),
  b_sshd_A ~ dnorm(0, 0.3),
  b_L_A ~ dnorm(0, 0.3),
  
  # Non-centered parameterization for rater effects
  z_rNV_A[rater] ~ dnorm(0, 1),  #  latent variable
  
  sigma_rater_A ~ dexp(1),  # Attractiveness
  
  gq> vector[rater]:a_rNV_A <<- aA + z_rNV_A * sigma_rater_A,  # Generate rater effects for Attractiveness
  
  # Varying effects for Attractiveness morpho-predictors
  transpars> matrix[FSMUi, 6]:f_per_group_pr_A <- compose_noncentered(sigma_pr_A, L_Rho_pr_A, z_pr_A),
  cholesky_factor_corr[6]:L_Rho_pr_A ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_A ~ normal(0, 1),
  vector[6]:sigma_pr_A ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_A <<- Chol_to_Corr(L_Rho_pr_A),
  
  # Priors for the multivariate normal distribution for face intercepts across groups
  transpars> matrix[face, 12]:f_per_group_A <- compose_noncentered(sigma_FSMUi_A, L_Rho_FSMUi_A, z_FSMUi_A), 
  cholesky_factor_corr[12]:L_Rho_FSMUi_A ~ lkj_corr_cholesky(2),
  matrix[12, face]:z_FSMUi_A ~ normal(0, 1),
  vector[12]:sigma_FSMUi_A ~ dexp(1),
  
  gq> matrix[12, 12]:Rho_FSMUi_A <<- Chol_to_Corr(L_Rho_FSMUi_A),
  
  
  # TRUSTWORTHINESS
  muT <- z_rNV_T[rater] * sigma_rater_T
  + f_per_group_T[face, FSMUi] # per face - per culture
  + (aT + f_per_group_pr_T[FSMUi,1])
  + (b_age_T + f_per_group_pr_T[FSMUi, 2]) * Age
  + (b_dist_T + f_per_group_pr_T[FSMUi, 3]) * dist
  + (b_FA_T + f_per_group_pr_T[FSMUi, 4]) * FA
  + (b_sshd_T + f_per_group_pr_T[FSMUi, 5]) * sshd
  + (b_L_T + f_per_group_pr_T[FSMUi,6]) * L, 
  
  aT ~ dnorm(0, 0.5),  
  
  b_age_T ~ dnorm(0, 0.3),
  b_dist_T ~ dnorm(0, 0.3),
  b_FA_T ~ dnorm(0, 0.3),
  b_sshd_T ~ dnorm(0, 0.3),
  b_L_T ~ dnorm(0, 0.3),
  
  z_rNV_T[rater] ~ dnorm(0, 1),  # Trustworthiness latent variable
  
  sigma_rater_T ~ dexp(1),  
  
  gq> vector[rater]:a_rNV_T <<- aT + z_rNV_T * sigma_rater_T,  # Generate rater effects for Trustworthiness
  
  # Varying effects for Trustworthiness morpho-predictors
  transpars> matrix[FSMUi, 6]:f_per_group_pr_T <- compose_noncentered(sigma_pr_T, L_Rho_pr_T, z_pr_T),
  cholesky_factor_corr[6]:L_Rho_pr_T ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_T ~ normal(0, 1),
  vector[6]:sigma_pr_T ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_T <<- Chol_to_Corr(L_Rho_pr_T),
  
  transpars> matrix[face, 12]:f_per_group_T <- compose_noncentered(sigma_FSMUi_T, L_Rho_FSMUi_T, z_FSMUi_T), 
  cholesky_factor_corr[12]:L_Rho_FSMUi_T ~ lkj_corr_cholesky(2),
  matrix[12, face]:z_FSMUi_T ~ normal(0, 1),
  vector[12]:sigma_FSMUi_T ~ dexp(1),
  
  gq> matrix[12, 12]:Rho_FSMUi_T <<- Chol_to_Corr(L_Rho_FSMUi_T),
  
  
  # DOMINANCE
  muD <- z_rNV_D[rater] * sigma_rater_D
  + f_per_group_D[face, FSMUi] # per face - per culture
  + (aD + f_per_group_pr_D[FSMUi,1])
  + (b_age_D + f_per_group_pr_D[FSMUi, 2]) * Age
  + (b_dist_D + f_per_group_pr_D[FSMUi, 3]) * dist
  + (b_FA_D + f_per_group_pr_D[FSMUi, 4]) * FA
  + (b_sshd_D + f_per_group_pr_D[FSMUi, 5]) * sshd
  + (b_L_D + f_per_group_pr_D[FSMUi,6]) * L, 
  
  aD ~ dnorm(0, 0.5),  
  
  b_age_D ~ dnorm(0, 0.3),
  b_dist_D ~ dnorm(0, 0.3),
  b_FA_D ~ dnorm(0, 0.3),
  b_sshd_D ~ dnorm(0, 0.3),
  b_L_D ~ dnorm(0, 0.3),
  
  # Non-centered parameterization for rater effects
  z_rNV_D[rater] ~ dnorm(0, 1),  #  latent variable
  
  sigma_rater_D ~ dexp(1),  # Dominance
  
  gq> vector[rater]:a_rNV_D <<- aD + z_rNV_D * sigma_rater_D,  
  
  transpars> matrix[FSMUi, 6]:f_per_group_pr_D <- compose_noncentered(sigma_pr_D, L_Rho_pr_D, z_pr_D),
  cholesky_factor_corr[6]:L_Rho_pr_D ~ lkj_corr_cholesky(2),
  matrix[6, FSMUi]:z_pr_D ~ normal(0, 1),
  vector[6]:sigma_pr_D ~ dexp(1),
  
  gq> matrix[6, 6]:Rho_pr_D <<- Chol_to_Corr(L_Rho_pr_D),
  
  # Priors for the multivariate normal distribution for face intercepts across groups
  transpars> matrix[face, 12]:f_per_group_D <- compose_noncentered(sigma_FSMUi_D, L_Rho_FSMUi_D, z_FSMUi_D), 
  cholesky_factor_corr[12]:L_Rho_FSMUi_D ~ lkj_corr_cholesky(2),
  matrix[12, face]:z_FSMUi_D ~ normal(0, 1),
  vector[12]:sigma_FSMUi_D ~ dexp(1),
  
  gq> matrix[12, 12]:Rho_FSMUi_D <<- Chol_to_Corr(L_Rho_FSMUi_D),
  
  
  # Common terms
  Rho_Scales ~ lkj_corr(2),
  sigma_Scales ~ dexp(1)
  
), data=D_AT_M, iter=700, sample=T, cores=14, chains=14)


post_M_ATD_25 <- extract.samples(ATD_25_M)
saveRDS(post_M_ATD_25, file="post_M_ATD_out25_25_08_25_Long.RDS")
save(ATD_25_M, file="mod_M_ATD_out25_25_08_25_Long.RDS")
save(ATD_25_M, file="mod_M_ATD_out25_25_08_25_Long.Rdata")




