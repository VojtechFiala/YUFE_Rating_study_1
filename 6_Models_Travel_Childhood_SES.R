# Models for YUFE4postdocs study one: 
# "Now-final" 

# Final models: 

# DATA in Long Format...
d1 <- read.csv("Whole_Dataset_long_GMM_INCLUDED_SIDESCENTRE_INCLUDED_SOCBACK_TRAVELABROAD_INCL_05_05_25.csv",T, dec=",")

d2 <- d1

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

#-------------------------------------------------------------------------------
#  Let's go for ScoreAbroad first
#-------------------------------------------------------------------------------

# ScoreAbroad - binarise: 

summary(as.factor(d2$ScoreAbroad))

# Leave NAs out (don't forget to start with d1 again for family soc. back.)
d2 <- d2[!is.na(d2$ScoreAbroad),]

above_sc <- c("Often","Rather often","Occasionally")

d2$ScoreAbroad2 <- ifelse(d2$ScoreAbroad %in% above_sc, "Above","Below")
summary(as.factor(d2$ScoreAbroad2))

d2$FSMUi <- paste(d2$Sample,d2$ScoreAbroad2)
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

#              1    2    3    4    5    6    7    8    9   10   11   12
# AUS Above 1160    0    0    0    0    0    0    0    0    0    0    0
# AUS Below    0 1640    0    0    0    0    0    0    0    0    0    0
# COL Above    0    0  100    0    0    0    0    0    0    0    0    0
# COL Below    0    0    0  692    0    0    0    0    0    0    0    0
# CZ Above     0    0    0    0 6172    0    0    0    0    0    0    0
# CZ Below     0    0    0    0    0 1878    0    0    0    0    0    0
# RSA Above    0    0    0    0    0    0  686    0    0    0    0    0
# RSA Below    0    0    0    0    0    0    0 1808    0    0    0    0
# TUR Above    0    0    0    0    0    0    0    0 1640    0    0    0
# TUR Below    0    0    0    0    0    0    0    0    0 2550    0    0
# VN Above     0    0    0    0    0    0    0    0    0    0 1422    0
# VN Below     0    0    0    0    0    0    0    0    0    0    0 2232

summary.data.frame(D_AT_F)

library(rethinking)

# Run it all with GMM already - there's no need to avoid them...

# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T
# ATTR - type I - without per-face Varying terms - mind to add Log_lik = T

# NOTE: The design of the model is pretty much the same like for Above/Below social media use intensity: 
# Why - well it just makes sense! 

# And... the analysis is exploratory, so no WAIC, unless I am told the opposite: 

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
saveRDS(post_F_ATD_Long, file="post_F_ATD_SCORE_ABROAD.RDS")



# MEN! 

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
# AUS Above  990    0    0    0    0    0    0    0    0    0    0    0
# AUS Below    0 1385    0    0    0    0    0    0    0    0    0    0
# COL Above    0    0  100    0    0    0    0    0    0    0    0    0
# COL Below    0    0    0  573    0    0    0    0    0    0    0    0
# CZ Above     0    0    0    0 5118    0    0    0    0    0    0    0
# CZ Below     0    0    0    0    0 1657    0    0    0    0    0    0
# RSA Above    0    0    0    0    0    0  584    0    0    0    0    0
# RSA Below    0    0    0    0    0    0    0 1502    0    0    0    0
# TUR Above    0    0    0    0    0    0    0    0 1385    0    0    0
# TUR Below    0    0    0    0    0    0    0    0    0 2125    0    0
# VN Above     0    0    0    0    0    0    0    0    0    0 1218    0
# VN Below     0    0    0    0    0    0    0    0    0    0    0 1858


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
saveRDS(post_M_ATD_Long, file="post_M_ATD_SCORE_ABROAD.RDS")




#-------------------------------------------------------------------------------
#  Now: Soc-Back - version 1 - "Rich" and "Upper CL" together...
#-------------------------------------------------------------------------------

# ScoreAbroad - binarise: 
# Mind that you got rid of NAs that need not be NAs now: 
d2 <- d1

summary(as.factor(d2$Fami_Back))

# Leave NAs out (don't forget to start with d1 again for family soc. back.)
d2 <- d2[!is.na(d2$Fami_Back),]

# Those who prefer not to report also stinks: 
d2 <- d2[d2$Fami_Back!="PreferNR",]

above_sc <- c("Rich","Upper CL")

d2$Fami_Back2 <- ifelse(d2$Fami_Back %in% above_sc, "Above","Below")
summary(as.factor(d2$Fami_Back2))

d2$FSMUi <- paste(d2$Sample,d2$Fami_Back2)
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

#               1    2    3    4    5    6    7    8    9   10   11   12
#  AUS Above 1578    0    0    0    0    0    0    0    0    0    0    0
#  AUS Below    0 1172    0    0    0    0    0    0    0    0    0    0
#  COL Above    0    0  106    0    0    0    0    0    0    0    0    0
#  COL Below    0    0    0  742    0    0    0    0    0    0    0    0
#  CZ Above     0    0    0    0 2344    0    0    0    0    0    0    0
#  CZ Below     0    0    0    0    0 5706    0    0    0    0    0    0
#  RSA Above    0    0    0    0    0    0  804    0    0    0    0    0
#  RSA Below    0    0    0    0    0    0    0 1690    0    0    0    0
#  TUR Above    0    0    0    0    0    0    0    0 2506    0    0    0
#  TUR Below    0    0    0    0    0    0    0    0    0 1366    0    0
#  VN Above     0    0    0    0    0    0    0    0    0    0 1852    0
#  VN Below     0    0    0    0    0    0    0    0    0    0    0 1260

summary.data.frame(D_AT_F)

library(rethinking)



ATD_long_F_2 <- ulam(alist(
  
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


post_F_ATD_Long_2 <- extract.samples(ATD_long_F_2)
saveRDS(post_F_ATD_Long_2, file="post_F_ATD_FAMI_BACK_two_Upper.RDS")


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# MEN 

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

#               1    2    3    4    5    6    7    8    9   10   11   12
#  AUS Above 1357    0    0    0    0    0    0    0    0    0    0    0
#  AUS Below    0  968    0    0    0    0    0    0    0    0    0    0
#  COL Above    0    0   89    0    0    0    0    0    0    0    0    0
#  COL Below    0    0    0  623    0    0    0    0    0    0    0    0
#  CZ Above     0    0    0    0 1936    0    0    0    0    0    0    0
#  CZ Below     0    0    0    0    0 4839    0    0    0    0    0    0
#  RSA Above    0    0    0    0    0    0  651    0    0    0    0    0
#  RSA Below    0    0    0    0    0    0    0 1435    0    0    0    0
#  TUR Above    0    0    0    0    0    0    0    0 2064    0    0    0
#  TUR Below    0    0    0    0    0    0    0    0    0 1179    0    0
#  VN Above     0    0    0    0    0    0    0    0    0    0 1563    0
#  VN Below     0    0    0    0    0    0    0    0    0    0    0 1090


ATD_long_M_2 <- ulam(alist(
  
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

post_M_ATD_Long_2 <- extract.samples(ATD_long_M_2)
saveRDS(post_M_ATD_Long_2, file="post_M_ATD_FAMI_BACK_two_Upper.RDS")





#-------------------------------------------------------------------------------
#  Now: Soc-Back - version 2 - "Rich", "Upper CL", and "Middle" together...
#-------------------------------------------------------------------------------

# ScoreAbroad - binarise: 
# Mind that you got rid of NAs that need not be NAs now: 
d2 <- d1

summary(as.factor(d2$Fami_Back))

# Leave NAs out (don't forget to start with d1 again for family soc. back.)
d2 <- d2[!is.na(d2$Fami_Back),]

# Those who prefer not to report also stinks: 
d2 <- d2[d2$Fami_Back!="PreferNR",]

above_sc <- c("Rich", "Upper CL", "Middle")

d2$Fami_Back2 <- ifelse(d2$Fami_Back %in% above_sc, "Above","Below")
summary(as.factor(d2$Fami_Back2))

d2$FSMUi <- paste(d2$Sample,d2$Fami_Back2)
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

#               1    2    3    4    5    6    7    8    9   10   11   12
#  AUS Above 2164    0    0    0    0    0    0    0    0    0    0    0
#  AUS Below    0  586    0    0    0    0    0    0    0    0    0    0
#  COL Above    0    0  430    0    0    0    0    0    0    0    0    0
#  COL Below    0    0    0  418    0    0    0    0    0    0    0    0
#  CZ Above     0    0    0    0 5630    0    0    0    0    0    0    0
#  CZ Below     0    0    0    0    0 2420    0    0    0    0    0    0
#  RSA Above    0    0    0    0    0    0 2008    0    0    0    0    0
#  RSA Below    0    0    0    0    0    0    0  486    0    0    0    0
#  TUR Above    0    0    0    0    0    0    0    0 3304    0    0    0
#  TUR Below    0    0    0    0    0    0    0    0    0  568    0    0
#  VN Above     0    0    0    0    0    0    0    0    0    0 2214    0
#  VN Below     0    0    0    0    0    0    0    0    0    0    0  898

summary.data.frame(D_AT_F)

ATD_long_F_2_2 <- ulam(alist(
  
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


post_ATD_long_F_2_2 <- extract.samples(ATD_long_F_2_2)
saveRDS(post_ATD_long_F_2_2, file="post_F_ATD_FAMI_BACK_three_Upper.RDS")


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# MEN 

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

#               1    2    3    4    5    6    7    8    9   10   11   12
#  AUS Above 1841    0    0    0    0    0    0    0    0    0    0    0
#  AUS Below    0  484    0    0    0    0    0    0    0    0    0    0
#  COL Above    0    0  345    0    0    0    0    0    0    0    0    0
#  COL Below    0    0    0  367    0    0    0    0    0    0    0    0
#  CZ Above     0    0    0    0 4695    0    0    0    0    0    0    0
#  CZ Below     0    0    0    0    0 2080    0    0    0    0    0    0
#  RSA Above    0    0    0    0    0    0 1702    0    0    0    0    0
#  RSA Below    0    0    0    0    0    0    0  384    0    0    0    0
#  TUR Above    0    0    0    0    0    0    0    0 2726    0    0    0
#  TUR Below    0    0    0    0    0    0    0    0    0  517    0    0
#  VN Above     0    0    0    0    0    0    0    0    0    0 1891    0
#  VN Below     0    0    0    0    0    0    0    0    0    0    0  762


ATD_long_M_2_2 <- ulam(alist(
  
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

post_ATD_long_M_2_2 <- extract.samples(ATD_long_M_2_2)
saveRDS(post_ATD_long_M_2_2, file="post_M_ATD_FAMI_BACK_three_Upper.RDS")


save.image("M_A_T_M_F_ATD_SCORE_ABROAD_FAMI_BACK_1_2.Rdata")