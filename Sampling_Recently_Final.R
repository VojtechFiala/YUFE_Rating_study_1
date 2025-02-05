# Models for YUFE4postdocs study one: 
# "Now-final" 

# Final models: 

# DATA in Long Format...
d <- read.csv2("1Whole_Dataset_long_GMM_INCLUDED_SIDESCENTRE_INCLUDED_17_01_25.csv",T, dec=",")
summary(as.data.frame(d))

summary(as.factor(is.na(d$Atr)))

d$Atr <- as.numeric(d$Atr)

stanATTR <- function(x){(x-mean(d$Atr))/sd(d$Atr)}
unstanATTR <- function(x){x*sd(d$Atr)+mean(d$Atr)}

stanTRU <- function(x){(x-mean(d$Tru))/sd(d$Tru)}
unstanTRU <- function(x){x*sd(d$Tru)+mean(d$Tru)}

stanL <- function(x){(x-mean(d$L))/sd(d$L)}
unstanL <- function(x){x*sd(d$L)+mean(d$L)}

stanSShD <- function(x){(x-mean(d$SShD))/sd(d$SShD)}
unstanSShD <- function(x){x*sd(d$SShD)+mean(d$SShD)}

stanDIST <- function(x){(x-mean(d$DIST))/sd(d$DIST)}
unstanDIST <- function(x){x*sd(d$DIST)+mean(d$DIST)}

stanFA <- function(x){(x-mean(d$Asym))/sd(d$Asym)}
unstanFA <- function(x){x*sd(d$Asym)+mean(d$Asym)}

stanAge <- function(x){(x-mean(d$Age))/sd(d$Age)}
unstanAge <- function(x){x*sd(d$Age)+mean(d$Age)}

summary(d$Atr)

d$FSMUi <- paste(d$Sample,d$Above_Below)
summary(as.factor(d$FSMUi))

# Subset of relevant variables as a list to be fed in the model: 
D_AT <- list(
  TrustwRating = scale(d$Tru),
  AtrRating = scale(as.numeric(d$Atr)),
  FSMUi = as.numeric(as.factor(d$FSMUi)), # Sample complete is: CZ Orig, CZ New Below, CZ New Above... 
  BSMUi = ifelse(d$Above_Below=="Above",0.5,ifelse(d$Above_Below=="Below",-0.5,0)), # 0.5 and -0.5 for the groups RSA and CZ new, 0 for the group from before...
  face = as.numeric(as.factor(d$Face_ID)),
  rater= as.numeric(as.factor(d$Particip_ID)),
  Age = scale(as.numeric(d$Age)), # Age of the stimuli
  dist = scale(as.numeric(d$DIST)), # facial distinctiveness of the stimuli (morphometric measure)
  sshd = scale(as.numeric(d$SShD)), # facial sextypicality of the stimuli (morphometric measure)
  FA = scale(as.numeric(d$Asym)), # facial asymmetry (morphometric measure)
  N_raters = length(unique(d$Particip_ID)),
  L = scale(as.numeric(d$L))# Add the total number of raters
)

summary.data.frame(D_AT)

D_AT <- list(
  TrustwRating = stanTRU(d$Tru),
  AtrRating = stanATTR(d$Atr),
  FSMUi = as.numeric(as.factor(d$FSMUi)), # Sample complete is: CZ Orig, CZ New Below, CZ New Above... 
  BSMUi = ifelse(d$Above_Below=="Above",0.5,ifelse(d$Above_Below=="Below",-0.5,0)), # 0.5 and -0.5 for the groups RSA and CZ new, 0 for the group from before...
  face = as.numeric(as.factor(d$Face_ID)),
  rater= as.numeric(as.factor(d$Particip_ID)),
  Age = stanAge(as.numeric(d$Age)), # Age of the stimuli
  dist = stanDIST(as.numeric(d$DIST)), # facial distinctiveness of the stimuli (morphometric measure)
  sshd = stanSShD(as.numeric(d$SShD)), # facial sextypicality of the stimuli (morphometric measure)
  FA = stanFA(as.numeric(d$Asym)), # facial asymmetry (morphometric measure)
  N_raters = length(unique(d$Particip_ID)),
  L = stanL(as.numeric(d$L))# Add the total number of raters
)

summary.data.frame(D_AT)


table(d$FSMUi,D_AT$FSMUi)

#              1     2     3     4     5     6     7     8     9    10    11    12    13
#AUS Above  2635     0     0     0     0     0     0     0     0     0     0     0     0
#AUS Below     0  2540     0     0     0     0     0     0     0     0     0     0     0
#COL Above     0     0   785     0     0     0     0     0     0     0     0     0     0
#COL Below     0     0     0   775     0     0     0     0     0     0     0     0     0
#CZ Above      0     0     0     0  7545     0     0     0     0     0     0     0     0
#CZ Below      0     0     0     0     0  7280     0     0     0     0     0     0     0
#CZ_orig 0     0     0     0     0     0     0 75386     0     0     0     0     0     0
#RSA Above     0     0     0     0     0     0     0  2435     0     0     0     0     0
#RSA Below     0     0     0     0     0     0     0     0  2145     0     0     0     0
#TUR Above     0     0     0     0     0     0     0     0     0  3790     0     0     0
#TUR Below     0     0     0     0     0     0     0     0     0     0  3910     0     0
#VN Above      0     0     0     0     0     0     0     0     0     0     0  3630     0
#VN Below      0     0     0     0     0     0     0     0     0     0     0     0  3395

summary.data.frame(D_AT)

library(rethinking)


#######
#     #
#  1  #
#     #
#######

# ATTR + TRUSTW - together: Type "without GMM"

# 

# This model has been run on 24/25th of January, 2025

#

CZRSAUS_AT_TW_5 <- ulam(alist(
  c(TrustwRating, AtrRating) ~ multi_normal(c(muA,muT), Rho_Scales, sigma_Scales),
  
  muA <- a_A + a_group_A[FSMUi] + f_per_group_A[face,FSMUi] + z_A[rater]*sigma_rater_A, 
  muT <- a_T + a_group_T[FSMUi] + f_per_group_T[face,FSMUi] + z_T[rater]*sigma_rater_T, 
  
  
  # Big intercept for Country_1_orig
  a_A ~ dnorm(0, 0.5),
  a_T ~ dnorm(0, 0.5),
  
  # intercept change per country
  a_group_A[FSMUi] ~ dnorm(0, 0.5),
  a_group_T[FSMUi] ~ dnorm(0, 0.5),
  
  z_A[rater] ~ normal(0,1), 
  sigma_rater_A ~ dexp(1),
  
  gq > vector[rater]:a_rater_A <<- a_A + z_A*sigma_rater_A,
  
  z_T[rater] ~ normal(0,1), 
  sigma_rater_T ~ dexp(1),
  
  gq > vector[rater]:a_rater_T <<- a_T + z_T*sigma_rater_T,
  
  # Group-level increases per face in attractiveness (here the correlation matrix shall be inspected to see similarities in ratings)
  transpars> matrix[face,9]:f_per_group_A <- compose_noncentered(sigma_FSMUi_A,L_Rho_FSMUi_A,z_FSMUi_A), 
  
  # Priors for multivariate normal distribution parameters - Face/FSMUi
  cholesky_factor_corr[9]:L_Rho_FSMUi_A ~ lkj_corr_cholesky(2),
  matrix[9,face]:z_FSMUi_A ~ normal(0, 1),
  vector[9]:sigma_FSMUi_A ~ dexp(1),
  
  # Face and rater varying effects
  gq> matrix[9,9]:Rho_FSMUi_A <<- Chol_to_Corr(L_Rho_FSMUi_A),
  
  # Group-level increases per face in attractiveness (here the correlation matrix shall be inspected to see similarities in ratings)
  transpars> matrix[face,9]:f_per_group_T <- compose_noncentered(sigma_FSMUi_T,L_Rho_FSMUi_T,z_FSMUi_T), 
  
  # Priors for multivariate normal distribution parameters - Face/FSMUi
  cholesky_factor_corr[9]:L_Rho_FSMUi_T ~ lkj_corr_cholesky(2),
  matrix[9,face]:z_FSMUi_T ~ normal(0, 1),
  vector[9]:sigma_FSMUi_T ~ dexp(1),
  
  # Face and rater varying effects
  gq> matrix[9,9]:Rho_FSMUi_T <<- Chol_to_Corr(L_Rho_FSMUi_T),
  
  # Uppermost
  sigma_Scales ~ dexp(1),
  Rho_Scales ~ lkj_corr(2)
  
), data=D_AT, iter=500, sample=T, cores=20, chains=20)

# Outcomes: 
str(CZRSAUS_AT_TW_5)

save(CZRSAUS_AT_TW_5, file="CZRSAUS_Atr_Tw_CORREL_FACES_23_01_25.Rdata")  # _A_ stands for "Attractiveness" 
CZRSAUS_AT_TW_5_COEFS <- precis(CZRSAUS_AT_TW_5, depth=3)

write.csv2(CZRSAUS_AT_TW_5_COEFS, file="CZRS_Atr_Tw_3_CORREL_FACES_COEFS_09_10_24.Rdata.csv")

save.image("my_workspace1.RData")


#######
#     #
#  2  #
#     #
#######

# ATTR + TRUSTW - together: Type "with GMM", age and skin lightness. It awaits evaluation. 

CZRSAUS_TW_4 <- ulam(alist(
  
  # Multivariate normal for Trustworthiness and Attractiveness
  c(TrustwRating, AtrRating) ~ multi_normal(c(muT, muA), Rho_Scales, sigma_Scales),
  
  # Mean structure for Trustworthiness and Attractiveness
  muT <- aT + a_group_T[FSMUi]
  + z_rNV_T[rater] * sigma_rater_T
  + f_per_group_T[face,FSMUi]
  + (b_age_T + f_per_group_pr_T[FSMUi, 1]) * Age
  + (b_dist_T + f_per_group_pr_T[FSMUi, 2]) * dist
  + (b_FA_T + f_per_group_pr_T[FSMUi, 3]) * FA
  + (b_sshd_T + f_per_group_pr_T[FSMUi, 4]) * sshd
  + (b_L_T + f_per_group_pr_T[FSMUi,5]) * L, 
  
  muA <- aA + a_group_A[FSMUi]
  + z_rNV_A[rater] * sigma_rater_A
  + f_per_group_A[face,FSMUi]
  + (b_age_A + f_per_group_pr_A[FSMUi, 1]) * Age
  + (b_dist_A + f_per_group_pr_A[FSMUi, 2]) * dist
  + (b_FA_A + f_per_group_pr_A[FSMUi, 3]) * FA
  + (b_sshd_A + f_per_group_pr_A[FSMUi, 4]) * sshd
  + (b_L_A + f_per_group_pr_A[FSMUi,5]) * L, 
  
  # Priors for Attractiveness and Trustworthiness intercepts
  aT ~ dnorm(0, 0.5),  # Trustworthiness
  aA ~ dnorm(0, 0.5),  # Attractiveness
  
  # Priors for Attractiveness and Trustworthiness slopes
  b_age_T ~ dnorm(0, 0.3),
  b_dist_T ~ dnorm(0, 0.3),
  b_FA_T ~ dnorm(0, 0.3),
  b_sshd_T ~ dnorm(0, 0.3),
  b_L_T ~ dnorm(0, 0.3),
  
  b_age_A ~ dnorm(0, 0.3),
  b_dist_A ~ dnorm(0, 0.3),
  b_FA_A ~ dnorm(0, 0.3),
  b_sshd_A ~ dnorm(0, 0.3),
  b_L_A ~ dnorm(0, 0.3),
  
  # Intercept change per country for both Trustworthiness and Attractiveness
  a_group_T[FSMUi] ~ dnorm(0, 0.5),  # Trustworthiness
  a_group_A[FSMUi] ~ dnorm(0, 0.5),  # Attractiveness
  
  # Non-centered parameterization for rater effects
  z_rNV_T[rater] ~ dnorm(0, 1),  # Trustworthiness latent variable
  z_rNV_A[rater] ~ dnorm(0, 1),  # Attractiveness latent variable
  
  sigma_rater_T ~ dexp(1),  # Trustworthiness
  sigma_rater_A ~ dexp(1),  # Attractiveness
  
  gq> vector[rater]:a_rNV_T <<- aT + z_rNV_T * sigma_rater_T,  # Generate rater effects for Trustworthiness
  gq> vector[rater]:a_rNV_A <<- aA + z_rNV_A * sigma_rater_A,  # Generate rater effects for Attractiveness
  
  # Varying effects for Trustworthiness morpho-predictors
  transpars> matrix[FSMUi, 5]:f_per_group_pr_T <- compose_noncentered(sigma_pr_T, L_Rho_pr_T, z_pr_T),
  cholesky_factor_corr[5]:L_Rho_pr_T ~ lkj_corr_cholesky(2),
  matrix[5, FSMUi]:z_pr_T ~ normal(0, 1),
  vector[5]:sigma_pr_T ~ dexp(1),
  
  gq> matrix[5, 5]:Rho_pr_T <<- Chol_to_Corr(L_Rho_pr_T),
  
  # Varying effects for Attractiveness morpho-predictors
  transpars> matrix[FSMUi, 5]:f_per_group_pr_A <- compose_noncentered(sigma_pr_A, L_Rho_pr_A, z_pr_A),
  cholesky_factor_corr[5]:L_Rho_pr_A ~ lkj_corr_cholesky(2),
  matrix[5, FSMUi]:z_pr_A ~ normal(0, 1),
  vector[5]:sigma_pr_A ~ dexp(1),
  
  gq> matrix[5, 5]:Rho_pr_A <<- Chol_to_Corr(L_Rho_pr_A),
  
  # Priors for the multivariate normal distribution for face intercepts across groups
  transpars> matrix[face, 9]:f_per_group_T <- compose_noncentered(sigma_FSMUi_T, L_Rho_FSMUi_T, z_FSMUi_T), 
  cholesky_factor_corr[9]:L_Rho_FSMUi_T ~ lkj_corr_cholesky(2),
  matrix[9, face]:z_FSMUi_T ~ normal(0, 1),
  vector[9]:sigma_FSMUi_T ~ dexp(1),
  
  transpars> matrix[face, 9]:f_per_group_A <- compose_noncentered(sigma_FSMUi_A, L_Rho_FSMUi_A, z_FSMUi_A), 
  cholesky_factor_corr[9]:L_Rho_FSMUi_A ~ lkj_corr_cholesky(2),
  matrix[9, face]:z_FSMUi_A ~ normal(0, 1),
  vector[9]:sigma_FSMUi_A ~ dexp(1),
  
  gq> matrix[9, 9]:Rho_FSMUi_T <<- Chol_to_Corr(L_Rho_FSMUi_T),
  gq> matrix[9, 9]:Rho_FSMUi_A <<- Chol_to_Corr(L_Rho_FSMUi_A),
  
  # Covariance between Attractiveness and Trustworthiness
  sigma_Scales ~ dexp(1),
  Rho_Scales ~ lkj_corr(2)
  
), data=D_AT, iter=500, sample=T, cores=20, chains=20)

str(CZRSAUS_TW_4)

save(CZRSAUS_TW_4, file="CZRSAUS_Atr_Tru_6_CORREL_FACES_24_01_25_WITHMORPHMMS.Rdata")  # _A_ stands for "Attractiveness" 

CZRSAUS_TW_4_COES <- precis(CZRSAUS_TW_4, depth=3)
write.csv2(CZRSAUS_TW_4_COES, file="CZRS_Atr_Tru_6_CORREL_FACES_COEFS_09_10_24.Rdata.csv")

save.image("my_workspace2.RData")



library(rethinking)
load("my_workspace2.RData")
Precis1 <- precis(CZRSAUS_TW_4, depth=3)
write.csv2(Precis1, file="Precis1.csv")

post1 <- extract.samples(post1)

str(postCZRSAUS_AT_TW_5$Rho_FSMUi_A)
hist(postCZRSAUS_AT_TW_5$Rho_FSMUi_A[,1,3])

load("my_workspace1.RData")
precis(CZRSAUS_AT_TW_5, depth=3)
post1 <- extract.samples(CZRSAUS_AT_TW_5)
