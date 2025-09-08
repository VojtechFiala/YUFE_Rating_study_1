# From this script onwards, all the data are available in the compressed folder data.rar. 
# Please download, extract (in the same folder as this script) and proceed.

load("OLDEMOG.Rdata")

library(psych)
library(nFactors)
library(lavaan)


# Check for underlying factors behind the questions on social media use: 
# Recode EVERYTHING to 1 = lowest category...
# Whatever is the maximum = the highest category
# And save into a new data frame seldemog (selected demography)
OLDEMOG <- as.data.frame(OLDEMOG)

# Function to check the order of levels - considering NAs 
# The point was that Chat did not beleive (Likert) numbers saved as factors would 
# be transferred into correctly-ordered integers 
check_num <- function(x) { table(raw=x, coerced=as.numeric(x), useNA="ifany") }

check_num(OLDEMOG$SMUi_Q1)
check_num(OLDEMOG$SMUi_Q2)
check_num(OLDEMOG$SMUi_Q3)

check_num(OLDEMOG$SM_too_much_time)
check_num(OLDEMOG$SM_friends_better)
check_num(OLDEMOG$Miss_other)
check_num(OLDEMOG$FOMO_SM)

check_num(OLDEMOG$Facebook_AP)
check_num(OLDEMOG$YouTube_AP)
check_num(OLDEMOG$Instagram_AP)
check_num(OLDEMOG$Twitter_AP)
check_num(OLDEMOG$TikTok_AP)

check_num(OLDEMOG$Facebook_Freq)
check_num(OLDEMOG$YouTube_Freq)
check_num(OLDEMOG$Instagram_Freq)
check_num(OLDEMOG$X_Freq)
check_num(OLDEMOG$TikTok_Freq)

# Chat proposes this function, to recode, and I don't trust it. Therefore, I'd always compare its outcome
# with the one of conventional as.numeric(). 

# PS: I did and they always overlapped.

f2i <- function(x) {  # factor/character/labelled -> integer  (keeps NA)
  if (is.factor(x)) x <- as.character(x)
  suppressWarnings(as.integer(x))
}

# Let's create a data frame with selected variables, recoded:  
# We need IDs:
seldemog <- data.frame(IDs=OLDEMOG$ID)

# Two "maybe unrelated scores"
# 1) ScoreAbroad: Participant is shown: "I travel abroad" And there these six options: 
#     1 = Often 2 = Rather often 3 = Occasionally 4 = Rarely 5 = Very rarely 6 = Never 
summary(as.factor(OLDEMOG$ScoreAbroad)) 

# The order makes no sense (well, it's alphabetic) and "Empty" shall be "NA". 
# Recode manually: 
seldemog$travel_abroad <- ifelse(OLDEMOG$ScoreAbroad=="Empty",NA,
                                 ifelse(OLDEMOG$ScoreAbroad=="Never",1,
                                        ifelse(OLDEMOG$ScoreAbroad=="Very rarely",2,
                                               ifelse(OLDEMOG$ScoreAbroad=="Rarely",3,
                                                      ifelse(OLDEMOG$ScoreAbroad=="Occasionally",4,
                                                             ifelse(OLDEMOG$ScoreAbroad=="Rather often",5,6))))))

table(OLDEMOG$ScoreAbroad, seldemog$travel_abroad) # Empty was when participant left the question unaswered... which is NA
# 1 = never, 2 = very rarely,  3 = rarely, 4 = occasionally, 5 = rather often, 6 = often 
seldemog$travel_abroad # and NAs are here - all good. 

# 2) Fami_Back: Participant is shown: "In childhood, our family had:" 
#     Six options: 1 = Rich 2 = Upper CL (note: CL = class) 3 = Middle 4 = Lower CL 5 = Poor 6 = Not disclosed 

# 1=Rich … 6=Not disclosed -> set 6 to NA, and make higher=richer (optional)
summary(as.factor(OLDEMOG$Fami_Back))
OLDEMOG$Fami_Back[OLDEMOG$Fami_Back=="Empty"]<-"PreferNR" # Let's simplify - it will end up as NA anyway

seldemog$fami_back <- ifelse(OLDEMOG$Fami_Back=="PreferNR",NA,
                             ifelse(OLDEMOG$Fami_Back=="Poor",1,
                                    ifelse(OLDEMOG$Fami_Back=="Lower CL",2,
                                           ifelse(OLDEMOG$Fami_Back=="Middle",3,
                                                  ifelse(OLDEMOG$Fami_Back=="Upper CL",4,5)))))


table(OLDEMOG$Fami_Back, seldemog$fami_back)
seldemog$fami_back # The more the better...

# 3) Next, there were three questions on social media use intensity (SMU-i): We will recode and check
#   3-1) How often PER DAY do you look at social network sites? 
#     (for example, Facebook, X[Twitter], Instagram, TikTok or YouTube). 
#     1 = Never or less than once a day 2 = 1-2 times per day 3 = 3-5 times a day 
#     4 = 6-10 times a day 5 = 11-20 times a day 6 = 21-40 times a day 7 = more than 40 times
as.numeric(OLDEMOG$SMUi_Q1);f2i(OLDEMOG$SMUi_Q1);
cor(as.numeric(OLDEMOG$SMUi_Q1),f2i(OLDEMOG$SMUi_Q1), use="complete.obs")
seldemog$SMU_i_Boer_1 <- as.numeric(OLDEMOG$SMUi_Q1)

#   3-2) How often A WEEK do you post, photo or video on social network sites? 
#     (for example, Facebook, X[Twitter], Instagram, TikTok or YouTube). 
#     1 = Never or less than once a week 2 = 1-2 times per week 3 = 3-5 times a week 4 = 6-10 times a week 
#     5 = 11-20 times a week 6 = 21-40 times a week 7 = more than 40 times 
as.numeric(OLDEMOG$SMUi_Q2);f2i(OLDEMOG$SMUi_Q2)
cor(as.numeric(OLDEMOG$SMUi_Q2),f2i(OLDEMOG$SMUi_Q2), use="complete.obs")
seldemog$SMU_i_Boer_2 <- as.numeric(OLDEMOG$SMUi_Q2)

#   3-3) How often A WEEK you 'like' your posts, photos or videos of others on social network sites 
#     (e.g. Facebook, X[Twitter], Instagram, TikTok or YouTube). 1 = Never or less than once a week 
#     2 = 1-2 times per week 3 = 3-5 times a week 4 = 6-10 times a week 5 = 11-20 times a week 6 = 21-40 times a week 
#     7 = more than 40 times 4) Four questions on potential self-perceived negative effects of using social media 
as.numeric(OLDEMOG$SMUi_Q3);f2i(OLDEMOG$SMUi_Q3)
cor(as.numeric(OLDEMOG$SMUi_Q3),f2i(OLDEMOG$SMUi_Q3), use="complete.obs")
seldemog$SMU_i_Boer_3 <- as.numeric(OLDEMOG$SMUi_Q3)


## Negative-effect items: 
# BEWARE - it has already been recalculated so that higher number, higher self-perceived negative impact: 
#   4-1) "I spend a lot of time on online social media" 
#     1 = totally disagree, 2 = mostly disagree, 3 = undecided, 4 = mostly agree, 5 = totally agree 
as.numeric(OLDEMOG$SM_too_much_time);f2i(OLDEMOG$SM_too_much_time)
seldemog$negative_1 <- as.numeric(OLDEMOG$SM_too_much_time)

#   4-2) "I'm loosing interest in other hobbies because of online social media:" 
#     1 = totally disagree, 2 = mostly disagree, 3 = undecided, 4 = mostly agree, 5 = totally agree 
as.numeric(OLDEMOG$Miss_other);f2i(OLDEMOG$Miss_other) # 6 shall be NA:
OLDEMOG$Miss_other[117] <- NA # Here is one "six"
seldemog$negative_2 <- as.numeric(OLDEMOG$Miss_other)

#   4-3) "I prefer following people and events on social media to interactions outside of it" 
#     1 = totally disagree, 2 = mostly disagree, 3 = undecided, 4 = mostly agree, 5 = totally agree 
as.numeric(OLDEMOG$SM_friends_better);f2i(OLDEMOG$SM_friends_better)
seldemog$negative_3 <- as.numeric(OLDEMOG$SM_friends_better)

#   4-4) "If for some reason I can't visit social networks, I feel like I'm missing out:" 
#     1 = totally disagree, 2 = mostly disagree, 3 = undecided, 4 = mostly agree, 5 = totally agree 
as.numeric(OLDEMOG$FOMO_SM);f2i(OLDEMOG$FOMO_SM)
seldemog$negative_4 <- as.numeric(OLDEMOG$FOMO_SM)



# 5) Next, participants were asked whether they use social media actively or passively. 
#    In here, actively means "posting, commenting...", semi-actively means "only liking and commenting, 
#    not not posting own content", passively = "only watching the content, not reacting to it in any way, 
#    not posting own content" 

# ActivePassive won't be used for search for the underlying factor, but we may use it later.

# BEWARE - it has already been recalculated so that higher number, the more active the participation: 

#   5-1) Facebook: 1 = I don't use it, 2 = Passively, 3 = Semi-Actively, 4 = Actively. 
as.numeric(OLDEMOG$Facebook_AP);f2i(OLDEMOG$Facebook_AP)
seldemog$AP_FCB <- as.numeric(OLDEMOG$Facebook_AP)

#   5-2) YouTube: 1 = I don't use it, 2 = Passively, 3 = Semi-Actively, 4 = Actively.  
as.numeric(OLDEMOG$YouTube_AP);f2i(OLDEMOG$YouTube_AP)
seldemog$AP_YT <- as.numeric(OLDEMOG$YouTube_AP)

#   5-3) Instagram: 1 = I don't use it, 2 = Passively, 3 = Semi-Actively, 4 = Actively. 
as.numeric(OLDEMOG$Instagram_AP);f2i(OLDEMOG$Instagram_AP)
seldemog$AP_INS <- as.numeric(OLDEMOG$Instagram_AP)

#   5-4) TikTok: 1 = I don't use it, 2 = Passively, 3 = Semi-Actively, 4 = Actively. 
as.numeric(OLDEMOG$TikTok_AP);f2i(OLDEMOG$TikTok_AP)
seldemog$AP_TT <- as.numeric(OLDEMOG$TikTok_AP)

#   5-5) X[Twitter]: 1 = I don't use it, 2 = Passively, 3 = Semi-Actively, 4 = Actively. 
as.numeric(OLDEMOG$Twitter_AP);f2i(OLDEMOG$Twitter_AP)
seldemog$AP_XTW <- as.numeric(OLDEMOG$Twitter_AP)

#   5-6) WeChat: 1 = I don't use it, 2 = Passively, 3 = Semi-Actively, 4 = Actively. 
# NOT INCLUDED - Some local cooperators insisted on using different social media, which was... 
#   Plus, participants were given an option to report additional three social media based on their own report: 
#   5-7) Self-reported media #1 1 = I don't use it, 2 = Passively, 3 = Semi-Actively, 4 = Actively. 
# NOT INCLUDED
#   5-8) Self-reported media #2 1 = I don't use it, 2 = Passively, 3 = Semi-Actively, 4 = Actively. 
# NOT INCLUDED
#   5-9) Self-reported media #3 1 = I don't use it, 2 = Passively, 3 = Semi-Actively, 4 = Actively.  
# NOT INCLUDED
# Even when combined, no other social media appeared

# 6) Participants also reported: "How frequently do you use social media:" 
# There was no need to recode - just make sure numbers are numbers and NAs are NAs
#   6-1) Facebook: 1 = I do not use it, 2 = Once a week or less, 3 = Several times a day, not daily, 4 = Once a day, 
#        5 = Several times a day, 6 = Frequently during the day
as.numeric(OLDEMOG$Facebook_Freq);f2i(OLDEMOG$Facebook_Freq)
seldemog$Freq_FCB <- as.numeric(OLDEMOG$Facebook_Freq)

#   6-2) YouTube: 1 = I do not use it, 2 = Once a week or less, 3 = Several times a day, not daily, 4 = Once a day, 
#        5 = Several times a day, 6 = Frequently during the day 
as.numeric(OLDEMOG$YouTube_Freq);f2i(OLDEMOG$YouTube_Freq)
seldemog$Freq_YT <- as.numeric(OLDEMOG$YouTube_Freq)

#   6-3) Instagram: 1 = I do not use it, 2 = Once a week or less, 3 = Several times a day, not daily, 4 = Once a day, 
#        5 = Several times a day, 6 = Frequently during the day 
as.numeric(OLDEMOG$Instagram_Freq);f2i(OLDEMOG$Instagram_Freq)
seldemog$Freq_INS <- as.numeric(OLDEMOG$Instagram_Freq)

#   6-4) TikTok: 1 = I do not use it, 2 = Once a week or less, 3 = Several times a day, not daily, 4 = Once a day, 
#        5 = Several times a day, 6 = Frequently during the day 
as.numeric(OLDEMOG$TikTok_Freq);f2i(OLDEMOG$TikTok_Freq)
seldemog$Freq_TT <- as.numeric(OLDEMOG$TikTok_Freq)

#   6-5) X[Twitter]: 1 = I do not use it, 2 = Once a week or less, 3 = Several times a day, not daily, 4 = Once a day, 
#        5 = Several times a day, 6 = Frequently during the day 
as.numeric(OLDEMOG$X_Freq);f2i(OLDEMOG$X_Freq)
seldemog$Freq_XTW <- as.numeric(OLDEMOG$X_Freq)

#   6-6) WeChat: 1 = I do not use it, 2 = Once a week or less, 3 = Several times a day, not daily, 4 = Once a day, 
#        5 = Several times a day, 6 = Frequently during the day 
# NOT INCLUDE - same story as above

# Plus three self-reported media: 
#   Self-reported #1: 1 = I do not use it, 2 = Once a week or less, 3 = Several times a day, not daily, 
#     4 = Once a day, 5 = Several times a day, 6 = Frequently during the day 
# NOT INCLUDE
#   Self-reported #2: 1 = I do not use it, 2 = Once a week or less, 3 = Several times a day, not daily, 
#     4 = Once a day, 5 = Several times a day, 6 = Frequently during the day 
# NOT INCLUDE
#   Self-reported #3: 1 = I do not use it, 2 = Once a week or less, 3 = Several times a day, not daily, 
#     4 = Once a day, 5 = Several times a day, 6 = Frequently during the day
# NOT INCLUDE

summary.data.frame(seldemog)

# Participants on rows (14,43,330,384,394) marked they don't use social media at all. 
nonusers <- c(14,43,330,384,394) # ADD "Lowes possible values" from column 3 onwards: 

seldemog[nonusers, 4:ncol(seldemog)] <- 1

summary.data.frame(seldemog) # Plus - a subset of participants do not fill in the negative_2 to 4 and 
# AP_XTW / Freq_XTW. We will add average values for the participants. Given that the purpose of the analysis
# is to split the participants into two groups, it's likey that the rest of their responses 
# supports enough variance for that split. 

seldemog$negative_2[is.na(seldemog$negative_2)] <- round(mean(seldemog$negative_2, na.rm=T))
seldemog$negative_3[is.na(seldemog$negative_3)] <- round(mean(seldemog$negative_3, na.rm=T))
seldemog$negative_4[is.na(seldemog$negative_4)] <- round(mean(seldemog$negative_4, na.rm=T))

seldemog$AP_XTW[is.na(seldemog$AP_XTW)] <- round(mean(seldemog$AP_XTW, na.rm=T))
seldemog$Freq_XTW[is.na(seldemog$Freq_XTW)] <- round(mean(seldemog$Freq_XTW, na.rm=T))

summary.data.frame(seldemog)



# The dataset's ready, let's do some checks...

# 1) Values are within expected ranges
# SMU_i_Boer should be between 1 and 7
# negative_ should be between 1 and 5 
# AP shoud be between 1 and 6
rng <- sapply(seldemog, function(x) range(x, na.rm=TRUE))
print(rng)

# 2) SMU items are strictly integers 1..K
#    We should also introduce two types of smu_names_ object:
#    There are two attitudes to select the final variables: 
#  
#    smu_names_12 -> will be used when we combine both the SMU-i and 
#    negative use.
#    smu_names_8 -> this time, we are only considering variables 
#    that theoretically refer to social media use intensity (unlike negative use does)
smu_names_12 <- c("SMU_i_Boer_1","SMU_i_Boer_2","SMU_i_Boer_3",
               "Freq_FCB","Freq_YT","Freq_INS","Freq_TT","Freq_XTW",
               "negative_1","negative_2","negative_3","negative_4")
for (nm in smu_names_12) {
  cat("\n", nm, ":\n"); print(sort(unique(seldemog[[nm]]))) # cat -> "concatenate and print"
}

smu_names_8 <- c("SMU_i_Boer_1","SMU_i_Boer_2","SMU_i_Boer_3",
                  "Freq_FCB","Freq_YT","Freq_INS","Freq_TT","Freq_XTW")

# 3) Crosstabs to ensure coercion is sane
ct <- function(x,y) { print(table(x,y,useNA="ifany")) }
ct(OLDEMOG$SMUi_Q1, seldemog$SMU_i_Boer_1)
ct(OLDEMOG$Facebook_Freq, seldemog$Freq_FCB) # For these variables, NAs were substituted by the least possible answer = 1,
# since NA means = I do not use SM
# Is sane...



# Subset the variables into separate table
# We only use the three scales of Boer and Separate Social Media use frequency as these should 
# add to common factor. 
# Later on, we will add travel abroad and socioeconomic background + active / semi / passive / don't use, which
# can be treated both as ordered factor and as a combination of Y/N and ordered factor (i.e., no good)
sel12items <- seldemog[,c(4:10,16:20)]
summary.data.frame(sel12items)

sel8items <- seldemog[,c(4:6,16:20)]
summary.data.frame(sel8items)


## 1) Build the polychoric correlation matrix (handles 7-pt + 6-pt mix)
pc  <- polychoric(sel8items)   # expect benign warnings about unequal categories
rho <- cor.smooth(pc$rho)      # ensure positive-definite

## 2) Inspect eigenvalues (scree)
ev <- eigen(rho, symmetric = TRUE)$values
print(round(ev, 3))

# 3) We are interested in number of underlying factors:
fa.parallel(rho, n.obs = nrow(sel8items), fm="ml", fa="both")
# Parallel analysis suggests that the number of factors =  3(4)  and the number of components =  2 
# The the plot shows the analysis indeed "keep factors where the observed eigenvalues exceed the simulated ones"
# but we also see steep decline from 1 factor to two in eigenvalue (and subsequent plateau)
# Then Elbow method suggest one is the number of factors 

# Note - Chat (and Petr, and previous evidence) suggest we are indeed interested in factor (underlying hidden 
# variables that make sense with respect to psychology - they do measure something and, as such, are interpretable).
# We are not looking for principal components: Linear combinations of the observed variables, coined with the sole 
# goal of reducing variance -> "smaller set of linear combinations that explain the maximum variance". 
# We are indeed looking for a "hidden psychological construct" - self-perceived social media use whatever... 

## 4) Numeric decision aids (parallel + Kaiser + acceleration)
ap      <- parallel(subject = nrow(sel8items),
                    var = ncol(sel8items),
                    rep = 500, cent = .05)
ap_eig  <- ap$eigen$qevpea
kaiserK <- sum(ev > 1)                         # Kaiser (very rough)
paK     <- sum(ev > ap_eig)                    # Parallel analysis cutoff

cat("\nKaiser suggests:", kaiserK, "factor(s)\n")
cat("Parallel suggests:", paK, "factor(s)\n")
# It's two and two...
# But the plot, you know, still supports just one factor to my eyes...

# Let's check this out (the prediction is that the other factor(s) will be somehow silly when 
# it comes to their loading by the observable variance): 

# choose nfact from the parallel/scree; then:
efa1_8_ml <- fa(r = rho, nfactors = 1, fm = "ml", rotate = "oblimin")
# r = rho: This specifies the input data, which is a correlation matrix named rho. 
# nfactors = 1: This indicates that the analysis should extract exactly one factor. 
# fm = "uls": This sets the factor extraction method to unweighted least squares (ULS), also known 
#    as minimum residual (minres). This method aims to minimize the differences between the observed 
#    and reproduced correlation matrices. 
# rotate = "oblimin": This applies an oblique rotation to the factor solution. Oblique rotation is used 
#    when factors are expected to be correlated, allowing the rotated factor axes to be non-orthogonal 
#    (i.e., not at right angles) to improve the interpretability of the factor structure. 

efa2_8_ml <- fa(r = rho, nfactors = 2, fm = "ml", rotate = "oblimin")

# From here onwards it shouts: 
# Warning message:
#  In fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs,  :
#                The estimated weights for the factor scores are probably incorrect.  Try a different factor score estimation method.

# Mind that 3 and 4 factors is "too many".  
efa3_8_ml <- fa(r = rho, nfactors = 3, fm = "ml", rotate = "oblimin")
efa4_8_ml <- fa(r = rho, nfactors = 4, fm = "ml", rotate = "oblimin")

print(efa1_8_ml$loadings, cutoff=.10)
print(efa2_8_ml$loadings, cutoff=.10)
print(efa3_8_ml$loadings, cutoff=.10)
print(efa4_8_ml$loadings, cutoff=.10)

# While maximum likelihood is more conventional (and usually shouts no warnings),
# some guidelines (https://stats.stackexchange.com/questions/88995/ml-vs-wlsmv-which-is-better-for-categorical-data-and-why)
# (https://arxiv.org/pdf/2004.07579), suggest we should use Weighted Least Squares Mean and Variance-Adjusted estimation
# (WLSMV), which is fitted via fm = "uls": 


# 3) We are interested in number of underlying factors:
fa.parallel(rho, n.obs = nrow(sel8items), fm="uls", fa="both")

ap      <- parallel(subject = nrow(sel8items),
                    var = ncol(sel8items),
                    rep = 500, cent = .05)
ap_eig  <- ap$eigen$qevpea
kaiserK <- sum(ev > 1)                         # Kaiser (very rough)
paK     <- sum(ev > ap_eig)                    # Parallel analysis cutoff

cat("\nKaiser suggests:", kaiserK, "factor(s)\n")
cat("Parallel suggests:", paK, "factor(s)\n")

# choose nfact from the parallel/scree; then:
efa1_8_uls <- fa(r = rho, nfactors = 1, fm = "uls", rotate = "oblimin")
efa2_8_uls <- fa(r = rho, nfactors = 2, fm = "uls", rotate = "oblimin")
efa3_8_uls <- fa(r = rho, nfactors = 3, fm = "uls", rotate = "oblimin")
efa4_8_uls <- fa(r = rho, nfactors = 4, fm = "uls", rotate = "oblimin")

print(efa1_8_uls$loadings, cutoff=.10)
print(efa2_8_uls$loadings, cutoff=.10)
print(efa3_8_uls$loadings, cutoff=.10)
print(efa4_8_uls$loadings, cutoff=.10)

# One factor solution is clearly supported: 
# let's 
# (a) get the underlying factors from the FA objects: 
fa_fit_8_ml <- fa(sel8items, nfactors = 1, fm = "ml", rotate="oblimin")

# store scores in seldemog: 
seldemog$fa_8_ml <- fa_fit_8_ml$scores  


fa_fit_8_uls <- fa(sel8items, nfactors = 1, fm = "uls", rotate="oblimin")
seldemog$fa_8_uls <- fa_fit_8_uls$scores  

cor(seldemog$fa_8_ml, seldemog$fa_8_uls) # Not identical, but very close...


# (b) Run CFAs with the corresponding variables and isolate the model prediction, too: 

## declare them as ordered factors with fixed levels; this prevents accidental re-indexing 
## if some categories are missing.
for (nm in c("SMU_i_Boer_1","SMU_i_Boer_2","SMU_i_Boer_3")) {
  seldemog[[nm]] <- factor(seldemog[[nm]], levels = 1:7, ordered = TRUE)
}
## For per-platform frequency items (1..6):
for (nm in c("Freq_INS","Freq_TT","Freq_XTW")) {
  seldemog[[nm]] <- factor(seldemog[[nm]], levels = 1:6, ordered = TRUE)
}

####
# 1
####

# 7 variables that load >0.3
smu7_names <- c("SMU_i_Boer_1","SMU_i_Boer_2","SMU_i_Boer_3","Freq_INS","Freq_TT","Freq_XTW","Freq_YT")

model_1f <- '
  SMU =~ SMU_i_Boer_1 + SMU_i_Boer_2 + SMU_i_Boer_3 +
         Freq_INS + Freq_TT + Freq_XTW + Freq_YT
'

fit_1f <- cfa(model_1f, data = seldemog,
              ordered   = smu7_names,     # tell lavaan these are ordinal
              estimator = "WLSMV",        # robust for ordinal items
              parameterization = "theta", # recommended for ordinals
              std.lv    = TRUE)           # factor variance = 1; loadings comparable

summary(fit_1f, fit.measures = TRUE, standardized = TRUE)

mi <- modificationIndices(fit_1f, sort.=TRUE)

# Show the top candidates between observed indicators only:
mi_pairs <- mi[mi$op == "~~" & mi$lhs %in% smu7_names & mi$rhs %in% smu7_names, ]

mi_pairs

####
# 2
####

# Let's add covariance between SMU_i_Boer_1 and SMU_i_Boer_3 in the model:
# mi_pairs suggest it may be needed: 

model_1f_cov1 <- '
  SMU =~ SMU_i_Boer_1 + SMU_i_Boer_2 + SMU_i_Boer_3 +
         Freq_INS + Freq_TT + Freq_XTW + Freq_YT
  SMU_i_Boer_1 ~~ SMU_i_Boer_3
'

fit_1f_cov1 <- cfa(model_1f_cov1, data = seldemog,
                   ordered = smu7_names, estimator = "WLSMV",
                   parameterization = "theta", std.lv = TRUE)

summary(fit_1f_cov1, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_1f_cov1, c("cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr",
                           "cfi.scaled","tli.scaled","rmsea.scaled"))
fitMeasures(fit_1f, c("cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr",
                      "cfi.scaled","tli.scaled","rmsea.scaled"))


# Extract the principal values, informing about quality of the fit: 
fitMeasures(fit_1f, c("cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr","chisq","df","pvalue"))

lavTestLRT(fit_1f, fit_1f_cov1)  # significant Δχ² supports freeing the residual

# Standardised loadings matrix (expect all positive, ideally ≥ .40)
inspect(fit_1f_cov1, "std")$lambda # Yes

## Get factor scores
fs_8_no_cov <- lavPredict(fit_1f, method = "EBM")   # empirical Bayes scores (robust)
as.numeric(fs_8_no_cov); range(fs_8_no_cov)
seldemog$CFA_smu_score_8_no_covariance <- as.numeric(fs_8_no_cov)

## Get factor scores # 2
fs_8_cov <- lavPredict(fit_1f_cov1, method = "EBM")   # empirical Bayes scores (robust)
as.numeric(fs_8_cov); range(fs_8_cov)
seldemog$CFA_smu_score_8_covariance <- as.numeric(fs_8_cov)

cor(seldemog[,21:24]) # It's all nearly the same...



## 1) Build the polychoric correlation matrix (handles 7-pt + 6-pt mix)
pc  <- polychoric(sel8items)   # expect benign warnings about unequal categories
rho <- cor.smooth(pc$rho)      # ensure positive-definite

## 2) Inspect eigenvalues (scree)
ev <- eigen(rho, symmetric = TRUE)$values
print(round(ev, 3))

# 3) We are interested in number of underlying factors:
fa.parallel(rho, n.obs = nrow(sel8items), fm="ml", fa="both")
# Parallel analysis suggests that the number of factors =  3(4)  and the number of components =  2 
# The the plot shows the analysis indeed "keep factors where the observed eigenvalues exceed the simulated ones"
# but we also see steep decline from 1 factor to two in eigenvalue (and subsequent plateau)
# Then Elbow method suggest one is the number of factors 

# Note - Chat (and Petr, and previous evidence) suggest we are indeed interested in factor (underlying hidden 
# variables that make sense with respect to psychology - they do measure something and, as such, are interpretable).
# We are not looking for principal components: Linear combinations of the observed variables, coined with the sole 
# goal of reducing variance -> "smaller set of linear combinations that explain the maximum variance". 
# We are indeed looking for a "hidden psychological construct" - self-perceived social media use whatever... 

## 4) Numeric decision aids (parallel + Kaiser + acceleration)
ap      <- parallel(subject = nrow(sel8items),
                    var = ncol(sel8items),
                    rep = 500, cent = .05)
ap_eig  <- ap$eigen$qevpea
kaiserK <- sum(ev > 1)                         # Kaiser (very rough)
paK     <- sum(ev > ap_eig)                    # Parallel analysis cutoff

cat("\nKaiser suggests:", kaiserK, "factor(s)\n")
cat("Parallel suggests:", paK, "factor(s)\n")
# It's two and two...
# But the plot, you know, still supports just one factor to my eyes...

# Let's check this out (the prediction is that the other factor(s) will be somehow silly when 
# it comes to their loading by the observable variance): 

# choose nfact from the parallel/scree; then:
efa1_8_ml <- fa(r = rho, nfactors = 1, fm = "ml", rotate = "oblimin")
# r = rho: This specifies the input data, which is a correlation matrix named rho. 
# nfactors = 1: This indicates that the analysis should extract exactly one factor. 
# fm = "uls": This sets the factor extraction method to unweighted least squares (ULS), also known 
#    as minimum residual (minres). This method aims to minimize the differences between the observed 
#    and reproduced correlation matrices. 
# rotate = "oblimin": This applies an oblique rotation to the factor solution. Oblique rotation is used 
#    when factors are expected to be correlated, allowing the rotated factor axes to be non-orthogonal 
#    (i.e., not at right angles) to improve the interpretability of the factor structure. 

efa2_8_ml <- fa(r = rho, nfactors = 2, fm = "ml", rotate = "oblimin")

# From here onwards it shouts: 
# Warning message:
#  In fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs,  :
#                The estimated weights for the factor scores are probably incorrect.  Try a different factor score estimation method.

# Mind that 3 and 4 factors is "too many".  
efa3_8_ml <- fa(r = rho, nfactors = 3, fm = "ml", rotate = "oblimin")
efa4_8_ml <- fa(r = rho, nfactors = 4, fm = "ml", rotate = "oblimin")

print(efa1_8_ml$loadings, cutoff=.10)
print(efa2_8_ml$loadings, cutoff=.10)
print(efa3_8_ml$loadings, cutoff=.10)
print(efa4_8_ml$loadings, cutoff=.10)

# While maximum likelihood is more conventional (and usually shouts no warnings),
# some guidelines (https://stats.stackexchange.com/questions/88995/ml-vs-wlsmv-which-is-better-for-categorical-data-and-why)
# (https://arxiv.org/pdf/2004.07579), suggest we should use Weighted Least Squares Mean and Variance-Adjusted estimation
# (WLSMV), which is fitted via fm = "uls": 


# 3) We are interested in number of underlying factors:
fa.parallel(rho, n.obs = nrow(sel8items), fm="uls", fa="both")

ap      <- parallel(subject = nrow(sel8items),
                    var = ncol(sel8items),
                    rep = 500, cent = .05)
ap_eig  <- ap$eigen$qevpea
kaiserK <- sum(ev > 1)                         # Kaiser (very rough)
paK     <- sum(ev > ap_eig)                    # Parallel analysis cutoff

cat("\nKaiser suggests:", kaiserK, "factor(s)\n")
cat("Parallel suggests:", paK, "factor(s)\n")

# choose nfact from the parallel/scree; then:
efa1_8_uls <- fa(r = rho, nfactors = 1, fm = "uls", rotate = "oblimin")
efa2_8_uls <- fa(r = rho, nfactors = 2, fm = "uls", rotate = "oblimin")
efa3_8_uls <- fa(r = rho, nfactors = 3, fm = "uls", rotate = "oblimin")
efa4_8_uls <- fa(r = rho, nfactors = 4, fm = "uls", rotate = "oblimin")

print(efa1_8_uls$loadings, cutoff=.10)
print(efa2_8_uls$loadings, cutoff=.10)
print(efa3_8_uls$loadings, cutoff=.10)
print(efa4_8_uls$loadings, cutoff=.10)

# One factor solution is clearly supported: 
# let's 
# (a) get the underlying factors from the FA objects: 
fa_fit_8_ml <- fa(sel8items, nfactors = 1, fm = "ml", rotate="oblimin")

# store scores in seldemog: 
seldemog$fa_8_ml <- fa_fit_8_ml$scores  


fa_fit_8_uls <- fa(sel8items, nfactors = 1, fm = "uls", rotate="oblimin")
seldemog$fa_8_uls <- fa_fit_8_uls$scores  

cor(seldemog$fa_8_ml, seldemog$fa_8_uls) # Not identical, but very close...


# (b) Run CFAs with the corresponding variables and isolate the model prediction, too: 

## declare them as ordered factors with fixed levels; this prevents accidental re-indexing 
## if some categories are missing.
for (nm in c("SMU_i_Boer_1","SMU_i_Boer_2","SMU_i_Boer_3")) {
  seldemog[[nm]] <- factor(seldemog[[nm]], levels = 1:7, ordered = TRUE)
}
## For per-platform frequency items (1..6):
for (nm in c("Freq_INS","Freq_TT","Freq_XTW")) {
  seldemog[[nm]] <- factor(seldemog[[nm]], levels = 1:6, ordered = TRUE)
}

####
# 1
####

# 7 variables that load >0.3
smu7_names <- c("SMU_i_Boer_1","SMU_i_Boer_2","SMU_i_Boer_3","Freq_INS","Freq_TT","Freq_XTW","Freq_YT")

model_1f <- '
  SMU =~ SMU_i_Boer_1 + SMU_i_Boer_2 + SMU_i_Boer_3 +
         Freq_INS + Freq_TT + Freq_XTW + Freq_YT
'

fit_1f <- cfa(model_1f, data = seldemog,
              ordered   = smu7_names,     # tell lavaan these are ordinal
              estimator = "WLSMV",        # robust for ordinal items
              parameterization = "theta", # recommended for ordinals
              std.lv    = TRUE)           # factor variance = 1; loadings comparable

summary(fit_1f, fit.measures = TRUE, standardized = TRUE)

mi <- modificationIndices(fit_1f, sort.=TRUE)

# Show the top candidates between observed indicators only:
mi_pairs <- mi[mi$op == "~~" & mi$lhs %in% smu7_names & mi$rhs %in% smu7_names, ]

mi_pairs

####
# 2
####

# Let's add covariance between SMU_i_Boer_1 and SMU_i_Boer_3 in the model:
# mi_pairs suggest it may be needed: 

model_1f_cov1 <- '
  SMU =~ SMU_i_Boer_1 + SMU_i_Boer_2 + SMU_i_Boer_3 +
         Freq_INS + Freq_TT + Freq_XTW + Freq_YT
  SMU_i_Boer_1 ~~ SMU_i_Boer_3
'

fit_1f_cov1 <- cfa(model_1f_cov1, data = seldemog,
                   ordered = smu7_names, estimator = "WLSMV",
                   parameterization = "theta", std.lv = TRUE)

summary(fit_1f_cov1, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_1f_cov1, c("cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr",
                           "cfi.scaled","tli.scaled","rmsea.scaled"))
fitMeasures(fit_1f, c("cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr",
                      "cfi.scaled","tli.scaled","rmsea.scaled"))


# Extract the principal values, informing about quality of the fit: 
fitMeasures(fit_1f, c("cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr","chisq","df","pvalue"))

lavTestLRT(fit_1f, fit_1f_cov1)  # significant Δχ² supports freeing the residual

# Standardised loadings matrix (expect all positive, ideally ≥ .40)
inspect(fit_1f_cov1, "std")$lambda # Yes

## Get factor scores
fs_8_no_cov <- lavPredict(fit_1f, method = "EBM")   # empirical Bayes scores (robust)
as.numeric(fs_8_no_cov); range(fs_8_no_cov)
seldemog$CFA_smu_score_8_no_covariance <- as.numeric(fs_8_no_cov)

## Get factor scores # 2
fs_8_cov <- lavPredict(fit_1f_cov1, method = "EBM")   # empirical Bayes scores (robust)
as.numeric(fs_8_cov); range(fs_8_cov)
seldemog$CFA_smu_score_8_covariance <- as.numeric(fs_8_cov)

cor(seldemog[,21:24]) # It's all nearly the same...



#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.


# OK, now - twelve factors: 

## 1) Build the polychoric correlation matrix (handles 7-pt + 6-pt mix)
pc  <- polychoric(sel12items)   # expect benign warnings about unequal categories
rho <- cor.smooth(pc$rho)      # ensure positive-definite

## 2) Inspect eigenvalues (scree)
ev <- eigen(rho, symmetric = TRUE)$values
print(round(ev, 3))

fa.parallel(rho, n.obs = nrow(sel12items), fm="ml", fa="both")
# Elbow again suggest one is the solution...

## 4) Numeric decision aids (parallel + Kaiser + acceleration)
ap      <- parallel(subject = nrow(sel12items),
                    var = ncol(sel12items),
                    rep = 500, cent = .05)
ap_eig  <- ap$eigen$qevpea
kaiserK <- sum(ev > 1)                         # Kaiser (very rough)
paK     <- sum(ev > ap_eig)                    # Parallel analysis cutoff

cat("\nKaiser suggests:", kaiserK, "factor(s)\n")
cat("Parallel suggests:", paK, "factor(s)\n")
# It's 3 and 3...

# choose nfact from the parallel/scree; then:
efa1_12_ml <- fa(r = rho, nfactors = 1, fm = "ml", rotate = "oblimin")
efa2_12_ml <- fa(r = rho, nfactors = 2, fm = "ml", rotate = "oblimin")
efa3_12_ml <- fa(r = rho, nfactors = 3, fm = "ml", rotate = "oblimin")
efa4_12_ml <- fa(r = rho, nfactors = 4, fm = "ml", rotate = "oblimin")

print(efa1_12_ml$loadings, cutoff=.10) # Take it from here
print(efa2_12_ml$loadings, cutoff=.10) # and from here
print(efa3_12_ml$loadings, cutoff=.10)
print(efa4_12_ml$loadings, cutoff=.10)


fa.parallel(rho, n.obs = nrow(sel12items), fm="uls", fa="both")

# choose nfact from the parallel/scree; then:
efa1_12_uls <- fa(r = rho, nfactors = 1, fm = "uls", rotate = "oblimin")
efa2_12_uls <- fa(r = rho, nfactors = 2, fm = "uls", rotate = "oblimin")
efa3_12_uls <- fa(r = rho, nfactors = 3, fm = "uls", rotate = "oblimin")
efa4_12_uls <- fa(r = rho, nfactors = 4, fm = "uls", rotate = "oblimin")

print(efa1_12_uls$loadings, cutoff=.10)
print(efa2_12_uls$loadings, cutoff=.10)
print(efa3_12_uls$loadings, cutoff=.10)
print(efa4_12_uls$loadings, cutoff=.10)

# One factor solution is clearly supported: 
# let's 
# (a) get the underlying factors from the FA objects: 
fa_fit_12_ml <- fa(sel12items, nfactors = 1, fm = "ml", rotate="oblimin")
seldemog$fa_12_ml_1FA <- fa_fit_12_ml$scores  

fa_fit_12_ml <- fa(sel12items, nfactors = 2, fm = "ml", rotate="oblimin")
fa_fit_12_ml$scores[,1]
seldemog$fa_12_ml_2FA <- fa_fit_12_ml$scores[,1]

fa_fit_12_ml$scores[,2]
seldemog$fa_12_ml_2FA_aka_NEG <- fa_fit_12_ml$scores[,2]


fa_fit_12_uls <- fa(sel12items, nfactors = 1, fm = "uls", rotate="oblimin")
seldemog$fa_12_uls_1FA <- fa_fit_12_uls$scores  

fa_fit_12_uls <- fa(sel12items, nfactors = 2, fm = "uls", rotate="oblimin")
fa_fit_12_uls$scores[,1]
seldemog$fa_12_uls_2FA <- fa_fit_12_uls$scores[,1]

fa_fit_12_uls$scores[,2]
seldemog$fa_12_ml_2FA_aka_NEG <- fa_fit_12_uls$scores[,2]

cor(seldemog[,21:29])

# Three factor solution: 
fa_fit_12_ml <- fa(sel12items, nfactors = 3, fm = "ml", rotate="oblimin")
fa_fit_12_ml$scores[,1]
seldemog$fa_12_ml_3FA_first <- fa_fit_12_ml$scores[,1]

fa_fit_12_ml$scores[,2]
seldemog$fa_12_ml_3FA_aka_NEG <- fa_fit_12_ml$scores[,2]

fa_fit_12_ml$scores[,3]
seldemog$fa_12_ml_3FA_third <- fa_fit_12_ml$scores[,3]

# uls
fa_fit_12_uls <- fa(sel12items, nfactors = 3, fm = "uls", rotate="oblimin")
fa_fit_12_uls$scores[,1]
seldemog$fa_12_uls_3FA_first <- fa_fit_12_uls$scores[,1]

fa_fit_12_uls$scores[,2]
seldemog$fa_12_uls_3FA_aka_NEG <- fa_fit_12_uls$scores[,2]

fa_fit_12_uls$scores[,3]
seldemog$fa_12_uls_3FA_third <- fa_fit_12_uls$scores[,3]


# (b) Run CFAs with the corresponding variables and isolate the model prediction, too: 

## declare them as ordered factors with fixed levels; this prevents accidental re-indexing 
## if some categories are missing.
for (nm in c("SMU_i_Boer_1","SMU_i_Boer_2","SMU_i_Boer_3")) {
  seldemog[[nm]] <- factor(seldemog[[nm]], levels = 1:7, ordered = TRUE)
}
## For per-platform frequency items (1..6):
for (nm in c("Freq_INS","Freq_TT","Freq_XTW","Freq_YT","Freq_FCB")) {
  seldemog[[nm]] <- factor(seldemog[[nm]], levels = 1:6, ordered = TRUE)
}
## For negative use items (1..5):
for (nm in c("negative_1","negative_2","negative_3","negative_4")) {
  seldemog[[nm]] <- factor(seldemog[[nm]], levels = 1:5, ordered = TRUE)
}


# We need to get two scores: 
#  - SMU score (standard as before)
#  - negative score
smu <- c("SMU_i_Boer_1","SMU_i_Boer_2","SMU_i_Boer_3","Freq_INS","Freq_TT","Freq_XTW","Freq_YT")
neg <- c("negative_1","negative_2","negative_3","negative_4")

model_2f <- '
  SMU =~ SMU_i_Boer_1 + SMU_i_Boer_2 + SMU_i_Boer_3 + Freq_INS + Freq_TT + Freq_XTW + Freq_YT
  NEG =~ negative_1 + negative_2 + negative_3 + negative_4
  SMU ~~ NEG
  SMU_i_Boer_1 ~~ SMU_i_Boer_3     # same small residual we already justified
'

fit_2f <- cfa(model_2f, data = seldemog,
              ordered = c(smu, neg), estimator = "WLSMV",
              parameterization = "theta", std.lv = TRUE)

summary(fit_2f, fit.measures=TRUE, standardized=TRUE)

# Standardised loadings matrix (expect all positive, ideally ≥ .40)
inspect(fit_2f, "std")$lambda # Yes, except for YT, but we already know it is worse...

# Now - the model without negative...
model_2f_without_negatives <- '
  ONE =~ SMU_i_Boer_1 + SMU_i_Boer_2 + SMU_i_Boer_3 + Freq_INS + Freq_TT + Freq_XTW + Freq_YT +
         negative_1 + negative_2 + negative_3 + negative_4
  SMU_i_Boer_1 ~~ SMU_i_Boer_3
'
fit_2f_without_negatives <- cfa(model_2f_without_negatives, data = seldemog,
                  ordered = c(smu, neg), estimator = "WLSMV",
                  parameterization = "theta", std.lv = TRUE)

lavTestLRT(fit_2f, fit_2f_without_negatives)  # scaled diff test
# the two-factor solution fits substantially better.


## Get factor scores
# 1: SMU score from model with both SMU and Negative Score (separately)
# 2: Negative score from model with both SMU and Negative Score
# 3: SMU Score from model where this score is predicted both by SMU and Negative...
fs <- lavPredict(fit_2f, method = "EBM")   # empirical Bayes scores (robust), You are interested in both...
(fs)[,1]; range(fs[,1])
(fs)[,2]; range(fs[,2])

seldemog$CFA_smu_score_12_SMU_negsep <- (fs)[,1]
seldemog$CFA_smu_score_12_NEG <- (fs)[,2]

fs <- lavPredict(fit_2f_without_negatives, method = "EBM")   # empirical Bayes scores (robust), You are interested in both...
(fs); range(fs)

seldemog$CFA_smu_score_12_SMU_negNEsep <- (fs)

# Also create the negative only subscale:


cor_subscales <- cor(seldemog[,21:38])

write.csv2(cor_subscales, file="cor_subscales_FA.csv")

# Let's stick to the most simple scale: 
seldemog$fa_8_ml


# And put it into the data frame. And finally kurva jdem samplovat.

range(seldemog$fa_8_ml)

seldemog$SMU_score <- as.numeric(seldemog$fa_8_ml)

seldemog$cultur <- as.factor(OLDEMOG$Cultur)

tapply(seldemog$SMU_score, seldemog$cultur, median) # Definitely split within country, not across countries.
# Plus do not forget to discuss that our result may partly depend on a nontrivial interaction between
# country of origin and locally popular social media (perhaps people in Vietnam may be less into western 
# social media & this may cause we in fact accidentally measure their adherence to western norms.


# Within country median split: 
# This was proposed by Chat - and it's elegant but maybe too complex. 
if ("cultur" %in% names(seldemog)) {
  med_by_cty <- tapply(seldemog$SMU_score, seldemog$cultur, median, na.rm = TRUE)
  seldemog$SMU_heavy <- as.integer(seldemog$SMU_score >= med_by_cty[as.character(seldemog$cultur)])
} else {
  cut <- median(seldemog$SMU_score, na.rm = TRUE)
  seldemog$SMU_heavy <- as.integer(seldemog$SMU_score >= cut)
}

seldemog$SMU_heavy

# Now - let's use more redneck method, take it as a sanity check: 
# repeat country dependent media times it is in our data: 
summary(as.factor(seldemog$cultur))

seldemog$median <- c(rep(med_by_cty[1],53), # Australia
                     rep(med_by_cty[2],32), # Colombia
                     rep(med_by_cty[3],152), # Czechia
                     rep(med_by_cty[4],47), # South Africa
                     rep(med_by_cty[5],79), # Turkey
                     rep(med_by_cty[6],72) # Vietnam
)

seldemog$SplitCHECK <- ifelse(seldemog$SMU_score >= seldemog$median, 1, 0)

summary(seldemog$SMU_heavy-seldemog$SplitCHECK) # Well done... 

# Now - put the SMU_heavy into OLDEMOG & turn it into "Split": above below:
table(OLDEMOG$Split,seldemog$SMU_heavy) # It overlaps quite well, after all! 

# Mean estimates of ratings are the sole results that changed, especially for Turks and Vietnamese. 
# Let's therefore check whether the split (which otherwise put most of the participants in the same folder as the old
# method) is somehow different for the two groups. 
# Mind that OLDEMOG already contains the new Colombian ratings (we just rerun the old "OLDEMOG" script with updated data)
# However, it changes nothing for Turks and Vietnamese - their Ns did not change:
table(OLDEMOG$Split[OLDEMOG$Cultur=="TUR"],seldemog$SMU_heavy[seldemog$cultur=="TUR"])
table(OLDEMOG$Split[OLDEMOG$Cultur=="VN"],seldemog$SMU_heavy[seldemog$cultur=="VN"]) # Not the case...


# Now: From Split to Zadar: 
OLDEMOG$OLD_Split <- OLDEMOG$Split

# And from Zadar to Dubrovnik: 
OLDEMOG$Split <- ifelse(seldemog$SMU_heavy==1,"Above","Below")

table(OLDEMOG$Split, OLDEMOG$OLD_Split) #... Both methods (the old, naive one, and the FA-based one, result in 
# similar sorting)

# Now - identify those who lie (in their country), among 25% (12.5 & 12.5%, sidewise) from the centre: 
seldemog$SMU_score;med_by_cty


## 1) Compute 37.5% and 62.5% quantiles within each country
cty <- seldemog$cultur
sc  <- seldemog$SMU_score

# Mind that the order is kept, we can proceed with the upper & lowe quantile, using tapply + function
(lower_q <- tapply(sc, cty, function(x) quantile(x, probs = 0.375, na.rm = TRUE, type = 7)))
med_by_cty # This should fit in the middle
(upper_q <- tapply(sc, cty, function(x) quantile(x, probs = 0.625, na.rm = TRUE, type = 7)))

## 2) Map those per-country cutoffs back to each row
lo <- lower_q[ as.character(cty) ];lo
hi <- upper_q[ as.character(cty) ];hi
# These are our borders in a vector-like manner (ensuring there is one lower and upper 
# border for each participant, corresponding to their ountries' quantiles)

## 3) Flag “within middle 25% band”; there are no NAs, so it's just 0/1, where 1 is within
# and the borders are included in the band
seldemog$NearMedian25 <- as.integer(sc >= lo & sc <= hi)

## Make a labeled factor - one may forgot meaning of 0/1 simply...
seldemog$NearMedian25_f <- factor(ifelse(is.na(seldemog$NearMedian25), NA,
                                         ifelse(seldemog$NearMedian25==1, "Within25%", "Outside")),
                                  levels = c("Outside","Within25%"))

## 4) Sanity check: proportion per country (should be ~0.25, small deviations due to ties)
round(tapply(seldemog$NearMedian25, cty, function(z) mean(z==1, na.rm=TRUE)), 3)

# 5) Put it to OLDEMOG: 
OLDEMOG$NearMedian25_0_1 <- seldemog$NearMedian25
OLDEMOG$NearMedian25_verbal <- seldemog$NearMedian25_f
OLDEMOG$SMU_new_score <- seldemog$SMU_score

cor(OLDEMOG$Freq_Scores_SUM, OLDEMOG$SMU_new_score)

# remove all but seldemog and OLDEMOG: 

# Specify the objects you want to keep
objects_to_keep <- c("OLDEMOG",
                     "seldemog")

# Remove all other objects
rm(list = setdiff(ls(), objects_to_keep))

# Now, we  need to add the Split into the long data: 

# Load the data back:
load("CZ_USE_THIS_FOR_LONG_TABLE.Rdata")
load("VN_USE_THIS_FOR_LONG_TABLE.Rdata")
load("RSA_USE_THIS_FOR_LONG_TABLE.Rdata")
load("AUS_USE_THIS_FOR_LONG_TABLE.Rdata")
load("COL_USE_THIS_FOR_LONG_TABLE.Rdata")
load("TUR_USE_THIS_FOR_LONG_TABLE.Rdata")

length(unique(COL_USE_THIS_FOR_LONG_TABLE$Particip_ID)) # 32 means good -> updated data...

# Put the data together: 
colnames(AUS_USE_THIS_FOR_LONG_TABLE)
colnames(COL_USE_THIS_FOR_LONG_TABLE)
colnames(CZ_USE_THIS_FOR_LONG_TABLE)
colnames(RSA_USE_THIS_FOR_LONG_TABLE)
colnames(TUR_USE_THIS_FOR_LONG_TABLE)
colnames(VN_USE_THIS_FOR_LONG_TABLE) # rbind.data.frame should work...


All_But_OCZ <- rbind.data.frame(AUS_USE_THIS_FOR_LONG_TABLE,COL_USE_THIS_FOR_LONG_TABLE,CZ_USE_THIS_FOR_LONG_TABLE,
                                RSA_USE_THIS_FOR_LONG_TABLE,TUR_USE_THIS_FOR_LONG_TABLE,VN_USE_THIS_FOR_LONG_TABLE)

#  ABOVE - BELOW -> Shall be "OLD_SPLIT":
names(All_But_OCZ)[names(All_But_OCZ) == "Above_Below"] <- "OLD_Split"  

# Now - substitude old Above-Below (Split) with new Split: 
OLDEMOG$ID;OLDEMOG$Split

unique(All_But_OCZ$Particip_ID) # Are you telling me that I actually have 435 participants? 
# No mistake? Well, let's see: 

all(unique(All_But_OCZ$Particip_ID) %in% OLDEMOG$ID)   # should be TRUE - then yes, they do overlap...

names(OLDEMOG)[names(OLDEMOG) == "ID"] <- "Particip_ID"

# Below, I create a new object "ratings", which contain:
# The table All_But_OCZ (as a whole) and two variables from OLDEMOG - participant ID (which serves to align 
# the tables) & Split -> which is the new split. 
ratings <- merge(All_But_OCZ, OLDEMOG[, c("Particip_ID","Split","NearMedian25_verbal","SMU_new_score")], by = "Particip_ID", all.x = TRUE, sort = FALSE)

## 0) sanity checks (optional)
stopifnot(!any(duplicated(OLDEMOG$Particip_ID)))             # each participant once
stopifnot(all(unique(All_But_OCZ$Particip_ID) %in% OLDEMOG$Particip_ID)) # all IDs present

# Drop the old split: 
ratings <- ratings[, -which(names(ratings) == "OLD_Split")]

# Add a rank so that I can reorder if needed: 
ratings$V_order <- seq(from=1, to=nrow(ratings), by=1)

# Adding demographic variables - for every face: 
load("FacesDemography.Rdata")

head(StimDemog)
head(ratings)

# Use "Face_ID" to  merge:  
temp_df <- merge(ratings, StimDemog, by = "Face_ID", all.x = TRUE, sort = FALSE)

New_Long_DF <- temp_df[order(temp_df$V_order), ]
New_Long_DF$V_order <- NULL

# Save the resultant df: 
write.csv(New_Long_DF, file="Yufe_rating_study_long_data_SM_factor_25_08_25.csv") 
write.csv2(New_Long_DF, file="2Yufe_rating_study_long_data_SM_factor_25_08_25.csv") 







#----------------------------------
# Demography (location of the participants, passport nationality, 
# mother tongue, ethnicity), updating demography within Colombian sample...

tapply(OLDEMOG$Cultur, OLDEMOG$Cultur, length)


tapply(OLDEMOG$MotherTongue, OLDEMOG$Cultur,
       function(x) summary(factor(x)))


tapply(OLDEMOG$SelectedLoc, OLDEMOG$Cultur,
       function(x) summary(factor(x)))

tapply(OLDEMOG$PasspNat, OLDEMOG$Cultur,
       function(x) summary(factor(x)))

tapply(OLDEMOG$Eth_SelfREP, OLDEMOG$Cultur,
       function(x) summary(factor(x)))

# Sex, Age, Height, Weight - COLOMBIA
OLDEMOG_COL <-OLDEMOG[OLDEMOG$Cultur=="COL",]

mean(OLDEMOG_COL$Age, na.rm = T)
sd(OLDEMOG_COL$Age, na.rm = T)

mean(OLDEMOG_COL$Height[OLDEMOG_COL$Height!=80], na.rm = T)
sd(OLDEMOG_COL$Height[OLDEMOG_COL$Height!=80], na.rm = T)

mean(OLDEMOG_COL$weight[OLDEMOG_COL$Height!=80], na.rm = T)
sd(OLDEMOG_COL$weight[OLDEMOG_COL$Height!=80], na.rm = T)

# Sex is a mess: 
sex <- c("F","F","M","M","M",
         "F","M","F","F","M",
         "M","F","M","M","F",
         "F","M","F","F","F",
         "F","F","M","F","F",
         "F","F","F","F","F",
         "M","M")

OLDEMOG_COL$Sex <- sex


mean(OLDEMOG_COL$Age[OLDEMOG_COL$Sex=="F"], na.rm = T)
sd(OLDEMOG_COL$Age[OLDEMOG_COL$Sex=="F"], na.rm = T)

mean(OLDEMOG_COL$Age[OLDEMOG_COL$Sex=="M"], na.rm = T)
sd(OLDEMOG_COL$Age[OLDEMOG_COL$Sex=="M"], na.rm = T)



mean(OLDEMOG_COL$Height[OLDEMOG_COL$Height!=80 & OLDEMOG_COL$Sex=="F"], na.rm = T)
sd(OLDEMOG_COL$Height[OLDEMOG_COL$Height!=80 & OLDEMOG_COL$Sex=="F"], na.rm = T)

mean(OLDEMOG_COL$Height[OLDEMOG_COL$Height!=80 & OLDEMOG_COL$Sex=="M"], na.rm = T)
sd(OLDEMOG_COL$Height[OLDEMOG_COL$Height!=80 & OLDEMOG_COL$Sex=="M"], na.rm = T)



mean(OLDEMOG_COL$weight[OLDEMOG_COL$Height!=80 & OLDEMOG_COL$Sex=="F"], na.rm = T)
sd(OLDEMOG_COL$weight[OLDEMOG_COL$Height!=80 & OLDEMOG_COL$Sex=="F"], na.rm = T)

mean(OLDEMOG_COL$weight[OLDEMOG_COL$Height!=80 & OLDEMOG_COL$Sex=="M"], na.rm = T)
sd(OLDEMOG_COL$weight[OLDEMOG_COL$Height!=80 & OLDEMOG_COL$Sex=="M"], na.rm = T)


# Device per country 
summary(as.factor(OLDEMOG$DEVICE_LABV))

summary(as.factor(OLDEMOG$DEVICE_LABV[OLDEMOG$Cultur=="AUSNZ"]))
summary(as.factor(OLDEMOG$DEVICE_LABV[OLDEMOG$Cultur=="COL"]))
summary(as.factor(OLDEMOG$DEVICE_LABV[OLDEMOG$Cultur=="CZ"]))
summary(as.factor(OLDEMOG$DEVICE_LABV[OLDEMOG$Cultur=="RSA"]))
summary(as.factor(OLDEMOG$DEVICE_LABV[OLDEMOG$Cultur=="TUR"]))
summary(as.factor(OLDEMOG$DEVICE_LABV[OLDEMOG$Cultur=="VN"]))
