
# Final version (unless JDL will miraculously jump in): Assembling the table made from 
# Czech raters (only Labvanced), Vietnamese, South African Raters, Australian + New Zealan raters, Colombian raters, Turkish Raters' Sample...

# If you are interested why I did this table by table (and not just once for all the raters): 
# 1) It took nine months to collect all the data. We checked the results initially
# 2) We needed to check the data for nonexistant social media
# 3) We needed to account for differences due to language (attention checks) and slight differences in social media listed in some questions

# In the end, participants shall be split into two groups: 
# -> High-iSMU (intensive Social Media user, the abbreviation designed not to resemble Boer's SMU-i)
# -> Low-iSMU (weak[er] social media user). 

# Split should be based on scores for: 
# (a) Visual SMU-i Score (TikTok, Instagram, YouTube) !PUNISHED SCORE!
#                  - unless the participants cannot be split, then 
# (b) general SMU-i Score @Boer at et al. Eijinden et al. (questions on the social media as a whole) !MEDIAN SCORE!
#                  - unless the participants cannot be split, then 
# (c) Specific "one-by-ine" SMU-i Score [how intesively the participant uses selected social media - the whole list] !PUNISHED SCORE!  
#                  - if still cannot be sorted then exclude...

# -------

# The original hypothesis aimed to test primarily Specific SMU-i score. Therefore, only analysis B-model I is confirmatory (testing hypotheses).

# Some parts of the script rely on the order of columns in data.frame "CZ_CZ_P_ID", make sure to update the order every time you add a variable 
# before the columns which order is the one you are referring to. 

# -------
  
# DELIBERATE DECISIONs: 
# (1) in the original CZ data, let's keep only the participants, who finished all or nearly all the stimuli. 
# (2) in the new CZ data and any data from abroad, let's keep only those who finished the SMU-i scales and passed the Attention Checks and can be 
#     considered conscious based on quiz. 

# Functions: 

# 1) A primitive function to get word from a string of NA,Na,NA,slovo,NA,NA...  
get_word2 <- function(WwW) {
  word <- WwW[!is.na(WwW)]
  if (length(word) == 0) {
    return(NA)
  } else {
    return(word)
  }
}
  
# 2) MEDAL SCORES - Define the function to apply the multipliers
apply_medal <- function(value) {
  if (value == 4) {
    return(4 * 4)
  } else if (value == 3) {
    return(3 * 2)
  } else if (value == 2) {
    return(2 * 1)
  } else {
    return(0)
  }
}
  
#-------------------------------------------------------------------------------
# 1 Uploading the data - CZ
#-------------------------------------------------------------------------------


CZ_CZ1 <- read.csv2("CZ_CZ_eye_face_SocMed_Checked.csv", T)

# Exclude participant "905023", who reported she recognised some of the participants, including herself: 
CZ_CZ1 <- CZ_CZ1[CZ_CZ1$Exp_Subject_Id_133!="905023",]

# Do it variable by variable: 
# Create a table for variables that has one unique answer per participant: 
# I need the sequence of lenghts for participants anyway: 

# What is the goal of this part: To get a table with demographic characteristics (age, weight, height, marital status),
# socioeconomic background (traveling abroad, childhood circumstances), way of using social media (active/passive,
# how frequently, SMU-i), self-perceived, potentially pathologic processes related to SMUse (FOMO and this bunch of Likert
# scale questions), self-reported ethnicity and eye colour, device they used to fill in the querry, and their performance
# assessment (correct answers in attention checks and the five "simple quiz" questions).
# The questions on social media  (SMU-i - median value, FOMO - median values, Active/Passive - podium formula and 
# Punished-score formula, Specific Intensity - podium formula and Punished-score formula) will serve to assess whether
# The user is active or passive. 

# We will also assess consistency across the scales. Nevertheless, the layout of associations between various scores will be
# presented in a separate contribution.


#-----
# You are interested in FACE raters, not EYE raters:
# The eye raters also answered the questions on SMU-i, etc.; however, they did not rate faces; 
# if they were included, the would distort the split. As some samples are small, the median scores may slightly differ by chance. 
# As a result, Face raters in some culture could have been splitted by a non-corresponding median (slightly lower or higher)
#-----
 
 
levels(as.factor(CZ_CZ1$Group_Name_134))
selected_levs <- c("FaceRaters_16A","FaceRaters_16B","FaceRaters_19A","FaceRaters_19B") # Check that correctly subsetted. 
CZ_CZ1 <- CZ_CZ1[CZ_CZ1$Group_Name_134 %in% selected_levs,]


CZ_CZ_P_ID <- levels(as.factor(CZ_CZ1$Exp_Subject_Id_133)) # Participants IDs
# Get rid of those who did not proceed: 
CZ_CZ_P_ID <- as.data.frame(CZ_CZ_P_ID)
CZ_CZ_P_ID$lengths <- tapply(CZ_CZ1$Rec_Session_Id_1, CZ_CZ1$Exp_Subject_Id_133, length)
hist(CZ_CZ_P_ID$lengths) # Being above 100 means they likely finished the study...


#-------------------------------------------------------------------------------
# 1 Basic demographics 
#-------------------------------------------------------------------------------

# Participant's demographic variables...
# All these variables either are naturally numeric (age...), or were coded as numeric (device)

# AGE: 
CZ_CZ_P_ID$Age <- tapply(CZ_CZ1$CZ_vek_CZ_age_39,CZ_CZ1$Exp_Subject_Id_133,mean, na.rm=T)
CZ_CZ_P_ID$Age[CZ_CZ_P_ID$Age=="NaN"]<-NA

# HEIGHT: 
CZ_CZ_P_ID$Height <- tapply(CZ_CZ1$b_height_F1_33,CZ_CZ1$Exp_Subject_Id_133,mean, na.rm=T)
CZ_CZ_P_ID$Height[CZ_CZ_P_ID$Height=="NaN"]<-NA

# WEIGHT: 
CZ_CZ_P_ID$weight <- tapply(CZ_CZ1$b_weight_F1_34,CZ_CZ1$Exp_Subject_Id_133,mean, na.rm=T)
CZ_CZ_P_ID$weight[CZ_CZ_P_ID$weight=="NaN"]<-NA

# MARITAL STATUS: 
CZ_CZ_P_ID$MariStat <- tapply(CZ_CZ1$Marital_status_F1_63,CZ_CZ1$Exp_Subject_Id_133,mean, na.rm=T)
CZ_CZ_P_ID$MariStat[CZ_CZ_P_ID$MariStat=="NaN"]<-NA

# Decoded to words: 
CZ_CZ_P_ID$MariStat_VERB <- ifelse(CZ_CZ_P_ID$MariStat==1,"Single",
                                   ifelse(CZ_CZ_P_ID$MariStat==2,"In_a_relationship",
                                          ifelse(CZ_CZ_P_ID$MariStat==3, "Married",
                                                 ifelse(CZ_CZ_P_ID$MariStat==4, "Divorced",
                                                        ifelse(CZ_CZ_P_ID$MariStat==5, "Widowed",
                                                               ifelse(CZ_CZ_P_ID$MariStat==6, "DontWaDeclare",NA))))))



# MOTHER TONGUE:
CZ_CZ1$Rodny_jazyk_73 <- ifelse(CZ_CZ1$Rodny_jazyk_73=="",NA,CZ_CZ1$Rodny_jazyk_73)
CZ_CZ_P_ID$MotherTongue <- tapply(CZ_CZ1$Rodny_jazyk_73, CZ_CZ1$Exp_Subject_Id_133, get_word2)

# PASSPORT NATIONALITY
CZ_CZ1$Passport.nationality_69 <- ifelse(CZ_CZ1$Passport.nationality_69=="",NA,CZ_CZ1$Passport.nationality_69)
CZ_CZ_P_ID$PasspNat <- tapply(CZ_CZ1$Passport.nationality_69, CZ_CZ1$Exp_Subject_Id_133, get_word2)

# ETHNICITY (definition)
CZ_CZ1$Eth_understanding_51 <- ifelse(CZ_CZ1$Eth_understanding_51=="",NA,CZ_CZ1$Eth_understanding_51)
CZ_CZ_P_ID$EthUNDERSTD <- tapply(CZ_CZ1$Eth_understanding_51, CZ_CZ1$Exp_Subject_Id_133, get_word2)

# ETHNICITY (self-reported)
CZ_CZ1$Self_reported_ethnicity_74 <- ifelse(CZ_CZ1$Self_reported_ethnicity_74=="",NA,CZ_CZ1$Self_reported_ethnicity_74)
CZ_CZ_P_ID$Eth_SelfREP <- tapply(CZ_CZ1$Self_reported_ethnicity_74, CZ_CZ1$Exp_Subject_Id_133, get_word2)

# GENDER (self-reported):
CZ_CZ1$Pohlavi_pri_narozeni_71 <- ifelse(CZ_CZ1$Pohlavi_pri_narozeni_71=="",NA,CZ_CZ1$Pohlavi_pri_narozeni_71)
CZ_CZ_P_ID$SEXATB <- tapply(CZ_CZ1$Pohlavi_pri_narozeni_71, CZ_CZ1$Exp_Subject_Id_133, get_word2)


#-------------------------------------------------------------------------------
# 2 Technical variables
#-------------------------------------------------------------------------------

CZ_CZ_P_ID$Technical <- "TECHNICAL"

# Device auto-selected: 
CZ_CZ_P_ID$DEVICE_LABV <- tapply(CZ_CZ1$System_Spec_154, CZ_CZ1$Exp_Subject_Id_133, unique)

# Password for Entrance Examination:
CZ_CZ1$Participant_Feedback_67 <- ifelse(CZ_CZ1$Participant_Feedback_67=="",NA,CZ_CZ1$Participant_Feedback_67)
CZ_CZ_P_ID$PASSW <- tapply(CZ_CZ1$Participant_Feedback_67, CZ_CZ1$Exp_Subject_Id_133, unique)

# Window resolution: 
# WIDTH:
CZ_CZ_P_ID$Window_WWW <- tapply(CZ_CZ1$Window_Width_In_Pixels_158, CZ_CZ1$Exp_Subject_Id_133, unique)
# HEIGHT: 
CZ_CZ_P_ID$Window_HHH <- tapply(CZ_CZ1$Window_Height_In_Pixels_157, CZ_CZ1$Exp_Subject_Id_133, unique)

# Device screen resolution: 
# WIDTH:
CZ_CZ_P_ID$Screen_WWW <- tapply(CZ_CZ1$Screen_Width_In_Pixels_141, CZ_CZ1$Exp_Subject_Id_133, unique)
# HEIGHT: 
CZ_CZ_P_ID$Screen_HHH <- tapply(CZ_CZ1$Screen_Height_In_Pixels_139, CZ_CZ1$Exp_Subject_Id_133, unique)

# Selected location (where LabVanced thinks the person is located - may be tricky due to people travelling 
# a lot and VPNs...)
CZ_CZ_P_ID$SelectedLoc <- tapply(CZ_CZ1$SelectedLocation_145, CZ_CZ1$Exp_Subject_Id_133, unique)


#-------------------------------------------------------------------------------
# 3 Attention Check, Test of Consciuousness: 
#-------------------------------------------------------------------------------

CZ_CZ_P_ID$Conscious <- "CONSCIOUS"

# Test score: Maximum value for a person in the variable "correct_36" is what you want...
# To get rid of a warning related to NA-only vectors, lets assign -1 to each body who did not bother to start the test:
CZ_CZ1$correct_36 <- ifelse(is.na(CZ_CZ1$correct_36),-1,CZ_CZ1$correct_36)

CZ_CZ_P_ID$TestScore <- tapply(as.numeric(CZ_CZ1$correct_36), CZ_CZ1$Exp_Subject_Id_133, max, na.rm=T)
CZ_CZ_P_ID$TestInterpr <- ifelse(CZ_CZ_P_ID$TestScore==5|CZ_CZ_P_ID$TestScore==4,"Good",ifelse(CZ_CZ_P_ID$TestScore==3,"Borderline","Exclude!"))
# The scores are already counted in the survey! 
# Also mind that the next time, you may compute some coefficients inside the survey as variables. 

summary(as.factor(CZ_CZ_P_ID$TestInterpr))


# Attention Checks (the kind of variable where there is one word for the vector [OR NA]: 
# 1 (Green/Zeleny)
CZ_CZ1$Attention_Check_Green_32 <- ifelse(CZ_CZ1$Attention_Check_Green_32=="",NA,CZ_CZ1$Attention_Check_Green_32)
CZ_CZ_P_ID$ATCH1 <- tapply(CZ_CZ1$Attention_Check_Green_32, CZ_CZ1$Exp_Subject_Id_133, get_word2)
summary(as.factor(CZ_CZ_P_ID$ATCH1 )) # One Grizzly's to be excluded! 

# 2 (1968)
CZ_CZ1$Attention_Check_CCCP_27 <- ifelse(CZ_CZ1$Attention_Check_CCCP_27=="",NA,CZ_CZ1$Attention_Check_CCCP_27)
CZ_CZ_P_ID$ATCH2 <- tapply(CZ_CZ1$Attention_Check_CCCP_27, CZ_CZ1$Exp_Subject_Id_133, get_word2)
summary(as.factor(CZ_CZ_P_ID$ATCH2 ))

# 3 (Motýl/Butterfly)
CZ_CZ1$Attention_Check_Butter_26 <- ifelse(CZ_CZ1$Attention_Check_Butter_26=="",NA,CZ_CZ1$Attention_Check_Butter_26)
CZ_CZ_P_ID$ATCH3 <- tapply(CZ_CZ1$Attention_Check_Butter_26, CZ_CZ1$Exp_Subject_Id_133, get_word2)
summary(as.factor(CZ_CZ_P_ID$ATCH3))


#-------------------------------------------------------------------------------
# 4 Social background (outside social media): 
#-------------------------------------------------------------------------------

CZ_CZ_P_ID$SocBack <- "SocBack"

# Travel abroad: 
# 1 = Velmi často = Often
# 2 = Spíše často = Rather often
# 3 = Občas = Occasionally
# 4 = Zřídka = Rarely
# 5 = Velmi zřídka = Very rarely
# 6 = Nikdy = never

CZ_CZ1$Travel_Abroad_111 <- ifelse(CZ_CZ1$Travel_Abroad_111=="",NA,CZ_CZ1$Travel_Abroad_111)
CZ_CZ_P_ID$ScoreAbroad <- tapply(CZ_CZ1$Travel_Abroad_111, CZ_CZ1$Exp_Subject_Id_133, get_word2)
summary(as.factor(CZ_CZ_P_ID$ScoreAbroad))

# Family background: 
# 1 = "Bohaté materiální zabezpečení, peníze jsme nikdy neřešili" = Rich 
# 2 = "Dost peněz, i když jsme se někdy museli uskormnit" = Upper CL
# 3 = "Někdy více, někdy méně, ale dokázali jsme vyžít bez ztráty úrovně" = Middle
# 4 = "Spíše málo peněz, ve srovnání s vrstevníky jsem se musel často uskromnit" = Lower CL 
# 5 = "Neustálé finanční potíže" = Poor
# 6 = "Nepřeji si odpovídat" = No family (the last level is labelled incorrectly, will be corrected)

CZ_CZ1$Family_SOC_1_52 <- ifelse(CZ_CZ1$Family_SOC_1_52=="",NA,CZ_CZ1$Family_SOC_1_52)
CZ_CZ_P_ID$Fami_Back <- tapply(CZ_CZ1$Family_SOC_1_52, CZ_CZ1$Exp_Subject_Id_133, get_word2)

# Correcting the level with wrong label: 
CZ_CZ_P_ID$Fami_Back[CZ_CZ_P_ID$Fami_Back=="No family"] <- "PreferNR"
summary(as.factor(CZ_CZ_P_ID$Fami_Back))


#-------------------------------------------------------------------------------
# 5 Social media use... All variables  are coded as numbers...
#
# 5.1 SNS - general - (c) Boer & Eijinden (2018 2022)
#  
#-------------------------------------------------------------------------------

# How many times a day - check the content: "JakCastoPas1"
# NOTE: The order is actually like this - from left, one being the leftmost.
# There is a misleading option/setting in Insert / Delete from: Right/Left.
# But even though it is set from "Right", it does not mean the 1 is on the right. 
# Just check by creating a new likert scale / multiple choice questions. 1 is on the leftmost end, the highest 
# number is the rightmost one.

# 1 = "Nikdy nebou méně než jednou denně"
# 2 = "1-2krát denně"
# 3 = "3-5krát denně"
# 4 = "6-10krát denně"
# 5 = "11-20krát denně"
# 6 = "21-40krát denně"
# 7 = "Více než 40krát denně"

CZ_CZ1$JakCastoPas1 <- ifelse(CZ_CZ1$JakCastoPas1=="",NA,CZ_CZ1$JakCastoPas1)
CZ_CZ_P_ID$SMUi_Q1 <- tapply(CZ_CZ1$JakCastoPas1, CZ_CZ1$Exp_Subject_Id_133, get_word2)


# How many times a week - posting: "JakCastoPost1"
# 1 = "Nikdy nebou méně než jednou týdně"
# 2 = "1-2krát týdně"
# 3 = "3-5krát týdně"
# 4 = "6-10krát týdně"
# 5 = "11-20krát týdně"
# 6 = "21-40krát týdně"
# 7 = "Více než 40krát týdně"

CZ_CZ1$JakCastoPost1_62 <- ifelse(CZ_CZ1$JakCastoPost1_62=="",NA,CZ_CZ1$JakCastoPost1_62)
CZ_CZ_P_ID$SMUi_Q2 <- tapply(CZ_CZ1$JakCastoPost1_62, CZ_CZ1$Exp_Subject_Id_133, get_word2)


# How many times a week - likes posts of the others: "JakCastoLikes1"
# 1 = "Nikdy nebou méně než jednou týdně"
# 2 = "1-2krát týdně"
# 3 = "3-5krát týdně"
# 4 = "6-10krát týdně"
# 5 = "11-20krát týdně"
# 6 = "21-40krát týdně"
# 7 = "Více než 40krát týdně"

CZ_CZ1$JakCastoLikes1 <- ifelse(CZ_CZ1$JakCastoLikes1=="",NA,CZ_CZ1$JakCastoLikes1)
CZ_CZ_P_ID$SMUi_Q3 <- tapply(CZ_CZ1$JakCastoLikes1, CZ_CZ1$Exp_Subject_Id_133, get_word2)

########
#      #
# (B)  # Social Media Use Intensity -> Mean, based on BOER... 
#      #
########

# Will then compute the MEAN value from the three variables' scores: 

CZ_CZ_P_ID$SMUi_MEAN <- apply(CZ_CZ_P_ID[,c(30:32)],1,mean)# The higher the more intensive user: 
# MEAN? Checked with 

#-------------------------------------------------------------------------------
# 5.2 Social media use - negative effects - FOMO, Too Much Time, Miss Others...
#-------------------------------------------------------------------------------

# 1 = zcela souhlasím (Totally agree)
# 2 = spíše souhlasím (rather agree)
# 3 = Ani souhlas ani nesouhlas (neither agree nor disagree)
# 4 = Spíše nesouhlasím (rather disagree)
# 5 = zcela nesouhlasím (Totally disagree)

# SNS_too_much_time:
CZ_CZ1$SNS_too_much_time_79
CZ_CZ_P_ID$SM_too_much_time <- tapply(CZ_CZ1$SNS_too_much_time_79, CZ_CZ1$Exp_Subject_Id_133, get_word2)

# SNS_sometimes_miss_other:
CZ_CZ1$SNS_Sometimes_miss_other_78
CZ_CZ_P_ID$Miss_other <- tapply(CZ_CZ1$SNS_Sometimes_miss_other_78, CZ_CZ1$Exp_Subject_Id_133, get_word2)

# row_35,col_66 - one guy did not reply just one question, will be ascribed zero instead of NA: 
CZ_CZ_P_ID[66,35] <- 0

# SNS_know_better_than_friends:
CZ_CZ1$SNS_know_better_than_friends_77
CZ_CZ_P_ID$SM_friends_better <- tapply(CZ_CZ1$SNS_know_better_than_friends_77, CZ_CZ1$Exp_Subject_Id_133, get_word2)

# SMP_fear_of_missing_out:
CZ_CZ1$SMP_fear_of_missing_out_76
CZ_CZ_P_ID$FOMO_SM <- tapply(CZ_CZ1$SMP_fear_of_missing_out_76, CZ_CZ1$Exp_Subject_Id_133, get_word2)

# Recalculate so that the higher (up to five), the worse: 
View(data.frame(Orig=CZ_CZ_P_ID$SM_too_much_time,New=6 - CZ_CZ_P_ID$SM_too_much_time))
CZ_CZ_P_ID$SM_too_much_time <- 6 - CZ_CZ_P_ID$SM_too_much_time

CZ_CZ_P_ID$Miss_other <- 6 - CZ_CZ_P_ID$Miss_other

CZ_CZ_P_ID$SM_friends_better <- 6 - CZ_CZ_P_ID$SM_friends_better

CZ_CZ_P_ID$FOMO_SM <- 6 - CZ_CZ_P_ID$FOMO_SM

# The score will be like "The higher, the more intensive user": 
CZ_CZ_P_ID$Score_negative_use_SUM <- rowSums(CZ_CZ_P_ID[,34:37])
CZ_CZ_P_ID$Score_negative_use_Mean <- apply(CZ_CZ_P_ID[,34:37],1,mean)


#-------------------------------------------------------------------------------
# 5.3 Which SNS - ACTIVELY, SEMI-ACTIVELY OR PASSIVELY
#-------------------------------------------------------------------------------

# AP - stands for ACTIVE/PASSIVE: 
CZ_CZ_P_ID$ACTPAS<- "ACTPAS"
CZ_CZ_P_ID$Facebook_AP <- tapply(CZ_CZ1$AP_Facebook_13,CZ_CZ1$Exp_Subject_Id_133, mean, na.rm=T)
CZ_CZ_P_ID$Facebook_AP[CZ_CZ_P_ID$Facebook_AP=="NaN"]<-NA
CZ_CZ_P_ID$Instagram_AP <- tapply(CZ_CZ1$AP_Instagram_17,CZ_CZ1$Exp_Subject_Id_133, mean, na.rm=T)
CZ_CZ_P_ID$Instagram_AP[CZ_CZ_P_ID$Instagram_AP=="NaN"]<-NA

CZ_CZ_P_ID$TikTok_AP <- tapply(CZ_CZ1$AP_TikTok_18,CZ_CZ1$Exp_Subject_Id_133, mean, na.rm=T)
CZ_CZ_P_ID$TikTok_AP[CZ_CZ_P_ID$TikTok_AP=="NaN"]<-NA
CZ_CZ_P_ID$WeChat_AP <- tapply(CZ_CZ1$AP_WeChat_19,CZ_CZ1$Exp_Subject_Id_133, mean, na.rm=T)
CZ_CZ_P_ID$WeChat_AP[CZ_CZ_P_ID$WeChat_AP=="NaN"]<-NA

CZ_CZ_P_ID$YouTube_AP <- tapply(CZ_CZ1$AP_YouTube_21,CZ_CZ1$Exp_Subject_Id_133, mean, na.rm=T)
CZ_CZ_P_ID$YouTube_AP[CZ_CZ_P_ID$YouTube_AP=="NaN"]<-NA
CZ_CZ_P_ID$Twitter_AP <- tapply(CZ_CZ1$AP_X..Twitter._20,CZ_CZ1$Exp_Subject_Id_133, mean, na.rm=T)
CZ_CZ_P_ID$Twitter_AP[CZ_CZ_P_ID$Twitter_AP=="NaN"]<-NA


# A/P - self reported (if any...), should be 3 items...  
# First one - which one (NAME): 
CZ_CZ1$AP_Input.social.media_1_16 <- ifelse(CZ_CZ1$AP_Input.social.media_1_16=="",NA,CZ_CZ1$AP_Input.social.media_1_16)
CZ_CZ_P_ID$SELF_AP_NAME_1 <- tapply(CZ_CZ1$AP_Input.social.media_1_16, CZ_CZ1$Exp_Subject_Id_133, get_word2)

# How (active/passive) - yes, 1st self reported SNS is (contrary to the expectation) the name of the variable...
CZ_CZ_P_ID$SELF_REP1_AP <- tapply(CZ_CZ1$X1st.self.reported.SNS_8,CZ_CZ1$Exp_Subject_Id_133, mean, na.rm=T)
CZ_CZ_P_ID$SELF_REP1_AP[CZ_CZ_P_ID$SELF_REP1_AP=="NaN"]<-NA

# Second one - which one (NAME): 
CZ_CZ1$AP_Input.social.media.2_15 <- ifelse(CZ_CZ1$AP_Input.social.media.2_15=="",NA,CZ_CZ1$AP_Input.social.media.2_15)
CZ_CZ_P_ID$SELF_AP_NAME_2 <- tapply(CZ_CZ1$AP_Input.social.media.2_15, CZ_CZ1$Exp_Subject_Id_133, get_word2)

# How (active/passive) - yes, 1st self reported SNS is (contrary to the expectation) the name of the variable...
CZ_CZ_P_ID$SELF_REP2_AP <- tapply(CZ_CZ1$X2nd.self.reported.SNS_9,CZ_CZ1$Exp_Subject_Id_133, mean, na.rm=T)
CZ_CZ_P_ID$SELF_REP2_AP[CZ_CZ_P_ID$SELF_REP2_AP=="NaN"]<-NA

# Third one - which one (NAME): 
CZ_CZ1$AP_Input.socal.media.3_14 <- ifelse(CZ_CZ1$AP_Input.socal.media.3_14=="",NA,CZ_CZ1$AP_Input.socal.media.3_14)
CZ_CZ_P_ID$SELF_AP_NAME_3 <- tapply(CZ_CZ1$AP_Input.socal.media.3_14, CZ_CZ1$Exp_Subject_Id_133, get_word2)

# How (active/passive) - yes, 1st self reported SNS is (contrary to the expectation) the name of the variable...
CZ_CZ_P_ID$SELF_REP3_AP <- tapply(CZ_CZ1$X3nd.self.reported.SNS_10,CZ_CZ1$Exp_Subject_Id_133, mean, na.rm=T)
CZ_CZ_P_ID$SELF_REP3_AP[CZ_CZ_P_ID$SELF_REP3_AP=="NaN"]<-NA


# Recalculating: Now the higher score the less she uses it, maximum is 4: 
CZ_CZ_P_ID$Facebook_AP <- 5 - CZ_CZ_P_ID$Facebook_AP
CZ_CZ_P_ID$Instagram_AP <- 5 - CZ_CZ_P_ID$Instagram_AP

CZ_CZ_P_ID$TikTok_AP <- 5 - CZ_CZ_P_ID$TikTok_AP
CZ_CZ_P_ID$WeChat_AP <- 5 - CZ_CZ_P_ID$WeChat_AP

CZ_CZ_P_ID$YouTube_AP <- 5 - CZ_CZ_P_ID$YouTube_AP
CZ_CZ_P_ID$Twitter_AP <- 5 - CZ_CZ_P_ID$Twitter_AP

CZ_CZ_P_ID$SELF_REP1_AP <- 5 - CZ_CZ_P_ID$SELF_REP1_AP
CZ_CZ_P_ID$SELF_REP2_AP <- 5 - CZ_CZ_P_ID$SELF_REP2_AP
CZ_CZ_P_ID$SELF_REP3_AP <- 5 - CZ_CZ_P_ID$SELF_REP3_AP


# Sanity check: Only social media that makes sense
# CZ_new: 
# Hofyland -> prehistoric Czech discussion server, included. 
# ne -> Bullshit, delete. EXCL [,47]
# Ashley Madison -> Keep, it's Tinder analogue. 
# Rodina cz - include, similar to Hofyland.
# Amateri cz - well, it is a pornsite. Exclude. [,47]
# Nyx cz - discussion server, included. 
# Garmin App - Exclude. Probably a typo. It's a sat naw software. EXCL [,49]
# Badoo - include, Tinder analogue. 
# MeWe.com - Include, SNS
# Kik.com - Iclude, SNS
# substack - exclude, it's a supportive infrastructure for newsletters. EXCL [,49]
# x [,51] - exclude, same as Twitter.

CZ_CZ_P_ID[82,47] <- NA
CZ_CZ_P_ID[82,48] <- NA

CZ_CZ_P_ID[184,47] <- NA
CZ_CZ_P_ID[184,48] <- NA

CZ_CZ_P_ID[268,49] <- NA
CZ_CZ_P_ID[268,50] <- NA

CZ_CZ_P_ID[232,49] <- NA
CZ_CZ_P_ID[232,50] <- NA

CZ_CZ_P_ID[268,51] <- NA
CZ_CZ_P_ID[268,52] <- NA


# Score 1: A/P (active/passive) punished: 
# max(vector_APs) + 0.75(^1)*max(vector_APs[vector_APs!=max()]) + 0.75(^2)*...

# Put these variables in a vector by rows:
ActPASS<-matrix(NA, nrow=9,ncol=292)
colnames(ActPASS) <- (CZ_CZ_P_ID[,1])
AP_Scores <- as.vector(rep(NA,292))

for (i in 1:nrow(CZ_CZ_P_ID)) {
  ActPASS[,i]<-unlist(CZ_CZ_P_ID[i,c(41:46, 48, 50, 52)])
  ActPASS[,i]<-ifelse(is.na(ActPASS[,i]),0,ActPASS[,i])
}


# Define the punishment factor and the length of the vector to consider
punishment_factor <- 0.75
vect_length <- nrow(ActPASS) 

# Create a vector of the punishment multipliers
multipliers <- punishment_factor^(0:(vect_length-1))


# Calculate scores using vectorized operations: Check whether it takes all the highest possible scores (If there is three times 6,
# does it count each?) 
AP_Scores <- apply(ActPASS, 2, function(column) {
  sorted_column <- sort(column, decreasing = TRUE)
  sum(sorted_column[1:vect_length] * multipliers)
})


CZ_CZ_P_ID$AP_Scores <- AP_Scores


# Score 2: A/P Podium: "MEDALS" 
# Count the "medals" = Determine the number of gold, silver, and bronze medals won by a country
# Apply the weights = Multiply the number of each type of medal by its respective point value
# Sum the totals = Add the weighted values together to get the total podium score

# Get coordinates of "horizontal vectors of medals": 
ActPASS

medals <- c(4,2,1,0)


# Calculate scores using vectorized operations: Check whether it takes all the highest possible scores (If there is three times 6,
# does it count each?) 

Medal_matrix <- apply(ActPASS, c(1, 2), apply_medal)

Medal_sums <- colSums(Medal_matrix)

CZ_CZ_P_ID$ActPAS_medals <- Medal_sums


#-------------------------------------------------------------------------------
# 5.4 Frequency of using of SPECIFIC social media (i.e., one by one):
#-------------------------------------------------------------------------------

CZ_CZ_P_ID$FreqSPEC<- "FreqSPEC"

# How_frequently_preselected: 
# 1 = I don't use it (Síť nepoužívám)
# 2 = Once a week or less (Jednou týdně či méně)
# 3 = Once a week, not daily (Víckrát týdně, ale ne denně)
# 4 = Once a day (Jednou za den)
# 5 = Several times a day (Několikrát denně)
# 6 = Frequently during the day (Často během dne) 

# The higher score the better - no need to recalculate anything


# Facebook = How_frequently_Facebook
CZ_CZ_P_ID$Facebook_Freq <- tapply(CZ_CZ1$How_frequently_Facebook,CZ_CZ1$Exp_Subject_Id_133, mean, na.rm=T)
CZ_CZ_P_ID$Facebook_Freq[CZ_CZ_P_ID$Facebook_Freq=="NaN"]<-NA

# YouTube = How_frequently_YouTube
CZ_CZ_P_ID$YouTube_Freq <- tapply(CZ_CZ1$How_frequently_YouTube,CZ_CZ1$Exp_Subject_Id_133, mean, na.rm=T)
CZ_CZ_P_ID$YouTube_Freq[CZ_CZ_P_ID$YouTube_Freq=="NaN"]<-NA

# Instagram = How_frequently_Instagram
CZ_CZ_P_ID$Instagram_Freq <- tapply(CZ_CZ1$How_frequently_Instagram,CZ_CZ1$Exp_Subject_Id_133, mean, na.rm=T)
CZ_CZ_P_ID$Instagram_Freq[CZ_CZ_P_ID$Instagram_Freq=="NaN"]<-NA

# TikTok = How_frequently_TikTok
CZ_CZ_P_ID$TikTok_Freq <- tapply(CZ_CZ1$How_frequently_TikTok,CZ_CZ1$Exp_Subject_Id_133, mean, na.rm=T)
CZ_CZ_P_ID$TikTok_Freq[CZ_CZ_P_ID$TikTok_Freq=="NaN"]<-NA

# ALSO USE SPECIFICALLY YouTube, Instagram and TikTok as the scales for "Visual" media.

# X[Twitter] = How_frequently_X
CZ_CZ_P_ID$X_Freq <- tapply(CZ_CZ1$How_frequently_X,CZ_CZ1$Exp_Subject_Id_133, mean, na.rm=T)
CZ_CZ_P_ID$X_Freq[CZ_CZ_P_ID$X_Freq=="NaN"]<-NA

# WeChat = How_Frequently_WECHAT
CZ_CZ_P_ID$WECHAT_Freq <- tapply(CZ_CZ1$How_Frequently_WECHAT,CZ_CZ1$Exp_Subject_Id_133, mean, na.rm=T)
CZ_CZ_P_ID$WECHAT_Freq[CZ_CZ_P_ID$WECHAT_Freq=="NaN"]<-NA


# Self reported SNS: 

# !! ALERT: The variable name "TIME" here does NOT refer to time from the beginning until the hit button
# Time = frequency...

# NAME_1: TIME_Input social media_1
CZ_CZ1$TIME_Input.social.media_1_103 <- ifelse(CZ_CZ1$TIME_Input.social.media_1_103=="",NA,CZ_CZ1$TIME_Input.social.media_1_103)
CZ_CZ_P_ID$Time_SELFREP_Name1 <- tapply(CZ_CZ1$TIME_Input.social.media_1_103, CZ_CZ1$Exp_Subject_Id_133, get_word2)

# Frequency_1: TIME_self reported SNS
CZ_CZ_P_ID$Time_SELFREP_Freq1 <- tapply(CZ_CZ1$TIME_self.reported.SNS_106,CZ_CZ1$Exp_Subject_Id_133, mean, na.rm=T)
CZ_CZ_P_ID$Time_SELFREP_Freq1[CZ_CZ_P_ID$Time_SELFREP_Freq1=="NaN"]<-NA


# NAME_2: TIME_Input social media_2
# Mistakenly, 2 as an implicit value. Therefore each 
# Participant who did not mark anyting is ascribed 2, instead of NaN / NA. This muset be fixed: 
CZ_CZ1$TIME_Input.social.media.2_102 <- ifelse(CZ_CZ1$TIME_Input.social.media.2_102=="",NA,CZ_CZ1$TIME_Input.social.media.2_102)
CZ_CZ_P_ID$Time_SELFREP_Name2 <- tapply(CZ_CZ1$TIME_Input.social.media.2_102, CZ_CZ1$Exp_Subject_Id_133, get_word2)

# Frequency_2: TIME_2nd self reported SNS
CZ_CZ_P_ID$Time_SELFREP_Freq2 <- tapply(CZ_CZ1$TIME_2nd.self.reported.SNS_81,CZ_CZ1$Exp_Subject_Id_133, mean, na.rm=T)
CZ_CZ_P_ID$Time_SELFREP_Freq2[CZ_CZ_P_ID$Time_SELFREP_Freq2=="NaN"]<-NA

CZ_CZ_P_ID$Time_SELFREP_Freq2 <- ifelse(is.na(CZ_CZ_P_ID$Time_SELFREP_Name2), NA, CZ_CZ_P_ID$Time_SELFREP_Freq2 )


# NAME_3: TIME_Input social media_3
CZ_CZ1$TIME_Input.social.media_3_104 <- ifelse(CZ_CZ1$TIME_Input.social.media_3_104=="",NA,CZ_CZ1$TIME_Input.social.media_3_104)
CZ_CZ_P_ID$Time_SELFREP_Name3 <- tapply(CZ_CZ1$TIME_Input.social.media_3_104, CZ_CZ1$Exp_Subject_Id_133, get_word2)

# Frequency_3: TIME_3nd self reported SNS
CZ_CZ_P_ID$Time_SELFREP_Freq3 <- tapply(CZ_CZ1$TIME_3nd.self.reported.SNS_82,CZ_CZ1$Exp_Subject_Id_133, mean, na.rm=T)
CZ_CZ_P_ID$Time_SELFREP_Freq3[CZ_CZ_P_ID$Time_SELFREP_Freq3=="NaN"]<-NA


CZ_CZ_P_ID[232,64] <- NA #
CZ_CZ_P_ID[232,65] <- NA #

CZ_CZ_P_ID[184,63] <- NA #
CZ_CZ_P_ID[184,64] <- NA #

CZ_CZ_P_ID[268,62] <- NA #
CZ_CZ_P_ID[268,63] <- NA #

CZ_CZ_P_ID[232,64] <- NA # 
CZ_CZ_P_ID[232,65] <- NA # 


#------------------

########
#      #
# (C)  # Specific Intensity Use -> Punished Score, one of the six (nine) social media 
#      #
########

# Frequency scores: 
# Score 1: Freq - punished: 

# Put these variables in a vector by rows:
FreqSC<-matrix(NA, nrow=9,ncol=292)
colnames(FreqSC) <- (CZ_CZ_P_ID[,1])
Freq_Scores <- as.vector(rep(NA,292)) # You have 292 rows, altough some are NAs

for (i in 1:nrow(CZ_CZ_P_ID)) {
  FreqSC[,i]<-unlist(CZ_CZ_P_ID[i,c(56:61,63,65,67)])
  FreqSC[,i]<-ifelse(is.na(FreqSC[,i]),0,FreqSC[,i])
}

# Define the punishment factor and the length of the vector to consider
punishment_factor <- 0.75
vect_length <- nrow(FreqSC) 

# Create a vector of the punishment multipliers
multipliers <- punishment_factor^(0:(vect_length-1));multipliers

# Calculate scores using vectorized operations
Freq_Scores <- apply(FreqSC, 2, function(column) {
  sorted_column <- sort(column, decreasing = TRUE)
  sum(sorted_column[1:vect_length] * multipliers)
})
Freq_Scores
hist(Freq_Scores) # Normally distributed, except those who are, in fact, NAs

CZ_CZ_P_ID$Freq_Scores <- Freq_Scores
CZ_CZ_P_ID$Freq_Scores_SUM<-rowSums(CZ_CZ_P_ID[,c(56:61,63,65,67)], na.rm=T)



# Other available scores: 
# Frequency of use: General (median score from the three variables) 
CZ_CZ_P_ID$SMUi_MEAN
hist(CZ_CZ_P_ID$SMUi_MEAN, breaks=25)

# Frequency of use: SPECIFIC - the higher the more intensive & on more SMs
CZ_CZ_P_ID$Freq_Scores
hist(CZ_CZ_P_ID$Freq_Scores, breaks=25)

# Actvie/Passive: the higher the more active & more individual SM
CZ_CZ_P_ID$AP_Scores
hist(CZ_CZ_P_ID$AP_Scores, breaks=25)

# Negative use - the higher the worse: 
CZ_CZ_P_ID$Score_negative_use_SUM
hist(CZ_CZ_P_ID$Score_negative_use_SUM, breaks=25)
hist(CZ_CZ_P_ID$Score_negative_use_Mean, breaks=25)

hist(CZ_CZ_P_ID$Freq_Scores_SUM,breaks=25)


# SMU-i - based only on "Visual" social media: TikTok, YouTube, Instagram
CZ_CZ_P_ID$VisualFreqSUM <- rowSums(CZ_CZ_P_ID[,c(57:59)])


########
#      #
# (A)  # Visual International Social Media Use -> Punished Score, one of the three Key Coefficients 
#      #
########

# Punished Score: 

# Put these variables in a vector by rows:
FreqSC<-matrix(NA, nrow=3,ncol=292)
colnames(FreqSC) <- (CZ_CZ_P_ID[,1])
Freq_Scores <- as.vector(rep(NA,292))

for (i in 1:nrow(CZ_CZ_P_ID)) {
  FreqSC[,i]<-unlist(CZ_CZ_P_ID[i,c(57:59)])
  FreqSC[,i]<-ifelse(is.na(FreqSC[,i]),0,FreqSC[,i])
}


# Define the punishment factor and the length of the vector to consider
punishment_factor <- 0.75
vect_length <- nrow(FreqSC) 

# Create a vector of the punishment multipliers
multipliers <- punishment_factor^(0:(vect_length-1))

# Calculate scores using vectorized operations
Freq_Scores <- apply(FreqSC, 2, function(column) {
  sorted_column <- sort(column, decreasing = TRUE)
  sum(sorted_column[1:vect_length] * multipliers)
})

CZ_CZ_P_ID$VisualFreq_Punish <- Freq_Scores


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# How related are these scales...
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Create a new data frame -> Add NAs instead of zeros (null score was not possible but when no response was provided):
CZ_MUi <- data.frame(SMUi_MEAN=CZ_CZ_P_ID$SMUi_MEAN,
                     Freq_Scores=CZ_CZ_P_ID$Freq_Scores,
                     Freq_Scores_SUM=CZ_CZ_P_ID$Freq_Scores_SUM,
                     AP_Scores=CZ_CZ_P_ID$AP_Scores,
                     Score_negative_use_SUM=as.numeric(CZ_CZ_P_ID$Score_negative_use_SUM),
                     Score_negative_use_Mean=as.numeric(CZ_CZ_P_ID$Score_negative_use_Mean),
                     VisualFreq_Punish=as.numeric(CZ_CZ_P_ID$VisualFreq_Punish),
                     VisualFreqSUM=as.numeric(CZ_CZ_P_ID$VisualFreqSUM))


CZ_MUi$SMUi_MEAN[CZ_MUi$SMUi_MEAN==0]<-NA
CZ_MUi$Freq_Scores[CZ_MUi$Freq_Scores==0]<-NA
CZ_MUi$AP_Scores[CZ_MUi$AP_Scores==0]<-NA
CZ_MUi$VisualFreq_Punish[CZ_MUi$VisualFreq_Punish==0]<-NA
CZ_MUi$Freq_Scores_SUM[CZ_MUi$Freq_Scores_SUM==0]<-NA

CZ_MUi <- CZ_MUi[!is.na(CZ_MUi$SMUi_MEAN),] # It looks like those to start filling in these scores actually filled in the whole 


library(Hmisc)
rcorr(as.matrix(CZ_MUi))


#-------------------------------------------------------------------------------
# 5.6. Preparation for the median splits (low/high intensity users)
#-------------------------------------------------------------------------------

# Which variables interests me at this point: 

# [1] A scale to assess the SMU-i: The candidate variables: 
CZ_CZ_P_ID$Score_negative_use_SUM # Score - negative use
CZ_CZ_P_ID$SMUi_MEAN # Scale Based on Boer et al. 2022

CZ_CZ_P_ID$AP_Scores # Score - Active Passive (Punished Score)
CZ_CZ_P_ID$ActPAS_medals # Score - Active Passive (Medals)

CZ_CZ_P_ID$Freq_Scores # Frequency of use of specific media - punished score
CZ_CZ_P_ID$Freq_Scores_SUM # Frequency score of specific media - sum

CZ_CZ_P_ID$VisualFreqSUM # Frequency of use visual media - sum
CZ_CZ_P_ID$VisualFreq_Punish # Frequency of use visual media - Punished Score

CZ_CZ1$Use_SNS_1 <- ifelse(CZ_CZ1$Use_SNS_1_118=="",NA,CZ_CZ1$Use_SNS_1_118)
CZ_CZ_P_ID$Social_Yes_No <- tapply(CZ_CZ1$Use_SNS_1, CZ_CZ1$Exp_Subject_Id_133, get_word2)
summary(as.factor(CZ_CZ_P_ID$Social_Yes_No)) # Everybody's using social media... no need to mess around with other variables...
CZ_CZ_P_ID <- CZ_CZ_P_ID[,c(1:71)]


# Passed all tests (Score, AttentionCH1-CH3): 
CZ_CZ_IP_good <- CZ_CZ_P_ID[CZ_CZ_P_ID$TestInterpr=="Good"|CZ_CZ_P_ID$TestInterpr=="Borderline",]
CZ_CZ_IP_good <- CZ_CZ_IP_good[!is.na(CZ_CZ_IP_good$ATCH3),]

# Did grizzly survive? 
CZ_CZ_IP_good <- CZ_CZ_IP_good[CZ_CZ_IP_good$ATCH1!="Grizzly",]
summary(as.factor(CZ_CZ_IP_good$ATCH1)) # NO (thus need not to exclude the guy...)

# Now - those who lacks score cannot be split: 
CZ_CZ_IP_good <- CZ_CZ_IP_good[!is.na(CZ_CZ_IP_good$SMUi_MEAN),]

# Adding the rest of the variables for a rater (SMU-i, etc.):
CZ_CZ_IP_good$Score_negative_use_Mean # Score - negative use
CZ_CZ_IP_good$SMUi_MEAN # Scale Based on Boer et al. 2022

CZ_CZ_IP_good$AP_Scores # Score - Active Passive (Punished Score)
CZ_CZ_IP_good$ActPAS_medals # Score - Active Passive (Medals)
cor(CZ_CZ_IP_good$AP_Scores,CZ_CZ_IP_good$ActPAS_medals) # Well...

CZ_CZ_IP_good$Freq_Scores # Frequency of use of specific media - punished score
CZ_CZ_IP_good$Freq_Scores_SUM # Frequency score of specific media - sum
cor(CZ_CZ_IP_good$Freq_Scores,CZ_CZ_IP_good$Freq_Scores_SUM) # Scores calculated differently says the same story.
# Why not to average: Since what I want to capture primarily is an intensive user of a social media - not necessarily 
# whether she uses many different Social Media. 

CZ_CZ_IP_good$VisualFreqSUM # Frequency of use visual media - sum
CZ_CZ_IP_good$VisualFreq_Punish # Frequency of use visual media - Punished Score
cor(CZ_CZ_IP_good$VisualFreqSUM,CZ_CZ_IP_good$VisualFreq_Punish) # Here it is effectively the same variable... 


# No NAs or zeros, let's split (a-b-c): 
SMUi_Mean <- median(CZ_CZ_IP_good$SMUi_MEAN) # YES (b)

Freq_Scores_median <- median(CZ_CZ_IP_good$Freq_Scores) # YES (c)

VisualFreq_Punish_median <- median(CZ_CZ_IP_good$VisualFreq_Punish) # YES (a)



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 5.7. Median splits: Simplified - for the full-size version see some previous versions of this script...
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# How it performs in splitting: 0<,0.5==,>1

#.-.-.-
# 1 Scale Based on Boer et al. 2022 - SCORE (b)
CZ_CZ_IP_good$SMUi_MEAN_Mean_SPLIT <- ifelse(CZ_CZ_IP_good$SMUi_MEAN<SMUi_Mean,0,
                                                  ifelse(CZ_CZ_IP_good$SMUi_MEAN==SMUi_Mean,0.5,1))
summary(as.factor(CZ_CZ_IP_good$SMUi_MEAN_Mean_SPLIT))


#.-.-.-
# 2 Frequency of use of media - per given media (Facebook, Instagram, Twitter, WeChat, TikTok, YouTube + self-reported) - punished score (c)
CZ_CZ_IP_good$Freq_Scores_SPLIT <- ifelse(CZ_CZ_IP_good$Freq_Scores<Freq_Scores_median,0,
                                            ifelse(CZ_CZ_IP_good$Freq_Scores==Freq_Scores_median,0.5,1))
summary(as.factor(CZ_CZ_IP_good$Freq_Scores_SPLIT))


#.-.-.-
# 3 Frequency of use visual media - Punished Score (YouTube, TikTok, Instagram) - ONE OF THE FOCUS SCORES
CZ_CZ_IP_good$VisualFreq_Punish_median_SPLIT <- ifelse(CZ_CZ_IP_good$VisualFreq_Punish<VisualFreq_Punish_median,0,
                                            ifelse(CZ_CZ_IP_good$VisualFreq_Punish==VisualFreq_Punish_median,0.5,1))
summary(as.factor(CZ_CZ_IP_good$VisualFreq_Punish_median_SPLIT))


# How does splitting according to different categories work: 
hist(rowSums(CZ_CZ_IP_good[,72:74]),breaks=12) # Brute force kinda works...
# Now - I am NOT interested in every variable of those... for the sake of visual diet. 



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 5.8. # Alternative attitude - excluding 25 % in the middle: 
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

CZ_CZ_IP_good$SMUi_MEAN
CZ_CZ_IP_good$Freq_Scores
CZ_CZ_IP_good$VisualFreq_Punish

CZ_CZ_IP_good$SMUi_Mean_25_percent <- ifelse(CZ_CZ_IP_good$SMUi_MEAN<=quantile(CZ_CZ_IP_good$SMUi_MEAN,0.375),"sides",
                                             ifelse(CZ_CZ_IP_good$SMUi_MEAN>=quantile(CZ_CZ_IP_good$SMUi_MEAN,0.625),"sides","centre"))
summary(as.factor(CZ_CZ_IP_good$SMUi_Mean_25_percent))


CZ_CZ_IP_good$Freq_Scores_25_percent <- ifelse(CZ_CZ_IP_good$Freq_Scores<=quantile(CZ_CZ_IP_good$Freq_Scores,0.375),"sides",
                                               ifelse(CZ_CZ_IP_good$Freq_Scores>=quantile(CZ_CZ_IP_good$Freq_Scores,0.625),"sides","centre"))
summary(as.factor(CZ_CZ_IP_good$Freq_Scores_25_percent))


CZ_CZ_IP_good$VisualFreqPunish_25_percent <- ifelse(CZ_CZ_IP_good$VisualFreq_Punish<quantile(CZ_CZ_IP_good$VisualFreq_Punish,0.375),"sides",
                                                    ifelse(CZ_CZ_IP_good$VisualFreq_Punish>quantile(CZ_CZ_IP_good$VisualFreq_Punish,0.625),"sides","centre"))
summary(as.factor(CZ_CZ_IP_good$VisualFreqPunish_25_percent)) # There is nothing in between 37.5 % and 62.5 % of the distribution.



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 5.9. Splitting (Above, Mediocre, Below)
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Split based on "A" (=VisualFreq_Punish)
#                    if not possible, split based on "B" (=SMUi_MEAN)
#                            if not possible, split based on "C" (=Freq_Scores)
# Otherwise "Mediocre"
CZ_CZ_IP_good$VisualFreq_Punish_median_SPLIT
CZ_CZ_IP_good$Split <- ifelse(CZ_CZ_IP_good$VisualFreq_Punish_median_SPLIT==1,"Above",
                              ifelse(CZ_CZ_IP_good$VisualFreq_Punish_median_SPLIT==0,"Below","Mediocre"))

# Step 2: For rows where Split is still "Mediocre", check the "SMUi_MEAN" variable
mediocre_indices <- which(CZ_CZ_IP_good$Split == "Mediocre")
CZ_CZ_IP_good$Split[mediocre_indices] <- ifelse(CZ_CZ_IP_good$SMUi_MEAN_Mean_SPLIT[mediocre_indices] == 1, "Above",
                                                ifelse(CZ_CZ_IP_good$SMUi_MEAN_Mean_SPLIT[mediocre_indices] == 0, "Below", "Mediocre"))

# Step 3: For rows still "Mediocre", check the "Freq_Scores" variable - not necessary! 


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 6.0 # Adding the ratings.
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# (1) Cut the data.frame based on rater's ID: 
# (2) Get the ratings - if she left some faces not-rated, add NAs (mind the specific length of each sample: 
#     2.1. CZ 2016: 50M + 50F
#     2.2. CZ 2019: 39M + 56F

# How to use trial ID: Add a data.frame where real face's IDs and Trial IDs are paired...
real_IDs <- read.csv2("CZ1619_Order_TRU.csv",T)

# Creating the actual ID of interest - FOR ATD! Then rewrite when for the eyes:  
real_IDs$FullID <- paste(real_IDs$Trial_Id,real_IDs$Grupa_ATD)
real_IDs$Full_Face_ID <- paste(real_IDs$Set,real_IDs$Real_ID) # IDs would overlap between 2016 and 2019. This is to avoid it...

# Faces: 
CZ_CZ1$ATR_16F_22
CZ_CZ1$ATR_16M_23
CZ_CZ1$ATR_19F_24
CZ_CZ1$ATR_19M_25

# How to (re)order it: Spot the variable "Trial ID" and block name:  
# And since you got the IDs (of both photos, raters, and blocks), you can get rid of NAs:
names(CZ_CZ1[,c(2,6,22:25,47:50,114:117,133)])
CZRAT_List_FACES <- as.data.frame(CZ_CZ1[,c(2,6,22:25,47:50,114:117,133)]) # Only rating variables + IDs (all of them)

# Adding the order in the table
CZRAT_List_FACES$OrOd <- seq(from=1, to=nrow(CZRAT_List_FACES), by=1)

# Only rows that contain ratings are of interest: 
levels(as.factor(CZRAT_List_FACES$Task_Name_6))
selected_levs <- c("CZ_16F_ATD","CZ_16M_ATD","CZ_19F_ATD","CZ_19M_ATD")  

CZRAT_List_FACES <- CZRAT_List_FACES[CZRAT_List_FACES$Task_Name_6 %in% selected_levs,]
CZRAT_List_FACES$FullID <- paste(CZRAT_List_FACES$Trial_Id_2,CZRAT_List_FACES$Task_Name_6)


# Match the two tables - the one with IDs and the one with ratings: 
CZRAT_List_FACES <- merge(CZRAT_List_FACES, real_IDs, by = "FullID", all = TRUE)

CZRAT_List_FACES <- CZRAT_List_FACES[order(CZRAT_List_FACES$OrOd),]

names(CZRAT_List_FACES)[names(CZRAT_List_FACES) == 'Exp_Subject_Id_133'] <- "CZ_CZ_P_ID"

# These are then the (only) raters you are interested in: 
# Subset the table: CZ_CZ1 - take only those "selected" raters
# Put the attractiveness, trustworthiness, and dominance, that are now split into 
# four columns each into just three columns (one for trustw., one for attractiveness, one for dominance):

CZRAT_List_FACES <- CZRAT_List_FACES[CZRAT_List_FACES$Task_Name_6 %in% selected_levs,]

CZRAT_List_FACES <- merge(CZRAT_List_FACES, CZ_CZ_IP_good, by = "CZ_CZ_P_ID", all = F)
CZRAT_List_FACES <- CZRAT_List_FACES[order(CZRAT_List_FACES$OrOd),]

#-------------------------------------------------------------------------------
# 6.2
#-------------------------------------------------------------------------------

# Every time there's a value in one column (FOR A SET), it is missing in another, see: 
rowSums(CZ_CZ1[,22:25], na.rm = T) # Attractiveness
rowSums(CZ_CZ1[,47:50], na.rm = T) # Dominance
rowSums(CZ_CZ1[,114:117], na.rm = T) # Dominance

CZRAT_List_FACES$ATR <- rowSums(CZRAT_List_FACES[,5:8], na.rm = T)
CZRAT_List_FACES$TRU <- rowSums(CZRAT_List_FACES[,13:16], na.rm = T)
CZRAT_List_FACES$DOM <- rowSums(CZRAT_List_FACES[,9:12], na.rm = T)

# The core of the model is like this: 
# I am interested in the effect Sample (country of origin) and SNS use intensity (expressed by whichever variable)
# as Fixed Effects have on rating scale / scales 

CZRAT_List_FACES$ATR
CZRAT_List_FACES$TRU
CZRAT_List_FACES$DOM

CZRAT_List_FACES$Sample <- "CZ"

# The simplest way: separate model for each scale of these...

# Prepare the table of "interesting variables" for the model: 
# Participant ID: CZ_CZ_P_ID (1)
# Full ID of the Photo: Full_Face_ID (23)
# Above/Below median: VThreeSplit_SPLIT (111)
# Ratings: ATR, TRU, DOM (112:114)
# Sample: CZ (115)


CZ_USE_THIS_FOR_LONG_TABLE <- data.frame(Particip_ID = CZRAT_List_FACES[,1],         # CZ_CZ_P_ID
                                         Face_ID = CZRAT_List_FACES[,23],            # Full_Face_ID
                                         Above_Below = CZRAT_List_FACES[,100],       # Split
                                         Atr = CZRAT_List_FACES[,101],               # ATR
                                         Tru = CZRAT_List_FACES[,102],               # TRU
                                         Dom = CZRAT_List_FACES[,103],               # DOM
                                         Sample = CZRAT_List_FACES[,104],
                                         CentreSide = paste(CZRAT_List_FACES$SMUi_Mean_25_percent,
                                                            CZRAT_List_FACES$Freq_Scores_25_percent,
                                                            CZRAT_List_FACES$VisualFreqPunish_25_percent),
                                         ScoreAbroad = CZRAT_List_FACES[,50],
                                         Fami_Back = CZRAT_List_FACES[,51]
)

save(CZ_USE_THIS_FOR_LONG_TABLE, file="CZ_USE_THIS_FOR_LONG_TABLE.Rdata")

# Specify the objects you want to keep
objects_to_keep <- c("CZ_USE_THIS_FOR_LONG_TABLE", "apply_medal", "get_word2", "real_IDs", "CZ_CZ_IP_good")  # Replace with your object names

# Remove all other objects
rm(list = setdiff(ls(), objects_to_keep))









#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 
#  Vietnamese data 
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.


VN_VN1 <- read.csv2("VN_VN_eye_face_SocMed_Checked_FINAL.csv", T)

# You are now interested in FACE raters: 
levels(as.factor(VN_VN1$Group_Name))
selected_levs <- c("FaceRaters_16A","FaceRaters_16B","FaceRaters_19A","FaceRaters_19B")  


VN_VN1 <- VN_VN1[VN_VN1$Group_Name %in% selected_levs,]

VN_VN_P_ID <- levels(as.factor(VN_VN1$Exp_Subject_Id)) # Participants IDs

VN_VN_P_ID<-as.data.frame(VN_VN_P_ID)
VN_VN_P_ID$lengths <- tapply(VN_VN1$Rec_Session_Id, VN_VN1$Exp_Subject_Id, length)
hist(VN_VN_P_ID$lengths) # Being above 100 means they likely finished the study...


#-------------------------------------------------------------------------------
# 1 Basic demographics 
#-------------------------------------------------------------------------------

# Participant's demographic variables...
# All these variables either are naturally numeric (age...), or were coded as numeric (device)

# AGE: 
VN_VN_P_ID$Age <- tapply(VN_VN1$SELF_age_1,VN_VN1$Exp_Subject_Id,mean, na.rm=T)
VN_VN_P_ID$Age[VN_VN_P_ID$Age=="NaN"]<-NA

# HEIGHT: 
VN_VN_P_ID$Height <- tapply(VN_VN1$b_height_F1,VN_VN1$Exp_Subject_Id,mean, na.rm=T)
VN_VN_P_ID$Height[VN_VN_P_ID$Height=="NaN"]<-NA

# WEIGHT: 
VN_VN1$b_weight_F1 <- as.numeric(VN_VN1$b_weight_F1)
VN_VN_P_ID$weight <- tapply(VN_VN1$b_weight_F1,VN_VN1$Exp_Subject_Id,mean, na.rm=T)
VN_VN_P_ID$weight[VN_VN_P_ID$weight=="NaN"]<-NA

# MARITAL STATUS: 
VN_VN_P_ID$MariStat <- tapply(VN_VN1$Marital_status_F1,VN_VN1$Exp_Subject_Id,mean, na.rm=T)
VN_VN_P_ID$MariStat[VN_VN_P_ID$MariStat=="NaN"]<-NA

# Decoded to words: 
VN_VN_P_ID$MariStat_VERB <- ifelse(VN_VN_P_ID$MariStat==1,"Single",
                                   ifelse(VN_VN_P_ID$MariStat==2,"In_a_relationship",
                                          ifelse(VN_VN_P_ID$MariStat==3, "Married",
                                                 ifelse(VN_VN_P_ID$MariStat==4, "Divorced",
                                                        ifelse(VN_VN_P_ID$MariStat==5, "Widowed",
                                                               ifelse(VN_VN_P_ID$MariStat==6, "DontWaDeclare",NA))))))


# MOTHER TONGUE:
VN_VN1$Rodny_jazyk <- ifelse(VN_VN1$Mother_Tongue_1=="",NA,VN_VN1$Mother_Tongue_1)
VN_VN_P_ID$MotherTongue <- tapply(VN_VN1$Rodny_jazyk, VN_VN1$Exp_Subject_Id, get_word2) # looks like all but those who will be excluded as they 
# did not finish the survey were of Vietnamese language origin...

# PASSPORT NATIONALITY
VN_VN1$Passport.nationality <- ifelse(VN_VN1$Passport.nationality=="",NA,VN_VN1$Passport.nationality)
VN_VN_P_ID$PasspNat <- tapply(VN_VN1$Passport.nationality, VN_VN1$Exp_Subject_Id, get_word2)

# Ethnicity (definition)
VN_VN1$Eth_understanding <- ifelse(VN_VN1$Eth_understanding=="",NA,VN_VN1$Eth_understanding)
VN_VN_P_ID$EthUNDERSTD <- tapply(VN_VN1$Eth_understanding, VN_VN1$Exp_Subject_Id, get_word2)

# Ethnicity (self-reported)
VN_VN1$Self_reported_ethnicity <- ifelse(VN_VN1$Self_reported_ethnicity=="",NA,VN_VN1$Self_reported_ethnicity)
VN_VN_P_ID$Eth_SelfREP <- tapply(VN_VN1$Self_reported_ethnicity, VN_VN1$Exp_Subject_Id, get_word2)

# Gender (self-reported):
VN_VN1$Pohlavi_pri_narozeni <- ifelse(VN_VN1$Gender.at.birth_1=="",NA,VN_VN1$Gender.at.birth_1)
VN_VN_P_ID$SEXATB <- tapply(VN_VN1$Pohlavi_pri_narozeni, VN_VN1$Exp_Subject_Id, get_word2)


#-------------------------------------------------------------------------------
# 2 Technical variables
#-------------------------------------------------------------------------------

VN_VN_P_ID$Technical <- "TECHNICAL"
# Device auto-selected: 
VN_VN_P_ID$DEVICE_LABV <- tapply(VN_VN1$System_Spec, VN_VN1$Exp_Subject_Id, unique)

# Password for Entrance Examination:
VN_VN1$Participant_Feedback <- ifelse(VN_VN1$Participant_Feedback=="",NA,VN_VN1$Participant_Feedback)
VN_VN_P_ID$PASSW <- tapply(VN_VN1$Participant_Feedback, VN_VN1$Exp_Subject_Id, unique)

# Window resolution: 
# WIDTH:
VN_VN_P_ID$Window_WWW <- tapply(VN_VN1$Window_Width_In_Pixels, VN_VN1$Exp_Subject_Id, unique)
# HEIGHT: 
VN_VN_P_ID$Window_HHH <- tapply(VN_VN1$Window_Height_In_Pixels, VN_VN1$Exp_Subject_Id, unique)

# Device screen resolution: 
# WIDTH:
VN_VN_P_ID$Screen_WWW <- tapply(VN_VN1$Screen_Width_In_Pixels, VN_VN1$Exp_Subject_Id, unique)
# HEIGHT: 
VN_VN_P_ID$Screen_HHH <- tapply(VN_VN1$Screen_Height_In_Pixels, VN_VN1$Exp_Subject_Id, unique)

# Selected location (where LabVanced thinks the person is located - may be tricky due to people travelling 
# a lot and VPNs...)
VN_VN_P_ID$SelectedLoc <- tapply(VN_VN1$SelectedLocation, VN_VN1$Exp_Subject_Id, unique)


#-------------------------------------------------------------------------------
# 3 Was the person conscious (attention checks and score in a test): 
#-------------------------------------------------------------------------------

VN_VN_P_ID$Conscious <- "CONSCIOUS"

# Test score: Maximum value for a person in the variable "correct_36" is what you want...
# To get rid of a warning related to NA-only vectors, lets assign -1 to each body who did not bother to start the test:
VN_VN1$correct <- ifelse(is.na(VN_VN1$correct),-1,VN_VN1$correct)

VN_VN_P_ID$TestScore <- tapply(as.numeric(VN_VN1$correct), VN_VN1$Exp_Subject_Id, max, na.rm=T)
VN_VN_P_ID$TestInterpr <- ifelse(VN_VN_P_ID$TestScore==5|VN_VN_P_ID$TestScore==4,"Good",ifelse(VN_VN_P_ID$TestScore==3,"Borderline","Exclude!"))

summary(as.factor(VN_VN_P_ID$TestInterpr))


# Attention Checks (the kind of variable where there is one word for the vector [OR NA]: 
# 1 (Green/Zeleny)
VN_VN1$Attention_Check_Green <- ifelse(VN_VN1$Attention_Check_Green=="",NA,VN_VN1$Attention_Check_Green)
VN_VN_P_ID$ATCH1 <- tapply(VN_VN1$Attention_Check_Green, VN_VN1$Exp_Subject_Id, get_word2)
summary(as.factor(VN_VN_P_ID$ATCH1 )) # One Grizzly's to be excluded! 

# 2 (1968)
VN_VN1$Attention_Check_CCCP <- ifelse(VN_VN1$Attention_Check_CCCP=="",NA,VN_VN1$Attention_Check_CCCP)
VN_VN_P_ID$ATCH2 <- tapply(VN_VN1$Attention_Check_CCCP, VN_VN1$Exp_Subject_Id, get_word2)
summary(as.factor(VN_VN_P_ID$ATCH2 ))

# 3 (Motýl/Butterfly)
VN_VN1$Attention_Check_Butter <- ifelse(VN_VN1$Attention_Check_Butter=="",NA,VN_VN1$Attention_Check_Butter)
VN_VN_P_ID$ATCH3 <- tapply(VN_VN1$Attention_Check_Butter, VN_VN1$Exp_Subject_Id, get_word2)
summary(as.factor(VN_VN_P_ID$ATCH3))

#-------------------------------------------------------------------------------
# 4 Social background (outside social media): 
#-------------------------------------------------------------------------------

VN_VN_P_ID$SocBack <- "SocBack"

# Travel abroad: 
VN_VN1$Travel_Abroad <- ifelse(VN_VN1$Travel_Abroad=="",NA,VN_VN1$Travel_Abroad)
VN_VN_P_ID$ScoreAbroad <- tapply(VN_VN1$Travel_Abroad, VN_VN1$Exp_Subject_Id, get_word2)
summary(as.factor(VN_VN_P_ID$ScoreAbroad))

# Family background: 

VN_VN1$Family_SOC <- ifelse(VN_VN1$Family_SOC=="",NA,VN_VN1$Family_SOC)
VN_VN_P_ID$Fami_Back <- tapply(VN_VN1$Family_SOC, VN_VN1$Exp_Subject_Id, get_word2)
# Correcting the level with wrong label: 
VN_VN_P_ID$Fami_Back[VN_VN_P_ID$Fami_Back=="No family"] <- "PreferNR"
summary(as.factor(VN_VN_P_ID$Fami_Back))


#-------------------------------------------------------------------------------
# 5 Social media use... All variables  are coded as numbers...
# 5.1 SNS - general - (c) Boer & Eijinden (2018 2022)
#-------------------------------------------------------------------------------

# How many times a day - check the content: "JakCastoPas1"
# 1 = "Nikdy nebou méně než jednou denně"
# 2 = "1-2krát denně"
# 3 = "3-5krát denně"
# 4 = "6-10krát denně"
# 5 = "11-20krát denně"
# 6 = "21-40krát denně"
# 7 = "Více než 40krát denně"
VN_VN1$JakCastoPas <- ifelse(VN_VN1$HowOftenPassive=="",NA,VN_VN1$HowOftenPassive)
VN_VN_P_ID$SMUi_Q1 <- tapply(VN_VN1$JakCastoPas, VN_VN1$Exp_Subject_Id, get_word2)

# How many times a week - posting: "JakCastoPost1"
# 1 = "Nikdy nebou méně než jednou týdně"
# 2 = "1-2krát týdně"
# 3 = "3-5krát týdně"
# 4 = "6-10krát týdně"
# 5 = "11-20krát týdně"
# 6 = "21-40krát týdně"
# 7 = "Více než 40krát týdně"
VN_VN1$JakCastoPost1 <- ifelse(VN_VN1$HowOftenPosting=="",NA,VN_VN1$HowOftenPosting)
VN_VN_P_ID$SMUi_Q2 <- tapply(VN_VN1$JakCastoPost1, VN_VN1$Exp_Subject_Id, get_word2)

# How many times a week - likes posts of the others: "JakCastoLikes1"
# 1 = "Nikdy nebou méně než jednou týdně"
# 2 = "1-2krát týdně"
# 3 = "3-5krát týdně"
# 4 = "6-10krát týdně"
# 5 = "11-20krát týdně"
# 6 = "21-40krát týdně"
# 7 = "Více než 40krát týdně"
VN_VN1$JakCastoLikes1 <- ifelse(VN_VN1$HowOftenLiking=="",NA,VN_VN1$HowOftenLiking)
VN_VN_P_ID$SMUi_Q3 <- tapply(VN_VN1$JakCastoLikes1, VN_VN1$Exp_Subject_Id, get_word2)

########
#      #
# (B)  # Social Media Use Intensity -> Mean, based on BOER... 
#      #
########

VN_VN_P_ID$SMUi_MEAN <- apply(VN_VN_P_ID[,c(30:32)],1,mean)# The higher the more intensive user: 

#-------------------------------------------------------------------------------
# 5.2 Social media use - negative effects - FOMO, Too Much Time, Miss Others...
#-------------------------------------------------------------------------------

# 1 = zcela souhlasím (Totally agree)
# 2 = spíše souhlasím (rather agree)
# 3 = Ani souhlas ani nesouhlas (neither agree nor disagree)
# 4 = Spíše nesouhlasím (rather disagree)
# 5 = zcela nesouhlasím (Totally disagree)

# SNS_too_much_time:
VN_VN1$SNS_too_much_time
VN_VN_P_ID$SM_too_much_time <- tapply(VN_VN1$SNS_too_much_time, VN_VN1$Exp_Subject_Id, get_word2)

# SNS_sometimes_miss_other:
VN_VN1$SNS_Sometimes_miss_other
VN_VN_P_ID$Miss_other <- tapply(VN_VN1$SNS_Sometimes_miss_other, VN_VN1$Exp_Subject_Id, get_word2)

# SNS_know_better_than_friends:
VN_VN1$SNS_know_better_than_friends
VN_VN_P_ID$SM_friends_better <- tapply(VN_VN1$SNS_know_better_than_friends, VN_VN1$Exp_Subject_Id, get_word2)

# SMP_fear_of_missing_out:
VN_VN1$SMP_fear_of_missing_out
VN_VN_P_ID$FOMO_SM <- tapply(VN_VN1$SMP_fear_of_missing_out, VN_VN1$Exp_Subject_Id, get_word2)

# Recalculate so that the higher (up to five), the worse: 
View(data.frame(Orig=VN_VN_P_ID$SM_too_much_time,New=6 - VN_VN_P_ID$SM_too_much_time))
VN_VN_P_ID$SM_too_much_time <- 6 - VN_VN_P_ID$SM_too_much_time

VN_VN_P_ID$Miss_other <- 6 - VN_VN_P_ID$Miss_other

VN_VN_P_ID$SM_friends_better <- 6 - VN_VN_P_ID$SM_friends_better

VN_VN_P_ID$FOMO_SM <- 6 - VN_VN_P_ID$FOMO_SM

# The score will be like "The higher, the more intensive user": 
VN_VN_P_ID$Score_negative_use_SUM <- rowSums(VN_VN_P_ID[,34:37])
VN_VN_P_ID$Score_negative_use_Mean <- apply(VN_VN_P_ID[,34:37],1,mean)
VN_VN_P_ID$Score_negative_use_Mean[is.na(VN_VN_P_ID$Score_negative_use_Mean)]<-0


#-------------------------------------------------------------------------------
# 5.3 Which SNS - ACTIVELY, SEMI-ACTIVELY OR PASSIVELY
#-------------------------------------------------------------------------------

# AP - stands for ACTIVE/PASSIVE: 
VN_VN_P_ID$ACTPAS<- "ACTPAS"
VN_VN_P_ID$Facebook_AP <- tapply(VN_VN1$AP_Facebook,VN_VN1$Exp_Subject_Id, mean, na.rm=T)
VN_VN_P_ID$Facebook_AP[VN_VN_P_ID$Facebook_AP=="NaN"]<-NA
VN_VN_P_ID$Instagram_AP <- tapply(VN_VN1$AP_Instagram,VN_VN1$Exp_Subject_Id, mean, na.rm=T)
VN_VN_P_ID$Instagram_AP[VN_VN_P_ID$Instagram_AP=="NaN"]<-NA

VN_VN_P_ID$TikTok_AP <- tapply(VN_VN1$AP_TikTok,VN_VN1$Exp_Subject_Id, mean, na.rm=T)
VN_VN_P_ID$TikTok_AP[VN_VN_P_ID$TikTok_AP=="NaN"]<-NA
VN_VN_P_ID$ZALO_AP <- tapply(VN_VN1$AP_ZALO,VN_VN1$Exp_Subject_Id, mean, na.rm=T)
VN_VN_P_ID$ZALO_AP[VN_VN_P_ID$ZALO_AP=="NaN"]<-NA

VN_VN_P_ID$YouTube_AP <- tapply(VN_VN1$AP_YouTube,VN_VN1$Exp_Subject_Id, mean, na.rm=T)
VN_VN_P_ID$YouTube_AP[VN_VN_P_ID$YouTube_AP=="NaN"]<-NA
VN_VN_P_ID$Twitter_AP <- tapply(VN_VN1$AP_X..Twitter.,VN_VN1$Exp_Subject_Id, mean, na.rm=T)
VN_VN_P_ID$Twitter_AP[VN_VN_P_ID$Twitter_AP=="NaN"]<-NA


# A/P - self reported (if any...), should be 3 items...  
# First one - which one (NAME): 
VN_VN1$AP_Input.social.media_1 <- ifelse(VN_VN1$AP_Input.social.media_1=="",NA,VN_VN1$AP_Input.social.media_1)
VN_VN_P_ID$SELF_AP_NAME_1 <- tapply(VN_VN1$AP_Input.social.media_1, VN_VN1$Exp_Subject_Id, get_word2)

# How (active/passive) - yes, 1st self reported SNS is (contrary to the expectation) the name of the variable...
VN_VN_P_ID$SELF_REP1_AP <- tapply(VN_VN1$X1st.self.reported.SNS,VN_VN1$Exp_Subject_Id, mean, na.rm=T)
VN_VN_P_ID$SELF_REP1_AP[VN_VN_P_ID$SELF_REP1_AP=="NaN"]<-NA

# Second one - which one (NAME): 
VN_VN1$AP_Input.social.media.2 <- ifelse(VN_VN1$AP_Input.social.media.2=="",NA,VN_VN1$AP_Input.social.media.2)
VN_VN_P_ID$SELF_AP_NAME_2 <- tapply(VN_VN1$AP_Input.social.media.2, VN_VN1$Exp_Subject_Id, get_word2)

# How (active/passive) - yes, 1st self reported SNS is (contrary to the expectation) the name of the variable...
VN_VN_P_ID$SELF_REP2_AP <- tapply(VN_VN1$X2nd.self.reported.SNS,VN_VN1$Exp_Subject_Id, mean, na.rm=T)
VN_VN_P_ID$SELF_REP2_AP[VN_VN_P_ID$SELF_REP2_AP=="NaN"]<-NA

# Third one - which one (NAME): 
VN_VN1$AP_Input.socal.media.3 <- ifelse(VN_VN1$AP_Input.socal.media.3=="",NA,VN_VN1$AP_Input.socal.media.3)
VN_VN_P_ID$SELF_AP_NAME_3 <- tapply(VN_VN1$AP_Input.socal.media.3, VN_VN1$Exp_Subject_Id, get_word2)

# How (active/passive) - yes, 1st self reported SNS is (contrary to the expectation) the name of the variable...
VN_VN_P_ID$SELF_REP3_AP <- tapply(VN_VN1$X3nd.self.reported.SNS,VN_VN1$Exp_Subject_Id, mean, na.rm=T)
VN_VN_P_ID$SELF_REP3_AP[VN_VN_P_ID$SELF_REP3_AP=="NaN"]<-NA

# Recalculating: Now the higher score the less she uses it, maximum is 4: 
VN_VN_P_ID$Facebook_AP <- 5 - VN_VN_P_ID$Facebook_AP
VN_VN_P_ID$Instagram_AP <- 5 - VN_VN_P_ID$Instagram_AP

VN_VN_P_ID$TikTok_AP <- 5 - VN_VN_P_ID$TikTok_AP
VN_VN_P_ID$ZALO_AP <- 5 - VN_VN_P_ID$ZALO_AP

VN_VN_P_ID$YouTube_AP <- 5 - VN_VN_P_ID$YouTube_AP
VN_VN_P_ID$Twitter_AP <- 5 - VN_VN_P_ID$Twitter_AP

VN_VN_P_ID$SELF_REP1_AP <- 5 - VN_VN_P_ID$SELF_REP1_AP
VN_VN_P_ID$SELF_REP2_AP <- 5 - VN_VN_P_ID$SELF_REP2_AP
VN_VN_P_ID$SELF_REP3_AP <- 5 - VN_VN_P_ID$SELF_REP3_AP

# List of those I did not know before: 
# Locket - yes
VN_VN_P_ID[70,47] <- NA #
VN_VN_P_ID[70,48] <- NA #

VN_VN_P_ID[70,49] <- NA #
VN_VN_P_ID[70,50] <- NA #

VN_VN_P_ID[78,47] <- NA #
VN_VN_P_ID[78,48] <- NA #

VN_VN_P_ID[109,47] <- NA # 
VN_VN_P_ID[109,48] <- NA # 

VN_VN_P_ID[109,49] <- NA # 
VN_VN_P_ID[109,50] <- NA # 

VN_VN_P_ID[109,51] <- NA # 
VN_VN_P_ID[109,52] <- NA # 

# Score 1: A/P punished: 
# max(vector_APs) + 0.75(^1)*max(vector_APs[vector_APs!=max()]) + 0.75(^2)*...

# Put these variables in a vector by rows:
ActPASS<-matrix(NA, nrow=9,ncol=123)
colnames(ActPASS) <- (VN_VN_P_ID[,1])
AP_Scores <- as.vector(rep(NA,123))

for (i in 1:nrow(VN_VN_P_ID)) {
  ActPASS[,i]<-unlist(VN_VN_P_ID[i,c(41:46, 48, 50, 52)])
  ActPASS[,i]<-ifelse(is.na(ActPASS[,i]),0,ActPASS[,i])
}

# Define the punishment factor and the length of the vector to consider
punishment_factor <- 0.75
vect_length <- nrow(ActPASS) 

# Create a vector of the punishment multipliers
multipliers <- punishment_factor^(0:(vect_length-1))

# Calculate scores using vectorized operations: Check whether it takes all the highest possible scores (If there is three times 6,
# does it count each?) 
AP_Scores <- apply(ActPASS, 2, function(column) {
  sorted_column <- sort(column, decreasing = TRUE)
  sum(sorted_column[1:vect_length] * multipliers)
})

VN_VN_P_ID$AP_Scores <- AP_Scores


# Get coordinates of "horizontal vectors of medals": 
medals <- c(4,2,1,0)

# Calculate scores using vectorized operations: Check whether it takes all the highest possible scores (If there is three times 6,
# does it count each?) 

Medal_matrix <- apply(ActPASS, c(1, 2), apply_medal)

Medal_sums <- colSums(Medal_matrix)

VN_VN_P_ID$ActPAS_medals <- Medal_sums


#-------------------------------------------------------------------------------
# 5.4 Frequency of using of SPECIFIC social media (i.e., one by one):
#-------------------------------------------------------------------------------

VN_VN_P_ID$FreqSPEC<- "FreqSPEC"

# How_frequently_preselected: 
# 1 = I don't use it (Síť nepoužívám)
# 2 = Once a week or less (Jednou týdně či méně)
# 3 = Once a week, not daily (Víckrát týdně, ale ne denně)
# 4 = Once a day (Jednou za den)
# 5 = Several times a day (Několikrát denně)
# 6 = Frequently during the day (Často během dne) 

# The higher score the better - no need to recalculate anything

# Facebook = How_frequently_Facebook
VN_VN_P_ID$Facebook_Freq <- tapply(VN_VN1$How_frequently_Facebook,VN_VN1$Exp_Subject_Id, mean, na.rm=T)
VN_VN_P_ID$Facebook_Freq[VN_VN_P_ID$Facebook_Freq=="NaN"]<-NA

# YouTube = How_frequently_YouTube
VN_VN_P_ID$YouTube_Freq <- tapply(VN_VN1$How_frequently_YouTube,VN_VN1$Exp_Subject_Id, mean, na.rm=T)
VN_VN_P_ID$YouTube_Freq[VN_VN_P_ID$YouTube_Freq=="NaN"]<-NA

# Instagram = How_frequently_Instagram
VN_VN_P_ID$Instagram_Freq <- tapply(VN_VN1$How_frequently_Instagram,VN_VN1$Exp_Subject_Id, mean, na.rm=T)
VN_VN_P_ID$Instagram_Freq[VN_VN_P_ID$Instagram_Freq=="NaN"]<-NA

# TikTok = How_frequently_TikTok
VN_VN_P_ID$TikTok_Freq <- tapply(VN_VN1$How_frequently_TikTok,VN_VN1$Exp_Subject_Id, mean, na.rm=T)
VN_VN_P_ID$TikTok_Freq[VN_VN_P_ID$TikTok_Freq=="NaN"]<-NA


# X[Twitter] = How_frequently_X
VN_VN_P_ID$X_Freq <- tapply(VN_VN1$How_frequently_X,VN_VN1$Exp_Subject_Id, mean, na.rm=T)
VN_VN_P_ID$X_Freq[VN_VN_P_ID$X_Freq=="NaN"]<-NA

# WeChat = How_Frequently_WECHAT
VN_VN_P_ID$ZALO_Freq <- tapply(VN_VN1$How_Frequently_Zalo,VN_VN1$Exp_Subject_Id, mean, na.rm=T)
VN_VN_P_ID$ZALO_Freq[VN_VN_P_ID$ZALO_Freq=="NaN"]<-NA


# Self reported SNS: 
# !! ALERT: The name "TIME" here does NOT refer to time from the beginning until the hit button
# Time = frequency...

# NAME_1: TIME_Input social media_1
VN_VN1$TIME_Input.social.media_1 <- ifelse(VN_VN1$TIME_Input.social.media_1=="",NA,VN_VN1$TIME_Input.social.media_1)
VN_VN_P_ID$Time_SELFREP_Name1 <- tapply(VN_VN1$TIME_Input.social.media_1, VN_VN1$Exp_Subject_Id, get_word2)

# Frequency_1: TIME_self reported SNS
VN_VN_P_ID$Time_SELFREP_Freq1 <- tapply(VN_VN1$TIME_self.reported.SNS,VN_VN1$Exp_Subject_Id, mean, na.rm=T)
VN_VN_P_ID$Time_SELFREP_Freq1[VN_VN_P_ID$Time_SELFREP_Freq1=="NaN"]<-NA

# NAME_2: TIME_Input social media_2
# Likert scale accidentaly set to == 2 as an implicit value. Therefore each 
# Participant who did not mark anyting is ascribed 2, instead of NaN / NA. This muset be fixed: 
VN_VN1$TIME_Input.social.media.2 <- ifelse(VN_VN1$TIME_Input.social.media.2=="",NA,VN_VN1$TIME_Input.social.media.2)
VN_VN_P_ID$Time_SELFREP_Name2 <- tapply(VN_VN1$TIME_Input.social.media.2, VN_VN1$Exp_Subject_Id, get_word2)

# Frequency_2: TIME_2nd self reported SNS
VN_VN_P_ID$Time_SELFREP_Freq2 <- tapply(VN_VN1$TIME_2nd.self.reported.SNS,VN_VN1$Exp_Subject_Id, mean, na.rm=T)
VN_VN_P_ID$Time_SELFREP_Freq2[VN_VN_P_ID$Time_SELFREP_Freq2=="NaN"]<-NA

VN_VN_P_ID$Time_SELFREP_Freq2 <- ifelse(is.na(VN_VN_P_ID$Time_SELFREP_Name2), NA, VN_VN_P_ID$Time_SELFREP_Freq2 )

# NAME_3: TIME_Input social media_3
VN_VN1$TIME_Input.social.media_3 <- ifelse(VN_VN1$TIME_Input.social.media_3=="",NA,VN_VN1$TIME_Input.social.media_3)
VN_VN_P_ID$Time_SELFREP_Name3 <- tapply(VN_VN1$TIME_Input.social.media_3, VN_VN1$Exp_Subject_Id, get_word2)

# Frequency_3: TIME_3nd self reported SNS
VN_VN_P_ID$Time_SELFREP_Freq3 <- tapply(VN_VN1$TIME_3nd.self.reported.SNS,VN_VN1$Exp_Subject_Id, mean, na.rm=T)
VN_VN_P_ID$Time_SELFREP_Freq3[VN_VN_P_ID$Time_SELFREP_Freq3=="NaN"]<-NA

VN_VN_P_ID[27,62] <- NA #
VN_VN_P_ID[27,63] <- NA #

VN_VN_P_ID[27,64] <- NA #
VN_VN_P_ID[27,65] <- NA #

VN_VN_P_ID[27,66] <- NA #
VN_VN_P_ID[27,67] <- NA #

# This one forgot he already replied on TikTok and Facebook and now replies differently. Let's exclude this guy
VN_VN_P_ID <- VN_VN_P_ID[-(70),]

#------------------

########
#      #
# (C)  # Specific Intensity Use -> Punished Score, one of the six (nine) social media 
#      #
########

# Frequency scores: 
# Score 1: Freq - punished: 

# Put these variables in a vector by rows:
FreqSC<-matrix(NA, nrow=9,ncol=122)
colnames(FreqSC) <- (VN_VN_P_ID[,1])
Freq_Scores <- as.vector(rep(NA,122)) # You have 292 rows, altough some are NAs

for (i in 1:nrow(VN_VN_P_ID)) {
  FreqSC[,i]<-unlist(VN_VN_P_ID[i,c(56:61,63,65,67)])
  FreqSC[,i]<-ifelse(is.na(FreqSC[,i]),0,FreqSC[,i])
}

# Define the punishment factor and the length of the vector to consider
punishment_factor <- 0.75
vect_length <- nrow(FreqSC) 

# Create a vector of the punishment multipliers
multipliers <- punishment_factor^(0:(vect_length-1));multipliers

# Calculate scores using vectorized operations
Freq_Scores <- apply(FreqSC, 2, function(column) {
  sorted_column <- sort(column, decreasing = TRUE)
  sum(sorted_column[1:vect_length] * multipliers)
})
Freq_Scores
hist(Freq_Scores) # Normally distributed, except those who are, in fact, NAs

VN_VN_P_ID$Freq_Scores <- Freq_Scores
VN_VN_P_ID$Freq_Scores_SUM<-rowSums(VN_VN_P_ID[,c(56:61,63,65,67)], na.rm=T)



# Other available scores: 
# Frequency of use: General (median score from the three variables) 
VN_VN_P_ID$SMUi_MEAN
hist(VN_VN_P_ID$SMUi_MEAN, breaks=25)

# Frequency of use: SPECIFIC - the higher the more intensive & on more SMs
VN_VN_P_ID$Freq_Scores
hist(VN_VN_P_ID$Freq_Scores, breaks=25)

# Actvie/Passive: the higher the more active & more individual SM
VN_VN_P_ID$AP_Scores
hist(VN_VN_P_ID$AP_Scores, breaks=25)

# Negative use - the higher the worse: 
VN_VN_P_ID$Score_negative_use_SUM
hist(VN_VN_P_ID$Score_negative_use_SUM, breaks=25)
hist(VN_VN_P_ID$Score_negative_use_Mean, breaks=25)

hist(VN_VN_P_ID$Freq_Scores_SUM,breaks=25)


# SMU-i - based only on "Visual" social media: TikTok, YouTube, Instagram
VN_VN_P_ID$VisualFreqSUM <- rowSums(VN_VN_P_ID[,c(57:59)])


########
#      #
# (A)  # Visual International Social Media Use -> Punished Score, one of the three Key Coefficients 
#      #
########

# Punished Score: 

# Put these variables in a vector by rows:
FreqSC<-matrix(NA, nrow=3,ncol=122)
colnames(FreqSC) <- (VN_VN_P_ID[,1])
Freq_Scores <- as.vector(rep(NA,122))

for (i in 1:nrow(VN_VN_P_ID)) {
  FreqSC[,i]<-unlist(VN_VN_P_ID[i,c(57:59)])
  FreqSC[,i]<-ifelse(is.na(FreqSC[,i]),0,FreqSC[,i])
}


# Define the punishment factor and the length of the vector to consider
punishment_factor <- 0.75
vect_length <- nrow(FreqSC) 

# Create a vector of the punishment multipliers
multipliers <- punishment_factor^(0:(vect_length-1))

# Calculate scores using vectorized operations
Freq_Scores <- apply(FreqSC, 2, function(column) {
  sorted_column <- sort(column, decreasing = TRUE)
  sum(sorted_column[1:vect_length] * multipliers)
})

VN_VN_P_ID$VisualFreq_Punish <- Freq_Scores


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# How related are these scales...
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Create a new data frame -> Add NAs instead of zeros (null score was not possible but when no response was provided):
VN_MUi <- data.frame(SMUi_MEAN=VN_VN_P_ID$SMUi_MEAN,
                     Freq_Scores=VN_VN_P_ID$Freq_Scores,
                     Freq_Scores_SUM=VN_VN_P_ID$Freq_Scores_SUM,
                     AP_Scores=VN_VN_P_ID$AP_Scores,
                     Score_negative_use_SUM=as.numeric(VN_VN_P_ID$Score_negative_use_SUM),
                     Score_negative_use_Mean=as.numeric(VN_VN_P_ID$Score_negative_use_Mean),
                     VisualFreq_Punish=as.numeric(VN_VN_P_ID$VisualFreq_Punish),
                     VisualFreqSUM=as.numeric(VN_VN_P_ID$VisualFreqSUM))


VN_MUi$SMUi_MEAN[VN_MUi$SMUi_MEAN==0]<-NA
VN_MUi$Freq_Scores[VN_MUi$Freq_Scores==0]<-NA
VN_MUi$AP_Scores[VN_MUi$AP_Scores==0]<-NA
VN_MUi$VisualFreq_Punish[VN_MUi$VisualFreq_Punish==0]<-NA
VN_MUi$Freq_Scores_SUM[VN_MUi$Freq_Scores_SUM==0]<-NA

VN_MUi <- VN_MUi[!is.na(VN_MUi$SMUi_MEAN),] # It looks like those to start filling in these scores actually filled in the whole 

rcorr(as.matrix(VN_MUi))


#-------------------------------------------------------------------------------
# 5.6. Preparation for the median splits (low/high intensity users)
#-------------------------------------------------------------------------------

# Which variables interests me at this point: 

# [1] A scale to assess the SMU-i: The candidate variables: 
VN_VN_P_ID$Score_negative_use_SUM # Score - negative use
VN_VN_P_ID$SMUi_MEAN # Scale Based on Boer et al. 2022

VN_VN_P_ID$AP_Scores # Score - Active Passive (Punished Score)
VN_VN_P_ID$ActPAS_medals # Score - Active Passive (Medals)

VN_VN_P_ID$Freq_Scores # Frequency of use of specific media - punished score
VN_VN_P_ID$Freq_Scores_SUM # Frequency score of specific media - sum

VN_VN_P_ID$VisualFreqSUM # Frequency of use visual media - sum
VN_VN_P_ID$VisualFreq_Punish # Frequency of use visual media - Punished Score

# Get rid of the... 979456
VN_VN1 <- VN_VN1[VN_VN1$Exp_Subject_Id!="979456",]

VN_VN1$Use_SNS_1 <- ifelse(VN_VN1$Use_SNS_1=="",NA,VN_VN1$Use_SNS_1)
VN_VN_P_ID$Social_Yes_No <- tapply(VN_VN1$Use_SNS_1, VN_VN1$Exp_Subject_Id, get_word2)
summary(as.factor(VN_VN_P_ID$Social_Yes_No)) # Row 29, 50 - both were probably hones, since they otherwise finished the survey! 
# Thus they must be assigned zero in column 33, 68 and 71, so that they are not excluded
VN_VN_P_ID[29,33] <- 0
VN_VN_P_ID[50,33] <- 0

# Passed all tests (Score, AttentionCH1-CH3): 
VN_VN_IP_good <- VN_VN_P_ID[VN_VN_P_ID$TestInterpr=="Good"|VN_VN_P_ID$TestInterpr=="Borderline",]
VN_VN_IP_good <- VN_VN_IP_good[!is.na(VN_VN_IP_good$ATCH3),]


# Now - those who lacks score cannot be split: 
VN_VN_IP_good <- VN_VN_IP_good[!is.na(VN_VN_IP_good$SMUi_MEAN),]

# Adding the rest of the variables for a rater (SMU-i, etc.):
VN_VN_IP_good$Score_negative_use_Mean # Score - negative use
VN_VN_IP_good$SMUi_MEAN # Scale Based on Boer et al. 2022

VN_VN_IP_good$AP_Scores # Score - Active Passive (Punished Score)
VN_VN_IP_good$ActPAS_medals # Score - Active Passive (Medals)

VN_VN_IP_good$Freq_Scores # Frequency of use of specific media - punished score
VN_VN_IP_good$Freq_Scores_SUM # Frequency score of specific media - sum

VN_VN_IP_good$VisualFreqSUM # Frequency of use visual media - sum
VN_VN_IP_good$VisualFreq_Punish # Frequency of use visual media - Punished Score


# No NAs or zeros, let's split (a-b-c): 
SMUi_Mean <- median(VN_VN_IP_good$SMUi_MEAN) # YES (b)

Freq_Scores_median <- median(VN_VN_IP_good$Freq_Scores) # YES (c)

VisualFreq_Punish_median <- median(VN_VN_IP_good$VisualFreq_Punish) # YES (a)



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 5.7. Median splits: Simplified - for the full-size version see some previous versions of this script...
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# How it performs in splitting: 0<,0.5==,>1

#.-.-.-
# 1 Scale Based on Boer et al. 2022 - SCORE (b)
VN_VN_IP_good$SMUi_MEAN_Mean_SPLIT <- ifelse(VN_VN_IP_good$SMUi_MEAN<SMUi_Mean,0,
                                             ifelse(VN_VN_IP_good$SMUi_MEAN==SMUi_Mean,0.5,1))
summary(as.factor(VN_VN_IP_good$SMUi_MEAN_Mean_SPLIT))


#.-.-.-
# 2 Frequency of use of media - per given media (Facebook, Instagram, Twitter, WeChat, TikTok, YouTube + self-reported) - punished score (c)
VN_VN_IP_good$Freq_Scores_SPLIT <- ifelse(VN_VN_IP_good$Freq_Scores<Freq_Scores_median,0,
                                          ifelse(VN_VN_IP_good$Freq_Scores==Freq_Scores_median,0.5,1))
summary(as.factor(VN_VN_IP_good$Freq_Scores_SPLIT))


#.-.-.-
# 3 Frequency of use visual media - Punished Score (YouTube, TikTok, Instagram) - ONE OF THE FOCUS SCORES
VN_VN_IP_good$VisualFreq_Punish_median_SPLIT <- ifelse(VN_VN_IP_good$VisualFreq_Punish<VisualFreq_Punish_median,0,
                                                       ifelse(VN_VN_IP_good$VisualFreq_Punish==VisualFreq_Punish_median,0.5,1))
summary(as.factor(VN_VN_IP_good$VisualFreq_Punish_median_SPLIT))


# How does splitting according to different categories work: 
hist(rowSums(VN_VN_IP_good[,73:75]),breaks=12) # Brute force kinda works...
# Now - I am NOT interested in every variable of those... for the sake of visual diet. 



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 5.8. # Alternative attitude - excluding 25 % in the middle: 
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

VN_VN_IP_good$SMUi_MEAN
VN_VN_IP_good$Freq_Scores
VN_VN_IP_good$VisualFreq_Punish

VN_VN_IP_good$SMUi_Mean_25_percent <- ifelse(VN_VN_IP_good$SMUi_MEAN<=quantile(VN_VN_IP_good$SMUi_MEAN,0.375),"sides",
                                             ifelse(VN_VN_IP_good$SMUi_MEAN>=quantile(VN_VN_IP_good$SMUi_MEAN,0.625),"sides","centre"))
summary(as.factor(VN_VN_IP_good$SMUi_Mean_25_percent))


VN_VN_IP_good$Freq_Scores_25_percent <- ifelse(VN_VN_IP_good$Freq_Scores<=quantile(VN_VN_IP_good$Freq_Scores,0.375),"sides",
                                               ifelse(VN_VN_IP_good$Freq_Scores>=quantile(VN_VN_IP_good$Freq_Scores,0.625),"sides","centre"))
summary(as.factor(VN_VN_IP_good$Freq_Scores_25_percent))


VN_VN_IP_good$VisualFreqPunish_25_percent <- ifelse(VN_VN_IP_good$VisualFreq_Punish<quantile(VN_VN_IP_good$VisualFreq_Punish,0.375),"sides",
                                                    ifelse(VN_VN_IP_good$VisualFreq_Punish>quantile(VN_VN_IP_good$VisualFreq_Punish,0.625),"sides","centre"))
summary(as.factor(VN_VN_IP_good$VisualFreqPunish_25_percent)) # There is nothing in between 37.5 % and 62.5 % of the distribution.



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 5.9. Splitting (Above, Mediocre, Below)
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Split based on "A" (=VisualFreq_Punish)
#                    if not possible, split based on "B" (=SMUi_MEAN)
#                            if not possible, split based on "C" (=Freq_Scores)
# Otherwise "Mediocre"
VN_VN_IP_good$VisualFreq_Punish_median_SPLIT
VN_VN_IP_good$Split <- ifelse(VN_VN_IP_good$VisualFreq_Punish_median_SPLIT==1,"Above",
                              ifelse(VN_VN_IP_good$VisualFreq_Punish_median_SPLIT==0,"Below","Mediocre"))

# Step 2: For rows where Split is still "Mediocre", check the "SMUi_MEAN" variable
mediocre_indices <- which(VN_VN_IP_good$Split == "Mediocre")
VN_VN_IP_good$Split[mediocre_indices] <- ifelse(VN_VN_IP_good$SMUi_MEAN_Mean_SPLIT[mediocre_indices] == 1, "Above",
                                                ifelse(VN_VN_IP_good$SMUi_MEAN_Mean_SPLIT[mediocre_indices] == 0, "Below", "Mediocre"))

summary(as.factor(VN_VN_IP_good$Split))
 


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 6.0 # Adding the ratings.
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# (1) Cut the data.frame based on rater's ID: 
# (2) Get the ratings - if she left some faces not-rated, add NAs (mind the specific length of each sample: 
#     2.1. CZ 2016: 50M + 50F
#     2.2. CZ 2019: 39M + 56F

# Faces: 
VN_VN1$ATR_16F
VN_VN1$ATR_16M
VN_VN1$ATR_19F
VN_VN1$ATR_19M

# How to (re)order it: Spot the variable "Trial ID" and block name:  
# And since you got the IDs (of both photos, raters, and blocks), you can get rid of NAs:
names(VN_VN1[,c(2,6,22:25,45:48,114:117,134)])
VNRAT_List_FACES <- as.data.frame(VN_VN1[,c(2,6,22:25,45:48,114:117,134)]) # Only rating variables + IDs (all of them)

# Adding the order in the table
VNRAT_List_FACES$OrOd <- seq(from=1, to=nrow(VNRAT_List_FACES), by=1)

# Only rows that contain ratings are of interest: 
levels(as.factor(VNRAT_List_FACES$Task_Name))
selected_levs <- c("CZ_16F_ATD","CZ_16M_ATD","CZ_19F_ATD","CZ_19M_ATD")  

VNRAT_List_FACES <- VNRAT_List_FACES[VNRAT_List_FACES$Task_Name %in% selected_levs,]
VNRAT_List_FACES$FullID <- paste(VNRAT_List_FACES$Trial_Id,VNRAT_List_FACES$Task_Name)


# Match the two tables - the one with IDs and the one with ratings: 
VNRAT_List_FACES <- merge(VNRAT_List_FACES, real_IDs, by = "FullID", all = TRUE)

VNRAT_List_FACES <- VNRAT_List_FACES[order(VNRAT_List_FACES$OrOd),]

names(VNRAT_List_FACES)[names(VNRAT_List_FACES) == 'Exp_Subject_Id'] <- "VN_VN_P_ID"

# These are then the (only) raters you are interested in: 
# Subset the table: CZ_CZ1 - take only those "selected" raters
# Put the attractiveness, trustworthiness, and dominance, that are now split into 
# four columns each into just three columns (one for trustw., one for attractiveness, one for dominance):

VNRAT_List_FACES <- VNRAT_List_FACES[VNRAT_List_FACES$Task_Name %in% selected_levs,]

VNRAT_List_FACES <- merge(VNRAT_List_FACES, VN_VN_IP_good, by = "VN_VN_P_ID", all = F)
VNRAT_List_FACES <- VNRAT_List_FACES[order(VNRAT_List_FACES$OrOd),]

#-------------------------------------------------------------------------------
# 6.2 Finalising, the first step is "there is always just one non-zero number in each quartet
#-------------------------------------------------------------------------------

VNRAT_List_FACES$ATR <- rowSums(VNRAT_List_FACES[,5:8], na.rm = T)
VNRAT_List_FACES$TRU <- rowSums(VNRAT_List_FACES[,13:16], na.rm = T)
VNRAT_List_FACES$DOM <- rowSums(VNRAT_List_FACES[,9:12], na.rm = T)

# The core of the model is like this: 
# I am interested in the effect Sample (country of origin) and SNS use intensity (expressed by whichever variable)
# as Fixed Effects have on rating scale / scales 

VNRAT_List_FACES$ATR
VNRAT_List_FACES$TRU
VNRAT_List_FACES$DOM

VNRAT_List_FACES$Sample <- "VN"

# The simplest way: separate model for each scale of these...

VN_USE_THIS_FOR_LONG_TABLE <- data.frame(Particip_ID = VNRAT_List_FACES[,1],
                        Face_ID = VNRAT_List_FACES[,23],
                        Above_Below = VNRAT_List_FACES[,101],
                        Atr = VNRAT_List_FACES[,102],
                        Tru = VNRAT_List_FACES[,103],
                        Dom = VNRAT_List_FACES[,104],
                        Sample = VNRAT_List_FACES[,105],
                        CentreSide = paste(VNRAT_List_FACES$SMUi_Mean_25_percent,
                                           VNRAT_List_FACES$Freq_Scores_25_percent,
                                           VNRAT_List_FACES$VisualFreqPunish_25_percent),
                        ScoreAbroad = VNRAT_List_FACES[,50],
                        Fami_Back = VNRAT_List_FACES[,51]
)

save(VN_USE_THIS_FOR_LONG_TABLE, file="VN_USE_THIS_FOR_LONG_TABLE.Rdata")

# Specify the objects you want to keep
objects_to_keep <- c("CZ_USE_THIS_FOR_LONG_TABLE",
                     "VN_USE_THIS_FOR_LONG_TABLE", 
                     "CZ_CZ_IP_good",
                     "VN_VN_IP_good",
                     "apply_medal", "get_word2", "real_IDs")  # Replace with your object names

# Remove all other objects
rm(list = setdiff(ls(), objects_to_keep))




#-------------------------------------------------------------------------------
# Data from South Africa (only necessary comments added): 
#-------------------------------------------------------------------------------

RSA_RSA1 <- read.csv2("RJA_CZ_face_SocMed_Checked.csv", T, sep=,)

# NOTES: 
# The participant No. 902319 filled some portions of the questionnaire twice. His second, unifnished 
# particiaption had been deleted before the data was uploaded... 

summary(as.factor(RSA_RSA1$SelectedLocation_129)) # All raters resided in ZA = South Africa

RSA_RSA_P_ID <- levels(as.factor(RSA_RSA1$Exp_Subject_Id_117)) # Participants IDs
# Get rid of those who did not proceed: 
RSA_RSA_P_ID<-as.data.frame(RSA_RSA_P_ID)
RSA_RSA_P_ID$lengths <- tapply(RSA_RSA1$Rec_Session_Id_1, RSA_RSA1$Exp_Subject_Id_117, length)
hist(RSA_RSA_P_ID$lengths) # All shall had finished the study. Those whose lenght is longer than ~115 are strange, perhaps 
# the same person (with the same ID) initiated the study, interruped and started again, idk, will check. 
# The 116 (1×) started twice... We will get rid of these data as they are incomplete (if needed)
# 902308


#-------------------------------------------------------------------------------
# 1 Basic demographics 
#-------------------------------------------------------------------------------

# Participant's demographic variables...
# All these variables either are naturally numeric (age...), or were coded as numeric (device)

# AGE: 
RSA_RSA_P_ID$Age <- tapply(RSA_RSA1$SELF_age_1_62,RSA_RSA1$Exp_Subject_Id_117,mean, na.rm=T)
RSA_RSA_P_ID$Age

# HEIGHT: 
RSA_RSA_P_ID$Height <- tapply(as.numeric(RSA_RSA1$b_height_F1_28),RSA_RSA1$Exp_Subject_Id_117,mean, na.rm=T)
RSA_RSA_P_ID$Height

# WEIGHT: 
RSA_RSA_P_ID$weight <- tapply(as.numeric(RSA_RSA1$b_weight_F1_29),RSA_RSA1$Exp_Subject_Id_117,mean, na.rm=T)
RSA_RSA_P_ID$weight

# MARITAL STATUS: 
RSA_RSA_P_ID$MariStat <- tapply(RSA_RSA1$Marital_status_F1_52,RSA_RSA1$Exp_Subject_Id_117,mean, na.rm=T)
RSA_RSA_P_ID$MariStat


# Decoded to words: 
RSA_RSA_P_ID$MariStat_VERB <- ifelse(RSA_RSA_P_ID$MariStat==1,"Single",
                                     ifelse(RSA_RSA_P_ID$MariStat==2,"In_a_relationship",
                                            ifelse(RSA_RSA_P_ID$MariStat==3, "Married",
                                                   ifelse(RSA_RSA_P_ID$MariStat==4, "Divorced",
                                                          ifelse(RSA_RSA_P_ID$MariStat==5, "Widowed",
                                                                 ifelse(RSA_RSA_P_ID$MariStat==6, "DontWaDeclare",NA))))))

RSA_RSA_P_ID$MariStat_VERB

# MOTHER TONGUE:
RSA_RSA1$Mother_Tongue_1_53 <- ifelse(RSA_RSA1$Mother_Tongue_1_53=="",NA,RSA_RSA1$Mother_Tongue_1_53)
RSA_RSA_P_ID$MotherTongue <- tapply(RSA_RSA1$Mother_Tongue_1_53, RSA_RSA1$Exp_Subject_Id_117, get_word2)
RSA_RSA_P_ID$MotherTongue
# There were two participants who report being from Czech Republic - probably they were asked to pilot the study, but openned it 
# way to late - these were exluced (see above) and are no more here...


# PASSPORT NATIONALITY
RSA_RSA1$Passport.nationality_59 <- ifelse(RSA_RSA1$Passport.nationality_59=="",NA,RSA_RSA1$Passport.nationality_59)
RSA_RSA_P_ID$PasspNat <- tapply(RSA_RSA1$Passport.nationality_59, RSA_RSA1$Exp_Subject_Id_117, get_word2)

# Ethnicity (definition)
RSA_RSA1$Eth_understanding_40 <- ifelse(RSA_RSA1$Eth_understanding_40=="",NA,RSA_RSA1$Eth_understanding_40)
RSA_RSA_P_ID$EthUNDERSTD <- tapply(RSA_RSA1$Eth_understanding_40, RSA_RSA1$Exp_Subject_Id_117, get_word2)

# Ethnicity (self-reported)
RSA_RSA1$Self_reported_ethnicity_63 <- ifelse(RSA_RSA1$Self_reported_ethnicity_63=="",NA,RSA_RSA1$Self_reported_ethnicity_63)
RSA_RSA_P_ID$Eth_SelfREP <- tapply(RSA_RSA1$Self_reported_ethnicity_63, RSA_RSA1$Exp_Subject_Id_117, get_word2)

# Gender (self-reported):
RSA_RSA1$Gender.at.birth_1_42 <- ifelse(RSA_RSA1$Gender.at.birth_1_42=="",NA,RSA_RSA1$Gender.at.birth_1_42)
RSA_RSA_P_ID$SEXATB <- tapply(RSA_RSA1$Gender.at.birth_1_42, RSA_RSA1$Exp_Subject_Id_117, get_word2)


#-------------------------------------------------------------------------------
# 2 Technical variables
#-------------------------------------------------------------------------------

RSA_RSA_P_ID$Technical <- "TECHNICAL"
# Device auto-selected: 
RSA_RSA_P_ID$DEVICE_LABV <- tapply(RSA_RSA1$System_Spec_138, RSA_RSA1$Exp_Subject_Id_117, unique)

# Password for Entrance Examination:
RSA_RSA1$Participant_FEEDBACK_57 <- ifelse(RSA_RSA1$Participant_FEEDBACK_57=="",NA,RSA_RSA1$Participant_FEEDBACK_57)
RSA_RSA_P_ID$PASSW <- tapply(RSA_RSA1$Participant_FEEDBACK_57, RSA_RSA1$Exp_Subject_Id_117, unique)

# Window resolution: 
# WIDTH:
RSA_RSA_P_ID$Window_WWW <- tapply(RSA_RSA1$Window_Width_In_Pixels_142, RSA_RSA1$Exp_Subject_Id_117, unique)
# HEIGHT: 
RSA_RSA_P_ID$Window_HHH <- tapply(RSA_RSA1$Window_Width_In_Pixels_142, RSA_RSA1$Exp_Subject_Id_117, unique)

# Device screen resolution: 
# WIDTH:
RSA_RSA_P_ID$Screen_WWW <- tapply(RSA_RSA1$Screen_Width_In_Pixels_125, RSA_RSA1$Exp_Subject_Id_117, unique)
# HEIGHT: 
RSA_RSA_P_ID$Screen_HHH <- tapply(RSA_RSA1$Screen_Width_In_Pixels_125, RSA_RSA1$Exp_Subject_Id_117, unique)

# Selected location (where LabVanced thinks the person is located - may be tricky due to people travelling 
# a lot and VPNs...)
RSA_RSA_P_ID$SelectedLoc <- tapply(RSA_RSA1$SelectedLocation_129, RSA_RSA1$Exp_Subject_Id_117, unique)



#-------------------------------------------------------------------------------
# 3 Was the person conscious (attention checks and score in a test): 
#-------------------------------------------------------------------------------

RSA_RSA_P_ID$Conscious <- "CONSCIOUS"

# To get rid of a warning related to NA-only vectors, lets assign -1 to each body who did not bother to start the test:
RSA_RSA1$correct_31 <- ifelse(is.na(RSA_RSA1$correct_31),-1,RSA_RSA1$correct_31)

RSA_RSA_P_ID$TestScore <- tapply(as.numeric(RSA_RSA1$correct_31), RSA_RSA1$Exp_Subject_Id_117, max, na.rm=T)
RSA_RSA_P_ID$TestInterpr <- ifelse(RSA_RSA_P_ID$TestScore==5|RSA_RSA_P_ID$TestScore==4,"Good",ifelse(RSA_RSA_P_ID$TestScore==3,"Borderline","Exclude!"))

summary(as.factor(RSA_RSA_P_ID$TestInterpr))

# Attention Checks (the kind of variable where there is one word for the vector [OR NA]: 
# 1 (Green/Zeleny)
RSA_RSA1$Attention_Check_Green_27 <- ifelse(RSA_RSA1$Attention_Check_Green_27=="",NA,RSA_RSA1$Attention_Check_Green_27)
RSA_RSA_P_ID$ATCH1 <- tapply(RSA_RSA1$Attention_Check_Green_27, RSA_RSA1$Exp_Subject_Id_117, get_word2)
summary(as.factor(RSA_RSA_P_ID$ATCH1 )) # All passed! 

# 2 (1968)
RSA_RSA1$Attention_Check_CCCP_26 <- ifelse(RSA_RSA1$Attention_Check_CCCP_26=="",NA,RSA_RSA1$Attention_Check_CCCP_26)
RSA_RSA_P_ID$ATCH2 <- tapply(RSA_RSA1$Attention_Check_CCCP_26, RSA_RSA1$Exp_Subject_Id_117, get_word2)
summary(as.factor(RSA_RSA_P_ID$ATCH2 )) # All passed!


#-------------------------------------------------------------------------------
# 4 Social background (outside social media): 
#-------------------------------------------------------------------------------

RSA_RSA_P_ID$SocBack <- "SocBack"

# Travel abroad: 
# 1 = Velmi často = Often
# 2 = Spíše často = Rather often
# 3 = Občas = Occasionally
# 4 = Zřídka = Rarely
# 5 = Velmi zřídka = Very rarely
# 6 = Nikdy = never
RSA_RSA1$Travel_Abroad_96 <- ifelse(RSA_RSA1$Travel_Abroad_96=="",NA,RSA_RSA1$Travel_Abroad_96)
RSA_RSA_P_ID$ScoreAbroad <- tapply(RSA_RSA1$Travel_Abroad_96, RSA_RSA1$Exp_Subject_Id_117, get_word2)
summary(as.factor(RSA_RSA_P_ID$ScoreAbroad))

# Family background: 
# 1 = "Bohaté materiální zabezpečení, peníze jsme nikdy neřešili" = Rich 
# 2 = "Dost peněz, i když jsme se někdy museli uskormnit" = Upper CL
# 3 = "Někdy více, někdy méně, ale dokázali jsme vyžít bez ztráty úrovně" = Middle
# 4 = "Spíše málo peněz, ve srovnání s vrstevníky jsem se musel často uskromnit" = Lower CL 
# 5 = "Neustálé finanční potíže" = Poor
# 6 = "Nepřeji si odpovídat" = No family (the last level is labelled incorrectly, will be corrected)
RSA_RSA1$Family_SOC_1_41 <- ifelse(RSA_RSA1$Family_SOC_1_41=="",NA,RSA_RSA1$Family_SOC_1_41)
RSA_RSA_P_ID$Fami_Back <- tapply(RSA_RSA1$Family_SOC_1_41, RSA_RSA1$Exp_Subject_Id_117, get_word2)
# Correcting the level with wrong label: 
RSA_RSA_P_ID$Fami_Back[RSA_RSA_P_ID$Fami_Back=="No family"] <- "PreferNR"
summary(as.factor(RSA_RSA_P_ID$Fami_Back))


#-------------------------------------------------------------------------------
# 5 Social media use... All variables  are coded as numbers...
# 5.1 SNS - general - (c) Boer & Eijinden (2018 2022)
#-------------------------------------------------------------------------------

# How many times a day - check the content: "HowOftenPassive_1_50"
# 1 = "Nikdy nebou méně než jednou denně"
# 2 = "1-2krát denně"
# 3 = "3-5krát denně"
# 4 = "6-10krát denně"
# 5 = "11-20krát denně"
# 6 = "21-40krát denně"
# 7 = "Více než 40krát denně"
RSA_RSA1$HowOftenPassive_1_50 <- ifelse(RSA_RSA1$HowOftenPassive_1_50=="",NA,RSA_RSA1$HowOftenPassive_1_50)
RSA_RSA_P_ID$SMUi_Q1 <- tapply(RSA_RSA1$HowOftenPassive_1_50, RSA_RSA1$Exp_Subject_Id_117, get_word2)

# How many times a week - posting: "HowOftenPosting_1_51"
# 1 = "Nikdy nebou méně než jednou týdně"
# 2 = "1-2krát týdně"
# 3 = "3-5krát týdně"
# 4 = "6-10krát týdně"
# 5 = "11-20krát týdně"
# 6 = "21-40krát týdně"
# 7 = "Více než 40krát týdně"
RSA_RSA1$HowOftenPosting_1_51 <- ifelse(RSA_RSA1$HowOftenPosting_1_51=="",NA,RSA_RSA1$HowOftenPosting_1_51)
RSA_RSA_P_ID$SMUi_Q2 <- tapply(RSA_RSA1$HowOftenPosting_1_51, RSA_RSA1$Exp_Subject_Id_117, get_word2)

# How many times a week - likes posts of the others: "HowOftenLiking_1_49"
# 1 = "Nikdy nebou méně než jednou týdně"
# 2 = "1-2krát týdně"
# 3 = "3-5krát týdně"
# 4 = "6-10krát týdně"
# 5 = "11-20krát týdně"
# 6 = "21-40krát týdně"
# 7 = "Více než 40krát týdně"
RSA_RSA1$HowOftenLiking_1_49 <- ifelse(RSA_RSA1$HowOftenLiking_1_49=="",NA,RSA_RSA1$HowOftenLiking_1_49)
RSA_RSA_P_ID$SMUi_Q3 <- tapply(RSA_RSA1$HowOftenLiking_1_49, RSA_RSA1$Exp_Subject_Id_117, get_word2)

########
#      #
# (B)  # Social Media Use Intensity -> Mean, based on BOER... 
#      #
########

# Will then compute the mean value from the three variables' scores: 
RSA_RSA_P_ID$SMUi_MEAN <- apply(RSA_RSA_P_ID[,c(29:31)],1,mean)# The higher the more intensive user: 
# Do you know why it is not 30:32, but 29:31? Because U have one attention check less


#-------------------------------------------------------------------------------
# 5.2 Social media use - negative effects - FOMO, Too Much Time, Miss Others...
#-------------------------------------------------------------------------------

# 1 = zcela souhlasím (Totally agree)
# 2 = spíše souhlasím (rather agree)
# 3 = Ani souhlas ani nesouhlas (neither agree nor disagree)
# 4 = Spíše nesouhlasím (rather disagree)
# 5 = zcela nesouhlasím (Totally disagree)

# SNS_too_much_time:
RSA_RSA1$SNS_too_much_time_68
RSA_RSA_P_ID$SM_too_much_time <- tapply(RSA_RSA1$SNS_too_much_time_68, RSA_RSA1$Exp_Subject_Id_117, get_word2)

# SNS_sometimes_miss_other:
RSA_RSA1$SNS_Sometimes_miss_other_67
RSA_RSA_P_ID$Miss_other <- tapply(RSA_RSA1$SNS_Sometimes_miss_other_67, RSA_RSA1$Exp_Subject_Id_117, get_word2)

# SNS_know_better_than_friends:
RSA_RSA1$SNS_know_better_than_friends_66
RSA_RSA_P_ID$SM_friends_better <- tapply(RSA_RSA1$SNS_know_better_than_friends_66, RSA_RSA1$Exp_Subject_Id_117, get_word2)

# SMP_fear_of_missing_out:
RSA_RSA1$SMP_fear_of_missing_out_65
RSA_RSA_P_ID$FOMO_SM <- tapply(RSA_RSA1$SMP_fear_of_missing_out_65, RSA_RSA1$Exp_Subject_Id_117, get_word2)

# Recalculate so that the higher (up to five), the worse: 
View(data.frame(Orig=RSA_RSA_P_ID$SM_too_much_time,New=6 - RSA_RSA_P_ID$SM_too_much_time))
RSA_RSA_P_ID$SM_too_much_time <- 6 - RSA_RSA_P_ID$SM_too_much_time

RSA_RSA_P_ID$Miss_other <- 6 - RSA_RSA_P_ID$Miss_other

RSA_RSA_P_ID$SM_friends_better <- 6 - RSA_RSA_P_ID$SM_friends_better

RSA_RSA_P_ID$FOMO_SM <- 6 - RSA_RSA_P_ID$FOMO_SM

# The score will be like "The higher, the more intensive user": 
RSA_RSA_P_ID$Score_negative_use_SUM <- rowSums(RSA_RSA_P_ID[,33:36])
RSA_RSA_P_ID$Score_negative_use_Mean <- apply(RSA_RSA_P_ID[,33:36],1,mean)
RSA_RSA_P_ID$Score_negative_use_Mean[is.na(RSA_RSA_P_ID$Score_negative_use_Mean)]<-0


#-------------------------------------------------------------------------------
# 5.3 Which SNS - ACTIVELY, SEMI-ACTIVELY OR PASSIVELY
#-------------------------------------------------------------------------------

# AP - stands for ACTIVE/PASSIVE: 
RSA_RSA_P_ID$ACTPAS<- "ACTPAS"
RSA_RSA_P_ID$Facebook_AP <- tapply(RSA_RSA1$AP_Facebook,RSA_RSA1$Exp_Subject_Id, mean, na.rm=T)
RSA_RSA_P_ID$Facebook_AP[RSA_RSA_P_ID$Facebook_AP=="NaN"]<-NA
RSA_RSA_P_ID$Instagram_AP <- tapply(RSA_RSA1$AP_Instagram,RSA_RSA1$Exp_Subject_Id, mean, na.rm=T)
RSA_RSA_P_ID$Instagram_AP[RSA_RSA_P_ID$Instagram_AP=="NaN"]<-NA

RSA_RSA_P_ID$TikTok_AP <- tapply(RSA_RSA1$AP_TikTok,RSA_RSA1$Exp_Subject_Id, mean, na.rm=T)
RSA_RSA_P_ID$TikTok_AP[RSA_RSA_P_ID$TikTok_AP=="NaN"]<-NA
RSA_RSA_P_ID$WeChat_AP <- tapply(RSA_RSA1$AP_WeChat_19,RSA_RSA1$Exp_Subject_Id, mean, na.rm=T)
RSA_RSA_P_ID$WeChat_AP[RSA_RSA_P_ID$WeChat_AP=="NaN"]<-NA

RSA_RSA_P_ID$YouTube_AP <- tapply(RSA_RSA1$AP_YouTube,RSA_RSA1$Exp_Subject_Id, mean, na.rm=T)
RSA_RSA_P_ID$YouTube_AP[RSA_RSA_P_ID$YouTube_AP=="NaN"]<-NA
RSA_RSA_P_ID$Twitter_AP <- tapply(RSA_RSA1$AP_X..Twitter.,RSA_RSA1$Exp_Subject_Id, mean, na.rm=T)
RSA_RSA_P_ID$Twitter_AP[RSA_RSA_P_ID$Twitter_AP=="NaN"]<-NA


# A/P - self reported (if any...), should be 3 items...  
# First one - which one (NAME): 
RSA_RSA1$AP_Input.social.media_1 <- ifelse(RSA_RSA1$AP_Input.social.media_1=="",NA,RSA_RSA1$AP_Input.social.media_1)
RSA_RSA_P_ID$SELF_AP_NAME_1 <- tapply(RSA_RSA1$AP_Input.social.media_1, RSA_RSA1$Exp_Subject_Id, get_word2)

# How (active/passive) - yes, 1st self reported SNS is (contrary to the expectation) the name of the variable...
RSA_RSA_P_ID$SELF_REP1_AP <- tapply(RSA_RSA1$X1st.self.reported.SNS,RSA_RSA1$Exp_Subject_Id, mean, na.rm=T)
RSA_RSA_P_ID$SELF_REP1_AP[RSA_RSA_P_ID$SELF_REP1_AP=="NaN"]<-NA

# Second one - which one (NAME): 
RSA_RSA1$AP_Input.social.media.2 <- ifelse(RSA_RSA1$AP_Input.social.media.2=="",NA,RSA_RSA1$AP_Input.social.media.2)
RSA_RSA_P_ID$SELF_AP_NAME_2 <- tapply(RSA_RSA1$AP_Input.social.media.2, RSA_RSA1$Exp_Subject_Id, get_word2)

# How (active/passive) - yes, 1st self reported SNS is (contrary to the expectation) the name of the variable...
RSA_RSA_P_ID$SELF_REP2_AP <- tapply(RSA_RSA1$X2nd.self.reported.SNS,RSA_RSA1$Exp_Subject_Id, mean, na.rm=T)
RSA_RSA_P_ID$SELF_REP2_AP[RSA_RSA_P_ID$SELF_REP2_AP=="NaN"]<-NA

# Third one - which one (NAME): 
RSA_RSA1$AP_Input.social.media.3_15 <- ifelse(RSA_RSA1$AP_Input.social.media.3_15=="",NA,RSA_RSA1$AP_Input.social.media.3_15)
RSA_RSA_P_ID$SELF_AP_NAME_3 <- tapply(RSA_RSA1$AP_Input.social.media.3_15, RSA_RSA1$Exp_Subject_Id, get_word2)

# How (active/passive) - yes, 1st self reported SNS is (contrary to the expectation) the name of the variable...
RSA_RSA_P_ID$SELF_REP3_AP <- tapply(RSA_RSA1$X3nd.self.reported.SNS,RSA_RSA1$Exp_Subject_Id, mean, na.rm=T)
RSA_RSA_P_ID$SELF_REP3_AP[RSA_RSA_P_ID$SELF_REP3_AP=="NaN"]<-NA
# Weverse is OK...

# Recalculating: Now the higher score the less she uses it, maximum is 4: 
RSA_RSA_P_ID$Facebook_AP <- 5 - RSA_RSA_P_ID$Facebook_AP
RSA_RSA_P_ID$Instagram_AP <- 5 - RSA_RSA_P_ID$Instagram_AP

RSA_RSA_P_ID$TikTok_AP <- 5 - RSA_RSA_P_ID$TikTok_AP
RSA_RSA_P_ID$WeChat_AP <- 5 - RSA_RSA_P_ID$WeChat_AP

RSA_RSA_P_ID$YouTube_AP <- 5 - RSA_RSA_P_ID$YouTube_AP
RSA_RSA_P_ID$Twitter_AP <- 5 - RSA_RSA_P_ID$Twitter_AP

RSA_RSA_P_ID$SELF_REP1_AP <- 5 - RSA_RSA_P_ID$SELF_REP1_AP
RSA_RSA_P_ID$SELF_REP2_AP <- 5 - RSA_RSA_P_ID$SELF_REP2_AP
RSA_RSA_P_ID$SELF_REP3_AP <- 5 - RSA_RSA_P_ID$SELF_REP3_AP

# List of those I did not know before: 
# Locket - yes
RSA_RSA_P_ID[30,50] <- NA #
RSA_RSA_P_ID[30,51] <- NA #


# Score 1: A/P punished: 
# max(vector_APs) + 0.75(^1)*max(vector_APs[vector_APs!=max()]) + 0.75(^2)*...

# Put these variables in a vector by rows:
ActPASS<-matrix(NA, nrow=9,ncol=47)
colnames(ActPASS) <- (RSA_RSA_P_ID[,1])
AP_Scores <- as.vector(rep(NA,47))

for (i in 1:nrow(RSA_RSA_P_ID)) {
  ActPASS[,i]<-unlist(RSA_RSA_P_ID[i,c(40:45, 47, 49, 51)])
  ActPASS[,i]<-ifelse(is.na(ActPASS[,i]),0,ActPASS[,i])
}

# Define the punishment factor and the length of the vector to consider
punishment_factor <- 0.75
vect_length <- nrow(ActPASS) 

# Create a vector of the punishment multipliers
multipliers <- punishment_factor^(0:(vect_length-1))

# Calculate scores using vectorized operations: Check whether it takes all the highest possible scores (If there is three times 6,
# does it count each?) 
AP_Scores <- apply(ActPASS, 2, function(column) {
  sorted_column <- sort(column, decreasing = TRUE)
  sum(sorted_column[1:vect_length] * multipliers)
})

RSA_RSA_P_ID$AP_Scores <- AP_Scores


# Get coordinates of "horizontal vectors of medals": 
medals <- c(4,2,1,0)

# Calculate scores using vectorized operations: Check whether it takes all the highest possible scores (If there is three times 6,
# does it count each?) 

Medal_matrix <- apply(ActPASS, c(1, 2), apply_medal)

Medal_sums <- colSums(Medal_matrix)

RSA_RSA_P_ID$ActPAS_medals <- Medal_sums


#-------------------------------------------------------------------------------
# 5.4 Frequency of using of SPECIFIC social media (i.e., one by one):
#-------------------------------------------------------------------------------

RSA_RSA_P_ID$FreqSPEC<- "FreqSPEC"

# How_frequently_preselected: 
# 1 = I don't use it (Síť nepoužívám)
# 2 = Once a week or less (Jednou týdně či méně)
# 3 = Once a week, not daily (Víckrát týdně, ale ne denně)
# 4 = Once a day (Jednou za den)
# 5 = Several times a day (Několikrát denně)
# 6 = Frequently during the day (Často během dne) 

# The higher score the better - no need to recalculate anything

# Facebook = How_frequently_Facebook
RSA_RSA_P_ID$Facebook_Freq <- tapply(RSA_RSA1$How_frequently_Facebook,RSA_RSA1$Exp_Subject_Id, mean, na.rm=T)
RSA_RSA_P_ID$Facebook_Freq[RSA_RSA_P_ID$Facebook_Freq=="NaN"]<-NA

# YouTube = How_frequently_YouTube
RSA_RSA_P_ID$YouTube_Freq <- tapply(RSA_RSA1$How_frequently_YouTube,RSA_RSA1$Exp_Subject_Id, mean, na.rm=T)
RSA_RSA_P_ID$YouTube_Freq[RSA_RSA_P_ID$YouTube_Freq=="NaN"]<-NA

# Instagram = How_frequently_Instagram
RSA_RSA_P_ID$Instagram_Freq <- tapply(RSA_RSA1$How_frequently_Instagram,RSA_RSA1$Exp_Subject_Id, mean, na.rm=T)
RSA_RSA_P_ID$Instagram_Freq[RSA_RSA_P_ID$Instagram_Freq=="NaN"]<-NA

# TikTok = How_frequently_TikTok
RSA_RSA_P_ID$TikTok_Freq <- tapply(RSA_RSA1$How_frequently_TikTok,RSA_RSA1$Exp_Subject_Id, mean, na.rm=T)
RSA_RSA_P_ID$TikTok_Freq[RSA_RSA_P_ID$TikTok_Freq=="NaN"]<-NA


# X[Twitter] = How_frequently_X
RSA_RSA_P_ID$X_Freq <- tapply(RSA_RSA1$How_frequently_X,RSA_RSA1$Exp_Subject_Id, mean, na.rm=T)
RSA_RSA_P_ID$X_Freq[RSA_RSA_P_ID$X_Freq=="NaN"]<-NA

# WeChat = How_Frequently_WECHAT
RSA_RSA_P_ID$WeChat_Freq <- tapply(RSA_RSA1$How_Frequently_WECHAT_46,RSA_RSA1$Exp_Subject_Id, mean, na.rm=T)
RSA_RSA_P_ID$WeChat_Freq[RSA_RSA_P_ID$WeChat_Freq=="NaN"]<-NA


# Self reported SNS: 
# !! ALERT: The name "TIME" here does NOT refer to time from the beginning until the hit button
# Time = frequency...

# NAME_1: TIME_Input social media_1
RSA_RSA1$SR_TIME_Input.social.media_1 <- ifelse(RSA_RSA1$SR_TIME_Input.social.media_1=="",NA,RSA_RSA1$SR_TIME_Input.social.media_1)
RSA_RSA_P_ID$Time_SELFREP_Name1 <- tapply(RSA_RSA1$SR_TIME_Input.social.media_1, RSA_RSA1$Exp_Subject_Id, get_word2)

# Frequency_1: TIME_self reported SNS
RSA_RSA_P_ID$Time_SELFREP_Freq1 <- tapply(RSA_RSA1$SR_TIME_self.reported.SNS,RSA_RSA1$Exp_Subject_Id, mean, na.rm=T)
RSA_RSA_P_ID$Time_SELFREP_Freq1[RSA_RSA_P_ID$Time_SELFREP_Freq1=="NaN"]<-NA

# NAME_2: TIME_Input social media_2
RSA_RSA1$SR_TIME_Input.social.media.2 <- ifelse(RSA_RSA1$SR_TIME_Input.social.media.2=="",NA,RSA_RSA1$SR_TIME_Input.social.media.2)
RSA_RSA_P_ID$Time_SELFREP_Name2 <- tapply(RSA_RSA1$SR_TIME_Input.social.media.2, RSA_RSA1$Exp_Subject_Id, get_word2)


# Frequency_2: TIME_2nd self reported SNS
RSA_RSA_P_ID$Time_SELFREP_Freq2 <- tapply(RSA_RSA1$SR_TIME_2nd.self.reported.SNS,RSA_RSA1$Exp_Subject_Id, mean, na.rm=T)
RSA_RSA_P_ID$Time_SELFREP_Freq2[RSA_RSA_P_ID$Time_SELFREP_Freq2=="NaN"]<-NA
RSA_RSA_P_ID$Time_SELFREP_Freq2 <- ifelse(is.na(RSA_RSA_P_ID$Time_SELFREP_Name2), NA, RSA_RSA_P_ID$Time_SELFREP_Freq2 )


# NAME_3: TIME_Input social media_3
RSA_RSA1$SR_TIME_Input.social.media.3 <- ifelse(RSA_RSA1$SR_TIME_Input.social.media.3=="",NA,RSA_RSA1$SR_TIME_Input.social.media.3)
RSA_RSA_P_ID$Time_SELFREP_Name3 <- tapply(RSA_RSA1$SR_TIME_Input.social.media.3, RSA_RSA1$Exp_Subject_Id, get_word2)

# Frequency_3: TIME_3nd self reported SNS
RSA_RSA_P_ID$Time_SELFREP_Freq3 <- tapply(RSA_RSA1$SR_TIME_3nd.self.reported.SNS,RSA_RSA1$Exp_Subject_Id, mean, na.rm=T)
RSA_RSA_P_ID$Time_SELFREP_Freq3[RSA_RSA_P_ID$Time_SELFREP_Freq3=="NaN"]<-NA


RSA_RSA_P_ID[30,65] <- NA #
RSA_RSA_P_ID[30,66] <- NA #



#------------------

########
#      #
# (C)  # Specific Intensity Use -> Punished Score, one of the six (nine) social media 
#      #
########

# Frequency scores: 
# Score 1: Freq - punished: 

# Put these variables in a vector by rows:
FreqSC<-matrix(NA, nrow=9,ncol=47)
colnames(FreqSC) <- (RSA_RSA_P_ID[,1])
Freq_Scores <- as.vector(rep(NA,47)) 

for (i in 1:nrow(RSA_RSA_P_ID)) {
  FreqSC[,i]<-unlist(RSA_RSA_P_ID[i,c(55:60,62,64,66)])
  FreqSC[,i]<-ifelse(is.na(FreqSC[,i]),0,FreqSC[,i])
}

# Define the punishment factor and the length of the vector to consider
punishment_factor <- 0.75
vect_length <- nrow(FreqSC) 

# Create a vector of the punishment multipliers
multipliers <- punishment_factor^(0:(vect_length-1));multipliers

# Calculate scores using vectorized operations
Freq_Scores <- apply(FreqSC, 2, function(column) {
  sorted_column <- sort(column, decreasing = TRUE)
  sum(sorted_column[1:vect_length] * multipliers)
})
Freq_Scores
hist(Freq_Scores) # Normally distributed, except those who are, in fact, NAs

RSA_RSA_P_ID$Freq_Scores <- Freq_Scores
RSA_RSA_P_ID$Freq_Scores_SUM <- rowSums(RSA_RSA_P_ID[,c(55:60,62,64,66)], na.rm=T)



# Other available scores: 
# Frequency of use: General (median score from the three variables) 
RSA_RSA_P_ID$SMUi_MEAN
hist(RSA_RSA_P_ID$SMUi_MEAN, breaks=25)

# Frequency of use: SPECIFIC - the higher the more intensive & on more SMs
RSA_RSA_P_ID$Freq_Scores
hist(RSA_RSA_P_ID$Freq_Scores, breaks=25)

# Actvie/Passive: the higher the more active & more individual SM
RSA_RSA_P_ID$AP_Scores
hist(RSA_RSA_P_ID$AP_Scores, breaks=25)

# Negative use - the higher the worse: 
RSA_RSA_P_ID$Score_negative_use_SUM
hist(RSA_RSA_P_ID$Score_negative_use_SUM, breaks=25)
hist(RSA_RSA_P_ID$Score_negative_use_Mean, breaks=25)

hist(RSA_RSA_P_ID$Freq_Scores_SUM,breaks=25)


# SMU-i - based only on "Visual" social media: TikTok, YouTube, Instagram
RSA_RSA_P_ID$VisualFreqSUM <- rowSums(RSA_RSA_P_ID[,c(56:58)])


########
#      #
# (A)  # Visual International Social Media Use -> Punished Score, one of the three Key Coefficients 
#      #
########

# Punished Score: 

# Put these variables in a vector by rows:
FreqSC<-matrix(NA, nrow=3,ncol=47)
colnames(FreqSC) <- (RSA_RSA_P_ID[,1])
Freq_Scores <- as.vector(rep(NA,47))

for (i in 1:nrow(RSA_RSA_P_ID)) {
  FreqSC[,i]<-unlist(RSA_RSA_P_ID[i,c(56:58)])
  FreqSC[,i]<-ifelse(is.na(FreqSC[,i]),0,FreqSC[,i])
}


# Define the punishment factor and the length of the vector to consider
punishment_factor <- 0.75
vect_length <- nrow(FreqSC) 

# Create a vector of the punishment multipliers
multipliers <- punishment_factor^(0:(vect_length-1))

# Calculate scores using vectorized operations
Freq_Scores <- apply(FreqSC, 2, function(column) {
  sorted_column <- sort(column, decreasing = TRUE)
  sum(sorted_column[1:vect_length] * multipliers)
})

RSA_RSA_P_ID$VisualFreq_Punish <- Freq_Scores


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# How related are these scales...
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Create a new data frame -> Add NAs instead of zeros (null score was not possible but when no response was provided):
RSA_MUi <- data.frame(SMUi_MEAN=RSA_RSA_P_ID$SMUi_MEAN,
                     Freq_Scores=RSA_RSA_P_ID$Freq_Scores,
                     Freq_Scores_SUM=RSA_RSA_P_ID$Freq_Scores_SUM,
                     AP_Scores=RSA_RSA_P_ID$AP_Scores,
                     Score_negative_use_SUM=as.numeric(RSA_RSA_P_ID$Score_negative_use_SUM),
                     Score_negative_use_Mean=as.numeric(RSA_RSA_P_ID$Score_negative_use_Mean),
                     VisualFreq_Punish=as.numeric(RSA_RSA_P_ID$VisualFreq_Punish),
                     VisualFreqSUM=as.numeric(RSA_RSA_P_ID$VisualFreqSUM))


RSA_MUi$SMUi_MEAN[RSA_MUi$SMUi_MEAN==0]<-NA
RSA_MUi$Freq_Scores[RSA_MUi$Freq_Scores==0]<-NA
RSA_MUi$AP_Scores[RSA_MUi$AP_Scores==0]<-NA
RSA_MUi$VisualFreq_Punish[RSA_MUi$VisualFreq_Punish==0]<-NA
RSA_MUi$Freq_Scores_SUM[RSA_MUi$Freq_Scores_SUM==0]<-NA

RSA_MUi <- RSA_MUi[!is.na(RSA_MUi$SMUi_MEAN),] # It looks like those to start filling in these scores actually filled in the whole 

rcorr(as.matrix(RSA_MUi))


#-------------------------------------------------------------------------------
# 5.6. Preparation for the median splits (low/high intensity users)
#-------------------------------------------------------------------------------

# Which variables interests me at this point: 

# [1] A scale to assess the SMU-i: The candidate variables: 
RSA_RSA_P_ID$Score_negative_use_SUM # Score - negative use
RSA_RSA_P_ID$SMUi_MEAN # Scale Based on Boer et al. 2022

RSA_RSA_P_ID$AP_Scores # Score - Active Passive (Punished Score)
RSA_RSA_P_ID$ActPAS_medals # Score - Active Passive (Medals)

RSA_RSA_P_ID$Freq_Scores # Frequency of use of specific media - punished score
RSA_RSA_P_ID$Freq_Scores_SUM # Frequency score of specific media - sum

RSA_RSA_P_ID$VisualFreqSUM # Frequency of use visual media - sum
RSA_RSA_P_ID$VisualFreq_Punish # Frequency of use visual media - Punished Score


RSA_RSA1$Use_SNS_1 <- ifelse(RSA_RSA1$Use_SNS_1=="",NA,RSA_RSA1$Use_SNS_1)
RSA_RSA_P_ID$Social_Yes_No <- tapply(RSA_RSA1$Use_SNS_1, RSA_RSA1$Exp_Subject_Id, get_word2)
summary(as.factor(RSA_RSA_P_ID$Social_Yes_No)) # All use social media (at least somehow)


# Passed all tests (Score, AttentionCH1-CH3): 
RSA_RSA_IP_good <- RSA_RSA_P_ID[RSA_RSA_P_ID$TestInterpr=="Good"|RSA_RSA_P_ID$TestInterpr=="Borderline",]
RSA_RSA_IP_good <- RSA_RSA_IP_good[!is.na(RSA_RSA_IP_good$ATCH2),]


# Now - those who lacks score cannot be split: 
RSA_RSA_IP_good <- RSA_RSA_IP_good[!is.na(RSA_RSA_IP_good$SMUi_MEAN),]

# Adding the rest of the variables for a rater (SMU-i, etc.):
RSA_RSA_IP_good$Score_negative_use_Mean # Score - negative use
RSA_RSA_IP_good$SMUi_MEAN # Scale Based on Boer et al. 2022

RSA_RSA_IP_good$AP_Scores # Score - Active Passive (Punished Score)
RSA_RSA_IP_good$ActPAS_medals # Score - Active Passive (Medals)

RSA_RSA_IP_good$Freq_Scores # Frequency of use of specific media - punished score
RSA_RSA_IP_good$Freq_Scores_SUM # Frequency score of specific media - sum

RSA_RSA_IP_good$VisualFreqSUM # Frequency of use visual media - sum
RSA_RSA_IP_good$VisualFreq_Punish # Frequency of use visual media - Punished Score


# No NAs or zeros, let's split (a-b-c): 
SMUi_Mean <- median(RSA_RSA_IP_good$SMUi_MEAN) # YES (b)

Freq_Scores_median <- median(RSA_RSA_IP_good$Freq_Scores) # YES (c)

VisualFreq_Punish_median <- median(RSA_RSA_IP_good$VisualFreq_Punish) # YES (a)



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 5.7. Median splits: Simplified - for the full-size version see some previous versions of this script...
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# How it performs in splitting: 0<,0.5==,>1

#.-.-.-
# 1 Scale Based on Boer et al. 2022 - SCORE (b)
RSA_RSA_IP_good$SMUi_MEAN_Mean_SPLIT <- ifelse(RSA_RSA_IP_good$SMUi_MEAN<SMUi_Mean,0,
                                             ifelse(RSA_RSA_IP_good$SMUi_MEAN==SMUi_Mean,0.5,1))
summary(as.factor(RSA_RSA_IP_good$SMUi_MEAN_Mean_SPLIT))


#.-.-.-
# 2 Frequency of use of media - per given media (Facebook, Instagram, Twitter, WeChat, TikTok, YouTube + self-reported) - punished score (c)
RSA_RSA_IP_good$Freq_Scores_SPLIT <- ifelse(RSA_RSA_IP_good$Freq_Scores<Freq_Scores_median,0,
                                          ifelse(RSA_RSA_IP_good$Freq_Scores==Freq_Scores_median,0.5,1))
summary(as.factor(RSA_RSA_IP_good$Freq_Scores_SPLIT))


#.-.-.-
# 3 Frequency of use visual media - Punished Score (YouTube, TikTok, Instagram) - ONE OF THE FOCUS SCORES
RSA_RSA_IP_good$VisualFreq_Punish_median_SPLIT <- ifelse(RSA_RSA_IP_good$VisualFreq_Punish<VisualFreq_Punish_median,0,
                                                       ifelse(RSA_RSA_IP_good$VisualFreq_Punish==VisualFreq_Punish_median,0.5,1))
summary(as.factor(RSA_RSA_IP_good$VisualFreq_Punish_median_SPLIT))


# How does splitting according to different categories work: 
hist(rowSums(RSA_RSA_IP_good[,72:74]),breaks=12) # Brute force kinda works...
# Now - I am NOT interested in every variable of those... for the sake of visual diet. 



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 5.8. # Alternative attitude - excluding 25 % in the middle: 
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

RSA_RSA_IP_good$SMUi_MEAN
RSA_RSA_IP_good$Freq_Scores
RSA_RSA_IP_good$VisualFreq_Punish

RSA_RSA_IP_good$SMUi_Mean_25_percent <- ifelse(RSA_RSA_IP_good$SMUi_MEAN<=quantile(RSA_RSA_IP_good$SMUi_MEAN,0.375),"sides",
                                             ifelse(RSA_RSA_IP_good$SMUi_MEAN>=quantile(RSA_RSA_IP_good$SMUi_MEAN,0.625),"sides","centre"))
summary(as.factor(RSA_RSA_IP_good$SMUi_Mean_25_percent))


RSA_RSA_IP_good$Freq_Scores_25_percent <- ifelse(RSA_RSA_IP_good$Freq_Scores<=quantile(RSA_RSA_IP_good$Freq_Scores,0.375),"sides",
                                               ifelse(RSA_RSA_IP_good$Freq_Scores>=quantile(RSA_RSA_IP_good$Freq_Scores,0.625),"sides","centre"))
summary(as.factor(RSA_RSA_IP_good$Freq_Scores_25_percent))


RSA_RSA_IP_good$VisualFreqPunish_25_percent <- ifelse(RSA_RSA_IP_good$VisualFreq_Punish<quantile(RSA_RSA_IP_good$VisualFreq_Punish,0.375),"sides",
                                                    ifelse(RSA_RSA_IP_good$VisualFreq_Punish>quantile(RSA_RSA_IP_good$VisualFreq_Punish,0.625),"sides","centre"))
summary(as.factor(RSA_RSA_IP_good$VisualFreqPunish_25_percent)) # There is nothing in between 37.5 % and 62.5 % of the distribution.



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 5.9. Splitting (Above, Mediocre, Below)
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Split based on "A" (=VisualFreq_Punish)
#                    if not possible, split based on "B" (=SMUi_MEAN)
#                            if not possible, split based on "C" (=Freq_Scores)
# Otherwise "Mediocre"
RSA_RSA_IP_good$VisualFreq_Punish_median_SPLIT
RSA_RSA_IP_good$Split <- ifelse(RSA_RSA_IP_good$VisualFreq_Punish_median_SPLIT==1,"Above",
                              ifelse(RSA_RSA_IP_good$VisualFreq_Punish_median_SPLIT==0,"Below","Mediocre"))

# Step 2: For rows where Split is still "Mediocre", check the "SMUi_MEAN" variable
mediocre_indices <- which(RSA_RSA_IP_good$Split == "Mediocre")
RSA_RSA_IP_good$Split[mediocre_indices] <- ifelse(RSA_RSA_IP_good$SMUi_MEAN_Mean_SPLIT[mediocre_indices] == 1, "Above",
                                                ifelse(RSA_RSA_IP_good$SMUi_MEAN_Mean_SPLIT[mediocre_indices] == 0, "Below", "Mediocre"))

summary(as.factor(RSA_RSA_IP_good$Split))



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 6.0 # Adding the ratings.
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# (1) Cut the data.frame based on rater's ID: 
# (2) Get the ratings - if she left some faces not-rated, add NAs (mind the specific length of each sample: 
#     2.1. CZ 2016: 50M + 50F
#     2.2. CZ 2019: 39M + 56F

# Faces: 
RSA_RSA1$ATR_16F
RSA_RSA1$ATR_16M
RSA_RSA1$ATR_19F
RSA_RSA1$ATR_19M

# How to (re)order it: Spot the variable "Trial ID" and block name:  
# And since you got the IDs (of both photos, raters, and blocks), you can get rid of NAs:
names(RSA_RSA1[,c(2,6,22:25,36:39,99:102,117)])
RSARAT_List_FACES <- as.data.frame(RSA_RSA1[,c(2,6,22:25,36:39,99:102,117)]) # Only rating variables + IDs (all of them)

# Adding the order in the table
RSARAT_List_FACES$OrOd <- seq(from=1, to=nrow(RSARAT_List_FACES), by=1)

# Only rows that contain ratings are of interest: 
levels(as.factor(RSARAT_List_FACES$Task_Name))
selected_levs <- c("CZ_16F_ATD","CZ_16M_ATD","CZ_19F_ATD","CZ_19M_ATD")  

RSARAT_List_FACES <- RSARAT_List_FACES[RSARAT_List_FACES$Task_Name %in% selected_levs,]
RSARAT_List_FACES$FullID <- paste(RSARAT_List_FACES$Trial_Id,RSARAT_List_FACES$Task_Name)


# Match the two tables - the one with IDs and the one with ratings: 
RSARAT_List_FACES <- merge(RSARAT_List_FACES, real_IDs, by = "FullID", all = TRUE)

RSARAT_List_FACES <- RSARAT_List_FACES[order(RSARAT_List_FACES$OrOd),]

names(RSARAT_List_FACES)[names(RSARAT_List_FACES) == 'Exp_Subject_Id_117'] <- "RSA_RSA_P_ID"

# These are then the (only) raters you are interested in: 
# Subset the table: CZ_CZ1 - take only those "selected" raters
# Put the attractiveness, trustworthiness, and dominance, that are now split into 
# four columns each into just three columns (one for trustw., one for attractiveness, one for dominance):

RSARAT_List_FACES <- RSARAT_List_FACES[RSARAT_List_FACES$Task_Name %in% selected_levs,]

RSARAT_List_FACES <- merge(RSARAT_List_FACES, RSA_RSA_IP_good, by = "RSA_RSA_P_ID", all = F)
RSARAT_List_FACES <- RSARAT_List_FACES[order(RSARAT_List_FACES$OrOd),]

#-------------------------------------------------------------------------------
# 6.2 Finalising, the first step is "there is always just one non-zero number in each quartet
#-------------------------------------------------------------------------------

RSARAT_List_FACES$ATR <- rowSums(RSARAT_List_FACES[,5:8], na.rm = T)
RSARAT_List_FACES$TRU <- rowSums(RSARAT_List_FACES[,13:16], na.rm = T)
RSARAT_List_FACES$DOM <- rowSums(RSARAT_List_FACES[,9:12], na.rm = T)

# The core of the model is like this: 
# I am interested in the effect Sample (country of origin) and SNS use intensity (expressed by whichever variable)
# as Fixed Effects have on rating scale / scales 

RSARAT_List_FACES$ATR
RSARAT_List_FACES$TRU
RSARAT_List_FACES$DOM

RSARAT_List_FACES$Sample <- "RSA"

# The simplest way: separate model for each scale of these...

RSA_USE_THIS_FOR_LONG_TABLE <- data.frame(Particip_ID = RSARAT_List_FACES[,1],
                                         Face_ID = RSARAT_List_FACES[,23],
                                         Above_Below = RSARAT_List_FACES[,100],
                                         Atr = RSARAT_List_FACES[,101],
                                         Tru = RSARAT_List_FACES[,102],
                                         Dom = RSARAT_List_FACES[,103],
                                         Sample = RSARAT_List_FACES[,104],
                                         CentreSide = paste(RSARAT_List_FACES$SMUi_Mean_25_percent,
                                                            RSARAT_List_FACES$Freq_Scores_25_percent,
                                                            RSARAT_List_FACES$VisualFreqPunish_25_percent),
                                         ScoreAbroad = RSARAT_List_FACES[,49],
                                         Fami_Back = RSARAT_List_FACES[,50]
)

save(RSA_USE_THIS_FOR_LONG_TABLE, file="RSA_USE_THIS_FOR_LONG_TABLE.Rdata")

# Specify the objects you want to keep
objects_to_keep <- c("CZ_USE_THIS_FOR_LONG_TABLE",
                     "VN_USE_THIS_FOR_LONG_TABLE", 
                     "RSA_USE_THIS_FOR_LONG_TABLE",
                     "CZ_CZ_IP_good",
                     "VN_VN_IP_good",
                     "RSA_RSA_IP_good",
                     "apply_medal", "get_word2", "real_IDs")  # Replace with your object names

# Remove all other objects
rm(list = setdiff(ls(), objects_to_keep))



#-------------------------------------------------------------------------------
#  Australia / New Zealand 
#-------------------------------------------------------------------------------

AUS_AUS1 <- read.csv2("Face_CZ_PC_PROLIFIC_AUSTRALIA_NZ_Checked.csv", T, sep=,)
AUS_AUS2 <- read.csv2("Face_CZ_PC_PROLIFIC_AUSTRALIA_NZ_Checked_RUN_2.csv",T, sep=,) # ! There are two runs of data collection in Australia...

# Merge: 
AUS_AUS1 <- rbind.data.frame(AUS_AUS1, AUS_AUS2)


# NOTES: 
# The participant No. 902319 filled some portions of the questionnaire twice. His second, unifnished 
# particiaption had been deleted before the data was uploaded... 

# Geographic location?  
summary(as.factor(AUS_AUS1$SelectedLocation_130)) # CAJK

AUS_AUS_P_ID <- levels(as.factor(AUS_AUS1$Exp_Subject_Id_118)) # Participants IDs

# Get rid of those who did not proceed (as they were hired via Prolific I already know those who did not proceed 
# are actually useless): 
AUS_AUS_P_ID<-as.data.frame(AUS_AUS_P_ID)
AUS_AUS_P_ID$lengths <- tapply(AUS_AUS1$Rec_Session_Id_1, AUS_AUS1$Exp_Subject_Id_118, length)
hist(AUS_AUS_P_ID$lengths) # Two did not finish - the 966214 and 976615.

AUS_AUS1 <- AUS_AUS1[!AUS_AUS1$Exp_Subject_Id_118 %in% c("966214", "976615"), ]
AUS_AUS_P_ID <- AUS_AUS_P_ID[!AUS_AUS_P_ID$AUS_AUS_P_ID %in% c("966214", "976615"), ]



#-------------------------------------------------------------------------------
# 1 Basic demographics 
#-------------------------------------------------------------------------------

# Participant's demographic variables...
# All these variables either are naturally numeric (age...), or were coded as numeric (device)
# AGE: 
AUS_AUS_P_ID$Age <- tapply(AUS_AUS1$SELF_age_1_62,AUS_AUS1$Exp_Subject_Id_118,mean, na.rm=T)
AUS_AUS_P_ID$Age[AUS_AUS_P_ID$Age=="NaN"]<-NA
AUS_AUS_P_ID$Age # They look old!
mean(AUS_AUS_P_ID$Age) # Not too bad (I mean, this is OK)

# HEIGHT: 
AUS_AUS_P_ID$Height <- tapply(as.numeric(AUS_AUS1$b_height_F1_28),AUS_AUS1$Exp_Subject_Id_118,mean, na.rm=T)
AUS_AUS_P_ID$Height[AUS_AUS_P_ID$Height=="NaN"]<-NA
AUS_AUS_P_ID$Height

# WEIGHT: 
AUS_AUS_P_ID$weight <- tapply(as.numeric(AUS_AUS1$b_weight_F1_29),AUS_AUS1$Exp_Subject_Id_118,mean, na.rm=T)
AUS_AUS_P_ID$weight[AUS_AUS_P_ID$weight=="NaN"]<-NA
AUS_AUS_P_ID$weight

# MARITAL STATUS: 
AUS_AUS_P_ID$MariStat <- tapply(AUS_AUS1$Marital_status_F1_52,AUS_AUS1$Exp_Subject_Id_118,mean, na.rm=T)
AUS_AUS_P_ID$MariStat[AUS_AUS_P_ID$MariStat=="NaN"]<-NA
AUS_AUS_P_ID$MariStat


# Decoded to words: 
AUS_AUS_P_ID$MariStat_VERB <- ifelse(AUS_AUS_P_ID$MariStat==1,"Single",
                                     ifelse(AUS_AUS_P_ID$MariStat==2,"In_a_relationship",
                                            ifelse(AUS_AUS_P_ID$MariStat==3, "Married",
                                                   ifelse(AUS_AUS_P_ID$MariStat==4, "Divorced",
                                                          ifelse(AUS_AUS_P_ID$MariStat==5, "Widowed",
                                                                 ifelse(AUS_AUS_P_ID$MariStat==6, "DontWaDeclare",NA))))))

AUS_AUS_P_ID$MariStat_VERB

# MOTHER TONGUE:
AUS_AUS1$Mother_Tongue_1_53 <- ifelse(AUS_AUS1$Mother_Tongue_1_53=="",NA,AUS_AUS1$Mother_Tongue_1_53)
AUS_AUS_P_ID$MotherTongue <- tapply(AUS_AUS1$Mother_Tongue_1_53, AUS_AUS1$Exp_Subject_Id_118, get_word2)
AUS_AUS_P_ID$MotherTongue
summary(as.factor(AUS_AUS_P_ID$MotherTongue))
AUS_AUS_P_ID$MotherTongue[AUS_AUS_P_ID$MotherTongue=="english" | 
                            AUS_AUS_P_ID$MotherTongue=="English " |
                            AUS_AUS_P_ID$MotherTongue=="english "] <- "English"
summary(as.factor(AUS_AUS_P_ID$MotherTongue))

# There were two participants who report being from Czech Republic - probably they were asked to pilot the study, but openned it 
# way to late - these were exluced (see above) and are no more here...


# PASSPORT NATIONALITY
AUS_AUS1$Passport.nationality_59 <- ifelse(AUS_AUS1$Passport.nationality_59=="",NA,AUS_AUS1$Passport.nationality_59)
AUS_AUS_P_ID$PasspNat <- tapply(AUS_AUS1$Passport.nationality_59, AUS_AUS1$Exp_Subject_Id_118, get_word2)

# Ethnicity (definition)
AUS_AUS1$Eth_understanding_40 <- ifelse(AUS_AUS1$Eth_understanding_40=="",NA,AUS_AUS1$Eth_understanding_40)
AUS_AUS_P_ID$EthUNDERSTD <- tapply(AUS_AUS1$Eth_understanding_40, AUS_AUS1$Exp_Subject_Id_118, get_word2)

# Ethnicity (self-reported)
AUS_AUS1$Self_reported_ethnicity_63 <- ifelse(AUS_AUS1$Self_reported_ethnicity_63=="",NA,AUS_AUS1$Self_reported_ethnicity_63)
AUS_AUS_P_ID$Eth_SelfREP <- tapply(AUS_AUS1$Self_reported_ethnicity_63, AUS_AUS1$Exp_Subject_Id_118, get_word2)

# Gender (self-reported):
AUS_AUS1$Gender.at.birth_1_42 <- ifelse(AUS_AUS1$Gender.at.birth_1_42=="",NA,AUS_AUS1$Gender.at.birth_1_42)
AUS_AUS_P_ID$SEXATB <- tapply(AUS_AUS1$Gender.at.birth_1_42, AUS_AUS1$Exp_Subject_Id_118, get_word2)



#-------------------------------------------------------------------------------
# 2 Technical variables
#-------------------------------------------------------------------------------

AUS_AUS_P_ID$Technical <- "TECHNICAL"
# Device auto-selected: 
AUS_AUS_P_ID$DEVICE_LABV <- tapply(AUS_AUS1$System_Spec_139, AUS_AUS1$Exp_Subject_Id_118, unique)

# Password for Entrance Examination:
AUS_AUS1$Participant_FEEDBACK_57 <- ifelse(AUS_AUS1$Participant_FEEDBACK_57=="",NA,AUS_AUS1$Participant_FEEDBACK_57)
AUS_AUS_P_ID$PASSW <- tapply(AUS_AUS1$Participant_FEEDBACK_57, AUS_AUS1$Exp_Subject_Id_118, unique)

# Window resolution: 
# WIDTH:
AUS_AUS_P_ID$Window_WWW <- tapply(AUS_AUS1$Window_Width_In_Pixels_143, AUS_AUS1$Exp_Subject_Id_118, unique)
# HEIGHT: 
AUS_AUS_P_ID$Window_HHH <- tapply(AUS_AUS1$Window_Height_In_Pixels_142, AUS_AUS1$Exp_Subject_Id_118, unique)

# Device screen resolution: 
# WIDTH:
AUS_AUS_P_ID$Screen_WWW <- tapply(AUS_AUS1$Screen_Width_In_Pixels_126, AUS_AUS1$Exp_Subject_Id_118, unique)
# HEIGHT: 
AUS_AUS_P_ID$Screen_HHH <- tapply(AUS_AUS1$Screen_Height_In_Pixels_124, AUS_AUS1$Exp_Subject_Id_118, unique)

# Selected location (where LabVanced thinks the person is located - may be tricky due to people travelling 
# a lot and VPNs...)
AUS_AUS_P_ID$SelectedLoc <- tapply(AUS_AUS1$SelectedLocation_130, AUS_AUS1$Exp_Subject_Id_118, unique)
summary(as.factor(AUS_AUS_P_ID$SelectedLoc))


#-------------------------------------------------------------------------------
# 3 Was the person conscious (attention checks and score in a test): 
#-------------------------------------------------------------------------------

AUS_AUS_P_ID$Conscious <- "CONSCIOUS"

# To get rid of a warning related to NA-only vectors, lets assign -1 to each body who did not bother to start the test:
AUS_AUS1$correct_31 <- ifelse(is.na(AUS_AUS1$correct_31),-1,AUS_AUS1$correct_31)

AUS_AUS_P_ID$TestScore <- tapply(as.numeric(AUS_AUS1$correct_31), AUS_AUS1$Exp_Subject_Id_118, max, na.rm=T)
AUS_AUS_P_ID$TestInterpr <- ifelse(AUS_AUS_P_ID$TestScore==5|AUS_AUS_P_ID$TestScore==4,"Good",ifelse(AUS_AUS_P_ID$TestScore==3,"Borderline","Exclude!"))
# Ól gut! 

summary(as.factor(AUS_AUS_P_ID$TestInterpr))

# Attention Checks (the kind of variable where there is one word for the vector [OR NA]: 
# 1 (Green/Zeleny)
AUS_AUS1$Attention_Check_Green_27 <- ifelse(AUS_AUS1$Attention_Check_Green_27=="",NA,AUS_AUS1$Attention_Check_Green_27)
AUS_AUS_P_ID$ATCH1 <- tapply(AUS_AUS1$Attention_Check_Green_27, AUS_AUS1$Exp_Subject_Id_118, get_word2)
summary(as.factor(AUS_AUS_P_ID$ATCH1 )) # All passed! 

# 2 (1968)
AUS_AUS1$Attention_Check_CCCP_26 <- ifelse(AUS_AUS1$Attention_Check_CCCP_26=="",NA,AUS_AUS1$Attention_Check_CCCP_26)
AUS_AUS_P_ID$ATCH2 <- tapply(AUS_AUS1$Attention_Check_CCCP_26, AUS_AUS1$Exp_Subject_Id_118, get_word2)
summary(as.factor(AUS_AUS_P_ID$ATCH2 )) # All passed!


#-------------------------------------------------------------------------------
# 4 Social background (outside social media): 
#-------------------------------------------------------------------------------

AUS_AUS_P_ID$SocBack <- "SocBack"

# Travel abroad: 
# 1 = Velmi často = Often
# 2 = Spíše často = Rather often
# 3 = Občas = Occasionally
# 4 = Zřídka = Rarely
# 5 = Velmi zřídka = Very rarely
# 6 = Nikdy = never
AUS_AUS1$Travel_Abroad_96 <- ifelse(AUS_AUS1$Travel_Abroad_96=="",NA,AUS_AUS1$Travel_Abroad_96)
AUS_AUS_P_ID$ScoreAbroad <- tapply(AUS_AUS1$Travel_Abroad_96, AUS_AUS1$Exp_Subject_Id_118, get_word2)
summary(as.factor(AUS_AUS_P_ID$ScoreAbroad))

# Family background: 
# 1 = "Bohaté materiální zabezpečení, peníze jsme nikdy neřešili" = Rich 
# 2 = "Dost peněz, i když jsme se někdy museli uskormnit" = Upper CL
# 3 = "Někdy více, někdy méně, ale dokázali jsme vyžít bez ztráty úrovně" = Middle
# 4 = "Spíše málo peněz, ve srovnání s vrstevníky jsem se musel často uskromnit" = Lower CL 
# 5 = "Neustálé finanční potíže" = Poor
# 6 = "Nepřeji si odpovídat" = No family (the last level is labelled incorrectly, will be corrected)
AUS_AUS1$Family_SOC_1_41 <- ifelse(AUS_AUS1$Family_SOC_1_41=="",NA,AUS_AUS1$Family_SOC_1_41)
AUS_AUS_P_ID$Fami_Back <- tapply(AUS_AUS1$Family_SOC_1_41, AUS_AUS1$Exp_Subject_Id_118, get_word2)
# Correcting the level with wrong label: 
AUS_AUS_P_ID$Fami_Back[AUS_AUS_P_ID$Fami_Back=="No family"] <- "PreferNR"
summary(as.factor(AUS_AUS_P_ID$Fami_Back))


#-------------------------------------------------------------------------------
# 5 Social media use... All variables  are coded as numbers...
# 5.1 SNS - general - (c) Boer & Eijinden (2018 2022)
#-------------------------------------------------------------------------------

# Are there some complete non-users? 
summary(as.factor(AUS_AUS1$Use_SNS_NO_105)) # YES! This rater must be added as "low" manually, I guess. 

# How many times a day - check the content: "HowOftenPassive_1_50"
# 1 = "Nikdy nebou méně než jednou denně"
# 2 = "1-2krát denně"
# 3 = "3-5krát denně"
# 4 = "6-10krát denně"
# 5 = "11-20krát denně"
# 6 = "21-40krát denně"
# 7 = "Více než 40krát denně"
AUS_AUS1$HowOftenPassive_1_50 <- ifelse(AUS_AUS1$HowOftenPassive_1_50=="",NA,AUS_AUS1$HowOftenPassive_1_50)
AUS_AUS_P_ID$SMUi_Q1 <- tapply(AUS_AUS1$HowOftenPassive_1_50, AUS_AUS1$Exp_Subject_Id_118, get_word2)

# How many times a week - posting: "HowOftenPosting_1_51"
# 1 = "Nikdy nebou méně než jednou týdně"
# 2 = "1-2krát týdně"
# 3 = "3-5krát týdně"
# 4 = "6-10krát týdně"
# 5 = "11-20krát týdně"
# 6 = "21-40krát týdně"
# 7 = "Více než 40krát týdně"
AUS_AUS1$HowOftenPosting_1_51 <- ifelse(AUS_AUS1$HowOftenPosting_1_51=="",NA,AUS_AUS1$HowOftenPosting_1_51)
AUS_AUS_P_ID$SMUi_Q2 <- tapply(AUS_AUS1$HowOftenPosting_1_51, AUS_AUS1$Exp_Subject_Id_118, get_word2)

# How many times a week - likes posts of the others: "HowOftenLiking_1_49"
# 1 = "Nikdy nebou méně než jednou týdně"
# 2 = "1-2krát týdně"
# 3 = "3-5krát týdně"
# 4 = "6-10krát týdně"
# 5 = "11-20krát týdně"
# 6 = "21-40krát týdně"
# 7 = "Více než 40krát týdně"
AUS_AUS1$HowOftenLiking_1_49 <- ifelse(AUS_AUS1$HowOftenLiking_1_49=="",NA,AUS_AUS1$HowOftenLiking_1_49)
AUS_AUS_P_ID$SMUi_Q3 <- tapply(AUS_AUS1$HowOftenLiking_1_49, AUS_AUS1$Exp_Subject_Id_118, get_word2)

########
#      #
# (B)  # Social Media Use Intensity -> Mean, based on BOER... 
#      #
########

# Will then compute the median value from the three variables' scores: 
AUS_AUS_P_ID$SMUi_MEAN <- apply(AUS_AUS_P_ID[,c(29:31)],1,mean)# The higher the more intensive user: 
AUS_AUS_P_ID$SMUi_MEAN[14] <- 0
AUS_AUS_P_ID$SMUi_MEAN[43] <- 0
# Do you know why it is not 30:32, but 29:31? Because U have one attention check less


#-------------------------------------------------------------------------------
# 5.2 Social media use - negative effects - FOMO, Too Much Time, Miss Others...
#-------------------------------------------------------------------------------

# 1 = zcela souhlasím (Totally agree)
# 2 = spíše souhlasím (rather agree)
# 3 = Ani souhlas ani nesouhlas (neither agree nor disagree)
# 4 = Spíše nesouhlasím (rather disagree)
# 5 = zcela nesouhlasím (Totally disagree)

# SNS_too_much_time:
AUS_AUS1$SNS_too_much_time_68
AUS_AUS_P_ID$SM_too_much_time <- tapply(AUS_AUS1$SNS_too_much_time_68, AUS_AUS1$Exp_Subject_Id_118, get_word2)

# SNS_sometimes_miss_other:
AUS_AUS1$SNS_Sometimes_miss_other_67
AUS_AUS_P_ID$Miss_other <- tapply(AUS_AUS1$SNS_Sometimes_miss_other_67, AUS_AUS1$Exp_Subject_Id_118, get_word2)

# SNS_know_better_than_friends:
AUS_AUS1$SNS_know_better_than_friends_66
AUS_AUS_P_ID$SM_friends_better <- tapply(AUS_AUS1$SNS_know_better_than_friends_66, AUS_AUS1$Exp_Subject_Id_118, get_word2)

# SMP_fear_of_missing_out:
AUS_AUS1$SMP_fear_of_missing_out_65
AUS_AUS_P_ID$FOMO_SM <- tapply(AUS_AUS1$SMP_fear_of_missing_out_65, AUS_AUS1$Exp_Subject_Id_118, get_word2)

# Recalculate so that the higher (up to five), the worse: 
View(data.frame(Orig=AUS_AUS_P_ID$SM_too_much_time,New=6 - AUS_AUS_P_ID$SM_too_much_time))
AUS_AUS_P_ID$SM_too_much_time <- 6 - AUS_AUS_P_ID$SM_too_much_time

AUS_AUS_P_ID$Miss_other <- 6 - AUS_AUS_P_ID$Miss_other

AUS_AUS_P_ID$SM_friends_better <- 6 - AUS_AUS_P_ID$SM_friends_better

AUS_AUS_P_ID$FOMO_SM <- 6 - AUS_AUS_P_ID$FOMO_SM

# The score will be like "The higher, the more intensive user": 
AUS_AUS_P_ID$Score_negative_use_SUM <- rowSums(AUS_AUS_P_ID[,33:36])
AUS_AUS_P_ID$Score_negative_use_Mean <- apply(AUS_AUS_P_ID[,33:36],1,mean)
AUS_AUS_P_ID$Score_negative_use_Mean[is.na(AUS_AUS_P_ID$Score_negative_use_Mean)]<-0


#-------------------------------------------------------------------------------
# 5.3 Which SNS - ACTIVELY, SEMI-ACTIVELY OR PASSIVELY
#-------------------------------------------------------------------------------

# AP - stands for ACTIVE/PASSIVE: 
AUS_AUS_P_ID$ACTPAS<- "ACTPAS"
AUS_AUS_P_ID$Facebook_AP <- tapply(AUS_AUS1$AP_Facebook,AUS_AUS1$Exp_Subject_Id, mean, na.rm=T)
AUS_AUS_P_ID$Facebook_AP[AUS_AUS_P_ID$Facebook_AP=="NaN"]<-NA
AUS_AUS_P_ID$Instagram_AP <- tapply(AUS_AUS1$AP_Instagram,AUS_AUS1$Exp_Subject_Id, mean, na.rm=T)
AUS_AUS_P_ID$Instagram_AP[AUS_AUS_P_ID$Instagram_AP=="NaN"]<-NA

AUS_AUS_P_ID$TikTok_AP <- tapply(AUS_AUS1$AP_TikTok_18,AUS_AUS1$Exp_Subject_Id, mean, na.rm=T)
AUS_AUS_P_ID$TikTok_AP[AUS_AUS_P_ID$TikTok_AP=="NaN"]<-NA
AUS_AUS_P_ID$WeChat_AP <- tapply(AUS_AUS1$AP_WeChat_19,AUS_AUS1$Exp_Subject_Id, mean, na.rm=T)
AUS_AUS_P_ID$WeChat_AP[AUS_AUS_P_ID$WeChat_AP=="NaN"]<-NA

AUS_AUS_P_ID$YouTube_AP <- tapply(AUS_AUS1$AP_YouTube,AUS_AUS1$Exp_Subject_Id, mean, na.rm=T)
AUS_AUS_P_ID$YouTube_AP[AUS_AUS_P_ID$YouTube_AP=="NaN"]<-NA
AUS_AUS_P_ID$Twitter_AP <- tapply(AUS_AUS1$AP_X..Twitter.,AUS_AUS1$Exp_Subject_Id, mean, na.rm=T)
AUS_AUS_P_ID$Twitter_AP[AUS_AUS_P_ID$Twitter_AP=="NaN"]<-NA


# A/P - self reported (if any...), should be 3 items...  
# First one - which one (NAME): 
AUS_AUS1$AP_Input.social.media_1 <- ifelse(AUS_AUS1$AP_Input.social.media_1=="",NA,AUS_AUS1$AP_Input.social.media_1)
AUS_AUS_P_ID$SELF_AP_NAME_1 <- tapply(AUS_AUS1$AP_Input.social.media_1, AUS_AUS1$Exp_Subject_Id, get_word2)

# How (active/passive) - yes, 1st self reported SNS is (contrary to the expectation) the name of the variable...
AUS_AUS_P_ID$SELF_REP1_AP <- tapply(AUS_AUS1$X1st.self.reported.SNS,AUS_AUS1$Exp_Subject_Id, mean, na.rm=T)
AUS_AUS_P_ID$SELF_REP1_AP[AUS_AUS_P_ID$SELF_REP1_AP=="NaN"]<-NA

# Second one - which one (NAME): 
AUS_AUS1$AP_Input.social.media.2 <- ifelse(AUS_AUS1$AP_Input.social.media.2=="",NA,AUS_AUS1$AP_Input.social.media.2)
AUS_AUS_P_ID$SELF_AP_NAME_2 <- tapply(AUS_AUS1$AP_Input.social.media.2, AUS_AUS1$Exp_Subject_Id, get_word2)

# How (active/passive) - yes, 1st self reported SNS is (contrary to the expectation) the name of the variable...
AUS_AUS_P_ID$SELF_REP2_AP <- tapply(AUS_AUS1$X2nd.self.reported.SNS,AUS_AUS1$Exp_Subject_Id, mean, na.rm=T)
AUS_AUS_P_ID$SELF_REP2_AP[AUS_AUS_P_ID$SELF_REP2_AP=="NaN"]<-NA

# Third one - which one (NAME): 
AUS_AUS1$AP_Input.social.media.3_15 <- ifelse(AUS_AUS1$AP_Input.social.media.3_15=="",NA,AUS_AUS1$AP_Input.social.media.3_15)
AUS_AUS_P_ID$SELF_AP_NAME_3 <- tapply(AUS_AUS1$AP_Input.social.media.3_15, AUS_AUS1$Exp_Subject_Id, get_word2)

# How (active/passive) - yes, 1st self reported SNS is (contrary to the expectation) the name of the variable...
AUS_AUS_P_ID$SELF_REP3_AP <- tapply(AUS_AUS1$X3nd.self.reported.SNS,AUS_AUS1$Exp_Subject_Id, mean, na.rm=T)
AUS_AUS_P_ID$SELF_REP3_AP[AUS_AUS_P_ID$SELF_REP3_AP=="NaN"]<-NA
# Pleroma is OK...
# Misskey is OK...

# Recalculating: Now the higher score the less she uses it, maximum is 4: 
AUS_AUS_P_ID$Facebook_AP <- 5 - AUS_AUS_P_ID$Facebook_AP
AUS_AUS_P_ID$Instagram_AP <- 5 - AUS_AUS_P_ID$Instagram_AP

AUS_AUS_P_ID$TikTok_AP <- 5 - AUS_AUS_P_ID$TikTok_AP
AUS_AUS_P_ID$WeChat_AP <- 5 - AUS_AUS_P_ID$WeChat_AP

AUS_AUS_P_ID$YouTube_AP <- 5 - AUS_AUS_P_ID$YouTube_AP
AUS_AUS_P_ID$Twitter_AP <- 5 - AUS_AUS_P_ID$Twitter_AP

AUS_AUS_P_ID$SELF_REP1_AP <- 5 - AUS_AUS_P_ID$SELF_REP1_AP
AUS_AUS_P_ID$SELF_REP2_AP <- 5 - AUS_AUS_P_ID$SELF_REP2_AP
AUS_AUS_P_ID$SELF_REP3_AP <- 5 - AUS_AUS_P_ID$SELF_REP3_AP



# Score 1: A/P punished: 
# max(vector_APs) + 0.75(^1)*max(vector_APs[vector_APs!=max()]) + 0.75(^2)*...

NN <- 53

# Put these variables in a vector by rows:
ActPASS<-matrix(NA, nrow=9,ncol=NN)
colnames(ActPASS) <- (AUS_AUS_P_ID[,1])
AP_Scores <- as.vector(rep(NA,NN))

for (i in 1:nrow(AUS_AUS_P_ID)) {
  ActPASS[,i]<-unlist(AUS_AUS_P_ID[i,c(40:45, 47, 49, 51)])
  ActPASS[,i]<-ifelse(is.na(ActPASS[,i]),0,ActPASS[,i])
}

# Define the punishment factor and the length of the vector to consider
punishment_factor <- 0.75
vect_length <- nrow(ActPASS) 

# Create a vector of the punishment multipliers
multipliers <- punishment_factor^(0:(vect_length-1))

# Calculate scores using vectorized operations: Check whether it takes all the highest possible scores (If there is three times 6,
# does it count each?) 
AP_Scores <- apply(ActPASS, 2, function(column) {
  sorted_column <- sort(column, decreasing = TRUE)
  sum(sorted_column[1:vect_length] * multipliers)
})

AUS_AUS_P_ID$AP_Scores <- AP_Scores


# Get coordinates of "horizontal vectors of medals": 
medals <- c(4,2,1,0)

# Calculate scores using vectorized operations: Check whether it takes all the highest possible scores (If there is three times 6,
# does it count each?) 

Medal_matrix <- apply(ActPASS, c(1, 2), apply_medal)

Medal_sums <- colSums(Medal_matrix)

AUS_AUS_P_ID$ActPAS_medals <- Medal_sums


#-------------------------------------------------------------------------------
# 5.4 Frequency of using of SPECIFIC social media (i.e., one by one):
#-------------------------------------------------------------------------------

AUS_AUS_P_ID$FreqSPEC<- "FreqSPEC"

# How_frequently_preselected: 
# 1 = I don't use it (Síť nepoužívám)
# 2 = Once a week or less (Jednou týdně či méně)
# 3 = Once a week, not daily (Víckrát týdně, ale ne denně)
# 4 = Once a day (Jednou za den)
# 5 = Several times a day (Několikrát denně)
# 6 = Frequently during the day (Často během dne) 

# The higher score the better - no need to recalculate anything

# Facebook = How_frequently_Facebook
AUS_AUS_P_ID$Facebook_Freq <- tapply(AUS_AUS1$How_frequently_Facebook,AUS_AUS1$Exp_Subject_Id, mean, na.rm=T)
AUS_AUS_P_ID$Facebook_Freq[AUS_AUS_P_ID$Facebook_Freq=="NaN"]<-NA

# YouTube = How_frequently_YouTube
AUS_AUS_P_ID$YouTube_Freq <- tapply(AUS_AUS1$How_frequently_YouTube,AUS_AUS1$Exp_Subject_Id, mean, na.rm=T)
AUS_AUS_P_ID$YouTube_Freq[AUS_AUS_P_ID$YouTube_Freq=="NaN"]<-NA

# Instagram = How_frequently_Instagram
AUS_AUS_P_ID$Instagram_Freq <- tapply(AUS_AUS1$How_frequently_Instagram,AUS_AUS1$Exp_Subject_Id, mean, na.rm=T)
AUS_AUS_P_ID$Instagram_Freq[AUS_AUS_P_ID$Instagram_Freq=="NaN"]<-NA

# TikTok = How_frequently_TikTok
AUS_AUS_P_ID$TikTok_Freq <- tapply(AUS_AUS1$How_frequently_TikTok,AUS_AUS1$Exp_Subject_Id, mean, na.rm=T)
AUS_AUS_P_ID$TikTok_Freq[AUS_AUS_P_ID$TikTok_Freq=="NaN"]<-NA


# X[Twitter] = How_frequently_X
AUS_AUS_P_ID$X_Freq <- tapply(AUS_AUS1$How_frequently_X,AUS_AUS1$Exp_Subject_Id, mean, na.rm=T)
AUS_AUS_P_ID$X_Freq[AUS_AUS_P_ID$X_Freq=="NaN"]<-NA

# WeChat = How_Frequently_WECHAT
AUS_AUS_P_ID$WeChat_Freq <- tapply(AUS_AUS1$How_Frequently_WECHAT_46,AUS_AUS1$Exp_Subject_Id, mean, na.rm=T)
AUS_AUS_P_ID$WeChat_Freq[AUS_AUS_P_ID$WeChat_Freq=="NaN"]<-NA


# Self reported SNS:  (Time = frequency...)

# NAME_1: TIME_Input social media_1
AUS_AUS1$SR_TIME_Input.social.media_1 <- ifelse(AUS_AUS1$SR_TIME_Input.social.media_1=="",NA,AUS_AUS1$SR_TIME_Input.social.media_1)
AUS_AUS_P_ID$Time_SELFREP_Name1 <- tapply(AUS_AUS1$SR_TIME_Input.social.media_1, AUS_AUS1$Exp_Subject_Id, get_word2)

# Frequency_1: TIME_self reported SNS
AUS_AUS_P_ID$Time_SELFREP_Freq1 <- tapply(AUS_AUS1$SR_TIME_self.reported.SNS,AUS_AUS1$Exp_Subject_Id, mean, na.rm=T)
AUS_AUS_P_ID$Time_SELFREP_Freq1[AUS_AUS_P_ID$Time_SELFREP_Freq1=="NaN"]<-NA


# NAME_2: TIME_Input social media_2
AUS_AUS1$SR_TIME_Input.social.media.2 <- ifelse(AUS_AUS1$SR_TIME_Input.social.media.2=="",NA,AUS_AUS1$SR_TIME_Input.social.media.2)
AUS_AUS_P_ID$Time_SELFREP_Name2 <- tapply(AUS_AUS1$SR_TIME_Input.social.media.2, AUS_AUS1$Exp_Subject_Id, get_word2)

# Frequency_2: TIME_2nd self reported SNS
AUS_AUS_P_ID$Time_SELFREP_Freq2 <- tapply(AUS_AUS1$SR_TIME_2nd.self.reported.SNS,AUS_AUS1$Exp_Subject_Id, mean, na.rm=T)
AUS_AUS_P_ID$Time_SELFREP_Freq2[AUS_AUS_P_ID$Time_SELFREP_Freq2=="NaN"]<-NA
AUS_AUS_P_ID$Time_SELFREP_Freq2 <- ifelse(is.na(AUS_AUS_P_ID$Time_SELFREP_Name2), NA, AUS_AUS_P_ID$Time_SELFREP_Freq2 )


# NAME_3: TIME_Input social media_3
AUS_AUS1$SR_TIME_Input.social.media.3 <- ifelse(AUS_AUS1$SR_TIME_Input.social.media.3=="",NA,AUS_AUS1$SR_TIME_Input.social.media.3)
AUS_AUS_P_ID$Time_SELFREP_Name3 <- tapply(AUS_AUS1$SR_TIME_Input.social.media.3, AUS_AUS1$Exp_Subject_Id, get_word2)

# Frequency_3: TIME_3nd self reported SNS
AUS_AUS_P_ID$Time_SELFREP_Freq3 <- tapply(AUS_AUS1$SR_TIME_3nd.self.reported.SNS,AUS_AUS1$Exp_Subject_Id, mean, na.rm=T)
AUS_AUS_P_ID$Time_SELFREP_Freq3[AUS_AUS_P_ID$Time_SELFREP_Freq3=="NaN"]<-NA



#------------------

########
#      #
# (C)  # Specific Intensity Use -> Punished Score, one of the six (nine) social media 
#      #
########

# Frequency scores: 
# Score 1: Freq - punished: 

# Put these variables in a vector by rows:
FreqSC<-matrix(NA, nrow=9,ncol=NN)
colnames(FreqSC) <- (AUS_AUS_P_ID[,1])
Freq_Scores <- as.vector(rep(NA,NN)) 

for (i in 1:nrow(AUS_AUS_P_ID)) {
  FreqSC[,i]<-unlist(AUS_AUS_P_ID[i,c(55:60,62,64,66)])
  FreqSC[,i]<-ifelse(is.na(FreqSC[,i]),0,FreqSC[,i])
}

# Define the punishment factor and the length of the vector to consider
punishment_factor <- 0.75
vect_length <- nrow(FreqSC) 

# Create a vector of the punishment multipliers
multipliers <- punishment_factor^(0:(vect_length-1));multipliers

# Calculate scores using vectorized operations
Freq_Scores <- apply(FreqSC, 2, function(column) {
  sorted_column <- sort(column, decreasing = TRUE)
  sum(sorted_column[1:vect_length] * multipliers)
})
Freq_Scores
hist(Freq_Scores) # Normally distributed, except those who are, in fact, NAs

AUS_AUS_P_ID$Freq_Scores <- Freq_Scores
AUS_AUS_P_ID$Freq_Scores_SUM <- rowSums(AUS_AUS_P_ID[,c(55:60,62,64,66)], na.rm=T)



# Other available scores: 
# Frequency of use: General (median score from the three variables) 
AUS_AUS_P_ID$SMUi_MEAN
hist(AUS_AUS_P_ID$SMUi_MEAN, breaks=25)

# Frequency of use: SPECIFIC - the higher the more intensive & on more SMs
AUS_AUS_P_ID$Freq_Scores
hist(AUS_AUS_P_ID$Freq_Scores, breaks=25)

# Actvie/Passive: the higher the more active & more individual SM
AUS_AUS_P_ID$AP_Scores
hist(AUS_AUS_P_ID$AP_Scores, breaks=25)

# Negative use - the higher the worse: 
AUS_AUS_P_ID$Score_negative_use_SUM
hist(AUS_AUS_P_ID$Score_negative_use_SUM, breaks=25)
hist(AUS_AUS_P_ID$Score_negative_use_Mean, breaks=25)

hist(AUS_AUS_P_ID$Freq_Scores_SUM,breaks=25)


# SMU-i - based only on "Visual" social media: TikTok, YouTube, Instagram
AUS_AUS_P_ID$VisualFreqSUM <- rowSums(AUS_AUS_P_ID[,c(56:58)])


########
#      #
# (A)  # Visual International Social Media Use -> Punished Score, one of the three Key Coefficients 
#      #
########

# Punished Score: 

# Put these variables in a vector by rows:
FreqSC<-matrix(NA, nrow=3,ncol=NN)
colnames(FreqSC) <- (AUS_AUS_P_ID[,1])
Freq_Scores <- as.vector(rep(NA,NN))

for (i in 1:nrow(AUS_AUS_P_ID)) {
  FreqSC[,i]<-unlist(AUS_AUS_P_ID[i,c(56:58)])
  FreqSC[,i]<-ifelse(is.na(FreqSC[,i]),0,FreqSC[,i])
}


# Define the punishment factor and the length of the vector to consider
punishment_factor <- 0.75
vect_length <- nrow(FreqSC) 

# Create a vector of the punishment multipliers
multipliers <- punishment_factor^(0:(vect_length-1))

# Calculate scores using vectorized operations
Freq_Scores <- apply(FreqSC, 2, function(column) {
  sorted_column <- sort(column, decreasing = TRUE)
  sum(sorted_column[1:vect_length] * multipliers)
})

AUS_AUS_P_ID$VisualFreq_Punish <- Freq_Scores


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# How related are these scales...
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Create a new data frame -> Add NAs instead of zeros (null score was not possible but when no response was provided):
AUS_MUi <- data.frame(SMUi_MEAN=AUS_AUS_P_ID$SMUi_MEAN,
                      Freq_Scores=AUS_AUS_P_ID$Freq_Scores,
                      Freq_Scores_SUM=AUS_AUS_P_ID$Freq_Scores_SUM,
                      AP_Scores=AUS_AUS_P_ID$AP_Scores,
                      Score_negative_use_SUM=as.numeric(AUS_AUS_P_ID$Score_negative_use_SUM),
                      Score_negative_use_Mean=as.numeric(AUS_AUS_P_ID$Score_negative_use_Mean),
                      VisualFreq_Punish=as.numeric(AUS_AUS_P_ID$VisualFreq_Punish),
                      VisualFreqSUM=as.numeric(AUS_AUS_P_ID$VisualFreqSUM))


AUS_MUi$SMUi_MEAN[AUS_MUi$SMUi_MEAN==0]<-NA
AUS_MUi$Freq_Scores[AUS_MUi$Freq_Scores==0]<-NA
AUS_MUi$AP_Scores[AUS_MUi$AP_Scores==0]<-NA
AUS_MUi$VisualFreq_Punish[AUS_MUi$VisualFreq_Punish==0]<-NA
AUS_MUi$Freq_Scores_SUM[AUS_MUi$Freq_Scores_SUM==0]<-NA

AUS_MUi <- AUS_MUi[!is.na(AUS_MUi$SMUi_MEAN),] # It looks like those to start filling in these scores actually filled in the whole 

rcorr(as.matrix(AUS_MUi))


#-------------------------------------------------------------------------------
# 5.6. Preparation for the median splits (low/high intensity users)
#-------------------------------------------------------------------------------

# Which variables interests me at this point: 

# [1] A scale to assess the SMU-i: The candidate variables: 
AUS_AUS_P_ID$Score_negative_use_SUM # Score - negative use
AUS_AUS_P_ID$SMUi_MEAN # Scale Based on Boer et al. 2022

AUS_AUS_P_ID$AP_Scores # Score - Active Passive (Punished Score)
AUS_AUS_P_ID$ActPAS_medals # Score - Active Passive (Medals)

AUS_AUS_P_ID$Freq_Scores # Frequency of use of specific media - punished score
AUS_AUS_P_ID$Freq_Scores_SUM # Frequency score of specific media - sum

AUS_AUS_P_ID$VisualFreqSUM # Frequency of use visual media - sum
AUS_AUS_P_ID$VisualFreq_Punish # Frequency of use visual media - Punished Score


AUS_AUS1$Use_SNS_1 <- ifelse(AUS_AUS1$Use_SNS_1=="",NA,AUS_AUS1$Use_SNS_1)
AUS_AUS_P_ID$Social_Yes_No <- tapply(AUS_AUS1$Use_SNS_1, AUS_AUS1$Exp_Subject_Id, get_word2)
summary(as.factor(AUS_AUS_P_ID$Social_Yes_No)) # All use social media (at least somehow)


# Passed all tests (Score, AttentionCH1-CH3): 
AUS_AUS_IP_good <- AUS_AUS_P_ID[AUS_AUS_P_ID$TestInterpr=="Good"|AUS_AUS_P_ID$TestInterpr=="Borderline",]
AUS_AUS_IP_good <- AUS_AUS_IP_good[!is.na(AUS_AUS_IP_good$ATCH2),]


# Now - those who lacks score cannot be split: 
AUS_AUS_IP_good <- AUS_AUS_IP_good[!is.na(AUS_AUS_IP_good$SMUi_MEAN),]

# Adding the rest of the variables for a rater (SMU-i, etc.):
AUS_AUS_IP_good$Score_negative_use_Mean # Score - negative use
AUS_AUS_IP_good$SMUi_MEAN # Scale Based on Boer et al. 2022

AUS_AUS_IP_good$AP_Scores # Score - Active Passive (Punished Score)
AUS_AUS_IP_good$ActPAS_medals # Score - Active Passive (Medals)

AUS_AUS_IP_good$Freq_Scores # Frequency of use of specific media - punished score
AUS_AUS_IP_good$Freq_Scores_SUM # Frequency score of specific media - sum

AUS_AUS_IP_good$VisualFreqSUM # Frequency of use visual media - sum
AUS_AUS_IP_good$VisualFreq_Punish # Frequency of use visual media - Punished Score


# No NAs or zeros, let's split (a-b-c): 
SMUi_Mean <- median(AUS_AUS_IP_good$SMUi_MEAN) # YES (b)

Freq_Scores_median <- median(AUS_AUS_IP_good$Freq_Scores) # YES (c)

VisualFreq_Punish_median <- median(AUS_AUS_IP_good$VisualFreq_Punish) # YES (a)



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 5.7. Median splits: Simplified - for the full-size version see some previous versions of this script...
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# How it performs in splitting: 0<,0.5==,>1

#.-.-.-
# 1 Scale Based on Boer et al. 2022 - SCORE (b)
AUS_AUS_IP_good$SMUi_MEAN_Mean_SPLIT <- ifelse(AUS_AUS_IP_good$SMUi_MEAN<SMUi_Mean,0,
                                               ifelse(AUS_AUS_IP_good$SMUi_MEAN==SMUi_Mean,0.5,1))
summary(as.factor(AUS_AUS_IP_good$SMUi_MEAN_Mean_SPLIT))


#.-.-.-
# 2 Frequency of use of media - per given media (Facebook, Instagram, Twitter, WeChat, TikTok, YouTube + self-reported) - punished score (c)
AUS_AUS_IP_good$Freq_Scores_SPLIT <- ifelse(AUS_AUS_IP_good$Freq_Scores<Freq_Scores_median,0,
                                            ifelse(AUS_AUS_IP_good$Freq_Scores==Freq_Scores_median,0.5,1))
summary(as.factor(AUS_AUS_IP_good$Freq_Scores_SPLIT))


#.-.-.-
# 3 Frequency of use visual media - Punished Score (YouTube, TikTok, Instagram) - ONE OF THE FOCUS SCORES
AUS_AUS_IP_good$VisualFreq_Punish_median_SPLIT <- ifelse(AUS_AUS_IP_good$VisualFreq_Punish<VisualFreq_Punish_median,0,
                                                         ifelse(AUS_AUS_IP_good$VisualFreq_Punish==VisualFreq_Punish_median,0.5,1))
summary(as.factor(AUS_AUS_IP_good$VisualFreq_Punish_median_SPLIT))


# How does splitting according to different categories work: 
hist(rowSums(AUS_AUS_IP_good[,72:74]),breaks=12) # Brute force kinda works...
# Now - I am NOT interested in every variable of those... for the sake of visual diet. 



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 5.8. # Alternative attitude - excluding 25 % in the middle: 
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

AUS_AUS_IP_good$SMUi_MEAN
AUS_AUS_IP_good$Freq_Scores
AUS_AUS_IP_good$VisualFreq_Punish

AUS_AUS_IP_good$SMUi_Mean_25_percent <- ifelse(AUS_AUS_IP_good$SMUi_MEAN<=quantile(AUS_AUS_IP_good$SMUi_MEAN,0.375),"sides",
                                               ifelse(AUS_AUS_IP_good$SMUi_MEAN>=quantile(AUS_AUS_IP_good$SMUi_MEAN,0.625),"sides","centre"))
summary(as.factor(AUS_AUS_IP_good$SMUi_Mean_25_percent))


AUS_AUS_IP_good$Freq_Scores_25_percent <- ifelse(AUS_AUS_IP_good$Freq_Scores<=quantile(AUS_AUS_IP_good$Freq_Scores,0.375),"sides",
                                                 ifelse(AUS_AUS_IP_good$Freq_Scores>=quantile(AUS_AUS_IP_good$Freq_Scores,0.625),"sides","centre"))
summary(as.factor(AUS_AUS_IP_good$Freq_Scores_25_percent))


AUS_AUS_IP_good$VisualFreqPunish_25_percent <- ifelse(AUS_AUS_IP_good$VisualFreq_Punish<quantile(AUS_AUS_IP_good$VisualFreq_Punish,0.375),"sides",
                                                      ifelse(AUS_AUS_IP_good$VisualFreq_Punish>quantile(AUS_AUS_IP_good$VisualFreq_Punish,0.625),"sides","centre"))
summary(as.factor(AUS_AUS_IP_good$VisualFreqPunish_25_percent)) # There is nothing in between 37.5 % and 62.5 % of the distribution.



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 5.9. Splitting (Above, Mediocre, Below)
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Split based on "A" (=VisualFreq_Punish)
#                    if not possible, split based on "B" (=SMUi_MEAN)
#                            if not possible, split based on "C" (=Freq_Scores)
# Otherwise "Mediocre"
AUS_AUS_IP_good$VisualFreq_Punish_median_SPLIT
AUS_AUS_IP_good$Split <- ifelse(AUS_AUS_IP_good$VisualFreq_Punish_median_SPLIT==1,"Above",
                                ifelse(AUS_AUS_IP_good$VisualFreq_Punish_median_SPLIT==0,"Below","Mediocre"))

# Step 2: For rows where Split is still "Mediocre", check the "SMUi_MEAN" variable
mediocre_indices <- which(AUS_AUS_IP_good$Split == "Mediocre")
AUS_AUS_IP_good$Split[mediocre_indices] <- ifelse(AUS_AUS_IP_good$SMUi_MEAN_Mean_SPLIT[mediocre_indices] == 1, "Above",
                                                  ifelse(AUS_AUS_IP_good$SMUi_MEAN_Mean_SPLIT[mediocre_indices] == 0, "Below", "Mediocre"))

summary(as.factor(AUS_AUS_IP_good$Split))



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 6.0 # Adding the ratings.
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# (1) Cut the data.frame based on rater's ID: 
# (2) Get the ratings - if she left some faces not-rated, add NAs (mind the specific length of each sample: 
#     2.1. CZ 2016: 50M + 50F
#     2.2. CZ 2019: 39M + 56F

# Faces: 
AUS_AUS1$ATR_16F
AUS_AUS1$ATR_16M
AUS_AUS1$ATR_19F
AUS_AUS1$ATR_19M

# How to (re)order it: Spot the variable "Trial ID" and block name:  
# And since you got the IDs (of both photos, raters, and blocks), you can get rid of NAs:
names(AUS_AUS1[,c(2,6,22:25,36:39,99:102,118)])
AUSRAT_List_FACES <- as.data.frame(AUS_AUS1[,c(2,6,22:25,36:39,99:102,118)]) # Only rating variables + IDs (all of them)

# Adding the order in the table
AUSRAT_List_FACES$OrOd <- seq(from=1, to=nrow(AUSRAT_List_FACES), by=1)

# Only rows that contain ratings are of interest: 
levels(as.factor(AUSRAT_List_FACES$Task_Name))
selected_levs <- c("CZ_16F_ATD","CZ_16M_ATD","CZ_19F_ATD","CZ_19M_ATD")  

AUSRAT_List_FACES <- AUSRAT_List_FACES[AUSRAT_List_FACES$Task_Name %in% selected_levs,]
AUSRAT_List_FACES$FullID <- paste(AUSRAT_List_FACES$Trial_Id,AUSRAT_List_FACES$Task_Name)


# Match the two tables - the one with IDs and the one with ratings: 
AUSRAT_List_FACES <- merge(AUSRAT_List_FACES, real_IDs, by = "FullID", all = TRUE)

AUSRAT_List_FACES <- AUSRAT_List_FACES[order(AUSRAT_List_FACES$OrOd),]

names(AUSRAT_List_FACES)[names(AUSRAT_List_FACES) == 'Exp_Subject_Id_118'] <- "AUS_AUS_P_ID"

# These are then the (only) raters you are interested in: 
# Subset the table: CZ_CZ1 - take only those "selected" raters
# Put the attractiveness, trustworthiness, and dominance, that are now split into 
# four columns each into just three columns (one for trustw., one for attractiveness, one for dominance):

AUSRAT_List_FACES <- AUSRAT_List_FACES[AUSRAT_List_FACES$Task_Name %in% selected_levs,]

AUSRAT_List_FACES <- merge(AUSRAT_List_FACES, AUS_AUS_IP_good, by = "AUS_AUS_P_ID", all = F)
AUSRAT_List_FACES <- AUSRAT_List_FACES[order(AUSRAT_List_FACES$OrOd),]

#-------------------------------------------------------------------------------
# 6.2 Finalising, the first step is "there is always just one non-zero number in each quartet
#-------------------------------------------------------------------------------

AUSRAT_List_FACES$ATR <- rowSums(AUSRAT_List_FACES[,5:8], na.rm = T)
AUSRAT_List_FACES$TRU <- rowSums(AUSRAT_List_FACES[,13:16], na.rm = T)
AUSRAT_List_FACES$DOM <- rowSums(AUSRAT_List_FACES[,9:12], na.rm = T)

# The core of the model is like this: 
# I am interested in the effect Sample (country of origin) and SNS use intensity (expressed by whichever variable)
# as Fixed Effects have on rating scale / scales 

AUSRAT_List_FACES$ATR
AUSRAT_List_FACES$TRU
AUSRAT_List_FACES$DOM

AUSRAT_List_FACES$Sample <- "AUS"


# The simplest way: separate model for each scale of these...
AUS_USE_THIS_FOR_LONG_TABLE <- data.frame(Particip_ID = AUSRAT_List_FACES[,1],
                                          Face_ID = AUSRAT_List_FACES[,23],
                                          Above_Below = AUSRAT_List_FACES[,100],
                                          Atr = AUSRAT_List_FACES[,101],
                                          Tru = AUSRAT_List_FACES[,102],
                                          Dom = AUSRAT_List_FACES[,103],
                                          Sample = AUSRAT_List_FACES[,104],
                                          CentreSide = paste(AUSRAT_List_FACES$SMUi_Mean_25_percent,
                                                             AUSRAT_List_FACES$Freq_Scores_25_percent,
                                                             AUSRAT_List_FACES$VisualFreqPunish_25_percent),
                                          ScoreAbroad = AUSRAT_List_FACES[,49],
                                          Fami_Back =  AUSRAT_List_FACES[,50]
)

save(AUS_USE_THIS_FOR_LONG_TABLE, file="AUS_USE_THIS_FOR_LONG_TABLE.Rdata")
load("AUS_USE_THIS_FOR_LONG_TABLE.Rdata")

# Specify the objects you want to keep
objects_to_keep <- c("CZ_USE_THIS_FOR_LONG_TABLE",
                     "VN_USE_THIS_FOR_LONG_TABLE", 
                     "RSA_USE_THIS_FOR_LONG_TABLE",
                     "AUS_USE_THIS_FOR_LONG_TABLE",
                     "CZ_CZ_IP_good",
                     "VN_VN_IP_good",
                     "RSA_RSA_IP_good",
                     "AUS_AUS_IP_good",
                     "apply_medal", "get_word2", "real_IDs")  # Replace with your object names

# Remove all other objects
rm(list = setdiff(ls(), objects_to_keep))




#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#  Colombian data 
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

COL_COL1 <- read.csv2("Colombia_02_12_24.csv",T)

# You are now interested in FACE raters: 
selected_levs <- c("FaceRaters_16A","FaceRaters_16B","FaceRaters_19A","FaceRaters_19B")  

COL_COL1 <- COL_COL1[COL_COL1$Group_Name %in% selected_levs,]

COL_COL_P_ID <- levels(as.factor(COL_COL1$Exp_Subject_Id)) # Participants IDs
# Get rid of those who did not proceed: 
COL_COL_P_ID<-as.data.frame(COL_COL_P_ID)
COL_COL_P_ID$lengths <- tapply(COL_COL1$Rec_Session_Id1, COL_COL1$Exp_Subject_Id, length)
hist(COL_COL_P_ID$lengths) # Being above 100 means they likely finished the study...


#-------------------------------------------------------------------------------
# 1 Basic demographics 
#-------------------------------------------------------------------------------

# Participant's demographic variables...
# All these variables either are naturally numeric (age...), or were coded as numeric (device)

# AGE: 
COL_COL_P_ID$Age <- tapply(COL_COL1$SELF_age_1,COL_COL1$Exp_Subject_Id,mean, na.rm=T)
COL_COL_P_ID$Age[COL_COL_P_ID$Age=="NaN"]<-NA

# HEIGHT: 
COL_COL_P_ID$Height <- tapply(COL_COL1$b_height_F1,COL_COL1$Exp_Subject_Id,mean, na.rm=T)
COL_COL_P_ID$Height[COL_COL_P_ID$Height=="NaN"]<-NA

# WEIGHT: 
COL_COL1$b_weight_F1 <- as.numeric(COL_COL1$b_weight_F1)
COL_COL_P_ID$weight <- tapply(COL_COL1$b_weight_F1,COL_COL1$Exp_Subject_Id,mean, na.rm=T)
COL_COL_P_ID$weight[COL_COL_P_ID$weight=="NaN"]<-NA

# MARITAL STATUS: 
COL_COL_P_ID$MariStat <- tapply(COL_COL1$Marital_status_F162,COL_COL1$Exp_Subject_Id,mean, na.rm=T)
COL_COL_P_ID$MariStat[COL_COL_P_ID$MariStat=="NaN"]<-NA

# Decoded to words: 
COL_COL_P_ID$MariStat_VERB <- ifelse(COL_COL_P_ID$MariStat==1,"Single",
                                   ifelse(COL_COL_P_ID$MariStat==2,"In_a_relationship",
                                          ifelse(COL_COL_P_ID$MariStat==3, "Married",
                                                 ifelse(COL_COL_P_ID$MariStat==4, "Divorced",
                                                        ifelse(COL_COL_P_ID$MariStat==5, "Widowed",
                                                               ifelse(COL_COL_P_ID$MariStat==6, "DontWaDeclare",NA))))))


# MOTHER TONGUE:
COL_COL1$Rodny_jazyk <- ifelse(COL_COL1$Mother_Tongue_1=="",NA,COL_COL1$Mother_Tongue_1)
COL_COL_P_ID$MotherTongue <- tapply(COL_COL1$Rodny_jazyk, COL_COL1$Exp_Subject_Id, get_word2)

# PASSPORT NATIONALITY
COL_COL1$Passport.nationality <- ifelse(COL_COL1$Passport.nationality=="",NA,COL_COL1$Passport.nationality)
COL_COL_P_ID$PasspNat <- tapply(COL_COL1$Passport.nationality, COL_COL1$Exp_Subject_Id, get_word2)

# Ethnicity (definition)
COL_COL1$Eth_understanding <- ifelse(COL_COL1$Eth_understanding=="",NA,COL_COL1$Eth_understanding)
COL_COL_P_ID$EthUNDERSTD <- tapply(COL_COL1$Eth_understanding, COL_COL1$Exp_Subject_Id, get_word2)

# Ethnicity (self-reported)
COL_COL1$Self_reported_ethnicity <- ifelse(COL_COL1$Self_reported_ethnicity=="",NA,COL_COL1$Self_reported_ethnicity)
COL_COL_P_ID$Eth_SelfREP <- tapply(COL_COL1$Self_reported_ethnicity, COL_COL1$Exp_Subject_Id, get_word2)

# Gender (self-reported):
COL_COL1$Pohlavi_pri_narozeni <- ifelse(COL_COL1$Gender.at.birth_1=="",NA,COL_COL1$Gender.at.birth_1)
COL_COL_P_ID$SEXATB <- tapply(COL_COL1$Pohlavi_pri_narozeni, COL_COL1$Exp_Subject_Id, get_word2)


#-------------------------------------------------------------------------------
# 2 Technical variables
#-------------------------------------------------------------------------------

COL_COL_P_ID$Technical <- "TECHNICAL"
# Device auto-selected: 
COL_COL_P_ID$DEVICE_LABV <- tapply(COL_COL1$System_Spec, COL_COL1$Exp_Subject_Id, unique)

# Password for Entrance Examination:
COL_COL1$Participant_Feedback <- ifelse(COL_COL1$Participant_Feedback=="",NA,COL_COL1$Participant_Feedback)
COL_COL_P_ID$PASSW <- tapply(COL_COL1$Participant_Feedback, COL_COL1$Exp_Subject_Id, unique)

# Window resolution: 
# WIDTH:
COL_COL_P_ID$Window_WWW <- tapply(COL_COL1$Window_Width_In_Pixels, COL_COL1$Exp_Subject_Id, unique)
# HEIGHT: 
COL_COL_P_ID$Window_HHH <- tapply(COL_COL1$Window_Height_In_Pixels, COL_COL1$Exp_Subject_Id, unique)

# Device screen resolution: 
# WIDTH:
COL_COL_P_ID$Screen_WWW <- tapply(COL_COL1$Screen_Width_In_Pixels, COL_COL1$Exp_Subject_Id, unique)
# HEIGHT: 
COL_COL_P_ID$Screen_HHH <- tapply(COL_COL1$Screen_Height_In_Pixels, COL_COL1$Exp_Subject_Id, unique)

# Selected location (where LabVanced thinks the person is located - may be tricky due to people travelling 
# a lot and VPNs...)
COL_COL_P_ID$SelectedLoc <- tapply(COL_COL1$SelectedLocation, COL_COL1$Exp_Subject_Id, unique)


#-------------------------------------------------------------------------------
# 3 Was the person conscious (attention checks and score in a test): 
#-------------------------------------------------------------------------------

COL_COL_P_ID$Conscious <- "CONSCIOUS"

# Test score: Maximum value for a person in the variable "correct_36" is what you want...
# To get rid of a warning related to NA-only vectors, lets assign -1 to each body who did not bother to start the test:
COL_COL1$correct <- ifelse(is.na(COL_COL1$correct),-1,COL_COL1$correct)

COL_COL_P_ID$TestScore <- tapply(as.numeric(COL_COL1$correct), COL_COL1$Exp_Subject_Id, max, na.rm=T)
COL_COL_P_ID$TestInterpr <- ifelse(COL_COL_P_ID$TestScore==5|COL_COL_P_ID$TestScore==4,"Good",ifelse(COL_COL_P_ID$TestScore==3,"Borderline","Exclude!"))

summary(as.factor(COL_COL_P_ID$TestInterpr))

# Attention Checks (the kind of variable where there is one word for the vector [OR NA]: 
# 1 (Green/Zeleny)
COL_COL1$Attention_Check_Green <- ifelse(COL_COL1$Attention_Check_Green=="",NA,COL_COL1$Attention_Check_Green)
COL_COL_P_ID$ATCH1 <- tapply(COL_COL1$Attention_Check_Green, COL_COL1$Exp_Subject_Id, get_word2)
summary(as.factor(COL_COL_P_ID$ATCH1 )) # One Grizzly's to be excluded! 

# 2 (1968)
COL_COL1$Attention_Check_CCCP <- ifelse(COL_COL1$Attention_Check_CCCP=="",NA,COL_COL1$Attention_Check_CCCP)
COL_COL_P_ID$ATCH2 <- tapply(COL_COL1$Attention_Check_CCCP, COL_COL1$Exp_Subject_Id, get_word2)
summary(as.factor(COL_COL_P_ID$ATCH2 ))

# 3 (Motýl/Butterfly)
COL_COL1$Attention_Check_Butter <- ifelse(COL_COL1$Attention_Check_Butter=="",NA,COL_COL1$Attention_Check_Butter)
COL_COL_P_ID$ATCH3 <- tapply(COL_COL1$Attention_Check_Butter, COL_COL1$Exp_Subject_Id, get_word2)
summary(as.factor(COL_COL_P_ID$ATCH3))


#-------------------------------------------------------------------------------
# 4 Social background (outside social media): 
#-------------------------------------------------------------------------------

COL_COL_P_ID$SocBack <- "SocBack"

# Travel abroad: 
# 1 = Velmi často = Often
# 2 = Spíše často = Rather often
# 3 = Občas = Occasionally
# 4 = Zřídka = Rarely
# 5 = Velmi zřídka = Very rarely
# 6 = Nikdy = never
COL_COL1$Travel_Abroad <- ifelse(COL_COL1$Travel_Abroad=="",NA,COL_COL1$Travel_Abroad)
COL_COL_P_ID$ScoreAbroad <- tapply(COL_COL1$Travel_Abroad, COL_COL1$Exp_Subject_Id, get_word2)
summary(as.factor(COL_COL_P_ID$ScoreAbroad))

# Family background: 
# 1 = "Bohaté materiální zabezpečení, peníze jsme nikdy neřešili" = Rich 
# 2 = "Dost peněz, i když jsme se někdy museli uskormnit" = Upper CL
# 3 = "Někdy více, někdy méně, ale dokázali jsme vyžít bez ztráty úrovně" = Middle
# 4 = "Spíše málo peněz, ve sroCOLání s vrstevníky jsem se musel často uskromnit" = Lower CL 
# 5 = "Neustálé finanční potíže" = Poor
# 6 = "Nepřeji si odpovídat" = No family (the last level is labelled incorrectly, will be corrected)
COL_COL1$Family_SOC <- ifelse(COL_COL1$Family_SOC=="",NA,COL_COL1$Family_SOC)
COL_COL_P_ID$Fami_Back <- tapply(COL_COL1$Family_SOC, COL_COL1$Exp_Subject_Id, get_word2)
# Correcting the level with wrong label: 
COL_COL_P_ID$Fami_Back[COL_COL_P_ID$Fami_Back=="No family"] <- "PreferNR"
summary(as.factor(COL_COL_P_ID$Fami_Back))


#-------------------------------------------------------------------------------
# 5 Social media use... All variables  are coded as numbers...
# 5.1 SNS - general - (c) Boer & Eijinden (2018 2022)
#-------------------------------------------------------------------------------


# Are there some complete non-users: 
summary(as.factor(COL_COL1$Use_SNS_NO)) # All 16 to respond the survey so far are users... 


# How many times a day - check the content: "JakCastoPas1"
# 1 = "Nikdy nebou méně než jednou denně"
# 2 = "1-2krát denně"
# 3 = "3-5krát denně"
# 4 = "6-10krát denně"
# 5 = "11-20krát denně"
# 6 = "21-40krát denně"
# 7 = "Více než 40krát denně"
COL_COL1$JakCastoPas <- ifelse(COL_COL1$HowOftenPassive=="",NA,COL_COL1$HowOftenPassive)
COL_COL_P_ID$SMUi_Q1 <- tapply(COL_COL1$JakCastoPas, COL_COL1$Exp_Subject_Id, get_word2)

# How many times a week - posting: "JakCastoPost1"
# 1 = "Nikdy nebou méně než jednou týdně"
# 2 = "1-2krát týdně"
# 3 = "3-5krát týdně"
# 4 = "6-10krát týdně"
# 5 = "11-20krát týdně"
# 6 = "21-40krát týdně"
# 7 = "Více než 40krát týdně"
COL_COL1$JakCastoPost1 <- ifelse(COL_COL1$HowOftenPosting=="",NA,COL_COL1$HowOftenPosting)
COL_COL_P_ID$SMUi_Q2 <- tapply(COL_COL1$JakCastoPost1, COL_COL1$Exp_Subject_Id, get_word2)

# How many times a week - likes posts of the others: "JakCastoLikes1"
# 1 = "Nikdy nebou méně než jednou týdně"
# 2 = "1-2krát týdně"
# 3 = "3-5krát týdně"
# 4 = "6-10krát týdně"
# 5 = "11-20krát týdně"
# 6 = "21-40krát týdně"
# 7 = "Více než 40krát týdně"
COL_COL1$JakCastoLikes1 <- ifelse(COL_COL1$HowOftenLiking=="",NA,COL_COL1$HowOftenLiking)
COL_COL_P_ID$SMUi_Q3 <- tapply(COL_COL1$JakCastoLikes1, COL_COL1$Exp_Subject_Id, get_word2)

########
#      #
# (B)  # Social Media Use Intensity -> Mean, based on BOER... 
#      #
########

# Will then compute the median value from the three variables' scores: 
COL_COL_P_ID$SMUi_MEAN <- apply(COL_COL_P_ID[,c(30:32)],1,mean)# The higher the more intensive user: 


#-------------------------------------------------------------------------------
# 5.2 Social media use - negative effects - FOMO, Too Much Time, Miss Others...
#-------------------------------------------------------------------------------

# 1 = zcela souhlasím (Totally agree)
# 2 = spíše souhlasím (rather agree)
# 3 = Ani souhlas ani nesouhlas (neither agree nor disagree)
# 4 = Spíše nesouhlasím (rather disagree)
# 5 = zcela nesouhlasím (Totally disagree)

# SNS_too_much_time:
COL_COL1$SNS_too_much_time
COL_COL_P_ID$SM_too_much_time <- tapply(COL_COL1$SNS_too_much_time, COL_COL1$Exp_Subject_Id, get_word2)

# SNS_sometimes_miss_other:
COL_COL1$SNS_Sometimes_miss_other
COL_COL_P_ID$Miss_other <- tapply(COL_COL1$SNS_Sometimes_miss_other, COL_COL1$Exp_Subject_Id, get_word2)

# SNS_know_better_than_friends:
COL_COL1$SNS_know_better_than_friends
COL_COL_P_ID$SM_friends_better <- tapply(COL_COL1$SNS_know_better_than_friends, COL_COL1$Exp_Subject_Id, get_word2)

# SMP_fear_of_missing_out:
COL_COL1$SMP_fear_of_missing_out76
COL_COL_P_ID$FOMO_SM <- tapply(COL_COL1$SMP_fear_of_missing_out, COL_COL1$Exp_Subject_Id, get_word2)

# Recalculate so that the higher (up to five), the worse: 
View(data.frame(Orig=COL_COL_P_ID$SM_too_much_time,New=6 - COL_COL_P_ID$SM_too_much_time))
COL_COL_P_ID$SM_too_much_time <- 6 - COL_COL_P_ID$SM_too_much_time

COL_COL_P_ID$Miss_other <- 6 - COL_COL_P_ID$Miss_other

COL_COL_P_ID$SM_friends_better <- 6 - COL_COL_P_ID$SM_friends_better

COL_COL_P_ID$FOMO_SM <- 6 - COL_COL_P_ID$FOMO_SM

# The score will be like "The higher, the more intensive user": 
COL_COL_P_ID$Score_negative_use_SUM <- rowSums(COL_COL_P_ID[,34:37])
COL_COL_P_ID$Score_negative_use_Mean <- apply(COL_COL_P_ID[,34:37],1,mean)
COL_COL_P_ID$Score_negative_use_Mean[is.na(COL_COL_P_ID$Score_negative_use_Mean)]<-0


#-------------------------------------------------------------------------------
# 5.3 Which SNS - ACTIVELY, SEMI-ACTIVELY OR PASSIVELY
#-------------------------------------------------------------------------------

# AP - stands for ACTIVE/PASSIVE: 
COL_COL_P_ID$ACTPAS<- "ACTPAS"
COL_COL_P_ID$Facebook_AP <- tapply(COL_COL1$AP_Facebook,COL_COL1$Exp_Subject_Id, mean, na.rm=T)
COL_COL_P_ID$Facebook_AP[COL_COL_P_ID$Facebook_AP=="NaN"]<-NA
COL_COL_P_ID$Instagram_AP <- tapply(COL_COL1$AP_Instagram,COL_COL1$Exp_Subject_Id, mean, na.rm=T)
COL_COL_P_ID$Instagram_AP[COL_COL_P_ID$Instagram_AP=="NaN"]<-NA

COL_COL_P_ID$TikTok_AP <- tapply(COL_COL1$AP_TikTok,COL_COL1$Exp_Subject_Id, mean, na.rm=T)
COL_COL_P_ID$TikTok_AP[COL_COL_P_ID$TikTok_AP=="NaN"]<-NA
COL_COL_P_ID$WhattsAPP_AP <- tapply(COL_COL1$AP_WhatsApp19,COL_COL1$Exp_Subject_Id, mean, na.rm=T)
COL_COL_P_ID$WhattsAPP_AP[COL_COL_P_ID$WhattsAPP_AP=="NaN"]<-NA

COL_COL_P_ID$YouTube_AP <- tapply(COL_COL1$AP_YouTube,COL_COL1$Exp_Subject_Id, mean, na.rm=T)
COL_COL_P_ID$YouTube_AP[COL_COL_P_ID$YouTube_AP=="NaN"]<-NA
COL_COL_P_ID$Twitter_AP <- tapply(COL_COL1$AP_X..Twitter.,COL_COL1$Exp_Subject_Id, mean, na.rm=T)
COL_COL_P_ID$Twitter_AP[COL_COL_P_ID$Twitter_AP=="NaN"]<-NA


# A/P - self reported (if any...), should be 3 items...  
# First one - which one (NAME): 
COL_COL1$AP_Input.social.media_1 <- ifelse(COL_COL1$AP_Input.social.media_1=="",NA,COL_COL1$AP_Input.social.media_1)
COL_COL_P_ID$SELF_AP_NAME_1 <- tapply(COL_COL1$AP_Input.social.media_1, COL_COL1$Exp_Subject_Id, get_word2)

# How (active/passive) - yes, 1st self reported SNS is (contrary to the expectation) the name of the variable...
COL_COL_P_ID$SELF_REP1_AP <- tapply(COL_COL1$X1st.self.reported.SNS,COL_COL1$Exp_Subject_Id, mean, na.rm=T)
COL_COL_P_ID$SELF_REP1_AP[COL_COL_P_ID$SELF_REP1_AP=="NaN"]<-NA

# Second one - which one (NAME): 
COL_COL1$AP_Input.social.media.2 <- ifelse(COL_COL1$AP_Input.social.media.2=="",NA,COL_COL1$AP_Input.social.media.2)
COL_COL_P_ID$SELF_AP_NAME_2 <- tapply(COL_COL1$AP_Input.social.media.2, COL_COL1$Exp_Subject_Id, get_word2)

# How (active/passive) - yes, 1st self reported SNS is (contrary to the expectation) the name of the variable...
COL_COL_P_ID$SELF_REP2_AP <- tapply(COL_COL1$X2nd.self.reported.SNS,COL_COL1$Exp_Subject_Id, mean, na.rm=T)
COL_COL_P_ID$SELF_REP2_AP[COL_COL_P_ID$SELF_REP2_AP=="NaN"]<-NA

# Third one - which one (NAME): 
COL_COL1$AP_Input.social.media.3 <- ifelse(COL_COL1$AP_Input.socal.media.314=="",NA,COL_COL1$AP_Input.social.media.314)
COL_COL_P_ID$SELF_AP_NAME_3 <- tapply(COL_COL1$AP_Input.social.media.3, COL_COL1$Exp_Subject_Id, get_word2)

# Now it creates three new variables but it's OK like this...

# How (active/passive) - yes, 1st self reported SNS is (contrary to the expectation) the name of the variable...
COL_COL_P_ID$SELF_REP3_AP <- tapply(COL_COL1$X3nd.self.reported.SNS,COL_COL1$Exp_Subject_Id, mean, na.rm=T)
COL_COL_P_ID$SELF_REP3_AP[COL_COL_P_ID$SELF_REP3_AP=="NaN"]<-NA


# Recalculating: Now the higher score the less she uses it, maximum is 4: 
COL_COL_P_ID$Facebook_AP <- 5 - COL_COL_P_ID$Facebook_AP
COL_COL_P_ID$Instagram_AP <- 5 - COL_COL_P_ID$Instagram_AP

COL_COL_P_ID$TikTok_AP <- 5 - COL_COL_P_ID$TikTok_AP
COL_COL_P_ID$WhattsAPP_AP <- 5 - COL_COL_P_ID$WhattsAPP_AP # Yes, it's misspelled, I better not fix it now as I would run into troubles not spotting all the occasions. 

COL_COL_P_ID$YouTube_AP <- 5 - COL_COL_P_ID$YouTube_AP
COL_COL_P_ID$Twitter_AP <- 5 - COL_COL_P_ID$Twitter_AP

COL_COL_P_ID$SELF_REP1_AP <- 5 - COL_COL_P_ID$SELF_REP1_AP
COL_COL_P_ID$SELF_REP2_AP <- 5 - COL_COL_P_ID$SELF_REP2_AP
COL_COL_P_ID$SELF_REP3_AP <- 5 - COL_COL_P_ID$SELF_REP3_AP



# Score 1: A/P punished: 
# max(vector_APs) + 0.75(^1)*max(vector_APs[vector_APs!=max()]) + 0.75(^2)*...

NN <- 36

# Put these variables in a vector by rows:
ActPASS<-matrix(NA, nrow=9,ncol=NN)
colnames(ActPASS) <- (COL_COL_P_ID[,1])
AP_Scores <- as.vector(rep(NA,NN))

for (i in 1:nrow(COL_COL_P_ID)) {
  ActPASS[,i]<-unlist(COL_COL_P_ID[i,c(41:46, 48, 50, 52)])
  ActPASS[,i]<-ifelse(is.na(ActPASS[,i]),0,ActPASS[,i])
}

# Define the punishment factor and the length of the vector to consider
punishment_factor <- 0.75
vect_length <- nrow(ActPASS) 

# Create a vector of the punishment multipliers
multipliers <- punishment_factor^(0:(vect_length-1))

# Calculate scores using vectorized operations: Check whether it takes all the highest possible scores (If there is three times 6,
# does it count each?) 
AP_Scores <- apply(ActPASS, 2, function(column) {
  sorted_column <- sort(column, decreasing = TRUE)
  sum(sorted_column[1:vect_length] * multipliers)
})

COL_COL_P_ID$AP_Scores <- AP_Scores


# Get coordinates of "horizontal vectors of medals": 
medals <- c(4,2,1,0)

# Calculate scores using vectorized operations: Check whether it takes all the highest possible scores (If there is three times 6,
# does it count each?) 

Medal_matrix <- apply(ActPASS, c(1, 2), apply_medal)

Medal_sums <- colSums(Medal_matrix)

COL_COL_P_ID$ActPAS_medals <- Medal_sums


#-------------------------------------------------------------------------------
# 5.4 Frequency of using of SPECIFIC social media (i.e., one by one):
#-------------------------------------------------------------------------------

COL_COL_P_ID$FreqSPEC<- "FreqSPEC"

# How_frequently_preselected: 
# 1 = I don't use it (Síť nepoužívám)
# 2 = Once a week or less (Jednou týdně či méně)
# 3 = Once a week, not daily (Víckrát týdně, ale ne denně)
# 4 = Once a day (Jednou za den)
# 5 = Several times a day (Několikrát denně)
# 6 = Frequently during the day (Často během dne) 

# The higher score the better - no need to recalculate anything

# Facebook = How_frequently_Facebook
COL_COL_P_ID$Facebook_Freq <- tapply(COL_COL1$How_frequently_Facebook,COL_COL1$Exp_Subject_Id, mean, na.rm=T)
COL_COL_P_ID$Facebook_Freq[COL_COL_P_ID$Facebook_Freq=="NaN"]<-NA

# YouTube = How_frequently_YouTube
COL_COL_P_ID$YouTube_Freq <- tapply(COL_COL1$How_frequently_YouTube,COL_COL1$Exp_Subject_Id, mean, na.rm=T)
COL_COL_P_ID$YouTube_Freq[COL_COL_P_ID$YouTube_Freq=="NaN"]<-NA

# Instagram = How_frequently_Instagram
COL_COL_P_ID$Instagram_Freq <- tapply(COL_COL1$How_frequently_Instagram,COL_COL1$Exp_Subject_Id, mean, na.rm=T)
COL_COL_P_ID$Instagram_Freq[COL_COL_P_ID$Instagram_Freq=="NaN"]<-NA

# TikTok = How_frequently_TikTok
COL_COL_P_ID$TikTok_Freq <- tapply(COL_COL1$How_frequently_TikTok,COL_COL1$Exp_Subject_Id, mean, na.rm=T)
COL_COL_P_ID$TikTok_Freq[COL_COL_P_ID$TikTok_Freq=="NaN"]<-NA


# X[Twitter] = How_frequently_X
COL_COL_P_ID$X_Freq <- tapply(COL_COL1$How_frequently_X,COL_COL1$Exp_Subject_Id, mean, na.rm=T)
COL_COL_P_ID$X_Freq[COL_COL_P_ID$X_Freq=="NaN"]<-NA

# WeChat = How_Frequently_WECHAT
COL_COL_P_ID$WhatsApp_Freq <- tapply(COL_COL1$How_Frequently_WECHAT56,COL_COL1$Exp_Subject_Id, mean, na.rm=T) # WE chat is WhatsAPP!!
COL_COL_P_ID$WhatsApp_Freq[COL_COL_P_ID$WhatsApp_Freq=="NaN"]<-NA


# Self reported SNS:  (Time = frequency...)

# NAME_1: TIME_Input social media_1
COL_COL1$TIME_Input.social.media_1103 <- ifelse(COL_COL1$TIME_Input.social.media_1103=="",NA,COL_COL1$TIME_Input.social.media_1103)
COL_COL_P_ID$Time_SELFREP_Name1 <- tapply(COL_COL1$TIME_Input.social.media_1103, COL_COL1$Exp_Subject_Id, get_word2)


# Frequency_1: TIME_self reported SNS
COL_COL_P_ID$Time_SELFREP_Freq1 <- tapply(COL_COL1$TIME_self.reported.SNS,COL_COL1$Exp_Subject_Id, mean, na.rm=T)
COL_COL_P_ID$Time_SELFREP_Freq1[COL_COL_P_ID$Time_SELFREP_Freq1=="NaN"]<-NA


# NAME_2: TIME_Input social media_2
COL_COL1$TIME_Input.social.media.2102 <- ifelse(COL_COL1$TIME_Input.social.media.2102=="",NA,COL_COL1$TIME_Input.social.media.2102)
COL_COL_P_ID$Time_SELFREP_Name2 <- tapply(COL_COL1$TIME_Input.social.media.2102, COL_COL1$Exp_Subject_Id, get_word2)

# Frequency_2: TIME_2nd self reported SNS
COL_COL_P_ID$Time_SELFREP_Freq2 <- tapply(COL_COL1$TIME_2nd.self.reported.SNS81,COL_COL1$Exp_Subject_Id, mean, na.rm=T)
COL_COL_P_ID$Time_SELFREP_Freq2[COL_COL_P_ID$Time_SELFREP_Freq2=="NaN"]<-NA
COL_COL_P_ID$Time_SELFREP_Freq2 <- ifelse(is.na(COL_COL_P_ID$Time_SELFREP_Name2), NA, COL_COL_P_ID$Time_SELFREP_Freq2 )


# NAME_3: TIME_Input social media_3
COL_COL1$TIME_Input.social.media_3104 <- ifelse(COL_COL1$TIME_Input.social.media_3104=="",NA,COL_COL1$TIME_Input.social.media_3104)
COL_COL_P_ID$Time_SELFREP_Name3 <- tapply(COL_COL1$TIME_Input.social.media_3104, COL_COL1$Exp_Subject_Id, get_word2)

# Frequency_3: TIME_3nd self reported SNS
COL_COL_P_ID$Time_SELFREP_Freq3 <- tapply(COL_COL1$TIME_3nd.self.reported.SNS82,COL_COL1$Exp_Subject_Id, mean, na.rm=T)
COL_COL_P_ID$Time_SELFREP_Freq3[COL_COL_P_ID$Time_SELFREP_Freq3=="NaN"]<-NA



#------------------

########
#      #
# (C)  # Specific Intensity Use -> Punished Score, one of the six (nine) social media 
#      #
########

# Frequency scores: 
# Score 1: Freq - punished: 

# Put these variables in a vector by rows:
FreqSC<-matrix(NA, nrow=9,ncol=NN)
colnames(FreqSC) <- (COL_COL_P_ID[,1])
Freq_Scores <- as.vector(rep(NA,NN)) 

for (i in 1:nrow(COL_COL_P_ID)) {
  FreqSC[,i]<-unlist(COL_COL_P_ID[i,c(56:61,63,65,67)])
  FreqSC[,i]<-ifelse(is.na(FreqSC[,i]),0,FreqSC[,i])
}

# Define the punishment factor and the length of the vector to consider
punishment_factor <- 0.75
vect_length <- nrow(FreqSC) 

# Create a vector of the punishment multipliers
multipliers <- punishment_factor^(0:(vect_length-1));multipliers

# Calculate scores using vectorized operations
Freq_Scores <- apply(FreqSC, 2, function(column) {
  sorted_column <- sort(column, decreasing = TRUE)
  sum(sorted_column[1:vect_length] * multipliers)
})
Freq_Scores
hist(Freq_Scores) # Normally distributed, except those who are, in fact, NAs

COL_COL_P_ID$Freq_Scores <- Freq_Scores
COL_COL_P_ID$Freq_Scores_SUM <- rowSums(COL_COL_P_ID[,c(56:61,63,65,67)], na.rm=T)



# Other available scores: 
# Frequency of use: General (median score from the three variables) 
COL_COL_P_ID$SMUi_MEAN
hist(COL_COL_P_ID$SMUi_MEAN, breaks=25)

# Frequency of use: SPECIFIC - the higher the more intensive & on more SMs
COL_COL_P_ID$Freq_Scores
hist(COL_COL_P_ID$Freq_Scores, breaks=25)

# Actvie/Passive: the higher the more active & more individual SM
COL_COL_P_ID$AP_Scores
hist(COL_COL_P_ID$AP_Scores, breaks=25)

# Negative use - the higher the worse: 
COL_COL_P_ID$Score_negative_use_SUM
hist(COL_COL_P_ID$Score_negative_use_SUM, breaks=25)
hist(COL_COL_P_ID$Score_negative_use_Mean, breaks=25)

hist(COL_COL_P_ID$Freq_Scores_SUM,breaks=25)


# SMU-i - based only on "Visual" social media: TikTok, YouTube, Instagram
COL_COL_P_ID$VisualFreqSUM <- rowSums(COL_COL_P_ID[,c(57:59)])


########
#      #
# (A)  # Visual International Social Media Use -> Punished Score, one of the three Key Coefficients 
#      #
########

# Punished Score: 

# Put these variables in a vector by rows:
FreqSC<-matrix(NA, nrow=3,ncol=NN)
colnames(FreqSC) <- (COL_COL_P_ID[,1])
Freq_Scores <- as.vector(rep(NA,NN))

for (i in 1:nrow(COL_COL_P_ID)) {
  FreqSC[,i]<-unlist(COL_COL_P_ID[i,c(57:59)])
  FreqSC[,i]<-ifelse(is.na(FreqSC[,i]),0,FreqSC[,i])
}


# Define the punishment factor and the length of the vector to consider
punishment_factor <- 0.75
vect_length <- nrow(FreqSC) 

# Create a vector of the punishment multipliers
multipliers <- punishment_factor^(0:(vect_length-1))

# Calculate scores using vectorized operations
Freq_Scores <- apply(FreqSC, 2, function(column) {
  sorted_column <- sort(column, decreasing = TRUE)
  sum(sorted_column[1:vect_length] * multipliers)
})

COL_COL_P_ID$VisualFreq_Punish <- Freq_Scores


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# How related are these scales...
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Create a new data frame -> Add NAs instead of zeros (null score was not possible but when no response was provided):
COL_MUi <- data.frame(SMUi_MEAN=COL_COL_P_ID$SMUi_MEAN,
                      Freq_Scores=COL_COL_P_ID$Freq_Scores,
                      Freq_Scores_SUM=COL_COL_P_ID$Freq_Scores_SUM,
                      AP_Scores=COL_COL_P_ID$AP_Scores,
                      Score_negative_use_SUM=as.numeric(COL_COL_P_ID$Score_negative_use_SUM),
                      Score_negative_use_Mean=as.numeric(COL_COL_P_ID$Score_negative_use_Mean),
                      VisualFreq_Punish=as.numeric(COL_COL_P_ID$VisualFreq_Punish),
                      VisualFreqSUM=as.numeric(COL_COL_P_ID$VisualFreqSUM))


COL_MUi$SMUi_MEAN[COL_MUi$SMUi_MEAN==0]<-NA
COL_MUi$Freq_Scores[COL_MUi$Freq_Scores==0]<-NA
COL_MUi$AP_Scores[COL_MUi$AP_Scores==0]<-NA
COL_MUi$VisualFreq_Punish[COL_MUi$VisualFreq_Punish==0]<-NA
COL_MUi$Freq_Scores_SUM[COL_MUi$Freq_Scores_SUM==0]<-NA

COL_MUi <- COL_MUi[!is.na(COL_MUi$SMUi_MEAN),] # It looks like those to start filling in these scores actually filled in the whole 

rcorr(as.matrix(COL_MUi))


#-------------------------------------------------------------------------------
# 5.6. Preparation for the median splits (low/high intensity users)
#-------------------------------------------------------------------------------

# Which variables interests me at this point: 

# [1] A scale to assess the SMU-i: The candidate variables: 
COL_COL_P_ID$Score_negative_use_SUM # Score - negative use
COL_COL_P_ID$SMUi_MEAN # Scale Based on Boer et al. 2022

COL_COL_P_ID$AP_Scores # Score - Active Passive (Punished Score)
COL_COL_P_ID$ActPAS_medals # Score - Active Passive (Medals)

COL_COL_P_ID$Freq_Scores # Frequency of use of specific media - punished score
COL_COL_P_ID$Freq_Scores_SUM # Frequency score of specific media - sum

COL_COL_P_ID$VisualFreqSUM # Frequency of use visual media - sum
COL_COL_P_ID$VisualFreq_Punish # Frequency of use visual media - Punished Score


COL_COL1$Use_SNS_1 <- ifelse(COL_COL1$Use_SNS_1=="",NA,COL_COL1$Use_SNS_1)
COL_COL_P_ID$Social_Yes_No <- tapply(COL_COL1$Use_SNS_1, COL_COL1$Exp_Subject_Id, get_word2)
summary(as.factor(COL_COL_P_ID$Social_Yes_No)) # All use social media (at least somehow)


# Passed all tests (Score, AttentionCH1-CH3): 
COL_COL_IP_good <- COL_COL_P_ID[COL_COL_P_ID$TestInterpr=="Good"|COL_COL_P_ID$TestInterpr=="Borderline",]
COL_COL_IP_good <- COL_COL_IP_good[!is.na(COL_COL_IP_good$ATCH2),]


# Now - those who lacks score cannot be split: 
COL_COL_IP_good <- COL_COL_IP_good[!is.na(COL_COL_IP_good$SMUi_MEAN),]

# Adding the rest of the variables for a rater (SMU-i, etc.):
COL_COL_IP_good$Score_negative_use_Mean # Score - negative use
COL_COL_IP_good$SMUi_MEAN # Scale Based on Boer et al. 2022

COL_COL_IP_good$AP_Scores # Score - Active Passive (Punished Score)
COL_COL_IP_good$ActPAS_medals # Score - Active Passive (Medals)

COL_COL_IP_good$Freq_Scores # Frequency of use of specific media - punished score
COL_COL_IP_good$Freq_Scores_SUM # Frequency score of specific media - sum

COL_COL_IP_good$VisualFreqSUM # Frequency of use visual media - sum
COL_COL_IP_good$VisualFreq_Punish # Frequency of use visual media - Punished Score


# No NAs or zeros, let's split (a-b-c): 
SMUi_Mean <- median(COL_COL_IP_good$SMUi_MEAN) # YES (b)

Freq_Scores_median <- median(COL_COL_IP_good$Freq_Scores) # YES (c)

VisualFreq_Punish_median <- median(COL_COL_IP_good$VisualFreq_Punish) # YES (a)



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 5.7. Median splits: Simplified - for the full-size version see some previous versions of this script...
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# How it performs in splitting: 0<,0.5==,>1

#.-.-.-
# 1 Scale Based on Boer et al. 2022 - SCORE (b)
COL_COL_IP_good$SMUi_MEAN_Mean_SPLIT <- ifelse(COL_COL_IP_good$SMUi_MEAN<SMUi_Mean,0,
                                               ifelse(COL_COL_IP_good$SMUi_MEAN==SMUi_Mean,0.5,1))
summary(as.factor(COL_COL_IP_good$SMUi_MEAN_Mean_SPLIT))


#.-.-.-
# 2 Frequency of use of media - per given media (Facebook, Instagram, Twitter, WeChat, TikTok, YouTube + self-reported) - punished score (c)
COL_COL_IP_good$Freq_Scores_SPLIT <- ifelse(COL_COL_IP_good$Freq_Scores<Freq_Scores_median,0,
                                            ifelse(COL_COL_IP_good$Freq_Scores==Freq_Scores_median,0.5,1))
summary(as.factor(COL_COL_IP_good$Freq_Scores_SPLIT))


#.-.-.-
# 3 Frequency of use visual media - Punished Score (YouTube, TikTok, Instagram) - ONE OF THE FOCUS SCORES
COL_COL_IP_good$VisualFreq_Punish_median_SPLIT <- ifelse(COL_COL_IP_good$VisualFreq_Punish<VisualFreq_Punish_median,0,
                                                         ifelse(COL_COL_IP_good$VisualFreq_Punish==VisualFreq_Punish_median,0.5,1))
summary(as.factor(COL_COL_IP_good$VisualFreq_Punish_median_SPLIT))


# How does splitting according to different categories work: 
hist(rowSums(COL_COL_IP_good[,73:75]),breaks=12) # Brute force kinda works...
# Now - I am NOT interested in every variable of those... for the sake of visual diet. 



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 5.8. # Alternative attitude - excluding 25 % in the middle: 
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

COL_COL_IP_good$SMUi_MEAN
COL_COL_IP_good$Freq_Scores
COL_COL_IP_good$VisualFreq_Punish

COL_COL_IP_good$SMUi_Mean_25_percent <- ifelse(COL_COL_IP_good$SMUi_MEAN<=quantile(COL_COL_IP_good$SMUi_MEAN,0.375),"sides",
                                               ifelse(COL_COL_IP_good$SMUi_MEAN>=quantile(COL_COL_IP_good$SMUi_MEAN,0.625),"sides","centre"))
summary(as.factor(COL_COL_IP_good$SMUi_Mean_25_percent))


COL_COL_IP_good$Freq_Scores_25_percent <- ifelse(COL_COL_IP_good$Freq_Scores<=quantile(COL_COL_IP_good$Freq_Scores,0.375),"sides",
                                                 ifelse(COL_COL_IP_good$Freq_Scores>=quantile(COL_COL_IP_good$Freq_Scores,0.625),"sides","centre"))
summary(as.factor(COL_COL_IP_good$Freq_Scores_25_percent))


COL_COL_IP_good$VisualFreqPunish_25_percent <- ifelse(COL_COL_IP_good$VisualFreq_Punish<quantile(COL_COL_IP_good$VisualFreq_Punish,0.375),"sides",
                                                      ifelse(COL_COL_IP_good$VisualFreq_Punish>quantile(COL_COL_IP_good$VisualFreq_Punish,0.625),"sides","centre"))
summary(as.factor(COL_COL_IP_good$VisualFreqPunish_25_percent)) # There is nothing in between 37.5 % and 62.5 % of the distribution.



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 5.9. Splitting (Above, Mediocre, Below)
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Split based on "A" (=VisualFreq_Punish)
#                    if not possible, split based on "B" (=SMUi_MEAN)
#                            if not possible, split based on "C" (=Freq_Scores)
# Otherwise "Mediocre"
COL_COL_IP_good$VisualFreq_Punish_median_SPLIT
COL_COL_IP_good$Split <- ifelse(COL_COL_IP_good$VisualFreq_Punish_median_SPLIT==1,"Above",
                                ifelse(COL_COL_IP_good$VisualFreq_Punish_median_SPLIT==0,"Below","Mediocre"))

# Step 2: For rows where Split is still "Mediocre", check the "SMUi_MEAN" variable
mediocre_indices <- which(COL_COL_IP_good$Split == "Mediocre")
COL_COL_IP_good$Split[mediocre_indices] <- ifelse(COL_COL_IP_good$SMUi_MEAN_Mean_SPLIT[mediocre_indices] == 1, "Above",
                                                  ifelse(COL_COL_IP_good$SMUi_MEAN_Mean_SPLIT[mediocre_indices] == 0, "Below", "Mediocre"))

summary(as.factor(COL_COL_IP_good$Split))



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 6.0 # Adding the ratings.
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# (1) Cut the data.frame based on rater's ID: 
# (2) Get the ratings - if she left some faces not-rated, add NAs (mind the specific length of each sample: 
#     2.1. CZ 2016: 50M + 50F
#     2.2. CZ 2019: 39M + 56F

# Faces: 
COL_COL1$ATR_16F
COL_COL1$ATR_16M
COL_COL1$ATR_19F
COL_COL1$ATR_19M

# How to (re)order it: Spot the variable "Trial ID" and block name:  
# And since you got the IDs (of both photos, raters, and blocks), you can get rid of NAs:
names(COL_COL1[,c(2,6,22:25,45:48,114:117,134)])
COLRAT_List_FACES <- as.data.frame(COL_COL1[,c(2,6,22:25,45:48,114:117,134)]) # Only rating variables + IDs (all of them)

# Adding the order in the table
COLRAT_List_FACES$OrOd <- seq(from=1, to=nrow(COLRAT_List_FACES), by=1)

# Only rows that contain ratings are of interest: 
levels(as.factor(COLRAT_List_FACES$Task_Name))
selected_levs <- c("CZ_16F_ATD","CZ_16M_ATD","CZ_19F_ATD","CZ_19M_ATD")  

COLRAT_List_FACES <- COLRAT_List_FACES[COLRAT_List_FACES$Task_Name %in% selected_levs,]
COLRAT_List_FACES$FullID <- paste(COLRAT_List_FACES$Trial_Id,COLRAT_List_FACES$Task_Name)


# Match the two tables - the one with IDs and the one with ratings: 
COLRAT_List_FACES <- merge(COLRAT_List_FACES, real_IDs, by = "FullID", all = TRUE)

COLRAT_List_FACES <- COLRAT_List_FACES[order(COLRAT_List_FACES$OrOd),]

names(COLRAT_List_FACES)[names(COLRAT_List_FACES) == 'Exp_Subject_Id134'] <- "COL_COL_P_ID"

# These are then the (only) raters you are interested in: 
# Subset the table: CZ_CZ1 - take only those "selected" raters
# Put the attractiveness, trustworthiness, and dominance, that are now split into 
# four columns each into just three columns (one for trustw., one for attractiveness, one for dominance):

COLRAT_List_FACES <- COLRAT_List_FACES[COLRAT_List_FACES$Task_Name %in% selected_levs,]

COLRAT_List_FACES <- merge(COLRAT_List_FACES, COL_COL_IP_good, by = "COL_COL_P_ID", all = F)
COLRAT_List_FACES <- COLRAT_List_FACES[order(COLRAT_List_FACES$OrOd),]

#-------------------------------------------------------------------------------
# 6.2 Finalising, the first step is "there is always just one non-zero number in each quartet
#-------------------------------------------------------------------------------

COLRAT_List_FACES$ATR <- rowSums(COLRAT_List_FACES[,5:8], na.rm = T)
COLRAT_List_FACES$TRU <- rowSums(COLRAT_List_FACES[,13:16], na.rm = T)
COLRAT_List_FACES$DOM <- rowSums(COLRAT_List_FACES[,9:12], na.rm = T)

# The core of the model is like this: 
# I am interested in the effect Sample (country of origin) and SNS use intensity (expressed by whichever variable)
# as Fixed Effects have on rating scale / scales 

COLRAT_List_FACES$ATR
COLRAT_List_FACES$TRU
COLRAT_List_FACES$DOM

COLRAT_List_FACES$Sample <- "COL"


# The simplest way: separate model for each scale of these...
COL_USE_THIS_FOR_LONG_TABLE <- data.frame(Particip_ID = COLRAT_List_FACES[,1], # good
                                          Face_ID = COLRAT_List_FACES[,23], # good
                                          Above_Below = COLRAT_List_FACES[,101], # 
                                          Atr = COLRAT_List_FACES[,102],
                                          Tru = COLRAT_List_FACES[,103],
                                          Dom = COLRAT_List_FACES[,104],
                                          Sample = COLRAT_List_FACES[,105], #Tohle je blbě! 
                                          CentreSide = paste(COLRAT_List_FACES$SMUi_Mean_25_percent,
                                                             COLRAT_List_FACES$Freq_Scores_25_percent,
                                                             COLRAT_List_FACES$VisualFreqPunish_25_percent),
                                          ScoreAbroad = COLRAT_List_FACES[,50],
                                          Fami_Back =  COLRAT_List_FACES[,51]
)

save(COL_USE_THIS_FOR_LONG_TABLE, file="COL_USE_THIS_FOR_LONG_TABLE.Rdata")
load("COL_USE_THIS_FOR_LONG_TABLE.Rdata")
  
# Specify the objects you want to keep
objects_to_keep <- c("CZ_USE_THIS_FOR_LONG_TABLE",
                     "VN_USE_THIS_FOR_LONG_TABLE", 
                     "RSA_USE_THIS_FOR_LONG_TABLE",
                     "AUS_USE_THIS_FOR_LONG_TABLE",
                     "COL_USE_THIS_FOR_LONG_TABLE",
                     "CZ_CZ_IP_good",
                     "VN_VN_IP_good",
                     "RSA_RSA_IP_good",
                     "AUS_AUS_IP_good",
                     "COL_COL_IP_good",
                     "apply_medal", "get_word2", "real_IDs")  # Replace with your object names

# Remove all other objects
rm(list = setdiff(ls(), objects_to_keep))






#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#  Turkish data 
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

TUR_TUR1 <- read.csv("Turkey_25_12_24.csv", T)

# Drop the first one - it's a test...
TUR_TUR1 <- TUR_TUR1[TUR_TUR1$Exp_Subject_Id!="1011185",]
# and 1045523 is debil: 
TUR_TUR1 <- TUR_TUR1[TUR_TUR1$Exp_Subject_Id!="1045523",]


# You are now interested in FACE raters: 
levels(as.factor(TUR_TUR1$Group_Name))
selected_levs <- c("FaceRaters_16A","FaceRaters_16B","FaceRaters_19A","FaceRaters_19B")  
#selected_levs <- c("EyeRaters_16A","EyeRaters_16B","EyeRaters_19A","EyeRaters_19B")  

TUR_TUR1 <- TUR_TUR1[TUR_TUR1$Group_Name %in% selected_levs,]

TUR_TUR_P_ID <- levels(as.factor(TUR_TUR1$Exp_Subject_Id)) # Participants IDs
# Get rid of those who did not proceed: 
TUR_TUR_P_ID<-as.data.frame(TUR_TUR_P_ID)
TUR_TUR_P_ID$lengths <- tapply(TUR_TUR1$Rec_Session_Id, TUR_TUR1$Exp_Subject_Id, length)
hist(TUR_TUR_P_ID$lengths) # Being above 100 means they likely finished the study...


#-------------------------------------------------------------------------------
# 1 Basic demographics 
#-------------------------------------------------------------------------------

# Participant's demographic variables...
# All these variables either are naturally numeric (age...), or were coded as numeric (device)

# AGE: 
TUR_TUR_P_ID$Age <- tapply(TUR_TUR1$SELF_age_1,TUR_TUR1$Exp_Subject_Id,mean, na.rm=T)
TUR_TUR_P_ID$Age[TUR_TUR_P_ID$Age=="NaN"]<-NA

# HEIGHT: 
TUR_TUR_P_ID$Height <- tapply(TUR_TUR1$b_height_F1,TUR_TUR1$Exp_Subject_Id,mean, na.rm=T)
TUR_TUR_P_ID$Height[TUR_TUR_P_ID$Height=="NaN"]<-NA

# WEIGHT: 
TUR_TUR1$b_weight_F1 <- as.numeric(TUR_TUR1$b_weight_F1)
TUR_TUR_P_ID$weight <- tapply(TUR_TUR1$b_weight_F1,TUR_TUR1$Exp_Subject_Id,mean, na.rm=T)
TUR_TUR_P_ID$weight[TUR_TUR_P_ID$weight=="NaN"]<-NA

# MARITAL STATUS: 
TUR_TUR_P_ID$MariStat <- tapply(TUR_TUR1$Marital_status_F1,TUR_TUR1$Exp_Subject_Id,mean, na.rm=T)
TUR_TUR_P_ID$MariStat[TUR_TUR_P_ID$MariStat=="NaN"]<-NA

# Decoded to words: 
TUR_TUR_P_ID$MariStat_VERB <- ifelse(TUR_TUR_P_ID$MariStat==1,"Single",
                                     ifelse(TUR_TUR_P_ID$MariStat==2,"In_a_relationship",
                                            ifelse(TUR_TUR_P_ID$MariStat==3, "Married",
                                                   ifelse(TUR_TUR_P_ID$MariStat==4, "Divorced",
                                                          ifelse(TUR_TUR_P_ID$MariStat==5, "Widowed",
                                                                 ifelse(TUR_TUR_P_ID$MariStat==6, "DontWaDeclare",NA))))))



# MOTHER TONGUE:
TUR_TUR1$Rodny_jazyk <- ifelse(TUR_TUR1$Mother_Tongue_1=="",NA,TUR_TUR1$Mother_Tongue_1)
TUR_TUR_P_ID$MotherTongue <- tapply(TUR_TUR1$Rodny_jazyk, TUR_TUR1$Exp_Subject_Id, get_word2)

# PASSPORT NATIONALITY
TUR_TUR1$Passport.nationality <- ifelse(TUR_TUR1$Passport.nationality=="",NA,TUR_TUR1$Passport.nationality)
TUR_TUR_P_ID$PasspNat <- tapply(TUR_TUR1$Passport.nationality, TUR_TUR1$Exp_Subject_Id, get_word2)

# Ethnicity (definition)
TUR_TUR1$Eth_understanding <- ifelse(TUR_TUR1$Eth_understanding=="",NA,TUR_TUR1$Eth_understanding)
TUR_TUR_P_ID$EthUNDERSTD <- tapply(TUR_TUR1$Eth_understanding, TUR_TUR1$Exp_Subject_Id, get_word2)

# Ethnicity (self-reported)
TUR_TUR1$Self_reported_ethnicity <- ifelse(TUR_TUR1$Self_reported_ethnicity=="",NA,TUR_TUR1$Self_reported_ethnicity)
TUR_TUR_P_ID$Eth_SelfREP <- tapply(TUR_TUR1$Self_reported_ethnicity, TUR_TUR1$Exp_Subject_Id, get_word2)

# Gender (self-reported):
TUR_TUR1$Pohlavi_pri_narozeni <- ifelse(TUR_TUR1$Gender.at.birth_1=="",NA,TUR_TUR1$Gender.at.birth_1)
TUR_TUR_P_ID$SEXATB <- tapply(TUR_TUR1$Pohlavi_pri_narozeni, TUR_TUR1$Exp_Subject_Id, get_word2)


#-------------------------------------------------------------------------------
# 2 Technical variables
#-------------------------------------------------------------------------------

TUR_TUR_P_ID$Technical <- "TECHNICAL"
# Device auto-selected: 
TUR_TUR_P_ID$DEVICE_LABV <- tapply(TUR_TUR1$System_Spec, TUR_TUR1$Exp_Subject_Id, unique)

# Password for Entrance Examination:
TUR_TUR1$Participant_Feedback <- ifelse(TUR_TUR1$Participant_Feedback=="",NA,TUR_TUR1$Participant_Feedback)
TUR_TUR_P_ID$PASSW <- tapply(TUR_TUR1$Participant_Feedback, TUR_TUR1$Exp_Subject_Id, unique)

# Window resolution: 

# WIDTH:
TUR_TUR_P_ID$Window_WWW <- tapply(TUR_TUR1$Window_Width_In_Pixels, TUR_TUR1$Exp_Subject_Id, unique)

# HEIGHT: 
TUR_TUR_P_ID$Window_HHH <- tapply(TUR_TUR1$Window_Height_In_Pixels, TUR_TUR1$Exp_Subject_Id, unique)

# Device screen resolution: 

# WIDTH:
TUR_TUR_P_ID$Screen_WWW <- tapply(TUR_TUR1$Screen_Width_In_Pixels, TUR_TUR1$Exp_Subject_Id, unique)

# HEIGHT: 
TUR_TUR_P_ID$Screen_HHH <- tapply(TUR_TUR1$Screen_Height_In_Pixels, TUR_TUR1$Exp_Subject_Id, unique)


# Selected location (where LabVanced thinks the person is located - may be tricky due to people travelling 
# a lot and VPNs...)
TUR_TUR_P_ID$SelectedLoc <- tapply(TUR_TUR1$SelectedLocation, TUR_TUR1$Exp_Subject_Id, unique)

# Student ID: variable "FIELD1_StudentID"
# Course Code: variable "FIELD2_Course_Code"
TUR_TUR1$FIELD1_StudentID <- ifelse(TUR_TUR1$FIELD1_StudentID=="",NA,TUR_TUR1$FIELD1_StudentID)
TUR_TUR_P_ID$Student_ID <- tapply(TUR_TUR1$FIELD1_StudentID, TUR_TUR1$Exp_Subject_Id, get_word2)

TUR_TUR1$FIELD2_Course_Code <- ifelse(TUR_TUR1$FIELD2_Course_Code=="",NA,TUR_TUR1$FIELD2_Course_Code)
TUR_TUR_P_ID$Course_Code <- tapply(TUR_TUR1$FIELD2_Course_Code, TUR_TUR1$Exp_Subject_Id, get_word2)



#-------------------------------------------------------------------------------
# 3 Was the person conscious (attention checks and score in a test): 
#-------------------------------------------------------------------------------

TUR_TUR_P_ID$Conscious <- "CONSCIOUS"

# Test score: Maximum value for a person in the variable "correct_36" is what you want...
# To get rid of a warning related to NA-only vectors, lets assign -1 to each body who did not bother to start the test:
TUR_TUR1$correct <- ifelse(is.na(TUR_TUR1$correct),-1,TUR_TUR1$correct)

TUR_TUR_P_ID$TestScore <- tapply(as.numeric(TUR_TUR1$correct), TUR_TUR1$Exp_Subject_Id, max, na.rm=T)
TUR_TUR_P_ID$TestInterpr <- ifelse(TUR_TUR_P_ID$TestScore==5|TUR_TUR_P_ID$TestScore==4,"Good",ifelse(TUR_TUR_P_ID$TestScore==3,"Borderline","Exclude!"))

summary(as.factor(TUR_TUR_P_ID$TestInterpr))

# Attention Checks (the kind of variable where there is one word for the vector [OR NA]: 

# 1 (Green/Zeleny)
TUR_TUR1$Attention_Check_Green <- ifelse(TUR_TUR1$Attention_Check_Green=="",NA,TUR_TUR1$Attention_Check_Green)
TUR_TUR_P_ID$ATCH1 <- tapply(TUR_TUR1$Attention_Check_Green, TUR_TUR1$Exp_Subject_Id, get_word2)
summary(as.factor(TUR_TUR_P_ID$ATCH1 )) # One Grizzly's to be excluded! 

# 2 (1968)
TUR_TUR1$Attention_Check_CCCP <- ifelse(TUR_TUR1$Attention_Check_CCCP=="",NA,TUR_TUR1$Attention_Check_CCCP)
TUR_TUR_P_ID$ATCH2 <- tapply(TUR_TUR1$Attention_Check_CCCP, TUR_TUR1$Exp_Subject_Id, get_word2)
summary(as.factor(TUR_TUR_P_ID$ATCH2 ))

# 3 (Motýl/Butterfly)
TUR_TUR1$Attention_Check_Butter <- ifelse(TUR_TUR1$Attention_Check_Butter=="",NA,TUR_TUR1$Attention_Check_Butter)
TUR_TUR_P_ID$ATCH3 <- tapply(TUR_TUR1$Attention_Check_Butter, TUR_TUR1$Exp_Subject_Id, get_word2)
summary(as.factor(TUR_TUR_P_ID$ATCH3))


#-------------------------------------------------------------------------------
# 4 Social background (outside social media): 
#-------------------------------------------------------------------------------

TUR_TUR_P_ID$SocBack <- "SocBack"

# Travel abroad: 
# 1 = Velmi často = Often
# 2 = Spíše často = Rather often
# 3 = Občas = Occasionally
# 4 = Zřídka = Rarely
# 5 = Velmi zřídka = Very rarely
# 6 = Nikdy = never

TUR_TUR1$Travel_Abroad <- ifelse(TUR_TUR1$Travel_Abroad=="",NA,TUR_TUR1$Travel_Abroad)
TUR_TUR_P_ID$ScoreAbroad <- tapply(TUR_TUR1$Travel_Abroad, TUR_TUR1$Exp_Subject_Id, get_word2)
summary(as.factor(TUR_TUR_P_ID$ScoreAbroad))

# Family background: 
# 1 = "Bohaté materiální zabezpečení, peníze jsme nikdy neřešili" = Rich 
# 2 = "Dost peněz, i když jsme se někdy museli uskormnit" = Upper CL
# 3 = "Někdy více, někdy méně, ale dokázali jsme vyžít bez ztráty úrovně" = Middle
# 4 = "Spíše málo peněz, ve srovnání s vrstevníky jsem se musel často uskromnit" = Lower CL 
# 5 = "Neustálé finanční potíže" = Poor
# 6 = "Nepřeji si odpovídat" = No family (the last level is labelled incorrectly, will be corrected)

TUR_TUR1$Family_SOC <- ifelse(TUR_TUR1$Family_SOC=="",NA,TUR_TUR1$Family_SOC)
TUR_TUR_P_ID$Fami_Back <- tapply(TUR_TUR1$Family_SOC, TUR_TUR1$Exp_Subject_Id, get_word2)

# Correcting the level with wrong label: 
TUR_TUR_P_ID$Fami_Back[TUR_TUR_P_ID$Fami_Back=="No family"] <- "PreferNR"
summary(as.factor(TUR_TUR_P_ID$Fami_Back))


#-------------------------------------------------------------------------------
# 5 Social media use... All variables  are coded as numbers...
# 5.1 SNS - general - (c) Boer & Eijinden (2018 2022)
#-------------------------------------------------------------------------------

# How many times a day - check the content: "JakCastoPas1"
# 1 = "Nikdy nebou méně než jednou denně"
# 2 = "1-2krát denně"
# 3 = "3-5krát denně"
# 4 = "6-10krát denně"
# 5 = "11-20krát denně"
# 6 = "21-40krát denně"
# 7 = "Více než 40krát denně"

TUR_TUR1$JakCastoPas <- ifelse(TUR_TUR1$HowOftenPassive=="",NA,TUR_TUR1$HowOftenPassive)
TUR_TUR_P_ID$SMUi_Q1 <- tapply(TUR_TUR1$JakCastoPas, TUR_TUR1$Exp_Subject_Id, get_word2)

# How many times a week - posting: "JakCastoPost1"
# 1 = "Nikdy nebou méně než jednou týdně"
# 2 = "1-2krát týdně"
# 3 = "3-5krát týdně"
# 4 = "6-10krát týdně"
# 5 = "11-20krát týdně"
# 6 = "21-40krát týdně"
# 7 = "Více než 40krát týdně"
TUR_TUR1$JakCastoPost1 <- ifelse(TUR_TUR1$HowOftenPosting=="",NA,TUR_TUR1$HowOftenPosting)
TUR_TUR_P_ID$SMUi_Q2 <- tapply(TUR_TUR1$JakCastoPost1, TUR_TUR1$Exp_Subject_Id, get_word2)

# How many times a week - likes posts of the others: "JakCastoLikes1"
# 1 = "Nikdy nebou méně než jednou týdně"
# 2 = "1-2krát týdně"
# 3 = "3-5krát týdně"
# 4 = "6-10krát týdně"
# 5 = "11-20krát týdně"
# 6 = "21-40krát týdně"
# 7 = "Více než 40krát týdně"
TUR_TUR1$JakCastoLikes1 <- ifelse(TUR_TUR1$HowOftenLiking=="",NA,TUR_TUR1$HowOftenLiking)
TUR_TUR_P_ID$SMUi_Q3 <- tapply(TUR_TUR1$JakCastoLikes1, TUR_TUR1$Exp_Subject_Id, get_word2)

########
#      #
# (B)  # Social Media Use Intensity -> Mean, based on BOER... 
#      #
########

# Will then compute the median value from the three variables' scores: 
TUR_TUR_P_ID$SMUi_MEAN <- apply(TUR_TUR_P_ID[,c(32:34)],1,mean)# The higher the more intensive user...

# Kill the student IDs, these won't be needed...
TUR_TUR_P_ID<-TUR_TUR_P_ID[,c(1:20,23:ncol(TUR_TUR_P_ID))]

#-------------------------------------------------------------------------------
# 5.2 Social media use - negative effects - FOMO, Too Much Time, Miss Others...
#-------------------------------------------------------------------------------

# 1 = zcela souhlasím (Totally agree)
# 2 = spíše souhlasím (rather agree)
# 3 = Ani souhlas ani nesouhlas (neither agree nor disagree)
# 4 = Spíše nesouhlasím (rather disagree)
# 5 = zcela nesouhlasím (Totally disagree)

# SNS_too_much_time:
TUR_TUR1$SNS_too_much_time
TUR_TUR_P_ID$SM_too_much_time <- tapply(TUR_TUR1$SNS_too_much_time, TUR_TUR1$Exp_Subject_Id, get_word2)

# SNS_sometimes_miss_other:
TUR_TUR1$SNS_Sometimes_miss_other
TUR_TUR_P_ID$Miss_other <- tapply(TUR_TUR1$SNS_Sometimes_miss_other, TUR_TUR1$Exp_Subject_Id, get_word2)

# SNS_know_better_than_friends:
TUR_TUR1$SNS_know_better_than_friends
TUR_TUR_P_ID$SM_friends_better <- tapply(TUR_TUR1$SNS_know_better_than_friends, TUR_TUR1$Exp_Subject_Id, get_word2)

# SMP_fear_of_missing_out:
TUR_TUR1$SMP_fear_of_missing_out
TUR_TUR_P_ID$FOMO_SM <- tapply(TUR_TUR1$SMP_fear_of_missing_out, TUR_TUR1$Exp_Subject_Id, get_word2)

# Recalculate so that the higher (up to five), the worse: 
View(data.frame(Orig=TUR_TUR_P_ID$SM_too_much_time,New=6 - TUR_TUR_P_ID$SM_too_much_time))
TUR_TUR_P_ID$SM_too_much_time <- 6 - TUR_TUR_P_ID$SM_too_much_time

TUR_TUR_P_ID$Miss_other <- 6 - TUR_TUR_P_ID$Miss_other

TUR_TUR_P_ID$SM_friends_better <- 6 - TUR_TUR_P_ID$SM_friends_better

TUR_TUR_P_ID$FOMO_SM <- 6 - TUR_TUR_P_ID$FOMO_SM

# The score will be like "The higher, the more intensive user": 
TUR_TUR_P_ID$Score_negative_use_SUM <- rowSums(TUR_TUR_P_ID[,34:37])
TUR_TUR_P_ID$Score_negative_use_Mean <- apply(TUR_TUR_P_ID[,34:37],1,mean)
TUR_TUR_P_ID$Score_negative_use_Mean[is.na(TUR_TUR_P_ID$Score_negative_use_Mean)]<-0


#-------------------------------------------------------------------------------
# 5.3 Which SNS - ACTIVELY, SEMI-ACTIVELY OR PASSIVELY
#-------------------------------------------------------------------------------

# AP - stands for ACTIVE/PASSIVE: 
TUR_TUR_P_ID$ACTPAS<- "ACTPAS"
TUR_TUR_P_ID$Facebook_AP <- tapply(TUR_TUR1$AP_Facebook,TUR_TUR1$Exp_Subject_Id, mean, na.rm=T)
TUR_TUR_P_ID$Facebook_AP[TUR_TUR_P_ID$Facebook_AP=="NaN"]<-NA
TUR_TUR_P_ID$Instagram_AP <- tapply(TUR_TUR1$AP_Instagram,TUR_TUR1$Exp_Subject_Id, mean, na.rm=T)
TUR_TUR_P_ID$Instagram_AP[TUR_TUR_P_ID$Instagram_AP=="NaN"]<-NA

TUR_TUR_P_ID$TikTok_AP <- tapply(TUR_TUR1$AP_TikTok,TUR_TUR1$Exp_Subject_Id, mean, na.rm=T)
TUR_TUR_P_ID$TikTok_AP[TUR_TUR_P_ID$TikTok_AP=="NaN"]<-NA
TUR_TUR_P_ID$WeChat_AP <- tapply(TUR_TUR1$AP_WeChat,TUR_TUR1$Exp_Subject_Id, mean, na.rm=T)
TUR_TUR_P_ID$WeChat_AP[TUR_TUR_P_ID$WeChat_AP=="NaN"]<-NA

TUR_TUR_P_ID$YouTube_AP <- tapply(TUR_TUR1$AP_YouTube,TUR_TUR1$Exp_Subject_Id, mean, na.rm=T)
TUR_TUR_P_ID$YouTube_AP[TUR_TUR_P_ID$YouTube_AP=="NaN"]<-NA
TUR_TUR_P_ID$Twitter_AP <- tapply(TUR_TUR1$AP_X..Twitter.,TUR_TUR1$Exp_Subject_Id, mean, na.rm=T)
TUR_TUR_P_ID$Twitter_AP[TUR_TUR_P_ID$Twitter_AP=="NaN"]<-NA


# A/P - self reported (if any...), should be 3 items...  
# First one - which one (NAME): 
TUR_TUR1$AP_Input.social.media_1 <- ifelse(TUR_TUR1$AP_Input.social.media_1=="",NA,TUR_TUR1$AP_Input.social.media_1)
TUR_TUR_P_ID$SELF_AP_NAME_1 <- tapply(TUR_TUR1$AP_Input.social.media_1, TUR_TUR1$Exp_Subject_Id, get_word2)

# How (active/passive) - yes, 1st self reported SNS is (contrary to the expectation) the name of the variable...
TUR_TUR_P_ID$SELF_REP1_AP <- tapply(TUR_TUR1$X1st.self.reported.SNS,TUR_TUR1$Exp_Subject_Id, mean, na.rm=T)
TUR_TUR_P_ID$SELF_REP1_AP[TUR_TUR_P_ID$SELF_REP1_AP=="NaN"]<-NA

# Second one - which one (NAME): 
TUR_TUR1$AP_Input.social.media.2 <- ifelse(TUR_TUR1$AP_Input.social.media.2=="",NA,TUR_TUR1$AP_Input.social.media.2)
TUR_TUR_P_ID$SELF_AP_NAME_2 <- tapply(TUR_TUR1$AP_Input.social.media.2, TUR_TUR1$Exp_Subject_Id, get_word2)

# How (active/passive) - yes, 1st self reported SNS is (contrary to the expectation) the name of the variable...
TUR_TUR_P_ID$SELF_REP2_AP <- tapply(TUR_TUR1$X2nd.self.reported.SNS,TUR_TUR1$Exp_Subject_Id, mean, na.rm=T)
TUR_TUR_P_ID$SELF_REP2_AP[TUR_TUR_P_ID$SELF_REP2_AP=="NaN"]<-NA

# Third one - which one (NAME): 
TUR_TUR1$AP_Input.socal.media.3 <- ifelse(TUR_TUR1$AP_Input.socal.media.3=="",NA,TUR_TUR1$AP_Input.socal.media.3)
TUR_TUR_P_ID$SELF_AP_NAME_3 <- tapply(TUR_TUR1$AP_Input.socal.media.3, TUR_TUR1$Exp_Subject_Id, get_word2)

# How (active/passive) - yes, 1st self reported SNS is (contrary to the expectation) the name of the variable...
TUR_TUR_P_ID$SELF_REP3_AP <- tapply(TUR_TUR1$X3nd.self.reported.SNS,TUR_TUR1$Exp_Subject_Id, mean, na.rm=T)
TUR_TUR_P_ID$SELF_REP3_AP[TUR_TUR_P_ID$SELF_REP3_AP=="NaN"]<-NA

# Getting rid of bullshits: 
TUR_TUR_P_ID[7,47] <- NA

TUR_TUR_P_ID[49,47] <- NA
TUR_TUR_P_ID[49,48] <- NA

TUR_TUR_P_ID[62,47] <- NA
TUR_TUR_P_ID[62,48] <- NA

TUR_TUR_P_ID[74,47] <- NA
TUR_TUR_P_ID[74,48] <- NA
TUR_TUR_P_ID[74,49] <- NA

TUR_TUR_P_ID[74,50] <- NA
TUR_TUR_P_ID[74,51] <- NA
TUR_TUR_P_ID[74,52] <- NA

TUR_TUR_P_ID[24,52] <- NA

# Recalculating: Now the higher score the less she uses it, maximum is 4: 
TUR_TUR_P_ID$Facebook_AP <- 5 - TUR_TUR_P_ID$Facebook_AP
TUR_TUR_P_ID$Instagram_AP <- 5 - TUR_TUR_P_ID$Instagram_AP

TUR_TUR_P_ID$TikTok_AP <- 5 - TUR_TUR_P_ID$TikTok_AP
TUR_TUR_P_ID$WeChat_AP <- 5 - TUR_TUR_P_ID$WeChat_AP

TUR_TUR_P_ID$YouTube_AP <- 5 - TUR_TUR_P_ID$YouTube_AP
TUR_TUR_P_ID$Twitter_AP <- 5 - TUR_TUR_P_ID$Twitter_AP

TUR_TUR_P_ID$SELF_REP1_AP <- 5 - TUR_TUR_P_ID$SELF_REP1_AP
TUR_TUR_P_ID$SELF_REP2_AP <- 5 - TUR_TUR_P_ID$SELF_REP2_AP
TUR_TUR_P_ID$SELF_REP3_AP <- 5 - TUR_TUR_P_ID$SELF_REP3_AP



# Score 1: A/P punished: 
# max(vector_APs) + 0.75(^1)*max(vector_APs[vector_APs!=max()]) + 0.75(^2)*...

# Put these variables in a vector by rows:
NN <- 90

ActPASS <- matrix(NA, nrow=9,ncol=NN)
colnames(ActPASS) <- (TUR_TUR_P_ID[,1])
AP_Scores <- as.vector(rep(NA,NN))

for (i in 1:nrow(TUR_TUR_P_ID)) {
  ActPASS[,i]<-unlist(TUR_TUR_P_ID[i,c(41:46, 48, 50, 52)])
  ActPASS[,i]<-ifelse(is.na(ActPASS[,i]),0,ActPASS[,i])
}

# Define the punishment factor and the length of the vector to consider
punishment_factor <- 0.75
vect_length <- nrow(ActPASS) 

# Create a vector of the punishment multipliers
multipliers <- punishment_factor^(0:(vect_length-1))

# Calculate scores using vectorized operations: Check whether it takes all the highest possible scores (If there is three times 6,
# does it count each?) 
AP_Scores <- apply(ActPASS, 2, function(TURumn) {
  sorted_TURumn <- sort(TURumn, decreasing = TRUE)
  sum(sorted_TURumn[1:vect_length] * multipliers)
})

TUR_TUR_P_ID$AP_Scores <- AP_Scores


# Get coordinates of "horizontal vectors of medals": 
medals <- c(4,2,1,0)

# Calculate scores using vectorized operations: Check whether it takes all the highest possible scores (If there is three times 6,
# does it count each?) 

Medal_matrix <- apply(ActPASS, c(1, 2), apply_medal)

Medal_sums <- colSums(Medal_matrix)

TUR_TUR_P_ID$ActPAS_medals <- Medal_sums


#-------------------------------------------------------------------------------
# 5.4 Frequency of using of SPECIFIC social media (i.e., one by one):
#-------------------------------------------------------------------------------

TUR_TUR_P_ID$FreqSPEC<- "FreqSPEC"

# How_frequently_preselected: 
# 1 = I don't use it (Síť nepoužívám)
# 2 = Once a week or less (Jednou týdně či méně)
# 3 = Once a week, not daily (Víckrát týdně, ale ne denně)
# 4 = Once a day (Jednou za den)
# 5 = Several times a day (Několikrát denně)
# 6 = Frequently during the day (Často během dne) 

# The higher score the better - no need to recalculate anything

# Facebook = How_frequently_Facebook
TUR_TUR_P_ID$Facebook_Freq <- tapply(TUR_TUR1$How_frequently_Facebook,TUR_TUR1$Exp_Subject_Id, mean, na.rm=T)
TUR_TUR_P_ID$Facebook_Freq[TUR_TUR_P_ID$Facebook_Freq=="NaN"]<-NA

# YouTube = How_frequently_YouTube
TUR_TUR_P_ID$YouTube_Freq <- tapply(TUR_TUR1$How_frequently_YouTube,TUR_TUR1$Exp_Subject_Id, mean, na.rm=T)
TUR_TUR_P_ID$YouTube_Freq[TUR_TUR_P_ID$YouTube_Freq=="NaN"]<-NA

# Instagram = How_frequently_Instagram
TUR_TUR_P_ID$Instagram_Freq <- tapply(TUR_TUR1$How_frequently_Instagram,TUR_TUR1$Exp_Subject_Id, mean, na.rm=T)
TUR_TUR_P_ID$Instagram_Freq[TUR_TUR_P_ID$Instagram_Freq=="NaN"]<-NA

# TikTok = How_frequently_TikTok
TUR_TUR_P_ID$TikTok_Freq <- tapply(TUR_TUR1$How_frequently_TikTok,TUR_TUR1$Exp_Subject_Id, mean, na.rm=T)
TUR_TUR_P_ID$TikTok_Freq[TUR_TUR_P_ID$TikTok_Freq=="NaN"]<-NA


# X[Twitter] = How_frequently_X
TUR_TUR_P_ID$X_Freq <- tapply(TUR_TUR1$How_frequently_X,TUR_TUR1$Exp_Subject_Id, mean, na.rm=T)
TUR_TUR_P_ID$X_Freq[TUR_TUR_P_ID$X_Freq=="NaN"]<-NA

# WeChat = How_Frequently_WECHAT
TUR_TUR_P_ID$WeChat_Freq <- tapply(TUR_TUR1$How_Frequently_WECHAT,TUR_TUR1$Exp_Subject_Id, mean, na.rm=T)
TUR_TUR_P_ID$WeChat_Freq[TUR_TUR_P_ID$WeChat_Freq=="NaN"]<-NA


# Self reported SNS:  (Time = frequency...)

# NAME_1: TIME_Input social media_1
TUR_TUR1$TIME_Input.social.media_1 <- ifelse(TUR_TUR1$TIME_Input.social.media_1=="",NA,TUR_TUR1$TIME_Input.social.media_1)
TUR_TUR_P_ID$Time_SELFREP_Name1 <- tapply(TUR_TUR1$TIME_Input.social.media_1, TUR_TUR1$Exp_Subject_Id, get_word2)

# Frequency_1: TIME_self reported SNS
TUR_TUR_P_ID$Time_SELFREP_Freq1 <- tapply(TUR_TUR1$TIME_self.reported.SNS,TUR_TUR1$Exp_Subject_Id, mean, na.rm=T)
TUR_TUR_P_ID$Time_SELFREP_Freq1[TUR_TUR_P_ID$Time_SELFREP_Freq1=="NaN"]<-NA


# NAME_2: TIME_Input social media_2
TUR_TUR1$TIME_Input.social.media.2 <- ifelse(TUR_TUR1$TIME_Input.social.media.2=="",NA,TUR_TUR1$TIME_Input.social.media.2)
TUR_TUR_P_ID$Time_SELFREP_Name2 <- tapply(TUR_TUR1$TIME_Input.social.media.2, TUR_TUR1$Exp_Subject_Id, get_word2)

# Frequency_2: TIME_2nd self reported SNS
TUR_TUR_P_ID$Time_SELFREP_Freq2 <- tapply(TUR_TUR1$TIME_2nd.self.reported.SNS,TUR_TUR1$Exp_Subject_Id, mean, na.rm=T)
TUR_TUR_P_ID$Time_SELFREP_Freq2[TUR_TUR_P_ID$Time_SELFREP_Freq2=="NaN"]<-NA
TUR_TUR_P_ID$Time_SELFREP_Freq2 <- ifelse(is.na(TUR_TUR_P_ID$Time_SELFREP_Name2), NA, TUR_TUR_P_ID$Time_SELFREP_Freq2 )


# NAME_3: TIME_Input social media_3
TUR_TUR1$TIME_Input.social.media_3 <- ifelse(TUR_TUR1$TIME_Input.social.media_3=="",NA,TUR_TUR1$TIME_Input.social.media_3)
TUR_TUR_P_ID$Time_SELFREP_Name3 <- tapply(TUR_TUR1$TIME_Input.social.media_3, TUR_TUR1$Exp_Subject_Id, get_word2)

# Frequency_3: TIME_3nd self reported SNS
TUR_TUR_P_ID$Time_SELFREP_Freq3 <- tapply(TUR_TUR1$TIME_3nd.self.reported.SNS,TUR_TUR1$Exp_Subject_Id, mean, na.rm=T)
TUR_TUR_P_ID$Time_SELFREP_Freq3[TUR_TUR_P_ID$Time_SELFREP_Freq3=="NaN"]<-NA

# Getting rid of bullshits: 
TUR_TUR_P_ID[74,62] <- NA
TUR_TUR_P_ID[74,63] <- NA
TUR_TUR_P_ID[74,64] <- NA

TUR_TUR_P_ID[74,65] <- NA
TUR_TUR_P_ID[74,66] <- NA
TUR_TUR_P_ID[74,67] <- NA

#------------------

########
#      #
# (C)  # Specific Intensity Use -> Punished Score, one of the six (nine) social media 
#      #
########

# Frequency scores: 
# Score 1: Freq - punished: 

# Put these variables in a vector by rows:
FreqSC<-matrix(NA, nrow=9,ncol=NN)
colnames(FreqSC) <- (TUR_TUR_P_ID[,1])
Freq_Scores <- as.vector(rep(NA,NN)) 

for (i in 1:nrow(TUR_TUR_P_ID)) {
  FreqSC[,i]<-unlist(TUR_TUR_P_ID[i,c(56:61,63,65,67)])
  FreqSC[,i]<-ifelse(is.na(FreqSC[,i]),0,FreqSC[,i])
}

# Define the punishment factor and the length of the vector to consider
punishment_factor <- 0.75
vect_length <- nrow(FreqSC) 

# Create a vector of the punishment multipliers
multipliers <- punishment_factor^(0:(vect_length-1));multipliers

# Calculate scores using vectorized operations
Freq_Scores <- apply(FreqSC, 2, function(column) {
  sorted_column <- sort(column, decreasing = TRUE)
  sum(sorted_column[1:vect_length] * multipliers)
})
Freq_Scores
hist(Freq_Scores) # Normally distributed, except those who are, in fact, NAs

TUR_TUR_P_ID$Freq_Scores <- Freq_Scores
TUR_TUR_P_ID$Freq_Scores_SUM <- rowSums(TUR_TUR_P_ID[,c(56:61,63,65,67)], na.rm=T)



# Other available scores: 
# Frequency of use: General (median score from the three variables) 
TUR_TUR_P_ID$SMUi_MEAN
hist(TUR_TUR_P_ID$SMUi_MEAN, breaks=25)

# Frequency of use: SPECIFIC - the higher the more intensive & on more SMs
TUR_TUR_P_ID$Freq_Scores
hist(TUR_TUR_P_ID$Freq_Scores, breaks=25)

# Actvie/Passive: the higher the more active & more individual SM
TUR_TUR_P_ID$AP_Scores
hist(TUR_TUR_P_ID$AP_Scores, breaks=25)

# Negative use - the higher the worse: 
TUR_TUR_P_ID$Score_negative_use_SUM
hist(TUR_TUR_P_ID$Score_negative_use_SUM, breaks=25)
hist(TUR_TUR_P_ID$Score_negative_use_Mean, breaks=25)

hist(TUR_TUR_P_ID$Freq_Scores_SUM,breaks=25)


# SMU-i - based only on "Visual" social media: TikTok, YouTube, Instagram
TUR_TUR_P_ID$VisualFreqSUM <- rowSums(TUR_TUR_P_ID[,c(57:59)])


########
#      #
# (A)  # Visual International Social Media Use -> Punished Score, one of the three Key Coefficients 
#      #
########

# Punished Score: 

# Put these variables in a vector by rows:
FreqSC<-matrix(NA, nrow=3,ncol=NN)
colnames(FreqSC) <- (TUR_TUR_P_ID[,1])
Freq_Scores <- as.vector(rep(NA,NN))

for (i in 1:nrow(TUR_TUR_P_ID)) {
  FreqSC[,i]<-unlist(TUR_TUR_P_ID[i,c(57:59)])
  FreqSC[,i]<-ifelse(is.na(FreqSC[,i]),0,FreqSC[,i])
}


# Define the punishment factor and the length of the vector to consider
punishment_factor <- 0.75
vect_length <- nrow(FreqSC) 

# Create a vector of the punishment multipliers
multipliers <- punishment_factor^(0:(vect_length-1))

# Calculate scores using vectorized operations
Freq_Scores <- apply(FreqSC, 2, function(column) {
  sorted_column <- sort(column, decreasing = TRUE)
  sum(sorted_column[1:vect_length] * multipliers)
})

TUR_TUR_P_ID$VisualFreq_Punish <- Freq_Scores


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# How related are these scales...
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Create a new data frame -> Add NAs instead of zeros (null score was not possible but when no response was provided):
TUR_MUi <- data.frame(SMUi_MEAN=TUR_TUR_P_ID$SMUi_MEAN,
                      Freq_Scores=TUR_TUR_P_ID$Freq_Scores,
                      Freq_Scores_SUM=TUR_TUR_P_ID$Freq_Scores_SUM,
                      AP_Scores=TUR_TUR_P_ID$AP_Scores,
                      Score_negative_use_SUM=as.numeric(TUR_TUR_P_ID$Score_negative_use_SUM),
                      Score_negative_use_Mean=as.numeric(TUR_TUR_P_ID$Score_negative_use_Mean),
                      VisualFreq_Punish=as.numeric(TUR_TUR_P_ID$VisualFreq_Punish),
                      VisualFreqSUM=as.numeric(TUR_TUR_P_ID$VisualFreqSUM))


TUR_MUi$SMUi_MEAN[TUR_MUi$SMUi_MEAN==0]<-NA
TUR_MUi$Freq_Scores[TUR_MUi$Freq_Scores==0]<-NA
TUR_MUi$AP_Scores[TUR_MUi$AP_Scores==0]<-NA
TUR_MUi$VisualFreq_Punish[TUR_MUi$VisualFreq_Punish==0]<-NA
TUR_MUi$Freq_Scores_SUM[TUR_MUi$Freq_Scores_SUM==0]<-NA

TUR_MUi <- TUR_MUi[!is.na(TUR_MUi$SMUi_MEAN),] # It looks like those to start filling in these scores actually filled in the whole 

rcorr(as.matrix(TUR_MUi))


#-------------------------------------------------------------------------------
# 5.6. Preparation for the median splits (low/high intensity users)
#-------------------------------------------------------------------------------

# Which variables interests me at this point: 

# [1] A scale to assess the SMU-i: The candidate variables: 
TUR_TUR_P_ID$Score_negative_use_SUM # Score - negative use
TUR_TUR_P_ID$SMUi_MEAN # Scale Based on Boer et al. 2022

TUR_TUR_P_ID$AP_Scores # Score - Active Passive (Punished Score)
TUR_TUR_P_ID$ActPAS_medals # Score - Active Passive (Medals)

TUR_TUR_P_ID$Freq_Scores # Frequency of use of specific media - punished score
TUR_TUR_P_ID$Freq_Scores_SUM # Frequency score of specific media - sum

TUR_TUR_P_ID$VisualFreqSUM # Frequency of use visual media - sum
TUR_TUR_P_ID$VisualFreq_Punish # Frequency of use visual media - Punished Score


TUR_TUR1$Use_SNS_1 <- ifelse(TUR_TUR1$Use_SNS_1=="",NA,TUR_TUR1$Use_SNS_1)
TUR_TUR_P_ID$Social_Yes_No <- tapply(TUR_TUR1$Use_SNS_1, TUR_TUR1$Exp_Subject_Id, get_word2)
summary(as.factor(TUR_TUR_P_ID$Social_Yes_No)) # 52 does not use but otherwise finished the survey...
TUR_TUR_P_ID[52,33] <- 0
TUR_TUR_P_ID[52,68]
TUR_TUR_P_ID[52,71]


# Passed all tests (Score, AttentionCH1-CH3): 
TUR_TUR_IP_good <- TUR_TUR_P_ID[TUR_TUR_P_ID$TestInterpr=="Good"|TUR_TUR_P_ID$TestInterpr=="Borderline",]
TUR_TUR_IP_good <- TUR_TUR_IP_good[!is.na(TUR_TUR_IP_good$ATCH2),]


# Now - those who lacks score cannot be split: 
TUR_TUR_IP_good <- TUR_TUR_IP_good[!is.na(TUR_TUR_IP_good$SMUi_MEAN),]

# Adding the rest of the variables for a rater (SMU-i, etc.):
TUR_TUR_IP_good$Score_negative_use_Mean # Score - negative use
TUR_TUR_IP_good$SMUi_MEAN # Scale Based on Boer et al. 2022

TUR_TUR_IP_good$AP_Scores # Score - Active Passive (Punished Score)
TUR_TUR_IP_good$ActPAS_medals # Score - Active Passive (Medals)

TUR_TUR_IP_good$Freq_Scores # Frequency of use of specific media - punished score
TUR_TUR_IP_good$Freq_Scores_SUM # Frequency score of specific media - sum

TUR_TUR_IP_good$VisualFreqSUM # Frequency of use visual media - sum
TUR_TUR_IP_good$VisualFreq_Punish # Frequency of use visual media - Punished Score


# No NAs or zeros, let's split (a-b-c): 
SMUi_Mean <- median(TUR_TUR_IP_good$SMUi_MEAN) # YES (b)

Freq_Scores_median <- median(TUR_TUR_IP_good$Freq_Scores) # YES (c)

VisualFreq_Punish_median <- median(TUR_TUR_IP_good$VisualFreq_Punish) # YES (a)



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 5.7. Median splits: Simplified - for the full-size version see some previous versions of this script...
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# How it performs in splitting: 0<,0.5==,>1

#.-.-.-
# 1 Scale Based on Boer et al. 2022 - SCORE (b)
TUR_TUR_IP_good$SMUi_MEAN_Mean_SPLIT <- ifelse(TUR_TUR_IP_good$SMUi_MEAN<SMUi_Mean,0,
                                               ifelse(TUR_TUR_IP_good$SMUi_MEAN==SMUi_Mean,0.5,1))
summary(as.factor(TUR_TUR_IP_good$SMUi_MEAN_Mean_SPLIT))


#.-.-.-
# 2 Frequency of use of media - per given media (Facebook, Instagram, Twitter, WeChat, TikTok, YouTube + self-reported) - punished score (c)
TUR_TUR_IP_good$Freq_Scores_SPLIT <- ifelse(TUR_TUR_IP_good$Freq_Scores<Freq_Scores_median,0,
                                            ifelse(TUR_TUR_IP_good$Freq_Scores==Freq_Scores_median,0.5,1))
summary(as.factor(TUR_TUR_IP_good$Freq_Scores_SPLIT))


#.-.-.-
# 3 Frequency of use visual media - Punished Score (YouTube, TikTok, Instagram) - ONE OF THE FOCUS SCORES
TUR_TUR_IP_good$VisualFreq_Punish_median_SPLIT <- ifelse(TUR_TUR_IP_good$VisualFreq_Punish<VisualFreq_Punish_median,0,
                                                         ifelse(TUR_TUR_IP_good$VisualFreq_Punish==VisualFreq_Punish_median,0.5,1))
summary(as.factor(TUR_TUR_IP_good$VisualFreq_Punish_median_SPLIT))


# How does splitting according to different categories work: 
hist(rowSums(TUR_TUR_IP_good[,73:75]),breaks=12) # Brute force kinda works...
# Now - I am NOT interested in every variable of those... for the sake of visual diet. 



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 5.8. # Alternative attitude - excluding 25 % in the middle: 
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

TUR_TUR_IP_good$SMUi_MEAN
TUR_TUR_IP_good$Freq_Scores
TUR_TUR_IP_good$VisualFreq_Punish

TUR_TUR_IP_good$SMUi_Mean_25_percent <- ifelse(TUR_TUR_IP_good$SMUi_MEAN<=quantile(TUR_TUR_IP_good$SMUi_MEAN,0.375),"sides",
                                               ifelse(TUR_TUR_IP_good$SMUi_MEAN>=quantile(TUR_TUR_IP_good$SMUi_MEAN,0.625),"sides","centre"))
summary(as.factor(TUR_TUR_IP_good$SMUi_Mean_25_percent))


TUR_TUR_IP_good$Freq_Scores_25_percent <- ifelse(TUR_TUR_IP_good$Freq_Scores<=quantile(TUR_TUR_IP_good$Freq_Scores,0.375),"sides",
                                                 ifelse(TUR_TUR_IP_good$Freq_Scores>=quantile(TUR_TUR_IP_good$Freq_Scores,0.625),"sides","centre"))
summary(as.factor(TUR_TUR_IP_good$Freq_Scores_25_percent))


TUR_TUR_IP_good$VisualFreqPunish_25_percent <- ifelse(TUR_TUR_IP_good$VisualFreq_Punish<quantile(TUR_TUR_IP_good$VisualFreq_Punish,0.375),"sides",
                                                      ifelse(TUR_TUR_IP_good$VisualFreq_Punish>quantile(TUR_TUR_IP_good$VisualFreq_Punish,0.625),"sides","centre"))
summary(as.factor(TUR_TUR_IP_good$VisualFreqPunish_25_percent)) # There is nothing in between 37.5 % and 62.5 % of the distribution.



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 5.9. Splitting (Above, Mediocre, Below)
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Split based on "A" (=VisualFreq_Punish)
#                    if not possible, split based on "B" (=SMUi_MEAN)
#                            if not possible, split based on "C" (=Freq_Scores)
# Otherwise "Mediocre"
TUR_TUR_IP_good$VisualFreq_Punish_median_SPLIT
TUR_TUR_IP_good$Split <- ifelse(TUR_TUR_IP_good$VisualFreq_Punish_median_SPLIT==1,"Above",
                                ifelse(TUR_TUR_IP_good$VisualFreq_Punish_median_SPLIT==0,"Below","Mediocre"))

# Step 2: For rows where Split is still "Mediocre", check the "SMUi_MEAN" variable
mediocre_indices <- which(TUR_TUR_IP_good$Split == "Mediocre")
TUR_TUR_IP_good$Split[mediocre_indices] <- ifelse(TUR_TUR_IP_good$SMUi_MEAN_Mean_SPLIT[mediocre_indices] == 1, "Above",
                                                  ifelse(TUR_TUR_IP_good$SMUi_MEAN_Mean_SPLIT[mediocre_indices] == 0, "Below", "Mediocre"))

summary(as.factor(TUR_TUR_IP_good$Split))



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 6.0 # Adding the ratings.
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# (1) Cut the data.frame based on rater's ID: 
# (2) Get the ratings - if she left some faces not-rated, add NAs (mind the specific length of each sample: 
#     2.1. CZ 2016: 50M + 50F
#     2.2. CZ 2019: 39M + 56F

# Faces: 
TUR_TUR1$ATR_16F
TUR_TUR1$ATR_16M
TUR_TUR1$ATR_19F
TUR_TUR1$ATR_19M

# How to (re)order it: Spot the variable "Trial ID" and block name:  
# And since you got the IDs (of both photos, raters, and blocks), you can get rid of NAs:
names(TUR_TUR1[,c(2,6,22:25,45:48,116:119,136)])
TURRAT_List_FACES <- as.data.frame(TUR_TUR1[,c(2,6,22:25,45:48,116:119,136)]) # Only rating variables + IDs (all of them)

# Adding the order in the table
TURRAT_List_FACES$OrOd <- seq(from=1, to=nrow(TURRAT_List_FACES), by=1)

# Only rows that contain ratings are of interest: 
levels(as.factor(TURRAT_List_FACES$Task_Name))
selected_levs <- c("CZ_16F_ATD","CZ_16M_ATD","CZ_19F_ATD","CZ_19M_ATD")  

TURRAT_List_FACES <- TURRAT_List_FACES[TURRAT_List_FACES$Task_Name %in% selected_levs,]
TURRAT_List_FACES$FullID <- paste(TURRAT_List_FACES$Trial_Id,TURRAT_List_FACES$Task_Name)


# Match the two tables - the one with IDs and the one with ratings: 
TURRAT_List_FACES <- merge(TURRAT_List_FACES, real_IDs, by = "FullID", all = TRUE)

TURRAT_List_FACES <- TURRAT_List_FACES[order(TURRAT_List_FACES$OrOd),]

names(TURRAT_List_FACES)[names(TURRAT_List_FACES) == 'Exp_Subject_Id'] <- "TUR_TUR_P_ID"

# These are then the (only) raters you are interested in: 
# Subset the table: CZ_CZ1 - take only those "selected" raters
# Put the attractiveness, trustworthiness, and dominance, that are now split into 
# four columns each into just three columns (one for trustw., one for attractiveness, one for dominance):

TURRAT_List_FACES <- TURRAT_List_FACES[TURRAT_List_FACES$Task_Name %in% selected_levs,]

TURRAT_List_FACES <- merge(TURRAT_List_FACES, TUR_TUR_IP_good, by = "TUR_TUR_P_ID", all = F)
TURRAT_List_FACES <- TURRAT_List_FACES[order(TURRAT_List_FACES$OrOd),]

#-------------------------------------------------------------------------------
# 6.2 Finalising, the first step is "there is always just one non-zero number in each quartet
#-------------------------------------------------------------------------------

# Every time there's a value in one column (FOR A SET), it is missing in another, see: 
rowSums(TUR_TUR1[,22:25], na.rm = T) # Attractiveness
rowSums(TUR_TUR1[,45:48], na.rm = T) # Dominance
rowSums(TUR_TUR1[,114:117], na.rm = T) # Dominance

TURRAT_List_FACES$ATR <- rowSums(TURRAT_List_FACES[,5:8], na.rm = T)
TURRAT_List_FACES$TRU <- rowSums(TURRAT_List_FACES[,13:16], na.rm = T)
TURRAT_List_FACES$DOM <- rowSums(TURRAT_List_FACES[,9:12], na.rm = T)

TURRAT_List_FACES$ATR
TURRAT_List_FACES$TRU
TURRAT_List_FACES$DOM

TURRAT_List_FACES$Sample <- "TUR"

# The simplest way: separate model for each scale of these...
TUR_USE_THIS_FOR_LONG_TABLE <- data.frame(Particip_ID = TURRAT_List_FACES[,1],
                                          Face_ID = TURRAT_List_FACES[,23],
                                          Above_Below = TURRAT_List_FACES[,101],
                                          Atr = TURRAT_List_FACES[,102],
                                          Tru = TURRAT_List_FACES[,103],
                                          Dom = TURRAT_List_FACES[,104],
                                          Sample = TURRAT_List_FACES[,105],
                                          CentreSide = paste(TURRAT_List_FACES$SMUi_Mean_25_percent,
                                                             TURRAT_List_FACES$Freq_Scores_25_percent,
                                                             TURRAT_List_FACES$VisualFreqPunish_25_percent),
                                          ScoreAbroad = TURRAT_List_FACES[,50],
                                          Fami_Back = TURRAT_List_FACES[,51]
)

save(TUR_USE_THIS_FOR_LONG_TABLE, file="TUR_USE_THIS_FOR_LONG_TABLE.Rdata")

# Specify the objects you want to keep
objects_to_keep <- c("CZ_USE_THIS_FOR_LONG_TABLE",
                     "VN_USE_THIS_FOR_LONG_TABLE", 
                     "RSA_USE_THIS_FOR_LONG_TABLE",
                     "AUS_USE_THIS_FOR_LONG_TABLE",
                     "COL_USE_THIS_FOR_LONG_TABLE",
                     "TUR_USE_THIS_FOR_LONG_TABLE",
                     "CZ_CZ_IP_good",
                     "VN_VN_IP_good",
                     "RSA_RSA_IP_good",
                     "AUS_AUS_IP_good",
                     "COL_COL_IP_good",
                     "TUR_TUR_IP_good",
                     "apply_medal", "get_word2", "real_IDs")  # Replace with your object names

# Remove all other objects
rm(list = setdiff(ls(), objects_to_keep))







AUS_AUS_IP_good$ATCH3 <- NA
AUS_AUS_IP_good <- AUS_AUS_IP_good[,c(1:25,79,26:78)]

RSA_RSA_IP_good$ATCH3 <- NA
RSA_RSA_IP_good <- RSA_RSA_IP_good[,c(1:25,79,26:78)]

CZ_CZ_IP_good$Social_Yes_No <- NA
CZ_CZ_IP_good <- CZ_CZ_IP_good[,c(1:71,79,72:78)]

# LocalFreq: 
colnames(AUS_AUS_IP_good)[61] <- "LocalFreq"
colnames(COL_COL_IP_good)[61] <- "LocalFreq"
colnames(CZ_CZ_IP_good)[61] <- "LocalFreq"
colnames(RSA_RSA_IP_good)[61] <- "LocalFreq"
colnames(TUR_TUR_IP_good)[61] <- "LocalFreq"
colnames(VN_VN_IP_good)[61] <- "LocalFreq"

colnames(AUS_AUS_IP_good)[44] <- "WeCHWappZalo"
colnames(COL_COL_IP_good)[44] <- "WeCHWappZalo"
colnames(CZ_CZ_IP_good)[44] <- "WeCHWappZalo"
colnames(RSA_RSA_IP_good)[44] <- "WeCHWappZalo"
colnames(TUR_TUR_IP_good)[44] <- "WeCHWappZalo"
colnames(VN_VN_IP_good)[44] <- "WeCHWappZalo"

colnames(AUS_AUS_IP_good)[1] <- "ID"
colnames(COL_COL_IP_good)[1] <- "ID"
colnames(CZ_CZ_IP_good)[1] <- "ID"
colnames(RSA_RSA_IP_good)[1] <- "ID"
colnames(TUR_TUR_IP_good)[1] <- "ID"
colnames(VN_VN_IP_good)[1] <- "ID"

# Get the demography together:
DF_colna <- data.frame(AUS79=colnames(AUS_AUS_IP_good),
                       COL79=colnames(COL_COL_IP_good),
                       CZ78=colnames(CZ_CZ_IP_good),
                       RSA79=colnames(RSA_RSA_IP_good),
                       TUR79=colnames(TUR_TUR_IP_good),
                       VN79=colnames(VN_VN_IP_good))

OLDEMOG <- rbind.data.frame(AUS_AUS_IP_good,COL_COL_IP_good,CZ_CZ_IP_good,
                 RSA_RSA_IP_good,TUR_TUR_IP_good,VN_VN_IP_good)

OLDEMOG <- OLDEMOG[,-15]

# NAs should be something else, otherwise, it can cause troubles...
OLDEMOG$ScoreAbroad <- ifelse(is.na(OLDEMOG$ScoreAbroad),"Empty",OLDEMOG$ScoreAbroad)
OLDEMOG$Fami_Back <- ifelse(is.na(OLDEMOG$Fami_Back),"Empty",OLDEMOG$Fami_Back)


OLDEMOG$Cultur <- rep(c("AUSNZ","COL","CZ","RSA","TUR","VN"),times=c(53,16,152,47,79,72))

save(OLDEMOG,file="OLDEMOG.Rdata")
class(OLDEMOG)

OLDEMOG <- as.data.frame(OLDEMOG)
OLDEMOG <- apply(OLDEMOG,2,as.character)

write.table(OLDEMOG, file="Demogr_all.txt")
write.csv2(OLDEMOG, file="Demogr_all.csv") # Rows of NAs mean that the participant do not use social media! 


# Load the data back if needed
load("CZ_USE_THIS_FOR_LONG_TABLE.Rdata")
load("VN_USE_THIS_FOR_LONG_TABLE.Rdata")
load("RSA_USE_THIS_FOR_LONG_TABLE.Rdata")
load("AUS_USE_THIS_FOR_LONG_TABLE.Rdata")
load("COL_USE_THIS_FOR_LONG_TABLE.Rdata")
load("TUR_USE_THIS_FOR_LONG_TABLE.Rdata")

# Put the data together: 
colnames(AUS_USE_THIS_FOR_LONG_TABLE)
colnames(COL_USE_THIS_FOR_LONG_TABLE)
colnames(CZ_USE_THIS_FOR_LONG_TABLE)
colnames(RSA_USE_THIS_FOR_LONG_TABLE)
colnames(TUR_USE_THIS_FOR_LONG_TABLE)
colnames(VN_USE_THIS_FOR_LONG_TABLE) # rbind.data.frame should work...


All_But_OCZ <- rbind.data.frame(AUS_USE_THIS_FOR_LONG_TABLE,COL_USE_THIS_FOR_LONG_TABLE,CZ_USE_THIS_FOR_LONG_TABLE,
                 RSA_USE_THIS_FOR_LONG_TABLE,TUR_USE_THIS_FOR_LONG_TABLE,VN_USE_THIS_FOR_LONG_TABLE)

# Adding original Czech Ratings - we won't use these ratings, but it contains GMM, age, etc. 
CZ_Orig <- read.csv2("CZ_16_19_R89_85.csv",T)

All_But_OCZ$Ord <- seq(from=nrow(CZ_Orig)+1,to=nrow(CZ_Orig)+nrow(All_But_OCZ))

All_But_OCZ <- All_But_OCZ[order(All_But_OCZ$Ord),]

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# EDITS STOP HERE (just call what's below): 

# Assembling the Long format table...


# Get rid of those who recognised a one: 
CZ_Orig <- CZ_Orig[CZ_Orig$Recognise!=1,] # Probably already done...

colnames(CZ_Orig)
head(CZ_Orig)

# Rename and reformat the variables in question: 
# Participant ID - probably enough to ensure it's unique
length(levels(as.factor(CZ_Orig$rater))) # As expected 
CZ_Orig$ID_helper <- ifelse(CZ_Orig$sex=="F" & CZ_Orig$year==2016,"CZ16_F",
                            ifelse(CZ_Orig$sex=="M" & CZ_Orig$year==2016,"CZ16_M",
                                   ifelse(CZ_Orig$sex=="F" & CZ_Orig$year==2019,"CZ19_F","CZ19_M")))

CZ_Orig$FuFa_ID <- paste(CZ_Orig$ID_helper,CZ_Orig$face)
head(CZ_Orig)
# POSEM
CZ_Orig$Sample <- "CZ_orig"

CZ_Orig_INTVAR <- data.frame(Particip_ID = CZ_Orig[,1],
                             Face_ID = CZ_Orig[,19],
                             Above_Below = 0,
                             Atr = CZ_Orig[,7],
                             Tru = CZ_Orig[,6],
                             Dom = NA,
                             Sample = CZ_Orig[,20],
                             CentreSide = NA,
                             ScoreAbroad = NA,
                             Fami_Back = NA,
                             Ord = seq(from=1,to=nrow(CZ_Orig)),
                             Age = CZ_Orig[,9],
                             DIST = CZ_Orig[,11],
                             SShD = CZ_Orig[,12],
                             Asym = CZ_Orig[,13],
                             L = CZ_Orig[,14]
)

#*#

# Adding DIST, FA, SShD, Age, skin L* as predictors in the other rater groups: 
StimDemog <- data.frame(Face_ID = levels(as.factor(CZ_Orig$FuFa_ID)),
                        Age = tapply(as.numeric(CZ_Orig$age),as.factor(CZ_Orig$FuFa_ID),mean),
                        DIST = tapply(as.numeric(CZ_Orig$dist),as.factor(CZ_Orig$FuFa_ID),mean),
                        SShD = tapply(as.numeric(CZ_Orig$SShD),as.factor(CZ_Orig$FuFa_ID),mean),
                        Asym = tapply(as.numeric(CZ_Orig$asym),as.factor(CZ_Orig$FuFa_ID),mean),
                        L = tapply(as.numeric(CZ_Orig$L),as.factor(CZ_Orig$FuFa_ID),mean))

# This is the original Czech rating data, combined with GMM
# Now, let's do the same for all the other rating data...
All_But_OCZ <- merge(All_But_OCZ, StimDemog, by="Face_ID")
head(All_But_OCZ)

colnames(All_But_OCZ)
colnames(CZ_Orig_INTVAR)

###
# Finally putting together, reordering (just for the humanreadability)...
# and saving: 
d <- rbind.data.frame(CZ_Orig_INTVAR,All_But_OCZ)
d <- d[order(d$Ord),]

# A bypass: Let's compare how the ratings go when averaged and compared between cultures
# For the same scale, we should get reasonably high correlation. Otherwise, something may be wrong

# The "Czech control group" will not be used..., let's leave it out for now...
d <- d[d$Above_Below!=0,]

write.csv(d, file="Whole_Dataset_long_GMM_INCLUDED_SIDESCENTRE_INCLUDED_SOCBACK_TRAVELABROAD_INCL_05_05_25.csv") 
write.csv2(d, file="1Whole_Dataset_long_GMM_INCLUDED_SIDESCENTRE_INCLUDED_SOCBACK_TRAVELABROAD_INCL_05_05_25.csv") 


