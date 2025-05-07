#.-.-.-.-.-.-.-.-.-.-.-.
# 
# Travel abroad - participants were splitted in two groups. 
# Participants who reported they travel abroad "Ofter", "Rather Often" and occasionally "Occasionaly"
# Are in one group, participants, who reported they travel abroad "Rarely", "Very rarely" or "Never" (NAs excluded)
# Are in the other.
#
#.-.-.-.-.-.-.-.-.-.-.-.

F25 <- readRDS("post_F_ATD_FAMI_BACK_two_Upper.RDS")

str(F25)

M25 <- readRDS("post_M_ATD_FAMI_BACK_two_Upper.RDS")

str(M25)


# Levels (above means the group that travels more, below means the group that travel less)

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


# Females 

labels_x <- c("AUS/NZE Above", "AUS/NZE Below", "COL Above","COL Below","CZE Above", "CZE Below","ZAF Above", "ZAF Below",
              "TUR Above", "TUR Below", "VNM Above", "VNM Below")
labels_y <- labels_x

str(F25$Rho_FSMUi_A)

library(rethinking)
library(qgraph)
library(bootnet)
library(beepr)
library(ComplexHeatmap)
library(circlize)
library(grid)


# Make it as a 3×1 Matrix
#  ATTRACTIVENESS:F

cor_table_mean <- apply(F25$Rho_FSMUi_A,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(F25$Rho_FSMUi_A,c(2,3),PI, prob=0.95)
cor_table_LOWCI <- cor_table_CI[1,,] 
cor_table_UPWCI <- cor_table_CI[2,,] 

font1 <- 22
font2 <- 28

# I want the coefficient to be printed above the axis, while the CIs should be below it: 
col_fun <- colorRamp2(c(0.3, 1.3/2, 0.9), c("#66e8ff","#70de66", "#ffec19"))
AF <- Heatmap((as.matrix(cor_table_mean)), column_title ="Attractiveness: Females", cluster_rows=FALSE, 
              cluster_columns = FALSE, column_title_gp = gpar(fontsize=font1), column_names_gp = gpar(fontsize = font1), 
              row_names_gp = gpar(fontsize = font1), column_names_rot = 45, column_names_side = "top", name = "r", 
              col = col_fun,
              cell_fun = function(j, i, x, y, width, height, fill) {
                if (j>i) {c(grid.text(sprintf("%.3f", cor_table_mean[i, j]), x, y, gp = gpar(fontsize = font2),just=c(0.50,0.50)))}
                else if (j<i){c(grid.text(sprintf("%.3f", cor_table_LOWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.95,-0.33)),
                                grid.text(sprintf("%.3f", cor_table_UPWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.05,0.92))
                )}
              },rect_gp = gpar(col = "white", lwd = 1), row_title = "95% Compatibility Interval", row_title_gp = gpar(fontsize = 22))


# TRUSTWORTHINESS: F

cor_table_mean <- apply(F25$Rho_FSMUi_T,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(F25$Rho_FSMUi_T,c(2,3),PI, prob=0.95)
cor_table_LOWCI <- cor_table_CI[1,,] 
cor_table_UPWCI <- cor_table_CI[2,,] 


# I want the coefficient to be printed above the axis, while the CIs should be below it: 
col_fun <- colorRamp2(c(0.3, 1.3/2, 0.9), c("#66e8ff","#70de66", "#ffec19"))
AT <- Heatmap((as.matrix(cor_table_mean)), column_title ="Trustworthiness: Females", cluster_rows=FALSE, 
              cluster_columns = FALSE, column_title_gp = gpar(fontsize=font1), column_names_gp = gpar(fontsize = font1), 
              row_names_gp = gpar(fontsize = font1), column_names_rot = 45, column_names_side = "top", name = "r", 
              col = col_fun,
              cell_fun = function(j, i, x, y, width, height, fill) {
                if (j>i) {c(grid.text(sprintf("%.3f", cor_table_mean[i, j]), x, y, gp = gpar(fontsize = font2),just=c(0.50,0.50)))}
                else if (j<i){c(grid.text(sprintf("%.3f", cor_table_LOWCI[i, j]), x, y, gp = gpar(fontsize = font1),just = c(0.95,-0.33)),
                                grid.text(sprintf("%.3f", cor_table_UPWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.05,0.92))
                )}
              },rect_gp = gpar(col = "white", lwd = 1), row_title = "95% Compatibility Interval", row_title_gp = gpar(fontsize = 22))


# Dominance: F

cor_table_mean <- apply(F25$Rho_FSMUi_D,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(F25$Rho_FSMUi_D,c(2,3),PI, prob=0.95)
cor_table_LOWCI <- cor_table_CI[1,,] 
cor_table_UPWCI <- cor_table_CI[2,,] 


# I want the coefficient to be printed above the axis, while the CIs should be below it: 
col_fun <- colorRamp2(c(0.3, 1.3/2, 0.9), c("#66e8ff","#70de66", "#ffec19"))
AD <- Heatmap((as.matrix(cor_table_mean)), column_title ="Dominance: Females", cluster_rows=FALSE, 
              cluster_columns = FALSE, column_title_gp = gpar(fontsize=font1), column_names_gp = gpar(fontsize = font1), 
              row_names_gp = gpar(fontsize = font1), column_names_rot = 45, column_names_side = "top", name = "r", 
              col = col_fun,
              cell_fun = function(j, i, x, y, width, height, fill) {
                if (j>i) {c(grid.text(sprintf("%.3f", cor_table_mean[i, j]), x, y, gp = gpar(fontsize = font2),just=c(0.50,0.50)))}
                else if (j<i){c(grid.text(sprintf("%.3f", cor_table_LOWCI[i, j]), x, y, gp = gpar(fontsize = font1),just = c(0.95,-0.33)),
                                grid.text(sprintf("%.3f", cor_table_UPWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.05,0.92))
                )}
              },rect_gp = gpar(col = "white", lwd = 1), row_title = "95% Compatibility Interval", row_title_gp = gpar(fontsize = 22))



tiff("Correlations_Heatmap_A_Females_Attr_Above_Below_SES_ver_1.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AF
dev.off()

tiff("Correlations_Heatmap_T_Females_Tru_Above_Below_SES_ver_1.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AT
dev.off()

tiff("Correlations_Heatmap_D_Females_Dom_Above_Below_SES_ver_1.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AD
dev.off()



# Males

# Make it as a 3×1 Matrix
#  ATTRACTIVENESS:M

cor_table_mean <- apply(M25$Rho_FSMUi_A,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(M25$Rho_FSMUi_A,c(2,3),PI, prob=0.95)
cor_table_LOWCI <- cor_table_CI[1,,] 
cor_table_UPWCI <- cor_table_CI[2,,] 

font1 <- 22
font2 <- 28

# I want the coefficient to be printed above the axis, while the CIs should be below it: 
col_fun <- colorRamp2(c(0.3, 1.3/2, 0.9), c("#66e8ff","#70de66", "#ffec19"))
AF <- Heatmap((as.matrix(cor_table_mean)), column_title ="Attractiveness: Males", cluster_rows=FALSE, 
              cluster_columns = FALSE, column_title_gp = gpar(fontsize=font1), column_names_gp = gpar(fontsize = font1), 
              row_names_gp = gpar(fontsize = font1), column_names_rot = 45, column_names_side = "top", name = "r", 
              col = col_fun,
              cell_fun = function(j, i, x, y, width, height, fill) {
                if (j>i) {c(grid.text(sprintf("%.3f", cor_table_mean[i, j]), x, y, gp = gpar(fontsize = font2),just=c(0.50,0.50)))}
                else if (j<i){c(grid.text(sprintf("%.3f", cor_table_LOWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.95,-0.33)),
                                grid.text(sprintf("%.3f", cor_table_UPWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.05,0.92))
                )}
              },rect_gp = gpar(col = "white", lwd = 1), row_title = "95% Compatibility Interval", row_title_gp = gpar(fontsize = 22))


# TRUSTWORTHINESS: M

cor_table_mean <- apply(M25$Rho_FSMUi_T,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(M25$Rho_FSMUi_T,c(2,3),PI, prob=0.95)
cor_table_LOWCI <- cor_table_CI[1,,] 
cor_table_UPWCI <- cor_table_CI[2,,] 


# I want the coefficient to be printed above the axis, while the CIs should be below it: 
col_fun <- colorRamp2(c(0.3, 1.3/2, 0.9), c("#66e8ff","#70de66", "#ffec19"))
AT <- Heatmap((as.matrix(cor_table_mean)), column_title ="Trustworthiness: Males", cluster_rows=FALSE, 
              cluster_columns = FALSE, column_title_gp = gpar(fontsize=font1), column_names_gp = gpar(fontsize = font1), 
              row_names_gp = gpar(fontsize = font1), column_names_rot = 45, column_names_side = "top", name = "r", 
              col = col_fun,
              cell_fun = function(j, i, x, y, width, height, fill) {
                if (j>i) {c(grid.text(sprintf("%.3f", cor_table_mean[i, j]), x, y, gp = gpar(fontsize = font2),just=c(0.50,0.50)))}
                else if (j<i){c(grid.text(sprintf("%.3f", cor_table_LOWCI[i, j]), x, y, gp = gpar(fontsize = font1),just = c(0.95,-0.33)),
                                grid.text(sprintf("%.3f", cor_table_UPWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.05,0.92))
                )}
              },rect_gp = gpar(col = "white", lwd = 1), row_title = "95% Compatibility Interval", row_title_gp = gpar(fontsize = 22))


# Dominance: M

cor_table_mean <- apply(M25$Rho_FSMUi_D,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(M25$Rho_FSMUi_D,c(2,3),PI, prob=0.95)
cor_table_LOWCI <- cor_table_CI[1,,] 
cor_table_UPWCI <- cor_table_CI[2,,] 


# I want the coefficient to be printed above the axis, while the CIs should be below it: 
col_fun <- colorRamp2(c(0.3, 1.3/2, 0.9), c("#66e8ff","#70de66", "#ffec19"))
AD <- Heatmap((as.matrix(cor_table_mean)), column_title ="Dominance: Males", cluster_rows=FALSE, 
              cluster_columns = FALSE, column_title_gp = gpar(fontsize=font1), column_names_gp = gpar(fontsize = font1), 
              row_names_gp = gpar(fontsize = font1), column_names_rot = 45, column_names_side = "top", name = "r", 
              col = col_fun,
              cell_fun = function(j, i, x, y, width, height, fill) {
                if (j>i) {c(grid.text(sprintf("%.3f", cor_table_mean[i, j]), x, y, gp = gpar(fontsize = font2),just=c(0.50,0.50)))}
                else if (j<i){c(grid.text(sprintf("%.3f", cor_table_LOWCI[i, j]), x, y, gp = gpar(fontsize = font1),just = c(0.95,-0.33)),
                                grid.text(sprintf("%.3f", cor_table_UPWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.05,0.92))
                )}
              },rect_gp = gpar(col = "white", lwd = 1), row_title = "95% Compatibility Interval", row_title_gp = gpar(fontsize = 22))

tiff("Correlations_Heatmap_A_Males_Attr_Above_Below_SES_ver_1.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AF
dev.off()

tiff("Correlations_Heatmap_T_Males_Tru_Above_Below_SES_ver_1.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AT
dev.off()

tiff("Correlations_Heatmap_D_Males_Dom_Above_Below_SES_ver_1.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AD
dev.off()





#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Figure S18 - analogue of the Figure 3 in the main body of the article, which is a figure (top) presenting mean estimates
# in every sample, (bottom) presenting effects of morphometric and demographic predictors: SPECIFICALLY for WOMEN
# Version without labels

# Upper part: Frequent - all
# Compute the average of all group estimates

# Note - these are the same data as those to enter the A-T-D model; therefore, 
# the order of levels is the same, too. 

Avg_Freq_U_A = ( 
  F25$aA + F25$f_per_group_pr_A[,1,1] + 
    F25$aA + F25$f_per_group_pr_A[,3,1] + 
    F25$aA + F25$f_per_group_pr_A[,5,1] + 
    F25$aA + F25$f_per_group_pr_A[,7,1] + 
    F25$aA + F25$f_per_group_pr_A[,9,1] + 
    F25$aA + F25$f_per_group_pr_A[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_A = ( 
  F25$aA + F25$f_per_group_pr_A[,2,1] + 
    F25$aA + F25$f_per_group_pr_A[,4,1] + 
    F25$aA + F25$f_per_group_pr_A[,6,1] + 
    F25$aA + F25$f_per_group_pr_A[,8,1] + 
    F25$aA + F25$f_per_group_pr_A[,10,1] + 
    F25$aA + F25$f_per_group_pr_A[,12,1] 
) / 6



F_posts_ATR <- list(
  # 1 Intercept: 
  Intercept = F25$aA,
  # Infrequent users
  Avg_Freq_U_A,
  # Frequent users 
  Avg_Infreq_U_A,
  # Difference 
  Difference = Avg_Freq_U_A - Avg_Infreq_U_A,
  # 4 Australia - above - median users  
  AUS_Intensive_User = F25$aA + F25$f_per_group_pr_A[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = F25$aA + F25$f_per_group_pr_A[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = F25$aA + F25$f_per_group_pr_A[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = F25$aA + F25$f_per_group_pr_A[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = F25$aA + F25$f_per_group_pr_A[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = F25$aA + F25$f_per_group_pr_A[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = F25$aA + F25$f_per_group_pr_A[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = F25$aA + F25$f_per_group_pr_A[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = F25$aA + F25$f_per_group_pr_A[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = F25$aA + F25$f_per_group_pr_A[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = F25$aA + F25$f_per_group_pr_A[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = F25$aA + F25$f_per_group_pr_A[,12,1]
)

summary.data.frame(F_posts_ATR)
str(F_posts_ATR) # Need 13 rows 

#--------------

# Trustworthiness
Avg_Freq_U_T = ( 
  F25$aT + F25$f_per_group_pr_T[,1,1] + 
    F25$aT + F25$f_per_group_pr_T[,3,1] + 
    F25$aT + F25$f_per_group_pr_T[,5,1] + 
    F25$aT + F25$f_per_group_pr_T[,7,1] + 
    F25$aT + F25$f_per_group_pr_T[,9,1] + 
    F25$aT + F25$f_per_group_pr_T[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_T = ( 
  F25$aT + F25$f_per_group_pr_T[,2,1] + 
    F25$aT + F25$f_per_group_pr_T[,4,1] + 
    F25$aT + F25$f_per_group_pr_T[,6,1] + 
    F25$aT + F25$f_per_group_pr_T[,8,1] + 
    F25$aT + F25$f_per_group_pr_T[,10,1] + 
    F25$aT + F25$f_per_group_pr_T[,12,1] 
) / 6

F_posts_TRU <- list(
  # 1 Intercept: 
  Intercept = F25$aT,
  # Infrequent users
  Avg_Freq_U_T,
  # Frequent users 
  Avg_Infreq_U_T,
  # Difference 
  Difference = Avg_Freq_U_T - Avg_Infreq_U_T,
  # 4 Australia - above - median users  
  AUS_Intensive_User = F25$aT + F25$f_per_group_pr_T[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = F25$aT + F25$f_per_group_pr_T[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = F25$aT + F25$f_per_group_pr_T[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = F25$aT + F25$f_per_group_pr_T[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = F25$aT + F25$f_per_group_pr_T[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = F25$aT + F25$f_per_group_pr_T[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = F25$aT + F25$f_per_group_pr_T[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = F25$aT + F25$f_per_group_pr_T[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = F25$aT + F25$f_per_group_pr_T[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = F25$aT + F25$f_per_group_pr_T[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = F25$aT + F25$f_per_group_pr_T[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = F25$aT + F25$f_per_group_pr_T[,12,1]
)

summary.data.frame(F_posts_TRU)
str(F_posts_TRU) # Need 13 rows 

#--------------

# Dominance
Avg_Freq_U_D = ( 
  F25$aD + F25$f_per_group_pr_D[,1,1] + 
    F25$aD + F25$f_per_group_pr_D[,3,1] + 
    F25$aD + F25$f_per_group_pr_D[,5,1] + 
    F25$aD + F25$f_per_group_pr_D[,7,1] + 
    F25$aD + F25$f_per_group_pr_D[,9,1] + 
    F25$aD + F25$f_per_group_pr_D[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_D = ( 
  F25$aD + F25$f_per_group_pr_D[,2,1] + 
    F25$aD + F25$f_per_group_pr_D[,4,1] + 
    F25$aD + F25$f_per_group_pr_D[,6,1] + 
    F25$aD + F25$f_per_group_pr_D[,8,1] + 
    F25$aD + F25$f_per_group_pr_D[,10,1] + 
    F25$aD + F25$f_per_group_pr_D[,12,1] 
) / 6

F_posts_DOM <- list(
  # 1 Intercept: 
  Intercept = F25$aD,
  # Infrequent users
  Avg_Freq_U_D,
  # Frequent users 
  Avg_Infreq_U_D,
  # Difference 
  Difference = Avg_Freq_U_D - Avg_Infreq_U_D,
  # 4 Australia - above - median users  
  AUS_Intensive_User = F25$aD + F25$f_per_group_pr_D[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = F25$aD + F25$f_per_group_pr_D[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = F25$aD + F25$f_per_group_pr_D[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = F25$aD + F25$f_per_group_pr_D[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = F25$aD + F25$f_per_group_pr_D[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = F25$aD + F25$f_per_group_pr_D[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = F25$aD + F25$f_per_group_pr_D[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = F25$aD + F25$f_per_group_pr_D[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = F25$aD + F25$f_per_group_pr_D[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = F25$aD + F25$f_per_group_pr_D[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = F25$aD + F25$f_per_group_pr_D[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = F25$aD + F25$f_per_group_pr_D[,12,1]
)

summary.data.frame(F_posts_DOM)
str(F_posts_DOM) 



# PLOTTING: 
cols<-c("#00FBFF", 
        "#8C00FF",
        "#FFADD0",
        "#09C9FF",
        
        "#00FF3380",
        "#0FEA9980",
        
        "#FFEA0080",
        "#c9b80080",
        
        "#e0935c80",
        "#ff7b1c80",
        
        "#4796ff80",
        "#1278ff80",
        
        "#acdb5a80",
        "#a1f21380",
        
        "#6174ed80",
        "#223ef080"
) 



labs<-c("Intercept",
        "Above",
        "Below",
        "Above - Below: Difference",
        "Australia/NewZ - Above",
        "Australia/NewZ - Below",
        "Colombia - Above",
        "Colombia - Below",
        "Czechia - Above",
        "Czechia - Below",
        "South Africa - Above",
        "South Africa - Below",
        "Turkey - Above",
        "Turkey - Below",
        "Vietnam - Above",
        "Vietnam - Below"
)


# Figure S18:Upper Panel - Analogue of Figure 3:Upper Panel (final assembly done in InkScape)...

tiff("F_ATD_SES_ver_1_LEVELS_NOLABELS.tiff",width=30,height=14,units="cm",res=600,compression="lzw")
layout(matrix(1:3,byrow=F,nrow=1), widths=c(1.8,1.2,1.2),heights=c(1))

# ATTRACTIVENESS

par(mar=c(3.1, 15.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-1,0.7),ylim=c(1,17),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Attractiveness - Group Estimates", side = 3, line = -0.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

rect(-1,0.5,0.75,17, col=F, border = "#b9b9B999", lwd = 2)
rect(-0.95,12.5,0.7,16.5, col="#F0F0F0", border = F)

segments(x0 = -1, y0 = 1:16, x1 = 0.75, y1 = 1:16, col = "#40808080", lwd = 1.5)
segments(x0 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y0 = 0.5, x1 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y1 = 16.5, col="#80808080",lty=2)

ys<-seq(from=1,to=16, by=1)
ys<-rev(ys)

cex_t <- 1.25
cex_t2 <- 0.95
pls <- 0.75


for(i in 1:length(F_posts_ATR)){ 
  toplot<-F_posts_ATR[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  textlab<-labs[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  axis(2, at=ycoord, labels=textlab, srt=45, las=2, cex.axis=1.2, tick=F)
  for (i in 1:16) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.007)+ycoord[i],border=T, lwd = 1, col=colpol))
  }
  #lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0 = 0, y0 = 0, x1 = 0, y1 = 16.5, col = "#999999", lwd = 2, lty = 1)



# TRUSTWORTHINESS
par(mar=c(3.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-1,0.7),ylim=c(1,17),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Trustworthiness - Group Estimates", side = 3, line = -0.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

rect(-1,0.5,0.75,17, col=F, border = "#b9b9B999", lwd = 2)
rect(-0.95,12.5,0.7,16.5, col="#F0F0F0", border = F)

segments(x0 = -1, y0 = 1:16, x1 = 0.75, y1 = 1:16, col = "#40808080", lwd = 1.5)
segments(x0 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y0 = 0.5, x1 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y1 = 16.5, col="#80808080",lty=2)

ys<-seq(from=1,to=16, by=1)
ys<-rev(ys)

for(i in 1:length(F_posts_TRU)){ 
  toplot<-F_posts_TRU[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  for (i in 1:16) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.007)+ycoord[i],border=T, lwd = 1, col=colpol))
  }
  #lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0 = 0, y0 = 0, x1 = 0, y1 = 16.5, col = "#999999", lwd = 2, lty = 1)




# DOMINANCE
par(mar=c(3.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-1,0.7),ylim=c(1,17),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Dominance - Group Estimates", side = 3, line = -0.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

rect(-1,0.5,0.75,17, col=F, border = "#b9b9B999", lwd = 2)
rect(-0.95,12.5,0.7,16.5, col="#F0F0F0", border = F)

segments(x0 = -1, y0 = 1:16, x1 = 0.75, y1 = 1:16, col = "#40808080", lwd = 1.5)
segments(x0 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y0 = 0.5, x1 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y1 = 16.5, col="#80808080",lty=2)

ys<-seq(from=1,to=16, by=1)
ys<-rev(ys)


for(i in 1:length(F_posts_DOM)){ 
  toplot<-F_posts_DOM[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  for (i in 1:16) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.007)+ycoord[i],border=T, lwd = 1, col=colpol))
  }
  #lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0 = 0, y0 = 0, x1 = 0, y1 = 16.5, col = "#999999", lwd = 2, lty = 1)


dev.off()


#---------------------------------

# And now GMM: 


# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS


# Preparing composite coefficients: 
# AGE:
Attr_Age_F_freq <- c(
  (F25$b_age_A + F25$f_per_group_pr_A[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (F25$b_age_A + F25$f_per_group_pr_A[,3,2])+   # Age (A): COL More Freq. Users
    (F25$b_age_A + F25$f_per_group_pr_A[,5,2])+   # Age (A): CZE More Freq. Users
    (F25$b_age_A + F25$f_per_group_pr_A[,7,2])+   # Age (A): ZAF More Freq. Users
    (F25$b_age_A + F25$f_per_group_pr_A[,9,2])+   # Age (A): TUR More Freq. Users
    (F25$b_age_A + F25$f_per_group_pr_A[,11,2])   # Age (A): COL More Freq. Users
)/6

Attr_Age_F_INfreq <- (
  (F25$b_age_A + F25$f_per_group_pr_A[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (F25$b_age_A + F25$f_per_group_pr_A[,4,2])+   # Age (A): COL More Freq. Users
    (F25$b_age_A + F25$f_per_group_pr_A[,6,2])+   # Age (A): CZE More Freq. Users
    (F25$b_age_A + F25$f_per_group_pr_A[,8,2])+   # Age (A): ZAF More Freq. Users
    (F25$b_age_A + F25$f_per_group_pr_A[,10,2])+   # Age (A): TUR More Freq. Users
    (F25$b_age_A + F25$f_per_group_pr_A[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
Attr_Dist_F_freq <- (
  (F25$b_dist_A + F25$f_per_group_pr_A[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (F25$b_dist_A + F25$f_per_group_pr_A[,3,3])+   # Dist (A): COL More Freq. Users
    (F25$b_dist_A + F25$f_per_group_pr_A[,5,3])+   # Dist (A): CZE More Freq. Users
    (F25$b_dist_A + F25$f_per_group_pr_A[,7,3])+   # Dist (A): ZAF More Freq. Users
    (F25$b_dist_A + F25$f_per_group_pr_A[,9,3])+   # Dist (A): TUR More Freq. Users
    (F25$b_dist_A + F25$f_per_group_pr_A[,11,3])   # Dist (A): COL More Freq. Users
)/6

Attr_Dist_F_INfreq <- (
  (F25$b_dist_A + F25$f_per_group_pr_A[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (F25$b_dist_A + F25$f_per_group_pr_A[,4,3])+   # Dist (A): COL More Freq. Users
    (F25$b_dist_A + F25$f_per_group_pr_A[,6,3])+   # Dist (A): CZE More Freq. Users
    (F25$b_dist_A + F25$f_per_group_pr_A[,8,3])+   # Dist (A): ZAF More Freq. Users
    (F25$b_dist_A + F25$f_per_group_pr_A[,10,3])+   # Dist (A): TUR More Freq. Users
    (F25$b_dist_A + F25$f_per_group_pr_A[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
Attr_Asym_F_freq <- (
  (F25$b_FA_A + F25$f_per_group_pr_A[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (F25$b_FA_A + F25$f_per_group_pr_A[,3,4])+   # Asym (A): COL More Freq. Users
    (F25$b_FA_A + F25$f_per_group_pr_A[,5,4])+   # Asym (A): CZE More Freq. Users
    (F25$b_FA_A + F25$f_per_group_pr_A[,7,4])+   # Asym (A): ZAF More Freq. Users
    (F25$b_FA_A + F25$f_per_group_pr_A[,9,4])+   # Asym (A): TUR More Freq. Users
    (F25$b_FA_A + F25$f_per_group_pr_A[,11,4])   # Asym (A): COL More Freq. Users
)/6

Attr_Asym_F_INfreq <- (
  (F25$b_FA_A + F25$f_per_group_pr_A[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (F25$b_FA_A + F25$f_per_group_pr_A[,4,4])+   # Asym (A): COL More Freq. Users
    (F25$b_FA_A + F25$f_per_group_pr_A[,6,4])+   # Asym (A): CZE More Freq. Users
    (F25$b_FA_A + F25$f_per_group_pr_A[,8,4])+   # Asym (A): ZAF More Freq. Users
    (F25$b_FA_A + F25$f_per_group_pr_A[,10,4])+   # Asym (A): TUR More Freq. Users
    (F25$b_FA_A + F25$f_per_group_pr_A[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
Attr_SexTyp_F_freq <- (
  (F25$b_sshd_A + F25$f_per_group_pr_A[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (F25$b_sshd_A + F25$f_per_group_pr_A[,3,5])+   # SShD (A): COL More Freq. Users
    (F25$b_sshd_A + F25$f_per_group_pr_A[,5,5])+   # SShD (A): CZE More Freq. Users
    (F25$b_sshd_A + F25$f_per_group_pr_A[,7,5])+   # SShD (A): ZAF More Freq. Users
    (F25$b_sshd_A + F25$f_per_group_pr_A[,9,5])+   # SShD (A): TUR More Freq. Users
    (F25$b_sshd_A + F25$f_per_group_pr_A[,11,5])   # SShD (A): COL More Freq. Users
)/6

Attr_SexTyp_F_INfreq <- (
  (F25$b_sshd_A + F25$f_per_group_pr_A[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (F25$b_sshd_A + F25$f_per_group_pr_A[,4,5])+   # SShD (A): COL More Freq. Users
    (F25$b_sshd_A + F25$f_per_group_pr_A[,6,5])+   # SShD (A): CZE More Freq. Users
    (F25$b_sshd_A + F25$f_per_group_pr_A[,8,5])+   # SShD (A): ZAF More Freq. Users
    (F25$b_sshd_A + F25$f_per_group_pr_A[,10,5])+   # SShD (A): TUR More Freq. Users
    (F25$b_sshd_A + F25$f_per_group_pr_A[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
Attr_L_F_freq <- (
  (F25$b_L_A + F25$f_per_group_pr_A[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (F25$b_L_A + F25$f_per_group_pr_A[,3,6])+   # L (A): COL More Freq. Users
    (F25$b_L_A + F25$f_per_group_pr_A[,5,6])+   # L (A): CZE More Freq. Users
    (F25$b_L_A + F25$f_per_group_pr_A[,7,6])+   # L (A): ZAF More Freq. Users
    (F25$b_L_A + F25$f_per_group_pr_A[,9,6])+   # L (A): TUR More Freq. Users
    (F25$b_L_A + F25$f_per_group_pr_A[,11,6])   # L (A): COL More Freq. Users
)/6

Attr_L_F_INfreq <- (
  (F25$b_L_A + F25$f_per_group_pr_A[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (F25$b_L_A + F25$f_per_group_pr_A[,4,6])+   # L (A): COL More Freq. Users
    (F25$b_L_A + F25$f_per_group_pr_A[,6,6])+   # L (A): CZE More Freq. Users
    (F25$b_L_A + F25$f_per_group_pr_A[,8,6])+   # L (A): ZAF More Freq. Users
    (F25$b_L_A + F25$f_per_group_pr_A[,10,6])+   # L (A): TUR More Freq. Users
    (F25$b_L_A + F25$f_per_group_pr_A[,12,6])   # L (A): COL More Freq. Users
)/6



# GMM - SEPARATELY 

# Attractiveness - GMM 
str(F25)
str(F25$f_per_group_pr_A) # Mind that 1 is intercept! 

F_posts_ATD_MM_Attr <- list(
  #  AGE
  Age_Slope = (F25$b_age_A),    # Slope prior to accessing culture
  
  Attr_Age_F_freq,
  Attr_Age_F_INfreq,
  
  Age_AUS_Freq = (F25$b_age_A + F25$f_per_group_pr_A[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (F25$b_age_A + F25$f_per_group_pr_A[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (F25$b_age_A + F25$f_per_group_pr_A[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (F25$b_age_A + F25$f_per_group_pr_A[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (F25$b_age_A + F25$f_per_group_pr_A[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (F25$b_age_A + F25$f_per_group_pr_A[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (F25$b_age_A + F25$f_per_group_pr_A[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (F25$b_age_A + F25$f_per_group_pr_A[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (F25$b_age_A + F25$f_per_group_pr_A[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (F25$b_age_A + F25$f_per_group_pr_A[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (F25$b_age_A + F25$f_per_group_pr_A[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (F25$b_age_A + F25$f_per_group_pr_A[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (F25$b_dist_A),    # Dist (A): CZE Control
  
  Attr_Dist_F_freq,
  Attr_Dist_F_INfreq,
  
  Dist_AUS_Freq = (F25$b_dist_A + F25$f_per_group_pr_A[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (F25$b_dist_A + F25$f_per_group_pr_A[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (F25$b_dist_A + F25$f_per_group_pr_A[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (F25$b_dist_A + F25$f_per_group_pr_A[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (F25$b_dist_A + F25$f_per_group_pr_A[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (F25$b_dist_A + F25$f_per_group_pr_A[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (F25$b_dist_A + F25$f_per_group_pr_A[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (F25$b_dist_A + F25$f_per_group_pr_A[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (F25$b_dist_A + F25$f_per_group_pr_A[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (F25$b_dist_A + F25$f_per_group_pr_A[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (F25$b_dist_A + F25$f_per_group_pr_A[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (F25$b_dist_A + F25$f_per_group_pr_A[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (F25$b_FA_A),    # Asym (A): CZE Control
  
  Attr_Asym_F_freq,
  Attr_Asym_F_INfreq,
  
  Asym_AUS_Freq = (F25$b_FA_A + F25$f_per_group_pr_A[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (F25$b_FA_A + F25$f_per_group_pr_A[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (F25$b_FA_A + F25$f_per_group_pr_A[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (F25$b_FA_A + F25$f_per_group_pr_A[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (F25$b_FA_A + F25$f_per_group_pr_A[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (F25$b_FA_A + F25$f_per_group_pr_A[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (F25$b_FA_A + F25$f_per_group_pr_A[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (F25$b_FA_A + F25$f_per_group_pr_A[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (F25$b_FA_A + F25$f_per_group_pr_A[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (F25$b_FA_A + F25$f_per_group_pr_A[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (F25$b_FA_A + F25$f_per_group_pr_A[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (F25$b_FA_A + F25$f_per_group_pr_A[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (F25$b_sshd_A),    # SexTyp (A): CZE Control
  
  Attr_SexTyp_F_freq,
  Attr_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (F25$b_sshd_A + F25$f_per_group_pr_A[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (F25$b_sshd_A + F25$f_per_group_pr_A[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (F25$b_sshd_A + F25$f_per_group_pr_A[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (F25$b_sshd_A + F25$f_per_group_pr_A[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (F25$b_sshd_A + F25$f_per_group_pr_A[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (F25$b_sshd_A + F25$f_per_group_pr_A[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (F25$b_sshd_A + F25$f_per_group_pr_A[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (F25$b_sshd_A + F25$f_per_group_pr_A[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (F25$b_sshd_A + F25$f_per_group_pr_A[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (F25$b_sshd_A + F25$f_per_group_pr_A[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (F25$b_sshd_A + F25$f_per_group_pr_A[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (F25$b_sshd_A + F25$f_per_group_pr_A[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (F25$b_L_A),    # L (A): CZE Control
  
  Attr_L_F_freq,
  Attr_L_F_INfreq,
  
  L_AUS_Freq = (F25$b_L_A + F25$f_per_group_pr_A[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (F25$b_L_A + F25$f_per_group_pr_A[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (F25$b_L_A + F25$f_per_group_pr_A[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (F25$b_L_A + F25$f_per_group_pr_A[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (F25$b_L_A + F25$f_per_group_pr_A[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (F25$b_L_A + F25$f_per_group_pr_A[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (F25$b_L_A + F25$f_per_group_pr_A[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (F25$b_L_A + F25$f_per_group_pr_A[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (F25$b_L_A + F25$f_per_group_pr_A[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (F25$b_L_A + F25$f_per_group_pr_A[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (F25$b_L_A + F25$f_per_group_pr_A[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (F25$b_L_A + F25$f_per_group_pr_A[,12,6])   # L (A): COL Less Freq. Users
)


summary.data.frame(F_posts_ATD_MM_Attr)
str(F_posts_ATD_MM_Attr) # Need 65 rows 


labs <- c("Fixed Slope",
          
          "Above (All)",
          "Below (All)",
          
          "AUS/NZE Above",
          "AUS/NZE Below",
          
          "COL Above",
          "COL Below",
          
          "CZE Above",
          "CZE Below",
          
          "ZAF Above",
          "ZAF Below",
          
          "TUR Above",
          "TUR Below",
          
          "VNM Above",
          "VNM Below"
)

cols<-c("#c56043", "#e04b5c", "#de5353", "#c25c42", "#d85652", "#c8585d", "#de534b", "#cd5440", "#d35d5e", "#c3615a", "#d46655", "#cf5552", "#c65f5d","#de534b", "#cd5440",
        "#856554", "#896757", "#815b69", "#8f6756", "#86554d", "#8a675d", "#85586a", "#805158", "#855d52", "#8f4e50", "#8e5960", "#80685a", "#75584c","#85586a", "#805158",
        "#85b68f", "#77bf98", "#86b18f", "#85b589", "#7eB68d", "#8bB37a", "#80B491", "#65Bd7d", "#81B784", "#8bb883", "#8ebf92", "#7bBe8f", "#91bd79","#81B784", "#8bb883",
        "#6b9168", "#69925b", "#689d55", "#6ba261", "#5aaa6b", "#599866", "#519c58", "#55a755", "#5caa53", "#5c9c60", "#588f5b", "#5e9d6f", "#5aa757","#5caa53", "#5c9c60", 
        "#64a9e6", "#79a5ee", "#809be9", "#6d9dfa", "#6899ec", "#6facde", "#77abed", "#63aaf0", "#63a4e4", "#74a5e3", "#79a9eb", "#6aaaf6", "#78a4dd","#6facde", "#77abed"
        
) 




tiff("F_ATD_GMM_SES_ver_1_COEFS.tiff",width=30,height=33,units="cm",res=600,compression="lzw")
layout(matrix(1:3,byrow=F,nrow=1), widths=c(1.1,0.7,0.7),heights=c(1))

par(mar=c(2.1, 14.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.2),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Attr: GMM - Females", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#3d465490", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#3d465490", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#3d465490", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#3d465490", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#3d465490", lwd = 2)


# Small squares...
rect(-0.24,12.5,0.19,15.25, col="#F0F0F0", border = F)
rect(-0.24,28,0.19,30.75, col="#F0F0F0", border = F)
rect(-0.24,43.5,0.19,46.25, col="#F0F0F0", border = F)
rect(-0.24,59,0.19,61.75, col="#F0F0F0", border = F)
rect(-0.24,74.5,0.19,77.25, col="#F0F0F0", border = F)

# 
segments(x0 = -0.25, y0 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), x1 = 0.2, 
         y1 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), col = "#40808080", lwd = 1.5)

# 1
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 0, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 15.5,
         col="#80808080",lty=2)

# 2
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 16, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 31,
         col="#80808080",lty=2)

# 3
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 31.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 46.5,
         col="#80808080",lty=2)

# 4 
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 47, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 62,
         col="#80808080",lty=2)

# 5
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 62.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 77.5,
         col="#80808080",lty=2)

labs <- rep(labs,5)

ys<-c(1:15,16.5:30.5,32:46,47.5:61.5,63:77)
ys<-rev(ys)


for(i in 1:length(F_posts_ATD_MM_Attr)){ 
  toplot<-F_posts_ATD_MM_Attr[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  textlab<-labs[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  axis(2, at=ycoord, labels=textlab, srt=45, las=2, cex.axis=1.2, tick=F)
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=5, lty=1, col=cols[i])
  segments(x0 = ci[1], y0 = ycoord-0.1, x1 = ci[1], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  segments(x0 = ci[2], y0 = ycoord-0.1, x1 = ci[2], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0=0.0, y0=c(0.0, 16, 31.5, 47, 62.5), x1=0.0, y1=c(15.5, 31, 46.5, 62, 77.5), col="#999999", lwd=2, lty=1)

text(x = -0.3, y = 77, labels = "Age", cex = 0.4, pos = 4)
text(x = -0.3, y = 62, labels = "Dist", cex = 0.4, pos = 4)
text(x = -0.3, y = 47, labels = "Asym", cex = 0.4, pos = 4)
text(x = -0.3, y = 32, labels = "SShD", cex = 0.4, pos = 4)
text(x = -0.3, y = 15, labels = "Lightness", cex = 0.4, pos = 4)

# 



# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 

# And now GMM: 
# Preparing composite coefficients: 
# AGE:
TR_Age_F_freq <- c(
  (F25$b_age_T + F25$f_per_group_pr_T[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (F25$b_age_T + F25$f_per_group_pr_T[,3,2])+   # Age (A): COL More Freq. Users
    (F25$b_age_T + F25$f_per_group_pr_T[,5,2])+   # Age (A): CZE More Freq. Users
    (F25$b_age_T + F25$f_per_group_pr_T[,7,2])+   # Age (A): ZAF More Freq. Users
    (F25$b_age_T + F25$f_per_group_pr_T[,9,2])+   # Age (A): TUR More Freq. Users
    (F25$b_age_T + F25$f_per_group_pr_T[,11,2])   # Age (A): COL More Freq. Users
)/6

TR_Age_F_INfreq <- (
  (F25$b_age_T + F25$f_per_group_pr_T[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (F25$b_age_T + F25$f_per_group_pr_T[,4,2])+   # Age (A): COL More Freq. Users
    (F25$b_age_T + F25$f_per_group_pr_T[,6,2])+   # Age (A): CZE More Freq. Users
    (F25$b_age_T + F25$f_per_group_pr_T[,8,2])+   # Age (A): ZAF More Freq. Users
    (F25$b_age_T + F25$f_per_group_pr_T[,10,2])+   # Age (A): TUR More Freq. Users
    (F25$b_age_T + F25$f_per_group_pr_T[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
TR_Dist_F_freq <- (
  (F25$b_dist_T + F25$f_per_group_pr_T[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (F25$b_dist_T + F25$f_per_group_pr_T[,3,3])+   # Dist (A): COL More Freq. Users
    (F25$b_dist_T + F25$f_per_group_pr_T[,5,3])+   # Dist (A): CZE More Freq. Users
    (F25$b_dist_T + F25$f_per_group_pr_T[,7,3])+   # Dist (A): ZAF More Freq. Users
    (F25$b_dist_T + F25$f_per_group_pr_T[,9,3])+   # Dist (A): TUR More Freq. Users
    (F25$b_dist_T + F25$f_per_group_pr_T[,11,3])   # Dist (A): COL More Freq. Users
)/6

TR_Dist_F_INfreq <- (
  (F25$b_dist_T + F25$f_per_group_pr_T[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (F25$b_dist_T + F25$f_per_group_pr_T[,4,3])+   # Dist (A): COL More Freq. Users
    (F25$b_dist_T + F25$f_per_group_pr_T[,6,3])+   # Dist (A): CZE More Freq. Users
    (F25$b_dist_T + F25$f_per_group_pr_T[,8,3])+   # Dist (A): ZAF More Freq. Users
    (F25$b_dist_T + F25$f_per_group_pr_T[,10,3])+   # Dist (A): TUR More Freq. Users
    (F25$b_dist_T + F25$f_per_group_pr_T[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
TR_Asym_F_freq <- (
  (F25$b_FA_T + F25$f_per_group_pr_T[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (F25$b_FA_T + F25$f_per_group_pr_T[,3,4])+   # Asym (A): COL More Freq. Users
    (F25$b_FA_T + F25$f_per_group_pr_T[,5,4])+   # Asym (A): CZE More Freq. Users
    (F25$b_FA_T + F25$f_per_group_pr_T[,7,4])+   # Asym (A): ZAF More Freq. Users
    (F25$b_FA_T + F25$f_per_group_pr_T[,9,4])+   # Asym (A): TUR More Freq. Users
    (F25$b_FA_T + F25$f_per_group_pr_T[,11,4])   # Asym (A): COL More Freq. Users
)/6

TR_Asym_F_INfreq <- (
  (F25$b_FA_T + F25$f_per_group_pr_T[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (F25$b_FA_T + F25$f_per_group_pr_T[,4,4])+   # Asym (A): COL More Freq. Users
    (F25$b_FA_T + F25$f_per_group_pr_T[,6,4])+   # Asym (A): CZE More Freq. Users
    (F25$b_FA_T + F25$f_per_group_pr_T[,8,4])+   # Asym (A): ZAF More Freq. Users
    (F25$b_FA_T + F25$f_per_group_pr_T[,10,4])+   # Asym (A): TUR More Freq. Users
    (F25$b_FA_T + F25$f_per_group_pr_T[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
TR_SexTyp_F_freq <- (
  (F25$b_sshd_T + F25$f_per_group_pr_T[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (F25$b_sshd_T + F25$f_per_group_pr_T[,3,5])+   # SShD (A): COL More Freq. Users
    (F25$b_sshd_T + F25$f_per_group_pr_T[,5,5])+   # SShD (A): CZE More Freq. Users
    (F25$b_sshd_T + F25$f_per_group_pr_T[,7,5])+   # SShD (A): ZAF More Freq. Users
    (F25$b_sshd_T + F25$f_per_group_pr_T[,9,5])+   # SShD (A): TUR More Freq. Users
    (F25$b_sshd_T + F25$f_per_group_pr_T[,11,5])   # SShD (A): COL More Freq. Users
)/6

TR_SexTyp_F_INfreq <- (
  (F25$b_sshd_T + F25$f_per_group_pr_T[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (F25$b_sshd_T + F25$f_per_group_pr_T[,4,5])+   # SShD (A): COL More Freq. Users
    (F25$b_sshd_T + F25$f_per_group_pr_T[,6,5])+   # SShD (A): CZE More Freq. Users
    (F25$b_sshd_T + F25$f_per_group_pr_T[,8,5])+   # SShD (A): ZAF More Freq. Users
    (F25$b_sshd_T + F25$f_per_group_pr_T[,10,5])+   # SShD (A): TUR More Freq. Users
    (F25$b_sshd_T + F25$f_per_group_pr_T[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
TR_L_F_freq <- (
  (F25$b_L_T + F25$f_per_group_pr_T[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (F25$b_L_T + F25$f_per_group_pr_T[,3,6])+   # L (A): COL More Freq. Users
    (F25$b_L_T + F25$f_per_group_pr_T[,5,6])+   # L (A): CZE More Freq. Users
    (F25$b_L_T + F25$f_per_group_pr_T[,7,6])+   # L (A): ZAF More Freq. Users
    (F25$b_L_T + F25$f_per_group_pr_T[,9,6])+   # L (A): TUR More Freq. Users
    (F25$b_L_T + F25$f_per_group_pr_T[,11,6])   # L (A): COL More Freq. Users
)/6

TR_L_F_INfreq <- (
  (F25$b_L_T + F25$f_per_group_pr_T[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (F25$b_L_T + F25$f_per_group_pr_T[,4,6])+   # L (A): COL More Freq. Users
    (F25$b_L_T + F25$f_per_group_pr_T[,6,6])+   # L (A): CZE More Freq. Users
    (F25$b_L_T + F25$f_per_group_pr_T[,8,6])+   # L (A): ZAF More Freq. Users
    (F25$b_L_T + F25$f_per_group_pr_T[,10,6])+   # L (A): TUR More Freq. Users
    (F25$b_L_T + F25$f_per_group_pr_T[,12,6])   # L (A): COL More Freq. Users
)/6



# Trusw - GMM 
str(F25)
str(F25$f_per_group_pr_T) # Mind that 1 is intercept! 

F_posts_ATD_MM_TRU <- list(
  #  AGE
  Age_Slope = (F25$b_age_T),    # Slope prior to accessing culture
  
  TR_Age_F_freq,
  TR_Age_F_INfreq,
  
  Age_AUS_Freq = (F25$b_age_T + F25$f_per_group_pr_T[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (F25$b_age_T + F25$f_per_group_pr_T[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (F25$b_age_T + F25$f_per_group_pr_T[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (F25$b_age_T + F25$f_per_group_pr_T[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (F25$b_age_T + F25$f_per_group_pr_T[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (F25$b_age_T + F25$f_per_group_pr_T[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (F25$b_age_T + F25$f_per_group_pr_T[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (F25$b_age_T + F25$f_per_group_pr_T[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (F25$b_age_T + F25$f_per_group_pr_T[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (F25$b_age_T + F25$f_per_group_pr_T[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (F25$b_age_T + F25$f_per_group_pr_T[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (F25$b_age_T + F25$f_per_group_pr_T[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (F25$b_dist_T),    # Dist (A): CZE Control
  
  TR_Dist_F_freq,
  TR_Dist_F_INfreq,
  
  Dist_AUS_Freq = (F25$b_dist_T + F25$f_per_group_pr_T[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (F25$b_dist_T + F25$f_per_group_pr_T[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (F25$b_dist_T + F25$f_per_group_pr_T[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (F25$b_dist_T + F25$f_per_group_pr_T[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (F25$b_dist_T + F25$f_per_group_pr_T[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (F25$b_dist_T + F25$f_per_group_pr_T[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (F25$b_dist_T + F25$f_per_group_pr_T[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (F25$b_dist_T + F25$f_per_group_pr_T[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (F25$b_dist_T + F25$f_per_group_pr_T[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (F25$b_dist_T + F25$f_per_group_pr_T[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (F25$b_dist_T + F25$f_per_group_pr_T[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (F25$b_dist_T + F25$f_per_group_pr_T[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (F25$b_FA_T),    # Asym (A): CZE Control
  
  TR_Asym_F_freq,
  TR_Asym_F_INfreq,
  
  Asym_AUS_Freq = (F25$b_FA_T + F25$f_per_group_pr_T[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (F25$b_FA_T + F25$f_per_group_pr_T[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (F25$b_FA_T + F25$f_per_group_pr_T[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (F25$b_FA_T + F25$f_per_group_pr_T[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (F25$b_FA_T + F25$f_per_group_pr_T[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (F25$b_FA_T + F25$f_per_group_pr_T[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (F25$b_FA_T + F25$f_per_group_pr_T[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (F25$b_FA_T + F25$f_per_group_pr_T[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (F25$b_FA_T + F25$f_per_group_pr_T[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (F25$b_FA_T + F25$f_per_group_pr_T[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (F25$b_FA_T + F25$f_per_group_pr_T[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (F25$b_FA_T + F25$f_per_group_pr_T[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (F25$b_sshd_T),    # SexTyp (A): CZE Control
  
  TR_SexTyp_F_freq,
  TR_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (F25$b_sshd_T + F25$f_per_group_pr_T[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (F25$b_sshd_T + F25$f_per_group_pr_T[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (F25$b_sshd_T + F25$f_per_group_pr_T[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (F25$b_sshd_T + F25$f_per_group_pr_T[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (F25$b_sshd_T + F25$f_per_group_pr_T[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (F25$b_sshd_T + F25$f_per_group_pr_T[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (F25$b_sshd_T + F25$f_per_group_pr_T[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (F25$b_sshd_T + F25$f_per_group_pr_T[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (F25$b_sshd_T + F25$f_per_group_pr_T[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (F25$b_sshd_T + F25$f_per_group_pr_T[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (F25$b_sshd_T + F25$f_per_group_pr_T[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (F25$b_sshd_T + F25$f_per_group_pr_T[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (F25$b_L_T),    # L (A): CZE Control
  
  TR_L_F_freq,
  TR_L_F_INfreq,
  
  L_AUS_Freq = (F25$b_L_T + F25$f_per_group_pr_T[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (F25$b_L_T + F25$f_per_group_pr_T[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (F25$b_L_T + F25$f_per_group_pr_T[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (F25$b_L_T + F25$f_per_group_pr_T[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (F25$b_L_T + F25$f_per_group_pr_T[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (F25$b_L_T + F25$f_per_group_pr_T[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (F25$b_L_T + F25$f_per_group_pr_T[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (F25$b_L_T + F25$f_per_group_pr_T[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (F25$b_L_T + F25$f_per_group_pr_T[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (F25$b_L_T + F25$f_per_group_pr_T[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (F25$b_L_T + F25$f_per_group_pr_T[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (F25$b_L_T + F25$f_per_group_pr_T[,12,6])   # L (A): COL Less Freq. Users
)


summary.data.frame(F_posts_ATD_MM_TRU)
str(F_posts_ATD_MM_TRU) # Need 65 rows 



par(mar=c(2.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.2),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Trustw: GMM - Females", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting


# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#3d465490", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#3d465490", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#3d465490", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#3d465490", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#3d465490", lwd = 2)


# Small squares...
rect(-0.24,12.5,0.19,15.25, col="#F0F0F0", border = F)
rect(-0.24,28,0.19,30.75, col="#F0F0F0", border = F)
rect(-0.24,43.5,0.19,46.25, col="#F0F0F0", border = F)
rect(-0.24,59,0.19,61.75, col="#F0F0F0", border = F)
rect(-0.24,74.5,0.19,77.25, col="#F0F0F0", border = F)

# 
segments(x0 = -0.25, y0 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), x1 = 0.2, 
         y1 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), col = "#40808080", lwd = 1.5)

# 1
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 0, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 15.5,
         col="#80808080",lty=2)

# 2
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 16, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 31,
         col="#80808080",lty=2)

# 3
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 31.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 46.5,
         col="#80808080",lty=2)

# 4 
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 47, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 62,
         col="#80808080",lty=2)

# 5
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 62.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 77.5,
         col="#80808080",lty=2)



ys<-c(1:15,16.5:30.5,32:46,47.5:61.5,63:77)
ys<-rev(ys)

labs <- rep(labs,5)


for(i in 1:length(F_posts_ATD_MM_TRU)){ 
  toplot<-F_posts_ATD_MM_TRU[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=5, lty=1, col=cols[i])
  segments(x0 = ci[1], y0 = ycoord-0.1, x1 = ci[1], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  segments(x0 = ci[2], y0 = ycoord-0.1, x1 = ci[2], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0=0.0, y0=c(0.0, 16, 31.5, 47, 62.5), x1=0.0, y1=c(15.5, 31, 46.5, 62, 77.5), col="#999999", lwd=2, lty=1)
# 





# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 

# And now GMM: 
# Preparing composite coefficients: 
# AGE:
DOM_Age_F_freq <- c(
  (F25$b_age_D + F25$f_per_group_pr_D[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (F25$b_age_D + F25$f_per_group_pr_D[,3,2])+   # Age (A): COL More Freq. Users
    (F25$b_age_D + F25$f_per_group_pr_D[,5,2])+   # Age (A): CZE More Freq. Users
    (F25$b_age_D + F25$f_per_group_pr_D[,7,2])+   # Age (A): ZAF More Freq. Users
    (F25$b_age_D + F25$f_per_group_pr_D[,9,2])+   # Age (A): TUR More Freq. Users
    (F25$b_age_D + F25$f_per_group_pr_D[,11,2])   # Age (A): COL More Freq. Users
)/6

DOM_Age_F_INfreq <- (
  (F25$b_age_D + F25$f_per_group_pr_D[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (F25$b_age_D + F25$f_per_group_pr_D[,4,2])+   # Age (A): COL More Freq. Users
    (F25$b_age_D + F25$f_per_group_pr_D[,6,2])+   # Age (A): CZE More Freq. Users
    (F25$b_age_D + F25$f_per_group_pr_D[,8,2])+   # Age (A): ZAF More Freq. Users
    (F25$b_age_D + F25$f_per_group_pr_D[,10,2])+   # Age (A): TUR More Freq. Users
    (F25$b_age_D + F25$f_per_group_pr_D[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
DOM_Dist_F_freq <- (
  (F25$b_dist_D + F25$f_per_group_pr_D[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (F25$b_dist_D + F25$f_per_group_pr_D[,3,3])+   # Dist (A): COL More Freq. Users
    (F25$b_dist_D + F25$f_per_group_pr_D[,5,3])+   # Dist (A): CZE More Freq. Users
    (F25$b_dist_D + F25$f_per_group_pr_D[,7,3])+   # Dist (A): ZAF More Freq. Users
    (F25$b_dist_D + F25$f_per_group_pr_D[,9,3])+   # Dist (A): TUR More Freq. Users
    (F25$b_dist_D + F25$f_per_group_pr_D[,11,3])   # Dist (A): COL More Freq. Users
)/6

DOM_Dist_F_INfreq <- (
  (F25$b_dist_D + F25$f_per_group_pr_D[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (F25$b_dist_D + F25$f_per_group_pr_D[,4,3])+   # Dist (A): COL More Freq. Users
    (F25$b_dist_D + F25$f_per_group_pr_D[,6,3])+   # Dist (A): CZE More Freq. Users
    (F25$b_dist_D + F25$f_per_group_pr_D[,8,3])+   # Dist (A): ZAF More Freq. Users
    (F25$b_dist_D + F25$f_per_group_pr_D[,10,3])+   # Dist (A): TUR More Freq. Users
    (F25$b_dist_D + F25$f_per_group_pr_D[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
DOM_Asym_F_freq <- (
  (F25$b_FA_D + F25$f_per_group_pr_D[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (F25$b_FA_D + F25$f_per_group_pr_D[,3,4])+   # Asym (A): COL More Freq. Users
    (F25$b_FA_D + F25$f_per_group_pr_D[,5,4])+   # Asym (A): CZE More Freq. Users
    (F25$b_FA_D + F25$f_per_group_pr_D[,7,4])+   # Asym (A): ZAF More Freq. Users
    (F25$b_FA_D + F25$f_per_group_pr_D[,9,4])+   # Asym (A): TUR More Freq. Users
    (F25$b_FA_D + F25$f_per_group_pr_D[,11,4])   # Asym (A): COL More Freq. Users
)/6

DOM_Asym_F_INfreq <- (
  (F25$b_FA_D + F25$f_per_group_pr_D[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (F25$b_FA_D + F25$f_per_group_pr_D[,4,4])+   # Asym (A): COL More Freq. Users
    (F25$b_FA_D + F25$f_per_group_pr_D[,6,4])+   # Asym (A): CZE More Freq. Users
    (F25$b_FA_D + F25$f_per_group_pr_D[,8,4])+   # Asym (A): ZAF More Freq. Users
    (F25$b_FA_D + F25$f_per_group_pr_D[,10,4])+   # Asym (A): TUR More Freq. Users
    (F25$b_FA_D + F25$f_per_group_pr_D[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
DOM_SexTyp_F_freq <- (
  (F25$b_sshd_D + F25$f_per_group_pr_D[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (F25$b_sshd_D + F25$f_per_group_pr_D[,3,5])+   # SShD (A): COL More Freq. Users
    (F25$b_sshd_D + F25$f_per_group_pr_D[,5,5])+   # SShD (A): CZE More Freq. Users
    (F25$b_sshd_D + F25$f_per_group_pr_D[,7,5])+   # SShD (A): ZAF More Freq. Users
    (F25$b_sshd_D + F25$f_per_group_pr_D[,9,5])+   # SShD (A): TUR More Freq. Users
    (F25$b_sshd_D + F25$f_per_group_pr_D[,11,5])   # SShD (A): COL More Freq. Users
)/6

DOM_SexTyp_F_INfreq <- (
  (F25$b_sshd_D + F25$f_per_group_pr_D[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (F25$b_sshd_D + F25$f_per_group_pr_D[,4,5])+   # SShD (A): COL More Freq. Users
    (F25$b_sshd_D + F25$f_per_group_pr_D[,6,5])+   # SShD (A): CZE More Freq. Users
    (F25$b_sshd_D + F25$f_per_group_pr_D[,8,5])+   # SShD (A): ZAF More Freq. Users
    (F25$b_sshd_D + F25$f_per_group_pr_D[,10,5])+   # SShD (A): TUR More Freq. Users
    (F25$b_sshd_D + F25$f_per_group_pr_D[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
DOM_L_F_freq <- (
  (F25$b_L_D + F25$f_per_group_pr_D[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (F25$b_L_D + F25$f_per_group_pr_D[,3,6])+   # L (A): COL More Freq. Users
    (F25$b_L_D + F25$f_per_group_pr_D[,5,6])+   # L (A): CZE More Freq. Users
    (F25$b_L_D + F25$f_per_group_pr_D[,7,6])+   # L (A): ZAF More Freq. Users
    (F25$b_L_D + F25$f_per_group_pr_D[,9,6])+   # L (A): TUR More Freq. Users
    (F25$b_L_D + F25$f_per_group_pr_D[,11,6])   # L (A): COL More Freq. Users
)/6

DOM_L_F_INfreq <- (
  (F25$b_L_D + F25$f_per_group_pr_D[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (F25$b_L_D + F25$f_per_group_pr_D[,4,6])+   # L (A): COL More Freq. Users
    (F25$b_L_D + F25$f_per_group_pr_D[,6,6])+   # L (A): CZE More Freq. Users
    (F25$b_L_D + F25$f_per_group_pr_D[,8,6])+   # L (A): ZAF More Freq. Users
    (F25$b_L_D + F25$f_per_group_pr_D[,10,6])+   # L (A): TUR More Freq. Users
    (F25$b_L_D + F25$f_per_group_pr_D[,12,6])   # L (A): COL More Freq. Users
)/6



# GMM - SEPARATELY 

# Dominance - GMM 
str(F25)
str(F25$f_per_group_pr_D) # Mind that 1 is intercept! 

F_posts_ATD_MM_DOM <- list(
  #  AGE
  Age_Slope = (F25$b_age_D),    # Slope prior to accessing culture
  
  DOM_Age_F_freq,
  DOM_Age_F_INfreq,
  
  Age_AUS_Freq = (F25$b_age_D + F25$f_per_group_pr_D[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (F25$b_age_D + F25$f_per_group_pr_D[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (F25$b_age_D + F25$f_per_group_pr_D[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (F25$b_age_D + F25$f_per_group_pr_D[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (F25$b_age_D + F25$f_per_group_pr_D[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (F25$b_age_D + F25$f_per_group_pr_D[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (F25$b_age_D + F25$f_per_group_pr_D[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (F25$b_age_D + F25$f_per_group_pr_D[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (F25$b_age_D + F25$f_per_group_pr_D[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (F25$b_age_D + F25$f_per_group_pr_D[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (F25$b_age_D + F25$f_per_group_pr_D[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (F25$b_age_D + F25$f_per_group_pr_D[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (F25$b_dist_D),    # Dist (A): CZE Control
  
  DOM_Dist_F_freq,
  DOM_Dist_F_INfreq,
  
  Dist_AUS_Freq = (F25$b_dist_D + F25$f_per_group_pr_D[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (F25$b_dist_D + F25$f_per_group_pr_D[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (F25$b_dist_D + F25$f_per_group_pr_D[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (F25$b_dist_D + F25$f_per_group_pr_D[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (F25$b_dist_D + F25$f_per_group_pr_D[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (F25$b_dist_D + F25$f_per_group_pr_D[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (F25$b_dist_D + F25$f_per_group_pr_D[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (F25$b_dist_D + F25$f_per_group_pr_D[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (F25$b_dist_D + F25$f_per_group_pr_D[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (F25$b_dist_D + F25$f_per_group_pr_D[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (F25$b_dist_D + F25$f_per_group_pr_D[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (F25$b_dist_D + F25$f_per_group_pr_D[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (F25$b_FA_D),    # Asym (A): CZE Control
  
  DOM_Asym_F_freq,
  DOM_Asym_F_INfreq,
  
  Asym_AUS_Freq = (F25$b_FA_D + F25$f_per_group_pr_D[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (F25$b_FA_D + F25$f_per_group_pr_D[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (F25$b_FA_D + F25$f_per_group_pr_D[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (F25$b_FA_D + F25$f_per_group_pr_D[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (F25$b_FA_D + F25$f_per_group_pr_D[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (F25$b_FA_D + F25$f_per_group_pr_D[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (F25$b_FA_D + F25$f_per_group_pr_D[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (F25$b_FA_D + F25$f_per_group_pr_D[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (F25$b_FA_D + F25$f_per_group_pr_D[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (F25$b_FA_D + F25$f_per_group_pr_D[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (F25$b_FA_D + F25$f_per_group_pr_D[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (F25$b_FA_D + F25$f_per_group_pr_D[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (F25$b_sshd_D),    # SexTyp (A): CZE Control
  
  DOM_SexTyp_F_freq,
  DOM_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (F25$b_sshd_D + F25$f_per_group_pr_D[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (F25$b_sshd_D + F25$f_per_group_pr_D[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (F25$b_sshd_D + F25$f_per_group_pr_D[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (F25$b_sshd_D + F25$f_per_group_pr_D[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (F25$b_sshd_D + F25$f_per_group_pr_D[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (F25$b_sshd_D + F25$f_per_group_pr_D[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (F25$b_sshd_D + F25$f_per_group_pr_D[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (F25$b_sshd_D + F25$f_per_group_pr_D[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (F25$b_sshd_D + F25$f_per_group_pr_D[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (F25$b_sshd_D + F25$f_per_group_pr_D[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (F25$b_sshd_D + F25$f_per_group_pr_D[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (F25$b_sshd_D + F25$f_per_group_pr_D[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (F25$b_L_D),    # L (A): CZE Control
  
  DOM_L_F_freq,
  DOM_L_F_INfreq,
  
  L_AUS_Freq = (F25$b_L_D + F25$f_per_group_pr_D[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (F25$b_L_D + F25$f_per_group_pr_D[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (F25$b_L_D + F25$f_per_group_pr_D[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (F25$b_L_D + F25$f_per_group_pr_D[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (F25$b_L_D + F25$f_per_group_pr_D[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (F25$b_L_D + F25$f_per_group_pr_D[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (F25$b_L_D + F25$f_per_group_pr_D[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (F25$b_L_D + F25$f_per_group_pr_D[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (F25$b_L_D + F25$f_per_group_pr_D[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (F25$b_L_D + F25$f_per_group_pr_D[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (F25$b_L_D + F25$f_per_group_pr_D[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (F25$b_L_D + F25$f_per_group_pr_D[,12,6])   # L (A): COL Less Freq. Users
)


summary.data.frame(F_posts_ATD_MM_DOM)
str(F_posts_ATD_MM_TRU) # Need 65 rows 



par(mar=c(2.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.2),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Dominance: GMM - Females", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting


# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#3d465490", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#3d465490", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#3d465490", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#3d465490", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#3d465490", lwd = 2)


# Small squares...
rect(-0.24,12.5,0.19,15.25, col="#F0F0F0", border = F)
rect(-0.24,28,0.19,30.75, col="#F0F0F0", border = F)
rect(-0.24,43.5,0.19,46.25, col="#F0F0F0", border = F)
rect(-0.24,59,0.19,61.75, col="#F0F0F0", border = F)
rect(-0.24,74.5,0.19,77.25, col="#F0F0F0", border = F)

# 
segments(x0 = -0.25, y0 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), x1 = 0.2, 
         y1 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), col = "#40808080", lwd = 1.5)

# 1
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 0, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 15.5,
         col="#80808080",lty=2)

# 2
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 16, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 31,
         col="#80808080",lty=2)

# 3
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 31.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 46.5,
         col="#80808080",lty=2)

# 4 
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 47, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 62,
         col="#80808080",lty=2)

# 5
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 62.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 77.5,
         col="#80808080",lty=2)



ys<-c(1:15,16.5:30.5,32:46,47.5:61.5,63:77)
ys<-rev(ys)

labs <- rep(labs,5)

cex_t <- 1.25
cex_t2 <- 1
pls <- 1

for(i in 1:length(F_posts_ATD_MM_DOM)){ 
  toplot<-F_posts_ATD_MM_DOM[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=5, lty=1, col=cols[i])
  segments(x0 = ci[1], y0 = ycoord-0.1, x1 = ci[1], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  segments(x0 = ci[2], y0 = ycoord-0.1, x1 = ci[2], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0=0.0, y0=c(0.0, 16, 31.5, 47, 62.5), x1=0.0, y1=c(15.5, 31, 46.5, 62, 77.5), col="#999999", lwd=2, lty=1)
# 


dev.off()









#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Figure S15 - analogue of the Figure 4 in the main body of the article, which is a figure (top) presenting mean estimates
# in every sample, (bottom) presenting effects of morphometric and demographic predictors: SPECIFICALLY for WOMEN
# Version without labels

# Upper part: Frequent - all
# Compute the average of all group estimates

# Note - these are the same data as those to enter the A-T-D model; therefore, 
# the order of levels is the same, too. 

Avg_Freq_U_A = ( 
  M25$aA + M25$f_per_group_pr_A[,1,1] + 
    M25$aA + M25$f_per_group_pr_A[,3,1] + 
    M25$aA + M25$f_per_group_pr_A[,5,1] + 
    M25$aA + M25$f_per_group_pr_A[,7,1] + 
    M25$aA + M25$f_per_group_pr_A[,9,1] + 
    M25$aA + M25$f_per_group_pr_A[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_A = ( 
  M25$aA + M25$f_per_group_pr_A[,2,1] + 
    M25$aA + M25$f_per_group_pr_A[,4,1] + 
    M25$aA + M25$f_per_group_pr_A[,6,1] + 
    M25$aA + M25$f_per_group_pr_A[,8,1] + 
    M25$aA + M25$f_per_group_pr_A[,10,1] + 
    M25$aA + M25$f_per_group_pr_A[,12,1] 
) / 6



F_posts_ATR <- list(
  # 1 Intercept: 
  Intercept = M25$aA,
  # Infrequent users
  Avg_Freq_U_A,
  # Frequent users 
  Avg_Infreq_U_A,
  # Difference 
  Difference = Avg_Freq_U_A - Avg_Infreq_U_A,
  # 4 Australia - above - median users  
  AUS_Intensive_User = M25$aA + M25$f_per_group_pr_A[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = M25$aA + M25$f_per_group_pr_A[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = M25$aA + M25$f_per_group_pr_A[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = M25$aA + M25$f_per_group_pr_A[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = M25$aA + M25$f_per_group_pr_A[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = M25$aA + M25$f_per_group_pr_A[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = M25$aA + M25$f_per_group_pr_A[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = M25$aA + M25$f_per_group_pr_A[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = M25$aA + M25$f_per_group_pr_A[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = M25$aA + M25$f_per_group_pr_A[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = M25$aA + M25$f_per_group_pr_A[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = M25$aA + M25$f_per_group_pr_A[,12,1]
)

summary.data.frame(F_posts_ATR)
str(F_posts_ATR) # Need 13 rows 

#--------------

# Trustworthiness
Avg_Freq_U_T = ( 
  M25$aT + M25$f_per_group_pr_T[,1,1] + 
    M25$aT + M25$f_per_group_pr_T[,3,1] + 
    M25$aT + M25$f_per_group_pr_T[,5,1] + 
    M25$aT + M25$f_per_group_pr_T[,7,1] + 
    M25$aT + M25$f_per_group_pr_T[,9,1] + 
    M25$aT + M25$f_per_group_pr_T[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_T = ( 
  M25$aT + M25$f_per_group_pr_T[,2,1] + 
    M25$aT + M25$f_per_group_pr_T[,4,1] + 
    M25$aT + M25$f_per_group_pr_T[,6,1] + 
    M25$aT + M25$f_per_group_pr_T[,8,1] + 
    M25$aT + M25$f_per_group_pr_T[,10,1] + 
    M25$aT + M25$f_per_group_pr_T[,12,1] 
) / 6

F_posts_TRU <- list(
  # 1 Intercept: 
  Intercept = M25$aT,
  # Infrequent users
  Avg_Freq_U_T,
  # Frequent users 
  Avg_Infreq_U_T,
  # Difference 
  Difference = Avg_Freq_U_T - Avg_Infreq_U_T,
  # 4 Australia - above - median users  
  AUS_Intensive_User = M25$aT + M25$f_per_group_pr_T[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = M25$aT + M25$f_per_group_pr_T[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = M25$aT + M25$f_per_group_pr_T[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = M25$aT + M25$f_per_group_pr_T[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = M25$aT + M25$f_per_group_pr_T[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = M25$aT + M25$f_per_group_pr_T[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = M25$aT + M25$f_per_group_pr_T[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = M25$aT + M25$f_per_group_pr_T[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = M25$aT + M25$f_per_group_pr_T[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = M25$aT + M25$f_per_group_pr_T[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = M25$aT + M25$f_per_group_pr_T[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = M25$aT + M25$f_per_group_pr_T[,12,1]
)

summary.data.frame(F_posts_TRU)
str(F_posts_TRU) # Need 13 rows 

#--------------

# Dominance
Avg_Freq_U_D = ( 
  M25$aD + M25$f_per_group_pr_D[,1,1] + 
    M25$aD + M25$f_per_group_pr_D[,3,1] + 
    M25$aD + M25$f_per_group_pr_D[,5,1] + 
    M25$aD + M25$f_per_group_pr_D[,7,1] + 
    M25$aD + M25$f_per_group_pr_D[,9,1] + 
    M25$aD + M25$f_per_group_pr_D[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_D = ( 
  M25$aD + M25$f_per_group_pr_D[,2,1] + 
    M25$aD + M25$f_per_group_pr_D[,4,1] + 
    M25$aD + M25$f_per_group_pr_D[,6,1] + 
    M25$aD + M25$f_per_group_pr_D[,8,1] + 
    M25$aD + M25$f_per_group_pr_D[,10,1] + 
    M25$aD + M25$f_per_group_pr_D[,12,1] 
) / 6

F_posts_DOM <- list(
  # 1 Intercept: 
  Intercept = M25$aD,
  # Infrequent users
  Avg_Freq_U_D,
  # Frequent users 
  Avg_Infreq_U_D,
  # Difference 
  Difference = Avg_Freq_U_D - Avg_Infreq_U_D,
  # 4 Australia - above - median users  
  AUS_Intensive_User = M25$aD + M25$f_per_group_pr_D[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = M25$aD + M25$f_per_group_pr_D[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = M25$aD + M25$f_per_group_pr_D[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = M25$aD + M25$f_per_group_pr_D[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = M25$aD + M25$f_per_group_pr_D[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = M25$aD + M25$f_per_group_pr_D[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = M25$aD + M25$f_per_group_pr_D[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = M25$aD + M25$f_per_group_pr_D[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = M25$aD + M25$f_per_group_pr_D[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = M25$aD + M25$f_per_group_pr_D[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = M25$aD + M25$f_per_group_pr_D[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = M25$aD + M25$f_per_group_pr_D[,12,1]
)

summary.data.frame(F_posts_DOM)
str(F_posts_DOM) 


# PLOTTING: 
cols<-c("#00FBFF", 
        "#8C00FF",
        "#FFADD0",
        "#09C9FF",
        
        "#00FF3380",
        "#0FEA9980",
        
        "#FFEA0080",
        "#c9b80080",
        
        "#e0935c80",
        "#ff7b1c80",
        
        "#4796ff80",
        "#1278ff80",
        
        "#acdb5a80",
        "#a1f21380",
        
        "#6174ed80",
        "#223ef080"
) 



labs<-c("Intercept",
        "Above",
        "Below",
        "Above - Below: Difference",
        "Australia/NewZ - Above",
        "Australia/NewZ - Below",
        "Colombia - Above",
        "Colombia - Below",
        "Czechia - Above",
        "Czechia - Below",
        "South Africa - Above",
        "South Africa - Below",
        "Turkey - Above",
        "Turkey - Below",
        "Vietnam - Above",
        "Vietnam - Below"
)



# Figure S3:Upper Panel - Analogue of Figure 3:Upper Panel (final assembly done in InkScape)...

tiff("M_ATD_SES_ver_1_LEVELS_NOLABELS.tiff",width=30,height=14,units="cm",res=600,compression="lzw")
layout(matrix(1:3,byrow=F,nrow=1), widths=c(1.8,1.2,1.2),heights=c(1))

# ATTRACTIVENESS

par(mar=c(3.1, 15.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-1,0.7),ylim=c(1,17),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Attractiveness - Group Estimates", side = 3, line = -0.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

rect(-1,0.5,0.75,17, col=F, border = "#b9b9B999", lwd = 2)
rect(-0.95,12.5,0.7,16.5, col="#F0F0F0", border = F)

segments(x0 = -1, y0 = 1:16, x1 = 0.75, y1 = 1:16, col = "#40808080", lwd = 1.5)
segments(x0 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y0 = 0.5, x1 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y1 = 16.5, col="#80808080",lty=2)

ys<-seq(from=1,to=16, by=1)
ys<-rev(ys)

cex_t <- 1.25
cex_t2 <- 0.95
pls <- 0.75


for(i in 1:length(F_posts_ATR)){ 
  toplot<-F_posts_ATR[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  textlab<-labs[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  axis(2, at=ycoord, labels=textlab, srt=45, las=2, cex.axis=1.2, tick=F)
  for (i in 1:16) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.007)+ycoord[i],border=T, lwd = 1, col=colpol))
  }
  #lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0 = 0, y0 = 0, x1 = 0, y1 = 16.5, col = "#999999", lwd = 2, lty = 1)



# TRUSTWORTHINESS
par(mar=c(3.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-1,0.7),ylim=c(1,17),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Trustworthiness - Group Estimates", side = 3, line = -0.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

rect(-1,0.5,0.75,17, col=F, border = "#b9b9B999", lwd = 2)
rect(-0.95,12.5,0.7,16.5, col="#F0F0F0", border = F)

segments(x0 = -1, y0 = 1:16, x1 = 0.75, y1 = 1:16, col = "#40808080", lwd = 1.5)
segments(x0 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y0 = 0.5, x1 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y1 = 16.5, col="#80808080",lty=2)

ys<-seq(from=1,to=16, by=1)
ys<-rev(ys)

for(i in 1:length(F_posts_TRU)){ 
  toplot<-F_posts_TRU[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  for (i in 1:16) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.007)+ycoord[i],border=T, lwd = 1, col=colpol))
  }
  #lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0 = 0, y0 = 0, x1 = 0, y1 = 16.5, col = "#999999", lwd = 2, lty = 1)




# DOMINANCE
par(mar=c(3.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-1,0.7),ylim=c(1,17),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Dominance - Group Estimates", side = 3, line = -0.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

rect(-1,0.5,0.75,17, col=F, border = "#b9b9B999", lwd = 2)
rect(-0.95,12.5,0.7,16.5, col="#F0F0F0", border = F)

segments(x0 = -1, y0 = 1:16, x1 = 0.75, y1 = 1:16, col = "#40808080", lwd = 1.5)
segments(x0 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y0 = 0.5, x1 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y1 = 16.5, col="#80808080",lty=2)

ys<-seq(from=1,to=16, by=1)
ys<-rev(ys)


for(i in 1:length(F_posts_DOM)){ 
  toplot<-F_posts_DOM[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  for (i in 1:16) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.007)+ycoord[i],border=T, lwd = 1, col=colpol))
  }
  #lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0 = 0, y0 = 0, x1 = 0, y1 = 16.5, col = "#999999", lwd = 2, lty = 1)


dev.off()


#---------------------------------

# And now GMM: 


# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS


# Preparing composite coefficients: 
# AGE:
Attr_Age_F_freq <- c(
  (M25$b_age_A + M25$f_per_group_pr_A[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (M25$b_age_A + M25$f_per_group_pr_A[,3,2])+   # Age (A): COL More Freq. Users
    (M25$b_age_A + M25$f_per_group_pr_A[,5,2])+   # Age (A): CZE More Freq. Users
    (M25$b_age_A + M25$f_per_group_pr_A[,7,2])+   # Age (A): ZAF More Freq. Users
    (M25$b_age_A + M25$f_per_group_pr_A[,9,2])+   # Age (A): TUR More Freq. Users
    (M25$b_age_A + M25$f_per_group_pr_A[,11,2])   # Age (A): COL More Freq. Users
)/6

Attr_Age_F_INfreq <- (
  (M25$b_age_A + M25$f_per_group_pr_A[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (M25$b_age_A + M25$f_per_group_pr_A[,4,2])+   # Age (A): COL More Freq. Users
    (M25$b_age_A + M25$f_per_group_pr_A[,6,2])+   # Age (A): CZE More Freq. Users
    (M25$b_age_A + M25$f_per_group_pr_A[,8,2])+   # Age (A): ZAF More Freq. Users
    (M25$b_age_A + M25$f_per_group_pr_A[,10,2])+   # Age (A): TUR More Freq. Users
    (M25$b_age_A + M25$f_per_group_pr_A[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
Attr_Dist_F_freq <- (
  (M25$b_dist_A + M25$f_per_group_pr_A[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (M25$b_dist_A + M25$f_per_group_pr_A[,3,3])+   # Dist (A): COL More Freq. Users
    (M25$b_dist_A + M25$f_per_group_pr_A[,5,3])+   # Dist (A): CZE More Freq. Users
    (M25$b_dist_A + M25$f_per_group_pr_A[,7,3])+   # Dist (A): ZAF More Freq. Users
    (M25$b_dist_A + M25$f_per_group_pr_A[,9,3])+   # Dist (A): TUR More Freq. Users
    (M25$b_dist_A + M25$f_per_group_pr_A[,11,3])   # Dist (A): COL More Freq. Users
)/6

Attr_Dist_F_INfreq <- (
  (M25$b_dist_A + M25$f_per_group_pr_A[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (M25$b_dist_A + M25$f_per_group_pr_A[,4,3])+   # Dist (A): COL More Freq. Users
    (M25$b_dist_A + M25$f_per_group_pr_A[,6,3])+   # Dist (A): CZE More Freq. Users
    (M25$b_dist_A + M25$f_per_group_pr_A[,8,3])+   # Dist (A): ZAF More Freq. Users
    (M25$b_dist_A + M25$f_per_group_pr_A[,10,3])+   # Dist (A): TUR More Freq. Users
    (M25$b_dist_A + M25$f_per_group_pr_A[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
Attr_Asym_F_freq <- (
  (M25$b_FA_A + M25$f_per_group_pr_A[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (M25$b_FA_A + M25$f_per_group_pr_A[,3,4])+   # Asym (A): COL More Freq. Users
    (M25$b_FA_A + M25$f_per_group_pr_A[,5,4])+   # Asym (A): CZE More Freq. Users
    (M25$b_FA_A + M25$f_per_group_pr_A[,7,4])+   # Asym (A): ZAF More Freq. Users
    (M25$b_FA_A + M25$f_per_group_pr_A[,9,4])+   # Asym (A): TUR More Freq. Users
    (M25$b_FA_A + M25$f_per_group_pr_A[,11,4])   # Asym (A): COL More Freq. Users
)/6

Attr_Asym_F_INfreq <- (
  (M25$b_FA_A + M25$f_per_group_pr_A[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (M25$b_FA_A + M25$f_per_group_pr_A[,4,4])+   # Asym (A): COL More Freq. Users
    (M25$b_FA_A + M25$f_per_group_pr_A[,6,4])+   # Asym (A): CZE More Freq. Users
    (M25$b_FA_A + M25$f_per_group_pr_A[,8,4])+   # Asym (A): ZAF More Freq. Users
    (M25$b_FA_A + M25$f_per_group_pr_A[,10,4])+   # Asym (A): TUR More Freq. Users
    (M25$b_FA_A + M25$f_per_group_pr_A[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
Attr_SexTyp_F_freq <- (
  (M25$b_sshd_A + M25$f_per_group_pr_A[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (M25$b_sshd_A + M25$f_per_group_pr_A[,3,5])+   # SShD (A): COL More Freq. Users
    (M25$b_sshd_A + M25$f_per_group_pr_A[,5,5])+   # SShD (A): CZE More Freq. Users
    (M25$b_sshd_A + M25$f_per_group_pr_A[,7,5])+   # SShD (A): ZAF More Freq. Users
    (M25$b_sshd_A + M25$f_per_group_pr_A[,9,5])+   # SShD (A): TUR More Freq. Users
    (M25$b_sshd_A + M25$f_per_group_pr_A[,11,5])   # SShD (A): COL More Freq. Users
)/6

Attr_SexTyp_F_INfreq <- (
  (M25$b_sshd_A + M25$f_per_group_pr_A[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (M25$b_sshd_A + M25$f_per_group_pr_A[,4,5])+   # SShD (A): COL More Freq. Users
    (M25$b_sshd_A + M25$f_per_group_pr_A[,6,5])+   # SShD (A): CZE More Freq. Users
    (M25$b_sshd_A + M25$f_per_group_pr_A[,8,5])+   # SShD (A): ZAF More Freq. Users
    (M25$b_sshd_A + M25$f_per_group_pr_A[,10,5])+   # SShD (A): TUR More Freq. Users
    (M25$b_sshd_A + M25$f_per_group_pr_A[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
Attr_L_F_freq <- (
  (M25$b_L_A + M25$f_per_group_pr_A[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (M25$b_L_A + M25$f_per_group_pr_A[,3,6])+   # L (A): COL More Freq. Users
    (M25$b_L_A + M25$f_per_group_pr_A[,5,6])+   # L (A): CZE More Freq. Users
    (M25$b_L_A + M25$f_per_group_pr_A[,7,6])+   # L (A): ZAF More Freq. Users
    (M25$b_L_A + M25$f_per_group_pr_A[,9,6])+   # L (A): TUR More Freq. Users
    (M25$b_L_A + M25$f_per_group_pr_A[,11,6])   # L (A): COL More Freq. Users
)/6

Attr_L_F_INfreq <- (
  (M25$b_L_A + M25$f_per_group_pr_A[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (M25$b_L_A + M25$f_per_group_pr_A[,4,6])+   # L (A): COL More Freq. Users
    (M25$b_L_A + M25$f_per_group_pr_A[,6,6])+   # L (A): CZE More Freq. Users
    (M25$b_L_A + M25$f_per_group_pr_A[,8,6])+   # L (A): ZAF More Freq. Users
    (M25$b_L_A + M25$f_per_group_pr_A[,10,6])+   # L (A): TUR More Freq. Users
    (M25$b_L_A + M25$f_per_group_pr_A[,12,6])   # L (A): COL More Freq. Users
)/6



# GMM - SEPARATELY 

# Attractiveness - GMM 
str(M25)
str(M25$f_per_group_pr_A) # Mind that 1 is intercept! 

F_posts_ATD_MM_Attr <- list(
  #  AGE
  Age_Slope = (M25$b_age_A),    # Slope prior to accessing culture
  
  Attr_Age_F_freq,
  Attr_Age_F_INfreq,
  
  Age_AUS_Freq = (M25$b_age_A + M25$f_per_group_pr_A[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (M25$b_age_A + M25$f_per_group_pr_A[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (M25$b_age_A + M25$f_per_group_pr_A[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (M25$b_age_A + M25$f_per_group_pr_A[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (M25$b_age_A + M25$f_per_group_pr_A[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (M25$b_age_A + M25$f_per_group_pr_A[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (M25$b_age_A + M25$f_per_group_pr_A[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (M25$b_age_A + M25$f_per_group_pr_A[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (M25$b_age_A + M25$f_per_group_pr_A[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (M25$b_age_A + M25$f_per_group_pr_A[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (M25$b_age_A + M25$f_per_group_pr_A[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (M25$b_age_A + M25$f_per_group_pr_A[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (M25$b_dist_A),    # Dist (A): CZE Control
  
  Attr_Dist_F_freq,
  Attr_Dist_F_INfreq,
  
  Dist_AUS_Freq = (M25$b_dist_A + M25$f_per_group_pr_A[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (M25$b_dist_A + M25$f_per_group_pr_A[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (M25$b_dist_A + M25$f_per_group_pr_A[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (M25$b_dist_A + M25$f_per_group_pr_A[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (M25$b_dist_A + M25$f_per_group_pr_A[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (M25$b_dist_A + M25$f_per_group_pr_A[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (M25$b_dist_A + M25$f_per_group_pr_A[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (M25$b_dist_A + M25$f_per_group_pr_A[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (M25$b_dist_A + M25$f_per_group_pr_A[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (M25$b_dist_A + M25$f_per_group_pr_A[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (M25$b_dist_A + M25$f_per_group_pr_A[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (M25$b_dist_A + M25$f_per_group_pr_A[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (M25$b_FA_A),    # Asym (A): CZE Control
  
  Attr_Asym_F_freq,
  Attr_Asym_F_INfreq,
  
  Asym_AUS_Freq = (M25$b_FA_A + M25$f_per_group_pr_A[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (M25$b_FA_A + M25$f_per_group_pr_A[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (M25$b_FA_A + M25$f_per_group_pr_A[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (M25$b_FA_A + M25$f_per_group_pr_A[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (M25$b_FA_A + M25$f_per_group_pr_A[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (M25$b_FA_A + M25$f_per_group_pr_A[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (M25$b_FA_A + M25$f_per_group_pr_A[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (M25$b_FA_A + M25$f_per_group_pr_A[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (M25$b_FA_A + M25$f_per_group_pr_A[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (M25$b_FA_A + M25$f_per_group_pr_A[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (M25$b_FA_A + M25$f_per_group_pr_A[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (M25$b_FA_A + M25$f_per_group_pr_A[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (M25$b_sshd_A),    # SexTyp (A): CZE Control
  
  Attr_SexTyp_F_freq,
  Attr_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (M25$b_sshd_A + M25$f_per_group_pr_A[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (M25$b_sshd_A + M25$f_per_group_pr_A[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (M25$b_sshd_A + M25$f_per_group_pr_A[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (M25$b_sshd_A + M25$f_per_group_pr_A[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (M25$b_sshd_A + M25$f_per_group_pr_A[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (M25$b_sshd_A + M25$f_per_group_pr_A[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (M25$b_sshd_A + M25$f_per_group_pr_A[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (M25$b_sshd_A + M25$f_per_group_pr_A[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (M25$b_sshd_A + M25$f_per_group_pr_A[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (M25$b_sshd_A + M25$f_per_group_pr_A[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (M25$b_sshd_A + M25$f_per_group_pr_A[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (M25$b_sshd_A + M25$f_per_group_pr_A[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (M25$b_L_A),    # L (A): CZE Control
  
  Attr_L_F_freq,
  Attr_L_F_INfreq,
  
  L_AUS_Freq = (M25$b_L_A + M25$f_per_group_pr_A[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (M25$b_L_A + M25$f_per_group_pr_A[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (M25$b_L_A + M25$f_per_group_pr_A[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (M25$b_L_A + M25$f_per_group_pr_A[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (M25$b_L_A + M25$f_per_group_pr_A[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (M25$b_L_A + M25$f_per_group_pr_A[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (M25$b_L_A + M25$f_per_group_pr_A[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (M25$b_L_A + M25$f_per_group_pr_A[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (M25$b_L_A + M25$f_per_group_pr_A[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (M25$b_L_A + M25$f_per_group_pr_A[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (M25$b_L_A + M25$f_per_group_pr_A[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (M25$b_L_A + M25$f_per_group_pr_A[,12,6])   # L (A): COL Less Freq. Users
)


summary.data.frame(F_posts_ATD_MM_Attr)
str(F_posts_ATD_MM_Attr) # Need 65 rows 


labs <- c("Fixed Slope",
          
          "Above (All)",
          "Below (All)",
          
          "AUS/NZE Above",
          "AUS/NZE Below",
          
          "COL Above",
          "COL Below",
          
          "CZE Above",
          "CZE Below",
          
          "ZAF Above",
          "ZAF Below",
          
          "TUR Above",
          "TUR Below",
          
          "VNM Above",
          "VNM Below"
)

cols<-c("#c56043", "#e04b5c", "#de5353", "#c25c42", "#d85652", "#c8585d", "#de534b", "#cd5440", "#d35d5e", "#c3615a", "#d46655", "#cf5552", "#c65f5d","#de534b", "#cd5440",
        "#856554", "#896757", "#815b69", "#8f6756", "#86554d", "#8a675d", "#85586a", "#805158", "#855d52", "#8f4e50", "#8e5960", "#80685a", "#75584c","#85586a", "#805158",
        "#85b68f", "#77bf98", "#86b18f", "#85b589", "#7eB68d", "#8bB37a", "#80B491", "#65Bd7d", "#81B784", "#8bb883", "#8ebf92", "#7bBe8f", "#91bd79","#81B784", "#8bb883",
        "#6b9168", "#69925b", "#689d55", "#6ba261", "#5aaa6b", "#599866", "#519c58", "#55a755", "#5caa53", "#5c9c60", "#588f5b", "#5e9d6f", "#5aa757","#5caa53", "#5c9c60", 
        "#64a9e6", "#79a5ee", "#809be9", "#6d9dfa", "#6899ec", "#6facde", "#77abed", "#63aaf0", "#63a4e4", "#74a5e3", "#79a9eb", "#6aaaf6", "#78a4dd","#6facde", "#77abed"
        
) 




tiff("M_ATD_GMM_SES_ver_1_no_COEFS.tiff",width=30,height=33,units="cm",res=600,compression="lzw")
layout(matrix(1:3,byrow=F,nrow=1), widths=c(1.1,0.7,0.7),heights=c(1))

par(mar=c(2.1, 14.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.2),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Attr: GMM - Males", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#3d465490", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#3d465490", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#3d465490", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#3d465490", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#3d465490", lwd = 2)


# Small squares...
rect(-0.24,12.5,0.19,15.25, col="#F0F0F0", border = F)
rect(-0.24,28,0.19,30.75, col="#F0F0F0", border = F)
rect(-0.24,43.5,0.19,46.25, col="#F0F0F0", border = F)
rect(-0.24,59,0.19,61.75, col="#F0F0F0", border = F)
rect(-0.24,74.5,0.19,77.25, col="#F0F0F0", border = F)

# 
segments(x0 = -0.25, y0 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), x1 = 0.2, 
         y1 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), col = "#40808080", lwd = 1.5)

# 1
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 0, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 15.5,
         col="#80808080",lty=2)

# 2
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 16, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 31,
         col="#80808080",lty=2)

# 3
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 31.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 46.5,
         col="#80808080",lty=2)

# 4 
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 47, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 62,
         col="#80808080",lty=2)

# 5
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 62.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 77.5,
         col="#80808080",lty=2)

labs <- rep(labs,5)

ys<-c(1:15,16.5:30.5,32:46,47.5:61.5,63:77)
ys<-rev(ys)


for(i in 1:length(F_posts_ATD_MM_Attr)){ 
  toplot<-F_posts_ATD_MM_Attr[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  textlab<-labs[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  axis(2, at=ycoord, labels=textlab, srt=45, las=2, cex.axis=1.2, tick=F)
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=5, lty=1, col=cols[i])
  segments(x0 = ci[1], y0 = ycoord-0.1, x1 = ci[1], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  segments(x0 = ci[2], y0 = ycoord-0.1, x1 = ci[2], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0=0.0, y0=c(0.0, 16, 31.5, 47, 62.5), x1=0.0, y1=c(15.5, 31, 46.5, 62, 77.5), col="#999999", lwd=2, lty=1)

text(x = -0.3, y = 77, labels = "Age", cex = 0.4, pos = 4)
text(x = -0.3, y = 62, labels = "Dist", cex = 0.4, pos = 4)
text(x = -0.3, y = 47, labels = "Asym", cex = 0.4, pos = 4)
text(x = -0.3, y = 32, labels = "SShD", cex = 0.4, pos = 4)
text(x = -0.3, y = 15, labels = "Lightness", cex = 0.4, pos = 4)

# 



# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 

# And now GMM: 
# Preparing composite coefficients: 
# AGE:
TR_Age_F_freq <- c(
  (M25$b_age_T + M25$f_per_group_pr_T[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (M25$b_age_T + M25$f_per_group_pr_T[,3,2])+   # Age (A): COL More Freq. Users
    (M25$b_age_T + M25$f_per_group_pr_T[,5,2])+   # Age (A): CZE More Freq. Users
    (M25$b_age_T + M25$f_per_group_pr_T[,7,2])+   # Age (A): ZAF More Freq. Users
    (M25$b_age_T + M25$f_per_group_pr_T[,9,2])+   # Age (A): TUR More Freq. Users
    (M25$b_age_T + M25$f_per_group_pr_T[,11,2])   # Age (A): COL More Freq. Users
)/6

TR_Age_F_INfreq <- (
  (M25$b_age_T + M25$f_per_group_pr_T[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (M25$b_age_T + M25$f_per_group_pr_T[,4,2])+   # Age (A): COL More Freq. Users
    (M25$b_age_T + M25$f_per_group_pr_T[,6,2])+   # Age (A): CZE More Freq. Users
    (M25$b_age_T + M25$f_per_group_pr_T[,8,2])+   # Age (A): ZAF More Freq. Users
    (M25$b_age_T + M25$f_per_group_pr_T[,10,2])+   # Age (A): TUR More Freq. Users
    (M25$b_age_T + M25$f_per_group_pr_T[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
TR_Dist_F_freq <- (
  (M25$b_dist_T + M25$f_per_group_pr_T[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (M25$b_dist_T + M25$f_per_group_pr_T[,3,3])+   # Dist (A): COL More Freq. Users
    (M25$b_dist_T + M25$f_per_group_pr_T[,5,3])+   # Dist (A): CZE More Freq. Users
    (M25$b_dist_T + M25$f_per_group_pr_T[,7,3])+   # Dist (A): ZAF More Freq. Users
    (M25$b_dist_T + M25$f_per_group_pr_T[,9,3])+   # Dist (A): TUR More Freq. Users
    (M25$b_dist_T + M25$f_per_group_pr_T[,11,3])   # Dist (A): COL More Freq. Users
)/6

TR_Dist_F_INfreq <- (
  (M25$b_dist_T + M25$f_per_group_pr_T[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (M25$b_dist_T + M25$f_per_group_pr_T[,4,3])+   # Dist (A): COL More Freq. Users
    (M25$b_dist_T + M25$f_per_group_pr_T[,6,3])+   # Dist (A): CZE More Freq. Users
    (M25$b_dist_T + M25$f_per_group_pr_T[,8,3])+   # Dist (A): ZAF More Freq. Users
    (M25$b_dist_T + M25$f_per_group_pr_T[,10,3])+   # Dist (A): TUR More Freq. Users
    (M25$b_dist_T + M25$f_per_group_pr_T[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
TR_Asym_F_freq <- (
  (M25$b_FA_T + M25$f_per_group_pr_T[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (M25$b_FA_T + M25$f_per_group_pr_T[,3,4])+   # Asym (A): COL More Freq. Users
    (M25$b_FA_T + M25$f_per_group_pr_T[,5,4])+   # Asym (A): CZE More Freq. Users
    (M25$b_FA_T + M25$f_per_group_pr_T[,7,4])+   # Asym (A): ZAF More Freq. Users
    (M25$b_FA_T + M25$f_per_group_pr_T[,9,4])+   # Asym (A): TUR More Freq. Users
    (M25$b_FA_T + M25$f_per_group_pr_T[,11,4])   # Asym (A): COL More Freq. Users
)/6

TR_Asym_F_INfreq <- (
  (M25$b_FA_T + M25$f_per_group_pr_T[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (M25$b_FA_T + M25$f_per_group_pr_T[,4,4])+   # Asym (A): COL More Freq. Users
    (M25$b_FA_T + M25$f_per_group_pr_T[,6,4])+   # Asym (A): CZE More Freq. Users
    (M25$b_FA_T + M25$f_per_group_pr_T[,8,4])+   # Asym (A): ZAF More Freq. Users
    (M25$b_FA_T + M25$f_per_group_pr_T[,10,4])+   # Asym (A): TUR More Freq. Users
    (M25$b_FA_T + M25$f_per_group_pr_T[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
TR_SexTyp_F_freq <- (
  (M25$b_sshd_T + M25$f_per_group_pr_T[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (M25$b_sshd_T + M25$f_per_group_pr_T[,3,5])+   # SShD (A): COL More Freq. Users
    (M25$b_sshd_T + M25$f_per_group_pr_T[,5,5])+   # SShD (A): CZE More Freq. Users
    (M25$b_sshd_T + M25$f_per_group_pr_T[,7,5])+   # SShD (A): ZAF More Freq. Users
    (M25$b_sshd_T + M25$f_per_group_pr_T[,9,5])+   # SShD (A): TUR More Freq. Users
    (M25$b_sshd_T + M25$f_per_group_pr_T[,11,5])   # SShD (A): COL More Freq. Users
)/6

TR_SexTyp_F_INfreq <- (
  (M25$b_sshd_T + M25$f_per_group_pr_T[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (M25$b_sshd_T + M25$f_per_group_pr_T[,4,5])+   # SShD (A): COL More Freq. Users
    (M25$b_sshd_T + M25$f_per_group_pr_T[,6,5])+   # SShD (A): CZE More Freq. Users
    (M25$b_sshd_T + M25$f_per_group_pr_T[,8,5])+   # SShD (A): ZAF More Freq. Users
    (M25$b_sshd_T + M25$f_per_group_pr_T[,10,5])+   # SShD (A): TUR More Freq. Users
    (M25$b_sshd_T + M25$f_per_group_pr_T[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
TR_L_F_freq <- (
  (M25$b_L_T + M25$f_per_group_pr_T[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (M25$b_L_T + M25$f_per_group_pr_T[,3,6])+   # L (A): COL More Freq. Users
    (M25$b_L_T + M25$f_per_group_pr_T[,5,6])+   # L (A): CZE More Freq. Users
    (M25$b_L_T + M25$f_per_group_pr_T[,7,6])+   # L (A): ZAF More Freq. Users
    (M25$b_L_T + M25$f_per_group_pr_T[,9,6])+   # L (A): TUR More Freq. Users
    (M25$b_L_T + M25$f_per_group_pr_T[,11,6])   # L (A): COL More Freq. Users
)/6

TR_L_F_INfreq <- (
  (M25$b_L_T + M25$f_per_group_pr_T[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (M25$b_L_T + M25$f_per_group_pr_T[,4,6])+   # L (A): COL More Freq. Users
    (M25$b_L_T + M25$f_per_group_pr_T[,6,6])+   # L (A): CZE More Freq. Users
    (M25$b_L_T + M25$f_per_group_pr_T[,8,6])+   # L (A): ZAF More Freq. Users
    (M25$b_L_T + M25$f_per_group_pr_T[,10,6])+   # L (A): TUR More Freq. Users
    (M25$b_L_T + M25$f_per_group_pr_T[,12,6])   # L (A): COL More Freq. Users
)/6



# Trusw - GMM 
str(M25)
str(M25$f_per_group_pr_T) # Mind that 1 is intercept! 

F_posts_ATD_MM_TRU <- list(
  #  AGE
  Age_Slope = (M25$b_age_T),    # Slope prior to accessing culture
  
  TR_Age_F_freq,
  TR_Age_F_INfreq,
  
  Age_AUS_Freq = (M25$b_age_T + M25$f_per_group_pr_T[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (M25$b_age_T + M25$f_per_group_pr_T[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (M25$b_age_T + M25$f_per_group_pr_T[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (M25$b_age_T + M25$f_per_group_pr_T[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (M25$b_age_T + M25$f_per_group_pr_T[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (M25$b_age_T + M25$f_per_group_pr_T[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (M25$b_age_T + M25$f_per_group_pr_T[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (M25$b_age_T + M25$f_per_group_pr_T[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (M25$b_age_T + M25$f_per_group_pr_T[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (M25$b_age_T + M25$f_per_group_pr_T[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (M25$b_age_T + M25$f_per_group_pr_T[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (M25$b_age_T + M25$f_per_group_pr_T[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (M25$b_dist_T),    # Dist (A): CZE Control
  
  TR_Dist_F_freq,
  TR_Dist_F_INfreq,
  
  Dist_AUS_Freq = (M25$b_dist_T + M25$f_per_group_pr_T[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (M25$b_dist_T + M25$f_per_group_pr_T[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (M25$b_dist_T + M25$f_per_group_pr_T[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (M25$b_dist_T + M25$f_per_group_pr_T[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (M25$b_dist_T + M25$f_per_group_pr_T[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (M25$b_dist_T + M25$f_per_group_pr_T[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (M25$b_dist_T + M25$f_per_group_pr_T[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (M25$b_dist_T + M25$f_per_group_pr_T[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (M25$b_dist_T + M25$f_per_group_pr_T[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (M25$b_dist_T + M25$f_per_group_pr_T[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (M25$b_dist_T + M25$f_per_group_pr_T[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (M25$b_dist_T + M25$f_per_group_pr_T[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (M25$b_FA_T),    # Asym (A): CZE Control
  
  TR_Asym_F_freq,
  TR_Asym_F_INfreq,
  
  Asym_AUS_Freq = (M25$b_FA_T + M25$f_per_group_pr_T[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (M25$b_FA_T + M25$f_per_group_pr_T[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (M25$b_FA_T + M25$f_per_group_pr_T[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (M25$b_FA_T + M25$f_per_group_pr_T[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (M25$b_FA_T + M25$f_per_group_pr_T[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (M25$b_FA_T + M25$f_per_group_pr_T[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (M25$b_FA_T + M25$f_per_group_pr_T[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (M25$b_FA_T + M25$f_per_group_pr_T[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (M25$b_FA_T + M25$f_per_group_pr_T[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (M25$b_FA_T + M25$f_per_group_pr_T[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (M25$b_FA_T + M25$f_per_group_pr_T[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (M25$b_FA_T + M25$f_per_group_pr_T[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (M25$b_sshd_T),    # SexTyp (A): CZE Control
  
  TR_SexTyp_F_freq,
  TR_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (M25$b_sshd_T + M25$f_per_group_pr_T[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (M25$b_sshd_T + M25$f_per_group_pr_T[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (M25$b_sshd_T + M25$f_per_group_pr_T[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (M25$b_sshd_T + M25$f_per_group_pr_T[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (M25$b_sshd_T + M25$f_per_group_pr_T[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (M25$b_sshd_T + M25$f_per_group_pr_T[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (M25$b_sshd_T + M25$f_per_group_pr_T[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (M25$b_sshd_T + M25$f_per_group_pr_T[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (M25$b_sshd_T + M25$f_per_group_pr_T[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (M25$b_sshd_T + M25$f_per_group_pr_T[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (M25$b_sshd_T + M25$f_per_group_pr_T[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (M25$b_sshd_T + M25$f_per_group_pr_T[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (M25$b_L_T),    # L (A): CZE Control
  
  TR_L_F_freq,
  TR_L_F_INfreq,
  
  L_AUS_Freq = (M25$b_L_T + M25$f_per_group_pr_T[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (M25$b_L_T + M25$f_per_group_pr_T[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (M25$b_L_T + M25$f_per_group_pr_T[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (M25$b_L_T + M25$f_per_group_pr_T[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (M25$b_L_T + M25$f_per_group_pr_T[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (M25$b_L_T + M25$f_per_group_pr_T[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (M25$b_L_T + M25$f_per_group_pr_T[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (M25$b_L_T + M25$f_per_group_pr_T[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (M25$b_L_T + M25$f_per_group_pr_T[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (M25$b_L_T + M25$f_per_group_pr_T[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (M25$b_L_T + M25$f_per_group_pr_T[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (M25$b_L_T + M25$f_per_group_pr_T[,12,6])   # L (A): COL Less Freq. Users
)


summary.data.frame(F_posts_ATD_MM_TRU)
str(F_posts_ATD_MM_TRU) # Need 65 rows 



par(mar=c(2.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.2),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Trustw: GMM - Males", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting


# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#3d465490", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#3d465490", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#3d465490", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#3d465490", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#3d465490", lwd = 2)


# Small squares...
rect(-0.24,12.5,0.19,15.25, col="#F0F0F0", border = F)
rect(-0.24,28,0.19,30.75, col="#F0F0F0", border = F)
rect(-0.24,43.5,0.19,46.25, col="#F0F0F0", border = F)
rect(-0.24,59,0.19,61.75, col="#F0F0F0", border = F)
rect(-0.24,74.5,0.19,77.25, col="#F0F0F0", border = F)

# 
segments(x0 = -0.25, y0 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), x1 = 0.2, 
         y1 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), col = "#40808080", lwd = 1.5)

# 1
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 0, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 15.5,
         col="#80808080",lty=2)

# 2
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 16, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 31,
         col="#80808080",lty=2)

# 3
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 31.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 46.5,
         col="#80808080",lty=2)

# 4 
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 47, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 62,
         col="#80808080",lty=2)

# 5
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 62.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 77.5,
         col="#80808080",lty=2)



ys<-c(1:15,16.5:30.5,32:46,47.5:61.5,63:77)
ys<-rev(ys)

labs <- rep(labs,5)


for(i in 1:length(F_posts_ATD_MM_TRU)){ 
  toplot<-F_posts_ATD_MM_TRU[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=5, lty=1, col=cols[i])
  segments(x0 = ci[1], y0 = ycoord-0.1, x1 = ci[1], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  segments(x0 = ci[2], y0 = ycoord-0.1, x1 = ci[2], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0=0.0, y0=c(0.0, 16, 31.5, 47, 62.5), x1=0.0, y1=c(15.5, 31, 46.5, 62, 77.5), col="#999999", lwd=2, lty=1)
# 





# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 

# And now GMM: 
# Preparing composite coefficients: 
# AGE:
DOM_Age_F_freq <- c(
  (M25$b_age_D + M25$f_per_group_pr_D[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (M25$b_age_D + M25$f_per_group_pr_D[,3,2])+   # Age (A): COL More Freq. Users
    (M25$b_age_D + M25$f_per_group_pr_D[,5,2])+   # Age (A): CZE More Freq. Users
    (M25$b_age_D + M25$f_per_group_pr_D[,7,2])+   # Age (A): ZAF More Freq. Users
    (M25$b_age_D + M25$f_per_group_pr_D[,9,2])+   # Age (A): TUR More Freq. Users
    (M25$b_age_D + M25$f_per_group_pr_D[,11,2])   # Age (A): COL More Freq. Users
)/6

DOM_Age_F_INfreq <- (
  (M25$b_age_D + M25$f_per_group_pr_D[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (M25$b_age_D + M25$f_per_group_pr_D[,4,2])+   # Age (A): COL More Freq. Users
    (M25$b_age_D + M25$f_per_group_pr_D[,6,2])+   # Age (A): CZE More Freq. Users
    (M25$b_age_D + M25$f_per_group_pr_D[,8,2])+   # Age (A): ZAF More Freq. Users
    (M25$b_age_D + M25$f_per_group_pr_D[,10,2])+   # Age (A): TUR More Freq. Users
    (M25$b_age_D + M25$f_per_group_pr_D[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
DOM_Dist_F_freq <- (
  (M25$b_dist_D + M25$f_per_group_pr_D[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (M25$b_dist_D + M25$f_per_group_pr_D[,3,3])+   # Dist (A): COL More Freq. Users
    (M25$b_dist_D + M25$f_per_group_pr_D[,5,3])+   # Dist (A): CZE More Freq. Users
    (M25$b_dist_D + M25$f_per_group_pr_D[,7,3])+   # Dist (A): ZAF More Freq. Users
    (M25$b_dist_D + M25$f_per_group_pr_D[,9,3])+   # Dist (A): TUR More Freq. Users
    (M25$b_dist_D + M25$f_per_group_pr_D[,11,3])   # Dist (A): COL More Freq. Users
)/6

DOM_Dist_F_INfreq <- (
  (M25$b_dist_D + M25$f_per_group_pr_D[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (M25$b_dist_D + M25$f_per_group_pr_D[,4,3])+   # Dist (A): COL More Freq. Users
    (M25$b_dist_D + M25$f_per_group_pr_D[,6,3])+   # Dist (A): CZE More Freq. Users
    (M25$b_dist_D + M25$f_per_group_pr_D[,8,3])+   # Dist (A): ZAF More Freq. Users
    (M25$b_dist_D + M25$f_per_group_pr_D[,10,3])+   # Dist (A): TUR More Freq. Users
    (M25$b_dist_D + M25$f_per_group_pr_D[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
DOM_Asym_F_freq <- (
  (M25$b_FA_D + M25$f_per_group_pr_D[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (M25$b_FA_D + M25$f_per_group_pr_D[,3,4])+   # Asym (A): COL More Freq. Users
    (M25$b_FA_D + M25$f_per_group_pr_D[,5,4])+   # Asym (A): CZE More Freq. Users
    (M25$b_FA_D + M25$f_per_group_pr_D[,7,4])+   # Asym (A): ZAF More Freq. Users
    (M25$b_FA_D + M25$f_per_group_pr_D[,9,4])+   # Asym (A): TUR More Freq. Users
    (M25$b_FA_D + M25$f_per_group_pr_D[,11,4])   # Asym (A): COL More Freq. Users
)/6

DOM_Asym_F_INfreq <- (
  (M25$b_FA_D + M25$f_per_group_pr_D[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (M25$b_FA_D + M25$f_per_group_pr_D[,4,4])+   # Asym (A): COL More Freq. Users
    (M25$b_FA_D + M25$f_per_group_pr_D[,6,4])+   # Asym (A): CZE More Freq. Users
    (M25$b_FA_D + M25$f_per_group_pr_D[,8,4])+   # Asym (A): ZAF More Freq. Users
    (M25$b_FA_D + M25$f_per_group_pr_D[,10,4])+   # Asym (A): TUR More Freq. Users
    (M25$b_FA_D + M25$f_per_group_pr_D[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
DOM_SexTyp_F_freq <- (
  (M25$b_sshd_D + M25$f_per_group_pr_D[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (M25$b_sshd_D + M25$f_per_group_pr_D[,3,5])+   # SShD (A): COL More Freq. Users
    (M25$b_sshd_D + M25$f_per_group_pr_D[,5,5])+   # SShD (A): CZE More Freq. Users
    (M25$b_sshd_D + M25$f_per_group_pr_D[,7,5])+   # SShD (A): ZAF More Freq. Users
    (M25$b_sshd_D + M25$f_per_group_pr_D[,9,5])+   # SShD (A): TUR More Freq. Users
    (M25$b_sshd_D + M25$f_per_group_pr_D[,11,5])   # SShD (A): COL More Freq. Users
)/6

DOM_SexTyp_F_INfreq <- (
  (M25$b_sshd_D + M25$f_per_group_pr_D[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (M25$b_sshd_D + M25$f_per_group_pr_D[,4,5])+   # SShD (A): COL More Freq. Users
    (M25$b_sshd_D + M25$f_per_group_pr_D[,6,5])+   # SShD (A): CZE More Freq. Users
    (M25$b_sshd_D + M25$f_per_group_pr_D[,8,5])+   # SShD (A): ZAF More Freq. Users
    (M25$b_sshd_D + M25$f_per_group_pr_D[,10,5])+   # SShD (A): TUR More Freq. Users
    (M25$b_sshd_D + M25$f_per_group_pr_D[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
DOM_L_F_freq <- (
  (M25$b_L_D + M25$f_per_group_pr_D[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (M25$b_L_D + M25$f_per_group_pr_D[,3,6])+   # L (A): COL More Freq. Users
    (M25$b_L_D + M25$f_per_group_pr_D[,5,6])+   # L (A): CZE More Freq. Users
    (M25$b_L_D + M25$f_per_group_pr_D[,7,6])+   # L (A): ZAF More Freq. Users
    (M25$b_L_D + M25$f_per_group_pr_D[,9,6])+   # L (A): TUR More Freq. Users
    (M25$b_L_D + M25$f_per_group_pr_D[,11,6])   # L (A): COL More Freq. Users
)/6

DOM_L_F_INfreq <- (
  (M25$b_L_D + M25$f_per_group_pr_D[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (M25$b_L_D + M25$f_per_group_pr_D[,4,6])+   # L (A): COL More Freq. Users
    (M25$b_L_D + M25$f_per_group_pr_D[,6,6])+   # L (A): CZE More Freq. Users
    (M25$b_L_D + M25$f_per_group_pr_D[,8,6])+   # L (A): ZAF More Freq. Users
    (M25$b_L_D + M25$f_per_group_pr_D[,10,6])+   # L (A): TUR More Freq. Users
    (M25$b_L_D + M25$f_per_group_pr_D[,12,6])   # L (A): COL More Freq. Users
)/6



# GMM - SEPARATELY 

# Dominance - GMM 
str(M25)
str(M25$f_per_group_pr_D) # Mind that 1 is intercept! 

F_posts_ATD_MM_DOM <- list(
  #  AGE
  Age_Slope = (M25$b_age_D),    # Slope prior to accessing culture
  
  DOM_Age_F_freq,
  DOM_Age_F_INfreq,
  
  Age_AUS_Freq = (M25$b_age_D + M25$f_per_group_pr_D[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (M25$b_age_D + M25$f_per_group_pr_D[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (M25$b_age_D + M25$f_per_group_pr_D[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (M25$b_age_D + M25$f_per_group_pr_D[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (M25$b_age_D + M25$f_per_group_pr_D[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (M25$b_age_D + M25$f_per_group_pr_D[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (M25$b_age_D + M25$f_per_group_pr_D[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (M25$b_age_D + M25$f_per_group_pr_D[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (M25$b_age_D + M25$f_per_group_pr_D[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (M25$b_age_D + M25$f_per_group_pr_D[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (M25$b_age_D + M25$f_per_group_pr_D[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (M25$b_age_D + M25$f_per_group_pr_D[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (M25$b_dist_D),    # Dist (A): CZE Control
  
  DOM_Dist_F_freq,
  DOM_Dist_F_INfreq,
  
  Dist_AUS_Freq = (M25$b_dist_D + M25$f_per_group_pr_D[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (M25$b_dist_D + M25$f_per_group_pr_D[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (M25$b_dist_D + M25$f_per_group_pr_D[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (M25$b_dist_D + M25$f_per_group_pr_D[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (M25$b_dist_D + M25$f_per_group_pr_D[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (M25$b_dist_D + M25$f_per_group_pr_D[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (M25$b_dist_D + M25$f_per_group_pr_D[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (M25$b_dist_D + M25$f_per_group_pr_D[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (M25$b_dist_D + M25$f_per_group_pr_D[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (M25$b_dist_D + M25$f_per_group_pr_D[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (M25$b_dist_D + M25$f_per_group_pr_D[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (M25$b_dist_D + M25$f_per_group_pr_D[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (M25$b_FA_D),    # Asym (A): CZE Control
  
  DOM_Asym_F_freq,
  DOM_Asym_F_INfreq,
  
  Asym_AUS_Freq = (M25$b_FA_D + M25$f_per_group_pr_D[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (M25$b_FA_D + M25$f_per_group_pr_D[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (M25$b_FA_D + M25$f_per_group_pr_D[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (M25$b_FA_D + M25$f_per_group_pr_D[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (M25$b_FA_D + M25$f_per_group_pr_D[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (M25$b_FA_D + M25$f_per_group_pr_D[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (M25$b_FA_D + M25$f_per_group_pr_D[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (M25$b_FA_D + M25$f_per_group_pr_D[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (M25$b_FA_D + M25$f_per_group_pr_D[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (M25$b_FA_D + M25$f_per_group_pr_D[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (M25$b_FA_D + M25$f_per_group_pr_D[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (M25$b_FA_D + M25$f_per_group_pr_D[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (M25$b_sshd_D),    # SexTyp (A): CZE Control
  
  DOM_SexTyp_F_freq,
  DOM_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (M25$b_sshd_D + M25$f_per_group_pr_D[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (M25$b_sshd_D + M25$f_per_group_pr_D[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (M25$b_sshd_D + M25$f_per_group_pr_D[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (M25$b_sshd_D + M25$f_per_group_pr_D[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (M25$b_sshd_D + M25$f_per_group_pr_D[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (M25$b_sshd_D + M25$f_per_group_pr_D[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (M25$b_sshd_D + M25$f_per_group_pr_D[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (M25$b_sshd_D + M25$f_per_group_pr_D[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (M25$b_sshd_D + M25$f_per_group_pr_D[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (M25$b_sshd_D + M25$f_per_group_pr_D[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (M25$b_sshd_D + M25$f_per_group_pr_D[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (M25$b_sshd_D + M25$f_per_group_pr_D[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (M25$b_L_D),    # L (A): CZE Control
  
  DOM_L_F_freq,
  DOM_L_F_INfreq,
  
  L_AUS_Freq = (M25$b_L_D + M25$f_per_group_pr_D[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (M25$b_L_D + M25$f_per_group_pr_D[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (M25$b_L_D + M25$f_per_group_pr_D[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (M25$b_L_D + M25$f_per_group_pr_D[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (M25$b_L_D + M25$f_per_group_pr_D[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (M25$b_L_D + M25$f_per_group_pr_D[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (M25$b_L_D + M25$f_per_group_pr_D[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (M25$b_L_D + M25$f_per_group_pr_D[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (M25$b_L_D + M25$f_per_group_pr_D[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (M25$b_L_D + M25$f_per_group_pr_D[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (M25$b_L_D + M25$f_per_group_pr_D[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (M25$b_L_D + M25$f_per_group_pr_D[,12,6])   # L (A): COL Less Freq. Users
)


summary.data.frame(F_posts_ATD_MM_DOM)
str(F_posts_ATD_MM_TRU) # Need 65 rows 



par(mar=c(2.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.2),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Dominance: GMM - Males", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting


# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#3d465490", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#3d465490", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#3d465490", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#3d465490", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#3d465490", lwd = 2)


# Small squares...
rect(-0.24,12.5,0.19,15.25, col="#F0F0F0", border = F)
rect(-0.24,28,0.19,30.75, col="#F0F0F0", border = F)
rect(-0.24,43.5,0.19,46.25, col="#F0F0F0", border = F)
rect(-0.24,59,0.19,61.75, col="#F0F0F0", border = F)
rect(-0.24,74.5,0.19,77.25, col="#F0F0F0", border = F)

# 
segments(x0 = -0.25, y0 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), x1 = 0.2, 
         y1 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), col = "#40808080", lwd = 1.5)

# 1
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 0, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 15.5,
         col="#80808080",lty=2)

# 2
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 16, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 31,
         col="#80808080",lty=2)

# 3
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 31.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 46.5,
         col="#80808080",lty=2)

# 4 
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 47, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 62,
         col="#80808080",lty=2)

# 5
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 62.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 77.5,
         col="#80808080",lty=2)



ys<-c(1:15,16.5:30.5,32:46,47.5:61.5,63:77)
ys<-rev(ys)

labs <- rep(labs,5)

cex_t <- 1.25
cex_t2 <- 1
pls <- 1

for(i in 1:length(F_posts_ATD_MM_DOM)){ 
  toplot<-F_posts_ATD_MM_DOM[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=5, lty=1, col=cols[i])
  segments(x0 = ci[1], y0 = ycoord-0.1, x1 = ci[1], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  segments(x0 = ci[2], y0 = ycoord-0.1, x1 = ci[2], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0=0.0, y0=c(0.0, 16, 31.5, 47, 62.5), x1=0.0, y1=c(15.5, 31, 46.5, 62, 77.5), col="#999999", lwd=2, lty=1)
# 


dev.off()


# Correlation comparisons: 

#-----
# Correlations fundamentally - SES: 
# Correlations fundamentally - SES: 
# Correlations fundamentally - SES: 
# Correlations fundamentally - SES: 
# Correlations fundamentally - SES: 
# Correlations fundamentally - SES: 
# Correlations fundamentally - SES: 
# Correlations fundamentally - SES: 
#-----

# Generate a random correlation matrix
# set.seed(42)
# n <- 12
# A <- matrix(runif(n^2, -1, 1), n, n)
# corr_mat <- (A + t(A)) / 2  # Make it symmetric
# diag(corr_mat) <- 1  # Set diagonal to 1

# Define group labels
# countries <- c("A", "B", "C", "D", "E", "F")
# usage <- c("h", "l")
# groups <- as.vector(t(outer(countries, usage, paste0)))
# rownames(corr_mat) <- colnames(corr_mat) <- groups

# Extract correlations based on color groups
all<-t(combn(1:12, 2)) #all possible combinations7
odd<-all %% 2 #is the umber odd?
both.same<-odd[,1]==odd[,2] #are both the same? both odd or even?
nodd<-rowSums(odd) #is the number of odd numbers odd? 

# Define groups by their coordinates
within<-matrix(c(1,2,3,4,5,6,7,8,9,10,11,12),ncol=2,byrow=T) #within country, red
between<-all[both.same==F & !(apply(all,1,function(row) paste(row, collapse = " ")) 
                              %in% apply(within,1,function(row) paste(row, collapse = " "))),] #between countries, but one group high usage, one group low usage, green

high<-all[both.same==T & nodd==2,] #high usage, yellow
low<-all[both.same==T & nodd==0,] #low usage, blue

#Sanity check, each used once
nrow(all)==sum(c(nrow(within),nrow(between),nrow(high),nrow(low)))

# Females - attractiveness

# Number of posterior samples
n_samples <- dim(F25$Rho_FSMUi_A)[1]


# BETWEEN! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(between), function(i) {
  F25$Rho_FSMUi_A[, between[i,1], between[i,2]]
})

# Convert to matrix to preserve structure
between_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(between))

# Is that correct? 
cor(F25$Rho_FSMUi_A[1:100,1,4],between_mat[1:100,1])
cor(F25$Rho_FSMUi_A[1:100,4,7],between_mat[1:100,16])
cor(F25$Rho_FSMUi_A[1:100,9,12],between_mat[1:100,29])
# Yes, Petr-Chat Rulez...

# WITHIN
selected_correlations <- sapply(1:nrow(within), function(i) {
  F25$Rho_FSMUi_A[, within[i,1], within[i,2]]
})

within_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(within))

cor(F25$Rho_FSMUi_A[1:100,1,2],within_mat[1:100,1])
cor(F25$Rho_FSMUi_A[1:100,3,4],within_mat[1:100,2])
cor(F25$Rho_FSMUi_A[1:100,9,10],within_mat[1:100,5])

# 1 both posteriors as vector - subset randomly from the longer... 
mean(as.vector(within_mat) - sample(as.vector(between_mat),4900))

# 2 within take as the whole, from between - sample 6 columns 
mean(as.vector(within_mat) - as.vector(between_mat[,sample(1:30,6,replace=F)]))

# Repeat 2 100-times:
(mean(as.vector(within_mat) - as.vector(between_mat[,sample(1:30,6,replace=F)])))
A <- replicate(1000,(mean(as.vector(within_mat) - as.vector(between_mat[,sample(1:30,6,replace=F)]))))
mean(A)

# We want this: 
# 3 create rowmeans and then compare thee
mean(rowMeans(within_mat)-rowMeans(between_mat))
PI(rowMeans(within_mat)-rowMeans(between_mat),prob=0.95)

mean(rowMeans(within_mat)); PI(rowMeans(within_mat), prob=0.95)
mean(rowMeans(between_mat)); PI(rowMeans(between_mat), prob=0.95) 



#Question 2:
# HIGH! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(high), function(i) {
  F25$Rho_FSMUi_A[, high[i,1], high[i,2]]
})

# Convert to matrix to preserve structure
high_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(high))

# Is that correct? 
cor(F25$Rho_FSMUi_A[1:100,1,3],high_mat[1:100,1])
cor(F25$Rho_FSMUi_A[1:100,3,7],high_mat[1:100,7])
cor(F25$Rho_FSMUi_A[1:100,9,11],high_mat[1:100,15])
# Yes, Petr-Chat Rulez...

# LOW
selected_correlations <- sapply(1:nrow(low), function(i) {
  F25$Rho_FSMUi_A[, low[i,1], low[i,2]]
})

low_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(low))

cor(F25$Rho_FSMUi_A[1:100,2,4],low_mat[1:100,1])
cor(F25$Rho_FSMUi_A[1:100,4,12],low_mat[1:100,9])
cor(F25$Rho_FSMUi_A[1:100,10,12],low_mat[1:100,15])

#difference of the averages

# 1 both posteriors as vector - subset randomly from the longer... 
mean(as.vector(high_mat) - as.vector(low_mat))

# 2 create rowmeans and then compare thee
mean(rowMeans(high_mat)-rowMeans(low_mat))

PI(rowMeans(high_mat)-rowMeans(low_mat),0.95)

mean(rowMeans(high_mat)); PI(rowMeans(high_mat), prob=0.95)
mean(rowMeans(low_mat)); PI(rowMeans(low_mat), prob=0.95) 



# Trustworthiness: 
# BETWEEN! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(between), function(i) {
  F25$Rho_FSMUi_T[, between[i,1], between[i,2]]
})

# Convert to matrix to preserve structure
between_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(between))

# Is that correct? 
cor(F25$Rho_FSMUi_T[1:100,1,4],between_mat[1:100,1])
cor(F25$Rho_FSMUi_T[1:100,4,7],between_mat[1:100,16])
cor(F25$Rho_FSMUi_T[1:100,9,12],between_mat[1:100,29])

# WITHIN
selected_correlations <- sapply(1:nrow(within), function(i) {
  F25$Rho_FSMUi_T[, within[i,1], within[i,2]]
})

within_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(within))

cor(F25$Rho_FSMUi_T[1:100,1,2],within_mat[1:100,1])
cor(F25$Rho_FSMUi_T[1:100,3,4],within_mat[1:100,2])
cor(F25$Rho_FSMUi_T[1:100,9,10],within_mat[1:100,5])

# 1 both posteriors as vector - subset randomly from the longer... 
mean(as.vector(within_mat) - sample(as.vector(between_mat),4900))

# 2 within take as the whole, from between - sample 6 columns 
mean(as.vector(within_mat) - as.vector(between_mat[,sample(1:30,6,replace=F)]))

# Repeat 2 100-times:
(mean(as.vector(within_mat) - as.vector(between_mat[,sample(1:30,6,replace=F)])))
A <- replicate(1000,(mean(as.vector(within_mat) - as.vector(between_mat[,sample(1:30,6,replace=F)]))))
mean(A)

# We want this: 
# 3 create rowmeans and then compare thee
mean(rowMeans(within_mat)-rowMeans(between_mat))
PI(rowMeans(within_mat)-rowMeans(between_mat),prob=0.95)

mean(rowMeans(within_mat)); PI(rowMeans(within_mat), prob=0.95)
mean(rowMeans(between_mat)); PI(rowMeans(between_mat), prob=0.95) 



#Question 2: 
# HIGH! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(high), function(i) {
  F25$Rho_FSMUi_T[, high[i,1], high[i,2]]
})

# Convert to matrix to preserve structure
high_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(high))

# Is that correct? 
cor(F25$Rho_FSMUi_T[1:100,1,3],high_mat[1:100,1])
cor(F25$Rho_FSMUi_T[1:100,3,7],high_mat[1:100,7])
cor(F25$Rho_FSMUi_T[1:100,9,11],high_mat[1:100,15])

# LOW
selected_correlations <- sapply(1:nrow(low), function(i) {
  F25$Rho_FSMUi_T[, low[i,1], low[i,2]]
})

low_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(low))

cor(F25$Rho_FSMUi_T[1:100,2,4],low_mat[1:100,1])
cor(F25$Rho_FSMUi_T[1:100,4,12],low_mat[1:100,9])
cor(F25$Rho_FSMUi_T[1:100,10,12],low_mat[1:100,15])

#difference of the averages

# 1 both posteriors as vector - subset randomly from the longer... 
mean(as.vector(high_mat) - as.vector(low_mat))

# 2 create rowmeans and then compare thee
mean(rowMeans(high_mat)-rowMeans(low_mat))

PI(rowMeans(high_mat)-rowMeans(low_mat),0.95)

mean(rowMeans(high_mat)); PI(rowMeans(high_mat), prob=0.95)
mean(rowMeans(low_mat)); PI(rowMeans(low_mat), prob=0.95) 



# Dominance: 
# BETWEEN! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(between), function(i) {
  F25$Rho_FSMUi_D[, between[i,1], between[i,2]]
})

# Convert to matrix to preserve structure
between_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(between))

# Is that correct? 
cor(F25$Rho_FSMUi_D[1:100,1,4],between_mat[1:100,1])
cor(F25$Rho_FSMUi_D[1:100,4,7],between_mat[1:100,16])
cor(F25$Rho_FSMUi_D[1:100,9,12],between_mat[1:100,29])

# WITHIN
selected_correlations <- sapply(1:nrow(within), function(i) {
  F25$Rho_FSMUi_D[, within[i,1], within[i,2]]
})

within_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(within))

cor(F25$Rho_FSMUi_D[1:100,1,2],within_mat[1:100,1])
cor(F25$Rho_FSMUi_D[1:100,3,4],within_mat[1:100,2])
cor(F25$Rho_FSMUi_D[1:100,9,10],within_mat[1:100,5])

# 1 both posteriors as vector - subset randomly from the longer... 
mean(as.vector(within_mat) - sample(as.vector(between_mat),4900))

# 2 within take as the whole, from between - sample 6 columns 
mean(as.vector(within_mat) - as.vector(between_mat[,sample(1:30,6,replace=F)]))

# Repeat 2 100-times:
(mean(as.vector(within_mat) - as.vector(between_mat[,sample(1:30,6,replace=F)])))
A <- replicate(1000,(mean(as.vector(within_mat) - as.vector(between_mat[,sample(1:30,6,replace=F)]))))
mean(A)

# We want this: 
# 3 create rowmeans and then compare thee
mean(rowMeans(within_mat)-rowMeans(between_mat))
PI(rowMeans(within_mat)-rowMeans(between_mat),prob=0.95)

mean(rowMeans(within_mat)); PI(rowMeans(within_mat), prob=0.95)
mean(rowMeans(between_mat)); PI(rowMeans(between_mat), prob=0.95) 



#Question 2: 
# HIGH! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(high), function(i) {
  F25$Rho_FSMUi_T[, high[i,1], high[i,2]]
})

# Convert to matrix to preserve structure
high_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(high))

# Is that correct? 
cor(F25$Rho_FSMUi_D[1:100,1,3],high_mat[1:100,1])
cor(F25$Rho_FSMUi_D[1:100,3,7],high_mat[1:100,7])
cor(F25$Rho_FSMUi_D[1:100,9,11],high_mat[1:100,15])
# Yes, Petr-Chat Rulez...

# LOW
selected_correlations <- sapply(1:nrow(low), function(i) {
  F25$Rho_FSMUi_D[, low[i,1], low[i,2]]
})

low_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(low))

cor(F25$Rho_FSMUi_D[1:100,2,4],low_mat[1:100,1])
cor(F25$Rho_FSMUi_D[1:100,4,12],low_mat[1:100,9])
cor(F25$Rho_FSMUi_D[1:100,10,12],low_mat[1:100,15])

#difference of the averages

# 1 both posteriors as vector - subset randomly from the longer... 
mean(as.vector(high_mat) - as.vector(low_mat))

# 2 create rowmeans and then compare thee
mean(rowMeans(high_mat)-rowMeans(low_mat))

PI(rowMeans(high_mat)-rowMeans(low_mat),0.95)

mean(rowMeans(high_mat)); PI(rowMeans(high_mat), prob=0.95)
mean(rowMeans(low_mat)); PI(rowMeans(low_mat), prob=0.95) 



# Between scales: 
F_precis <- precis(M25, depth=3, prob=0.95)

F25$Rho
F_precis["Rho_Scales[1,2]",]
F_precis["Rho_Scales[1,3]",]
F_precis["Rho_Scales[2,3]",]




# Males 
# Males 
# Males 
# Males 
# Males 
# Males 
# Males 
# Males 


# Males - attractiveness

# Number of posterior samples
n_samples <- dim(M25$Rho_FSMUi_A)[1]


# BETWEEN! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(between), function(i) {
  M25$Rho_FSMUi_A[, between[i,1], between[i,2]]
})

# Convert to matrix to preserve structure
between_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(between))

# Is that correct? 
cor(M25$Rho_FSMUi_A[1:100,1,4],between_mat[1:100,1])
cor(M25$Rho_FSMUi_A[1:100,4,7],between_mat[1:100,16])
cor(M25$Rho_FSMUi_A[1:100,9,12],between_mat[1:100,29])
# Yes, Petr-Chat Rulez...

# WITHIN
selected_correlations <- sapply(1:nrow(within), function(i) {
  M25$Rho_FSMUi_A[, within[i,1], within[i,2]]
})

within_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(within))

cor(M25$Rho_FSMUi_A[1:100,1,2],within_mat[1:100,1])
cor(M25$Rho_FSMUi_A[1:100,3,4],within_mat[1:100,2])
cor(M25$Rho_FSMUi_A[1:100,9,10],within_mat[1:100,5])

# We want this: Create rowmeans and then compare thee
mean(rowMeans(within_mat)-rowMeans(between_mat))
PI(rowMeans(within_mat)-rowMeans(between_mat),prob=0.95)

mean(rowMeans(within_mat)); PI(rowMeans(within_mat), prob=0.95)
mean(rowMeans(between_mat)); PI(rowMeans(between_mat), prob=0.95) 



#Question 2: 
# HIGH! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(high), function(i) {
  M25$Rho_FSMUi_A[, high[i,1], high[i,2]]
})

# Convert to matrix to preserve structure
high_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(high))

# Is that correct? 
cor(M25$Rho_FSMUi_A[1:100,1,3],high_mat[1:100,1])
cor(M25$Rho_FSMUi_A[1:100,9,11],high_mat[1:100,15])

# LOW
selected_correlations <- sapply(1:nrow(low), function(i) {
  M25$Rho_FSMUi_A[, low[i,1], low[i,2]]
})

low_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(low))

cor(M25$Rho_FSMUi_A[1:100,2,4],low_mat[1:100,1])
cor(M25$Rho_FSMUi_A[1:100,4,12],low_mat[1:100,9])
cor(M25$Rho_FSMUi_A[1:100,10,12],low_mat[1:100,15])

#difference of the averages

# 1 both posteriors as vector - subset randomly from the longer... 
mean(as.vector(high_mat) - as.vector(low_mat))

# 2 create rowmeans and then compare thee
mean(rowMeans(high_mat)-rowMeans(low_mat))

PI(rowMeans(high_mat)-rowMeans(low_mat),0.95)

mean(rowMeans(high_mat)); PI(rowMeans(high_mat), prob=0.95)
mean(rowMeans(low_mat)); PI(rowMeans(low_mat), prob=0.95) 



# Trustworthiness: 
# BETWEEN! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(between), function(i) {
  M25$Rho_FSMUi_T[, between[i,1], between[i,2]]
})

# Convert to matrix to preserve structure
between_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(between))

# Is that correct? 
cor(M25$Rho_FSMUi_T[1:100,1,4],between_mat[1:100,1])
cor(M25$Rho_FSMUi_T[1:100,4,7],between_mat[1:100,16])
cor(M25$Rho_FSMUi_T[1:100,9,12],between_mat[1:100,29])
# Yes, Petr-Chat Rulez...

# WITHIN
selected_correlations <- sapply(1:nrow(within), function(i) {
  M25$Rho_FSMUi_T[, within[i,1], within[i,2]]
})

within_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(within))

cor(M25$Rho_FSMUi_T[1:100,1,2],within_mat[1:100,1])
cor(M25$Rho_FSMUi_T[1:100,3,4],within_mat[1:100,2])
cor(M25$Rho_FSMUi_T[1:100,9,10],within_mat[1:100,5])

# create rowmeans and then compare thee
mean(rowMeans(within_mat)-rowMeans(between_mat))
PI(rowMeans(within_mat)-rowMeans(between_mat),prob=0.95)


mean(rowMeans(within_mat)); PI(rowMeans(within_mat), prob=0.95)
mean(rowMeans(between_mat)); PI(rowMeans(between_mat), prob=0.95) 



#Question 2: 
# HIGH! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(high), function(i) {
  M25$Rho_FSMUi_T[, high[i,1], high[i,2]]
})

# Convert to matrix to preserve structure
high_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(high))

# Is that correct? 
cor(M25$Rho_FSMUi_T[1:100,1,3],high_mat[1:100,1])
cor(M25$Rho_FSMUi_T[1:100,3,7],high_mat[1:100,7])
cor(M25$Rho_FSMUi_T[1:100,9,11],high_mat[1:100,15])

# LOW
selected_correlations <- sapply(1:nrow(low), function(i) {
  M25$Rho_FSMUi_T[, low[i,1], low[i,2]]
})

low_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(low))

cor(M25$Rho_FSMUi_T[1:100,2,4],low_mat[1:100,1])
cor(M25$Rho_FSMUi_T[1:100,4,12],low_mat[1:100,9])
cor(M25$Rho_FSMUi_T[1:100,10,12],low_mat[1:100,15])

#difference of the averages

# 1 both posteriors as vector - subset randomly from the longer... 
mean(as.vector(high_mat) - as.vector(low_mat))

# 2 create rowmeans and then compare thee
mean(rowMeans(high_mat)-rowMeans(low_mat))

PI(rowMeans(high_mat)-rowMeans(low_mat),0.95)

mean(rowMeans(high_mat)); PI(rowMeans(high_mat), prob=0.95)
mean(rowMeans(low_mat)); PI(rowMeans(low_mat), prob=0.95) 



# Dominance: 
# BETWEEN! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(between), function(i) {
  M25$Rho_FSMUi_D[, between[i,1], between[i,2]]
})

# Convert to matrix to preserve structure
between_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(between))

# Is that correct? 
cor(M25$Rho_FSMUi_D[1:100,1,4],between_mat[1:100,1])
cor(M25$Rho_FSMUi_D[1:100,4,7],between_mat[1:100,16])
cor(M25$Rho_FSMUi_D[1:100,9,12],between_mat[1:100,29])


# WITHIN
selected_correlations <- sapply(1:nrow(within), function(i) {
  M25$Rho_FSMUi_D[, within[i,1], within[i,2]]
})

within_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(within))

cor(M25$Rho_FSMUi_D[1:100,1,2],within_mat[1:100,1])
cor(M25$Rho_FSMUi_D[1:100,3,4],within_mat[1:100,2])
cor(M25$Rho_FSMUi_D[1:100,9,10],within_mat[1:100,5])

# create rowmeans and then compare thee
mean(rowMeans(within_mat)-rowMeans(between_mat))
PI(rowMeans(within_mat)-rowMeans(between_mat),prob=0.95)

mean(rowMeans(within_mat)); PI(rowMeans(within_mat), prob=0.95)
mean(rowMeans(between_mat)); PI(rowMeans(between_mat), prob=0.95) 



#Question 2: 
# HIGH! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(high), function(i) {
  M25$Rho_FSMUi_D[, high[i,1], high[i,2]]
})

# Convert to matrix to preserve structure
high_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(high))

# Is that correct? 
cor(M25$Rho_FSMUi_D[1:100,1,3],high_mat[1:100,1])
cor(M25$Rho_FSMUi_D[1:100,3,7],high_mat[1:100,7])
cor(M25$Rho_FSMUi_D[1:100,9,11],high_mat[1:100,15])


# LOW
selected_correlations <- sapply(1:nrow(low), function(i) {
  M25$Rho_FSMUi_D[, low[i,1], low[i,2]]
})

low_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(low))

cor(M25$Rho_FSMUi_D[1:100,2,4],low_mat[1:100,1])
cor(M25$Rho_FSMUi_D[1:100,4,12],low_mat[1:100,9])
cor(M25$Rho_FSMUi_D[1:100,10,12],low_mat[1:100,15])

#difference of the averages

# 1 both posteriors as vector - subset randomly from the longer... 
mean(as.vector(high_mat) - as.vector(low_mat))

# 2 create rowmeans and then compare thee
mean(rowMeans(high_mat)-rowMeans(low_mat))

PI(rowMeans(high_mat)-rowMeans(low_mat),0.95)

mean(rowMeans(high_mat)); PI(rowMeans(high_mat), prob=0.95)
mean(rowMeans(low_mat)); PI(rowMeans(low_mat), prob=0.95) 



# Between scales: 
M_precis <- precis(M25, depth=3, prob=0.95)

M25$Rho
M_precis["Rho_Scales[1,2]",]
M_precis["Rho_Scales[1,3]",]
M_precis["Rho_Scales[2,3]",]





#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# Ver 2
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

rm(list=ls())


#.-.-.-.-.-.-.-.-.-.-.-.
# 
# Travel abroad - participants were splitted in two groups. 
# Participants who reported they travel abroad "Ofter", "Rather Often" and occasionally "Occasionaly"
# Are in one group, participants, who reported they travel abroad "Rarely", "Very rarely" or "Never" (NAs excluded)
# Are in the other.
#
#.-.-.-.-.-.-.-.-.-.-.-.

F25 <- readRDS("post_F_ATD_FAMI_BACK_three_Upper.RDS")

str(F25)

M25 <- readRDS("post_M_ATD_FAMI_BACK_three_Upper.RDS")

str(M25)


# Levels (above means the group that travels more, below means the group that travel less)

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


# Females 

labels_x <- c("AUS/NZE Above", "AUS/NZE Below", "COL Above","COL Below","CZE Above", "CZE Below","ZAF Above", "ZAF Below",
              "TUR Above", "TUR Below", "VNM Above", "VNM Below")
labels_y <- labels_x

str(F25$Rho_FSMUi_A)

library(rethinking)
library(qgraph)
library(bootnet)
library(beepr)
library(ComplexHeatmap)
library(circlize)
library(grid)


# Make it as a 3×1 Matrix
#  ATTRACTIVENESS:F

cor_table_mean <- apply(F25$Rho_FSMUi_A,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(F25$Rho_FSMUi_A,c(2,3),PI, prob=0.95)
cor_table_LOWCI <- cor_table_CI[1,,] 
cor_table_UPWCI <- cor_table_CI[2,,] 

font1 <- 22
font2 <- 28

# I want the coefficient to be printed above the axis, while the CIs should be below it: 
col_fun <- colorRamp2(c(0.3, 1.3/2, 0.9), c("#66e8ff","#70de66", "#ffec19"))
AF <- Heatmap((as.matrix(cor_table_mean)), column_title ="Attractiveness: Females", cluster_rows=FALSE, 
              cluster_columns = FALSE, column_title_gp = gpar(fontsize=font1), column_names_gp = gpar(fontsize = font1), 
              row_names_gp = gpar(fontsize = font1), column_names_rot = 45, column_names_side = "top", name = "r", 
              col = col_fun,
              cell_fun = function(j, i, x, y, width, height, fill) {
                if (j>i) {c(grid.text(sprintf("%.3f", cor_table_mean[i, j]), x, y, gp = gpar(fontsize = font2),just=c(0.50,0.50)))}
                else if (j<i){c(grid.text(sprintf("%.3f", cor_table_LOWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.95,-0.33)),
                                grid.text(sprintf("%.3f", cor_table_UPWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.05,0.92))
                )}
              },rect_gp = gpar(col = "white", lwd = 1), row_title = "95% Compatibility Interval", row_title_gp = gpar(fontsize = 22))


# TRUSTWORTHINESS: F

cor_table_mean <- apply(F25$Rho_FSMUi_T,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(F25$Rho_FSMUi_T,c(2,3),PI, prob=0.95)
cor_table_LOWCI <- cor_table_CI[1,,] 
cor_table_UPWCI <- cor_table_CI[2,,] 


# I want the coefficient to be printed above the axis, while the CIs should be below it: 
col_fun <- colorRamp2(c(0.3, 1.3/2, 0.9), c("#66e8ff","#70de66", "#ffec19"))
AT <- Heatmap((as.matrix(cor_table_mean)), column_title ="Trustworthiness: Females", cluster_rows=FALSE, 
              cluster_columns = FALSE, column_title_gp = gpar(fontsize=font1), column_names_gp = gpar(fontsize = font1), 
              row_names_gp = gpar(fontsize = font1), column_names_rot = 45, column_names_side = "top", name = "r", 
              col = col_fun,
              cell_fun = function(j, i, x, y, width, height, fill) {
                if (j>i) {c(grid.text(sprintf("%.3f", cor_table_mean[i, j]), x, y, gp = gpar(fontsize = font2),just=c(0.50,0.50)))}
                else if (j<i){c(grid.text(sprintf("%.3f", cor_table_LOWCI[i, j]), x, y, gp = gpar(fontsize = font1),just = c(0.95,-0.33)),
                                grid.text(sprintf("%.3f", cor_table_UPWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.05,0.92))
                )}
              },rect_gp = gpar(col = "white", lwd = 1), row_title = "95% Compatibility Interval", row_title_gp = gpar(fontsize = 22))


# Dominance: F

cor_table_mean <- apply(F25$Rho_FSMUi_D,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(F25$Rho_FSMUi_D,c(2,3),PI, prob=0.95)
cor_table_LOWCI <- cor_table_CI[1,,] 
cor_table_UPWCI <- cor_table_CI[2,,] 


# I want the coefficient to be printed above the axis, while the CIs should be below it: 
col_fun <- colorRamp2(c(0.3, 1.3/2, 0.9), c("#66e8ff","#70de66", "#ffec19"))
AD <- Heatmap((as.matrix(cor_table_mean)), column_title ="Dominance: Females", cluster_rows=FALSE, 
              cluster_columns = FALSE, column_title_gp = gpar(fontsize=font1), column_names_gp = gpar(fontsize = font1), 
              row_names_gp = gpar(fontsize = font1), column_names_rot = 45, column_names_side = "top", name = "r", 
              col = col_fun,
              cell_fun = function(j, i, x, y, width, height, fill) {
                if (j>i) {c(grid.text(sprintf("%.3f", cor_table_mean[i, j]), x, y, gp = gpar(fontsize = font2),just=c(0.50,0.50)))}
                else if (j<i){c(grid.text(sprintf("%.3f", cor_table_LOWCI[i, j]), x, y, gp = gpar(fontsize = font1),just = c(0.95,-0.33)),
                                grid.text(sprintf("%.3f", cor_table_UPWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.05,0.92))
                )}
              },rect_gp = gpar(col = "white", lwd = 1), row_title = "95% Compatibility Interval", row_title_gp = gpar(fontsize = 22))



tiff("Correlations_Heatmap_A_Females_Attr_Above_Below_SES_ver_2.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AF
dev.off()

tiff("Correlations_Heatmap_T_Females_Tru_Above_Below_SES_ver_2.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AT
dev.off()

tiff("Correlations_Heatmap_D_Females_Dom_Above_Below_SES_ver_2.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AD
dev.off()



# Males

# Make it as a 3×1 Matrix
#  ATTRACTIVENESS:M

cor_table_mean <- apply(M25$Rho_FSMUi_A,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(M25$Rho_FSMUi_A,c(2,3),PI, prob=0.95)
cor_table_LOWCI <- cor_table_CI[1,,] 
cor_table_UPWCI <- cor_table_CI[2,,] 

font1 <- 22
font2 <- 28

# I want the coefficient to be printed above the axis, while the CIs should be below it: 
col_fun <- colorRamp2(c(0.3, 1.3/2, 0.9), c("#66e8ff","#70de66", "#ffec19"))
AF <- Heatmap((as.matrix(cor_table_mean)), column_title ="Attractiveness: Males", cluster_rows=FALSE, 
              cluster_columns = FALSE, column_title_gp = gpar(fontsize=font1), column_names_gp = gpar(fontsize = font1), 
              row_names_gp = gpar(fontsize = font1), column_names_rot = 45, column_names_side = "top", name = "r", 
              col = col_fun,
              cell_fun = function(j, i, x, y, width, height, fill) {
                if (j>i) {c(grid.text(sprintf("%.3f", cor_table_mean[i, j]), x, y, gp = gpar(fontsize = font2),just=c(0.50,0.50)))}
                else if (j<i){c(grid.text(sprintf("%.3f", cor_table_LOWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.95,-0.33)),
                                grid.text(sprintf("%.3f", cor_table_UPWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.05,0.92))
                )}
              },rect_gp = gpar(col = "white", lwd = 1), row_title = "95% Compatibility Interval", row_title_gp = gpar(fontsize = 22))


# TRUSTWORTHINESS: M

cor_table_mean <- apply(M25$Rho_FSMUi_T,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(M25$Rho_FSMUi_T,c(2,3),PI, prob=0.95)
cor_table_LOWCI <- cor_table_CI[1,,] 
cor_table_UPWCI <- cor_table_CI[2,,] 


# I want the coefficient to be printed above the axis, while the CIs should be below it: 
col_fun <- colorRamp2(c(0.3, 1.3/2, 0.9), c("#66e8ff","#70de66", "#ffec19"))
AT <- Heatmap((as.matrix(cor_table_mean)), column_title ="Trustworthiness: Males", cluster_rows=FALSE, 
              cluster_columns = FALSE, column_title_gp = gpar(fontsize=font1), column_names_gp = gpar(fontsize = font1), 
              row_names_gp = gpar(fontsize = font1), column_names_rot = 45, column_names_side = "top", name = "r", 
              col = col_fun,
              cell_fun = function(j, i, x, y, width, height, fill) {
                if (j>i) {c(grid.text(sprintf("%.3f", cor_table_mean[i, j]), x, y, gp = gpar(fontsize = font2),just=c(0.50,0.50)))}
                else if (j<i){c(grid.text(sprintf("%.3f", cor_table_LOWCI[i, j]), x, y, gp = gpar(fontsize = font1),just = c(0.95,-0.33)),
                                grid.text(sprintf("%.3f", cor_table_UPWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.05,0.92))
                )}
              },rect_gp = gpar(col = "white", lwd = 1), row_title = "95% Compatibility Interval", row_title_gp = gpar(fontsize = 22))


# Dominance: M

cor_table_mean <- apply(M25$Rho_FSMUi_D,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(M25$Rho_FSMUi_D,c(2,3),PI, prob=0.95)
cor_table_LOWCI <- cor_table_CI[1,,] 
cor_table_UPWCI <- cor_table_CI[2,,] 


# I want the coefficient to be printed above the axis, while the CIs should be below it: 
col_fun <- colorRamp2(c(0.3, 1.3/2, 0.9), c("#66e8ff","#70de66", "#ffec19"))
AD <- Heatmap((as.matrix(cor_table_mean)), column_title ="Dominance: Males", cluster_rows=FALSE, 
              cluster_columns = FALSE, column_title_gp = gpar(fontsize=font1), column_names_gp = gpar(fontsize = font1), 
              row_names_gp = gpar(fontsize = font1), column_names_rot = 45, column_names_side = "top", name = "r", 
              col = col_fun,
              cell_fun = function(j, i, x, y, width, height, fill) {
                if (j>i) {c(grid.text(sprintf("%.3f", cor_table_mean[i, j]), x, y, gp = gpar(fontsize = font2),just=c(0.50,0.50)))}
                else if (j<i){c(grid.text(sprintf("%.3f", cor_table_LOWCI[i, j]), x, y, gp = gpar(fontsize = font1),just = c(0.95,-0.33)),
                                grid.text(sprintf("%.3f", cor_table_UPWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.05,0.92))
                )}
              },rect_gp = gpar(col = "white", lwd = 1), row_title = "95% Compatibility Interval", row_title_gp = gpar(fontsize = 22))

tiff("Correlations_Heatmap_A_Males_Attr_Above_Below_SES_ver_2.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AF
dev.off()

tiff("Correlations_Heatmap_T_Males_Tru_Above_Below_SES_ver_2.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AT
dev.off()

tiff("Correlations_Heatmap_D_Males_Dom_Above_Below_SES_ver_2.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AD
dev.off()





#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Figure S22 - analogue of the Figure 3 in the main body of the article, which is a figure (top) presenting mean estimates
# in every sample, (bottom) presenting effects of morphometric and demographic predictors: SPECIFICALLY for WOMEN
# Version without labels

# Upper part: Frequent - all
# Compute the average of all group estimates

# Note - these are the same data as those to enter the A-T-D model; therefore, 
# the order of levels is the same, too. 

Avg_Freq_U_A = ( 
  F25$aA + F25$f_per_group_pr_A[,1,1] + 
    F25$aA + F25$f_per_group_pr_A[,3,1] + 
    F25$aA + F25$f_per_group_pr_A[,5,1] + 
    F25$aA + F25$f_per_group_pr_A[,7,1] + 
    F25$aA + F25$f_per_group_pr_A[,9,1] + 
    F25$aA + F25$f_per_group_pr_A[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_A = ( 
  F25$aA + F25$f_per_group_pr_A[,2,1] + 
    F25$aA + F25$f_per_group_pr_A[,4,1] + 
    F25$aA + F25$f_per_group_pr_A[,6,1] + 
    F25$aA + F25$f_per_group_pr_A[,8,1] + 
    F25$aA + F25$f_per_group_pr_A[,10,1] + 
    F25$aA + F25$f_per_group_pr_A[,12,1] 
) / 6



F_posts_ATR <- list(
  # 1 Intercept: 
  Intercept = F25$aA,
  # Infrequent users
  Avg_Freq_U_A,
  # Frequent users 
  Avg_Infreq_U_A,
  # Difference 
  Difference = Avg_Freq_U_A - Avg_Infreq_U_A,
  # 4 Australia - above - median users  
  AUS_Intensive_User = F25$aA + F25$f_per_group_pr_A[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = F25$aA + F25$f_per_group_pr_A[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = F25$aA + F25$f_per_group_pr_A[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = F25$aA + F25$f_per_group_pr_A[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = F25$aA + F25$f_per_group_pr_A[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = F25$aA + F25$f_per_group_pr_A[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = F25$aA + F25$f_per_group_pr_A[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = F25$aA + F25$f_per_group_pr_A[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = F25$aA + F25$f_per_group_pr_A[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = F25$aA + F25$f_per_group_pr_A[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = F25$aA + F25$f_per_group_pr_A[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = F25$aA + F25$f_per_group_pr_A[,12,1]
)

summary.data.frame(F_posts_ATR)
str(F_posts_ATR) # Need 13 rows 

#--------------

# Trustworthiness
Avg_Freq_U_T = ( 
  F25$aT + F25$f_per_group_pr_T[,1,1] + 
    F25$aT + F25$f_per_group_pr_T[,3,1] + 
    F25$aT + F25$f_per_group_pr_T[,5,1] + 
    F25$aT + F25$f_per_group_pr_T[,7,1] + 
    F25$aT + F25$f_per_group_pr_T[,9,1] + 
    F25$aT + F25$f_per_group_pr_T[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_T = ( 
  F25$aT + F25$f_per_group_pr_T[,2,1] + 
    F25$aT + F25$f_per_group_pr_T[,4,1] + 
    F25$aT + F25$f_per_group_pr_T[,6,1] + 
    F25$aT + F25$f_per_group_pr_T[,8,1] + 
    F25$aT + F25$f_per_group_pr_T[,10,1] + 
    F25$aT + F25$f_per_group_pr_T[,12,1] 
) / 6

F_posts_TRU <- list(
  # 1 Intercept: 
  Intercept = F25$aT,
  # Infrequent users
  Avg_Freq_U_T,
  # Frequent users 
  Avg_Infreq_U_T,
  # Difference 
  Difference = Avg_Freq_U_T - Avg_Infreq_U_T,
  # 4 Australia - above - median users  
  AUS_Intensive_User = F25$aT + F25$f_per_group_pr_T[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = F25$aT + F25$f_per_group_pr_T[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = F25$aT + F25$f_per_group_pr_T[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = F25$aT + F25$f_per_group_pr_T[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = F25$aT + F25$f_per_group_pr_T[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = F25$aT + F25$f_per_group_pr_T[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = F25$aT + F25$f_per_group_pr_T[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = F25$aT + F25$f_per_group_pr_T[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = F25$aT + F25$f_per_group_pr_T[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = F25$aT + F25$f_per_group_pr_T[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = F25$aT + F25$f_per_group_pr_T[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = F25$aT + F25$f_per_group_pr_T[,12,1]
)

summary.data.frame(F_posts_TRU)
str(F_posts_TRU) # Need 13 rows 

#--------------

# Dominance
Avg_Freq_U_D = ( 
  F25$aD + F25$f_per_group_pr_D[,1,1] + 
    F25$aD + F25$f_per_group_pr_D[,3,1] + 
    F25$aD + F25$f_per_group_pr_D[,5,1] + 
    F25$aD + F25$f_per_group_pr_D[,7,1] + 
    F25$aD + F25$f_per_group_pr_D[,9,1] + 
    F25$aD + F25$f_per_group_pr_D[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_D = ( 
  F25$aD + F25$f_per_group_pr_D[,2,1] + 
    F25$aD + F25$f_per_group_pr_D[,4,1] + 
    F25$aD + F25$f_per_group_pr_D[,6,1] + 
    F25$aD + F25$f_per_group_pr_D[,8,1] + 
    F25$aD + F25$f_per_group_pr_D[,10,1] + 
    F25$aD + F25$f_per_group_pr_D[,12,1] 
) / 6

F_posts_DOM <- list(
  # 1 Intercept: 
  Intercept = F25$aD,
  # Infrequent users
  Avg_Freq_U_D,
  # Frequent users 
  Avg_Infreq_U_D,
  # Difference 
  Difference = Avg_Freq_U_D - Avg_Infreq_U_D,
  # 4 Australia - above - median users  
  AUS_Intensive_User = F25$aD + F25$f_per_group_pr_D[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = F25$aD + F25$f_per_group_pr_D[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = F25$aD + F25$f_per_group_pr_D[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = F25$aD + F25$f_per_group_pr_D[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = F25$aD + F25$f_per_group_pr_D[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = F25$aD + F25$f_per_group_pr_D[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = F25$aD + F25$f_per_group_pr_D[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = F25$aD + F25$f_per_group_pr_D[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = F25$aD + F25$f_per_group_pr_D[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = F25$aD + F25$f_per_group_pr_D[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = F25$aD + F25$f_per_group_pr_D[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = F25$aD + F25$f_per_group_pr_D[,12,1]
)

summary.data.frame(F_posts_DOM)
str(F_posts_DOM) 



# PLOTTING: 
cols<-c("#00FBFF", 
        "#8C00FF",
        "#FFADD0",
        "#09C9FF",
        
        "#00FF3380",
        "#0FEA9980",
        
        "#FFEA0080",
        "#c9b80080",
        
        "#e0935c80",
        "#ff7b1c80",
        
        "#4796ff80",
        "#1278ff80",
        
        "#acdb5a80",
        "#a1f21380",
        
        "#6174ed80",
        "#223ef080"
) 



labs<-c("Intercept",
        "Above",
        "Below",
        "Above - Below: Difference",
        "Australia/NewZ - Above",
        "Australia/NewZ - Below",
        "Colombia - Above",
        "Colombia - Below",
        "Czechia - Above",
        "Czechia - Below",
        "South Africa - Above",
        "South Africa - Below",
        "Turkey - Above",
        "Turkey - Below",
        "Vietnam - Above",
        "Vietnam - Below"
)


# Figure S22:Upper Panel - Analogue of Figure 3:Upper Panel (final assembly done in InkScape)...

tiff("F_ATD_SES_ver_2_LEVELS_NOLABELS.tiff",width=30,height=14,units="cm",res=600,compression="lzw")
layout(matrix(1:3,byrow=F,nrow=1), widths=c(1.8,1.2,1.2),heights=c(1))

# ATTRACTIVENESS

par(mar=c(3.1, 15.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-1,0.7),ylim=c(1,17),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Attractiveness - Group Estimates", side = 3, line = -0.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

rect(-1,0.5,0.75,17, col=F, border = "#b9b9B999", lwd = 2)
rect(-0.95,12.5,0.7,16.5, col="#F0F0F0", border = F)

segments(x0 = -1, y0 = 1:16, x1 = 0.75, y1 = 1:16, col = "#40808080", lwd = 1.5)
segments(x0 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y0 = 0.5, x1 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y1 = 16.5, col="#80808080",lty=2)

ys<-seq(from=1,to=16, by=1)
ys<-rev(ys)

cex_t <- 1.25
cex_t2 <- 0.95
pls <- 0.75


for(i in 1:length(F_posts_ATR)){ 
  toplot<-F_posts_ATR[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  textlab<-labs[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  axis(2, at=ycoord, labels=textlab, srt=45, las=2, cex.axis=1.2, tick=F)
  for (i in 1:16) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.007)+ycoord[i],border=T, lwd = 1, col=colpol))
  }
  #lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0 = 0, y0 = 0, x1 = 0, y1 = 16.5, col = "#999999", lwd = 2, lty = 1)



# TRUSTWORTHINESS
par(mar=c(3.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-1,0.7),ylim=c(1,17),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Trustworthiness - Group Estimates", side = 3, line = -0.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

rect(-1,0.5,0.75,17, col=F, border = "#b9b9B999", lwd = 2)
rect(-0.95,12.5,0.7,16.5, col="#F0F0F0", border = F)

segments(x0 = -1, y0 = 1:16, x1 = 0.75, y1 = 1:16, col = "#40808080", lwd = 1.5)
segments(x0 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y0 = 0.5, x1 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y1 = 16.5, col="#80808080",lty=2)

ys<-seq(from=1,to=16, by=1)
ys<-rev(ys)

for(i in 1:length(F_posts_TRU)){ 
  toplot<-F_posts_TRU[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  for (i in 1:16) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.007)+ycoord[i],border=T, lwd = 1, col=colpol))
  }
  #lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0 = 0, y0 = 0, x1 = 0, y1 = 16.5, col = "#999999", lwd = 2, lty = 1)




# DOMINANCE
par(mar=c(3.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-1,0.7),ylim=c(1,17),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Dominance - Group Estimates", side = 3, line = -0.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

rect(-1,0.5,0.75,17, col=F, border = "#b9b9B999", lwd = 2)
rect(-0.95,12.5,0.7,16.5, col="#F0F0F0", border = F)

segments(x0 = -1, y0 = 1:16, x1 = 0.75, y1 = 1:16, col = "#40808080", lwd = 1.5)
segments(x0 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y0 = 0.5, x1 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y1 = 16.5, col="#80808080",lty=2)

ys<-seq(from=1,to=16, by=1)
ys<-rev(ys)


for(i in 1:length(F_posts_DOM)){ 
  toplot<-F_posts_DOM[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  for (i in 1:16) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.007)+ycoord[i],border=T, lwd = 1, col=colpol))
  }
  #lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0 = 0, y0 = 0, x1 = 0, y1 = 16.5, col = "#999999", lwd = 2, lty = 1)


dev.off()


#---------------------------------

# And now GMM: 


# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS


# Preparing composite coefficients: 
# AGE:
Attr_Age_F_freq <- c(
  (F25$b_age_A + F25$f_per_group_pr_A[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (F25$b_age_A + F25$f_per_group_pr_A[,3,2])+   # Age (A): COL More Freq. Users
    (F25$b_age_A + F25$f_per_group_pr_A[,5,2])+   # Age (A): CZE More Freq. Users
    (F25$b_age_A + F25$f_per_group_pr_A[,7,2])+   # Age (A): ZAF More Freq. Users
    (F25$b_age_A + F25$f_per_group_pr_A[,9,2])+   # Age (A): TUR More Freq. Users
    (F25$b_age_A + F25$f_per_group_pr_A[,11,2])   # Age (A): COL More Freq. Users
)/6

Attr_Age_F_INfreq <- (
  (F25$b_age_A + F25$f_per_group_pr_A[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (F25$b_age_A + F25$f_per_group_pr_A[,4,2])+   # Age (A): COL More Freq. Users
    (F25$b_age_A + F25$f_per_group_pr_A[,6,2])+   # Age (A): CZE More Freq. Users
    (F25$b_age_A + F25$f_per_group_pr_A[,8,2])+   # Age (A): ZAF More Freq. Users
    (F25$b_age_A + F25$f_per_group_pr_A[,10,2])+   # Age (A): TUR More Freq. Users
    (F25$b_age_A + F25$f_per_group_pr_A[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
Attr_Dist_F_freq <- (
  (F25$b_dist_A + F25$f_per_group_pr_A[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (F25$b_dist_A + F25$f_per_group_pr_A[,3,3])+   # Dist (A): COL More Freq. Users
    (F25$b_dist_A + F25$f_per_group_pr_A[,5,3])+   # Dist (A): CZE More Freq. Users
    (F25$b_dist_A + F25$f_per_group_pr_A[,7,3])+   # Dist (A): ZAF More Freq. Users
    (F25$b_dist_A + F25$f_per_group_pr_A[,9,3])+   # Dist (A): TUR More Freq. Users
    (F25$b_dist_A + F25$f_per_group_pr_A[,11,3])   # Dist (A): COL More Freq. Users
)/6

Attr_Dist_F_INfreq <- (
  (F25$b_dist_A + F25$f_per_group_pr_A[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (F25$b_dist_A + F25$f_per_group_pr_A[,4,3])+   # Dist (A): COL More Freq. Users
    (F25$b_dist_A + F25$f_per_group_pr_A[,6,3])+   # Dist (A): CZE More Freq. Users
    (F25$b_dist_A + F25$f_per_group_pr_A[,8,3])+   # Dist (A): ZAF More Freq. Users
    (F25$b_dist_A + F25$f_per_group_pr_A[,10,3])+   # Dist (A): TUR More Freq. Users
    (F25$b_dist_A + F25$f_per_group_pr_A[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
Attr_Asym_F_freq <- (
  (F25$b_FA_A + F25$f_per_group_pr_A[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (F25$b_FA_A + F25$f_per_group_pr_A[,3,4])+   # Asym (A): COL More Freq. Users
    (F25$b_FA_A + F25$f_per_group_pr_A[,5,4])+   # Asym (A): CZE More Freq. Users
    (F25$b_FA_A + F25$f_per_group_pr_A[,7,4])+   # Asym (A): ZAF More Freq. Users
    (F25$b_FA_A + F25$f_per_group_pr_A[,9,4])+   # Asym (A): TUR More Freq. Users
    (F25$b_FA_A + F25$f_per_group_pr_A[,11,4])   # Asym (A): COL More Freq. Users
)/6

Attr_Asym_F_INfreq <- (
  (F25$b_FA_A + F25$f_per_group_pr_A[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (F25$b_FA_A + F25$f_per_group_pr_A[,4,4])+   # Asym (A): COL More Freq. Users
    (F25$b_FA_A + F25$f_per_group_pr_A[,6,4])+   # Asym (A): CZE More Freq. Users
    (F25$b_FA_A + F25$f_per_group_pr_A[,8,4])+   # Asym (A): ZAF More Freq. Users
    (F25$b_FA_A + F25$f_per_group_pr_A[,10,4])+   # Asym (A): TUR More Freq. Users
    (F25$b_FA_A + F25$f_per_group_pr_A[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
Attr_SexTyp_F_freq <- (
  (F25$b_sshd_A + F25$f_per_group_pr_A[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (F25$b_sshd_A + F25$f_per_group_pr_A[,3,5])+   # SShD (A): COL More Freq. Users
    (F25$b_sshd_A + F25$f_per_group_pr_A[,5,5])+   # SShD (A): CZE More Freq. Users
    (F25$b_sshd_A + F25$f_per_group_pr_A[,7,5])+   # SShD (A): ZAF More Freq. Users
    (F25$b_sshd_A + F25$f_per_group_pr_A[,9,5])+   # SShD (A): TUR More Freq. Users
    (F25$b_sshd_A + F25$f_per_group_pr_A[,11,5])   # SShD (A): COL More Freq. Users
)/6

Attr_SexTyp_F_INfreq <- (
  (F25$b_sshd_A + F25$f_per_group_pr_A[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (F25$b_sshd_A + F25$f_per_group_pr_A[,4,5])+   # SShD (A): COL More Freq. Users
    (F25$b_sshd_A + F25$f_per_group_pr_A[,6,5])+   # SShD (A): CZE More Freq. Users
    (F25$b_sshd_A + F25$f_per_group_pr_A[,8,5])+   # SShD (A): ZAF More Freq. Users
    (F25$b_sshd_A + F25$f_per_group_pr_A[,10,5])+   # SShD (A): TUR More Freq. Users
    (F25$b_sshd_A + F25$f_per_group_pr_A[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
Attr_L_F_freq <- (
  (F25$b_L_A + F25$f_per_group_pr_A[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (F25$b_L_A + F25$f_per_group_pr_A[,3,6])+   # L (A): COL More Freq. Users
    (F25$b_L_A + F25$f_per_group_pr_A[,5,6])+   # L (A): CZE More Freq. Users
    (F25$b_L_A + F25$f_per_group_pr_A[,7,6])+   # L (A): ZAF More Freq. Users
    (F25$b_L_A + F25$f_per_group_pr_A[,9,6])+   # L (A): TUR More Freq. Users
    (F25$b_L_A + F25$f_per_group_pr_A[,11,6])   # L (A): COL More Freq. Users
)/6

Attr_L_F_INfreq <- (
  (F25$b_L_A + F25$f_per_group_pr_A[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (F25$b_L_A + F25$f_per_group_pr_A[,4,6])+   # L (A): COL More Freq. Users
    (F25$b_L_A + F25$f_per_group_pr_A[,6,6])+   # L (A): CZE More Freq. Users
    (F25$b_L_A + F25$f_per_group_pr_A[,8,6])+   # L (A): ZAF More Freq. Users
    (F25$b_L_A + F25$f_per_group_pr_A[,10,6])+   # L (A): TUR More Freq. Users
    (F25$b_L_A + F25$f_per_group_pr_A[,12,6])   # L (A): COL More Freq. Users
)/6



# GMM - SEPARATELY 

# Attractiveness - GMM 
str(F25)
str(F25$f_per_group_pr_A) # Mind that 1 is intercept! 

F_posts_ATD_MM_Attr <- list(
  #  AGE
  Age_Slope = (F25$b_age_A),    # Slope prior to accessing culture
  
  Attr_Age_F_freq,
  Attr_Age_F_INfreq,
  
  Age_AUS_Freq = (F25$b_age_A + F25$f_per_group_pr_A[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (F25$b_age_A + F25$f_per_group_pr_A[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (F25$b_age_A + F25$f_per_group_pr_A[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (F25$b_age_A + F25$f_per_group_pr_A[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (F25$b_age_A + F25$f_per_group_pr_A[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (F25$b_age_A + F25$f_per_group_pr_A[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (F25$b_age_A + F25$f_per_group_pr_A[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (F25$b_age_A + F25$f_per_group_pr_A[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (F25$b_age_A + F25$f_per_group_pr_A[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (F25$b_age_A + F25$f_per_group_pr_A[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (F25$b_age_A + F25$f_per_group_pr_A[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (F25$b_age_A + F25$f_per_group_pr_A[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (F25$b_dist_A),    # Dist (A): CZE Control
  
  Attr_Dist_F_freq,
  Attr_Dist_F_INfreq,
  
  Dist_AUS_Freq = (F25$b_dist_A + F25$f_per_group_pr_A[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (F25$b_dist_A + F25$f_per_group_pr_A[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (F25$b_dist_A + F25$f_per_group_pr_A[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (F25$b_dist_A + F25$f_per_group_pr_A[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (F25$b_dist_A + F25$f_per_group_pr_A[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (F25$b_dist_A + F25$f_per_group_pr_A[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (F25$b_dist_A + F25$f_per_group_pr_A[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (F25$b_dist_A + F25$f_per_group_pr_A[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (F25$b_dist_A + F25$f_per_group_pr_A[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (F25$b_dist_A + F25$f_per_group_pr_A[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (F25$b_dist_A + F25$f_per_group_pr_A[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (F25$b_dist_A + F25$f_per_group_pr_A[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (F25$b_FA_A),    # Asym (A): CZE Control
  
  Attr_Asym_F_freq,
  Attr_Asym_F_INfreq,
  
  Asym_AUS_Freq = (F25$b_FA_A + F25$f_per_group_pr_A[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (F25$b_FA_A + F25$f_per_group_pr_A[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (F25$b_FA_A + F25$f_per_group_pr_A[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (F25$b_FA_A + F25$f_per_group_pr_A[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (F25$b_FA_A + F25$f_per_group_pr_A[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (F25$b_FA_A + F25$f_per_group_pr_A[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (F25$b_FA_A + F25$f_per_group_pr_A[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (F25$b_FA_A + F25$f_per_group_pr_A[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (F25$b_FA_A + F25$f_per_group_pr_A[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (F25$b_FA_A + F25$f_per_group_pr_A[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (F25$b_FA_A + F25$f_per_group_pr_A[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (F25$b_FA_A + F25$f_per_group_pr_A[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (F25$b_sshd_A),    # SexTyp (A): CZE Control
  
  Attr_SexTyp_F_freq,
  Attr_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (F25$b_sshd_A + F25$f_per_group_pr_A[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (F25$b_sshd_A + F25$f_per_group_pr_A[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (F25$b_sshd_A + F25$f_per_group_pr_A[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (F25$b_sshd_A + F25$f_per_group_pr_A[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (F25$b_sshd_A + F25$f_per_group_pr_A[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (F25$b_sshd_A + F25$f_per_group_pr_A[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (F25$b_sshd_A + F25$f_per_group_pr_A[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (F25$b_sshd_A + F25$f_per_group_pr_A[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (F25$b_sshd_A + F25$f_per_group_pr_A[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (F25$b_sshd_A + F25$f_per_group_pr_A[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (F25$b_sshd_A + F25$f_per_group_pr_A[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (F25$b_sshd_A + F25$f_per_group_pr_A[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (F25$b_L_A),    # L (A): CZE Control
  
  Attr_L_F_freq,
  Attr_L_F_INfreq,
  
  L_AUS_Freq = (F25$b_L_A + F25$f_per_group_pr_A[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (F25$b_L_A + F25$f_per_group_pr_A[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (F25$b_L_A + F25$f_per_group_pr_A[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (F25$b_L_A + F25$f_per_group_pr_A[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (F25$b_L_A + F25$f_per_group_pr_A[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (F25$b_L_A + F25$f_per_group_pr_A[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (F25$b_L_A + F25$f_per_group_pr_A[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (F25$b_L_A + F25$f_per_group_pr_A[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (F25$b_L_A + F25$f_per_group_pr_A[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (F25$b_L_A + F25$f_per_group_pr_A[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (F25$b_L_A + F25$f_per_group_pr_A[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (F25$b_L_A + F25$f_per_group_pr_A[,12,6])   # L (A): COL Less Freq. Users
)


summary.data.frame(F_posts_ATD_MM_Attr)
str(F_posts_ATD_MM_Attr) # Need 65 rows 


labs <- c("Fixed Slope",
          
          "Above (All)",
          "Below (All)",
          
          "AUS/NZE Above",
          "AUS/NZE Below",
          
          "COL Above",
          "COL Below",
          
          "CZE Above",
          "CZE Below",
          
          "ZAF Above",
          "ZAF Below",
          
          "TUR Above",
          "TUR Below",
          
          "VNM Above",
          "VNM Below"
)

cols<-c("#c56043", "#e04b5c", "#de5353", "#c25c42", "#d85652", "#c8585d", "#de534b", "#cd5440", "#d35d5e", "#c3615a", "#d46655", "#cf5552", "#c65f5d","#de534b", "#cd5440",
        "#856554", "#896757", "#815b69", "#8f6756", "#86554d", "#8a675d", "#85586a", "#805158", "#855d52", "#8f4e50", "#8e5960", "#80685a", "#75584c","#85586a", "#805158",
        "#85b68f", "#77bf98", "#86b18f", "#85b589", "#7eB68d", "#8bB37a", "#80B491", "#65Bd7d", "#81B784", "#8bb883", "#8ebf92", "#7bBe8f", "#91bd79","#81B784", "#8bb883",
        "#6b9168", "#69925b", "#689d55", "#6ba261", "#5aaa6b", "#599866", "#519c58", "#55a755", "#5caa53", "#5c9c60", "#588f5b", "#5e9d6f", "#5aa757","#5caa53", "#5c9c60", 
        "#64a9e6", "#79a5ee", "#809be9", "#6d9dfa", "#6899ec", "#6facde", "#77abed", "#63aaf0", "#63a4e4", "#74a5e3", "#79a9eb", "#6aaaf6", "#78a4dd","#6facde", "#77abed"
        
) 




tiff("F_ATD_GMM_SES_ver_2_COEFS.tiff",width=30,height=33,units="cm",res=600,compression="lzw")
layout(matrix(1:3,byrow=F,nrow=1), widths=c(1.1,0.7,0.7),heights=c(1))

par(mar=c(2.1, 14.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.2),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Attr: GMM - Females", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#3d465490", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#3d465490", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#3d465490", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#3d465490", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#3d465490", lwd = 2)


# Small squares...
rect(-0.24,12.5,0.19,15.25, col="#F0F0F0", border = F)
rect(-0.24,28,0.19,30.75, col="#F0F0F0", border = F)
rect(-0.24,43.5,0.19,46.25, col="#F0F0F0", border = F)
rect(-0.24,59,0.19,61.75, col="#F0F0F0", border = F)
rect(-0.24,74.5,0.19,77.25, col="#F0F0F0", border = F)

# 
segments(x0 = -0.25, y0 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), x1 = 0.2, 
         y1 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), col = "#40808080", lwd = 1.5)

# 1
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 0, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 15.5,
         col="#80808080",lty=2)

# 2
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 16, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 31,
         col="#80808080",lty=2)

# 3
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 31.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 46.5,
         col="#80808080",lty=2)

# 4 
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 47, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 62,
         col="#80808080",lty=2)

# 5
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 62.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 77.5,
         col="#80808080",lty=2)

labs <- rep(labs,5)

ys<-c(1:15,16.5:30.5,32:46,47.5:61.5,63:77)
ys<-rev(ys)


for(i in 1:length(F_posts_ATD_MM_Attr)){ 
  toplot<-F_posts_ATD_MM_Attr[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  textlab<-labs[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  axis(2, at=ycoord, labels=textlab, srt=45, las=2, cex.axis=1.2, tick=F)
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=5, lty=1, col=cols[i])
  segments(x0 = ci[1], y0 = ycoord-0.1, x1 = ci[1], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  segments(x0 = ci[2], y0 = ycoord-0.1, x1 = ci[2], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0=0.0, y0=c(0.0, 16, 31.5, 47, 62.5), x1=0.0, y1=c(15.5, 31, 46.5, 62, 77.5), col="#999999", lwd=2, lty=1)

text(x = -0.3, y = 77, labels = "Age", cex = 0.4, pos = 4)
text(x = -0.3, y = 62, labels = "Dist", cex = 0.4, pos = 4)
text(x = -0.3, y = 47, labels = "Asym", cex = 0.4, pos = 4)
text(x = -0.3, y = 32, labels = "SShD", cex = 0.4, pos = 4)
text(x = -0.3, y = 15, labels = "Lightness", cex = 0.4, pos = 4)

# 



# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 

# And now GMM: 
# Preparing composite coefficients: 
# AGE:
TR_Age_F_freq <- c(
  (F25$b_age_T + F25$f_per_group_pr_T[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (F25$b_age_T + F25$f_per_group_pr_T[,3,2])+   # Age (A): COL More Freq. Users
    (F25$b_age_T + F25$f_per_group_pr_T[,5,2])+   # Age (A): CZE More Freq. Users
    (F25$b_age_T + F25$f_per_group_pr_T[,7,2])+   # Age (A): ZAF More Freq. Users
    (F25$b_age_T + F25$f_per_group_pr_T[,9,2])+   # Age (A): TUR More Freq. Users
    (F25$b_age_T + F25$f_per_group_pr_T[,11,2])   # Age (A): COL More Freq. Users
)/6

TR_Age_F_INfreq <- (
  (F25$b_age_T + F25$f_per_group_pr_T[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (F25$b_age_T + F25$f_per_group_pr_T[,4,2])+   # Age (A): COL More Freq. Users
    (F25$b_age_T + F25$f_per_group_pr_T[,6,2])+   # Age (A): CZE More Freq. Users
    (F25$b_age_T + F25$f_per_group_pr_T[,8,2])+   # Age (A): ZAF More Freq. Users
    (F25$b_age_T + F25$f_per_group_pr_T[,10,2])+   # Age (A): TUR More Freq. Users
    (F25$b_age_T + F25$f_per_group_pr_T[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
TR_Dist_F_freq <- (
  (F25$b_dist_T + F25$f_per_group_pr_T[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (F25$b_dist_T + F25$f_per_group_pr_T[,3,3])+   # Dist (A): COL More Freq. Users
    (F25$b_dist_T + F25$f_per_group_pr_T[,5,3])+   # Dist (A): CZE More Freq. Users
    (F25$b_dist_T + F25$f_per_group_pr_T[,7,3])+   # Dist (A): ZAF More Freq. Users
    (F25$b_dist_T + F25$f_per_group_pr_T[,9,3])+   # Dist (A): TUR More Freq. Users
    (F25$b_dist_T + F25$f_per_group_pr_T[,11,3])   # Dist (A): COL More Freq. Users
)/6

TR_Dist_F_INfreq <- (
  (F25$b_dist_T + F25$f_per_group_pr_T[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (F25$b_dist_T + F25$f_per_group_pr_T[,4,3])+   # Dist (A): COL More Freq. Users
    (F25$b_dist_T + F25$f_per_group_pr_T[,6,3])+   # Dist (A): CZE More Freq. Users
    (F25$b_dist_T + F25$f_per_group_pr_T[,8,3])+   # Dist (A): ZAF More Freq. Users
    (F25$b_dist_T + F25$f_per_group_pr_T[,10,3])+   # Dist (A): TUR More Freq. Users
    (F25$b_dist_T + F25$f_per_group_pr_T[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
TR_Asym_F_freq <- (
  (F25$b_FA_T + F25$f_per_group_pr_T[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (F25$b_FA_T + F25$f_per_group_pr_T[,3,4])+   # Asym (A): COL More Freq. Users
    (F25$b_FA_T + F25$f_per_group_pr_T[,5,4])+   # Asym (A): CZE More Freq. Users
    (F25$b_FA_T + F25$f_per_group_pr_T[,7,4])+   # Asym (A): ZAF More Freq. Users
    (F25$b_FA_T + F25$f_per_group_pr_T[,9,4])+   # Asym (A): TUR More Freq. Users
    (F25$b_FA_T + F25$f_per_group_pr_T[,11,4])   # Asym (A): COL More Freq. Users
)/6

TR_Asym_F_INfreq <- (
  (F25$b_FA_T + F25$f_per_group_pr_T[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (F25$b_FA_T + F25$f_per_group_pr_T[,4,4])+   # Asym (A): COL More Freq. Users
    (F25$b_FA_T + F25$f_per_group_pr_T[,6,4])+   # Asym (A): CZE More Freq. Users
    (F25$b_FA_T + F25$f_per_group_pr_T[,8,4])+   # Asym (A): ZAF More Freq. Users
    (F25$b_FA_T + F25$f_per_group_pr_T[,10,4])+   # Asym (A): TUR More Freq. Users
    (F25$b_FA_T + F25$f_per_group_pr_T[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
TR_SexTyp_F_freq <- (
  (F25$b_sshd_T + F25$f_per_group_pr_T[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (F25$b_sshd_T + F25$f_per_group_pr_T[,3,5])+   # SShD (A): COL More Freq. Users
    (F25$b_sshd_T + F25$f_per_group_pr_T[,5,5])+   # SShD (A): CZE More Freq. Users
    (F25$b_sshd_T + F25$f_per_group_pr_T[,7,5])+   # SShD (A): ZAF More Freq. Users
    (F25$b_sshd_T + F25$f_per_group_pr_T[,9,5])+   # SShD (A): TUR More Freq. Users
    (F25$b_sshd_T + F25$f_per_group_pr_T[,11,5])   # SShD (A): COL More Freq. Users
)/6

TR_SexTyp_F_INfreq <- (
  (F25$b_sshd_T + F25$f_per_group_pr_T[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (F25$b_sshd_T + F25$f_per_group_pr_T[,4,5])+   # SShD (A): COL More Freq. Users
    (F25$b_sshd_T + F25$f_per_group_pr_T[,6,5])+   # SShD (A): CZE More Freq. Users
    (F25$b_sshd_T + F25$f_per_group_pr_T[,8,5])+   # SShD (A): ZAF More Freq. Users
    (F25$b_sshd_T + F25$f_per_group_pr_T[,10,5])+   # SShD (A): TUR More Freq. Users
    (F25$b_sshd_T + F25$f_per_group_pr_T[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
TR_L_F_freq <- (
  (F25$b_L_T + F25$f_per_group_pr_T[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (F25$b_L_T + F25$f_per_group_pr_T[,3,6])+   # L (A): COL More Freq. Users
    (F25$b_L_T + F25$f_per_group_pr_T[,5,6])+   # L (A): CZE More Freq. Users
    (F25$b_L_T + F25$f_per_group_pr_T[,7,6])+   # L (A): ZAF More Freq. Users
    (F25$b_L_T + F25$f_per_group_pr_T[,9,6])+   # L (A): TUR More Freq. Users
    (F25$b_L_T + F25$f_per_group_pr_T[,11,6])   # L (A): COL More Freq. Users
)/6

TR_L_F_INfreq <- (
  (F25$b_L_T + F25$f_per_group_pr_T[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (F25$b_L_T + F25$f_per_group_pr_T[,4,6])+   # L (A): COL More Freq. Users
    (F25$b_L_T + F25$f_per_group_pr_T[,6,6])+   # L (A): CZE More Freq. Users
    (F25$b_L_T + F25$f_per_group_pr_T[,8,6])+   # L (A): ZAF More Freq. Users
    (F25$b_L_T + F25$f_per_group_pr_T[,10,6])+   # L (A): TUR More Freq. Users
    (F25$b_L_T + F25$f_per_group_pr_T[,12,6])   # L (A): COL More Freq. Users
)/6



# Trusw - GMM 
str(F25)
str(F25$f_per_group_pr_T) # Mind that 1 is intercept! 

F_posts_ATD_MM_TRU <- list(
  #  AGE
  Age_Slope = (F25$b_age_T),    # Slope prior to accessing culture
  
  TR_Age_F_freq,
  TR_Age_F_INfreq,
  
  Age_AUS_Freq = (F25$b_age_T + F25$f_per_group_pr_T[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (F25$b_age_T + F25$f_per_group_pr_T[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (F25$b_age_T + F25$f_per_group_pr_T[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (F25$b_age_T + F25$f_per_group_pr_T[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (F25$b_age_T + F25$f_per_group_pr_T[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (F25$b_age_T + F25$f_per_group_pr_T[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (F25$b_age_T + F25$f_per_group_pr_T[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (F25$b_age_T + F25$f_per_group_pr_T[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (F25$b_age_T + F25$f_per_group_pr_T[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (F25$b_age_T + F25$f_per_group_pr_T[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (F25$b_age_T + F25$f_per_group_pr_T[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (F25$b_age_T + F25$f_per_group_pr_T[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (F25$b_dist_T),    # Dist (A): CZE Control
  
  TR_Dist_F_freq,
  TR_Dist_F_INfreq,
  
  Dist_AUS_Freq = (F25$b_dist_T + F25$f_per_group_pr_T[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (F25$b_dist_T + F25$f_per_group_pr_T[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (F25$b_dist_T + F25$f_per_group_pr_T[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (F25$b_dist_T + F25$f_per_group_pr_T[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (F25$b_dist_T + F25$f_per_group_pr_T[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (F25$b_dist_T + F25$f_per_group_pr_T[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (F25$b_dist_T + F25$f_per_group_pr_T[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (F25$b_dist_T + F25$f_per_group_pr_T[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (F25$b_dist_T + F25$f_per_group_pr_T[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (F25$b_dist_T + F25$f_per_group_pr_T[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (F25$b_dist_T + F25$f_per_group_pr_T[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (F25$b_dist_T + F25$f_per_group_pr_T[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (F25$b_FA_T),    # Asym (A): CZE Control
  
  TR_Asym_F_freq,
  TR_Asym_F_INfreq,
  
  Asym_AUS_Freq = (F25$b_FA_T + F25$f_per_group_pr_T[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (F25$b_FA_T + F25$f_per_group_pr_T[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (F25$b_FA_T + F25$f_per_group_pr_T[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (F25$b_FA_T + F25$f_per_group_pr_T[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (F25$b_FA_T + F25$f_per_group_pr_T[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (F25$b_FA_T + F25$f_per_group_pr_T[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (F25$b_FA_T + F25$f_per_group_pr_T[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (F25$b_FA_T + F25$f_per_group_pr_T[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (F25$b_FA_T + F25$f_per_group_pr_T[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (F25$b_FA_T + F25$f_per_group_pr_T[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (F25$b_FA_T + F25$f_per_group_pr_T[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (F25$b_FA_T + F25$f_per_group_pr_T[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (F25$b_sshd_T),    # SexTyp (A): CZE Control
  
  TR_SexTyp_F_freq,
  TR_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (F25$b_sshd_T + F25$f_per_group_pr_T[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (F25$b_sshd_T + F25$f_per_group_pr_T[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (F25$b_sshd_T + F25$f_per_group_pr_T[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (F25$b_sshd_T + F25$f_per_group_pr_T[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (F25$b_sshd_T + F25$f_per_group_pr_T[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (F25$b_sshd_T + F25$f_per_group_pr_T[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (F25$b_sshd_T + F25$f_per_group_pr_T[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (F25$b_sshd_T + F25$f_per_group_pr_T[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (F25$b_sshd_T + F25$f_per_group_pr_T[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (F25$b_sshd_T + F25$f_per_group_pr_T[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (F25$b_sshd_T + F25$f_per_group_pr_T[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (F25$b_sshd_T + F25$f_per_group_pr_T[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (F25$b_L_T),    # L (A): CZE Control
  
  TR_L_F_freq,
  TR_L_F_INfreq,
  
  L_AUS_Freq = (F25$b_L_T + F25$f_per_group_pr_T[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (F25$b_L_T + F25$f_per_group_pr_T[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (F25$b_L_T + F25$f_per_group_pr_T[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (F25$b_L_T + F25$f_per_group_pr_T[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (F25$b_L_T + F25$f_per_group_pr_T[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (F25$b_L_T + F25$f_per_group_pr_T[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (F25$b_L_T + F25$f_per_group_pr_T[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (F25$b_L_T + F25$f_per_group_pr_T[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (F25$b_L_T + F25$f_per_group_pr_T[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (F25$b_L_T + F25$f_per_group_pr_T[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (F25$b_L_T + F25$f_per_group_pr_T[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (F25$b_L_T + F25$f_per_group_pr_T[,12,6])   # L (A): COL Less Freq. Users
)


summary.data.frame(F_posts_ATD_MM_TRU)
str(F_posts_ATD_MM_TRU) # Need 65 rows 



par(mar=c(2.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.2),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Trustw: GMM - Females", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting


# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#3d465490", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#3d465490", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#3d465490", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#3d465490", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#3d465490", lwd = 2)


# Small squares...
rect(-0.24,12.5,0.19,15.25, col="#F0F0F0", border = F)
rect(-0.24,28,0.19,30.75, col="#F0F0F0", border = F)
rect(-0.24,43.5,0.19,46.25, col="#F0F0F0", border = F)
rect(-0.24,59,0.19,61.75, col="#F0F0F0", border = F)
rect(-0.24,74.5,0.19,77.25, col="#F0F0F0", border = F)

# 
segments(x0 = -0.25, y0 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), x1 = 0.2, 
         y1 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), col = "#40808080", lwd = 1.5)

# 1
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 0, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 15.5,
         col="#80808080",lty=2)

# 2
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 16, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 31,
         col="#80808080",lty=2)

# 3
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 31.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 46.5,
         col="#80808080",lty=2)

# 4 
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 47, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 62,
         col="#80808080",lty=2)

# 5
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 62.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 77.5,
         col="#80808080",lty=2)



ys<-c(1:15,16.5:30.5,32:46,47.5:61.5,63:77)
ys<-rev(ys)

labs <- rep(labs,5)


for(i in 1:length(F_posts_ATD_MM_TRU)){ 
  toplot<-F_posts_ATD_MM_TRU[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=5, lty=1, col=cols[i])
  segments(x0 = ci[1], y0 = ycoord-0.1, x1 = ci[1], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  segments(x0 = ci[2], y0 = ycoord-0.1, x1 = ci[2], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0=0.0, y0=c(0.0, 16, 31.5, 47, 62.5), x1=0.0, y1=c(15.5, 31, 46.5, 62, 77.5), col="#999999", lwd=2, lty=1)
# 





# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 

# And now GMM: 
# Preparing composite coefficients: 
# AGE:
DOM_Age_F_freq <- c(
  (F25$b_age_D + F25$f_per_group_pr_D[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (F25$b_age_D + F25$f_per_group_pr_D[,3,2])+   # Age (A): COL More Freq. Users
    (F25$b_age_D + F25$f_per_group_pr_D[,5,2])+   # Age (A): CZE More Freq. Users
    (F25$b_age_D + F25$f_per_group_pr_D[,7,2])+   # Age (A): ZAF More Freq. Users
    (F25$b_age_D + F25$f_per_group_pr_D[,9,2])+   # Age (A): TUR More Freq. Users
    (F25$b_age_D + F25$f_per_group_pr_D[,11,2])   # Age (A): COL More Freq. Users
)/6

DOM_Age_F_INfreq <- (
  (F25$b_age_D + F25$f_per_group_pr_D[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (F25$b_age_D + F25$f_per_group_pr_D[,4,2])+   # Age (A): COL More Freq. Users
    (F25$b_age_D + F25$f_per_group_pr_D[,6,2])+   # Age (A): CZE More Freq. Users
    (F25$b_age_D + F25$f_per_group_pr_D[,8,2])+   # Age (A): ZAF More Freq. Users
    (F25$b_age_D + F25$f_per_group_pr_D[,10,2])+   # Age (A): TUR More Freq. Users
    (F25$b_age_D + F25$f_per_group_pr_D[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
DOM_Dist_F_freq <- (
  (F25$b_dist_D + F25$f_per_group_pr_D[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (F25$b_dist_D + F25$f_per_group_pr_D[,3,3])+   # Dist (A): COL More Freq. Users
    (F25$b_dist_D + F25$f_per_group_pr_D[,5,3])+   # Dist (A): CZE More Freq. Users
    (F25$b_dist_D + F25$f_per_group_pr_D[,7,3])+   # Dist (A): ZAF More Freq. Users
    (F25$b_dist_D + F25$f_per_group_pr_D[,9,3])+   # Dist (A): TUR More Freq. Users
    (F25$b_dist_D + F25$f_per_group_pr_D[,11,3])   # Dist (A): COL More Freq. Users
)/6

DOM_Dist_F_INfreq <- (
  (F25$b_dist_D + F25$f_per_group_pr_D[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (F25$b_dist_D + F25$f_per_group_pr_D[,4,3])+   # Dist (A): COL More Freq. Users
    (F25$b_dist_D + F25$f_per_group_pr_D[,6,3])+   # Dist (A): CZE More Freq. Users
    (F25$b_dist_D + F25$f_per_group_pr_D[,8,3])+   # Dist (A): ZAF More Freq. Users
    (F25$b_dist_D + F25$f_per_group_pr_D[,10,3])+   # Dist (A): TUR More Freq. Users
    (F25$b_dist_D + F25$f_per_group_pr_D[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
DOM_Asym_F_freq <- (
  (F25$b_FA_D + F25$f_per_group_pr_D[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (F25$b_FA_D + F25$f_per_group_pr_D[,3,4])+   # Asym (A): COL More Freq. Users
    (F25$b_FA_D + F25$f_per_group_pr_D[,5,4])+   # Asym (A): CZE More Freq. Users
    (F25$b_FA_D + F25$f_per_group_pr_D[,7,4])+   # Asym (A): ZAF More Freq. Users
    (F25$b_FA_D + F25$f_per_group_pr_D[,9,4])+   # Asym (A): TUR More Freq. Users
    (F25$b_FA_D + F25$f_per_group_pr_D[,11,4])   # Asym (A): COL More Freq. Users
)/6

DOM_Asym_F_INfreq <- (
  (F25$b_FA_D + F25$f_per_group_pr_D[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (F25$b_FA_D + F25$f_per_group_pr_D[,4,4])+   # Asym (A): COL More Freq. Users
    (F25$b_FA_D + F25$f_per_group_pr_D[,6,4])+   # Asym (A): CZE More Freq. Users
    (F25$b_FA_D + F25$f_per_group_pr_D[,8,4])+   # Asym (A): ZAF More Freq. Users
    (F25$b_FA_D + F25$f_per_group_pr_D[,10,4])+   # Asym (A): TUR More Freq. Users
    (F25$b_FA_D + F25$f_per_group_pr_D[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
DOM_SexTyp_F_freq <- (
  (F25$b_sshd_D + F25$f_per_group_pr_D[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (F25$b_sshd_D + F25$f_per_group_pr_D[,3,5])+   # SShD (A): COL More Freq. Users
    (F25$b_sshd_D + F25$f_per_group_pr_D[,5,5])+   # SShD (A): CZE More Freq. Users
    (F25$b_sshd_D + F25$f_per_group_pr_D[,7,5])+   # SShD (A): ZAF More Freq. Users
    (F25$b_sshd_D + F25$f_per_group_pr_D[,9,5])+   # SShD (A): TUR More Freq. Users
    (F25$b_sshd_D + F25$f_per_group_pr_D[,11,5])   # SShD (A): COL More Freq. Users
)/6

DOM_SexTyp_F_INfreq <- (
  (F25$b_sshd_D + F25$f_per_group_pr_D[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (F25$b_sshd_D + F25$f_per_group_pr_D[,4,5])+   # SShD (A): COL More Freq. Users
    (F25$b_sshd_D + F25$f_per_group_pr_D[,6,5])+   # SShD (A): CZE More Freq. Users
    (F25$b_sshd_D + F25$f_per_group_pr_D[,8,5])+   # SShD (A): ZAF More Freq. Users
    (F25$b_sshd_D + F25$f_per_group_pr_D[,10,5])+   # SShD (A): TUR More Freq. Users
    (F25$b_sshd_D + F25$f_per_group_pr_D[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
DOM_L_F_freq <- (
  (F25$b_L_D + F25$f_per_group_pr_D[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (F25$b_L_D + F25$f_per_group_pr_D[,3,6])+   # L (A): COL More Freq. Users
    (F25$b_L_D + F25$f_per_group_pr_D[,5,6])+   # L (A): CZE More Freq. Users
    (F25$b_L_D + F25$f_per_group_pr_D[,7,6])+   # L (A): ZAF More Freq. Users
    (F25$b_L_D + F25$f_per_group_pr_D[,9,6])+   # L (A): TUR More Freq. Users
    (F25$b_L_D + F25$f_per_group_pr_D[,11,6])   # L (A): COL More Freq. Users
)/6

DOM_L_F_INfreq <- (
  (F25$b_L_D + F25$f_per_group_pr_D[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (F25$b_L_D + F25$f_per_group_pr_D[,4,6])+   # L (A): COL More Freq. Users
    (F25$b_L_D + F25$f_per_group_pr_D[,6,6])+   # L (A): CZE More Freq. Users
    (F25$b_L_D + F25$f_per_group_pr_D[,8,6])+   # L (A): ZAF More Freq. Users
    (F25$b_L_D + F25$f_per_group_pr_D[,10,6])+   # L (A): TUR More Freq. Users
    (F25$b_L_D + F25$f_per_group_pr_D[,12,6])   # L (A): COL More Freq. Users
)/6



# GMM - SEPARATELY 

# Dominance - GMM 
str(F25)
str(F25$f_per_group_pr_D) # Mind that 1 is intercept! 

F_posts_ATD_MM_DOM <- list(
  #  AGE
  Age_Slope = (F25$b_age_D),    # Slope prior to accessing culture
  
  DOM_Age_F_freq,
  DOM_Age_F_INfreq,
  
  Age_AUS_Freq = (F25$b_age_D + F25$f_per_group_pr_D[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (F25$b_age_D + F25$f_per_group_pr_D[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (F25$b_age_D + F25$f_per_group_pr_D[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (F25$b_age_D + F25$f_per_group_pr_D[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (F25$b_age_D + F25$f_per_group_pr_D[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (F25$b_age_D + F25$f_per_group_pr_D[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (F25$b_age_D + F25$f_per_group_pr_D[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (F25$b_age_D + F25$f_per_group_pr_D[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (F25$b_age_D + F25$f_per_group_pr_D[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (F25$b_age_D + F25$f_per_group_pr_D[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (F25$b_age_D + F25$f_per_group_pr_D[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (F25$b_age_D + F25$f_per_group_pr_D[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (F25$b_dist_D),    # Dist (A): CZE Control
  
  DOM_Dist_F_freq,
  DOM_Dist_F_INfreq,
  
  Dist_AUS_Freq = (F25$b_dist_D + F25$f_per_group_pr_D[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (F25$b_dist_D + F25$f_per_group_pr_D[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (F25$b_dist_D + F25$f_per_group_pr_D[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (F25$b_dist_D + F25$f_per_group_pr_D[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (F25$b_dist_D + F25$f_per_group_pr_D[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (F25$b_dist_D + F25$f_per_group_pr_D[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (F25$b_dist_D + F25$f_per_group_pr_D[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (F25$b_dist_D + F25$f_per_group_pr_D[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (F25$b_dist_D + F25$f_per_group_pr_D[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (F25$b_dist_D + F25$f_per_group_pr_D[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (F25$b_dist_D + F25$f_per_group_pr_D[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (F25$b_dist_D + F25$f_per_group_pr_D[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (F25$b_FA_D),    # Asym (A): CZE Control
  
  DOM_Asym_F_freq,
  DOM_Asym_F_INfreq,
  
  Asym_AUS_Freq = (F25$b_FA_D + F25$f_per_group_pr_D[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (F25$b_FA_D + F25$f_per_group_pr_D[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (F25$b_FA_D + F25$f_per_group_pr_D[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (F25$b_FA_D + F25$f_per_group_pr_D[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (F25$b_FA_D + F25$f_per_group_pr_D[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (F25$b_FA_D + F25$f_per_group_pr_D[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (F25$b_FA_D + F25$f_per_group_pr_D[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (F25$b_FA_D + F25$f_per_group_pr_D[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (F25$b_FA_D + F25$f_per_group_pr_D[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (F25$b_FA_D + F25$f_per_group_pr_D[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (F25$b_FA_D + F25$f_per_group_pr_D[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (F25$b_FA_D + F25$f_per_group_pr_D[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (F25$b_sshd_D),    # SexTyp (A): CZE Control
  
  DOM_SexTyp_F_freq,
  DOM_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (F25$b_sshd_D + F25$f_per_group_pr_D[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (F25$b_sshd_D + F25$f_per_group_pr_D[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (F25$b_sshd_D + F25$f_per_group_pr_D[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (F25$b_sshd_D + F25$f_per_group_pr_D[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (F25$b_sshd_D + F25$f_per_group_pr_D[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (F25$b_sshd_D + F25$f_per_group_pr_D[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (F25$b_sshd_D + F25$f_per_group_pr_D[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (F25$b_sshd_D + F25$f_per_group_pr_D[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (F25$b_sshd_D + F25$f_per_group_pr_D[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (F25$b_sshd_D + F25$f_per_group_pr_D[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (F25$b_sshd_D + F25$f_per_group_pr_D[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (F25$b_sshd_D + F25$f_per_group_pr_D[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (F25$b_L_D),    # L (A): CZE Control
  
  DOM_L_F_freq,
  DOM_L_F_INfreq,
  
  L_AUS_Freq = (F25$b_L_D + F25$f_per_group_pr_D[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (F25$b_L_D + F25$f_per_group_pr_D[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (F25$b_L_D + F25$f_per_group_pr_D[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (F25$b_L_D + F25$f_per_group_pr_D[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (F25$b_L_D + F25$f_per_group_pr_D[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (F25$b_L_D + F25$f_per_group_pr_D[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (F25$b_L_D + F25$f_per_group_pr_D[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (F25$b_L_D + F25$f_per_group_pr_D[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (F25$b_L_D + F25$f_per_group_pr_D[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (F25$b_L_D + F25$f_per_group_pr_D[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (F25$b_L_D + F25$f_per_group_pr_D[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (F25$b_L_D + F25$f_per_group_pr_D[,12,6])   # L (A): COL Less Freq. Users
)


summary.data.frame(F_posts_ATD_MM_DOM)
str(F_posts_ATD_MM_TRU) # Need 65 rows 



par(mar=c(2.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.2),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Dominance: GMM - Females", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting


# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#3d465490", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#3d465490", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#3d465490", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#3d465490", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#3d465490", lwd = 2)


# Small squares...
rect(-0.24,12.5,0.19,15.25, col="#F0F0F0", border = F)
rect(-0.24,28,0.19,30.75, col="#F0F0F0", border = F)
rect(-0.24,43.5,0.19,46.25, col="#F0F0F0", border = F)
rect(-0.24,59,0.19,61.75, col="#F0F0F0", border = F)
rect(-0.24,74.5,0.19,77.25, col="#F0F0F0", border = F)

# 
segments(x0 = -0.25, y0 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), x1 = 0.2, 
         y1 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), col = "#40808080", lwd = 1.5)

# 1
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 0, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 15.5,
         col="#80808080",lty=2)

# 2
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 16, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 31,
         col="#80808080",lty=2)

# 3
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 31.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 46.5,
         col="#80808080",lty=2)

# 4 
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 47, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 62,
         col="#80808080",lty=2)

# 5
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 62.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 77.5,
         col="#80808080",lty=2)



ys<-c(1:15,16.5:30.5,32:46,47.5:61.5,63:77)
ys<-rev(ys)

labs <- rep(labs,5)

cex_t <- 1.25
cex_t2 <- 1
pls <- 1

for(i in 1:length(F_posts_ATD_MM_DOM)){ 
  toplot<-F_posts_ATD_MM_DOM[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=5, lty=1, col=cols[i])
  segments(x0 = ci[1], y0 = ycoord-0.1, x1 = ci[1], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  segments(x0 = ci[2], y0 = ycoord-0.1, x1 = ci[2], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0=0.0, y0=c(0.0, 16, 31.5, 47, 62.5), x1=0.0, y1=c(15.5, 31, 46.5, 62, 77.5), col="#999999", lwd=2, lty=1)
# 


dev.off()









#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Figure S23 - analogue of the Figure 4 in the main body of the article, which is a figure (top) presenting mean estimates
# in every sample, (bottom) presenting effects of morphometric and demographic predictors: SPECIFICALLY for WOMEN
# Version without labels

# Upper part: Frequent - all
# Compute the average of all group estimates

# Note - these are the same data as those to enter the A-T-D model; therefore, 
# the order of levels is the same, too. 

Avg_Freq_U_A = ( 
  M25$aA + M25$f_per_group_pr_A[,1,1] + 
    M25$aA + M25$f_per_group_pr_A[,3,1] + 
    M25$aA + M25$f_per_group_pr_A[,5,1] + 
    M25$aA + M25$f_per_group_pr_A[,7,1] + 
    M25$aA + M25$f_per_group_pr_A[,9,1] + 
    M25$aA + M25$f_per_group_pr_A[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_A = ( 
  M25$aA + M25$f_per_group_pr_A[,2,1] + 
    M25$aA + M25$f_per_group_pr_A[,4,1] + 
    M25$aA + M25$f_per_group_pr_A[,6,1] + 
    M25$aA + M25$f_per_group_pr_A[,8,1] + 
    M25$aA + M25$f_per_group_pr_A[,10,1] + 
    M25$aA + M25$f_per_group_pr_A[,12,1] 
) / 6



F_posts_ATR <- list(
  # 1 Intercept: 
  Intercept = M25$aA,
  # Infrequent users
  Avg_Freq_U_A,
  # Frequent users 
  Avg_Infreq_U_A,
  # Difference 
  Difference = Avg_Freq_U_A - Avg_Infreq_U_A,
  # 4 Australia - above - median users  
  AUS_Intensive_User = M25$aA + M25$f_per_group_pr_A[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = M25$aA + M25$f_per_group_pr_A[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = M25$aA + M25$f_per_group_pr_A[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = M25$aA + M25$f_per_group_pr_A[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = M25$aA + M25$f_per_group_pr_A[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = M25$aA + M25$f_per_group_pr_A[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = M25$aA + M25$f_per_group_pr_A[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = M25$aA + M25$f_per_group_pr_A[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = M25$aA + M25$f_per_group_pr_A[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = M25$aA + M25$f_per_group_pr_A[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = M25$aA + M25$f_per_group_pr_A[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = M25$aA + M25$f_per_group_pr_A[,12,1]
)

summary.data.frame(F_posts_ATR)
str(F_posts_ATR) # Need 13 rows 

#--------------

# Trustworthiness
Avg_Freq_U_T = ( 
  M25$aT + M25$f_per_group_pr_T[,1,1] + 
    M25$aT + M25$f_per_group_pr_T[,3,1] + 
    M25$aT + M25$f_per_group_pr_T[,5,1] + 
    M25$aT + M25$f_per_group_pr_T[,7,1] + 
    M25$aT + M25$f_per_group_pr_T[,9,1] + 
    M25$aT + M25$f_per_group_pr_T[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_T = ( 
  M25$aT + M25$f_per_group_pr_T[,2,1] + 
    M25$aT + M25$f_per_group_pr_T[,4,1] + 
    M25$aT + M25$f_per_group_pr_T[,6,1] + 
    M25$aT + M25$f_per_group_pr_T[,8,1] + 
    M25$aT + M25$f_per_group_pr_T[,10,1] + 
    M25$aT + M25$f_per_group_pr_T[,12,1] 
) / 6

F_posts_TRU <- list(
  # 1 Intercept: 
  Intercept = M25$aT,
  # Infrequent users
  Avg_Freq_U_T,
  # Frequent users 
  Avg_Infreq_U_T,
  # Difference 
  Difference = Avg_Freq_U_T - Avg_Infreq_U_T,
  # 4 Australia - above - median users  
  AUS_Intensive_User = M25$aT + M25$f_per_group_pr_T[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = M25$aT + M25$f_per_group_pr_T[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = M25$aT + M25$f_per_group_pr_T[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = M25$aT + M25$f_per_group_pr_T[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = M25$aT + M25$f_per_group_pr_T[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = M25$aT + M25$f_per_group_pr_T[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = M25$aT + M25$f_per_group_pr_T[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = M25$aT + M25$f_per_group_pr_T[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = M25$aT + M25$f_per_group_pr_T[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = M25$aT + M25$f_per_group_pr_T[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = M25$aT + M25$f_per_group_pr_T[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = M25$aT + M25$f_per_group_pr_T[,12,1]
)

summary.data.frame(F_posts_TRU)
str(F_posts_TRU) # Need 13 rows 

#--------------

# Dominance
Avg_Freq_U_D = ( 
  M25$aD + M25$f_per_group_pr_D[,1,1] + 
    M25$aD + M25$f_per_group_pr_D[,3,1] + 
    M25$aD + M25$f_per_group_pr_D[,5,1] + 
    M25$aD + M25$f_per_group_pr_D[,7,1] + 
    M25$aD + M25$f_per_group_pr_D[,9,1] + 
    M25$aD + M25$f_per_group_pr_D[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_D = ( 
  M25$aD + M25$f_per_group_pr_D[,2,1] + 
    M25$aD + M25$f_per_group_pr_D[,4,1] + 
    M25$aD + M25$f_per_group_pr_D[,6,1] + 
    M25$aD + M25$f_per_group_pr_D[,8,1] + 
    M25$aD + M25$f_per_group_pr_D[,10,1] + 
    M25$aD + M25$f_per_group_pr_D[,12,1] 
) / 6

F_posts_DOM <- list(
  # 1 Intercept: 
  Intercept = M25$aD,
  # Infrequent users
  Avg_Freq_U_D,
  # Frequent users 
  Avg_Infreq_U_D,
  # Difference 
  Difference = Avg_Freq_U_D - Avg_Infreq_U_D,
  # 4 Australia - above - median users  
  AUS_Intensive_User = M25$aD + M25$f_per_group_pr_D[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = M25$aD + M25$f_per_group_pr_D[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = M25$aD + M25$f_per_group_pr_D[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = M25$aD + M25$f_per_group_pr_D[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = M25$aD + M25$f_per_group_pr_D[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = M25$aD + M25$f_per_group_pr_D[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = M25$aD + M25$f_per_group_pr_D[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = M25$aD + M25$f_per_group_pr_D[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = M25$aD + M25$f_per_group_pr_D[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = M25$aD + M25$f_per_group_pr_D[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = M25$aD + M25$f_per_group_pr_D[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = M25$aD + M25$f_per_group_pr_D[,12,1]
)

summary.data.frame(F_posts_DOM)
str(F_posts_DOM) 


# PLOTTING: 
cols<-c("#00FBFF", 
        "#8C00FF",
        "#FFADD0",
        "#09C9FF",
        
        "#00FF3380",
        "#0FEA9980",
        
        "#FFEA0080",
        "#c9b80080",
        
        "#e0935c80",
        "#ff7b1c80",
        
        "#4796ff80",
        "#1278ff80",
        
        "#acdb5a80",
        "#a1f21380",
        
        "#6174ed80",
        "#223ef080"
) 



labs<-c("Intercept",
        "Above",
        "Below",
        "Above - Below: Difference",
        "Australia/NewZ - Above",
        "Australia/NewZ - Below",
        "Colombia - Above",
        "Colombia - Below",
        "Czechia - Above",
        "Czechia - Below",
        "South Africa - Above",
        "South Africa - Below",
        "Turkey - Above",
        "Turkey - Below",
        "Vietnam - Above",
        "Vietnam - Below"
)



# Figure S3:Upper Panel - Analogue of Figure 3:Upper Panel (final assembly done in InkScape)...

tiff("M_ATD_SES_ver_2_LEVELS_NOLABELS.tiff",width=30,height=14,units="cm",res=600,compression="lzw")
layout(matrix(1:3,byrow=F,nrow=1), widths=c(1.8,1.2,1.2),heights=c(1))

# ATTRACTIVENESS

par(mar=c(3.1, 15.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-1,0.7),ylim=c(1,17),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Attractiveness - Group Estimates", side = 3, line = -0.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

rect(-1,0.5,0.75,17, col=F, border = "#b9b9B999", lwd = 2)
rect(-0.95,12.5,0.7,16.5, col="#F0F0F0", border = F)

segments(x0 = -1, y0 = 1:16, x1 = 0.75, y1 = 1:16, col = "#40808080", lwd = 1.5)
segments(x0 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y0 = 0.5, x1 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y1 = 16.5, col="#80808080",lty=2)

ys<-seq(from=1,to=16, by=1)
ys<-rev(ys)

cex_t <- 1.25
cex_t2 <- 0.95
pls <- 0.75


for(i in 1:length(F_posts_ATR)){ 
  toplot<-F_posts_ATR[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  textlab<-labs[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  axis(2, at=ycoord, labels=textlab, srt=45, las=2, cex.axis=1.2, tick=F)
  for (i in 1:16) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.007)+ycoord[i],border=T, lwd = 1, col=colpol))
  }
  #lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0 = 0, y0 = 0, x1 = 0, y1 = 16.5, col = "#999999", lwd = 2, lty = 1)



# TRUSTWORTHINESS
par(mar=c(3.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-1,0.7),ylim=c(1,17),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Trustworthiness - Group Estimates", side = 3, line = -0.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

rect(-1,0.5,0.75,17, col=F, border = "#b9b9B999", lwd = 2)
rect(-0.95,12.5,0.7,16.5, col="#F0F0F0", border = F)

segments(x0 = -1, y0 = 1:16, x1 = 0.75, y1 = 1:16, col = "#40808080", lwd = 1.5)
segments(x0 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y0 = 0.5, x1 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y1 = 16.5, col="#80808080",lty=2)

ys<-seq(from=1,to=16, by=1)
ys<-rev(ys)

for(i in 1:length(F_posts_TRU)){ 
  toplot<-F_posts_TRU[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  for (i in 1:16) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.007)+ycoord[i],border=T, lwd = 1, col=colpol))
  }
  #lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0 = 0, y0 = 0, x1 = 0, y1 = 16.5, col = "#999999", lwd = 2, lty = 1)




# DOMINANCE
par(mar=c(3.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-1,0.7),ylim=c(1,17),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Dominance - Group Estimates", side = 3, line = -0.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

rect(-1,0.5,0.75,17, col=F, border = "#b9b9B999", lwd = 2)
rect(-0.95,12.5,0.7,16.5, col="#F0F0F0", border = F)

segments(x0 = -1, y0 = 1:16, x1 = 0.75, y1 = 1:16, col = "#40808080", lwd = 1.5)
segments(x0 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y0 = 0.5, x1 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y1 = 16.5, col="#80808080",lty=2)

ys<-seq(from=1,to=16, by=1)
ys<-rev(ys)


for(i in 1:length(F_posts_DOM)){ 
  toplot<-F_posts_DOM[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  for (i in 1:16) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.007)+ycoord[i],border=T, lwd = 1, col=colpol))
  }
  #lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0 = 0, y0 = 0, x1 = 0, y1 = 16.5, col = "#999999", lwd = 2, lty = 1)


dev.off()


#---------------------------------

# And now GMM: 


# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS
# ATTRACTIVENESS


# Preparing composite coefficients: 
# AGE:
Attr_Age_F_freq <- c(
  (M25$b_age_A + M25$f_per_group_pr_A[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (M25$b_age_A + M25$f_per_group_pr_A[,3,2])+   # Age (A): COL More Freq. Users
    (M25$b_age_A + M25$f_per_group_pr_A[,5,2])+   # Age (A): CZE More Freq. Users
    (M25$b_age_A + M25$f_per_group_pr_A[,7,2])+   # Age (A): ZAF More Freq. Users
    (M25$b_age_A + M25$f_per_group_pr_A[,9,2])+   # Age (A): TUR More Freq. Users
    (M25$b_age_A + M25$f_per_group_pr_A[,11,2])   # Age (A): COL More Freq. Users
)/6

Attr_Age_F_INfreq <- (
  (M25$b_age_A + M25$f_per_group_pr_A[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (M25$b_age_A + M25$f_per_group_pr_A[,4,2])+   # Age (A): COL More Freq. Users
    (M25$b_age_A + M25$f_per_group_pr_A[,6,2])+   # Age (A): CZE More Freq. Users
    (M25$b_age_A + M25$f_per_group_pr_A[,8,2])+   # Age (A): ZAF More Freq. Users
    (M25$b_age_A + M25$f_per_group_pr_A[,10,2])+   # Age (A): TUR More Freq. Users
    (M25$b_age_A + M25$f_per_group_pr_A[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
Attr_Dist_F_freq <- (
  (M25$b_dist_A + M25$f_per_group_pr_A[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (M25$b_dist_A + M25$f_per_group_pr_A[,3,3])+   # Dist (A): COL More Freq. Users
    (M25$b_dist_A + M25$f_per_group_pr_A[,5,3])+   # Dist (A): CZE More Freq. Users
    (M25$b_dist_A + M25$f_per_group_pr_A[,7,3])+   # Dist (A): ZAF More Freq. Users
    (M25$b_dist_A + M25$f_per_group_pr_A[,9,3])+   # Dist (A): TUR More Freq. Users
    (M25$b_dist_A + M25$f_per_group_pr_A[,11,3])   # Dist (A): COL More Freq. Users
)/6

Attr_Dist_F_INfreq <- (
  (M25$b_dist_A + M25$f_per_group_pr_A[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (M25$b_dist_A + M25$f_per_group_pr_A[,4,3])+   # Dist (A): COL More Freq. Users
    (M25$b_dist_A + M25$f_per_group_pr_A[,6,3])+   # Dist (A): CZE More Freq. Users
    (M25$b_dist_A + M25$f_per_group_pr_A[,8,3])+   # Dist (A): ZAF More Freq. Users
    (M25$b_dist_A + M25$f_per_group_pr_A[,10,3])+   # Dist (A): TUR More Freq. Users
    (M25$b_dist_A + M25$f_per_group_pr_A[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
Attr_Asym_F_freq <- (
  (M25$b_FA_A + M25$f_per_group_pr_A[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (M25$b_FA_A + M25$f_per_group_pr_A[,3,4])+   # Asym (A): COL More Freq. Users
    (M25$b_FA_A + M25$f_per_group_pr_A[,5,4])+   # Asym (A): CZE More Freq. Users
    (M25$b_FA_A + M25$f_per_group_pr_A[,7,4])+   # Asym (A): ZAF More Freq. Users
    (M25$b_FA_A + M25$f_per_group_pr_A[,9,4])+   # Asym (A): TUR More Freq. Users
    (M25$b_FA_A + M25$f_per_group_pr_A[,11,4])   # Asym (A): COL More Freq. Users
)/6

Attr_Asym_F_INfreq <- (
  (M25$b_FA_A + M25$f_per_group_pr_A[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (M25$b_FA_A + M25$f_per_group_pr_A[,4,4])+   # Asym (A): COL More Freq. Users
    (M25$b_FA_A + M25$f_per_group_pr_A[,6,4])+   # Asym (A): CZE More Freq. Users
    (M25$b_FA_A + M25$f_per_group_pr_A[,8,4])+   # Asym (A): ZAF More Freq. Users
    (M25$b_FA_A + M25$f_per_group_pr_A[,10,4])+   # Asym (A): TUR More Freq. Users
    (M25$b_FA_A + M25$f_per_group_pr_A[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
Attr_SexTyp_F_freq <- (
  (M25$b_sshd_A + M25$f_per_group_pr_A[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (M25$b_sshd_A + M25$f_per_group_pr_A[,3,5])+   # SShD (A): COL More Freq. Users
    (M25$b_sshd_A + M25$f_per_group_pr_A[,5,5])+   # SShD (A): CZE More Freq. Users
    (M25$b_sshd_A + M25$f_per_group_pr_A[,7,5])+   # SShD (A): ZAF More Freq. Users
    (M25$b_sshd_A + M25$f_per_group_pr_A[,9,5])+   # SShD (A): TUR More Freq. Users
    (M25$b_sshd_A + M25$f_per_group_pr_A[,11,5])   # SShD (A): COL More Freq. Users
)/6

Attr_SexTyp_F_INfreq <- (
  (M25$b_sshd_A + M25$f_per_group_pr_A[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (M25$b_sshd_A + M25$f_per_group_pr_A[,4,5])+   # SShD (A): COL More Freq. Users
    (M25$b_sshd_A + M25$f_per_group_pr_A[,6,5])+   # SShD (A): CZE More Freq. Users
    (M25$b_sshd_A + M25$f_per_group_pr_A[,8,5])+   # SShD (A): ZAF More Freq. Users
    (M25$b_sshd_A + M25$f_per_group_pr_A[,10,5])+   # SShD (A): TUR More Freq. Users
    (M25$b_sshd_A + M25$f_per_group_pr_A[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
Attr_L_F_freq <- (
  (M25$b_L_A + M25$f_per_group_pr_A[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (M25$b_L_A + M25$f_per_group_pr_A[,3,6])+   # L (A): COL More Freq. Users
    (M25$b_L_A + M25$f_per_group_pr_A[,5,6])+   # L (A): CZE More Freq. Users
    (M25$b_L_A + M25$f_per_group_pr_A[,7,6])+   # L (A): ZAF More Freq. Users
    (M25$b_L_A + M25$f_per_group_pr_A[,9,6])+   # L (A): TUR More Freq. Users
    (M25$b_L_A + M25$f_per_group_pr_A[,11,6])   # L (A): COL More Freq. Users
)/6

Attr_L_F_INfreq <- (
  (M25$b_L_A + M25$f_per_group_pr_A[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (M25$b_L_A + M25$f_per_group_pr_A[,4,6])+   # L (A): COL More Freq. Users
    (M25$b_L_A + M25$f_per_group_pr_A[,6,6])+   # L (A): CZE More Freq. Users
    (M25$b_L_A + M25$f_per_group_pr_A[,8,6])+   # L (A): ZAF More Freq. Users
    (M25$b_L_A + M25$f_per_group_pr_A[,10,6])+   # L (A): TUR More Freq. Users
    (M25$b_L_A + M25$f_per_group_pr_A[,12,6])   # L (A): COL More Freq. Users
)/6



# GMM - SEPARATELY 

# Attractiveness - GMM 
str(M25)
str(M25$f_per_group_pr_A) # Mind that 1 is intercept! 

F_posts_ATD_MM_Attr <- list(
  #  AGE
  Age_Slope = (M25$b_age_A),    # Slope prior to accessing culture
  
  Attr_Age_F_freq,
  Attr_Age_F_INfreq,
  
  Age_AUS_Freq = (M25$b_age_A + M25$f_per_group_pr_A[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (M25$b_age_A + M25$f_per_group_pr_A[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (M25$b_age_A + M25$f_per_group_pr_A[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (M25$b_age_A + M25$f_per_group_pr_A[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (M25$b_age_A + M25$f_per_group_pr_A[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (M25$b_age_A + M25$f_per_group_pr_A[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (M25$b_age_A + M25$f_per_group_pr_A[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (M25$b_age_A + M25$f_per_group_pr_A[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (M25$b_age_A + M25$f_per_group_pr_A[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (M25$b_age_A + M25$f_per_group_pr_A[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (M25$b_age_A + M25$f_per_group_pr_A[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (M25$b_age_A + M25$f_per_group_pr_A[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (M25$b_dist_A),    # Dist (A): CZE Control
  
  Attr_Dist_F_freq,
  Attr_Dist_F_INfreq,
  
  Dist_AUS_Freq = (M25$b_dist_A + M25$f_per_group_pr_A[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (M25$b_dist_A + M25$f_per_group_pr_A[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (M25$b_dist_A + M25$f_per_group_pr_A[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (M25$b_dist_A + M25$f_per_group_pr_A[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (M25$b_dist_A + M25$f_per_group_pr_A[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (M25$b_dist_A + M25$f_per_group_pr_A[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (M25$b_dist_A + M25$f_per_group_pr_A[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (M25$b_dist_A + M25$f_per_group_pr_A[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (M25$b_dist_A + M25$f_per_group_pr_A[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (M25$b_dist_A + M25$f_per_group_pr_A[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (M25$b_dist_A + M25$f_per_group_pr_A[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (M25$b_dist_A + M25$f_per_group_pr_A[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (M25$b_FA_A),    # Asym (A): CZE Control
  
  Attr_Asym_F_freq,
  Attr_Asym_F_INfreq,
  
  Asym_AUS_Freq = (M25$b_FA_A + M25$f_per_group_pr_A[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (M25$b_FA_A + M25$f_per_group_pr_A[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (M25$b_FA_A + M25$f_per_group_pr_A[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (M25$b_FA_A + M25$f_per_group_pr_A[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (M25$b_FA_A + M25$f_per_group_pr_A[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (M25$b_FA_A + M25$f_per_group_pr_A[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (M25$b_FA_A + M25$f_per_group_pr_A[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (M25$b_FA_A + M25$f_per_group_pr_A[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (M25$b_FA_A + M25$f_per_group_pr_A[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (M25$b_FA_A + M25$f_per_group_pr_A[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (M25$b_FA_A + M25$f_per_group_pr_A[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (M25$b_FA_A + M25$f_per_group_pr_A[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (M25$b_sshd_A),    # SexTyp (A): CZE Control
  
  Attr_SexTyp_F_freq,
  Attr_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (M25$b_sshd_A + M25$f_per_group_pr_A[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (M25$b_sshd_A + M25$f_per_group_pr_A[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (M25$b_sshd_A + M25$f_per_group_pr_A[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (M25$b_sshd_A + M25$f_per_group_pr_A[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (M25$b_sshd_A + M25$f_per_group_pr_A[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (M25$b_sshd_A + M25$f_per_group_pr_A[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (M25$b_sshd_A + M25$f_per_group_pr_A[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (M25$b_sshd_A + M25$f_per_group_pr_A[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (M25$b_sshd_A + M25$f_per_group_pr_A[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (M25$b_sshd_A + M25$f_per_group_pr_A[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (M25$b_sshd_A + M25$f_per_group_pr_A[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (M25$b_sshd_A + M25$f_per_group_pr_A[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (M25$b_L_A),    # L (A): CZE Control
  
  Attr_L_F_freq,
  Attr_L_F_INfreq,
  
  L_AUS_Freq = (M25$b_L_A + M25$f_per_group_pr_A[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (M25$b_L_A + M25$f_per_group_pr_A[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (M25$b_L_A + M25$f_per_group_pr_A[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (M25$b_L_A + M25$f_per_group_pr_A[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (M25$b_L_A + M25$f_per_group_pr_A[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (M25$b_L_A + M25$f_per_group_pr_A[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (M25$b_L_A + M25$f_per_group_pr_A[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (M25$b_L_A + M25$f_per_group_pr_A[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (M25$b_L_A + M25$f_per_group_pr_A[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (M25$b_L_A + M25$f_per_group_pr_A[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (M25$b_L_A + M25$f_per_group_pr_A[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (M25$b_L_A + M25$f_per_group_pr_A[,12,6])   # L (A): COL Less Freq. Users
)


summary.data.frame(F_posts_ATD_MM_Attr)
str(F_posts_ATD_MM_Attr) # Need 65 rows 


labs <- c("Fixed Slope",
          
          "Above (All)",
          "Below (All)",
          
          "AUS/NZE Above",
          "AUS/NZE Below",
          
          "COL Above",
          "COL Below",
          
          "CZE Above",
          "CZE Below",
          
          "ZAF Above",
          "ZAF Below",
          
          "TUR Above",
          "TUR Below",
          
          "VNM Above",
          "VNM Below"
)

cols<-c("#c56043", "#e04b5c", "#de5353", "#c25c42", "#d85652", "#c8585d", "#de534b", "#cd5440", "#d35d5e", "#c3615a", "#d46655", "#cf5552", "#c65f5d","#de534b", "#cd5440",
        "#856554", "#896757", "#815b69", "#8f6756", "#86554d", "#8a675d", "#85586a", "#805158", "#855d52", "#8f4e50", "#8e5960", "#80685a", "#75584c","#85586a", "#805158",
        "#85b68f", "#77bf98", "#86b18f", "#85b589", "#7eB68d", "#8bB37a", "#80B491", "#65Bd7d", "#81B784", "#8bb883", "#8ebf92", "#7bBe8f", "#91bd79","#81B784", "#8bb883",
        "#6b9168", "#69925b", "#689d55", "#6ba261", "#5aaa6b", "#599866", "#519c58", "#55a755", "#5caa53", "#5c9c60", "#588f5b", "#5e9d6f", "#5aa757","#5caa53", "#5c9c60", 
        "#64a9e6", "#79a5ee", "#809be9", "#6d9dfa", "#6899ec", "#6facde", "#77abed", "#63aaf0", "#63a4e4", "#74a5e3", "#79a9eb", "#6aaaf6", "#78a4dd","#6facde", "#77abed"
        
) 




tiff("M_ATD_GMM_SES_ver_2_no_COEFS.tiff",width=30,height=33,units="cm",res=600,compression="lzw")
layout(matrix(1:3,byrow=F,nrow=1), widths=c(1.1,0.7,0.7),heights=c(1))

par(mar=c(2.1, 14.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.2),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Attr: GMM - Males", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#3d465490", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#3d465490", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#3d465490", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#3d465490", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#3d465490", lwd = 2)


# Small squares...
rect(-0.24,12.5,0.19,15.25, col="#F0F0F0", border = F)
rect(-0.24,28,0.19,30.75, col="#F0F0F0", border = F)
rect(-0.24,43.5,0.19,46.25, col="#F0F0F0", border = F)
rect(-0.24,59,0.19,61.75, col="#F0F0F0", border = F)
rect(-0.24,74.5,0.19,77.25, col="#F0F0F0", border = F)

# 
segments(x0 = -0.25, y0 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), x1 = 0.2, 
         y1 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), col = "#40808080", lwd = 1.5)

# 1
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 0, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 15.5,
         col="#80808080",lty=2)

# 2
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 16, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 31,
         col="#80808080",lty=2)

# 3
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 31.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 46.5,
         col="#80808080",lty=2)

# 4 
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 47, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 62,
         col="#80808080",lty=2)

# 5
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 62.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 77.5,
         col="#80808080",lty=2)

labs <- rep(labs,5)

ys<-c(1:15,16.5:30.5,32:46,47.5:61.5,63:77)
ys<-rev(ys)


for(i in 1:length(F_posts_ATD_MM_Attr)){ 
  toplot<-F_posts_ATD_MM_Attr[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  textlab<-labs[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  axis(2, at=ycoord, labels=textlab, srt=45, las=2, cex.axis=1.2, tick=F)
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=5, lty=1, col=cols[i])
  segments(x0 = ci[1], y0 = ycoord-0.1, x1 = ci[1], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  segments(x0 = ci[2], y0 = ycoord-0.1, x1 = ci[2], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0=0.0, y0=c(0.0, 16, 31.5, 47, 62.5), x1=0.0, y1=c(15.5, 31, 46.5, 62, 77.5), col="#999999", lwd=2, lty=1)

text(x = -0.3, y = 77, labels = "Age", cex = 0.4, pos = 4)
text(x = -0.3, y = 62, labels = "Dist", cex = 0.4, pos = 4)
text(x = -0.3, y = 47, labels = "Asym", cex = 0.4, pos = 4)
text(x = -0.3, y = 32, labels = "SShD", cex = 0.4, pos = 4)
text(x = -0.3, y = 15, labels = "Lightness", cex = 0.4, pos = 4)

# 



# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 

# And now GMM: 
# Preparing composite coefficients: 
# AGE:
TR_Age_F_freq <- c(
  (M25$b_age_T + M25$f_per_group_pr_T[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (M25$b_age_T + M25$f_per_group_pr_T[,3,2])+   # Age (A): COL More Freq. Users
    (M25$b_age_T + M25$f_per_group_pr_T[,5,2])+   # Age (A): CZE More Freq. Users
    (M25$b_age_T + M25$f_per_group_pr_T[,7,2])+   # Age (A): ZAF More Freq. Users
    (M25$b_age_T + M25$f_per_group_pr_T[,9,2])+   # Age (A): TUR More Freq. Users
    (M25$b_age_T + M25$f_per_group_pr_T[,11,2])   # Age (A): COL More Freq. Users
)/6

TR_Age_F_INfreq <- (
  (M25$b_age_T + M25$f_per_group_pr_T[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (M25$b_age_T + M25$f_per_group_pr_T[,4,2])+   # Age (A): COL More Freq. Users
    (M25$b_age_T + M25$f_per_group_pr_T[,6,2])+   # Age (A): CZE More Freq. Users
    (M25$b_age_T + M25$f_per_group_pr_T[,8,2])+   # Age (A): ZAF More Freq. Users
    (M25$b_age_T + M25$f_per_group_pr_T[,10,2])+   # Age (A): TUR More Freq. Users
    (M25$b_age_T + M25$f_per_group_pr_T[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
TR_Dist_F_freq <- (
  (M25$b_dist_T + M25$f_per_group_pr_T[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (M25$b_dist_T + M25$f_per_group_pr_T[,3,3])+   # Dist (A): COL More Freq. Users
    (M25$b_dist_T + M25$f_per_group_pr_T[,5,3])+   # Dist (A): CZE More Freq. Users
    (M25$b_dist_T + M25$f_per_group_pr_T[,7,3])+   # Dist (A): ZAF More Freq. Users
    (M25$b_dist_T + M25$f_per_group_pr_T[,9,3])+   # Dist (A): TUR More Freq. Users
    (M25$b_dist_T + M25$f_per_group_pr_T[,11,3])   # Dist (A): COL More Freq. Users
)/6

TR_Dist_F_INfreq <- (
  (M25$b_dist_T + M25$f_per_group_pr_T[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (M25$b_dist_T + M25$f_per_group_pr_T[,4,3])+   # Dist (A): COL More Freq. Users
    (M25$b_dist_T + M25$f_per_group_pr_T[,6,3])+   # Dist (A): CZE More Freq. Users
    (M25$b_dist_T + M25$f_per_group_pr_T[,8,3])+   # Dist (A): ZAF More Freq. Users
    (M25$b_dist_T + M25$f_per_group_pr_T[,10,3])+   # Dist (A): TUR More Freq. Users
    (M25$b_dist_T + M25$f_per_group_pr_T[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
TR_Asym_F_freq <- (
  (M25$b_FA_T + M25$f_per_group_pr_T[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (M25$b_FA_T + M25$f_per_group_pr_T[,3,4])+   # Asym (A): COL More Freq. Users
    (M25$b_FA_T + M25$f_per_group_pr_T[,5,4])+   # Asym (A): CZE More Freq. Users
    (M25$b_FA_T + M25$f_per_group_pr_T[,7,4])+   # Asym (A): ZAF More Freq. Users
    (M25$b_FA_T + M25$f_per_group_pr_T[,9,4])+   # Asym (A): TUR More Freq. Users
    (M25$b_FA_T + M25$f_per_group_pr_T[,11,4])   # Asym (A): COL More Freq. Users
)/6

TR_Asym_F_INfreq <- (
  (M25$b_FA_T + M25$f_per_group_pr_T[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (M25$b_FA_T + M25$f_per_group_pr_T[,4,4])+   # Asym (A): COL More Freq. Users
    (M25$b_FA_T + M25$f_per_group_pr_T[,6,4])+   # Asym (A): CZE More Freq. Users
    (M25$b_FA_T + M25$f_per_group_pr_T[,8,4])+   # Asym (A): ZAF More Freq. Users
    (M25$b_FA_T + M25$f_per_group_pr_T[,10,4])+   # Asym (A): TUR More Freq. Users
    (M25$b_FA_T + M25$f_per_group_pr_T[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
TR_SexTyp_F_freq <- (
  (M25$b_sshd_T + M25$f_per_group_pr_T[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (M25$b_sshd_T + M25$f_per_group_pr_T[,3,5])+   # SShD (A): COL More Freq. Users
    (M25$b_sshd_T + M25$f_per_group_pr_T[,5,5])+   # SShD (A): CZE More Freq. Users
    (M25$b_sshd_T + M25$f_per_group_pr_T[,7,5])+   # SShD (A): ZAF More Freq. Users
    (M25$b_sshd_T + M25$f_per_group_pr_T[,9,5])+   # SShD (A): TUR More Freq. Users
    (M25$b_sshd_T + M25$f_per_group_pr_T[,11,5])   # SShD (A): COL More Freq. Users
)/6

TR_SexTyp_F_INfreq <- (
  (M25$b_sshd_T + M25$f_per_group_pr_T[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (M25$b_sshd_T + M25$f_per_group_pr_T[,4,5])+   # SShD (A): COL More Freq. Users
    (M25$b_sshd_T + M25$f_per_group_pr_T[,6,5])+   # SShD (A): CZE More Freq. Users
    (M25$b_sshd_T + M25$f_per_group_pr_T[,8,5])+   # SShD (A): ZAF More Freq. Users
    (M25$b_sshd_T + M25$f_per_group_pr_T[,10,5])+   # SShD (A): TUR More Freq. Users
    (M25$b_sshd_T + M25$f_per_group_pr_T[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
TR_L_F_freq <- (
  (M25$b_L_T + M25$f_per_group_pr_T[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (M25$b_L_T + M25$f_per_group_pr_T[,3,6])+   # L (A): COL More Freq. Users
    (M25$b_L_T + M25$f_per_group_pr_T[,5,6])+   # L (A): CZE More Freq. Users
    (M25$b_L_T + M25$f_per_group_pr_T[,7,6])+   # L (A): ZAF More Freq. Users
    (M25$b_L_T + M25$f_per_group_pr_T[,9,6])+   # L (A): TUR More Freq. Users
    (M25$b_L_T + M25$f_per_group_pr_T[,11,6])   # L (A): COL More Freq. Users
)/6

TR_L_F_INfreq <- (
  (M25$b_L_T + M25$f_per_group_pr_T[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (M25$b_L_T + M25$f_per_group_pr_T[,4,6])+   # L (A): COL More Freq. Users
    (M25$b_L_T + M25$f_per_group_pr_T[,6,6])+   # L (A): CZE More Freq. Users
    (M25$b_L_T + M25$f_per_group_pr_T[,8,6])+   # L (A): ZAF More Freq. Users
    (M25$b_L_T + M25$f_per_group_pr_T[,10,6])+   # L (A): TUR More Freq. Users
    (M25$b_L_T + M25$f_per_group_pr_T[,12,6])   # L (A): COL More Freq. Users
)/6



# Trusw - GMM 
str(M25)
str(M25$f_per_group_pr_T) # Mind that 1 is intercept! 

F_posts_ATD_MM_TRU <- list(
  #  AGE
  Age_Slope = (M25$b_age_T),    # Slope prior to accessing culture
  
  TR_Age_F_freq,
  TR_Age_F_INfreq,
  
  Age_AUS_Freq = (M25$b_age_T + M25$f_per_group_pr_T[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (M25$b_age_T + M25$f_per_group_pr_T[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (M25$b_age_T + M25$f_per_group_pr_T[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (M25$b_age_T + M25$f_per_group_pr_T[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (M25$b_age_T + M25$f_per_group_pr_T[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (M25$b_age_T + M25$f_per_group_pr_T[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (M25$b_age_T + M25$f_per_group_pr_T[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (M25$b_age_T + M25$f_per_group_pr_T[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (M25$b_age_T + M25$f_per_group_pr_T[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (M25$b_age_T + M25$f_per_group_pr_T[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (M25$b_age_T + M25$f_per_group_pr_T[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (M25$b_age_T + M25$f_per_group_pr_T[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (M25$b_dist_T),    # Dist (A): CZE Control
  
  TR_Dist_F_freq,
  TR_Dist_F_INfreq,
  
  Dist_AUS_Freq = (M25$b_dist_T + M25$f_per_group_pr_T[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (M25$b_dist_T + M25$f_per_group_pr_T[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (M25$b_dist_T + M25$f_per_group_pr_T[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (M25$b_dist_T + M25$f_per_group_pr_T[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (M25$b_dist_T + M25$f_per_group_pr_T[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (M25$b_dist_T + M25$f_per_group_pr_T[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (M25$b_dist_T + M25$f_per_group_pr_T[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (M25$b_dist_T + M25$f_per_group_pr_T[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (M25$b_dist_T + M25$f_per_group_pr_T[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (M25$b_dist_T + M25$f_per_group_pr_T[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (M25$b_dist_T + M25$f_per_group_pr_T[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (M25$b_dist_T + M25$f_per_group_pr_T[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (M25$b_FA_T),    # Asym (A): CZE Control
  
  TR_Asym_F_freq,
  TR_Asym_F_INfreq,
  
  Asym_AUS_Freq = (M25$b_FA_T + M25$f_per_group_pr_T[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (M25$b_FA_T + M25$f_per_group_pr_T[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (M25$b_FA_T + M25$f_per_group_pr_T[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (M25$b_FA_T + M25$f_per_group_pr_T[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (M25$b_FA_T + M25$f_per_group_pr_T[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (M25$b_FA_T + M25$f_per_group_pr_T[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (M25$b_FA_T + M25$f_per_group_pr_T[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (M25$b_FA_T + M25$f_per_group_pr_T[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (M25$b_FA_T + M25$f_per_group_pr_T[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (M25$b_FA_T + M25$f_per_group_pr_T[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (M25$b_FA_T + M25$f_per_group_pr_T[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (M25$b_FA_T + M25$f_per_group_pr_T[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (M25$b_sshd_T),    # SexTyp (A): CZE Control
  
  TR_SexTyp_F_freq,
  TR_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (M25$b_sshd_T + M25$f_per_group_pr_T[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (M25$b_sshd_T + M25$f_per_group_pr_T[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (M25$b_sshd_T + M25$f_per_group_pr_T[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (M25$b_sshd_T + M25$f_per_group_pr_T[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (M25$b_sshd_T + M25$f_per_group_pr_T[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (M25$b_sshd_T + M25$f_per_group_pr_T[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (M25$b_sshd_T + M25$f_per_group_pr_T[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (M25$b_sshd_T + M25$f_per_group_pr_T[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (M25$b_sshd_T + M25$f_per_group_pr_T[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (M25$b_sshd_T + M25$f_per_group_pr_T[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (M25$b_sshd_T + M25$f_per_group_pr_T[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (M25$b_sshd_T + M25$f_per_group_pr_T[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (M25$b_L_T),    # L (A): CZE Control
  
  TR_L_F_freq,
  TR_L_F_INfreq,
  
  L_AUS_Freq = (M25$b_L_T + M25$f_per_group_pr_T[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (M25$b_L_T + M25$f_per_group_pr_T[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (M25$b_L_T + M25$f_per_group_pr_T[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (M25$b_L_T + M25$f_per_group_pr_T[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (M25$b_L_T + M25$f_per_group_pr_T[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (M25$b_L_T + M25$f_per_group_pr_T[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (M25$b_L_T + M25$f_per_group_pr_T[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (M25$b_L_T + M25$f_per_group_pr_T[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (M25$b_L_T + M25$f_per_group_pr_T[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (M25$b_L_T + M25$f_per_group_pr_T[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (M25$b_L_T + M25$f_per_group_pr_T[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (M25$b_L_T + M25$f_per_group_pr_T[,12,6])   # L (A): COL Less Freq. Users
)


summary.data.frame(F_posts_ATD_MM_TRU)
str(F_posts_ATD_MM_TRU) # Need 65 rows 



par(mar=c(2.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.2),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Trustw: GMM - Males", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting


# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#3d465490", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#3d465490", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#3d465490", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#3d465490", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#3d465490", lwd = 2)


# Small squares...
rect(-0.24,12.5,0.19,15.25, col="#F0F0F0", border = F)
rect(-0.24,28,0.19,30.75, col="#F0F0F0", border = F)
rect(-0.24,43.5,0.19,46.25, col="#F0F0F0", border = F)
rect(-0.24,59,0.19,61.75, col="#F0F0F0", border = F)
rect(-0.24,74.5,0.19,77.25, col="#F0F0F0", border = F)

# 
segments(x0 = -0.25, y0 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), x1 = 0.2, 
         y1 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), col = "#40808080", lwd = 1.5)

# 1
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 0, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 15.5,
         col="#80808080",lty=2)

# 2
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 16, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 31,
         col="#80808080",lty=2)

# 3
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 31.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 46.5,
         col="#80808080",lty=2)

# 4 
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 47, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 62,
         col="#80808080",lty=2)

# 5
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 62.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 77.5,
         col="#80808080",lty=2)



ys<-c(1:15,16.5:30.5,32:46,47.5:61.5,63:77)
ys<-rev(ys)

labs <- rep(labs,5)


for(i in 1:length(F_posts_ATD_MM_TRU)){ 
  toplot<-F_posts_ATD_MM_TRU[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=5, lty=1, col=cols[i])
  segments(x0 = ci[1], y0 = ycoord-0.1, x1 = ci[1], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  segments(x0 = ci[2], y0 = ycoord-0.1, x1 = ci[2], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0=0.0, y0=c(0.0, 16, 31.5, 47, 62.5), x1=0.0, y1=c(15.5, 31, 46.5, 62, 77.5), col="#999999", lwd=2, lty=1)
# 





# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 

# And now GMM: 
# Preparing composite coefficients: 
# AGE:
DOM_Age_F_freq <- c(
  (M25$b_age_D + M25$f_per_group_pr_D[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (M25$b_age_D + M25$f_per_group_pr_D[,3,2])+   # Age (A): COL More Freq. Users
    (M25$b_age_D + M25$f_per_group_pr_D[,5,2])+   # Age (A): CZE More Freq. Users
    (M25$b_age_D + M25$f_per_group_pr_D[,7,2])+   # Age (A): ZAF More Freq. Users
    (M25$b_age_D + M25$f_per_group_pr_D[,9,2])+   # Age (A): TUR More Freq. Users
    (M25$b_age_D + M25$f_per_group_pr_D[,11,2])   # Age (A): COL More Freq. Users
)/6

DOM_Age_F_INfreq <- (
  (M25$b_age_D + M25$f_per_group_pr_D[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (M25$b_age_D + M25$f_per_group_pr_D[,4,2])+   # Age (A): COL More Freq. Users
    (M25$b_age_D + M25$f_per_group_pr_D[,6,2])+   # Age (A): CZE More Freq. Users
    (M25$b_age_D + M25$f_per_group_pr_D[,8,2])+   # Age (A): ZAF More Freq. Users
    (M25$b_age_D + M25$f_per_group_pr_D[,10,2])+   # Age (A): TUR More Freq. Users
    (M25$b_age_D + M25$f_per_group_pr_D[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
DOM_Dist_F_freq <- (
  (M25$b_dist_D + M25$f_per_group_pr_D[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (M25$b_dist_D + M25$f_per_group_pr_D[,3,3])+   # Dist (A): COL More Freq. Users
    (M25$b_dist_D + M25$f_per_group_pr_D[,5,3])+   # Dist (A): CZE More Freq. Users
    (M25$b_dist_D + M25$f_per_group_pr_D[,7,3])+   # Dist (A): ZAF More Freq. Users
    (M25$b_dist_D + M25$f_per_group_pr_D[,9,3])+   # Dist (A): TUR More Freq. Users
    (M25$b_dist_D + M25$f_per_group_pr_D[,11,3])   # Dist (A): COL More Freq. Users
)/6

DOM_Dist_F_INfreq <- (
  (M25$b_dist_D + M25$f_per_group_pr_D[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (M25$b_dist_D + M25$f_per_group_pr_D[,4,3])+   # Dist (A): COL More Freq. Users
    (M25$b_dist_D + M25$f_per_group_pr_D[,6,3])+   # Dist (A): CZE More Freq. Users
    (M25$b_dist_D + M25$f_per_group_pr_D[,8,3])+   # Dist (A): ZAF More Freq. Users
    (M25$b_dist_D + M25$f_per_group_pr_D[,10,3])+   # Dist (A): TUR More Freq. Users
    (M25$b_dist_D + M25$f_per_group_pr_D[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
DOM_Asym_F_freq <- (
  (M25$b_FA_D + M25$f_per_group_pr_D[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (M25$b_FA_D + M25$f_per_group_pr_D[,3,4])+   # Asym (A): COL More Freq. Users
    (M25$b_FA_D + M25$f_per_group_pr_D[,5,4])+   # Asym (A): CZE More Freq. Users
    (M25$b_FA_D + M25$f_per_group_pr_D[,7,4])+   # Asym (A): ZAF More Freq. Users
    (M25$b_FA_D + M25$f_per_group_pr_D[,9,4])+   # Asym (A): TUR More Freq. Users
    (M25$b_FA_D + M25$f_per_group_pr_D[,11,4])   # Asym (A): COL More Freq. Users
)/6

DOM_Asym_F_INfreq <- (
  (M25$b_FA_D + M25$f_per_group_pr_D[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (M25$b_FA_D + M25$f_per_group_pr_D[,4,4])+   # Asym (A): COL More Freq. Users
    (M25$b_FA_D + M25$f_per_group_pr_D[,6,4])+   # Asym (A): CZE More Freq. Users
    (M25$b_FA_D + M25$f_per_group_pr_D[,8,4])+   # Asym (A): ZAF More Freq. Users
    (M25$b_FA_D + M25$f_per_group_pr_D[,10,4])+   # Asym (A): TUR More Freq. Users
    (M25$b_FA_D + M25$f_per_group_pr_D[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
DOM_SexTyp_F_freq <- (
  (M25$b_sshd_D + M25$f_per_group_pr_D[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (M25$b_sshd_D + M25$f_per_group_pr_D[,3,5])+   # SShD (A): COL More Freq. Users
    (M25$b_sshd_D + M25$f_per_group_pr_D[,5,5])+   # SShD (A): CZE More Freq. Users
    (M25$b_sshd_D + M25$f_per_group_pr_D[,7,5])+   # SShD (A): ZAF More Freq. Users
    (M25$b_sshd_D + M25$f_per_group_pr_D[,9,5])+   # SShD (A): TUR More Freq. Users
    (M25$b_sshd_D + M25$f_per_group_pr_D[,11,5])   # SShD (A): COL More Freq. Users
)/6

DOM_SexTyp_F_INfreq <- (
  (M25$b_sshd_D + M25$f_per_group_pr_D[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (M25$b_sshd_D + M25$f_per_group_pr_D[,4,5])+   # SShD (A): COL More Freq. Users
    (M25$b_sshd_D + M25$f_per_group_pr_D[,6,5])+   # SShD (A): CZE More Freq. Users
    (M25$b_sshd_D + M25$f_per_group_pr_D[,8,5])+   # SShD (A): ZAF More Freq. Users
    (M25$b_sshd_D + M25$f_per_group_pr_D[,10,5])+   # SShD (A): TUR More Freq. Users
    (M25$b_sshd_D + M25$f_per_group_pr_D[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
DOM_L_F_freq <- (
  (M25$b_L_D + M25$f_per_group_pr_D[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (M25$b_L_D + M25$f_per_group_pr_D[,3,6])+   # L (A): COL More Freq. Users
    (M25$b_L_D + M25$f_per_group_pr_D[,5,6])+   # L (A): CZE More Freq. Users
    (M25$b_L_D + M25$f_per_group_pr_D[,7,6])+   # L (A): ZAF More Freq. Users
    (M25$b_L_D + M25$f_per_group_pr_D[,9,6])+   # L (A): TUR More Freq. Users
    (M25$b_L_D + M25$f_per_group_pr_D[,11,6])   # L (A): COL More Freq. Users
)/6

DOM_L_F_INfreq <- (
  (M25$b_L_D + M25$f_per_group_pr_D[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (M25$b_L_D + M25$f_per_group_pr_D[,4,6])+   # L (A): COL More Freq. Users
    (M25$b_L_D + M25$f_per_group_pr_D[,6,6])+   # L (A): CZE More Freq. Users
    (M25$b_L_D + M25$f_per_group_pr_D[,8,6])+   # L (A): ZAF More Freq. Users
    (M25$b_L_D + M25$f_per_group_pr_D[,10,6])+   # L (A): TUR More Freq. Users
    (M25$b_L_D + M25$f_per_group_pr_D[,12,6])   # L (A): COL More Freq. Users
)/6



# GMM - SEPARATELY 

# Dominance - GMM 
str(M25)
str(M25$f_per_group_pr_D) # Mind that 1 is intercept! 

F_posts_ATD_MM_DOM <- list(
  #  AGE
  Age_Slope = (M25$b_age_D),    # Slope prior to accessing culture
  
  DOM_Age_F_freq,
  DOM_Age_F_INfreq,
  
  Age_AUS_Freq = (M25$b_age_D + M25$f_per_group_pr_D[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (M25$b_age_D + M25$f_per_group_pr_D[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (M25$b_age_D + M25$f_per_group_pr_D[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (M25$b_age_D + M25$f_per_group_pr_D[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (M25$b_age_D + M25$f_per_group_pr_D[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (M25$b_age_D + M25$f_per_group_pr_D[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (M25$b_age_D + M25$f_per_group_pr_D[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (M25$b_age_D + M25$f_per_group_pr_D[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (M25$b_age_D + M25$f_per_group_pr_D[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (M25$b_age_D + M25$f_per_group_pr_D[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (M25$b_age_D + M25$f_per_group_pr_D[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (M25$b_age_D + M25$f_per_group_pr_D[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (M25$b_dist_D),    # Dist (A): CZE Control
  
  DOM_Dist_F_freq,
  DOM_Dist_F_INfreq,
  
  Dist_AUS_Freq = (M25$b_dist_D + M25$f_per_group_pr_D[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (M25$b_dist_D + M25$f_per_group_pr_D[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (M25$b_dist_D + M25$f_per_group_pr_D[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (M25$b_dist_D + M25$f_per_group_pr_D[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (M25$b_dist_D + M25$f_per_group_pr_D[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (M25$b_dist_D + M25$f_per_group_pr_D[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (M25$b_dist_D + M25$f_per_group_pr_D[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (M25$b_dist_D + M25$f_per_group_pr_D[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (M25$b_dist_D + M25$f_per_group_pr_D[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (M25$b_dist_D + M25$f_per_group_pr_D[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (M25$b_dist_D + M25$f_per_group_pr_D[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (M25$b_dist_D + M25$f_per_group_pr_D[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (M25$b_FA_D),    # Asym (A): CZE Control
  
  DOM_Asym_F_freq,
  DOM_Asym_F_INfreq,
  
  Asym_AUS_Freq = (M25$b_FA_D + M25$f_per_group_pr_D[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (M25$b_FA_D + M25$f_per_group_pr_D[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (M25$b_FA_D + M25$f_per_group_pr_D[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (M25$b_FA_D + M25$f_per_group_pr_D[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (M25$b_FA_D + M25$f_per_group_pr_D[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (M25$b_FA_D + M25$f_per_group_pr_D[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (M25$b_FA_D + M25$f_per_group_pr_D[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (M25$b_FA_D + M25$f_per_group_pr_D[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (M25$b_FA_D + M25$f_per_group_pr_D[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (M25$b_FA_D + M25$f_per_group_pr_D[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (M25$b_FA_D + M25$f_per_group_pr_D[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (M25$b_FA_D + M25$f_per_group_pr_D[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (M25$b_sshd_D),    # SexTyp (A): CZE Control
  
  DOM_SexTyp_F_freq,
  DOM_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (M25$b_sshd_D + M25$f_per_group_pr_D[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (M25$b_sshd_D + M25$f_per_group_pr_D[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (M25$b_sshd_D + M25$f_per_group_pr_D[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (M25$b_sshd_D + M25$f_per_group_pr_D[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (M25$b_sshd_D + M25$f_per_group_pr_D[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (M25$b_sshd_D + M25$f_per_group_pr_D[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (M25$b_sshd_D + M25$f_per_group_pr_D[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (M25$b_sshd_D + M25$f_per_group_pr_D[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (M25$b_sshd_D + M25$f_per_group_pr_D[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (M25$b_sshd_D + M25$f_per_group_pr_D[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (M25$b_sshd_D + M25$f_per_group_pr_D[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (M25$b_sshd_D + M25$f_per_group_pr_D[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (M25$b_L_D),    # L (A): CZE Control
  
  DOM_L_F_freq,
  DOM_L_F_INfreq,
  
  L_AUS_Freq = (M25$b_L_D + M25$f_per_group_pr_D[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (M25$b_L_D + M25$f_per_group_pr_D[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (M25$b_L_D + M25$f_per_group_pr_D[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (M25$b_L_D + M25$f_per_group_pr_D[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (M25$b_L_D + M25$f_per_group_pr_D[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (M25$b_L_D + M25$f_per_group_pr_D[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (M25$b_L_D + M25$f_per_group_pr_D[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (M25$b_L_D + M25$f_per_group_pr_D[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (M25$b_L_D + M25$f_per_group_pr_D[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (M25$b_L_D + M25$f_per_group_pr_D[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (M25$b_L_D + M25$f_per_group_pr_D[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (M25$b_L_D + M25$f_per_group_pr_D[,12,6])   # L (A): COL Less Freq. Users
)


summary.data.frame(F_posts_ATD_MM_DOM)
str(F_posts_ATD_MM_TRU) # Need 65 rows 



par(mar=c(2.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.2),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Dominance: GMM - Males", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting


# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#3d465490", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#3d465490", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#3d465490", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#3d465490", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#3d465490", lwd = 2)


# Small squares...
rect(-0.24,12.5,0.19,15.25, col="#F0F0F0", border = F)
rect(-0.24,28,0.19,30.75, col="#F0F0F0", border = F)
rect(-0.24,43.5,0.19,46.25, col="#F0F0F0", border = F)
rect(-0.24,59,0.19,61.75, col="#F0F0F0", border = F)
rect(-0.24,74.5,0.19,77.25, col="#F0F0F0", border = F)

# 
segments(x0 = -0.25, y0 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), x1 = 0.2, 
         y1 = c(1:15,16.5:30.5,32:46,47.5:61.5,63:77), col = "#40808080", lwd = 1.5)

# 1
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 0, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 15.5,
         col="#80808080",lty=2)

# 2
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 16, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 31,
         col="#80808080",lty=2)

# 3
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 31.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 46.5,
         col="#80808080",lty=2)

# 4 
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 47, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 62,
         col="#80808080",lty=2)

# 5
segments(x0 = c(-0.2,-0.1,0.0,0.1), y0 = 62.5, x1 = c(-0.2,-0.1,0.0,0.1), y1 = 77.5,
         col="#80808080",lty=2)



ys<-c(1:15,16.5:30.5,32:46,47.5:61.5,63:77)
ys<-rev(ys)

labs <- rep(labs,5)

cex_t <- 1.25
cex_t2 <- 1
pls <- 1

for(i in 1:length(F_posts_ATD_MM_DOM)){ 
  toplot<-F_posts_ATD_MM_DOM[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=5, lty=1, col=cols[i])
  segments(x0 = ci[1], y0 = ycoord-0.1, x1 = ci[1], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  segments(x0 = ci[2], y0 = ycoord-0.1, x1 = ci[2], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0=0.0, y0=c(0.0, 16, 31.5, 47, 62.5), x1=0.0, y1=c(15.5, 31, 46.5, 62, 77.5), col="#999999", lwd=2, lty=1)
# 


dev.off()


# Correlation comparisons: 

#-----
# Correlations fundamentally - SES: 
# Correlations fundamentally - SES: 
# Correlations fundamentally - SES: 
# Correlations fundamentally - SES: 
# Correlations fundamentally - SES: 
# Correlations fundamentally - SES: 
# Correlations fundamentally - SES: 
# Correlations fundamentally - SES: 
#-----

# Generate a random correlation matrix
# set.seed(42)
# n <- 12
# A <- matrix(runif(n^2, -1, 1), n, n)
# corr_mat <- (A + t(A)) / 2  # Make it symmetric
# diag(corr_mat) <- 1  # Set diagonal to 1

# Define group labels
# countries <- c("A", "B", "C", "D", "E", "F")
# usage <- c("h", "l")
# groups <- as.vector(t(outer(countries, usage, paste0)))
# rownames(corr_mat) <- colnames(corr_mat) <- groups

# Extract correlations based on color groups
all<-t(combn(1:12, 2)) #all possible combinations7
odd<-all %% 2 #is the umber odd?
both.same<-odd[,1]==odd[,2] #are both the same? both odd or even?
nodd<-rowSums(odd) #is the number of odd numbers odd? 

# Define groups by their coordinates
within<-matrix(c(1,2,3,4,5,6,7,8,9,10,11,12),ncol=2,byrow=T) #within country, red
between<-all[both.same==F & !(apply(all,1,function(row) paste(row, collapse = " ")) 
                              %in% apply(within,1,function(row) paste(row, collapse = " "))),] #between countries, but one group high usage, one group low usage, green

high<-all[both.same==T & nodd==2,] #high usage, yellow
low<-all[both.same==T & nodd==0,] #low usage, blue

#Sanity check, each used once
nrow(all)==sum(c(nrow(within),nrow(between),nrow(high),nrow(low)))

# Females - attractiveness

# Number of posterior samples
n_samples <- dim(F25$Rho_FSMUi_A)[1]


# BETWEEN! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(between), function(i) {
  F25$Rho_FSMUi_A[, between[i,1], between[i,2]]
})

# Convert to matrix to preserve structure
between_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(between))

# Is that correct? 
cor(F25$Rho_FSMUi_A[1:100,1,4],between_mat[1:100,1])
cor(F25$Rho_FSMUi_A[1:100,4,7],between_mat[1:100,16])
cor(F25$Rho_FSMUi_A[1:100,9,12],between_mat[1:100,29])
# Yes, Petr-Chat Rulez...

# WITHIN
selected_correlations <- sapply(1:nrow(within), function(i) {
  F25$Rho_FSMUi_A[, within[i,1], within[i,2]]
})

within_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(within))

cor(F25$Rho_FSMUi_A[1:100,1,2],within_mat[1:100,1])
cor(F25$Rho_FSMUi_A[1:100,3,4],within_mat[1:100,2])
cor(F25$Rho_FSMUi_A[1:100,9,10],within_mat[1:100,5])

# 1 both posteriors as vector - subset randomly from the longer... 
mean(as.vector(within_mat) - sample(as.vector(between_mat),4900))

# 2 within take as the whole, from between - sample 6 columns 
mean(as.vector(within_mat) - as.vector(between_mat[,sample(1:30,6,replace=F)]))

# Repeat 2 100-times:
(mean(as.vector(within_mat) - as.vector(between_mat[,sample(1:30,6,replace=F)])))
A <- replicate(1000,(mean(as.vector(within_mat) - as.vector(between_mat[,sample(1:30,6,replace=F)]))))
mean(A)

# We want this: 
# 3 create rowmeans and then compare thee
mean(rowMeans(within_mat)-rowMeans(between_mat))
PI(rowMeans(within_mat)-rowMeans(between_mat),prob=0.95)

mean(rowMeans(within_mat)); PI(rowMeans(within_mat), prob=0.95)
mean(rowMeans(between_mat)); PI(rowMeans(between_mat), prob=0.95) 



#Question 2:
# HIGH! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(high), function(i) {
  F25$Rho_FSMUi_A[, high[i,1], high[i,2]]
})

# Convert to matrix to preserve structure
high_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(high))

# Is that correct? 
cor(F25$Rho_FSMUi_A[1:100,1,3],high_mat[1:100,1])
cor(F25$Rho_FSMUi_A[1:100,3,7],high_mat[1:100,7])
cor(F25$Rho_FSMUi_A[1:100,9,11],high_mat[1:100,15])
# Yes, Petr-Chat Rulez...

# LOW
selected_correlations <- sapply(1:nrow(low), function(i) {
  F25$Rho_FSMUi_A[, low[i,1], low[i,2]]
})

low_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(low))

cor(F25$Rho_FSMUi_A[1:100,2,4],low_mat[1:100,1])
cor(F25$Rho_FSMUi_A[1:100,4,12],low_mat[1:100,9])
cor(F25$Rho_FSMUi_A[1:100,10,12],low_mat[1:100,15])

#difference of the averages

# 1 both posteriors as vector - subset randomly from the longer... 
mean(as.vector(high_mat) - as.vector(low_mat))

# 2 create rowmeans and then compare thee
mean(rowMeans(high_mat)-rowMeans(low_mat))

PI(rowMeans(high_mat)-rowMeans(low_mat),0.95)

mean(rowMeans(high_mat)); PI(rowMeans(high_mat), prob=0.95)
mean(rowMeans(low_mat)); PI(rowMeans(low_mat), prob=0.95) 



# Trustworthiness: 
# BETWEEN! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(between), function(i) {
  F25$Rho_FSMUi_T[, between[i,1], between[i,2]]
})

# Convert to matrix to preserve structure
between_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(between))

# Is that correct? 
cor(F25$Rho_FSMUi_T[1:100,1,4],between_mat[1:100,1])
cor(F25$Rho_FSMUi_T[1:100,4,7],between_mat[1:100,16])
cor(F25$Rho_FSMUi_T[1:100,9,12],between_mat[1:100,29])

# WITHIN
selected_correlations <- sapply(1:nrow(within), function(i) {
  F25$Rho_FSMUi_T[, within[i,1], within[i,2]]
})

within_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(within))

cor(F25$Rho_FSMUi_T[1:100,1,2],within_mat[1:100,1])
cor(F25$Rho_FSMUi_T[1:100,3,4],within_mat[1:100,2])
cor(F25$Rho_FSMUi_T[1:100,9,10],within_mat[1:100,5])

# 1 both posteriors as vector - subset randomly from the longer... 
mean(as.vector(within_mat) - sample(as.vector(between_mat),4900))

# 2 within take as the whole, from between - sample 6 columns 
mean(as.vector(within_mat) - as.vector(between_mat[,sample(1:30,6,replace=F)]))

# Repeat 2 100-times:
(mean(as.vector(within_mat) - as.vector(between_mat[,sample(1:30,6,replace=F)])))
A <- replicate(1000,(mean(as.vector(within_mat) - as.vector(between_mat[,sample(1:30,6,replace=F)]))))
mean(A)

# We want this: 
# 3 create rowmeans and then compare thee
mean(rowMeans(within_mat)-rowMeans(between_mat))
PI(rowMeans(within_mat)-rowMeans(between_mat),prob=0.95)

mean(rowMeans(within_mat)); PI(rowMeans(within_mat), prob=0.95)
mean(rowMeans(between_mat)); PI(rowMeans(between_mat), prob=0.95) 



#Question 2: 
# HIGH! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(high), function(i) {
  F25$Rho_FSMUi_T[, high[i,1], high[i,2]]
})

# Convert to matrix to preserve structure
high_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(high))

# Is that correct? 
cor(F25$Rho_FSMUi_T[1:100,1,3],high_mat[1:100,1])
cor(F25$Rho_FSMUi_T[1:100,3,7],high_mat[1:100,7])
cor(F25$Rho_FSMUi_T[1:100,9,11],high_mat[1:100,15])

# LOW
selected_correlations <- sapply(1:nrow(low), function(i) {
  F25$Rho_FSMUi_T[, low[i,1], low[i,2]]
})

low_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(low))

cor(F25$Rho_FSMUi_T[1:100,2,4],low_mat[1:100,1])
cor(F25$Rho_FSMUi_T[1:100,4,12],low_mat[1:100,9])
cor(F25$Rho_FSMUi_T[1:100,10,12],low_mat[1:100,15])

#difference of the averages

# 1 both posteriors as vector - subset randomly from the longer... 
mean(as.vector(high_mat) - as.vector(low_mat))

# 2 create rowmeans and then compare thee
mean(rowMeans(high_mat)-rowMeans(low_mat))

PI(rowMeans(high_mat)-rowMeans(low_mat),0.95)

mean(rowMeans(high_mat)); PI(rowMeans(high_mat), prob=0.95)
mean(rowMeans(low_mat)); PI(rowMeans(low_mat), prob=0.95) 



# Dominance: 
# BETWEEN! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(between), function(i) {
  F25$Rho_FSMUi_D[, between[i,1], between[i,2]]
})

# Convert to matrix to preserve structure
between_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(between))

# Is that correct? 
cor(F25$Rho_FSMUi_D[1:100,1,4],between_mat[1:100,1])
cor(F25$Rho_FSMUi_D[1:100,4,7],between_mat[1:100,16])
cor(F25$Rho_FSMUi_D[1:100,9,12],between_mat[1:100,29])

# WITHIN
selected_correlations <- sapply(1:nrow(within), function(i) {
  F25$Rho_FSMUi_D[, within[i,1], within[i,2]]
})

within_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(within))

cor(F25$Rho_FSMUi_D[1:100,1,2],within_mat[1:100,1])
cor(F25$Rho_FSMUi_D[1:100,3,4],within_mat[1:100,2])
cor(F25$Rho_FSMUi_D[1:100,9,10],within_mat[1:100,5])

# 1 both posteriors as vector - subset randomly from the longer... 
mean(as.vector(within_mat) - sample(as.vector(between_mat),4900))

# 2 within take as the whole, from between - sample 6 columns 
mean(as.vector(within_mat) - as.vector(between_mat[,sample(1:30,6,replace=F)]))

# Repeat 2 100-times:
(mean(as.vector(within_mat) - as.vector(between_mat[,sample(1:30,6,replace=F)])))
A <- replicate(1000,(mean(as.vector(within_mat) - as.vector(between_mat[,sample(1:30,6,replace=F)]))))
mean(A)

# We want this: 
# 3 create rowmeans and then compare thee
mean(rowMeans(within_mat)-rowMeans(between_mat))
PI(rowMeans(within_mat)-rowMeans(between_mat),prob=0.95)

mean(rowMeans(within_mat)); PI(rowMeans(within_mat), prob=0.95)
mean(rowMeans(between_mat)); PI(rowMeans(between_mat), prob=0.95) 



#Question 2: 
# HIGH! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(high), function(i) {
  F25$Rho_FSMUi_T[, high[i,1], high[i,2]]
})

# Convert to matrix to preserve structure
high_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(high))

# Is that correct? 
cor(F25$Rho_FSMUi_D[1:100,1,3],high_mat[1:100,1])
cor(F25$Rho_FSMUi_D[1:100,3,7],high_mat[1:100,7])
cor(F25$Rho_FSMUi_D[1:100,9,11],high_mat[1:100,15])
# Yes, Petr-Chat Rulez...

# LOW
selected_correlations <- sapply(1:nrow(low), function(i) {
  F25$Rho_FSMUi_D[, low[i,1], low[i,2]]
})

low_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(low))

cor(F25$Rho_FSMUi_D[1:100,2,4],low_mat[1:100,1])
cor(F25$Rho_FSMUi_D[1:100,4,12],low_mat[1:100,9])
cor(F25$Rho_FSMUi_D[1:100,10,12],low_mat[1:100,15])

#difference of the averages

# 1 both posteriors as vector - subset randomly from the longer... 
mean(as.vector(high_mat) - as.vector(low_mat))

# 2 create rowmeans and then compare thee
mean(rowMeans(high_mat)-rowMeans(low_mat))

PI(rowMeans(high_mat)-rowMeans(low_mat),0.95)

mean(rowMeans(high_mat)); PI(rowMeans(high_mat), prob=0.95)
mean(rowMeans(low_mat)); PI(rowMeans(low_mat), prob=0.95) 



# Between scales: 
F_precis <- precis(M25, depth=3, prob=0.95)

F25$Rho
F_precis["Rho_Scales[1,2]",]
F_precis["Rho_Scales[1,3]",]
F_precis["Rho_Scales[2,3]",]




# Males 
# Males 
# Males 
# Males 
# Males 
# Males 
# Males 
# Males 


# Males - attractiveness

# Number of posterior samples
n_samples <- dim(M25$Rho_FSMUi_A)[1]


# BETWEEN! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(between), function(i) {
  M25$Rho_FSMUi_A[, between[i,1], between[i,2]]
})

# Convert to matrix to preserve structure
between_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(between))

# Is that correct? 
cor(M25$Rho_FSMUi_A[1:100,1,4],between_mat[1:100,1])
cor(M25$Rho_FSMUi_A[1:100,4,7],between_mat[1:100,16])
cor(M25$Rho_FSMUi_A[1:100,9,12],between_mat[1:100,29])
# Yes, Petr-Chat Rulez...

# WITHIN
selected_correlations <- sapply(1:nrow(within), function(i) {
  M25$Rho_FSMUi_A[, within[i,1], within[i,2]]
})

within_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(within))

cor(M25$Rho_FSMUi_A[1:100,1,2],within_mat[1:100,1])
cor(M25$Rho_FSMUi_A[1:100,3,4],within_mat[1:100,2])
cor(M25$Rho_FSMUi_A[1:100,9,10],within_mat[1:100,5])

# We want this: Create rowmeans and then compare thee
mean(rowMeans(within_mat)-rowMeans(between_mat))
PI(rowMeans(within_mat)-rowMeans(between_mat),prob=0.95)

mean(rowMeans(within_mat)); PI(rowMeans(within_mat), prob=0.95)
mean(rowMeans(between_mat)); PI(rowMeans(between_mat), prob=0.95) 



#Question 2: 
# HIGH! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(high), function(i) {
  M25$Rho_FSMUi_A[, high[i,1], high[i,2]]
})

# Convert to matrix to preserve structure
high_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(high))

# Is that correct? 
cor(M25$Rho_FSMUi_A[1:100,1,3],high_mat[1:100,1])
cor(M25$Rho_FSMUi_A[1:100,9,11],high_mat[1:100,15])

# LOW
selected_correlations <- sapply(1:nrow(low), function(i) {
  M25$Rho_FSMUi_A[, low[i,1], low[i,2]]
})

low_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(low))

cor(M25$Rho_FSMUi_A[1:100,2,4],low_mat[1:100,1])
cor(M25$Rho_FSMUi_A[1:100,4,12],low_mat[1:100,9])
cor(M25$Rho_FSMUi_A[1:100,10,12],low_mat[1:100,15])

#difference of the averages

# 1 both posteriors as vector - subset randomly from the longer... 
mean(as.vector(high_mat) - as.vector(low_mat))

# 2 create rowmeans and then compare thee
mean(rowMeans(high_mat)-rowMeans(low_mat))

PI(rowMeans(high_mat)-rowMeans(low_mat),0.95)

mean(rowMeans(high_mat)); PI(rowMeans(high_mat), prob=0.95)
mean(rowMeans(low_mat)); PI(rowMeans(low_mat), prob=0.95) 



# Trustworthiness: 
# BETWEEN! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(between), function(i) {
  M25$Rho_FSMUi_T[, between[i,1], between[i,2]]
})

# Convert to matrix to preserve structure
between_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(between))

# Is that correct? 
cor(M25$Rho_FSMUi_T[1:100,1,4],between_mat[1:100,1])
cor(M25$Rho_FSMUi_T[1:100,4,7],between_mat[1:100,16])
cor(M25$Rho_FSMUi_T[1:100,9,12],between_mat[1:100,29])
# Yes, Petr-Chat Rulez...

# WITHIN
selected_correlations <- sapply(1:nrow(within), function(i) {
  M25$Rho_FSMUi_T[, within[i,1], within[i,2]]
})

within_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(within))

cor(M25$Rho_FSMUi_T[1:100,1,2],within_mat[1:100,1])
cor(M25$Rho_FSMUi_T[1:100,3,4],within_mat[1:100,2])
cor(M25$Rho_FSMUi_T[1:100,9,10],within_mat[1:100,5])

# create rowmeans and then compare thee
mean(rowMeans(within_mat)-rowMeans(between_mat))
PI(rowMeans(within_mat)-rowMeans(between_mat),prob=0.95)


mean(rowMeans(within_mat)); PI(rowMeans(within_mat), prob=0.95)
mean(rowMeans(between_mat)); PI(rowMeans(between_mat), prob=0.95) 



#Question 2: 
# HIGH! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(high), function(i) {
  M25$Rho_FSMUi_T[, high[i,1], high[i,2]]
})

# Convert to matrix to preserve structure
high_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(high))

# Is that correct? 
cor(M25$Rho_FSMUi_T[1:100,1,3],high_mat[1:100,1])
cor(M25$Rho_FSMUi_T[1:100,3,7],high_mat[1:100,7])
cor(M25$Rho_FSMUi_T[1:100,9,11],high_mat[1:100,15])

# LOW
selected_correlations <- sapply(1:nrow(low), function(i) {
  M25$Rho_FSMUi_T[, low[i,1], low[i,2]]
})

low_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(low))

cor(M25$Rho_FSMUi_T[1:100,2,4],low_mat[1:100,1])
cor(M25$Rho_FSMUi_T[1:100,4,12],low_mat[1:100,9])
cor(M25$Rho_FSMUi_T[1:100,10,12],low_mat[1:100,15])

#difference of the averages

# 1 both posteriors as vector - subset randomly from the longer... 
mean(as.vector(high_mat) - as.vector(low_mat))

# 2 create rowmeans and then compare thee
mean(rowMeans(high_mat)-rowMeans(low_mat))

PI(rowMeans(high_mat)-rowMeans(low_mat),0.95)

mean(rowMeans(high_mat)); PI(rowMeans(high_mat), prob=0.95)
mean(rowMeans(low_mat)); PI(rowMeans(low_mat), prob=0.95) 



# Dominance: 
# BETWEEN! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(between), function(i) {
  M25$Rho_FSMUi_D[, between[i,1], between[i,2]]
})

# Convert to matrix to preserve structure
between_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(between))

# Is that correct? 
cor(M25$Rho_FSMUi_D[1:100,1,4],between_mat[1:100,1])
cor(M25$Rho_FSMUi_D[1:100,4,7],between_mat[1:100,16])
cor(M25$Rho_FSMUi_D[1:100,9,12],between_mat[1:100,29])


# WITHIN
selected_correlations <- sapply(1:nrow(within), function(i) {
  M25$Rho_FSMUi_D[, within[i,1], within[i,2]]
})

within_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(within))

cor(M25$Rho_FSMUi_D[1:100,1,2],within_mat[1:100,1])
cor(M25$Rho_FSMUi_D[1:100,3,4],within_mat[1:100,2])
cor(M25$Rho_FSMUi_D[1:100,9,10],within_mat[1:100,5])

# create rowmeans and then compare thee
mean(rowMeans(within_mat)-rowMeans(between_mat))
PI(rowMeans(within_mat)-rowMeans(between_mat),prob=0.95)

mean(rowMeans(within_mat)); PI(rowMeans(within_mat), prob=0.95)
mean(rowMeans(between_mat)); PI(rowMeans(between_mat), prob=0.95) 



#Question 2: 
# HIGH! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(high), function(i) {
  M25$Rho_FSMUi_D[, high[i,1], high[i,2]]
})

# Convert to matrix to preserve structure
high_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(high))

# Is that correct? 
cor(M25$Rho_FSMUi_D[1:100,1,3],high_mat[1:100,1])
cor(M25$Rho_FSMUi_D[1:100,3,7],high_mat[1:100,7])
cor(M25$Rho_FSMUi_D[1:100,9,11],high_mat[1:100,15])


# LOW
selected_correlations <- sapply(1:nrow(low), function(i) {
  M25$Rho_FSMUi_D[, low[i,1], low[i,2]]
})

low_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(low))

cor(M25$Rho_FSMUi_D[1:100,2,4],low_mat[1:100,1])
cor(M25$Rho_FSMUi_D[1:100,4,12],low_mat[1:100,9])
cor(M25$Rho_FSMUi_D[1:100,10,12],low_mat[1:100,15])

#difference of the averages

# 1 both posteriors as vector - subset randomly from the longer... 
mean(as.vector(high_mat) - as.vector(low_mat))

# 2 create rowmeans and then compare thee
mean(rowMeans(high_mat)-rowMeans(low_mat))

PI(rowMeans(high_mat)-rowMeans(low_mat),0.95)

mean(rowMeans(high_mat)); PI(rowMeans(high_mat), prob=0.95)
mean(rowMeans(low_mat)); PI(rowMeans(low_mat), prob=0.95) 



# Between scales: 
M_precis <- precis(M25, depth=3, prob=0.95)

M25$Rho
M_precis["Rho_Scales[1,2]",]
M_precis["Rho_Scales[1,3]",]
M_precis["Rho_Scales[2,3]",]

