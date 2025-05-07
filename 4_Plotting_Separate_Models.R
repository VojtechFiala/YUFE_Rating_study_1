
# Pro forma, we checked whether the single-scale models give similar predictions like the model with all three scales 
# included at once. 
# Log_lik = T is only possible when there is one reponse variable. Instead of calculating WAIC manually, we decided to 
# split the models and decide on their out of sample prediction accuracy separately.


library(rethinking)

# Load posteriors: 

# Females 

# - Attr

post_A_F <- readRDS("post_F_Atr_10_03_25_Long.RDS") # This is a model with A

str(post_A_F)

# - Trustw

post_T_F <- readRDS("post_F_Tru_10_03_25_Long.RDS") # This is a model with T 

str(post_T_F)

# - Dom 

post_D_F <- readRDS("post_F_Dom_10_03_25_Long.RDS") # This is a model with D 

str(post_D_F)


# Males 

# - Attr

post_A_M <- readRDS("post_M_Atr_10_03_25_Long.RDS") # This is a model with A 

str(post_A_M)

# - Trustw

post_T_M <- readRDS("post_M_Tru_10_03_25_Long.RDS") # This is a model with T 

str(post_T_M)

# - Dom 

post_D_M <- readRDS("post_M_Dom_10_03_25_Long.RDS") # This is a model with D  

str(post_D_M)


## Figure 1 analogue

# Females (1a)

# Correlations 

labels_x <- c("AUS/NZE Freq", "AUS/NZE Infreq", "COL Freq","COL Infreq","CZE Freq", "CZE Infreq","ZAF Freq", "ZAF Infreq",
              "TUR Freq", "TUR Infreq", "VNM Freq", "VNM Infreq")
labels_y <- labels_x

str(post_A_F$Rho_FSMUi_A)

library(qgraph)
library(bootnet)
library(beepr)
library(ComplexHeatmap)
library(circlize)
library(grid)


# Make it as a 3×1 Matrix
#  ATTRACTIVENESS:F

cor_table_mean <- apply(post_A_F$Rho_FSMUi_A,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(post_A_F$Rho_FSMUi_A,c(2,3),PI, prob=0.95)
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

cor_table_mean <- apply(post_T_F$Rho_FSMUi_T,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(post_T_F$Rho_FSMUi_T,c(2,3),PI, prob=0.95)
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

cor_table_mean <- apply(post_D_F$Rho_FSMUi_D,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(post_D_F$Rho_FSMUi_D,c(2,3),PI, prob=0.95)
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

tiff("Correlations_Heatmap_ONLY_A_Females_Attr_wide.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AF
dev.off()

tiff("Correlations_Heatmap_ONLY_T_Females_Tru_wide.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AT
dev.off()

tiff("Correlations_Heatmap_ONLY_D_Females_Dom_wide.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AD
dev.off()


# Males (1b)

# Correlations  

# Make it as a 3×1 Matrix
#  ATTRACTIVENESS:M

cor_table_mean <- apply(post_A_M$Rho_FSMUi_A,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(post_A_M$Rho_FSMUi_A,c(2,3),PI, prob=0.95)
cor_table_LOWCI <- cor_table_CI[1,,] 
cor_table_UPWCI <- cor_table_CI[2,,] 

font1 <- 22
font2 <- 28

# I want the coefficient to be printed above the axis, while the CIs should be below it: 
col_fun <- colorRamp2(c(0.3, 1.3/2, 0.9), c("#66e8ff","#70de66", "#ffec19"))
AF <- Heatmap((as.matrix(cor_table_mean)), column_title ="Attractiveness: males", cluster_rows=FALSE, 
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

cor_table_mean <- apply(post_T_M$Rho_FSMUi_T,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(post_T_M$Rho_FSMUi_T,c(2,3),PI, prob=0.95)
cor_table_LOWCI <- cor_table_CI[1,,] 
cor_table_UPWCI <- cor_table_CI[2,,] 


# I want the coefficient to be printed above the axis, while the CIs should be below it: 
col_fun <- colorRamp2(c(0.3, 1.3/2, 0.9), c("#66e8ff","#70de66", "#ffec19"))
AT <- Heatmap((as.matrix(cor_table_mean)), column_title ="Trustworthiness: males", cluster_rows=FALSE, 
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

cor_table_mean <- apply(post_D_M$Rho_FSMUi_D,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(post_D_M$Rho_FSMUi_D,c(2,3),PI, prob=0.95)
cor_table_LOWCI <- cor_table_CI[1,,] 
cor_table_UPWCI <- cor_table_CI[2,,] 


# I want the coefficient to be printed above the axis, while the CIs should be below it: 
col_fun <- colorRamp2(c(0.3, 1.3/2, 0.9), c("#66e8ff","#70de66", "#ffec19"))
AD <- Heatmap((as.matrix(cor_table_mean)), column_title ="Dominance: males", cluster_rows=FALSE, 
              cluster_columns = FALSE, column_title_gp = gpar(fontsize=font1), column_names_gp = gpar(fontsize = font1), 
              row_names_gp = gpar(fontsize = font1), column_names_rot = 45, column_names_side = "top", name = "r", 
              col = col_fun,
              cell_fun = function(j, i, x, y, width, height, fill) {
                if (j>i) {c(grid.text(sprintf("%.3f", cor_table_mean[i, j]), x, y, gp = gpar(fontsize = font2),just=c(0.50,0.50)))}
                else if (j<i){c(grid.text(sprintf("%.3f", cor_table_LOWCI[i, j]), x, y, gp = gpar(fontsize = font1),just = c(0.95,-0.33)),
                                grid.text(sprintf("%.3f", cor_table_UPWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.05,0.92))
                )}
              },rect_gp = gpar(col = "white", lwd = 1), row_title = "95% Compatibility Interval", row_title_gp = gpar(fontsize = 22))

tiff("Correlations_Heatmap_ONLY_A_Males_Attr_wide.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AF
dev.off()

tiff("Correlations_Heatmap_ONLY_T_Males_Tru_wide.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AT
dev.off()

tiff("Correlations_Heatmap_ONLY_D_Males_Dom_wide.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AD
dev.off()


# Figure S3 - analogue of the Figure 3 in the main body of the article, which is a figure (top) presenting mean estimates
# in every sample, (bottom) presenting effects of morphometric and demographic predictors: SPECIFICALLY for WOMEN
# Version without labels

# Upper part: Frequent - all
# Compute the average of all group estimates

# Note - these are the same data as those to enter the A-T-D model; therefore, 
# the order of levels is the same, too. 

Avg_Freq_U_A = ( 
  post_A_F$aA + post_A_F$f_per_group_pr_A[,1,1] + 
    post_A_F$aA + post_A_F$f_per_group_pr_A[,3,1] + 
    post_A_F$aA + post_A_F$f_per_group_pr_A[,5,1] + 
    post_A_F$aA + post_A_F$f_per_group_pr_A[,7,1] + 
    post_A_F$aA + post_A_F$f_per_group_pr_A[,9,1] + 
    post_A_F$aA + post_A_F$f_per_group_pr_A[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_A = ( 
  post_A_F$aA + post_A_F$f_per_group_pr_A[,2,1] + 
    post_A_F$aA + post_A_F$f_per_group_pr_A[,4,1] + 
    post_A_F$aA + post_A_F$f_per_group_pr_A[,6,1] + 
    post_A_F$aA + post_A_F$f_per_group_pr_A[,8,1] + 
    post_A_F$aA + post_A_F$f_per_group_pr_A[,10,1] + 
    post_A_F$aA + post_A_F$f_per_group_pr_A[,12,1] 
) / 6



F_posts_ATR <- list(
  # 1 Intercept: 
  Intercept = post_A_F$aA,
  # Infrequent users
  Avg_Freq_U_A,
  # Frequent users 
  Avg_Infreq_U_A,
  # Difference 
  Difference = Avg_Freq_U_A - Avg_Infreq_U_A,
  # 4 Australia - above - median users  
  AUS_Intensive_User = post_A_F$aA + post_A_F$f_per_group_pr_A[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = post_A_F$aA + post_A_F$f_per_group_pr_A[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = post_A_F$aA + post_A_F$f_per_group_pr_A[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = post_A_F$aA + post_A_F$f_per_group_pr_A[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = post_A_F$aA + post_A_F$f_per_group_pr_A[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = post_A_F$aA + post_A_F$f_per_group_pr_A[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = post_A_F$aA + post_A_F$f_per_group_pr_A[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = post_A_F$aA + post_A_F$f_per_group_pr_A[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = post_A_F$aA + post_A_F$f_per_group_pr_A[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = post_A_F$aA + post_A_F$f_per_group_pr_A[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = post_A_F$aA + post_A_F$f_per_group_pr_A[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = post_A_F$aA + post_A_F$f_per_group_pr_A[,12,1]
)

summary.data.frame(F_posts_ATR)
str(F_posts_ATR) # Need 13 rows 

#--------------

# Trustworthiness
Avg_Freq_U_T = ( 
  post_T_F$aT + post_T_F$f_per_group_pr_T[,1,1] + 
    post_T_F$aT + post_T_F$f_per_group_pr_T[,3,1] + 
    post_T_F$aT + post_T_F$f_per_group_pr_T[,5,1] + 
    post_T_F$aT + post_T_F$f_per_group_pr_T[,7,1] + 
    post_T_F$aT + post_T_F$f_per_group_pr_T[,9,1] + 
    post_T_F$aT + post_T_F$f_per_group_pr_T[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_T = ( 
  post_T_F$aT + post_T_F$f_per_group_pr_T[,2,1] + 
    post_T_F$aT + post_T_F$f_per_group_pr_T[,4,1] + 
    post_T_F$aT + post_T_F$f_per_group_pr_T[,6,1] + 
    post_T_F$aT + post_T_F$f_per_group_pr_T[,8,1] + 
    post_T_F$aT + post_T_F$f_per_group_pr_T[,10,1] + 
    post_T_F$aT + post_T_F$f_per_group_pr_T[,12,1] 
) / 6

F_posts_TRU <- list(
  # 1 Intercept: 
  Intercept = post_T_F$aT,
  # Infrequent users
  Avg_Freq_U_T,
  # Frequent users 
  Avg_Infreq_U_T,
  # Difference 
  Difference = Avg_Freq_U_T - Avg_Infreq_U_T,
  # 4 Australia - above - median users  
  AUS_Intensive_User = post_T_F$aT + post_T_F$f_per_group_pr_T[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = post_T_F$aT + post_T_F$f_per_group_pr_T[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = post_T_F$aT + post_T_F$f_per_group_pr_T[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = post_T_F$aT + post_T_F$f_per_group_pr_T[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = post_T_F$aT + post_T_F$f_per_group_pr_T[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = post_T_F$aT + post_T_F$f_per_group_pr_T[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = post_T_F$aT + post_T_F$f_per_group_pr_T[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = post_T_F$aT + post_T_F$f_per_group_pr_T[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = post_T_F$aT + post_T_F$f_per_group_pr_T[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = post_T_F$aT + post_T_F$f_per_group_pr_T[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = post_T_F$aT + post_T_F$f_per_group_pr_T[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = post_T_F$aT + post_T_F$f_per_group_pr_T[,12,1]
)

summary.data.frame(F_posts_TRU)
str(F_posts_TRU) # Need 13 rows 

#--------------

# Dominance
Avg_Freq_U_D = ( 
  post_D_F$aD + post_D_F$f_per_group_pr_D[,1,1] + 
    post_D_F$aD + post_D_F$f_per_group_pr_D[,3,1] + 
    post_D_F$aD + post_D_F$f_per_group_pr_D[,5,1] + 
    post_D_F$aD + post_D_F$f_per_group_pr_D[,7,1] + 
    post_D_F$aD + post_D_F$f_per_group_pr_D[,9,1] + 
    post_D_F$aD + post_D_F$f_per_group_pr_D[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_D = ( 
  post_D_F$aD + post_D_F$f_per_group_pr_D[,2,1] + 
    post_D_F$aD + post_D_F$f_per_group_pr_D[,4,1] + 
    post_D_F$aD + post_D_F$f_per_group_pr_D[,6,1] + 
    post_D_F$aD + post_D_F$f_per_group_pr_D[,8,1] + 
    post_D_F$aD + post_D_F$f_per_group_pr_D[,10,1] + 
    post_D_F$aD + post_D_F$f_per_group_pr_D[,12,1] 
) / 6

F_posts_DOM <- list(
  # 1 Intercept: 
  Intercept = post_D_F$aD,
  # Infrequent users
  Avg_Freq_U_D,
  # Frequent users 
  Avg_Infreq_U_D,
  # Difference 
  Difference = Avg_Freq_U_D - Avg_Infreq_U_D,
  # 4 Australia - above - median users  
  AUS_Intensive_User = post_D_F$aD + post_D_F$f_per_group_pr_D[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = post_D_F$aD + post_D_F$f_per_group_pr_D[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = post_D_F$aD + post_D_F$f_per_group_pr_D[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = post_D_F$aD + post_D_F$f_per_group_pr_D[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = post_D_F$aD + post_D_F$f_per_group_pr_D[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = post_D_F$aD + post_D_F$f_per_group_pr_D[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = post_D_F$aD + post_D_F$f_per_group_pr_D[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = post_D_F$aD + post_D_F$f_per_group_pr_D[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = post_D_F$aD + post_D_F$f_per_group_pr_D[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = post_D_F$aD + post_D_F$f_per_group_pr_D[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = post_D_F$aD + post_D_F$f_per_group_pr_D[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = post_D_F$aD + post_D_F$f_per_group_pr_D[,12,1]
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
        "More freq.U",
        "Less freq.U",
        "Freq - Less.freq: Difference",
        "Australia/NewZ - More freq.U",
        "Australia/NewZ - Less freq.U",
        "Colombia - More freq.U",
        "Colombia - Less freq.U",
        "Czechia - More freq.U",
        "Czechia - Less freq.U",
        "South Africa - More freq.U",
        "South Africa - Less freq.U",
        "Turkey - More freq.U",
        "Turkey - Less freq.U",
        "Vietnam - More freq.U",
        "Vietnam - Less freq.U"
)


# Figure S3:Upper Panel - Analogue of Figure 3:Upper Panel (final assembly done in InkScape)...

tiff("F_ATD_separate_models_LEVELS_NOLABELS.tiff",width=30,height=14,units="cm",res=600,compression="lzw")
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
  (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,3,2])+   # Age (A): COL More Freq. Users
    (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,5,2])+   # Age (A): CZE More Freq. Users
    (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,7,2])+   # Age (A): ZAF More Freq. Users
    (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,9,2])+   # Age (A): TUR More Freq. Users
    (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,11,2])   # Age (A): COL More Freq. Users
)/6

Attr_Age_F_INfreq <- (
  (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,4,2])+   # Age (A): COL More Freq. Users
    (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,6,2])+   # Age (A): CZE More Freq. Users
    (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,8,2])+   # Age (A): ZAF More Freq. Users
    (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,10,2])+   # Age (A): TUR More Freq. Users
    (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
Attr_Dist_F_freq <- (
  (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,3,3])+   # Dist (A): COL More Freq. Users
    (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,5,3])+   # Dist (A): CZE More Freq. Users
    (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,7,3])+   # Dist (A): ZAF More Freq. Users
    (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,9,3])+   # Dist (A): TUR More Freq. Users
    (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,11,3])   # Dist (A): COL More Freq. Users
)/6

Attr_Dist_F_INfreq <- (
  (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,4,3])+   # Dist (A): COL More Freq. Users
    (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,6,3])+   # Dist (A): CZE More Freq. Users
    (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,8,3])+   # Dist (A): ZAF More Freq. Users
    (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,10,3])+   # Dist (A): TUR More Freq. Users
    (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
Attr_Asym_F_freq <- (
  (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,3,4])+   # Asym (A): COL More Freq. Users
    (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,5,4])+   # Asym (A): CZE More Freq. Users
    (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,7,4])+   # Asym (A): ZAF More Freq. Users
    (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,9,4])+   # Asym (A): TUR More Freq. Users
    (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,11,4])   # Asym (A): COL More Freq. Users
)/6

Attr_Asym_F_INfreq <- (
  (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,4,4])+   # Asym (A): COL More Freq. Users
    (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,6,4])+   # Asym (A): CZE More Freq. Users
    (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,8,4])+   # Asym (A): ZAF More Freq. Users
    (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,10,4])+   # Asym (A): TUR More Freq. Users
    (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
Attr_SexTyp_F_freq <- (
  (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,3,5])+   # SShD (A): COL More Freq. Users
    (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,5,5])+   # SShD (A): CZE More Freq. Users
    (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,7,5])+   # SShD (A): ZAF More Freq. Users
    (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,9,5])+   # SShD (A): TUR More Freq. Users
    (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,11,5])   # SShD (A): COL More Freq. Users
)/6

Attr_SexTyp_F_INfreq <- (
  (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,4,5])+   # SShD (A): COL More Freq. Users
    (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,6,5])+   # SShD (A): CZE More Freq. Users
    (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,8,5])+   # SShD (A): ZAF More Freq. Users
    (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,10,5])+   # SShD (A): TUR More Freq. Users
    (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
Attr_L_F_freq <- (
  (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,3,6])+   # L (A): COL More Freq. Users
    (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,5,6])+   # L (A): CZE More Freq. Users
    (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,7,6])+   # L (A): ZAF More Freq. Users
    (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,9,6])+   # L (A): TUR More Freq. Users
    (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,11,6])   # L (A): COL More Freq. Users
)/6

Attr_L_F_INfreq <- (
  (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,4,6])+   # L (A): COL More Freq. Users
    (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,6,6])+   # L (A): CZE More Freq. Users
    (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,8,6])+   # L (A): ZAF More Freq. Users
    (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,10,6])+   # L (A): TUR More Freq. Users
    (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,12,6])   # L (A): COL More Freq. Users
)/6



# GMM - SEPARATELY 

# Attractiveness - GMM 
str(post_A_F)
str(post_A_F$f_per_group_pr_A) # Mind that 1 is intercept! 

F_posts_ATD_MM_Attr <- list(
  #  AGE
  Age_Slope = (post_A_F$b_age_A),    # Slope prior to accessing culture
  
  Attr_Age_F_freq,
  Attr_Age_F_INfreq,
  
  Age_AUS_Freq = (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (post_A_F$b_age_A + post_A_F$f_per_group_pr_A[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (post_A_F$b_dist_A),    # Dist (A): CZE Control
  
  Attr_Dist_F_freq,
  Attr_Dist_F_INfreq,
  
  Dist_AUS_Freq = (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (post_A_F$b_dist_A + post_A_F$f_per_group_pr_A[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (post_A_F$b_FA_A),    # Asym (A): CZE Control
  
  Attr_Asym_F_freq,
  Attr_Asym_F_INfreq,
  
  Asym_AUS_Freq = (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (post_A_F$b_FA_A + post_A_F$f_per_group_pr_A[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (post_A_F$b_sshd_A),    # SexTyp (A): CZE Control
  
  Attr_SexTyp_F_freq,
  Attr_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (post_A_F$b_sshd_A + post_A_F$f_per_group_pr_A[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (post_A_F$b_L_A),    # L (A): CZE Control
  
  Attr_L_F_freq,
  Attr_L_F_INfreq,
  
  L_AUS_Freq = (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (post_A_F$b_L_A + post_A_F$f_per_group_pr_A[,12,6])   # L (A): COL Less Freq. Users
)


summary.data.frame(F_posts_ATD_MM_Attr)
str(F_posts_ATD_MM_Attr) # Need 65 rows 


labs <- c("Fixed Slope",
          
          "Frequent (All)",
          "Infrequent (All)",
          
          "AUS/NZE Frequent",
          "AUS/NZE Infrequent",
          
          "COL Frequent",
          "COL Infrequent",
          
          "CZE Frequent",
          "CZE Infreqeunt",
          
          "ZAF Frequent",
          "ZAF Infrequent",
          
          "TUR Frequent",
          "TUR Infrequent",
          
          "VNM Frequent",
          "VNM Infrequent"
)

cols<-c("#c56043", "#e04b5c", "#de5353", "#c25c42", "#d85652", "#c8585d", "#de534b", "#cd5440", "#d35d5e", "#c3615a", "#d46655", "#cf5552", "#c65f5d","#de534b", "#cd5440",
        "#856554", "#896757", "#815b69", "#8f6756", "#86554d", "#8a675d", "#85586a", "#805158", "#855d52", "#8f4e50", "#8e5960", "#80685a", "#75584c","#85586a", "#805158",
        "#85b68f", "#77bf98", "#86b18f", "#85b589", "#7eB68d", "#8bB37a", "#80B491", "#65Bd7d", "#81B784", "#8bb883", "#8ebf92", "#7bBe8f", "#91bd79","#81B784", "#8bb883",
        "#6b9168", "#69925b", "#689d55", "#6ba261", "#5aaa6b", "#599866", "#519c58", "#55a755", "#5caa53", "#5c9c60", "#588f5b", "#5e9d6f", "#5aa757","#5caa53", "#5c9c60", 
        "#64a9e6", "#79a5ee", "#809be9", "#6d9dfa", "#6899ec", "#6facde", "#77abed", "#63aaf0", "#63a4e4", "#74a5e3", "#79a9eb", "#6aaaf6", "#78a4dd","#6facde", "#77abed"
        
) 




tiff("F_ATD_GMM_SEPARATELY_no_COEFS.tiff",width=30,height=33,units="cm",res=600,compression="lzw")
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
  (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,3,2])+   # Age (A): COL More Freq. Users
    (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,5,2])+   # Age (A): CZE More Freq. Users
    (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,7,2])+   # Age (A): ZAF More Freq. Users
    (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,9,2])+   # Age (A): TUR More Freq. Users
    (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,11,2])   # Age (A): COL More Freq. Users
)/6

TR_Age_F_INfreq <- (
  (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,4,2])+   # Age (A): COL More Freq. Users
    (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,6,2])+   # Age (A): CZE More Freq. Users
    (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,8,2])+   # Age (A): ZAF More Freq. Users
    (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,10,2])+   # Age (A): TUR More Freq. Users
    (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
TR_Dist_F_freq <- (
  (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,3,3])+   # Dist (A): COL More Freq. Users
    (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,5,3])+   # Dist (A): CZE More Freq. Users
    (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,7,3])+   # Dist (A): ZAF More Freq. Users
    (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,9,3])+   # Dist (A): TUR More Freq. Users
    (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,11,3])   # Dist (A): COL More Freq. Users
)/6

TR_Dist_F_INfreq <- (
  (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,4,3])+   # Dist (A): COL More Freq. Users
    (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,6,3])+   # Dist (A): CZE More Freq. Users
    (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,8,3])+   # Dist (A): ZAF More Freq. Users
    (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,10,3])+   # Dist (A): TUR More Freq. Users
    (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
TR_Asym_F_freq <- (
  (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,3,4])+   # Asym (A): COL More Freq. Users
    (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,5,4])+   # Asym (A): CZE More Freq. Users
    (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,7,4])+   # Asym (A): ZAF More Freq. Users
    (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,9,4])+   # Asym (A): TUR More Freq. Users
    (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,11,4])   # Asym (A): COL More Freq. Users
)/6

TR_Asym_F_INfreq <- (
  (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,4,4])+   # Asym (A): COL More Freq. Users
    (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,6,4])+   # Asym (A): CZE More Freq. Users
    (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,8,4])+   # Asym (A): ZAF More Freq. Users
    (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,10,4])+   # Asym (A): TUR More Freq. Users
    (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
TR_SexTyp_F_freq <- (
  (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,3,5])+   # SShD (A): COL More Freq. Users
    (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,5,5])+   # SShD (A): CZE More Freq. Users
    (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,7,5])+   # SShD (A): ZAF More Freq. Users
    (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,9,5])+   # SShD (A): TUR More Freq. Users
    (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,11,5])   # SShD (A): COL More Freq. Users
)/6

TR_SexTyp_F_INfreq <- (
  (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,4,5])+   # SShD (A): COL More Freq. Users
    (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,6,5])+   # SShD (A): CZE More Freq. Users
    (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,8,5])+   # SShD (A): ZAF More Freq. Users
    (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,10,5])+   # SShD (A): TUR More Freq. Users
    (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
TR_L_F_freq <- (
  (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,3,6])+   # L (A): COL More Freq. Users
    (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,5,6])+   # L (A): CZE More Freq. Users
    (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,7,6])+   # L (A): ZAF More Freq. Users
    (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,9,6])+   # L (A): TUR More Freq. Users
    (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,11,6])   # L (A): COL More Freq. Users
)/6

TR_L_F_INfreq <- (
  (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,4,6])+   # L (A): COL More Freq. Users
    (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,6,6])+   # L (A): CZE More Freq. Users
    (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,8,6])+   # L (A): ZAF More Freq. Users
    (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,10,6])+   # L (A): TUR More Freq. Users
    (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,12,6])   # L (A): COL More Freq. Users
)/6



# Trusw - GMM 
str(post_T_F)
str(post_T_F$f_per_group_pr_T) # Mind that 1 is intercept! 

F_posts_ATD_MM_TRU <- list(
  #  AGE
  Age_Slope = (post_T_F$b_age_T),    # Slope prior to accessing culture
  
  TR_Age_F_freq,
  TR_Age_F_INfreq,
  
  Age_AUS_Freq = (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (post_T_F$b_age_T + post_T_F$f_per_group_pr_T[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (post_T_F$b_dist_T),    # Dist (A): CZE Control
  
  TR_Dist_F_freq,
  TR_Dist_F_INfreq,
  
  Dist_AUS_Freq = (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (post_T_F$b_dist_T + post_T_F$f_per_group_pr_T[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (post_T_F$b_FA_T),    # Asym (A): CZE Control
  
  TR_Asym_F_freq,
  TR_Asym_F_INfreq,
  
  Asym_AUS_Freq = (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (post_T_F$b_FA_T + post_T_F$f_per_group_pr_T[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (post_T_F$b_sshd_T),    # SexTyp (A): CZE Control
  
  TR_SexTyp_F_freq,
  TR_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (post_T_F$b_sshd_T + post_T_F$f_per_group_pr_T[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (post_T_F$b_L_T),    # L (A): CZE Control
  
  TR_L_F_freq,
  TR_L_F_INfreq,
  
  L_AUS_Freq = (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (post_T_F$b_L_T + post_T_F$f_per_group_pr_T[,12,6])   # L (A): COL Less Freq. Users
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
  (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,3,2])+   # Age (A): COL More Freq. Users
    (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,5,2])+   # Age (A): CZE More Freq. Users
    (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,7,2])+   # Age (A): ZAF More Freq. Users
    (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,9,2])+   # Age (A): TUR More Freq. Users
    (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,11,2])   # Age (A): COL More Freq. Users
)/6

DOM_Age_F_INfreq <- (
  (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,4,2])+   # Age (A): COL More Freq. Users
    (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,6,2])+   # Age (A): CZE More Freq. Users
    (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,8,2])+   # Age (A): ZAF More Freq. Users
    (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,10,2])+   # Age (A): TUR More Freq. Users
    (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
DOM_Dist_F_freq <- (
  (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,3,3])+   # Dist (A): COL More Freq. Users
    (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,5,3])+   # Dist (A): CZE More Freq. Users
    (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,7,3])+   # Dist (A): ZAF More Freq. Users
    (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,9,3])+   # Dist (A): TUR More Freq. Users
    (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,11,3])   # Dist (A): COL More Freq. Users
)/6

DOM_Dist_F_INfreq <- (
  (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,4,3])+   # Dist (A): COL More Freq. Users
    (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,6,3])+   # Dist (A): CZE More Freq. Users
    (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,8,3])+   # Dist (A): ZAF More Freq. Users
    (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,10,3])+   # Dist (A): TUR More Freq. Users
    (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
DOM_Asym_F_freq <- (
  (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,3,4])+   # Asym (A): COL More Freq. Users
    (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,5,4])+   # Asym (A): CZE More Freq. Users
    (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,7,4])+   # Asym (A): ZAF More Freq. Users
    (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,9,4])+   # Asym (A): TUR More Freq. Users
    (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,11,4])   # Asym (A): COL More Freq. Users
)/6

DOM_Asym_F_INfreq <- (
  (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,4,4])+   # Asym (A): COL More Freq. Users
    (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,6,4])+   # Asym (A): CZE More Freq. Users
    (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,8,4])+   # Asym (A): ZAF More Freq. Users
    (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,10,4])+   # Asym (A): TUR More Freq. Users
    (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
DOM_SexTyp_F_freq <- (
  (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,3,5])+   # SShD (A): COL More Freq. Users
    (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,5,5])+   # SShD (A): CZE More Freq. Users
    (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,7,5])+   # SShD (A): ZAF More Freq. Users
    (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,9,5])+   # SShD (A): TUR More Freq. Users
    (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,11,5])   # SShD (A): COL More Freq. Users
)/6

DOM_SexTyp_F_INfreq <- (
  (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,4,5])+   # SShD (A): COL More Freq. Users
    (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,6,5])+   # SShD (A): CZE More Freq. Users
    (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,8,5])+   # SShD (A): ZAF More Freq. Users
    (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,10,5])+   # SShD (A): TUR More Freq. Users
    (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
DOM_L_F_freq <- (
  (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,3,6])+   # L (A): COL More Freq. Users
    (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,5,6])+   # L (A): CZE More Freq. Users
    (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,7,6])+   # L (A): ZAF More Freq. Users
    (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,9,6])+   # L (A): TUR More Freq. Users
    (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,11,6])   # L (A): COL More Freq. Users
)/6

DOM_L_F_INfreq <- (
  (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,4,6])+   # L (A): COL More Freq. Users
    (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,6,6])+   # L (A): CZE More Freq. Users
    (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,8,6])+   # L (A): ZAF More Freq. Users
    (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,10,6])+   # L (A): TUR More Freq. Users
    (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,12,6])   # L (A): COL More Freq. Users
)/6



# GMM - SEPARATELY 

# Dominance - GMM 
str(post_D_F)
str(post_D_F$f_per_group_pr_D) # Mind that 1 is intercept! 

F_posts_ATD_MM_DOM <- list(
  #  AGE
  Age_Slope = (post_D_F$b_age_D),    # Slope prior to accessing culture
  
  DOM_Age_F_freq,
  DOM_Age_F_INfreq,
  
  Age_AUS_Freq = (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (post_D_F$b_age_D + post_D_F$f_per_group_pr_D[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (post_D_F$b_dist_D),    # Dist (A): CZE Control
  
  DOM_Dist_F_freq,
  DOM_Dist_F_INfreq,
  
  Dist_AUS_Freq = (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (post_D_F$b_dist_D + post_D_F$f_per_group_pr_D[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (post_D_F$b_FA_D),    # Asym (A): CZE Control
  
  DOM_Asym_F_freq,
  DOM_Asym_F_INfreq,
  
  Asym_AUS_Freq = (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (post_D_F$b_FA_D + post_D_F$f_per_group_pr_D[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (post_D_F$b_sshd_D),    # SexTyp (A): CZE Control
  
  DOM_SexTyp_F_freq,
  DOM_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (post_D_F$b_sshd_D + post_D_F$f_per_group_pr_D[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (post_D_F$b_L_D),    # L (A): CZE Control
  
  DOM_L_F_freq,
  DOM_L_F_INfreq,
  
  L_AUS_Freq = (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (post_D_F$b_L_D + post_D_F$f_per_group_pr_D[,12,6])   # L (A): COL Less Freq. Users
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



# Figure S4 - analogue of the Figure 4 in the main body of the article, which is a figure (top) presenting mean estimates
# in every sample, (bottom) presenting effects of morphometric and demographic predictors: SPECIFICALLY for MEN
# Version without labels

# Upper part: Frequent - all
# Compute the average of all group estimates

# Note - these are the same data as those to enter the A-T-D model; therefore, 
# the order of levels is the same, too. 

Avg_Freq_U_A = ( 
  post_A_M$aA + post_A_M$f_per_group_pr_A[,1,1] + 
    post_A_M$aA + post_A_M$f_per_group_pr_A[,3,1] + 
    post_A_M$aA + post_A_M$f_per_group_pr_A[,5,1] + 
    post_A_M$aA + post_A_M$f_per_group_pr_A[,7,1] + 
    post_A_M$aA + post_A_M$f_per_group_pr_A[,9,1] + 
    post_A_M$aA + post_A_M$f_per_group_pr_A[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_A = ( 
  post_A_M$aA + post_A_M$f_per_group_pr_A[,2,1] + 
    post_A_M$aA + post_A_M$f_per_group_pr_A[,4,1] + 
    post_A_M$aA + post_A_M$f_per_group_pr_A[,6,1] + 
    post_A_M$aA + post_A_M$f_per_group_pr_A[,8,1] + 
    post_A_M$aA + post_A_M$f_per_group_pr_A[,10,1] + 
    post_A_M$aA + post_A_M$f_per_group_pr_A[,12,1] 
) / 6



F_posts_ATR <- list(
  # 1 Intercept: 
  Intercept = post_A_M$aA,
  # Infrequent users
  Avg_Freq_U_A,
  # Frequent users 
  Avg_Infreq_U_A,
  # Difference 
  Difference = Avg_Freq_U_A - Avg_Infreq_U_A,
  # 4 Australia - above - median users  
  AUS_Intensive_User = post_A_M$aA + post_A_M$f_per_group_pr_A[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = post_A_M$aA + post_A_M$f_per_group_pr_A[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = post_A_M$aA + post_A_M$f_per_group_pr_A[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = post_A_M$aA + post_A_M$f_per_group_pr_A[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = post_A_M$aA + post_A_M$f_per_group_pr_A[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = post_A_M$aA + post_A_M$f_per_group_pr_A[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = post_A_M$aA + post_A_M$f_per_group_pr_A[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = post_A_M$aA + post_A_M$f_per_group_pr_A[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = post_A_M$aA + post_A_M$f_per_group_pr_A[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = post_A_M$aA + post_A_M$f_per_group_pr_A[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = post_A_M$aA + post_A_M$f_per_group_pr_A[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = post_A_M$aA + post_A_M$f_per_group_pr_A[,12,1]
)

summary.data.frame(F_posts_ATR)
str(F_posts_ATR) # Need 13 rows 

#--------------

# Trustworthiness
Avg_Freq_U_T = ( 
  post_T_M$aT + post_T_M$f_per_group_pr_T[,1,1] + 
    post_T_M$aT + post_T_M$f_per_group_pr_T[,3,1] + 
    post_T_M$aT + post_T_M$f_per_group_pr_T[,5,1] + 
    post_T_M$aT + post_T_M$f_per_group_pr_T[,7,1] + 
    post_T_M$aT + post_T_M$f_per_group_pr_T[,9,1] + 
    post_T_M$aT + post_T_M$f_per_group_pr_T[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_T = ( 
  post_T_M$aT + post_T_M$f_per_group_pr_T[,2,1] + 
    post_T_M$aT + post_T_M$f_per_group_pr_T[,4,1] + 
    post_T_M$aT + post_T_M$f_per_group_pr_T[,6,1] + 
    post_T_M$aT + post_T_M$f_per_group_pr_T[,8,1] + 
    post_T_M$aT + post_T_M$f_per_group_pr_T[,10,1] + 
    post_T_M$aT + post_T_M$f_per_group_pr_T[,12,1] 
) / 6

F_posts_TRU <- list(
  # 1 Intercept: 
  Intercept = post_T_M$aT,
  # Infrequent users
  Avg_Freq_U_T,
  # Frequent users 
  Avg_Infreq_U_T,
  # Difference 
  Difference = Avg_Freq_U_T - Avg_Infreq_U_T,
  # 4 Australia - above - median users  
  AUS_Intensive_User = post_T_M$aT + post_T_M$f_per_group_pr_T[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = post_T_M$aT + post_T_M$f_per_group_pr_T[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = post_T_M$aT + post_T_M$f_per_group_pr_T[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = post_T_M$aT + post_T_M$f_per_group_pr_T[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = post_T_M$aT + post_T_M$f_per_group_pr_T[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = post_T_M$aT + post_T_M$f_per_group_pr_T[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = post_T_M$aT + post_T_M$f_per_group_pr_T[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = post_T_M$aT + post_T_M$f_per_group_pr_T[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = post_T_M$aT + post_T_M$f_per_group_pr_T[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = post_T_M$aT + post_T_M$f_per_group_pr_T[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = post_T_M$aT + post_T_M$f_per_group_pr_T[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = post_T_M$aT + post_T_M$f_per_group_pr_T[,12,1]
)

summary.data.frame(F_posts_TRU)
str(F_posts_TRU) # Need 13 rows 

#--------------

# Dominance
Avg_Freq_U_D = ( 
  post_D_M$aD + post_D_M$f_per_group_pr_D[,1,1] + 
    post_D_M$aD + post_D_M$f_per_group_pr_D[,3,1] + 
    post_D_M$aD + post_D_M$f_per_group_pr_D[,5,1] + 
    post_D_M$aD + post_D_M$f_per_group_pr_D[,7,1] + 
    post_D_M$aD + post_D_M$f_per_group_pr_D[,9,1] + 
    post_D_M$aD + post_D_M$f_per_group_pr_D[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_D = ( 
  post_D_M$aD + post_D_M$f_per_group_pr_D[,2,1] + 
    post_D_M$aD + post_D_M$f_per_group_pr_D[,4,1] + 
    post_D_M$aD + post_D_M$f_per_group_pr_D[,6,1] + 
    post_D_M$aD + post_D_M$f_per_group_pr_D[,8,1] + 
    post_D_M$aD + post_D_M$f_per_group_pr_D[,10,1] + 
    post_D_M$aD + post_D_M$f_per_group_pr_D[,12,1] 
) / 6

F_posts_DOM <- list(
  # 1 Intercept: 
  Intercept = post_D_M$aD,
  # Infrequent users
  Avg_Freq_U_D,
  # Frequent users 
  Avg_Infreq_U_D,
  # Difference 
  Difference = Avg_Freq_U_D - Avg_Infreq_U_D,
  # 4 Australia - above - median users  
  AUS_Intensive_User = post_D_M$aD + post_D_M$f_per_group_pr_D[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = post_D_M$aD + post_D_M$f_per_group_pr_D[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = post_D_M$aD + post_D_M$f_per_group_pr_D[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = post_D_M$aD + post_D_M$f_per_group_pr_D[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = post_D_M$aD + post_D_M$f_per_group_pr_D[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = post_D_M$aD + post_D_M$f_per_group_pr_D[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = post_D_M$aD + post_D_M$f_per_group_pr_D[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = post_D_M$aD + post_D_M$f_per_group_pr_D[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = post_D_M$aD + post_D_M$f_per_group_pr_D[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = post_D_M$aD + post_D_M$f_per_group_pr_D[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = post_D_M$aD + post_D_M$f_per_group_pr_D[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = post_D_M$aD + post_D_M$f_per_group_pr_D[,12,1]
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
        "More freq.U",
        "Less freq.U",
        "Freq - Less.freq: Difference",
        "Australia/NewZ - More freq.U",
        "Australia/NewZ - Less freq.U",
        "Colombia - More freq.U",
        "Colombia - Less freq.U",
        "Czechia - More freq.U",
        "Czechia - Less freq.U",
        "South Africa - More freq.U",
        "South Africa - Less freq.U",
        "Turkey - More freq.U",
        "Turkey - Less freq.U",
        "Vietnam - More freq.U",
        "Vietnam - Less freq.U"
)


# Figure S3:Upper Panel - Analogue of Figure 3:Upper Panel (final assembly done in InkScape)...

tiff("M_ATD_separate_models_LEVELS_NOLABELS.tiff",width=30,height=14,units="cm",res=600,compression="lzw")
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
  (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,3,2])+   # Age (A): COL More Freq. Users
    (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,5,2])+   # Age (A): CZE More Freq. Users
    (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,7,2])+   # Age (A): ZAF More Freq. Users
    (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,9,2])+   # Age (A): TUR More Freq. Users
    (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,11,2])   # Age (A): COL More Freq. Users
)/6

Attr_Age_F_INfreq <- (
  (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,4,2])+   # Age (A): COL More Freq. Users
    (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,6,2])+   # Age (A): CZE More Freq. Users
    (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,8,2])+   # Age (A): ZAF More Freq. Users
    (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,10,2])+   # Age (A): TUR More Freq. Users
    (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
Attr_Dist_F_freq <- (
  (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,3,3])+   # Dist (A): COL More Freq. Users
    (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,5,3])+   # Dist (A): CZE More Freq. Users
    (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,7,3])+   # Dist (A): ZAF More Freq. Users
    (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,9,3])+   # Dist (A): TUR More Freq. Users
    (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,11,3])   # Dist (A): COL More Freq. Users
)/6

Attr_Dist_F_INfreq <- (
  (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,4,3])+   # Dist (A): COL More Freq. Users
    (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,6,3])+   # Dist (A): CZE More Freq. Users
    (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,8,3])+   # Dist (A): ZAF More Freq. Users
    (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,10,3])+   # Dist (A): TUR More Freq. Users
    (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
Attr_Asym_F_freq <- (
  (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,3,4])+   # Asym (A): COL More Freq. Users
    (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,5,4])+   # Asym (A): CZE More Freq. Users
    (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,7,4])+   # Asym (A): ZAF More Freq. Users
    (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,9,4])+   # Asym (A): TUR More Freq. Users
    (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,11,4])   # Asym (A): COL More Freq. Users
)/6

Attr_Asym_F_INfreq <- (
  (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,4,4])+   # Asym (A): COL More Freq. Users
    (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,6,4])+   # Asym (A): CZE More Freq. Users
    (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,8,4])+   # Asym (A): ZAF More Freq. Users
    (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,10,4])+   # Asym (A): TUR More Freq. Users
    (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
Attr_SexTyp_F_freq <- (
  (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,3,5])+   # SShD (A): COL More Freq. Users
    (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,5,5])+   # SShD (A): CZE More Freq. Users
    (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,7,5])+   # SShD (A): ZAF More Freq. Users
    (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,9,5])+   # SShD (A): TUR More Freq. Users
    (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,11,5])   # SShD (A): COL More Freq. Users
)/6

Attr_SexTyp_F_INfreq <- (
  (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,4,5])+   # SShD (A): COL More Freq. Users
    (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,6,5])+   # SShD (A): CZE More Freq. Users
    (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,8,5])+   # SShD (A): ZAF More Freq. Users
    (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,10,5])+   # SShD (A): TUR More Freq. Users
    (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
Attr_L_F_freq <- (
  (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,3,6])+   # L (A): COL More Freq. Users
    (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,5,6])+   # L (A): CZE More Freq. Users
    (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,7,6])+   # L (A): ZAF More Freq. Users
    (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,9,6])+   # L (A): TUR More Freq. Users
    (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,11,6])   # L (A): COL More Freq. Users
)/6

Attr_L_F_INfreq <- (
  (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,4,6])+   # L (A): COL More Freq. Users
    (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,6,6])+   # L (A): CZE More Freq. Users
    (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,8,6])+   # L (A): ZAF More Freq. Users
    (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,10,6])+   # L (A): TUR More Freq. Users
    (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,12,6])   # L (A): COL More Freq. Users
)/6



# GMM - SEPARATELY 

# Attractiveness - GMM 
str(post_A_M)
str(post_A_M$f_per_group_pr_A) # Mind that 1 is intercept! 

F_posts_ATD_MM_Attr <- list(
  #  AGE
  Age_Slope = (post_A_M$b_age_A),    # Slope prior to accessing culture
  
  Attr_Age_F_freq,
  Attr_Age_F_INfreq,
  
  Age_AUS_Freq = (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (post_A_M$b_age_A + post_A_M$f_per_group_pr_A[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (post_A_M$b_dist_A),    # Dist (A): CZE Control
  
  Attr_Dist_F_freq,
  Attr_Dist_F_INfreq,
  
  Dist_AUS_Freq = (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (post_A_M$b_dist_A + post_A_M$f_per_group_pr_A[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (post_A_M$b_FA_A),    # Asym (A): CZE Control
  
  Attr_Asym_F_freq,
  Attr_Asym_F_INfreq,
  
  Asym_AUS_Freq = (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (post_A_M$b_FA_A + post_A_M$f_per_group_pr_A[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (post_A_M$b_sshd_A),    # SexTyp (A): CZE Control
  
  Attr_SexTyp_F_freq,
  Attr_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (post_A_M$b_sshd_A + post_A_M$f_per_group_pr_A[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (post_A_M$b_L_A),    # L (A): CZE Control
  
  Attr_L_F_freq,
  Attr_L_F_INfreq,
  
  L_AUS_Freq = (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (post_A_M$b_L_A + post_A_M$f_per_group_pr_A[,12,6])   # L (A): COL Less Freq. Users
)


summary.data.frame(F_posts_ATD_MM_Attr)
str(F_posts_ATD_MM_Attr) # Need 65 rows 


labs <- c("Fixed Slope",
          
          "Frequent (All)",
          "Infrequent (All)",
          
          "AUS/NZE Frequent",
          "AUS/NZE Infrequent",
          
          "COL Frequent",
          "COL Infrequent",
          
          "CZE Frequent",
          "CZE Infreqeunt",
          
          "ZAF Frequent",
          "ZAF Infrequent",
          
          "TUR Frequent",
          "TUR Infrequent",
          
          "VNM Frequent",
          "VNM Infrequent"
)

cols<-c("#c56043", "#e04b5c", "#de5353", "#c25c42", "#d85652", "#c8585d", "#de534b", "#cd5440", "#d35d5e", "#c3615a", "#d46655", "#cf5552", "#c65f5d","#de534b", "#cd5440",
        "#856554", "#896757", "#815b69", "#8f6756", "#86554d", "#8a675d", "#85586a", "#805158", "#855d52", "#8f4e50", "#8e5960", "#80685a", "#75584c","#85586a", "#805158",
        "#85b68f", "#77bf98", "#86b18f", "#85b589", "#7eB68d", "#8bB37a", "#80B491", "#65Bd7d", "#81B784", "#8bb883", "#8ebf92", "#7bBe8f", "#91bd79","#81B784", "#8bb883",
        "#6b9168", "#69925b", "#689d55", "#6ba261", "#5aaa6b", "#599866", "#519c58", "#55a755", "#5caa53", "#5c9c60", "#588f5b", "#5e9d6f", "#5aa757","#5caa53", "#5c9c60", 
        "#64a9e6", "#79a5ee", "#809be9", "#6d9dfa", "#6899ec", "#6facde", "#77abed", "#63aaf0", "#63a4e4", "#74a5e3", "#79a9eb", "#6aaaf6", "#78a4dd","#6facde", "#77abed"
        
) 




tiff("M_ATD_GMM_SEPARATELY_no_COEFS.tiff",width=30,height=33,units="cm",res=600,compression="lzw")
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
  (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,3,2])+   # Age (A): COL More Freq. Users
    (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,5,2])+   # Age (A): CZE More Freq. Users
    (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,7,2])+   # Age (A): ZAF More Freq. Users
    (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,9,2])+   # Age (A): TUR More Freq. Users
    (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,11,2])   # Age (A): COL More Freq. Users
)/6

TR_Age_F_INfreq <- (
  (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,4,2])+   # Age (A): COL More Freq. Users
    (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,6,2])+   # Age (A): CZE More Freq. Users
    (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,8,2])+   # Age (A): ZAF More Freq. Users
    (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,10,2])+   # Age (A): TUR More Freq. Users
    (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
TR_Dist_F_freq <- (
  (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,3,3])+   # Dist (A): COL More Freq. Users
    (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,5,3])+   # Dist (A): CZE More Freq. Users
    (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,7,3])+   # Dist (A): ZAF More Freq. Users
    (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,9,3])+   # Dist (A): TUR More Freq. Users
    (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,11,3])   # Dist (A): COL More Freq. Users
)/6

TR_Dist_F_INfreq <- (
  (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,4,3])+   # Dist (A): COL More Freq. Users
    (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,6,3])+   # Dist (A): CZE More Freq. Users
    (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,8,3])+   # Dist (A): ZAF More Freq. Users
    (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,10,3])+   # Dist (A): TUR More Freq. Users
    (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
TR_Asym_F_freq <- (
  (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,3,4])+   # Asym (A): COL More Freq. Users
    (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,5,4])+   # Asym (A): CZE More Freq. Users
    (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,7,4])+   # Asym (A): ZAF More Freq. Users
    (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,9,4])+   # Asym (A): TUR More Freq. Users
    (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,11,4])   # Asym (A): COL More Freq. Users
)/6

TR_Asym_F_INfreq <- (
  (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,4,4])+   # Asym (A): COL More Freq. Users
    (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,6,4])+   # Asym (A): CZE More Freq. Users
    (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,8,4])+   # Asym (A): ZAF More Freq. Users
    (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,10,4])+   # Asym (A): TUR More Freq. Users
    (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
TR_SexTyp_F_freq <- (
  (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,3,5])+   # SShD (A): COL More Freq. Users
    (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,5,5])+   # SShD (A): CZE More Freq. Users
    (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,7,5])+   # SShD (A): ZAF More Freq. Users
    (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,9,5])+   # SShD (A): TUR More Freq. Users
    (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,11,5])   # SShD (A): COL More Freq. Users
)/6

TR_SexTyp_F_INfreq <- (
  (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,4,5])+   # SShD (A): COL More Freq. Users
    (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,6,5])+   # SShD (A): CZE More Freq. Users
    (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,8,5])+   # SShD (A): ZAF More Freq. Users
    (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,10,5])+   # SShD (A): TUR More Freq. Users
    (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
TR_L_F_freq <- (
  (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,3,6])+   # L (A): COL More Freq. Users
    (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,5,6])+   # L (A): CZE More Freq. Users
    (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,7,6])+   # L (A): ZAF More Freq. Users
    (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,9,6])+   # L (A): TUR More Freq. Users
    (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,11,6])   # L (A): COL More Freq. Users
)/6

TR_L_F_INfreq <- (
  (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,4,6])+   # L (A): COL More Freq. Users
    (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,6,6])+   # L (A): CZE More Freq. Users
    (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,8,6])+   # L (A): ZAF More Freq. Users
    (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,10,6])+   # L (A): TUR More Freq. Users
    (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,12,6])   # L (A): COL More Freq. Users
)/6



# Trusw - GMM 
str(post_T_M)
str(post_T_M$f_per_group_pr_T) # Mind that 1 is intercept! 

F_posts_ATD_MM_TRU <- list(
  #  AGE
  Age_Slope = (post_T_M$b_age_T),    # Slope prior to accessing culture
  
  TR_Age_F_freq,
  TR_Age_F_INfreq,
  
  Age_AUS_Freq = (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (post_T_M$b_age_T + post_T_M$f_per_group_pr_T[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (post_T_M$b_dist_T),    # Dist (A): CZE Control
  
  TR_Dist_F_freq,
  TR_Dist_F_INfreq,
  
  Dist_AUS_Freq = (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (post_T_M$b_dist_T + post_T_M$f_per_group_pr_T[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (post_T_M$b_FA_T),    # Asym (A): CZE Control
  
  TR_Asym_F_freq,
  TR_Asym_F_INfreq,
  
  Asym_AUS_Freq = (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (post_T_M$b_FA_T + post_T_M$f_per_group_pr_T[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (post_T_M$b_sshd_T),    # SexTyp (A): CZE Control
  
  TR_SexTyp_F_freq,
  TR_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (post_T_M$b_sshd_T + post_T_M$f_per_group_pr_T[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (post_T_M$b_L_T),    # L (A): CZE Control
  
  TR_L_F_freq,
  TR_L_F_INfreq,
  
  L_AUS_Freq = (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (post_T_M$b_L_T + post_T_M$f_per_group_pr_T[,12,6])   # L (A): COL Less Freq. Users
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
  (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,3,2])+   # Age (A): COL More Freq. Users
    (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,5,2])+   # Age (A): CZE More Freq. Users
    (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,7,2])+   # Age (A): ZAF More Freq. Users
    (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,9,2])+   # Age (A): TUR More Freq. Users
    (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,11,2])   # Age (A): COL More Freq. Users
)/6

DOM_Age_F_INfreq <- (
  (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,4,2])+   # Age (A): COL More Freq. Users
    (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,6,2])+   # Age (A): CZE More Freq. Users
    (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,8,2])+   # Age (A): ZAF More Freq. Users
    (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,10,2])+   # Age (A): TUR More Freq. Users
    (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
DOM_Dist_F_freq <- (
  (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,3,3])+   # Dist (A): COL More Freq. Users
    (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,5,3])+   # Dist (A): CZE More Freq. Users
    (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,7,3])+   # Dist (A): ZAF More Freq. Users
    (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,9,3])+   # Dist (A): TUR More Freq. Users
    (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,11,3])   # Dist (A): COL More Freq. Users
)/6

DOM_Dist_F_INfreq <- (
  (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,4,3])+   # Dist (A): COL More Freq. Users
    (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,6,3])+   # Dist (A): CZE More Freq. Users
    (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,8,3])+   # Dist (A): ZAF More Freq. Users
    (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,10,3])+   # Dist (A): TUR More Freq. Users
    (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
DOM_Asym_F_freq <- (
  (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,3,4])+   # Asym (A): COL More Freq. Users
    (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,5,4])+   # Asym (A): CZE More Freq. Users
    (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,7,4])+   # Asym (A): ZAF More Freq. Users
    (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,9,4])+   # Asym (A): TUR More Freq. Users
    (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,11,4])   # Asym (A): COL More Freq. Users
)/6

DOM_Asym_F_INfreq <- (
  (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,4,4])+   # Asym (A): COL More Freq. Users
    (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,6,4])+   # Asym (A): CZE More Freq. Users
    (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,8,4])+   # Asym (A): ZAF More Freq. Users
    (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,10,4])+   # Asym (A): TUR More Freq. Users
    (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
DOM_SexTyp_F_freq <- (
  (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,3,5])+   # SShD (A): COL More Freq. Users
    (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,5,5])+   # SShD (A): CZE More Freq. Users
    (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,7,5])+   # SShD (A): ZAF More Freq. Users
    (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,9,5])+   # SShD (A): TUR More Freq. Users
    (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,11,5])   # SShD (A): COL More Freq. Users
)/6

DOM_SexTyp_F_INfreq <- (
  (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,4,5])+   # SShD (A): COL More Freq. Users
    (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,6,5])+   # SShD (A): CZE More Freq. Users
    (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,8,5])+   # SShD (A): ZAF More Freq. Users
    (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,10,5])+   # SShD (A): TUR More Freq. Users
    (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
DOM_L_F_freq <- (
  (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,3,6])+   # L (A): COL More Freq. Users
    (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,5,6])+   # L (A): CZE More Freq. Users
    (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,7,6])+   # L (A): ZAF More Freq. Users
    (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,9,6])+   # L (A): TUR More Freq. Users
    (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,11,6])   # L (A): COL More Freq. Users
)/6

DOM_L_F_INfreq <- (
  (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,4,6])+   # L (A): COL More Freq. Users
    (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,6,6])+   # L (A): CZE More Freq. Users
    (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,8,6])+   # L (A): ZAF More Freq. Users
    (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,10,6])+   # L (A): TUR More Freq. Users
    (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,12,6])   # L (A): COL More Freq. Users
)/6



# GMM - SEPARATELY 

# Dominance - GMM 
str(post_D_M)
str(post_D_M$f_per_group_pr_D) # Mind that 1 is intercept! 

F_posts_ATD_MM_DOM <- list(
  #  AGE
  Age_Slope = (post_D_M$b_age_D),    # Slope prior to accessing culture
  
  DOM_Age_F_freq,
  DOM_Age_F_INfreq,
  
  Age_AUS_Freq = (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (post_D_M$b_age_D + post_D_M$f_per_group_pr_D[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (post_D_M$b_dist_D),    # Dist (A): CZE Control
  
  DOM_Dist_F_freq,
  DOM_Dist_F_INfreq,
  
  Dist_AUS_Freq = (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (post_D_M$b_dist_D + post_D_M$f_per_group_pr_D[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (post_D_M$b_FA_D),    # Asym (A): CZE Control
  
  DOM_Asym_F_freq,
  DOM_Asym_F_INfreq,
  
  Asym_AUS_Freq = (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (post_D_M$b_FA_D + post_D_M$f_per_group_pr_D[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (post_D_M$b_sshd_D),    # SexTyp (A): CZE Control
  
  DOM_SexTyp_F_freq,
  DOM_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (post_D_M$b_sshd_D + post_D_M$f_per_group_pr_D[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (post_D_M$b_L_D),    # L (A): CZE Control
  
  DOM_L_F_freq,
  DOM_L_F_INfreq,
  
  L_AUS_Freq = (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (post_D_M$b_L_D + post_D_M$f_per_group_pr_D[,12,6])   # L (A): COL Less Freq. Users
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
