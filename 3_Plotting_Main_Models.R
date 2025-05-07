# Drawing

# Please mind that the model objects are not provided, but you have the data and the script. 
# All you need to do is to sample (and save) the posteriors on your own. 
# Name them "post_F_ATD_10_03_25_long.RDS" for women and "post_M_ATD_10_03_25_long.RDS" for men 
# and upload...

library(rethinking)

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 
#   ATTR/TRUSWT/DOM In one model
# 
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.


# FEMAELS 

post_ATD_F <- readRDS("post_F_ATD_10_03_25_long.RDS") # This is a model with A / T / D at once

str(post_ATD_F)

# MALES

post_ATD_M <- readRDS("post_M_ATD_10_03_25_long.RDS") # This is a model with A / T / D at once

str(post_ATD_M)


# Levels

#              1    2    3    4    5    6    7    8    9   10   11   12
# AUS Above 1428    0    0    0    0    0    0    0    0    0    0    0
# AUS Below    0 1372    0    0    0    0    0    0    0    0    0    0
# COL Above    0    0  418    0    0    0    0    0    0    0    0    0
# COL Below    0    0    0  430    0    0    0    0    0    0    0    0
# CZ Above     0    0    0    0 4036    0    0    0    0    0    0    0
# CZ Below     0    0    0    0    0 4014    0    0    0    0    0    0
# RSA Above    0    0    0    0    0    0 1328    0    0    0    0    0
# RSA Below    0    0    0    0    0    0    0 1166    0    0    0    0
# TUR Above    0    0    0    0    0    0    0    0 2082    0    0    0
# TUR Below    0    0    0    0    0    0    0    0    0 2108    0    0
# VN Above     0    0    0    0    0    0    0    0    0    0 1934    0
# VN Below     0    0    0    0    0    0    0    0    0    0    0 1876




# Correlations 

labels_x <- c("AUS/NZE Freq", "AUS/NZE Infreq", "COL Freq","COL Infreq","CZE Freq", "CZE Infreq","ZAF Freq", "ZAF Infreq",
              "TUR Freq", "TUR Infreq", "VNM Freq", "VNM Infreq")
labels_y <- labels_x

str(post_ATD_F$Rho_FSMUi_A)

library(qgraph)
library(bootnet)
library(beepr)
library(ComplexHeatmap)
library(circlize)
library(grid)


# Make it as a 3×1 Matrix
#  ATTRACTIVENESS:F

cor_table_mean <- apply(post_ATD_F$Rho_FSMUi_A,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(post_ATD_F$Rho_FSMUi_A,c(2,3),PI, prob=0.95)
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

cor_table_mean <- apply(post_ATD_F$Rho_FSMUi_T,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(post_ATD_F$Rho_FSMUi_T,c(2,3),PI, prob=0.95)
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

cor_table_mean <- apply(post_ATD_F$Rho_FSMUi_D,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(post_ATD_F$Rho_FSMUi_D,c(2,3),PI, prob=0.95)
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

tiff("Correlations_Heatmap_ATD_Females_Attr_wide.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AF
dev.off()

tiff("Correlations_Heatmap_ATD_Females_Tru_wide.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AT
dev.off()

tiff("Correlations_Heatmap_ATD_Females_Dom_wide.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AD
dev.off()


# Figure 3: upper part... Mean estimated ratings (standardised scale) across the raters' samples.

# Attractiveness: 

# Frequent - all
# Compute the average of all group estimates
Avg_Freq_U_A = ( 
  post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,1,1] + 
    post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,3,1] + 
    post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,5,1] + 
    post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,7,1] + 
    post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,9,1] + 
    post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_A = ( 
  post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,2,1] + 
  post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,4,1] + 
  post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,6,1] + 
  post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,8,1] + 
  post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,10,1] + 
  post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,12,1] 
) / 6

F_posts_ATR <- list(
  # 1 Intercept: 
  Intercept = post_ATD_F$aA,
  # Infrequent users
  Avg_Freq_U_A,
  # Frequent users 
  Avg_Infreq_U_A,
  # Difference 
  Difference = Avg_Freq_U_A - Avg_Infreq_U_A,
  # 4 Australia - above - median users  
  AUS_Intensive_User = post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = post_ATD_F$aA + post_ATD_F$f_per_group_pr_A[,12,1]
)

summary.data.frame(F_posts_ATR)
str(F_posts_ATR) # Need 13 rows 

#--------------

# Trustworthiness
Avg_Freq_U_T = ( 
  post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,1,1] + 
    post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,3,1] + 
    post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,5,1] + 
    post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,7,1] + 
    post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,9,1] + 
    post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_T = ( 
  post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,2,1] + 
    post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,4,1] + 
    post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,6,1] + 
    post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,8,1] + 
    post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,10,1] + 
    post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,12,1] 
) / 6

F_posts_TRU <- list(
  # 1 Intercept: 
  Intercept = post_ATD_F$aT,
  # Infrequent users
  Avg_Freq_U_T,
  # Frequent users 
  Avg_Infreq_U_T,
  # Difference 
  Difference = Avg_Freq_U_T - Avg_Infreq_U_T,
  # 4 Australia - above - median users  
  AUS_Intensive_User = post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = post_ATD_F$aT + post_ATD_F$f_per_group_pr_T[,12,1]
)

summary.data.frame(F_posts_TRU)
str(F_posts_TRU) # Need 13 rows 

#--------------

# Dominance
Avg_Freq_U_D = ( 
  post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,1,1] + 
    post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,3,1] + 
    post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,5,1] + 
    post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,7,1] + 
    post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,9,1] + 
    post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_D = ( 
  post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,2,1] + 
    post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,4,1] + 
    post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,6,1] + 
    post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,8,1] + 
    post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,10,1] + 
    post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,12,1] 
) / 6

F_posts_DOM <- list(
  # 1 Intercept: 
  Intercept = post_ATD_F$aD,
  # Infrequent users
  Avg_Freq_U_D,
  # Frequent users 
  Avg_Infreq_U_D,
  # Difference 
  Difference = Avg_Freq_U_D - Avg_Infreq_U_D,
  # 4 Australia - above - median users  
  AUS_Intensive_User = post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = post_ATD_F$aD + post_ATD_F$f_per_group_pr_D[,12,1]
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


tiff("F_ATD_model_LEVELS.tiff",width=34,height=14,units="cm",res=600,compression="lzw")
layout(matrix(1:3,byrow=F,nrow=1), widths=c(1.8,1.2,1.2),heights=c(1))


# ATTRACTIVENESS

par(mar=c(3.1, 15.1, 1.1, 2.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-1,1),ylim=c(1,17),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Attractiveness - Group Estimates", side = 3, line = -0.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

rect(-1,0.5,1.5,17, col=F, border = "#b9b9B999", lwd = 2)
rect(-0.95,12.5,0.5,16.5, col="#F0F0F0", border = F)


segments(x0 = -1, y0 = 1:16, x1 = 0.5, y1 = 1:16, col = "#40808080", lwd = 1.5)
segments(x0 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y0 = 0.5, x1 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y1 = 16.5, col="#80808080",lty=2)

ys<-seq(from=1,to=16, by=1)
ys<-rev(ys)

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,16),
                               Mean=rep(0,16),Up_CI=rep(0,16)) 

for (i in 1:length(F_posts_ATR)) {
  Coefficients_check$Mean[i]<-mean(F_posts_ATR[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(F_posts_ATR)) {
  Coefficients_check$Low_CI[i]<-PI(F_posts_ATR[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(F_posts_ATR)) {
  Coefficients_check$Up_CI[i]<-PI(F_posts_ATR[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))

cex_t <- 1.25
cex_t2 <- 0.95
pls <- 0.75

text("Mean", x=0.8, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.6, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=1.0, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=0.8, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=0.6, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=1.0, y=ys[i], cex=cex_t2)
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
plot(NULL,xlim=c(-1,1),ylim=c(1,17),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Trustworthiness - Group Estimates", side = 3, line = -0.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

rect(-1,0.5,1.5,17, col=F, border = "#b9b9B999", lwd = 2)
rect(-0.95,12.5,0.5,16.5, col="#F0F0F0", border = F)


segments(x0 = -1, y0 = 1:16, x1 = 0.5, y1 = 1:16, col = "#40808080", lwd = 1.5)
segments(x0 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y0 = 0.5, x1 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y1 = 16.5, col="#80808080",lty=2)

ys<-seq(from=1,to=16, by=1)
ys<-rev(ys)

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,16),
                               Mean=rep(0,16),Up_CI=rep(0,16)) 

for (i in 1:length(F_posts_TRU)) {
  Coefficients_check$Mean[i]<-mean(F_posts_TRU[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(F_posts_TRU)) {
  Coefficients_check$Low_CI[i]<-PI(F_posts_TRU[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(F_posts_TRU)) {
  Coefficients_check$Up_CI[i]<-PI(F_posts_TRU[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))


text("Mean", x=0.8, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.6, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=1.0, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=0.8, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=0.6, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=1.0, y=ys[i], cex=cex_t2)
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
plot(NULL,xlim=c(-1,1),ylim=c(1,17),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Dominance - Group Estimates", side = 3, line = -0.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

rect(-1,0.5,1.5,17, col=F, border = "#b9b9B999", lwd = 2)
rect(-0.95,12.5,0.5,16.5, col="#F0F0F0", border = F)


segments(x0 = -1, y0 = 1:16, x1 = 0.5, y1 = 1:16, col = "#40808080", lwd = 1.5)
segments(x0 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y0 = 0.5, x1 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y1 = 16.5, col="#80808080",lty=2)

ys<-seq(from=1,to=16, by=1)
ys<-rev(ys)

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,16),
                               Mean=rep(0,16),Up_CI=rep(0,16)) 

for (i in 1:length(F_posts_DOM)) {
  Coefficients_check$Mean[i]<-mean(F_posts_DOM[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(F_posts_DOM)) {
  Coefficients_check$Low_CI[i]<-PI(F_posts_DOM[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(F_posts_DOM)) {
  Coefficients_check$Up_CI[i]<-PI(F_posts_DOM[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))


text("Mean", x=0.8, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.6, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=1.0, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=0.8, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=0.6, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=1.0, y=ys[i], cex=cex_t2)
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



# DTTO without the labels: 

tiff("F_ATD_model_LEVELS_NOLABELS.tiff",width=30,height=14,units="cm",res=600,compression="lzw")
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




# And now GMM: 
# Preparing composite coefficients: 
# AGE:
Attr_Age_F_freq <- c(
(post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,1,2])+   # Age (A): AUS/NZE More Freq. Users
(post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,3,2])+   # Age (A): COL More Freq. Users
(post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,5,2])+   # Age (A): CZE More Freq. Users
(post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,7,2])+   # Age (A): ZAF More Freq. Users
(post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,9,2])+   # Age (A): TUR More Freq. Users
(post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,11,2])   # Age (A): COL More Freq. Users
)/6

Attr_Age_F_INfreq <- (
  (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,4,2])+   # Age (A): COL More Freq. Users
    (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,6,2])+   # Age (A): CZE More Freq. Users
    (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,8,2])+   # Age (A): ZAF More Freq. Users
    (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,10,2])+   # Age (A): TUR More Freq. Users
    (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
Attr_Dist_F_freq <- (
  (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
  (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,3,3])+   # Dist (A): COL More Freq. Users
  (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,5,3])+   # Dist (A): CZE More Freq. Users
  (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,7,3])+   # Dist (A): ZAF More Freq. Users
  (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,9,3])+   # Dist (A): TUR More Freq. Users
  (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,11,3])   # Dist (A): COL More Freq. Users
)/6

Attr_Dist_F_INfreq <- (
  (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
  (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,4,3])+   # Dist (A): COL More Freq. Users
  (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,6,3])+   # Dist (A): CZE More Freq. Users
  (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,8,3])+   # Dist (A): ZAF More Freq. Users
  (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,10,3])+   # Dist (A): TUR More Freq. Users
  (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
Attr_Asym_F_freq <- (
(post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
(post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,3,4])+   # Asym (A): COL More Freq. Users
(post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,5,4])+   # Asym (A): CZE More Freq. Users
(post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,7,4])+   # Asym (A): ZAF More Freq. Users
(post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,9,4])+   # Asym (A): TUR More Freq. Users
(post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,11,4])   # Asym (A): COL More Freq. Users
)/6

Attr_Asym_F_INfreq <- (
  (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,4,4])+   # Asym (A): COL More Freq. Users
    (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,6,4])+   # Asym (A): CZE More Freq. Users
    (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,8,4])+   # Asym (A): ZAF More Freq. Users
    (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,10,4])+   # Asym (A): TUR More Freq. Users
    (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
Attr_SexTyp_F_freq <- (
  (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,3,5])+   # SShD (A): COL More Freq. Users
    (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,5,5])+   # SShD (A): CZE More Freq. Users
    (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,7,5])+   # SShD (A): ZAF More Freq. Users
    (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,9,5])+   # SShD (A): TUR More Freq. Users
    (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,11,5])   # SShD (A): COL More Freq. Users
)/6

Attr_SexTyp_F_INfreq <- (
  (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,4,5])+   # SShD (A): COL More Freq. Users
    (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,6,5])+   # SShD (A): CZE More Freq. Users
    (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,8,5])+   # SShD (A): ZAF More Freq. Users
    (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,10,5])+   # SShD (A): TUR More Freq. Users
    (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
Attr_L_F_freq <- (
  (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,3,6])+   # L (A): COL More Freq. Users
    (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,5,6])+   # L (A): CZE More Freq. Users
    (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,7,6])+   # L (A): ZAF More Freq. Users
    (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,9,6])+   # L (A): TUR More Freq. Users
    (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,11,6])   # L (A): COL More Freq. Users
)/6

Attr_L_F_INfreq <- (
  (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,4,6])+   # L (A): COL More Freq. Users
    (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,6,6])+   # L (A): CZE More Freq. Users
    (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,8,6])+   # L (A): ZAF More Freq. Users
    (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,10,6])+   # L (A): TUR More Freq. Users
    (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,12,6])   # L (A): COL More Freq. Users
)/6



# GMM - SEPARATELY 

# Attractiveness - GMM 
str(post_ATD_F)
str(post_ATD_F$f_per_group_pr_A) # Mind that 1 is intercept! 

F_posts_ATD_MM_Attr <- list(
  #  AGE
  Age_Slope = (post_ATD_F$b_age_A),    # Slope prior to accessing culture
  
  Attr_Age_F_freq,
  Attr_Age_F_INfreq,
  
  Age_AUS_Freq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (post_ATD_F$b_dist_A),    # Dist (A): CZE Control
  
  Attr_Dist_F_freq,
  Attr_Dist_F_INfreq,
  
  Dist_AUS_Freq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (post_ATD_F$b_FA_A),    # Asym (A): CZE Control
  
  Attr_Asym_F_freq,
  Attr_Asym_F_INfreq,
  
  Asym_AUS_Freq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (post_ATD_F$b_sshd_A),    # SexTyp (A): CZE Control
  
  Attr_SexTyp_F_freq,
  Attr_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (post_ATD_F$b_L_A),    # L (A): CZE Control
  
  Attr_L_F_freq,
  Attr_L_F_INfreq,
  
  L_AUS_Freq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,6,6]),   # L (A): CZE Less Freq. Users

  L_ZAF_Freq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,8,6]),   # L (A): ZAF Less Freq. Users
    
  L_TUR_Freq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,12,6])   # L (A): COL Less Freq. Users
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



tiff("F_ATD_GMM_14_03.tiff",width=35,height=30,units="cm",res=600,compression="lzw")
layout(matrix(1:3,byrow=F,nrow=1), widths=c(1.7,1.2,1.2),heights=c(1))

par(mar=c(2.1, 14.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.5),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Attr: GMM - Females", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#b9b9B999", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#b9b9B999", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#b9b9B999", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#b9b9B999", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#b9b9B999", lwd = 2)


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

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,75),
                               Mean=rep(0,75),Up_CI=rep(0,75)) 

for (i in 1:length(F_posts_ATD_MM_Attr)) {
  Coefficients_check$Mean[i]<-mean(F_posts_ATD_MM_Attr[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(F_posts_ATD_MM_Attr)) {
  Coefficients_check$Low_CI[i]<-PI(F_posts_ATD_MM_Attr[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(F_posts_ATD_MM_Attr)) {
  Coefficients_check$Up_CI[i]<-PI(F_posts_ATD_MM_Attr[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))

cex_t <- 1.25
cex_t2 <- 1
pls <- 1

text("Mean", x=0.325, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.25, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=0.4, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  axis(2, at=ycoord, labels=textlab, srt=45, las=2, cex.axis=1, tick=F)
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=0.325, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=0.25, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=0.4, y=ys[i], cex=cex_t2)
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
  (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,3,2])+   # Age (A): COL More Freq. Users
    (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,5,2])+   # Age (A): CZE More Freq. Users
    (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,7,2])+   # Age (A): ZAF More Freq. Users
    (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,9,2])+   # Age (A): TUR More Freq. Users
    (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,11,2])   # Age (A): COL More Freq. Users
)/6

TR_Age_F_INfreq <- (
  (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,4,2])+   # Age (A): COL More Freq. Users
    (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,6,2])+   # Age (A): CZE More Freq. Users
    (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,8,2])+   # Age (A): ZAF More Freq. Users
    (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,10,2])+   # Age (A): TUR More Freq. Users
    (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
TR_Dist_F_freq <- (
  (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,3,3])+   # Dist (A): COL More Freq. Users
    (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,5,3])+   # Dist (A): CZE More Freq. Users
    (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,7,3])+   # Dist (A): ZAF More Freq. Users
    (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,9,3])+   # Dist (A): TUR More Freq. Users
    (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,11,3])   # Dist (A): COL More Freq. Users
)/6

TR_Dist_F_INfreq <- (
  (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,4,3])+   # Dist (A): COL More Freq. Users
    (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,6,3])+   # Dist (A): CZE More Freq. Users
    (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,8,3])+   # Dist (A): ZAF More Freq. Users
    (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,10,3])+   # Dist (A): TUR More Freq. Users
    (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
TR_Asym_F_freq <- (
  (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,3,4])+   # Asym (A): COL More Freq. Users
    (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,5,4])+   # Asym (A): CZE More Freq. Users
    (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,7,4])+   # Asym (A): ZAF More Freq. Users
    (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,9,4])+   # Asym (A): TUR More Freq. Users
    (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,11,4])   # Asym (A): COL More Freq. Users
)/6

TR_Asym_F_INfreq <- (
  (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,4,4])+   # Asym (A): COL More Freq. Users
    (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,6,4])+   # Asym (A): CZE More Freq. Users
    (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,8,4])+   # Asym (A): ZAF More Freq. Users
    (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,10,4])+   # Asym (A): TUR More Freq. Users
    (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
TR_SexTyp_F_freq <- (
  (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,3,5])+   # SShD (A): COL More Freq. Users
    (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,5,5])+   # SShD (A): CZE More Freq. Users
    (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,7,5])+   # SShD (A): ZAF More Freq. Users
    (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,9,5])+   # SShD (A): TUR More Freq. Users
    (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,11,5])   # SShD (A): COL More Freq. Users
)/6

TR_SexTyp_F_INfreq <- (
  (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,4,5])+   # SShD (A): COL More Freq. Users
    (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,6,5])+   # SShD (A): CZE More Freq. Users
    (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,8,5])+   # SShD (A): ZAF More Freq. Users
    (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,10,5])+   # SShD (A): TUR More Freq. Users
    (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
TR_L_F_freq <- (
  (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,3,6])+   # L (A): COL More Freq. Users
    (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,5,6])+   # L (A): CZE More Freq. Users
    (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,7,6])+   # L (A): ZAF More Freq. Users
    (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,9,6])+   # L (A): TUR More Freq. Users
    (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,11,6])   # L (A): COL More Freq. Users
)/6

TR_L_F_INfreq <- (
  (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,4,6])+   # L (A): COL More Freq. Users
    (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,6,6])+   # L (A): CZE More Freq. Users
    (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,8,6])+   # L (A): ZAF More Freq. Users
    (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,10,6])+   # L (A): TUR More Freq. Users
    (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,12,6])   # L (A): COL More Freq. Users
)/6



# Trusw - GMM 
str(post_ATD_F)
str(post_ATD_F$f_per_group_pr_T) # Mind that 1 is intercept! 

F_posts_ATD_MM_TRU <- list(
  #  AGE
  Age_Slope = (post_ATD_F$b_age_T),    # Slope prior to accessing culture
  
  TR_Age_F_freq,
  TR_Age_F_INfreq,
  
  Age_AUS_Freq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (post_ATD_F$b_dist_T),    # Dist (A): CZE Control
  
  TR_Dist_F_freq,
  TR_Dist_F_INfreq,
  
  Dist_AUS_Freq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (post_ATD_F$b_FA_T),    # Asym (A): CZE Control
  
  TR_Asym_F_freq,
  TR_Asym_F_INfreq,
  
  Asym_AUS_Freq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (post_ATD_F$b_sshd_T),    # SexTyp (A): CZE Control
  
  TR_SexTyp_F_freq,
  TR_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (post_ATD_F$b_L_T),    # L (A): CZE Control
  
  TR_L_F_freq,
  TR_L_F_INfreq,
  
  L_AUS_Freq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,12,6])   # L (A): COL Less Freq. Users
)


summary.data.frame(F_posts_ATD_MM_TRU)
str(F_posts_ATD_MM_TRU) # Need 65 rows 



par(mar=c(2.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.5),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Trustw: GMM - Females", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting


# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#b9b9B999", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#b9b9B999", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#b9b9B999", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#b9b9B999", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#b9b9B999", lwd = 2)


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

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,75),
                               Mean=rep(0,75),Up_CI=rep(0,75)) 

for (i in 1:length(F_posts_ATD_MM_TRU)) {
  Coefficients_check$Mean[i]<-mean(F_posts_ATD_MM_TRU[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(F_posts_ATD_MM_TRU)) {
  Coefficients_check$Low_CI[i]<-PI(F_posts_ATD_MM_TRU[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(F_posts_ATD_MM_TRU)) {
  Coefficients_check$Up_CI[i]<-PI(F_posts_ATD_MM_TRU[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))

cex_t <- 1.25
cex_t2 <- 1
pls <- 1

text("Mean", x=0.325, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.25, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=0.4, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=0.325, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=0.25, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=0.4, y=ys[i], cex=cex_t2)
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
  (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,3,2])+   # Age (A): COL More Freq. Users
    (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,5,2])+   # Age (A): CZE More Freq. Users
    (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,7,2])+   # Age (A): ZAF More Freq. Users
    (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,9,2])+   # Age (A): TUR More Freq. Users
    (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,11,2])   # Age (A): COL More Freq. Users
)/6

DOM_Age_F_INfreq <- (
  (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,4,2])+   # Age (A): COL More Freq. Users
    (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,6,2])+   # Age (A): CZE More Freq. Users
    (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,8,2])+   # Age (A): ZAF More Freq. Users
    (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,10,2])+   # Age (A): TUR More Freq. Users
    (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
DOM_Dist_F_freq <- (
  (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,3,3])+   # Dist (A): COL More Freq. Users
    (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,5,3])+   # Dist (A): CZE More Freq. Users
    (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,7,3])+   # Dist (A): ZAF More Freq. Users
    (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,9,3])+   # Dist (A): TUR More Freq. Users
    (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,11,3])   # Dist (A): COL More Freq. Users
)/6

DOM_Dist_F_INfreq <- (
  (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,4,3])+   # Dist (A): COL More Freq. Users
    (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,6,3])+   # Dist (A): CZE More Freq. Users
    (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,8,3])+   # Dist (A): ZAF More Freq. Users
    (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,10,3])+   # Dist (A): TUR More Freq. Users
    (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
DOM_Asym_F_freq <- (
  (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,3,4])+   # Asym (A): COL More Freq. Users
    (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,5,4])+   # Asym (A): CZE More Freq. Users
    (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,7,4])+   # Asym (A): ZAF More Freq. Users
    (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,9,4])+   # Asym (A): TUR More Freq. Users
    (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,11,4])   # Asym (A): COL More Freq. Users
)/6

DOM_Asym_F_INfreq <- (
  (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,4,4])+   # Asym (A): COL More Freq. Users
    (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,6,4])+   # Asym (A): CZE More Freq. Users
    (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,8,4])+   # Asym (A): ZAF More Freq. Users
    (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,10,4])+   # Asym (A): TUR More Freq. Users
    (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
DOM_SexTyp_F_freq <- (
  (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,3,5])+   # SShD (A): COL More Freq. Users
    (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,5,5])+   # SShD (A): CZE More Freq. Users
    (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,7,5])+   # SShD (A): ZAF More Freq. Users
    (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,9,5])+   # SShD (A): TUR More Freq. Users
    (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,11,5])   # SShD (A): COL More Freq. Users
)/6

DOM_SexTyp_F_INfreq <- (
  (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,4,5])+   # SShD (A): COL More Freq. Users
    (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,6,5])+   # SShD (A): CZE More Freq. Users
    (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,8,5])+   # SShD (A): ZAF More Freq. Users
    (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,10,5])+   # SShD (A): TUR More Freq. Users
    (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
DOM_L_F_freq <- (
  (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,3,6])+   # L (A): COL More Freq. Users
    (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,5,6])+   # L (A): CZE More Freq. Users
    (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,7,6])+   # L (A): ZAF More Freq. Users
    (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,9,6])+   # L (A): TUR More Freq. Users
    (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,11,6])   # L (A): COL More Freq. Users
)/6

DOM_L_F_INfreq <- (
  (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,4,6])+   # L (A): COL More Freq. Users
    (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,6,6])+   # L (A): CZE More Freq. Users
    (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,8,6])+   # L (A): ZAF More Freq. Users
    (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,10,6])+   # L (A): TUR More Freq. Users
    (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,12,6])   # L (A): COL More Freq. Users
)/6



# GMM - SEPARATELY 

# Dominance - GMM 
str(post_ATD_F)
str(post_ATD_F$f_per_group_pr_D) # Mind that 1 is intercept! 

F_posts_ATD_MM_DOM <- list(
  #  AGE
  Age_Slope = (post_ATD_F$b_age_D),    # Slope prior to accessing culture
  
  DOM_Age_F_freq,
  DOM_Age_F_INfreq,
  
  Age_AUS_Freq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (post_ATD_F$b_dist_D),    # Dist (A): CZE Control
  
  DOM_Dist_F_freq,
  DOM_Dist_F_INfreq,
  
  Dist_AUS_Freq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (post_ATD_F$b_FA_D),    # Asym (A): CZE Control
  
  DOM_Asym_F_freq,
  DOM_Asym_F_INfreq,
  
  Asym_AUS_Freq = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_D[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (post_ATD_F$b_sshd_T),    # SexTyp (A): CZE Control
  
  DOM_SexTyp_F_freq,
  DOM_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (post_ATD_F$b_L_D),    # L (A): CZE Control
  
  DOM_L_F_freq,
  DOM_L_F_INfreq,
  
  L_AUS_Freq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,12,6])   # L (A): COL Less Freq. Users
)


summary.data.frame(F_posts_ATD_MM_DOM)
str(F_posts_ATD_MM_TRU) # Need 65 rows 



par(mar=c(2.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.5),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Dominance: GMM - Females", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting


# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#b9b9B999", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#b9b9B999", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#b9b9B999", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#b9b9B999", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#b9b9B999", lwd = 2)


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

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,75),
                               Mean=rep(0,75),Up_CI=rep(0,75)) 

for (i in 1:length(F_posts_ATD_MM_DOM)) {
  Coefficients_check$Mean[i]<-mean(F_posts_ATD_MM_DOM[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(F_posts_ATD_MM_DOM)) {
  Coefficients_check$Low_CI[i]<-PI(F_posts_ATD_MM_DOM[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(F_posts_ATD_MM_DOM)) {
  Coefficients_check$Up_CI[i]<-PI(F_posts_ATD_MM_DOM[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))

cex_t <- 1.25
cex_t2 <- 1
pls <- 1

text("Mean", x=0.325, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.25, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=0.4, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=0.325, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=0.25, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=0.4, y=ys[i], cex=cex_t2)
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=5, lty=1, col=cols[i])
  segments(x0 = ci[1], y0 = ycoord-0.1, x1 = ci[1], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  segments(x0 = ci[2], y0 = ycoord-0.1, x1 = ci[2], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0=0.0, y0=c(0.0, 16, 31.5, 47, 62.5), x1=0.0, y1=c(15.5, 31, 46.5, 62, 77.5), col="#999999", lwd=2, lty=1)
# 


dev.off()


# GMM - without coefs printed
# GMM - without coefs printed
# GMM - without coefs printed
# GMM - without coefs printed
# GMM - without coefs printed
# GMM - without coefs printed
# GMM - without coefs printed
# GMM - without coefs printed
# GMM - without coefs printed
# GMM - without coefs printed



# Preparing composite coefficients: 
# AGE:
Attr_Age_F_freq <- c(
  (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,3,2])+   # Age (A): COL More Freq. Users
    (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,5,2])+   # Age (A): CZE More Freq. Users
    (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,7,2])+   # Age (A): ZAF More Freq. Users
    (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,9,2])+   # Age (A): TUR More Freq. Users
    (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,11,2])   # Age (A): COL More Freq. Users
)/6

Attr_Age_F_INfreq <- (
  (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,4,2])+   # Age (A): COL More Freq. Users
    (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,6,2])+   # Age (A): CZE More Freq. Users
    (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,8,2])+   # Age (A): ZAF More Freq. Users
    (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,10,2])+   # Age (A): TUR More Freq. Users
    (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
Attr_Dist_F_freq <- (
  (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,3,3])+   # Dist (A): COL More Freq. Users
    (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,5,3])+   # Dist (A): CZE More Freq. Users
    (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,7,3])+   # Dist (A): ZAF More Freq. Users
    (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,9,3])+   # Dist (A): TUR More Freq. Users
    (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,11,3])   # Dist (A): COL More Freq. Users
)/6

Attr_Dist_F_INfreq <- (
  (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,4,3])+   # Dist (A): COL More Freq. Users
    (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,6,3])+   # Dist (A): CZE More Freq. Users
    (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,8,3])+   # Dist (A): ZAF More Freq. Users
    (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,10,3])+   # Dist (A): TUR More Freq. Users
    (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
Attr_Asym_F_freq <- (
  (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,3,4])+   # Asym (A): COL More Freq. Users
    (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,5,4])+   # Asym (A): CZE More Freq. Users
    (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,7,4])+   # Asym (A): ZAF More Freq. Users
    (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,9,4])+   # Asym (A): TUR More Freq. Users
    (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,11,4])   # Asym (A): COL More Freq. Users
)/6

Attr_Asym_F_INfreq <- (
  (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,4,4])+   # Asym (A): COL More Freq. Users
    (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,6,4])+   # Asym (A): CZE More Freq. Users
    (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,8,4])+   # Asym (A): ZAF More Freq. Users
    (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,10,4])+   # Asym (A): TUR More Freq. Users
    (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
Attr_SexTyp_F_freq <- (
  (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,3,5])+   # SShD (A): COL More Freq. Users
    (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,5,5])+   # SShD (A): CZE More Freq. Users
    (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,7,5])+   # SShD (A): ZAF More Freq. Users
    (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,9,5])+   # SShD (A): TUR More Freq. Users
    (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,11,5])   # SShD (A): COL More Freq. Users
)/6

Attr_SexTyp_F_INfreq <- (
  (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,4,5])+   # SShD (A): COL More Freq. Users
    (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,6,5])+   # SShD (A): CZE More Freq. Users
    (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,8,5])+   # SShD (A): ZAF More Freq. Users
    (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,10,5])+   # SShD (A): TUR More Freq. Users
    (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
Attr_L_F_freq <- (
  (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,3,6])+   # L (A): COL More Freq. Users
    (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,5,6])+   # L (A): CZE More Freq. Users
    (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,7,6])+   # L (A): ZAF More Freq. Users
    (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,9,6])+   # L (A): TUR More Freq. Users
    (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,11,6])   # L (A): COL More Freq. Users
)/6

Attr_L_F_INfreq <- (
  (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,4,6])+   # L (A): COL More Freq. Users
    (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,6,6])+   # L (A): CZE More Freq. Users
    (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,8,6])+   # L (A): ZAF More Freq. Users
    (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,10,6])+   # L (A): TUR More Freq. Users
    (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,12,6])   # L (A): COL More Freq. Users
)/6



# GMM - SEPARATELY 

# Attractiveness - GMM 
str(post_ATD_F)
str(post_ATD_F$f_per_group_pr_A) # Mind that 1 is intercept! 

F_posts_ATD_MM_Attr <- list(
  #  AGE
  Age_Slope = (post_ATD_F$b_age_A),    # Slope prior to accessing culture
  
  Attr_Age_F_freq,
  Attr_Age_F_INfreq,
  
  Age_AUS_Freq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (post_ATD_F$b_age_A + post_ATD_F$f_per_group_pr_A[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (post_ATD_F$b_dist_A),    # Dist (A): CZE Control
  
  Attr_Dist_F_freq,
  Attr_Dist_F_INfreq,
  
  Dist_AUS_Freq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (post_ATD_F$b_dist_A + post_ATD_F$f_per_group_pr_A[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (post_ATD_F$b_FA_A),    # Asym (A): CZE Control
  
  Attr_Asym_F_freq,
  Attr_Asym_F_INfreq,
  
  Asym_AUS_Freq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (post_ATD_F$b_FA_A + post_ATD_F$f_per_group_pr_A[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (post_ATD_F$b_sshd_A),    # SexTyp (A): CZE Control
  
  Attr_SexTyp_F_freq,
  Attr_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (post_ATD_F$b_sshd_A + post_ATD_F$f_per_group_pr_A[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (post_ATD_F$b_L_A),    # L (A): CZE Control
  
  Attr_L_F_freq,
  Attr_L_F_INfreq,
  
  L_AUS_Freq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (post_ATD_F$b_L_A + post_ATD_F$f_per_group_pr_A[,12,6])   # L (A): COL Less Freq. Users
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



tiff("F_ATD_GMM_14_03_no_COEFS.tiff",width=30,height=33,units="cm",res=600,compression="lzw")
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
  (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,3,2])+   # Age (A): COL More Freq. Users
    (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,5,2])+   # Age (A): CZE More Freq. Users
    (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,7,2])+   # Age (A): ZAF More Freq. Users
    (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,9,2])+   # Age (A): TUR More Freq. Users
    (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,11,2])   # Age (A): COL More Freq. Users
)/6

TR_Age_F_INfreq <- (
  (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,4,2])+   # Age (A): COL More Freq. Users
    (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,6,2])+   # Age (A): CZE More Freq. Users
    (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,8,2])+   # Age (A): ZAF More Freq. Users
    (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,10,2])+   # Age (A): TUR More Freq. Users
    (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
TR_Dist_F_freq <- (
  (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,3,3])+   # Dist (A): COL More Freq. Users
    (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,5,3])+   # Dist (A): CZE More Freq. Users
    (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,7,3])+   # Dist (A): ZAF More Freq. Users
    (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,9,3])+   # Dist (A): TUR More Freq. Users
    (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,11,3])   # Dist (A): COL More Freq. Users
)/6

TR_Dist_F_INfreq <- (
  (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,4,3])+   # Dist (A): COL More Freq. Users
    (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,6,3])+   # Dist (A): CZE More Freq. Users
    (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,8,3])+   # Dist (A): ZAF More Freq. Users
    (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,10,3])+   # Dist (A): TUR More Freq. Users
    (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
TR_Asym_F_freq <- (
  (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,3,4])+   # Asym (A): COL More Freq. Users
    (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,5,4])+   # Asym (A): CZE More Freq. Users
    (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,7,4])+   # Asym (A): ZAF More Freq. Users
    (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,9,4])+   # Asym (A): TUR More Freq. Users
    (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,11,4])   # Asym (A): COL More Freq. Users
)/6

TR_Asym_F_INfreq <- (
  (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,4,4])+   # Asym (A): COL More Freq. Users
    (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,6,4])+   # Asym (A): CZE More Freq. Users
    (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,8,4])+   # Asym (A): ZAF More Freq. Users
    (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,10,4])+   # Asym (A): TUR More Freq. Users
    (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
TR_SexTyp_F_freq <- (
  (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,3,5])+   # SShD (A): COL More Freq. Users
    (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,5,5])+   # SShD (A): CZE More Freq. Users
    (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,7,5])+   # SShD (A): ZAF More Freq. Users
    (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,9,5])+   # SShD (A): TUR More Freq. Users
    (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,11,5])   # SShD (A): COL More Freq. Users
)/6

TR_SexTyp_F_INfreq <- (
  (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,4,5])+   # SShD (A): COL More Freq. Users
    (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,6,5])+   # SShD (A): CZE More Freq. Users
    (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,8,5])+   # SShD (A): ZAF More Freq. Users
    (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,10,5])+   # SShD (A): TUR More Freq. Users
    (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
TR_L_F_freq <- (
  (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,3,6])+   # L (A): COL More Freq. Users
    (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,5,6])+   # L (A): CZE More Freq. Users
    (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,7,6])+   # L (A): ZAF More Freq. Users
    (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,9,6])+   # L (A): TUR More Freq. Users
    (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,11,6])   # L (A): COL More Freq. Users
)/6

TR_L_F_INfreq <- (
  (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,4,6])+   # L (A): COL More Freq. Users
    (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,6,6])+   # L (A): CZE More Freq. Users
    (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,8,6])+   # L (A): ZAF More Freq. Users
    (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,10,6])+   # L (A): TUR More Freq. Users
    (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,12,6])   # L (A): COL More Freq. Users
)/6



# Trusw - GMM 
str(post_ATD_F)
str(post_ATD_F$f_per_group_pr_T) # Mind that 1 is intercept! 

F_posts_ATD_MM_TRU <- list(
  #  AGE
  Age_Slope = (post_ATD_F$b_age_T),    # Slope prior to accessing culture
  
  TR_Age_F_freq,
  TR_Age_F_INfreq,
  
  Age_AUS_Freq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (post_ATD_F$b_age_T + post_ATD_F$f_per_group_pr_T[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (post_ATD_F$b_dist_T),    # Dist (A): CZE Control
  
  TR_Dist_F_freq,
  TR_Dist_F_INfreq,
  
  Dist_AUS_Freq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (post_ATD_F$b_dist_T + post_ATD_F$f_per_group_pr_T[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (post_ATD_F$b_FA_T),    # Asym (A): CZE Control
  
  TR_Asym_F_freq,
  TR_Asym_F_INfreq,
  
  Asym_AUS_Freq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_T[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (post_ATD_F$b_sshd_T),    # SexTyp (A): CZE Control
  
  TR_SexTyp_F_freq,
  TR_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (post_ATD_F$b_sshd_T + post_ATD_F$f_per_group_pr_T[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (post_ATD_F$b_L_T),    # L (A): CZE Control
  
  TR_L_F_freq,
  TR_L_F_INfreq,
  
  L_AUS_Freq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (post_ATD_F$b_L_T + post_ATD_F$f_per_group_pr_T[,12,6])   # L (A): COL Less Freq. Users
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
  (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,3,2])+   # Age (A): COL More Freq. Users
    (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,5,2])+   # Age (A): CZE More Freq. Users
    (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,7,2])+   # Age (A): ZAF More Freq. Users
    (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,9,2])+   # Age (A): TUR More Freq. Users
    (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,11,2])   # Age (A): COL More Freq. Users
)/6

DOM_Age_F_INfreq <- (
  (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,4,2])+   # Age (A): COL More Freq. Users
    (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,6,2])+   # Age (A): CZE More Freq. Users
    (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,8,2])+   # Age (A): ZAF More Freq. Users
    (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,10,2])+   # Age (A): TUR More Freq. Users
    (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
DOM_Dist_F_freq <- (
  (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,3,3])+   # Dist (A): COL More Freq. Users
    (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,5,3])+   # Dist (A): CZE More Freq. Users
    (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,7,3])+   # Dist (A): ZAF More Freq. Users
    (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,9,3])+   # Dist (A): TUR More Freq. Users
    (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,11,3])   # Dist (A): COL More Freq. Users
)/6

DOM_Dist_F_INfreq <- (
  (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,4,3])+   # Dist (A): COL More Freq. Users
    (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,6,3])+   # Dist (A): CZE More Freq. Users
    (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,8,3])+   # Dist (A): ZAF More Freq. Users
    (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,10,3])+   # Dist (A): TUR More Freq. Users
    (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
DOM_Asym_F_freq <- (
  (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,3,4])+   # Asym (A): COL More Freq. Users
    (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,5,4])+   # Asym (A): CZE More Freq. Users
    (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,7,4])+   # Asym (A): ZAF More Freq. Users
    (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,9,4])+   # Asym (A): TUR More Freq. Users
    (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,11,4])   # Asym (A): COL More Freq. Users
)/6

DOM_Asym_F_INfreq <- (
  (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,4,4])+   # Asym (A): COL More Freq. Users
    (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,6,4])+   # Asym (A): CZE More Freq. Users
    (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,8,4])+   # Asym (A): ZAF More Freq. Users
    (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,10,4])+   # Asym (A): TUR More Freq. Users
    (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
DOM_SexTyp_F_freq <- (
  (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,3,5])+   # SShD (A): COL More Freq. Users
    (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,5,5])+   # SShD (A): CZE More Freq. Users
    (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,7,5])+   # SShD (A): ZAF More Freq. Users
    (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,9,5])+   # SShD (A): TUR More Freq. Users
    (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,11,5])   # SShD (A): COL More Freq. Users
)/6

DOM_SexTyp_F_INfreq <- (
  (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,4,5])+   # SShD (A): COL More Freq. Users
    (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,6,5])+   # SShD (A): CZE More Freq. Users
    (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,8,5])+   # SShD (A): ZAF More Freq. Users
    (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,10,5])+   # SShD (A): TUR More Freq. Users
    (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
DOM_L_F_freq <- (
  (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,3,6])+   # L (A): COL More Freq. Users
    (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,5,6])+   # L (A): CZE More Freq. Users
    (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,7,6])+   # L (A): ZAF More Freq. Users
    (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,9,6])+   # L (A): TUR More Freq. Users
    (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,11,6])   # L (A): COL More Freq. Users
)/6

DOM_L_F_INfreq <- (
  (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,4,6])+   # L (A): COL More Freq. Users
    (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,6,6])+   # L (A): CZE More Freq. Users
    (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,8,6])+   # L (A): ZAF More Freq. Users
    (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,10,6])+   # L (A): TUR More Freq. Users
    (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,12,6])   # L (A): COL More Freq. Users
)/6



# GMM - SEPARATELY 

# Dominance - GMM 
str(post_ATD_F)
str(post_ATD_F$f_per_group_pr_D) # Mind that 1 is intercept! 

F_posts_ATD_MM_DOM <- list(
  #  AGE
  Age_Slope = (post_ATD_F$b_age_D),    # Slope prior to accessing culture
  
  DOM_Age_F_freq,
  DOM_Age_F_INfreq,
  
  Age_AUS_Freq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (post_ATD_F$b_age_D + post_ATD_F$f_per_group_pr_D[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (post_ATD_F$b_dist_D),    # Dist (A): CZE Control
  
  DOM_Dist_F_freq,
  DOM_Dist_F_INfreq,
  
  Dist_AUS_Freq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (post_ATD_F$b_dist_D + post_ATD_F$f_per_group_pr_D[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (post_ATD_F$b_FA_D),    # Asym (A): CZE Control
  
  DOM_Asym_F_freq,
  DOM_Asym_F_INfreq,
  
  Asym_AUS_Freq = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (post_ATD_F$b_FA_T + post_ATD_F$f_per_group_pr_D[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (post_ATD_F$b_FA_D + post_ATD_F$f_per_group_pr_D[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (post_ATD_F$b_sshd_T),    # SexTyp (A): CZE Control
  
  DOM_SexTyp_F_freq,
  DOM_SexTyp_F_INfreq,
  
  SexTyp_AUS_Freq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (post_ATD_F$b_sshd_D + post_ATD_F$f_per_group_pr_D[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (post_ATD_F$b_L_D),    # L (A): CZE Control
  
  DOM_L_F_freq,
  DOM_L_F_INfreq,
  
  L_AUS_Freq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (post_ATD_F$b_L_D + post_ATD_F$f_per_group_pr_D[,12,6])   # L (A): COL Less Freq. Users
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


# MEN 
# MEN
# MEN 
# MEN 
# MEN
# MEN 
# MEN 
# MEN
# MEN 
# MEN 
# MEN
# MEN 
# MEN 
# MEN
# MEN 
# MEN 
# MEN
# MEN 
# MEN 
# MEN
# MEN 
# MEN 
# MEN
# MEN 
# MEN 
# MEN
# MEN 
# MEN 
# MEN
# MEN 






str(post_ATD_M)

# Correlations 
labels_x <- c("AUS/NZE Freq", "AUS/NZE Infreq", "COL Freq","COL Infreq","CZE Freq", "CZE Infreq","ZAF Freq", "ZAF Infreq",
              "TUR Freq", "TUR Infreq", "VNM Freq", "VNM Infreq")
labels_y <- labels_x

str(post_ATD_M$Rho_FSMUi_A)

library(qgraph)
library(bootnet)
library(beepr)
library(ComplexHeatmap)
library(circlize)
library(grid)


# Make it as a 3×2 Matrix
#  ATTRACTIVENESS:M

cor_table_mean <- apply(post_ATD_M$Rho_FSMUi_A,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(post_ATD_M$Rho_FSMUi_A,c(2,3),PI, prob=0.95)
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
                else if (j<i){c(grid.text(sprintf("%.3f", cor_table_LOWCI[i, j]), x, y, gp = gpar(fontsize = font1),just = c(0.95,-0.33)),
                                grid.text(sprintf("%.3f", cor_table_UPWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.05,0.92))
                )}
              },rect_gp = gpar(col = "white", lwd = 1), row_title = "95% Compatibility Interval", row_title_gp = gpar(fontsize = 22))


# TRUSTWORTHINESS: M

cor_table_mean <- apply(post_ATD_M$Rho_FSMUi_T,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(post_ATD_M$Rho_FSMUi_T,c(2,3),PI, prob=0.95)
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

cor_table_mean <- apply(post_ATD_M$Rho_FSMUi_D,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(post_ATD_M$Rho_FSMUi_D,c(2,3),PI, prob=0.95)
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

tiff("Correlations_Heatmap_ATD_Males_Attr_wide.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AF
dev.off()

tiff("Correlations_Heatmap_ATD_Males_Tru_wide.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AT
dev.off()

tiff("Correlations_Heatmap_ATD_Males_Dom_Wide.tiff",width=57,height=25,units="cm",res=600,compression="lzw")
AD
dev.off()






# Upper part of Figure 4 - mean ratings per groups: 

# Frequent - all
# Compute the average of all group estimates
Avg_Freq_U_A = ( 
  post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,1,1] + 
    post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,3,1] + 
    post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,5,1] + 
    post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,7,1] + 
    post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,9,1] + 
    post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_A = ( 
  post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,2,1] + 
    post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,4,1] + 
    post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,6,1] + 
    post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,8,1] + 
    post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,10,1] + 
    post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,12,1] 
) / 6

M_posts_ATR <- list(
  # 1 Intercept: 
  Intercept = post_ATD_M$aA,
  # Infrequent users
  Avg_Freq_U_A,
  # Frequent users 
  Avg_Infreq_U_A,
  # Difference 
  Difference = Avg_Freq_U_A - Avg_Infreq_U_A,
  # 4 Australia - above - median users  
  AUS_Intensive_User = post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = post_ATD_M$aA + post_ATD_M$f_per_group_pr_A[,12,1]
)

summary.data.frame(M_posts_ATR)
str(M_posts_ATR) # Need 13 rows 

#--------------

# Trustworthiness
Avg_Freq_U_T = ( 
  post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,1,1] + 
    post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,3,1] + 
    post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,5,1] + 
    post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,7,1] + 
    post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,9,1] + 
    post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_T = ( 
  post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,2,1] + 
    post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,4,1] + 
    post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,6,1] + 
    post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,8,1] + 
    post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,10,1] + 
    post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,12,1] 
) / 6

M_posts_TRU <- list(
  # 1 Intercept: 
  Intercept = post_ATD_M$aT,
  # Infrequent users
  Avg_Freq_U_T,
  # Frequent users 
  Avg_Infreq_U_T,
  # Difference 
  Difference = Avg_Freq_U_T - Avg_Infreq_U_T,
  # 4 Australia - above - median users  
  AUS_Intensive_User = post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = post_ATD_M$aT + post_ATD_M$f_per_group_pr_T[,12,1]
)

summary.data.frame(M_posts_TRU)
str(M_posts_TRU) # Need 13 rows 

#--------------

# Dominance
Avg_Freq_U_D = ( 
  post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,1,1] + 
    post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,3,1] + 
    post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,5,1] + 
    post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,7,1] + 
    post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,9,1] + 
    post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,11,1] 
) / 6

# Infrequent - all 
Avg_Infreq_U_D = ( 
  post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,2,1] + 
    post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,4,1] + 
    post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,6,1] + 
    post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,8,1] + 
    post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,10,1] + 
    post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,12,1] 
) / 6

M_posts_DOM <- list(
  # 1 Intercept: 
  Intercept = post_ATD_M$aD,
  # Infrequent users
  Avg_Freq_U_D,
  # Frequent users 
  Avg_Infreq_U_D,
  # Difference 
  Difference = Avg_Freq_U_D - Avg_Infreq_U_D,
  # 4 Australia - above - median users  
  AUS_Intensive_User = post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,1,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,2,1],
  # Colombia - above - median users 
  COL_Intensive_User = post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,3,1], 
  # COlombia - below - median users
  COL_Less_Intensive = post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,4,1],
  # 2 CZECH - above - median users
  CZ_Intensive_Users = post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,5,1],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,6,1],
  # RSA - above - median users 
  RSA_Intenstive_User = post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,7,1],
  # RSA - below - median users 
  RSA_Less_Intensive = post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,8,1],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,9,1],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,10,1],
  # Vietnam - above - median users 
  VN_Intensive_User = post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,11,1],
  # Vietnam - below - median users 
  VN_Less_Intensive = post_ATD_M$aD + post_ATD_M$f_per_group_pr_D[,12,1]
)

summary.data.frame(M_posts_DOM)
str(M_posts_DOM) 



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


tiff("M_ATD_model_LEVELS.tiff",width=34,height=14,units="cm",res=600,compression="lzw")
layout(matrix(1:3,byrow=F,nrow=1), widths=c(1.8,1.2,1.2),heights=c(1))


# ATTRACTIVENESS

par(mar=c(3.1, 15.1, 1.1, 2.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-1,1),ylim=c(1,17),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Attractiveness - Group Estimates", side = 3, line = -0.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

rect(-1,0.5,1.5,17, col=F, border = "#b9b9B999", lwd = 2)
rect(-0.95,12.5,0.5,16.5, col="#F0F0F0", border = F)


segments(x0 = -1, y0 = 1:16, x1 = 0.5, y1 = 1:16, col = "#40808080", lwd = 1.5)
segments(x0 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y0 = 0.5, x1 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y1 = 16.5, col="#80808080",lty=2)

ys<-seq(from=1,to=16, by=1)
ys<-rev(ys)

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,16),
                               Mean=rep(0,16),Up_CI=rep(0,16)) 

for (i in 1:length(M_posts_ATR)) {
  Coefficients_check$Mean[i]<-mean(M_posts_ATR[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(M_posts_ATR)) {
  Coefficients_check$Low_CI[i]<-PI(M_posts_ATR[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(M_posts_ATR)) {
  Coefficients_check$Up_CI[i]<-PI(M_posts_ATR[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))

cex_t <- 1.25
cex_t2 <- 0.95
pls <- 0.75

text("Mean", x=0.8, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.6, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=1.0, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


for(i in 1:length(M_posts_ATR)){ 
  toplot<-M_posts_ATR[[i]]
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
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=0.8, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=0.6, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=1.0, y=ys[i], cex=cex_t2)
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
plot(NULL,xlim=c(-1,1),ylim=c(1,17),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Trustworthiness - Group Estimates", side = 3, line = -0.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

rect(-1,0.5,1.5,17, col=F, border = "#b9b9B999", lwd = 2)
rect(-0.95,12.5,0.5,16.5, col="#F0F0F0", border = F)


segments(x0 = -1, y0 = 1:16, x1 = 0.5, y1 = 1:16, col = "#40808080", lwd = 1.5)
segments(x0 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y0 = 0.5, x1 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y1 = 16.5, col="#80808080",lty=2)

ys<-seq(from=1,to=16, by=1)
ys<-rev(ys)

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,16),
                               Mean=rep(0,16),Up_CI=rep(0,16)) 

for (i in 1:length(M_posts_TRU)) {
  Coefficients_check$Mean[i]<-mean(M_posts_TRU[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(M_posts_TRU)) {
  Coefficients_check$Low_CI[i]<-PI(M_posts_TRU[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(M_posts_TRU)) {
  Coefficients_check$Up_CI[i]<-PI(M_posts_TRU[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))


text("Mean", x=0.8, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.6, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=1.0, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


for(i in 1:length(M_posts_TRU)){ 
  toplot<-M_posts_TRU[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=0.8, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=0.6, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=1.0, y=ys[i], cex=cex_t2)
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
plot(NULL,xlim=c(-1,1),ylim=c(1,17),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Dominance - Group Estimates", side = 3, line = -0.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

rect(-1,0.5,1.5,17, col=F, border = "#b9b9B999", lwd = 2)
rect(-0.95,12.5,0.5,16.5, col="#F0F0F0", border = F)


segments(x0 = -1, y0 = 1:16, x1 = 0.5, y1 = 1:16, col = "#40808080", lwd = 1.5)
segments(x0 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y0 = 0.5, x1 = c(-0.75,-0.5,-0.25,0,0.25, 0.5), y1 = 16.5, col="#80808080",lty=2)

ys<-seq(from=1,to=16, by=1)
ys<-rev(ys)

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,16),
                               Mean=rep(0,16),Up_CI=rep(0,16)) 

for (i in 1:length(M_posts_DOM)) {
  Coefficients_check$Mean[i]<-mean(M_posts_DOM[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(M_posts_DOM)) {
  Coefficients_check$Low_CI[i]<-PI(M_posts_DOM[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(M_posts_DOM)) {
  Coefficients_check$Up_CI[i]<-PI(M_posts_DOM[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))


text("Mean", x=0.8, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.6, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=1.0, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


for(i in 1:length(M_posts_DOM)){ 
  toplot<-M_posts_DOM[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=0.8, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=0.6, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=1.0, y=ys[i], cex=cex_t2)
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



# DTTO without the labels: 
# DTTO without the labels: 
# DTTO without the labels: 
# DTTO without the labels: 
# DTTO without the labels: 
# DTTO without the labels: 




tiff("M_ATD_model_LEVELS_NOLABELS.tiff",width=30,height=14,units="cm",res=600,compression="lzw")
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


for(i in 1:length(M_posts_ATR)){ 
  toplot<-M_posts_ATR[[i]]
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

for(i in 1:length(M_posts_TRU)){ 
  toplot<-M_posts_TRU[[i]]
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


for(i in 1:length(M_posts_DOM)){ 
  toplot<-M_posts_DOM[[i]]
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



# And now GMM: 
# Preparing composite coefficients: 

# Attractiveness


# AGE:
Attr_Age_M_freq <- c(
  (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,3,2])+   # Age (A): COL More Freq. Users
    (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,5,2])+   # Age (A): CZE More Freq. Users
    (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,7,2])+   # Age (A): ZAF More Freq. Users
    (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,9,2])+   # Age (A): TUR More Freq. Users
    (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,11,2])   # Age (A): COL More Freq. Users
)/6

Attr_Age_M_INfreq <- (
  (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,4,2])+   # Age (A): COL More Freq. Users
    (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,6,2])+   # Age (A): CZE More Freq. Users
    (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,8,2])+   # Age (A): ZAF More Freq. Users
    (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,10,2])+   # Age (A): TUR More Freq. Users
    (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
Attr_Dist_M_freq <- (
  (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,3,3])+   # Dist (A): COL More Freq. Users
    (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,5,3])+   # Dist (A): CZE More Freq. Users
    (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,7,3])+   # Dist (A): ZAF More Freq. Users
    (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,9,3])+   # Dist (A): TUR More Freq. Users
    (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,11,3])   # Dist (A): COL More Freq. Users
)/6

Attr_Dist_M_INfreq <- (
  (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,4,3])+   # Dist (A): COL More Freq. Users
    (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,6,3])+   # Dist (A): CZE More Freq. Users
    (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,8,3])+   # Dist (A): ZAF More Freq. Users
    (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,10,3])+   # Dist (A): TUR More Freq. Users
    (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
Attr_Asym_M_freq <- (
  (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,3,4])+   # Asym (A): COL More Freq. Users
    (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,5,4])+   # Asym (A): CZE More Freq. Users
    (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,7,4])+   # Asym (A): ZAF More Freq. Users
    (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,9,4])+   # Asym (A): TUR More Freq. Users
    (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,11,4])   # Asym (A): COL More Freq. Users
)/6

Attr_Asym_M_INfreq <- (
  (post_ATD_M$b_FA_A + post_ATD_F$f_per_group_pr_A[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,4,4])+   # Asym (A): COL More Freq. Users
    (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,6,4])+   # Asym (A): CZE More Freq. Users
    (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,8,4])+   # Asym (A): ZAF More Freq. Users
    (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,10,4])+   # Asym (A): TUR More Freq. Users
    (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
Attr_SexTyp_M_freq <- (
  (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,3,5])+   # SShD (A): COL More Freq. Users
    (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,5,5])+   # SShD (A): CZE More Freq. Users
    (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,7,5])+   # SShD (A): ZAF More Freq. Users
    (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,9,5])+   # SShD (A): TUR More Freq. Users
    (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,11,5])   # SShD (A): COL More Freq. Users
)/6

Attr_SexTyp_M_INfreq <- (
  (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,4,5])+   # SShD (A): COL More Freq. Users
    (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,6,5])+   # SShD (A): CZE More Freq. Users
    (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,8,5])+   # SShD (A): ZAF More Freq. Users
    (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,10,5])+   # SShD (A): TUR More Freq. Users
    (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
Attr_L_M_freq <- (
  (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,3,6])+   # L (A): COL More Freq. Users
    (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,5,6])+   # L (A): CZE More Freq. Users
    (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,7,6])+   # L (A): ZAF More Freq. Users
    (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,9,6])+   # L (A): TUR More Freq. Users
    (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,11,6])   # L (A): COL More Freq. Users
)/6

Attr_L_M_INfreq <- (
  (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,4,6])+   # L (A): COL More Freq. Users
    (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,6,6])+   # L (A): CZE More Freq. Users
    (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,8,6])+   # L (A): ZAF More Freq. Users
    (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,10,6])+   # L (A): TUR More Freq. Users
    (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,12,6])   # L (A): COL More Freq. Users
)/6


# GMM - SEPARATELY 
M_posts_ATD_MM_Attr <- list(
  #  AGE
  Age_Slope = (post_ATD_M$b_age_A),    # Slope prior to accessing culture
  
  Attr_Age_M_freq,
  Attr_Age_M_INfreq,
  
  Age_AUS_Freq = (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (post_ATD_M$b_age_A + post_ATD_M$f_per_group_pr_A[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (post_ATD_M$b_dist_A),    # Dist (A): CZE Control
  
  Attr_Dist_M_freq,
  Attr_Dist_M_INfreq,
  
  Dist_AUS_Freq = (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (post_ATD_M$b_dist_A + post_ATD_M$f_per_group_pr_A[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (post_ATD_M$b_FA_A),    # Asym (A): CZE Control
  
  Attr_Asym_M_freq,
  Attr_Asym_M_INfreq,
  
  Asym_AUS_Freq = (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (post_ATD_M$b_FA_A + post_ATD_M$f_per_group_pr_A[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (post_ATD_M$b_sshd_A),    # SexTyp (A): CZE Control
  
  Attr_SexTyp_M_freq,
  Attr_SexTyp_M_INfreq,
  
  SexTyp_AUS_Freq = (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (post_ATD_M$b_sshd_A + post_ATD_M$f_per_group_pr_A[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (post_ATD_M$b_L_A),    # L (A): CZE Control
  
  Attr_L_M_freq,
  Attr_L_M_INfreq,
  
  L_AUS_Freq = (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (post_ATD_M$b_L_A + post_ATD_M$f_per_group_pr_A[,12,6])   # L (A): COL Less Freq. Users
)


summary.data.frame(M_posts_ATD_MM_Attr)
str(M_posts_ATD_MM_Attr) # Need 65 rows 


# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 
# TRUSTWORTHINESS: 

# Preparing composite coefficients: 
# AGE:
TR_Age_M_freq <- c(
  (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,3,2])+   # Age (A): COL More Freq. Users
    (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,5,2])+   # Age (A): CZE More Freq. Users
    (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,7,2])+   # Age (A): ZAF More Freq. Users
    (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,9,2])+   # Age (A): TUR More Freq. Users
    (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,11,2])   # Age (A): COL More Freq. Users
)/6

TR_Age_M_INfreq <- (
  (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,4,2])+   # Age (A): COL More Freq. Users
    (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,6,2])+   # Age (A): CZE More Freq. Users
    (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,8,2])+   # Age (A): ZAF More Freq. Users
    (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,10,2])+   # Age (A): TUR More Freq. Users
    (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
TR_Dist_M_freq <- (
  (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,3,3])+   # Dist (A): COL More Freq. Users
    (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,5,3])+   # Dist (A): CZE More Freq. Users
    (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,7,3])+   # Dist (A): ZAF More Freq. Users
    (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,9,3])+   # Dist (A): TUR More Freq. Users
    (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,11,3])   # Dist (A): COL More Freq. Users
)/6

TR_Dist_M_INfreq <- (
  (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,4,3])+   # Dist (A): COL More Freq. Users
    (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,6,3])+   # Dist (A): CZE More Freq. Users
    (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,8,3])+   # Dist (A): ZAF More Freq. Users
    (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,10,3])+   # Dist (A): TUR More Freq. Users
    (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
TR_Asym_M_freq <- (
  (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,3,4])+   # Asym (A): COL More Freq. Users
    (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,5,4])+   # Asym (A): CZE More Freq. Users
    (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,7,4])+   # Asym (A): ZAF More Freq. Users
    (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,9,4])+   # Asym (A): TUR More Freq. Users
    (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,11,4])   # Asym (A): COL More Freq. Users
)/6

TR_Asym_M_INfreq <- (
  (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,4,4])+   # Asym (A): COL More Freq. Users
    (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,6,4])+   # Asym (A): CZE More Freq. Users
    (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,8,4])+   # Asym (A): ZAF More Freq. Users
    (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,10,4])+   # Asym (A): TUR More Freq. Users
    (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
TR_SexTyp_M_freq <- (
  (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,3,5])+   # SShD (A): COL More Freq. Users
    (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,5,5])+   # SShD (A): CZE More Freq. Users
    (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,7,5])+   # SShD (A): ZAF More Freq. Users
    (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,9,5])+   # SShD (A): TUR More Freq. Users
    (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,11,5])   # SShD (A): COL More Freq. Users
)/6

TR_SexTyp_M_INfreq <- (
  (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,4,5])+   # SShD (A): COL More Freq. Users
    (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,6,5])+   # SShD (A): CZE More Freq. Users
    (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,8,5])+   # SShD (A): ZAF More Freq. Users
    (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,10,5])+   # SShD (A): TUR More Freq. Users
    (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
TR_L_M_freq <- (
  (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,3,6])+   # L (A): COL More Freq. Users
    (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,5,6])+   # L (A): CZE More Freq. Users
    (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,7,6])+   # L (A): ZAF More Freq. Users
    (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,9,6])+   # L (A): TUR More Freq. Users
    (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,11,6])   # L (A): COL More Freq. Users
)/6

TR_L_M_INfreq <- (
  (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,4,6])+   # L (A): COL More Freq. Users
    (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,6,6])+   # L (A): CZE More Freq. Users
    (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,8,6])+   # L (A): ZAF More Freq. Users
    (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,10,6])+   # L (A): TUR More Freq. Users
    (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,12,6])   # L (A): COL More Freq. Users
)/6



# GMM - SEPARATELY 
M_posts_ATD_MM_TRU <- list(
  #  AGE
  Age_Slope = (post_ATD_M$b_age_T),    # Slope prior to accessing culture
  
  TR_Age_M_freq,
  TR_Age_M_INfreq,
  
  Age_AUS_Freq = (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (post_ATD_M$b_age_T + post_ATD_M$f_per_group_pr_T[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (post_ATD_M$b_dist_T),    # Dist (A): CZE Control
  
  TR_Dist_M_freq,
  TR_Dist_M_INfreq,
  
  Dist_AUS_Freq = (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (post_ATD_M$b_dist_T + post_ATD_M$f_per_group_pr_T[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (post_ATD_M$b_FA_T),    # Asym (A): CZE Control
  
  TR_Asym_M_freq,
  TR_Asym_M_INfreq,
  
  Asym_AUS_Freq = (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_T[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (post_ATD_M$b_sshd_T),    # SexTyp (A): CZE Control
  
  TR_SexTyp_M_freq,
  TR_SexTyp_M_INfreq,
  
  SexTyp_AUS_Freq = (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (post_ATD_M$b_sshd_T + post_ATD_M$f_per_group_pr_T[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (post_ATD_M$b_L_T),    # L (A): CZE Control
  
  TR_L_M_freq,
  TR_L_M_INfreq,
  
  L_AUS_Freq = (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (post_ATD_M$b_L_T + post_ATD_M$f_per_group_pr_T[,12,6])   # L (A): COL Less Freq. Users
)


summary.data.frame(M_posts_ATD_MM_TRU)
str(M_posts_ATD_MM_TRU) 


# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 
# DOMINANCE: 

# AGE:
DOM_Age_M_freq <- c(
  (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,1,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,3,2])+   # Age (A): COL More Freq. Users
    (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,5,2])+   # Age (A): CZE More Freq. Users
    (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,7,2])+   # Age (A): ZAF More Freq. Users
    (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,9,2])+   # Age (A): TUR More Freq. Users
    (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,11,2])   # Age (A): COL More Freq. Users
)/6

DOM_Age_M_INfreq <- (
  (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,2,2])+   # Age (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,4,2])+   # Age (A): COL More Freq. Users
    (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,6,2])+   # Age (A): CZE More Freq. Users
    (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,8,2])+   # Age (A): ZAF More Freq. Users
    (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,10,2])+   # Age (A): TUR More Freq. Users
    (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,12,2])   # Age (A): COL More Freq. Users
)/6



# DIST: 
DOM_Dist_M_freq <- (
  (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,1,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,3,3])+   # Dist (A): COL More Freq. Users
    (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,5,3])+   # Dist (A): CZE More Freq. Users
    (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,7,3])+   # Dist (A): ZAF More Freq. Users
    (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,9,3])+   # Dist (A): TUR More Freq. Users
    (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,11,3])   # Dist (A): COL More Freq. Users
)/6

DOM_Dist_M_INfreq <- (
  (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,2,3])+   # Dist (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,4,3])+   # Dist (A): COL More Freq. Users
    (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,6,3])+   # Dist (A): CZE More Freq. Users
    (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,8,3])+   # Dist (A): ZAF More Freq. Users
    (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,10,3])+   # Dist (A): TUR More Freq. Users
    (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,12,3])   # Dist (A): COL More Freq. Users
)/6



# ASYM
DOM_Asym_M_freq <- (
  (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,1,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,3,4])+   # Asym (A): COL More Freq. Users
    (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,5,4])+   # Asym (A): CZE More Freq. Users
    (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,7,4])+   # Asym (A): ZAF More Freq. Users
    (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,9,4])+   # Asym (A): TUR More Freq. Users
    (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,11,4])   # Asym (A): COL More Freq. Users
)/6

DOM_Asym_M_INfreq <- (
  (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,2,4])+   # Asym (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,4,4])+   # Asym (A): COL More Freq. Users
    (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,6,4])+   # Asym (A): CZE More Freq. Users
    (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,8,4])+   # Asym (A): ZAF More Freq. Users
    (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,10,4])+   # Asym (A): TUR More Freq. Users
    (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,12,4])   # Asym (A): COL More Freq. Users
)/6



# SShD
DOM_SexTyp_M_freq <- (
  (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,1,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,3,5])+   # SShD (A): COL More Freq. Users
    (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,5,5])+   # SShD (A): CZE More Freq. Users
    (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,7,5])+   # SShD (A): ZAF More Freq. Users
    (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,9,5])+   # SShD (A): TUR More Freq. Users
    (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,11,5])   # SShD (A): COL More Freq. Users
)/6

DOM_SexTyp_M_INfreq <- (
  (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,2,5])+   # SShD (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,4,5])+   # SShD (A): COL More Freq. Users
    (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,6,5])+   # SShD (A): CZE More Freq. Users
    (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,8,5])+   # SShD (A): ZAF More Freq. Users
    (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,10,5])+   # SShD (A): TUR More Freq. Users
    (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,12,5])   # SShD (A): COL More Freq. Users
)/6



# L
DOM_L_M_freq <- (
  (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,1,6])+   # L (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,3,6])+   # L (A): COL More Freq. Users
    (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,5,6])+   # L (A): CZE More Freq. Users
    (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,7,6])+   # L (A): ZAF More Freq. Users
    (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,9,6])+   # L (A): TUR More Freq. Users
    (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,11,6])   # L (A): COL More Freq. Users
)/6

DOM_L_M_INfreq <- (
  (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,2,6])+   # L (A): AUS/NZE More Freq. Users
    (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,4,6])+   # L (A): COL More Freq. Users
    (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,6,6])+   # L (A): CZE More Freq. Users
    (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,8,6])+   # L (A): ZAF More Freq. Users
    (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,10,6])+   # L (A): TUR More Freq. Users
    (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,12,6])   # L (A): COL More Freq. Users
)/6



# Dominance - GMM 
str(post_ATD_M)
str(post_ATD_M$f_per_group_pr_D) # Mind that 1 is intercept! 

M_posts_ATD_MM_DOM <- list(
  #  AGE
  Age_Slope = (post_ATD_M$b_age_D),    # Slope prior to accessing culture
  
  DOM_Age_M_freq,
  DOM_Age_M_INfreq,
  
  Age_AUS_Freq = (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,1,2]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,2,2]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_COL_Freq = (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,3,2]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,4,2]),   # Age (A): COL Less Freq. Users
  
  Age_CZ_Frequent = (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,5,2]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,6,2]),   # Age (A): CZE Less Freq. Users
  
  Age_ZAF_Freq = (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,7,2]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,8,2]),   # Age (A): ZAF Less Freq. Users
  
  Age_TUR_Freq = (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,9,2]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,10,2]),   # Age (A): TUR Less Freq. Users
  
  Age_VNM_Freq = (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,11,2]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (post_ATD_M$b_age_D + post_ATD_M$f_per_group_pr_D[,12,2]),   # Age (A): COL Less Freq. Users
  
  
  # Distinctiveness
  Dist_Slope = (post_ATD_M$b_dist_D),    # Dist (A): CZE Control
  
  DOM_Dist_M_freq,
  DOM_Dist_M_INfreq,
  
  Dist_AUS_Freq = (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,1,3]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,2,3]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_COL_Freq = (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,3,3]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,4,3]),   # Dist (A): COL Less Freq. Users
  
  Dist_CZ_Frequent = (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,5,3]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,6,3]),   # Dist (A): CZE Less Freq. Users
  
  Dist_ZAF_Freq = (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,7,3]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,8,3]),   # Dist (A): ZAF Less Freq. Users
  
  Dist_TUR_Freq = (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,9,3]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,10,3]),   # Dist (A): TUR Less Freq. Users
  
  Dist_VNM_Freq = (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,11,3]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (post_ATD_M$b_dist_D + post_ATD_M$f_per_group_pr_D[,12,3]),   # Dist (A): COL Less Freq. Users
  
  
  # Facial asymmetry
  Asym_Slope = (post_ATD_M$b_FA_D),    # Asym (A): CZE Control
  
  DOM_Asym_M_freq,
  DOM_Asym_M_INfreq,
  
  Asym_AUS_Freq = (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,1,4]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (post_ATD_M$b_FA_T + post_ATD_M$f_per_group_pr_D[,2,4]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_COL_Freq = (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,3,4]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,4,4]),   # Asym (A): COL Less Freq. Users
  
  Asym_CZ_Frequent = (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,5,4]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,6,4]),   # Asym (A): CZE Less Freq. Users
  
  Asym_ZAF_Freq = (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,7,4]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,8,4]),   # Asym (A): ZAF Less Freq. Users
  
  Asym_TUR_Freq = (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,9,4]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,10,4]),   # Asym (A): TUR Less Freq. Users
  
  Asym_VNM_Freq = (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,11,4]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (post_ATD_M$b_FA_D + post_ATD_M$f_per_group_pr_D[,12,4]),   # Asym (A): COL Less Freq. Users
  
  
  # SShD
  SexTyp_slope = (post_ATD_M$b_sshd_D),    # SexTyp (A): CZE Control
  
  DOM_SexTyp_M_freq,
  DOM_SexTyp_M_INfreq,
  
  SexTyp_AUS_Freq = (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,1,5]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,2,5]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_COL_Freq = (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,3,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,4,5]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_CZ_Frequent = (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,5,5]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,6,5]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_ZAF_Freq = (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,7,5]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,8,5]),   # SexTyp (A): ZAF Less Freq. Users
  
  SexTyp_TUR_Freq = (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,9,5]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,10,5]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_VNM_Freq = (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,11,5]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (post_ATD_M$b_sshd_D + post_ATD_M$f_per_group_pr_D[,12,5]),   # SexTyp (A): COL Less Freq. Users
  
  
  # Skil L*
  L_CZ_slope = (post_ATD_M$b_L_D),    # L (A): CZE Control
  
  DOM_L_M_freq,
  DOM_L_M_INfreq,
  
  L_AUS_Freq = (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,1,6]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,2,6]),   # L (A): AUS/NZE Less Freq. Users
  
  L_COL_Freq = (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,3,6]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,4,6]),   # L (A): COL Less Freq. Users
  
  L_CZ_Frequent = (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,5,6]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,6,6]),   # L (A): CZE Less Freq. Users
  
  L_ZAF_Freq = (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,7,6]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,8,6]),   # L (A): ZAF Less Freq. Users
  
  L_TUR_Freq = (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,9,6]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,10,6]),   # L (A): TUR Less Freq. Users
  
  L_VNM_Freq = (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,11,6]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (post_ATD_M$b_L_D + post_ATD_M$f_per_group_pr_D[,12,6])   # L (A): COL Less Freq. Users
)


summary.data.frame(M_posts_ATD_MM_DOM)
str(M_posts_ATD_MM_TRU) # Need 65 rows 




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



tiff("M_ATD_GMM.tiff",width=35,height=30,units="cm",res=600,compression="lzw")
layout(matrix(1:3,byrow=F,nrow=1), widths=c(1.6,1.2,1.2),heights=c(1))

par(mar=c(2.1, 14.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.5),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Attr: GMM - Males", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#b9b9B999", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#b9b9B999", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#b9b9B999", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#b9b9B999", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#b9b9B999", lwd = 2)


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

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,75),
                               Mean=rep(0,75),Up_CI=rep(0,75)) 

for (i in 1:length(M_posts_ATD_MM_Attr)) {
  Coefficients_check$Mean[i]<-mean(M_posts_ATD_MM_Attr[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(M_posts_ATD_MM_Attr)) {
  Coefficients_check$Low_CI[i]<-PI(M_posts_ATD_MM_Attr[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(M_posts_ATD_MM_Attr)) {
  Coefficients_check$Up_CI[i]<-PI(M_posts_ATD_MM_Attr[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))

cex_t <- 1.25
cex_t2 <- 1
pls <- 1

text("Mean", x=0.325, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.25, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=0.4, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


for(i in 1:length(M_posts_ATD_MM_Attr)){ 
  toplot<-M_posts_ATD_MM_Attr[[i]]
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
  axis(2, at=ycoord, labels=textlab, srt=45, las=2, cex.axis=1, tick=F)
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=0.325, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=0.25, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=0.4, y=ys[i], cex=cex_t2)
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





par(mar=c(2.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.5),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Trustw: GMM - Males", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting


# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#b9b9B999", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#b9b9B999", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#b9b9B999", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#b9b9B999", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#b9b9B999", lwd = 2)


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

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,75),
                               Mean=rep(0,75),Up_CI=rep(0,75)) 

for (i in 1:length(M_posts_ATD_MM_TRU)) {
  Coefficients_check$Mean[i]<-mean(M_posts_ATD_MM_TRU[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(M_posts_ATD_MM_TRU)) {
  Coefficients_check$Low_CI[i]<-PI(M_posts_ATD_MM_TRU[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(M_posts_ATD_MM_TRU)) {
  Coefficients_check$Up_CI[i]<-PI(M_posts_ATD_MM_TRU[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))

cex_t <- 1.25
cex_t2 <- 1
pls <- 1

text("Mean", x=0.325, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.25, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=0.4, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


for(i in 1:length(M_posts_ATD_MM_TRU)){ 
  toplot<-M_posts_ATD_MM_TRU[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=0.325, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=0.25, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=0.4, y=ys[i], cex=cex_t2)
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=5, lty=1, col=cols[i])
  segments(x0 = ci[1], y0 = ycoord-0.1, x1 = ci[1], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  segments(x0 = ci[2], y0 = ycoord-0.1, x1 = ci[2], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0=0.0, y0=c(0.0, 16, 31.5, 47, 62.5), x1=0.0, y1=c(15.5, 31, 46.5, 62, 77.5), col="#999999", lwd=2, lty=1)
# 


par(mar=c(2.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.5),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Dominance: GMM - Males", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting


# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#b9b9B999", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#b9b9B999", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#b9b9B999", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#b9b9B999", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#b9b9B999", lwd = 2)


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

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,75),
                               Mean=rep(0,75),Up_CI=rep(0,75)) 

for (i in 1:length(M_posts_ATD_MM_DOM)) {
  Coefficients_check$Mean[i]<-mean(M_posts_ATD_MM_DOM[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(M_posts_ATD_MM_DOM)) {
  Coefficients_check$Low_CI[i]<-PI(M_posts_ATD_MM_DOM[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(M_posts_ATD_MM_DOM)) {
  Coefficients_check$Up_CI[i]<-PI(M_posts_ATD_MM_DOM[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))

cex_t <- 1.25
cex_t2 <- 1
pls <- 1

text("Mean", x=0.325, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.25, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=0.4, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


for(i in 1:length(M_posts_ATD_MM_DOM)){ 
  toplot<-M_posts_ATD_MM_DOM[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=0.325, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=0.25, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=0.4, y=ys[i], cex=cex_t2)
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=5, lty=1, col=cols[i])
  segments(x0 = ci[1], y0 = ycoord-0.1, x1 = ci[1], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  segments(x0 = ci[2], y0 = ycoord-0.1, x1 = ci[2], y1 = ycoord+0.1, lwd = 5, col = cols[i])
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

segments(x0=0.0, y0=c(0.0, 16, 31.5, 47, 62.5), x1=0.0, y1=c(15.5, 31, 46.5, 62, 77.5), col="#999999", lwd=2, lty=1)
# 


dev.off()



# GMM SHORT (without printing coeffiecients)
# GMM SHORT (without printing coeffiecients) 
# GMM SHORT (without printing coeffiecients)
# GMM SHORT (without printing coeffiecients)
# GMM SHORT (without printing coeffiecients)



tiff("M_ATD_GMM_No_Labels.tiff",width=33,height=30,units="cm",res=600,compression="lzw")
layout(matrix(1:3,byrow=F,nrow=1), widths=c(1.1,0.7,0.7),heights=c(1))

par(mar=c(2.1, 14.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.25),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Attr: GMM - Males", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting

# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#b9b9B999", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#b9b9B999", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#b9b9B999", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#b9b9B999", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#b9b9B999", lwd = 2)


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


for(i in 1:length(M_posts_ATD_MM_Attr)){ 
  toplot<-M_posts_ATD_MM_Attr[[i]]
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
  axis(2, at=ycoord, labels=textlab, srt=45, las=2, cex.axis=1, tick=F)
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



par(mar=c(2.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.25),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Trustw: GMM - Males", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting


# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#b9b9B999", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#b9b9B999", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#b9b9B999", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#b9b9B999", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#b9b9B999", lwd = 2)


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


for(i in 1:length(M_posts_ATD_MM_TRU)){ 
  toplot<-M_posts_ATD_MM_TRU[[i]]
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



par(mar=c(2.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.25),ylim=c(1,78),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Dominance: GMM - Males", side = 3, line = -2.20, adj = 0.5) # Adjust 'adj' for horizontal shifting


# Empty graph layout 

# Frames...
# 1
rect(-0.25,0.0,0.2,15.5, col=F, border = "#b9b9B999", lwd = 2)

# 2
rect(-0.25,16,0.2,31, col=F, border = "#b9b9B999", lwd = 2)

# 3
rect(-0.25,31.5,0.2,46.5, col=F, border = "#b9b9B999", lwd = 2)

# 4
rect(-0.25,47,0.2,62, col=F, border = "#b9b9B999", lwd = 2)

# 5 
rect(-0.25,62.5,0.2,77.5, col=F, border = "#b9b9B999", lwd = 2)


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


for(i in 1:length(M_posts_ATD_MM_DOM)){ 
  toplot<-M_posts_ATD_MM_DOM[[i]]
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





#-----
# Correlations comparisons: 
# Correlations comparisons: 
# Correlations comparisons: 
# Correlations comparisons: 
# Correlations comparisons: 
# Correlations comparisons: 
# Correlations comparisons: 
# Correlations comparisons: 
# Correlations comparisons: 
# Correlations comparisons: 
# Correlations comparisons: 
# Correlations comparisons: 
#-----

# WOMEN

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
n_samples <- dim(post_ATD_F$Rho_FSMUi_A)[1]


# BETWEEN! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(between), function(i) {
  post_ATD_F$Rho_FSMUi_A[, between[i,1], between[i,2]]
})

# Convert to matrix to preserve structure
between_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(between))

# Is that correct? 
cor(post_ATD_F$Rho_FSMUi_A[1:100,1,4],between_mat[1:100,1])
cor(post_ATD_F$Rho_FSMUi_A[1:100,4,7],between_mat[1:100,16])
cor(post_ATD_F$Rho_FSMUi_A[1:100,9,12],between_mat[1:100,29])
# Yes, Petr-Chat Rulez...

# WITHIN
selected_correlations <- sapply(1:nrow(within), function(i) {
  post_ATD_F$Rho_FSMUi_A[, within[i,1], within[i,2]]
})

within_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(within))

cor(post_ATD_F$Rho_FSMUi_A[1:100,1,2],within_mat[1:100,1])
cor(post_ATD_F$Rho_FSMUi_A[1:100,3,4],within_mat[1:100,2])
cor(post_ATD_F$Rho_FSMUi_A[1:100,9,10],within_mat[1:100,5])

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



#Question 2: Do high-SM consuming groups agree across countries more than low-SM consuming groups?
# HIGH! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(high), function(i) {
  post_ATD_F$Rho_FSMUi_A[, high[i,1], high[i,2]]
})

# Convert to matrix to preserve structure
high_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(high))

# Is that correct? 
cor(post_ATD_F$Rho_FSMUi_A[1:100,1,3],high_mat[1:100,1])
cor(post_ATD_F$Rho_FSMUi_A[1:100,3,7],high_mat[1:100,7])
cor(post_ATD_F$Rho_FSMUi_A[1:100,9,11],high_mat[1:100,15])
# Yes, Petr-Chat Rulez...

# LOW
selected_correlations <- sapply(1:nrow(low), function(i) {
  post_ATD_F$Rho_FSMUi_A[, low[i,1], low[i,2]]
})

low_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(low))

cor(post_ATD_F$Rho_FSMUi_A[1:100,2,4],low_mat[1:100,1])
cor(post_ATD_F$Rho_FSMUi_A[1:100,4,12],low_mat[1:100,9])
cor(post_ATD_F$Rho_FSMUi_A[1:100,10,12],low_mat[1:100,15])

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
  post_ATD_F$Rho_FSMUi_T[, between[i,1], between[i,2]]
})

# Convert to matrix to preserve structure
between_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(between))

# Is that correct? 
cor(post_ATD_F$Rho_FSMUi_T[1:100,1,4],between_mat[1:100,1])
cor(post_ATD_F$Rho_FSMUi_T[1:100,4,7],between_mat[1:100,16])
cor(post_ATD_F$Rho_FSMUi_T[1:100,9,12],between_mat[1:100,29])

# WITHIN
selected_correlations <- sapply(1:nrow(within), function(i) {
  post_ATD_F$Rho_FSMUi_T[, within[i,1], within[i,2]]
})

within_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(within))

cor(post_ATD_F$Rho_FSMUi_T[1:100,1,2],within_mat[1:100,1])
cor(post_ATD_F$Rho_FSMUi_T[1:100,3,4],within_mat[1:100,2])
cor(post_ATD_F$Rho_FSMUi_T[1:100,9,10],within_mat[1:100,5])

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


#Question 2: Do high-SM consuming groups agree across countries more than low-SM consuming groups?
# HIGH! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(high), function(i) {
  post_ATD_F$Rho_FSMUi_T[, high[i,1], high[i,2]]
})

# Convert to matrix to preserve structure
high_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(high))

# Is that correct? 
cor(post_ATD_F$Rho_FSMUi_T[1:100,1,3],high_mat[1:100,1])
cor(post_ATD_F$Rho_FSMUi_T[1:100,3,7],high_mat[1:100,7])
cor(post_ATD_F$Rho_FSMUi_T[1:100,9,11],high_mat[1:100,15])

# LOW
selected_correlations <- sapply(1:nrow(low), function(i) {
  post_ATD_F$Rho_FSMUi_T[, low[i,1], low[i,2]]
})

low_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(low))

cor(post_ATD_F$Rho_FSMUi_T[1:100,2,4],low_mat[1:100,1])
cor(post_ATD_F$Rho_FSMUi_T[1:100,4,12],low_mat[1:100,9])
cor(post_ATD_F$Rho_FSMUi_T[1:100,10,12],low_mat[1:100,15])

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
  post_ATD_F$Rho_FSMUi_D[, between[i,1], between[i,2]]
})

# Convert to matrix to preserve structure
between_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(between))

# Is that correct? 
cor(post_ATD_F$Rho_FSMUi_D[1:100,1,4],between_mat[1:100,1])
cor(post_ATD_F$Rho_FSMUi_D[1:100,4,7],between_mat[1:100,16])
cor(post_ATD_F$Rho_FSMUi_D[1:100,9,12],between_mat[1:100,29])

# WITHIN
selected_correlations <- sapply(1:nrow(within), function(i) {
  post_ATD_F$Rho_FSMUi_D[, within[i,1], within[i,2]]
})

within_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(within))

cor(post_ATD_F$Rho_FSMUi_D[1:100,1,2],within_mat[1:100,1])
cor(post_ATD_F$Rho_FSMUi_D[1:100,3,4],within_mat[1:100,2])
cor(post_ATD_F$Rho_FSMUi_D[1:100,9,10],within_mat[1:100,5])

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



#Question 2: Do high-SM consuming groups agree across countries more than low-SM consuming groups?
# HIGH! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(high), function(i) {
  post_ATD_F$Rho_FSMUi_T[, high[i,1], high[i,2]]
})

# Convert to matrix to preserve structure
high_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(high))

# Is that correct? 
cor(post_ATD_F$Rho_FSMUi_D[1:100,1,3],high_mat[1:100,1])
cor(post_ATD_F$Rho_FSMUi_D[1:100,3,7],high_mat[1:100,7])
cor(post_ATD_F$Rho_FSMUi_D[1:100,9,11],high_mat[1:100,15])
# Yes, Petr-Chat Rulez...

# LOW
selected_correlations <- sapply(1:nrow(low), function(i) {
  post_ATD_F$Rho_FSMUi_D[, low[i,1], low[i,2]]
})

low_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(low))

cor(post_ATD_F$Rho_FSMUi_D[1:100,2,4],low_mat[1:100,1])
cor(post_ATD_F$Rho_FSMUi_D[1:100,4,12],low_mat[1:100,9])
cor(post_ATD_F$Rho_FSMUi_D[1:100,10,12],low_mat[1:100,15])

#difference of the averages

# 1 both posteriors as vector - subset randomly from the longer... 
mean(as.vector(high_mat) - as.vector(low_mat))

# 2 create rowmeans and then compare thee
mean(rowMeans(high_mat)-rowMeans(low_mat))

PI(rowMeans(high_mat)-rowMeans(low_mat),0.95)

mean(rowMeans(high_mat)); PI(rowMeans(high_mat), prob=0.95)
mean(rowMeans(low_mat)); PI(rowMeans(low_mat), prob=0.95) 


# Between scales: 
F_precis <- precis(post_ATD_F, depth=3, prob=0.95)

post_ATD_F$Rho
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
n_samples <- dim(post_ATD_M$Rho_FSMUi_A)[1]


# BETWEEN! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(between), function(i) {
  post_ATD_M$Rho_FSMUi_A[, between[i,1], between[i,2]]
})

# Convert to matrix to preserve structure
between_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(between))

# Is that correct? 
cor(post_ATD_M$Rho_FSMUi_A[1:100,1,4],between_mat[1:100,1])
cor(post_ATD_M$Rho_FSMUi_A[1:100,4,7],between_mat[1:100,16])
cor(post_ATD_M$Rho_FSMUi_A[1:100,9,12],between_mat[1:100,29])
# Yes, Petr-Chat Rulez...

# WITHIN
selected_correlations <- sapply(1:nrow(within), function(i) {
  post_ATD_M$Rho_FSMUi_A[, within[i,1], within[i,2]]
})

within_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(within))

cor(post_ATD_M$Rho_FSMUi_A[1:100,1,2],within_mat[1:100,1])
cor(post_ATD_M$Rho_FSMUi_A[1:100,3,4],within_mat[1:100,2])
cor(post_ATD_M$Rho_FSMUi_A[1:100,9,10],within_mat[1:100,5])

# We want this: Create rowmeans and then compare thee
mean(rowMeans(within_mat)-rowMeans(between_mat))
PI(rowMeans(within_mat)-rowMeans(between_mat),prob=0.95)

mean(rowMeans(within_mat)); PI(rowMeans(within_mat), prob=0.95)
mean(rowMeans(between_mat)); PI(rowMeans(between_mat), prob=0.95) 



#Question 2: Do high-SM consuming groups agree across countries more than low-SM consuming groups?
# HIGH! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(high), function(i) {
  post_ATD_M$Rho_FSMUi_A[, high[i,1], high[i,2]]
})

# Convert to matrix to preserve structure
high_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(high))

# Is that correct? 
cor(post_ATD_M$Rho_FSMUi_A[1:100,1,3],high_mat[1:100,1])
cor(post_ATD_M$Rho_FSMUi_A[1:100,9,11],high_mat[1:100,15])

# LOW
selected_correlations <- sapply(1:nrow(low), function(i) {
  post_ATD_M$Rho_FSMUi_A[, low[i,1], low[i,2]]
})

low_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(low))

cor(post_ATD_M$Rho_FSMUi_A[1:100,2,4],low_mat[1:100,1])
cor(post_ATD_M$Rho_FSMUi_A[1:100,4,12],low_mat[1:100,9])
cor(post_ATD_M$Rho_FSMUi_A[1:100,10,12],low_mat[1:100,15])

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
  post_ATD_M$Rho_FSMUi_T[, between[i,1], between[i,2]]
})

# Convert to matrix to preserve structure
between_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(between))

# Is that correct? 
cor(post_ATD_M$Rho_FSMUi_T[1:100,1,4],between_mat[1:100,1])
cor(post_ATD_M$Rho_FSMUi_T[1:100,4,7],between_mat[1:100,16])
cor(post_ATD_M$Rho_FSMUi_T[1:100,9,12],between_mat[1:100,29])
# Yes, Petr-Chat Rulez...

# WITHIN
selected_correlations <- sapply(1:nrow(within), function(i) {
  post_ATD_M$Rho_FSMUi_T[, within[i,1], within[i,2]]
})

within_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(within))

cor(post_ATD_M$Rho_FSMUi_T[1:100,1,2],within_mat[1:100,1])
cor(post_ATD_M$Rho_FSMUi_T[1:100,3,4],within_mat[1:100,2])
cor(post_ATD_M$Rho_FSMUi_T[1:100,9,10],within_mat[1:100,5])

# create rowmeans and then compare thee
mean(rowMeans(within_mat)-rowMeans(between_mat))
PI(rowMeans(within_mat)-rowMeans(between_mat),prob=0.95)


mean(rowMeans(within_mat)); PI(rowMeans(within_mat), prob=0.95)
mean(rowMeans(between_mat)); PI(rowMeans(between_mat), prob=0.95) 



#Question 2: Do high-SM consuming groups agree across countries more than low-SM consuming groups?
# HIGH! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(high), function(i) {
  post_ATD_M$Rho_FSMUi_T[, high[i,1], high[i,2]]
})

# Convert to matrix to preserve structure
high_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(high))

# Is that correct? 
cor(post_ATD_M$Rho_FSMUi_T[1:100,1,3],high_mat[1:100,1])
cor(post_ATD_M$Rho_FSMUi_T[1:100,3,7],high_mat[1:100,7])
cor(post_ATD_M$Rho_FSMUi_T[1:100,9,11],high_mat[1:100,15])

# LOW
selected_correlations <- sapply(1:nrow(low), function(i) {
  post_ATD_M$Rho_FSMUi_T[, low[i,1], low[i,2]]
})

low_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(low))

cor(post_ATD_M$Rho_FSMUi_T[1:100,2,4],low_mat[1:100,1])
cor(post_ATD_M$Rho_FSMUi_T[1:100,4,12],low_mat[1:100,9])
cor(post_ATD_M$Rho_FSMUi_T[1:100,10,12],low_mat[1:100,15])

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
  post_ATD_M$Rho_FSMUi_D[, between[i,1], between[i,2]]
})

# Convert to matrix to preserve structure
between_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(between))

# Is that correct? 
cor(post_ATD_M$Rho_FSMUi_D[1:100,1,4],between_mat[1:100,1])
cor(post_ATD_M$Rho_FSMUi_D[1:100,4,7],between_mat[1:100,16])
cor(post_ATD_M$Rho_FSMUi_D[1:100,9,12],between_mat[1:100,29])


# WITHIN
selected_correlations <- sapply(1:nrow(within), function(i) {
  post_ATD_M$Rho_FSMUi_D[, within[i,1], within[i,2]]
})

within_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(within))

cor(post_ATD_M$Rho_FSMUi_D[1:100,1,2],within_mat[1:100,1])
cor(post_ATD_M$Rho_FSMUi_D[1:100,3,4],within_mat[1:100,2])
cor(post_ATD_M$Rho_FSMUi_D[1:100,9,10],within_mat[1:100,5])

# create rowmeans and then compare thee
mean(rowMeans(within_mat)-rowMeans(between_mat))
PI(rowMeans(within_mat)-rowMeans(between_mat),prob=0.95)

mean(rowMeans(within_mat)); PI(rowMeans(within_mat), prob=0.95)
mean(rowMeans(between_mat)); PI(rowMeans(between_mat), prob=0.95) 


#Question 2: Do high-SM consuming groups agree across countries more than low-SM consuming groups?
# HIGH! 

# Extract the correlations for each specified pair
selected_correlations <- sapply(1:nrow(high), function(i) {
  post_ATD_M$Rho_FSMUi_D[, high[i,1], high[i,2]]
})

# Convert to matrix to preserve structure
high_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(high))

# Is that correct? 
cor(post_ATD_M$Rho_FSMUi_D[1:100,1,3],high_mat[1:100,1])
cor(post_ATD_M$Rho_FSMUi_D[1:100,3,7],high_mat[1:100,7])
cor(post_ATD_M$Rho_FSMUi_D[1:100,9,11],high_mat[1:100,15])


# LOW
selected_correlations <- sapply(1:nrow(low), function(i) {
  post_ATD_M$Rho_FSMUi_D[, low[i,1], low[i,2]]
})

low_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(low))

cor(post_ATD_M$Rho_FSMUi_D[1:100,2,4],low_mat[1:100,1])
cor(post_ATD_M$Rho_FSMUi_D[1:100,4,12],low_mat[1:100,9])
cor(post_ATD_M$Rho_FSMUi_D[1:100,10,12],low_mat[1:100,15])

#difference of the averages

# 1 both posteriors as vector - subset randomly from the longer... 
mean(as.vector(high_mat) - as.vector(low_mat))

# 2 create rowmeans and then compare thee
mean(rowMeans(high_mat)-rowMeans(low_mat))

PI(rowMeans(high_mat)-rowMeans(low_mat),0.95)

mean(rowMeans(high_mat)); PI(rowMeans(high_mat), prob=0.95)
mean(rowMeans(low_mat)); PI(rowMeans(low_mat), prob=0.95) 



# Between scales: 
M_precis <- precis(post_ATD_M, depth=3, prob=0.95)

post_ATD_M$Rho
M_precis["Rho_Scales[1,2]",]
M_precis["Rho_Scales[1,3]",]
M_precis["Rho_Scales[2,3]",]



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Within country average difference vs across country average difference...
# Within country average difference vs across country average difference...
# Within country average difference vs across country average difference...
# Within country average difference vs across country average difference...
# Within country average difference vs across country average difference...
# Within country average difference vs across country average difference...


# Females 

# Attractiveness

# Average difference within cultures: 
F_Attr <- post_ATD_F$f_per_group_pr_A[,,1]
str(F_Attr)

# Define the index pairs for the first set (1:2, 3:4, ..., 11:12)
within_pairs <- matrix(1:12, ncol = 2, byrow = TRUE); within_pairs

# Compute mean absolute differences for predefined pairs
within <- abs(F_Attr[,c(1,3,5,7,9,11)]-F_Attr[,c(2,4,6,8,10,12)])
head(within); F_Attr[1:5,3]-F_Attr[1:5,4]; F_Attr[1:5,5]-F_Attr[1:5,6] # I beleive it does what it is supposed to
within_rowmean <- rowMeans(within); within_rowmean

# Generate all possible column pairs
?combn
all_pairs <- t(as.matrix(combn(1:12, 2)))
all_pairs

# Delete those pairs that are now in both matrixes... 
between_pairs <- all_pairs[-c(1,22, 39, 52, 61, 66),] #1:2, 3:4, 5:6, 7:8, 9:10, 11:12

between_pairs <- t(between_pairs)
# Compute mean absolute differences for non-predefined pairs
between <- abs(F_Attr[, between_pairs[1, ]] - F_Attr[, between_pairs[2, ]])

between_rowmean <- rowMeans(between)

mean(within_rowmean)
PI(within_rowmean, prob=0.95)

mean(between_rowmean)
PI(between_rowmean, prob=0.95)

mean(between_rowmean-within_rowmean)
PI((between_rowmean-within_rowmean), prob=0.95)


# Trustworthiness
F_Tru <- post_ATD_F$f_per_group_pr_T[,,1]
str(F_Tru)

# Compute mean absolute differences for predefined pairs
within <- abs(F_Tru[,c(1,3,5,7,9,11)]-F_Tru[,c(2,4,6,8,10,12)])
head(within); F_Tru[1:5,3]-F_Tru[1:5,4]; F_Tru[1:5,5]-F_Tru[1:5,6] # I beleive it does what it is supposed to
within_rowmean <- rowMeans(within); within_rowmean

# Compute mean absolute differences for non-predefined pairs
between <- abs(F_Tru[, between_pairs[1, ]] - F_Tru[, between_pairs[2, ]])
between_rowmean <- rowMeans(between)

mean(within_rowmean)
PI(within_rowmean, prob=0.95)

mean(between_rowmean)
PI(between_rowmean, prob=0.95)

mean(between_rowmean-within_rowmean)
PI((between_rowmean-within_rowmean), prob=0.95)


# Dominance
F_Dom <- post_ATD_F$f_per_group_pr_D[,,1]
str(F_Dom)

# Compute mean absolute differences for predefined pairs
within <- abs(F_Dom[,c(1,3,5,7,9,11)]-F_Dom[,c(2,4,6,8,10,12)])
head(within); F_Dom[1:5,3]-F_Dom[1:5,4]; F_Dom[1:5,5]-F_Dom[1:5,6] # I beleive it does what it is supposed to
within_rowmean <- rowMeans(within); within_rowmean

# Compute mean absolute differences for non-predefined pairs
between <- abs(F_Dom[, between_pairs[1, ]] - F_Dom[, between_pairs[2, ]])
between_rowmean <- rowMeans(between)

mean(within_rowmean)
PI(within_rowmean, prob=0.95)

mean(between_rowmean)
PI(between_rowmean, prob=0.95)

mean(between_rowmean-within_rowmean)
PI((between_rowmean-within_rowmean), prob=0.95)




# MEN 
# Attractiveness
M_sca <- post_ATD_M$f_per_group_pr_A[,,1]
str(M_sca)

# Compute mean absolute differences for predefined pairs
within <- abs(M_sca[,c(1,3,5,7,9,11)]-M_sca[,c(2,4,6,8,10,12)])
head(within); M_sca[1:5,3]-M_sca[1:5,4]; M_sca[1:5,5]-M_sca[1:5,6] # I beleive it does what it is supposed to
within_rowmean <- rowMeans(within); within_rowmean

# Compute mean absolute differences for non-predefined pairs
between <- abs(M_sca[, between_pairs[1, ]] - M_sca[, between_pairs[2, ]])
between_rowmean <- rowMeans(between)

mean(within_rowmean)
PI(within_rowmean, prob=0.95)

mean(between_rowmean)
PI(between_rowmean, prob=0.95)

mean(between_rowmean-within_rowmean)
PI((between_rowmean-within_rowmean), prob=0.95)



# Trustworthiness
M_sca <- post_ATD_M$f_per_group_pr_T[,,1]
str(M_sca)

# Compute mean absolute differences for predefined pairs
within <- abs(M_sca[,c(1,3,5,7,9,11)]-M_sca[,c(2,4,6,8,10,12)])
head(within); M_sca[1:5,3]-M_sca[1:5,4]; M_sca[1:5,5]-M_sca[1:5,6] # I beleive it does what it is supposed to
within_rowmean <- rowMeans(within); within_rowmean

# Compute mean absolute differences for non-predefined pairs
between <- abs(M_sca[, between_pairs[1, ]] - M_sca[, between_pairs[2, ]])
between_rowmean <- rowMeans(between)

mean(within_rowmean)
PI(within_rowmean, prob=0.95)

mean(between_rowmean)
PI(between_rowmean, prob=0.95)

mean(between_rowmean-within_rowmean)
PI((between_rowmean-within_rowmean), prob=0.95)



# Dominance
M_sca <- post_ATD_M$f_per_group_pr_D[,,1]
str(M_sca)

# Compute mean absolute differences for predefined pairs
within <- abs(M_sca[,c(1,3,5,7,9,11)]-M_sca[,c(2,4,6,8,10,12)])
head(within); M_sca[1:5,3]-M_sca[1:5,4]; M_sca[1:5,5]-M_sca[1:5,6] # I beleive it does what it is supposed to
within_rowmean <- rowMeans(within); within_rowmean

# Compute mean absolute differences for non-predefined pairs
between <- abs(M_sca[, between_pairs[1, ]] - M_sca[, between_pairs[2, ]])
between_rowmean <- rowMeans(between)

mean(within_rowmean)
PI(within_rowmean, prob=0.95)

mean(between_rowmean)
PI(between_rowmean, prob=0.95)

mean(between_rowmean-within_rowmean)
PI((between_rowmean-within_rowmean), prob=0.95)


# POST #1 
# Mean - CZ vs "Rest of the world average" 

# Now - every other combination but CZ is what we need: 
within<-t(as.matrix(c(1,2)))
all <- t(combn(1:12, 2))  # Row 39 ~ CZ (5,6)

others <- all[-39,]

n_samples <- dim(post_ATD_F$Rho_FSMUi_A)[1]

# 1.1 Female 

# A) Attractiveness
str(post_ATD_F$Rho_FSMUi_A)
mean(post_ATD_F$Rho_FSMUi_A[,5,6]) # CZ
CZ_F <- post_ATD_F$Rho_FSMUi_A[,5,6]

selected_correlations <- sapply(1:nrow(others), function(i) {
  post_ATD_F$Rho_FSMUi_A[, others[i,1], others[i,2]]
})

out_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(others))

cor(post_ATD_F$Rho_FSMUi_A[1:100,1,2],out_mat[1:100,1])
cor(post_ATD_F$Rho_FSMUi_A[1:100,4,12],out_mat[1:100,38])

out_rwmns <- rowMeans(out_mat)

# means and PIs in the two groups: 
mean(CZ_F)
PI(CZ_F,prob=0.95)

mean(out_rwmns)
PI(out_rwmns,prob=0.95)

mean(CZ_F - out_rwmns)
PI((CZ_F - out_rwmns), prob=0.95)



# B) Trustworthiness
CZ_F <- post_ATD_F$Rho_FSMUi_T[,5,6]

selected_correlations <- sapply(1:nrow(others), function(i) {
  post_ATD_F$Rho_FSMUi_T[, others[i,1], others[i,2]]
})

out_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(others))

cor(post_ATD_F$Rho_FSMUi_T[1:100,1,2],out_mat[1:100,1])
cor(post_ATD_F$Rho_FSMUi_T[1:100,4,12],out_mat[1:100,38])

out_rwmns <- rowMeans(out_mat)

# means and PIs in the two groups: 
mean(CZ_F)
PI(CZ_F,prob=0.95)

mean(out_rwmns)
PI(out_rwmns,prob=0.95)

mean(CZ_F - out_rwmns)
PI((CZ_F - out_rwmns), prob=0.95)



# C) Dominance
CZ_F <- post_ATD_F$Rho_FSMUi_D[,5,6]

selected_correlations <- sapply(1:nrow(others), function(i) {
  post_ATD_F$Rho_FSMUi_D[, others[i,1], others[i,2]]
})

out_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(others))

cor(post_ATD_F$Rho_FSMUi_D[1:100,1,2],out_mat[1:100,1])
cor(post_ATD_F$Rho_FSMUi_D[1:100,4,12],out_mat[1:100,38])

out_rwmns <- rowMeans(out_mat)

# means and PIs in the two groups: 
mean(CZ_F)
PI(CZ_F,prob=0.95)

mean(out_rwmns)
PI(out_rwmns,prob=0.95)

mean(CZ_F - out_rwmns)
PI((CZ_F - out_rwmns), prob=0.95)


# 1.2 Male 
# A) Attractiveness
str(post_ATD_M$Rho_FSMUi_A)
mean(post_ATD_M$Rho_FSMUi_A[,5,6]) # CZ
CZ_M <- post_ATD_M$Rho_FSMUi_A[,5,6]

selected_correlations <- sapply(1:nrow(others), function(i) {
  post_ATD_M$Rho_FSMUi_A[, others[i,1], others[i,2]]
})

out_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(others))

cor(post_ATD_M$Rho_FSMUi_A[1:100,1,2],out_mat[1:100,1])
cor(post_ATD_M$Rho_FSMUi_A[1:100,4,12],out_mat[1:100,38])

out_rwmns <- rowMeans(out_mat)

# means and PIs in the two groups: 
mean(CZ_M)
PI(CZ_M,prob=0.95)

mean(out_rwmns)
PI(out_rwmns,prob=0.95)

mean(CZ_M - out_rwmns)
PI((CZ_M - out_rwmns), prob=0.95)



# B) Trustworthiness
CZ_M <- post_ATD_M$Rho_FSMUi_T[,5,6]

selected_correlations <- sapply(1:nrow(others), function(i) {
  post_ATD_M$Rho_FSMUi_T[, others[i,1], others[i,2]]
})

out_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(others))

cor(post_ATD_M$Rho_FSMUi_T[1:100,1,2],out_mat[1:100,1])
cor(post_ATD_M$Rho_FSMUi_T[1:100,4,12],out_mat[1:100,38])

out_rwmns <- rowMeans(out_mat)

# means and PIs in the two groups: 
mean(CZ_M)
PI(CZ_M,prob=0.95)

mean(out_rwmns)
PI(out_rwmns,prob=0.95)

mean(CZ_M - out_rwmns)
PI((CZ_M - out_rwmns), prob=0.95)



# C) Dominance
CZ_M <- post_ATD_M$Rho_FSMUi_D[,5,6]

selected_correlations <- sapply(1:nrow(others), function(i) {
  post_ATD_M$Rho_FSMUi_D[, others[i,1], others[i,2]]
})

out_mat <- matrix(selected_correlations, nrow = n_samples, ncol = nrow(others))

cor(post_ATD_M$Rho_FSMUi_D[1:100,1,2],out_mat[1:100,1])
cor(post_ATD_M$Rho_FSMUi_D[1:100,4,12],out_mat[1:100,38])

out_rwmns <- rowMeans(out_mat)

# means and PIs in the two groups: 
mean(CZ_M)
PI(CZ_M,prob=0.95)

mean(out_rwmns)
PI(out_rwmns,prob=0.95)

mean(CZ_M - out_rwmns)
PI((CZ_M - out_rwmns), prob=0.95)




# 2 CZ + AUS/NZ -> 1:2, 5:6, 1:5, 1:6, 2:5, 2:6

CZAUSNZ <- t(combn(c(1,2,5,6),2));CZAUSNZ
RestOF <-all[-c(1,4,5,14,15,39),]

# 2.1. Females 

# A) Attractiveness

cor_CZAUSNZ <- sapply(1:nrow(CZAUSNZ), function(i) {
  post_ATD_F$Rho_FSMUi_A[, CZAUSNZ[i,1], CZAUSNZ[i,2]]
})

colMeans(cor_CZAUSNZ) # Correct (see Figure 1, sry that I have not noticed I have it...)

cor_RESTOF <- sapply(1:nrow(RestOF), function(i) {
  post_ATD_F$Rho_FSMUi_A[, RestOF[i,1], RestOF[i,2]]
})

mean_CZAUSNZ <- rowMeans(cor_CZAUSNZ)
mean_RESTOF <- rowMeans(cor_RESTOF)

mean(mean_CZAUSNZ)
PI(mean_CZAUSNZ, prob=0.95)

mean(mean_RESTOF)
PI(mean_RESTOF, prob=0.95)

mean(mean_CZAUSNZ-mean_RESTOF)
PI((mean_CZAUSNZ-mean_RESTOF),prob=0.95)



# B) Trustworthiness

cor_CZAUSNZ <- sapply(1:nrow(CZAUSNZ), function(i) {
  post_ATD_F$Rho_FSMUi_T[, CZAUSNZ[i,1], CZAUSNZ[i,2]]
})

colMeans(cor_CZAUSNZ) # Correct (see Figure 1, sry that I have not noticed I have it...)

cor_RESTOF <- sapply(1:nrow(RestOF), function(i) {
  post_ATD_F$Rho_FSMUi_T[, RestOF[i,1], RestOF[i,2]]
})

mean_CZAUSNZ <- rowMeans(cor_CZAUSNZ)
mean_RESTOF <- rowMeans(cor_RESTOF)

mean(mean_CZAUSNZ)
PI(mean_CZAUSNZ, prob=0.95)

mean(mean_RESTOF)
PI(mean_RESTOF, prob=0.95)

mean(mean_CZAUSNZ-mean_RESTOF)
PI((mean_CZAUSNZ-mean_RESTOF),prob=0.95)




# C) Dominance

cor_CZAUSNZ <- sapply(1:nrow(CZAUSNZ), function(i) {
  post_ATD_F$Rho_FSMUi_D[, CZAUSNZ[i,1], CZAUSNZ[i,2]]
})

colMeans(cor_CZAUSNZ) # Correct (see Figure 1, sry that I have not noticed I have it...)

cor_RESTOF <- sapply(1:nrow(RestOF), function(i) {
  post_ATD_F$Rho_FSMUi_D[, RestOF[i,1], RestOF[i,2]]
})

mean_CZAUSNZ <- rowMeans(cor_CZAUSNZ)
mean_RESTOF <- rowMeans(cor_RESTOF)

mean(mean_CZAUSNZ)
PI(mean_CZAUSNZ, prob=0.95)

mean(mean_RESTOF)
PI(mean_RESTOF, prob=0.95)

mean(mean_CZAUSNZ-mean_RESTOF)
PI((mean_CZAUSNZ-mean_RESTOF),prob=0.95)




# 2.2. Males 

# A) Attractiveness

cor_CZAUSNZ <- sapply(1:nrow(CZAUSNZ), function(i) {
  post_ATD_M$Rho_FSMUi_A[, CZAUSNZ[i,1], CZAUSNZ[i,2]]
})

colMeans(cor_CZAUSNZ) # Correct (see Figure 1, sry that I have not noticed I have it...)

cor_RESTOF <- sapply(1:nrow(RestOF), function(i) {
  post_ATD_M$Rho_FSMUi_A[, RestOF[i,1], RestOF[i,2]]
})

mean_CZAUSNZ <- rowMeans(cor_CZAUSNZ)
mean_RESTOF <- rowMeans(cor_RESTOF)

mean(mean_CZAUSNZ)
PI(mean_CZAUSNZ, prob=0.95)

mean(mean_RESTOF)
PI(mean_RESTOF, prob=0.95)

mean(mean_CZAUSNZ-mean_RESTOF)
PI((mean_CZAUSNZ-mean_RESTOF),prob=0.95)



# B) Trustworthiness

cor_CZAUSNZ <- sapply(1:nrow(CZAUSNZ), function(i) {
  post_ATD_M$Rho_FSMUi_T[, CZAUSNZ[i,1], CZAUSNZ[i,2]]
})

colMeans(cor_CZAUSNZ) # Correct (see Figure 1, sry that I have not noticed I have it...)

cor_RESTOF <- sapply(1:nrow(RestOF), function(i) {
  post_ATD_M$Rho_FSMUi_T[, RestOF[i,1], RestOF[i,2]]
})

mean_CZAUSNZ <- rowMeans(cor_CZAUSNZ)
mean_RESTOF <- rowMeans(cor_RESTOF)

mean(mean_CZAUSNZ)
PI(mean_CZAUSNZ, prob=0.95)

mean(mean_RESTOF)
PI(mean_RESTOF, prob=0.95)

mean(mean_CZAUSNZ-mean_RESTOF)
PI((mean_CZAUSNZ-mean_RESTOF),prob=0.95)



# C) Dominance

cor_CZAUSNZ <- sapply(1:nrow(CZAUSNZ), function(i) {
  post_ATD_M$Rho_FSMUi_D[, CZAUSNZ[i,1], CZAUSNZ[i,2]]
})

colMeans(cor_CZAUSNZ) # Correct (see Figure 1, sry that I have not noticed I have it...)

cor_RESTOF <- sapply(1:nrow(RestOF), function(i) {
  post_ATD_M$Rho_FSMUi_D[, RestOF[i,1], RestOF[i,2]]
})

mean_CZAUSNZ <- rowMeans(cor_CZAUSNZ)
mean_RESTOF <- rowMeans(cor_RESTOF)

mean(mean_CZAUSNZ)
PI(mean_CZAUSNZ, prob=0.95)

mean(mean_RESTOF)
PI(mean_RESTOF, prob=0.95)

mean(mean_CZAUSNZ-mean_RESTOF)
PI((mean_CZAUSNZ-mean_RESTOF),prob=0.95)




# 2 Prolific vs The rest of the world: 

Prolific <- t(combn(c(1,2,8,9),2));Prolific
RestOF <-all[-c(1,7,8,17,18,57),];RestOF

# 2.1. Females 

# A) Attractiveness

cor_Prolific <- sapply(1:nrow(Prolific), function(i) {
  post_ATD_F$Rho_FSMUi_A[, Prolific[i,1], Prolific[i,2]]
})

colMeans(cor_Prolific) # Correct (see Figure 1, sry that I have not noticed I have it...)

cor_RESTOF <- sapply(1:nrow(RestOF), function(i) {
  post_ATD_F$Rho_FSMUi_A[, RestOF[i,1], RestOF[i,2]]
})

mean_Prol <- rowMeans(cor_Prolific)
mean_RESTOF <- rowMeans(cor_RESTOF)

mean(mean_Prol)
PI(mean_Prol, prob=0.95)

mean(mean_RESTOF)
PI(mean_RESTOF, prob=0.95)

mean(mean_Prol-mean_RESTOF)
PI((mean_Prol-mean_RESTOF),prob=0.95)



# B) Trustworthiness

cor_Prolific <- sapply(1:nrow(Prolific), function(i) {
  post_ATD_F$Rho_FSMUi_T[, Prolific[i,1], Prolific[i,2]]
})

colMeans(cor_Prolific) # Correct (see Figure 1, sry that I have not noticed I have it...)

cor_RESTOF <- sapply(1:nrow(RestOF), function(i) {
  post_ATD_F$Rho_FSMUi_T[, RestOF[i,1], RestOF[i,2]]
})

mean_Prol <- rowMeans(cor_Prolific)
mean_RESTOF <- rowMeans(cor_RESTOF)

mean(mean_Prol)
PI(mean_Prol, prob=0.95)

mean(mean_RESTOF)
PI(mean_RESTOF, prob=0.95)

mean(mean_Prol-mean_RESTOF)
PI((mean_Prol-mean_RESTOF),prob=0.95)




# C) Dominance

cor_Prolific <- sapply(1:nrow(Prolific), function(i) {
  post_ATD_F$Rho_FSMUi_D[, Prolific[i,1], Prolific[i,2]]
})

colMeans(cor_Prolific) # Correct (see Figure 1, sry that I have not noticed I have it...)

cor_RESTOF <- sapply(1:nrow(RestOF), function(i) {
  post_ATD_F$Rho_FSMUi_D[, RestOF[i,1], RestOF[i,2]]
})

mean_Prol <- rowMeans(cor_Prolific)
mean_RESTOF <- rowMeans(cor_RESTOF)

mean(mean_Prol)
PI(mean_Prol, prob=0.95)

mean(mean_RESTOF)
PI(mean_RESTOF, prob=0.95)

mean(mean_Prol-mean_RESTOF)
PI((mean_Prol-mean_RESTOF),prob=0.95)




# 2.2. Males 

# A) Attractiveness

cor_Prolific <- sapply(1:nrow(Prolific), function(i) {
  post_ATD_M$Rho_FSMUi_A[, Prolific[i,1], Prolific[i,2]]
})

colMeans(cor_Prolific) # Correct (see Figure 1, sry that I have not noticed I have it...)

cor_RESTOF <- sapply(1:nrow(RestOF), function(i) {
  post_ATD_M$Rho_FSMUi_A[, RestOF[i,1], RestOF[i,2]]
})

mean_Prol <- rowMeans(cor_Prolific)
mean_RESTOF <- rowMeans(cor_RESTOF)

mean(mean_Prol)
PI(mean_Prol, prob=0.95)

mean(mean_RESTOF)
PI(mean_RESTOF, prob=0.95)

mean(mean_Prol-mean_RESTOF)
PI((mean_Prol-mean_RESTOF),prob=0.95)



# B) Trustworthiness

cor_Prolific <- sapply(1:nrow(Prolific), function(i) {
  post_ATD_M$Rho_FSMUi_T[, Prolific[i,1], Prolific[i,2]]
})

colMeans(cor_Prolific) # Correct (see Figure 1, sry that I have not noticed I have it...)

cor_RESTOF <- sapply(1:nrow(RestOF), function(i) {
  post_ATD_M$Rho_FSMUi_T[, RestOF[i,1], RestOF[i,2]]
})

mean_Prol <- rowMeans(cor_Prolific)
mean_RESTOF <- rowMeans(cor_RESTOF)

mean(mean_Prol)
PI(mean_Prol, prob=0.95)

mean(mean_RESTOF)
PI(mean_RESTOF, prob=0.95)

mean(mean_Prol-mean_RESTOF)
PI((mean_Prol-mean_RESTOF),prob=0.95)



# C) Dominance

cor_Prolific <- sapply(1:nrow(Prolific), function(i) {
  post_ATD_M$Rho_FSMUi_D[, Prolific[i,1], Prolific[i,2]]
})

colMeans(cor_Prolific) # Correct (see Figure 1, sry that I have not noticed I have it...)

cor_RESTOF <- sapply(1:nrow(RestOF), function(i) {
  post_ATD_M$Rho_FSMUi_D[, RestOF[i,1], RestOF[i,2]]
})

mean_Prol <- rowMeans(cor_Prolific)
mean_RESTOF <- rowMeans(cor_RESTOF)

mean(mean_Prol)
PI(mean_Prol, prob=0.95)

mean(mean_RESTOF)
PI(mean_RESTOF, prob=0.95)

mean(mean_Prol-mean_RESTOF)
PI((mean_Prol-mean_RESTOF),prob=0.95)




