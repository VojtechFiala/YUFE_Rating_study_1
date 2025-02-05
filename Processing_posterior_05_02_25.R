# Drawing

library(rethinking)


# DRAWING FIGURES: 

# Models without, respectively with GMM, all the raters included: 
load("my_workspace1.Rdata")
load("my_workspace2.Rdata")

# Unfortunately, the posterior cannot be sampled in my case, so they were take separately: 
load("posts.Rdata")

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 
#   M1 
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.


# Estimates per culture - Model Without GMM
post_noGMM <- postCZRSAUS_AT_TW_5

summary(as.factor(CZRSAUS_AT_TW_5@data$FSMUi))
str(post_noGMM$Rho_FSMUi_A) 

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

# BEWARE - CZ ORIG IS on a different position than before! 

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# Attractiveness

M1_posts_ATR <- list(
  # 1 CZECH ORIGINAL 
  CZ_orig = post_noGMM$a_A + post_noGMM$a_group_A[,7],
  # 2 CZECH - above - median users.
  CZ_Intensive_Users = post_noGMM$a_A + post_noGMM$a_group_A[,5],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = post_noGMM$a_A + post_noGMM$a_group_A[,6],
  # 4 Australia - above - median users  
  AUS_Intensive_User = post_noGMM$a_A + post_noGMM$a_group_A[,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = post_noGMM$a_A + post_noGMM$a_group_A[,2],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = post_noGMM$a_A + post_noGMM$a_group_A[,10],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = post_noGMM$a_A + post_noGMM$a_group_A[,11],
  # Colombia - above - median users 
  COL_Intensive_User = post_noGMM$a_A + post_noGMM$a_group_A[,3], 
  # COlombia - below - median users
  COL_Less_Intensive = post_noGMM$a_A + post_noGMM$a_group_A[,4],
  # Vietnam - above - median users 
  VN_Intensive_User = post_noGMM$a_A + post_noGMM$a_group_A[,12],
  # Vietnam - below - median users 
  VN_Less_Intensive = post_noGMM$a_A + post_noGMM$a_group_A[,13],
  # RSA - above - median users 
  RSA_Intenstive_User = post_noGMM$a_A + post_noGMM$a_group_A[,8],
  # RSA - below - median users 
  RSA_Less_Intensive = post_noGMM$a_A + post_noGMM$a_group_A[,9]
)

summary.data.frame(M1_posts_ATR)
str(M1_posts_ATR) # Need 13 rows 

tiff("M1_no_GMM_Attractiveness_04_02_2025.tiff",width=20,height=12,units="cm",res=600,compression="lzw")
par(mar=c(3.1, 18.1, 1.1, 2.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-1,1),ylim=c(1,13.5),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Czech Faces - Rated Attractiveness: Model without GMM", side = 3, line = 0.20, adj = 2.5) # Adjust 'adj' for horizontal shifting

segments(x0 = -1, y0 = 1:13, x1 = 0.5, y1 = 1:13, col = "#40808080", lwd = 1.5)
abline(v=c(seq(from=-1, to=0.5, by=0.25)),col="#80808080",lty=2)
# Empty graph layout 

cols<-c("#00FBFF", 
        "#8C00FF",
        "#FFADD0",
        "#09C9FF",
        "#00FF33",
        
        "#0FEA33",
        "#FFEA00",
        "#FFA600",
        "#00FF33",
        "#EEEA00",
        
        "#EEEDD0",
        "#FFFD0D",
        "#DDEE0D"
) 

ys<-seq(from=1,to=13, by=1)
ys<-rev(ys)

labs<-c("Czechs (Control group)",    
        "Czechs - More frequent users",
        "Czechs - Less frequent users",
        "Australia/NewZ - More freq. users",
        "Australia/NewZ - Less freq. users",
        "Turkey - More frequent users",
        "Turkey - Less frequent users",
        "Colombia - More frequent users",
        "Colombia - Less frequent users",
        "Vietnamese - More freq. users",
        "Vietnamese - Less freq. users",
        "South Africans - More freq. users",
        "South Africans - Less freq. users"
)

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,13),
                               Mean=rep(0,13),Up_CI=rep(0,13)) 

for (i in 1:length(M1_posts_ATR)) {
  Coefficients_check$Mean[i]<-mean(M1_posts_ATR[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(M1_posts_ATR)) {
  Coefficients_check$Low_CI[i]<-PI(M1_posts_ATR[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(M1_posts_ATR)) {
  Coefficients_check$Up_CI[i]<-PI(M1_posts_ATR[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))

cex_t <- 1.25
cex_t2 <- 0.75
pls <- 0.4

text("Mean", x=0.8, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.6, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=1.0, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


for(i in 1:length(M1_posts_ATR)){ 
  toplot<-M1_posts_ATR[[i]]
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
  axis(2, at=ycoord, labels=textlab, srt=45, las=2, cex.axis=1.2)
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=0.8, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=0.6, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=1.0, y=ys[i], cex=cex_t2)
  for (i in 1:13) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.01)+ycoord[i],border=T, lwd = 1, col=colpol))
  }
  #lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

abline(v=0.0, col="#999999", lwd=2, lty=1)

dev.off()

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# Trustworthiness

M1_posts_TRU <- list(
  # 1 CZECH ORIGINAL 
  CZ_orig = post_noGMM$a_T + post_noGMM$a_group_T[,7],
  # 2 CZECH - above - median users.
  CZ_Intensive_Users = post_noGMM$a_T + post_noGMM$a_group_T[,5],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = post_noGMM$a_T + post_noGMM$a_group_T[,6],
  # 4 Australia - above - median users  
  AUS_Intensive_User = post_noGMM$a_T + post_noGMM$a_group_T[,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = post_noGMM$a_T + post_noGMM$a_group_T[,2],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = post_noGMM$a_T + post_noGMM$a_group_T[,10],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = post_noGMM$a_T + post_noGMM$a_group_T[,11],
  # Colombia - above - median users 
  COL_Intensive_User = post_noGMM$a_T + post_noGMM$a_group_T[,3], 
  # COlombia - below - median users
  COL_Less_Intensive = post_noGMM$a_T + post_noGMM$a_group_T[,4],
  # Vietnam - above - median users 
  VN_Intensive_User = post_noGMM$a_T + post_noGMM$a_group_T[,12],
  # Vietnam - below - median users 
  VN_Less_Intensive = post_noGMM$a_T + post_noGMM$a_group_T[,13],
  # RSA - above - median users 
  RSA_Intenstive_User = post_noGMM$a_T + post_noGMM$a_group_T[,8],
  # RSA - below - median users 
  RSA_Less_Intensive = post_noGMM$a_T + post_noGMM$a_group_T[,9]
)

summary.data.frame(M1_posts_ATR)
str(M1_posts_ATR) # Need 13 rows 

tiff("M1_no_GMM_Trustworthiness_04_02_2025.tiff",width=20,height=12,units="cm",res=600,compression="lzw")
par(mar=c(3.1, 18.1, 1.1, 2.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-1,1),ylim=c(1,13.5),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Czech Faces - Rated Trustworthiness: Model without GMM", side = 3, line = 0.20, adj = 2.5) # Adjust 'adj' for horizontal shifting

segments(x0 = -1, y0 = 1:13, x1 = 0.5, y1 = 1:13, col = "#40808080", lwd = 1.5)
abline(v=c(seq(from=-1, to=0.5, by=0.25)),col="#80808080",lty=2)
# Empty graph layout 

ys<-seq(from=1,to=13, by=1)
ys<-rev(ys)

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,13),
                               Mean=rep(0,13),Up_CI=rep(0,13)) 

for (i in 1:length(M1_posts_TRU)) {
  Coefficients_check$Mean[i]<-mean(M1_posts_TRU[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(M1_posts_TRU)) {
  Coefficients_check$Low_CI[i]<-PI(M1_posts_TRU[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(M1_posts_TRU)) {
  Coefficients_check$Up_CI[i]<-PI(M1_posts_TRU[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))

cex_t <- 1.25
cex_t2 <- 0.75
pls <- 0.4

text("Mean", x=0.8, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.6, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=1.0, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


for(i in 1:length(M1_posts_TRU)){ 
  toplot<-M1_posts_TRU[[i]]
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
  axis(2, at=ycoord, labels=textlab, srt=45, las=2, cex.axis=1.2)
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=0.8, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=0.6, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=1.0, y=ys[i], cex=cex_t2)
  for (i in 1:13) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.01)+ycoord[i],border=T, lwd = 1, col=colpol))
  }
  #lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

abline(v=0.0, col="#999999", lwd=2, lty=1)

dev.off()

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# Correlations - Model without GMM 

# Attractiveness:

# List of the relevant correlations (such that deserve comparisons):
str(post_noGMM)
str(post_noGMM$Rho_FSMUi_A) 


# Czech...
# CZE control - CZE more frequent users 
# CZE control - CZE less frequent users
# CZE more vs. CZE less frequent users

post_CorAT_plot <- list( # Run the means without (cursor selection). The numbers shall be reasonable (0.5 - 0.9), it's a correlation of psychological scales
  # Attractiveness
  Atr_CZ_Orig_CZ_Above = post_noGMM$Rho_FSMUi_A[,7,5], # mean(post_noGMM$Rho_FSMUi_A[,7,5])
  Atr_CZ_Orig_CZ_below = post_noGMM$Rho_FSMUi_A[,7,6], # mean(post_noGMM$Rho_FSMUi_A[,7,6])
  Atr_CZ_above_CZ_below = post_noGMM$Rho_FSMUi_A[,5,6], # mean(post_noGMM$Rho_FSMUi_A[,5,6])
  
# Australia & New Zealand
# CZE control - AUS/NZE more freq. users 
# CZE control - AUS/NZE less freq. users
# AUS/NZE more vs. AUS/NZE less freq. users
Atr_CZ_Orig_AUS_Above = post_noGMM$Rho_FSMUi_A[,7,1], # mean(post_noGMM$Rho_FSMUi_A[,7,1])
Atr_CZ_Orig_AUS_Below = post_noGMM$Rho_FSMUi_A[,7,2], # mean(post_noGMM$Rho_FSMUi_A[,7,2])
Atr_AUS_above_AUS_Below = post_noGMM$Rho_FSMUi_A[,2,1], # mean(post_noGMM$Rho_FSMUi_A[,2,1])

# Turkey
# CZE control - TUR more freq. users 
# CZE control - TUR less freq. users
# TUR more vs. TUR less freq. users
Atr_CZ_Orig_TUR_Above = post_noGMM$Rho_FSMUi_A[,7,10], # mean(post_noGMM$Rho_FSMUi_A[,7,10])
Atr_CZ_Orig_TUR_Below = post_noGMM$Rho_FSMUi_A[,7,11], # mean(post_noGMM$Rho_FSMUi_A[,7,11])
Atr_TUR_above_TUR_Below = post_noGMM$Rho_FSMUi_A[,10,11], # mean(post_noGMM$Rho_FSMUi_A[,10,11])

# Colombia 
# CZE control - COL more freq. users 
# CZE control - COL less freq. users
# COL more vs. COL less freq. users
Atr_CZ_Orig_COL_Above = post_noGMM$Rho_FSMUi_A[,7,3], # mean(post_noGMM$Rho_FSMUi_A[,7,3])
Atr_CZ_Orig_COL_Below = post_noGMM$Rho_FSMUi_A[,7,4], # mean(post_noGMM$Rho_FSMUi_A[,7,4])
Atr_COL_above_COL_Below = post_noGMM$Rho_FSMUi_A[,3,4], # mean(post_noGMM$Rho_FSMUi_A[,3,4])

# Vietnam 
# CZE control - VNM more freq. users 
# CZE control - VNM less freq. users
# VNM more vs. VNM less freq. users
Atr_CZ_Orig_VNM_Above = post_noGMM$Rho_FSMUi_A[,7,12], # mean(post_noGMM$Rho_FSMUi_A[,7,12])
Atr_CZ_Orig_VNM_Below = post_noGMM$Rho_FSMUi_A[,7,13], # mean(post_noGMM$Rho_FSMUi_A[,7,13])
Atr_VNM_above_VNM_Below = post_noGMM$Rho_FSMUi_A[,12,13], # mean(post_noGMM$Rho_FSMUi_A[,12,13])

# South Africa...
# CZE control - ZAF more freq. users 
# CZE control - ZAF less freq. users
# ZAF more vs. ZAF less freq. users
Atr_CZ_Orig_ZAF_Above = post_noGMM$Rho_FSMUi_A[,7,8], # mean(post_noGMM$Rho_FSMUi_A[,7,8])
Atr_CZ_Orig_ZAF_Below = post_noGMM$Rho_FSMUi_A[,7,9], # mean(post_noGMM$Rho_FSMUi_A[,7,9])
Atr_ZAF_above_ZAF_Below = post_noGMM$Rho_FSMUi_A[,8,9], # mean(post_noGMM$Rho_FSMUi_A[,8,9])



# Trustworthiness
Tru_CZ_Orig_CZ_Above = post_noGMM$Rho_FSMUi_T[,7,5], # mean(post_noGMM$Rho_FSMUi_T[,7,5])
Tru_CZ_Orig_CZ_below = post_noGMM$Rho_FSMUi_T[,7,6], # mean(post_noGMM$Rho_FSMUi_T[,7,6])
Tru_CZ_above_CZ_below = post_noGMM$Rho_FSMUi_T[,5,6], # mean(post_noGMM$Rho_FSMUi_T[,5,6])

# Australia & New Zealand
# CZE control - AUS/NZE more freq. users 
# CZE control - AUS/NZE less freq. users
# AUS/NZE more vs. AUS/NZE less freq. users
Tru_CZ_Orig_AUS_Above = post_noGMM$Rho_FSMUi_T[,7,1], # mean(post_noGMM$Rho_FSMUi_T[,7,1])
Tru_CZ_Orig_AUS_Below = post_noGMM$Rho_FSMUi_T[,7,2], # mean(post_noGMM$Rho_FSMUi_T[,7,2])
Tru_AUS_above_AUS_Below = post_noGMM$Rho_FSMUi_T[,2,1], # mean(post_noGMM$Rho_FSMUi_T[,2,1])

# Turkey
# CZE control - TUR more freq. users 
# CZE control - TUR less freq. users
# TUR more vs. TUR less freq. users
Tru_CZ_Orig_TUR_Above = post_noGMM$Rho_FSMUi_T[,7,10], # mean(post_noGMM$Rho_FSMUi_T[,7,10])
Tru_CZ_Orig_TUR_Below = post_noGMM$Rho_FSMUi_T[,7,11], # mean(post_noGMM$Rho_FSMUi_T[,7,11])
Tru_TUR_above_TUR_Below = post_noGMM$Rho_FSMUi_T[,10,11], # mean(post_noGMM$Rho_FSMUi_T[,10,11])

# Colombia 
# CZE control - COL more freq. users 
# CZE control - COL less freq. users
# COL more vs. COL less freq. users
Tru_CZ_Orig_COL_Above = post_noGMM$Rho_FSMUi_T[,7,3], # mean(post_noGMM$Rho_FSMUi_T[,7,3])
Tru_CZ_Orig_COL_Below = post_noGMM$Rho_FSMUi_T[,7,4], # mean(post_noGMM$Rho_FSMUi_T[,7,4])
Tru_COL_above_COL_Below = post_noGMM$Rho_FSMUi_T[,3,4], # mean(post_noGMM$Rho_FSMUi_T[,3,4])

# Vietnam 
# CZE control - VNM more freq. users 
# CZE control - VNM less freq. users
# VNM more vs. VNM less freq. users
Tru_CZ_Orig_VNM_Above = post_noGMM$Rho_FSMUi_T[,7,12], # mean(post_noGMM$Rho_FSMUi_T[,7,12])
Tru_CZ_Orig_VNM_Below = post_noGMM$Rho_FSMUi_T[,7,13], # mean(post_noGMM$Rho_FSMUi_T[,7,13])
Tru_VNM_above_VNM_Below = post_noGMM$Rho_FSMUi_T[,12,13], # mean(post_noGMM$Rho_FSMUi_T[,12,13])

# South Africa...
# CZE control - ZAF more freq. users 
# CZE control - ZAF less freq. users
# ZAF more vs. ZAF less freq. users
Tru_CZ_Orig_ZAF_Above = post_noGMM$Rho_FSMUi_T[,7,8], # mean(post_noGMM$Rho_FSMUi_T[,7,8])
Tru_CZ_Orig_ZAF_Below = post_noGMM$Rho_FSMUi_T[,7,9], # mean(post_noGMM$Rho_FSMUi_T[,7,9])
Tru_ZAF_above_ZAF_Below = post_noGMM$Rho_FSMUi_T[,8,9] # mean(post_noGMM$Rho_FSMUi_T[,8,9])
)


tiff("Correlations_ATTR_model_without_GMM_04_02_25.tiff",width=30,height=20.5,units="cm",res=600,compression="lzw")
par(mar=c(1.7, 19.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(0,1),ylim=c(1,43),yaxt="n",bty="n", ylab="",xlab="", cex.axis=1)
segments(x0 = 0.4, y0 = c(1:3,
                          4.5:6.5,
                          8:10,
                          11.5:13.5,
                          15:17,
                          18.5:20.5,
                          23:25,
                          26.5:28.5,
                          30:32,
                          33.5:35.5,
                          37:39,
                          40.5:42.5), x1 = 1.0, y1 = c(1:3,
                                                       4.5:6.5,
                                                       8:10,
                                                       11.5:13.5,
                                                       15:17,
                                                       18.5:20.5,
                                                       23:25,
                                                       26.5:28.5,
                                                       30:32,
                                                       33.5:35.5,
                                                       37:39,
                                                       40.5:42.5), col = "#40808080", lwd = 1.5)

abline(v=c(seq(from=0.5, to=1, by=0.1)),col="#80808080",lty=2)
abline(v=c(0,0.5,1),col="darkgrey",lty=2, lwd=3)

# Empty graph layout 
cols<-c("#d1584f", 
        "#9e3931",
        "#825d5b", 
        
        "#83f287",
        "#52bf56",
        "#5f9c61", 
        
        "#719deb",
        "#5f7db3", 
        "#305391",
        
        "#eef58c",
        "#c6cf4c", 
        "#81854c",
        
        "#d1584f", 
        "#9e3931",
        "#825d5b", 
        
        "#83f287",
        "#52bf56",
        "#5f9c61", 
        
        "#719deb",
        "#5f7db3", 
        "#305391",
        
        "#eef58c",
        "#c6cf4c", 
        "#81854c",
        
        "#d1584f", 
        "#9e3931",
        "#825d5b", 
        
        "#83f287",
        "#52bf56",
        "#5f9c61", 
        
        "#719deb",
        "#5f7db3", 
        "#305391",
        
        "#eef58c",
        "#c6cf4c", 
        "#81854c"
) 

ys<-c(1:3,
      4.5:6.5,
      8:10,
      11.5:13.5,
      15:17,
      18.5:20.5,
      23:25,
      26.5:28.5,
      30:32,
      33.5:35.5,
      37:39,
      40.5:42.5)

ys<-rev(ys)

COLROW_names<- c("(A) CZE control - CZE more frequent users", 
                 "(A) CZE control - CZE less frequent users",
                 "(A) CZE more - CZE less frequent users",
                 
                 "(A) AUS/NZE more freq. users - CZE control",
                 "(A) AUS/NZE less freq. users - CZE control",
                 "(A) AUS/NZE more - AUS/NZE less freq. users",
                 
                 "(A) TUR more freq. users - CZE control", 
                 "(A) TUR less freq. users - CZE control",
                 "(A) TUR more - TUR less freq. users",
                 
                 "(A) COL more freq. users - CZE control", 
                 "(A) COL less freq. users - CZE control",
                 "(A) COL more vs. COL less freq. users",
                 
                 "(A) VNM more freq. users - CZE control", 
                 "(A) VNM less freq. users - CZE control",
                 "(A) VNM more - VNM less freq. users",
                 
                 "(A) ZAF more freq. users - CZE control", 
                 "(A) ZAF less freq. users - CZE control",
                 "(A) ZAF more - ZAF less freq. users",
                 
                 
                 "(T) CZE control - CZE more frequent users", 
                 "(T) CZE control - CZE less frequent users",
                 "(T) CZE more - CZE less frequent users",
                 
                 "(T) AUS/NZE more freq. users - CZE control",
                 "(T) AUS/NZE less freq. users - CZE control",
                 "(T) AUS/NZE more - AUS/NZE less freq. users",
                 
                 "(T) TUR more freq. users - CZE control", 
                 "(T) TUR less freq. users - CZE control",
                 "(T) TUR more - TUR less freq. users",
                 
                 "(T) COL more freq. users - CZE control", 
                 "(T) COL less freq. users - CZE control",
                 "(T) COL more - COL less freq. users",
                 
                 "(T) VNM more freq. users - CZE control", 
                 "(T) VNM less freq. users - CZE control",
                 "(T) VNM more - VNM less freq. users",
                 
                 "(T) ZAF more freq. users - CZE control", 
                 "(T) ZAF less freq. users - CZE control",
                 "(T) ZAF more - ZAF less freq. users"
)

Coefficients_check<-data.frame(Name=COLROW_names,Low_CI=rep(0,36),
                               Mean=rep(0,36),Up_CI=rep(0,36)) # Sorry, not in a mood to reinvent, how to .... do it aesthetically
for (i in 1:length(post_CorAT_plot)) {
  Coefficients_check$Mean[i]<-mean(post_CorAT_plot[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(post_CorAT_plot)) {
  Coefficients_check$Low_CI[i]<-PI(post_CorAT_plot[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(post_CorAT_plot)) {
  Coefficients_check$Up_CI[i]<-PI(post_CorAT_plot[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))

cex_t <- 1.05
pls <- 1.2
text("Mean", x=0.2, y=max(ys+pls), cex=cex_t, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.10, y=max(ys+pls), cex=cex_t, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=0.30, y=max(ys+pls), cex=cex_t, offset=0.5, xpd=NA, srt=0)

for(i in 1:length(post_CorAT_plot)){ 
  toplot<-post_CorAT_plot[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  textlab<-COLROW_names[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  axis(2, at=ycoord, labels=textlab, las=2, cex.axis=1.05, tick=F)
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=0.2, y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=0.1,y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=0.3,y=ys[i], cex=cex_t)
  for (i in 1:36) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.002)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
 # lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

dev.off()

# Plot of correlations from the model (not just the selected pairs as above, complete story).
# Mind that no correlation was estimated between trustworthiness and attractiveness, except for the main one. 

# Two tables - top, attractiveness, bottom, trustworthiness

labels_x <- c("AUS/NZE Freq", "AUS/NZE Infreq", "COL Freq","COL Infreq", "CZE Control","CZE Freq", "CZE Infreq","ZAF Freq", "ZAF Infreq",
              "TUR Freq", "TUR Infreq", "VNM Freq", "VNM Infreq")
labels_y <- labels_x

# Attr
str(post_noGMM$Rho_FSMUi_A)

library(qgraph)
library(bootnet)
library(beepr)
library(ComplexHeatmap)
library(circlize)


tiff("Correlations_Heatmap_ATTR_model_without_GMM_04_02_25.tiff",width=30,height=25,units="cm",res=600,compression="lzw")
cor_table_mean <- apply(post_noGMM$Rho_FSMUi_A,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(post_noGMM$Rho_FSMUi_A,c(2,3),PI, prob=0.95)
cor_table_LOWCI <- cor_table_CI[1,,] 
cor_table_UPWCI <- cor_table_CI[2,,] 

font1 <- 15
font2 <- 18

# I want the coefficient to be printed above the axis, while the CIs should be below it: 
col_fun <- colorRamp2(c(0.5, 0.75, 1), c("salmon", "white", "lightblue"))
Heatmap((as.matrix(cor_table_mean)), column_title ="Attractiveness: All correlations", cluster_rows=FALSE, 
        cluster_columns = FALSE, column_title_gp = gpar(fontsize=font1), column_names_gp = gpar(fontsize = font1), 
        row_names_gp = gpar(fontsize = font1), column_names_rot = 45, column_names_side = "top", name = "r", 
        col = col_fun,
        cell_fun = function(j, i, x, y, width, height, fill) {
          if (j>i) {c(grid.text(sprintf("%.3f", cor_table_mean[i, j]), x, y, gp = gpar(fontsize = font2),just="top"))}
          else if (j<i){c(grid.text(sprintf("%.3f", cor_table_LOWCI[i, j]), x, y, gp = gpar(fontsize = font1),just = c(0.65,-0.35)),
                          grid.text(sprintf("%.3f", cor_table_UPWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.45,0.95))
          )}
        },rect_gp = gpar(col = "white", lwd = 1), row_title = "95% Compatibility Interval")

dev.off()



tiff("Correlations_Heatmap_TRUSTW_model_without_GMM_04_02_25.tiff",width=30,height=25,units="cm",res=600,compression="lzw")
cor_table_mean <- apply(post_noGMM$Rho_FSMUi_T,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(post_noGMM$Rho_FSMUi_T,c(2,3),PI, prob=0.95)
cor_table_LOWCI <- cor_table_CI[1,,] 
cor_table_UPWCI <- cor_table_CI[2,,] 

font1 <- 15
font2 <- 18

# I want the coefficient to be printed above the axis, while the CIs should be below it: 
col_fun <- colorRamp2(c(0.5, 0.75, 1), c("salmon", "white", "lightblue"))
Heatmap((as.matrix(cor_table_mean)), column_title ="Trustworthiness: All correlations", cluster_rows=FALSE, 
        cluster_columns = FALSE, column_title_gp = gpar(fontsize=font1), column_names_gp = gpar(fontsize = font1), 
        row_names_gp = gpar(fontsize = font1), column_names_rot = 45, column_names_side = "top", name = "r", 
        col = col_fun,
        cell_fun = function(j, i, x, y, width, height, fill) {
          if (j>i) {c(grid.text(sprintf("%.3f", cor_table_mean[i, j]), x, y, gp = gpar(fontsize = font2),just="top"))}
          else if (j<i){c(grid.text(sprintf("%.3f", cor_table_LOWCI[i, j]), x, y, gp = gpar(fontsize = font1),just = c(0.65,-0.35)),
                          grid.text(sprintf("%.3f", cor_table_UPWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.45,0.95))
          )}
        },rect_gp = gpar(col = "white", lwd = 1), row_title = "95% Compatibility Interval")

dev.off()

# This is moreless everyting I can instantly gain from this model, let's move on...


#-------
#-------
#-------
#-------
#-------
#-------

load("my_workspace2.Rdata")


# Estimates per culture - Model WITH GMM 
post_GMM <- postCZRSAUS_TW_4

str(post_GMM)

summary(as.factor(CZRSAUS_TW_4@data$FSMUi))
#   NUM   INT   CORR_Label
#  2635   1     AUS Above
#  2540   2     AUS Below
#   785   3     COL Above
#   775   4     COL Below
#  7545   5     CZ Above
#  7280   6     CZ Below
# 75386   7     CZ_orig 0
#  2435   8     RSA Above
#  2145   9     RSA Below
#  3790  10     TUR Above
#  3910  11     TUR Below
#  3630  12     VN Above
#  3395  13     VN Below

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

# BEWARE - CZ ORIG IS on a different position than before! (before means in previous [preliminary] analyses)

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# Attractiveness

M2_posts_ATR <- list(
  # 1 CZECH ORIGINAL 
  CZ_orig = post_GMM$aA + post_GMM$a_group_A[,7],
  # 2 CZECH - above - median users.
  CZ_Intensive_Users = post_GMM$aA + post_GMM$a_group_A[,5],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = post_GMM$aA + post_GMM$a_group_A[,6],
  # 4 Australia - above - median users  
  AUS_Intensive_User = post_GMM$aA + post_GMM$a_group_A[,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = post_GMM$aA + post_GMM$a_group_A[,2],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = post_GMM$aA + post_GMM$a_group_A[,10],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = post_GMM$aA + post_GMM$a_group_A[,11],
  # Colombia - above - median users 
  COL_Intensive_User = post_GMM$aA + post_GMM$a_group_A[,3], 
  # COlombia - below - median users
  COL_Less_Intensive = post_GMM$aA + post_GMM$a_group_A[,4],
  # Vietnam - above - median users 
  VN_Intensive_User = post_GMM$aA + post_GMM$a_group_A[,12],
  # Vietnam - below - median users 
  VN_Less_Intensive = post_GMM$aA + post_GMM$a_group_A[,13],
  # RSA - above - median users 
  RSA_Intenstive_User = post_GMM$aA + post_GMM$a_group_A[,8],
  # RSA - below - median users 
  RSA_Less_Intensive = post_GMM$aA + post_GMM$a_group_A[,9]
)

summary.data.frame(M2_posts_ATR)
str(M2_posts_ATR) # Need 13 rows 

tiff("M2_WITH_GMM_Attractiveness_04_02_2025.tiff",width=20,height=12,units="cm",res=600,compression="lzw")
par(mar=c(3.1, 18.1, 1.1, 2.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-1,1),ylim=c(1,13.5),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Czech Faces - Rated Attractiveness: Model *with* GMM", side = 3, line = 0.20, adj = 2.5) # Adjust 'adj' for horizontal shifting

segments(x0 = -1, y0 = 1:13, x1 = 0.5, y1 = 1:13, col = "#40808080", lwd = 1.5)
abline(v=c(seq(from=-1, to=0.5, by=0.25)),col="#80808080",lty=2)
# Empty graph layout 

cols<-c("#00FBFF", 
        "#8C00FF",
        "#FFADD0",
        "#09C9FF",
        "#00FF33",
        
        "#0FEA33",
        "#FFEA00",
        "#FFA600",
        "#00FF33",
        "#EEEA00",
        
        "#EEEDD0",
        "#FFFD0D",
        "#DDEE0D"
) 

ys<-seq(from=1,to=13, by=1)
ys<-rev(ys)

labs<-c("Czechs (Control group)",    
        "Czechs - More frequent users",
        "Czechs - Less frequent users",
        "Australia/NewZ - More freq. users",
        "Australia/NewZ - Less freq. users",
        "Turkey - More frequent users",
        "Turkey - Less frequent users",
        "Colombia - More frequent users",
        "Colombia - Less frequent users",
        "Vietnamese - More freq. users",
        "Vietnamese - Less freq. users",
        "South Africans - More freq. users",
        "South Africans - Less freq. users"
)

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,13),
                               Mean=rep(0,13),Up_CI=rep(0,13)) 

for (i in 1:length(M2_posts_ATR)) {
  Coefficients_check$Mean[i]<-mean(M2_posts_ATR[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(M2_posts_ATR)) {
  Coefficients_check$Low_CI[i]<-PI(M2_posts_ATR[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(M2_posts_ATR)) {
  Coefficients_check$Up_CI[i]<-PI(M2_posts_ATR[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))

cex_t <- 1.25
cex_t2 <- 0.75
pls <- 0.4

text("Mean", x=0.8, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.6, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=1.0, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


for(i in 1:length(M2_posts_ATR)){ 
  toplot<-M2_posts_ATR[[i]]
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
  axis(2, at=ycoord, labels=textlab, srt=45, las=2, cex.axis=1.2)
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=0.8, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=0.6, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=1.0, y=ys[i], cex=cex_t2)
  for (i in 1:13) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.01)+ycoord[i],border=T, lwd = 1, col=colpol))
  }
  #lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

abline(v=0.0, col="#999999", lwd=2, lty=1)

dev.off()

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# Trustworthiness

M2_posts_TRU <- list(
  # 1 CZECH ORIGINAL 
  CZ_orig = post_GMM$aT + post_GMM$a_group_T[,7],
  # 2 CZECH - above - median users.
  CZ_Intensive_Users = post_GMM$aT + post_GMM$a_group_T[,5],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = post_GMM$aT + post_GMM$a_group_T[,6],
  # 4 Australia - above - median users  
  AUS_Intensive_User = post_GMM$aT + post_GMM$a_group_T[,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = post_GMM$aT + post_GMM$a_group_T[,2],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = post_GMM$aT + post_GMM$a_group_T[,10],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = post_GMM$aT + post_GMM$a_group_T[,11],
  # Colombia - above - median users 
  COL_Intensive_User = post_GMM$aT + post_GMM$a_group_T[,3], 
  # COlombia - below - median users
  COL_Less_Intensive = post_GMM$aT + post_GMM$a_group_T[,4],
  # Vietnam - above - median users 
  VN_Intensive_User = post_GMM$aT + post_GMM$a_group_T[,12],
  # Vietnam - below - median users 
  VN_Less_Intensive = post_GMM$aT + post_GMM$a_group_T[,13],
  # RSA - above - median users 
  RSA_Intenstive_User = post_GMM$aT + post_GMM$a_group_T[,8],
  # RSA - below - median users 
  RSA_Less_Intensive = post_GMM$aT + post_GMM$a_group_T[,9]
)

summary.data.frame(M2_posts_ATR)
str(M2_posts_ATR) # Need 13 rows 

tiff("M2_with_GMM_Trustworthiness_04_02_2025.tiff",width=20,height=12,units="cm",res=600,compression="lzw")
par(mar=c(3.1, 18.1, 1.1, 2.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-1,1),ylim=c(1,13.5),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Czech Faces - Rated Trustworthiness: Model *with* GMM", side = 3, line = 0.20, adj = 2.5) # Adjust 'adj' for horizontal shifting

segments(x0 = -1, y0 = 1:13, x1 = 0.5, y1 = 1:13, col = "#40808080", lwd = 1.5)
abline(v=c(seq(from=-1, to=0.5, by=0.25)),col="#80808080",lty=2)
# Empty graph layout 

ys<-seq(from=1,to=13, by=1)
ys<-rev(ys)

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,13),
                               Mean=rep(0,13),Up_CI=rep(0,13)) 

for (i in 1:length(M2_posts_ATR)) {
  Coefficients_check$Mean[i]<-mean(M2_posts_ATR[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(M2_posts_ATR)) {
  Coefficients_check$Low_CI[i]<-PI(M2_posts_ATR[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(M2_posts_ATR)) {
  Coefficients_check$Up_CI[i]<-PI(M2_posts_ATR[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))

cex_t <- 1.25
cex_t2 <- 0.75
pls <- 0.4

text("Mean", x=0.8, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.6, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=1.0, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


for(i in 1:length(M2_posts_ATR)){ 
  toplot<-M2_posts_ATR[[i]]
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
  axis(2, at=ycoord, labels=textlab, srt=45, las=2, cex.axis=1.2)
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=0.8, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=0.6, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=1.0, y=ys[i], cex=cex_t2)
  for (i in 1:13) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.01)+ycoord[i],border=T, lwd = 1, col=colpol))
  }
  #lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

abline(v=0.0, col="#999999", lwd=2, lty=1)

dev.off()

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# Correlations - Model WITH GMM 

# Attractiveness:

# List of the relevant correlations (such that deserve comparisons):
str(post_GMM)
str(post_GMM$Rho_FSMUi_A) 
str(post_GMM$Rho_FSMUi_T) 


# Czech...
# CZE control - CZE more frequent users 
# CZE control - CZE less frequent users
# CZE more vs. CZE less frequent users

post_CorAT_plot <- list( # Run the means without (cursor selection). The numbers shall be reasonable (0.5 - 0.9), it's a correlation of psychological scales
  # Attractiveness
  Atr_CZ_Orig_CZ_Above = post_GMM$Rho_FSMUi_A[,7,5], # mean(post_GMM$Rho_FSMUi_A[,7,5])
  Atr_CZ_Orig_CZ_below = post_GMM$Rho_FSMUi_A[,7,6], # mean(post_GMM$Rho_FSMUi_A[,7,6])
  Atr_CZ_above_CZ_below = post_GMM$Rho_FSMUi_A[,5,6], # mean(post_GMM$Rho_FSMUi_A[,5,6])
  
  # Australia & New Zealand
  # CZE control - AUS/NZE more freq. users 
  # CZE control - AUS/NZE less freq. users
  # AUS/NZE more vs. AUS/NZE less freq. users
  Atr_CZ_Orig_AUS_Above = post_GMM$Rho_FSMUi_A[,7,1], # mean(post_GMM$Rho_FSMUi_A[,7,1])
  Atr_CZ_Orig_AUS_Below = post_GMM$Rho_FSMUi_A[,7,2], # mean(post_GMM$Rho_FSMUi_A[,7,2])
  Atr_AUS_above_AUS_Below = post_GMM$Rho_FSMUi_A[,2,1], # mean(post_GMM$Rho_FSMUi_A[,2,1])
  
  # Turkey
  # CZE control - TUR more freq. users 
  # CZE control - TUR less freq. users
  # TUR more vs. TUR less freq. users
  Atr_CZ_Orig_TUR_Above = post_GMM$Rho_FSMUi_A[,7,10], # mean(post_GMM$Rho_FSMUi_A[,7,10])
  Atr_CZ_Orig_TUR_Below = post_GMM$Rho_FSMUi_A[,7,11], # mean(post_GMM$Rho_FSMUi_A[,7,11])
  Atr_TUR_above_TUR_Below = post_GMM$Rho_FSMUi_A[,10,11], # mean(post_GMM$Rho_FSMUi_A[,10,11])
  
  # Colombia 
  # CZE control - COL more freq. users 
  # CZE control - COL less freq. users
  # COL more vs. COL less freq. users
  Atr_CZ_Orig_COL_Above = post_GMM$Rho_FSMUi_A[,7,3], # mean(post_GMM$Rho_FSMUi_A[,7,3])
  Atr_CZ_Orig_COL_Below = post_GMM$Rho_FSMUi_A[,7,4], # mean(post_GMM$Rho_FSMUi_A[,7,4])
  Atr_COL_above_COL_Below = post_GMM$Rho_FSMUi_A[,3,4], # mean(post_GMM$Rho_FSMUi_A[,3,4])
  
  # Vietnam 
  # CZE control - VNM more freq. users 
  # CZE control - VNM less freq. users
  # VNM more vs. VNM less freq. users
  Atr_CZ_Orig_VNM_Above = post_GMM$Rho_FSMUi_A[,7,12], # mean(post_GMM$Rho_FSMUi_A[,7,12])
  Atr_CZ_Orig_VNM_Below = post_GMM$Rho_FSMUi_A[,7,13], # mean(post_GMM$Rho_FSMUi_A[,7,13])
  Atr_VNM_above_VNM_Below = post_GMM$Rho_FSMUi_A[,12,13], # mean(post_GMM$Rho_FSMUi_A[,12,13])
  
  # South Africa...
  # CZE control - ZAF more freq. users 
  # CZE control - ZAF less freq. users
  # ZAF more vs. ZAF less freq. users
  Atr_CZ_Orig_ZAF_Above = post_GMM$Rho_FSMUi_A[,7,8], # mean(post_GMM$Rho_FSMUi_A[,7,8])
  Atr_CZ_Orig_ZAF_Below = post_GMM$Rho_FSMUi_A[,7,9], # mean(post_GMM$Rho_FSMUi_A[,7,9])
  Atr_ZAF_above_ZAF_Below = post_GMM$Rho_FSMUi_A[,8,9], # mean(post_GMM$Rho_FSMUi_A[,8,9])
  
  
  
  # Trustworthiness
  Tru_CZ_Orig_CZ_Above = post_GMM$Rho_FSMUi_T[,7,5], # mean(post_GMM$Rho_FSMUi_T[,7,5])
  Tru_CZ_Orig_CZ_below = post_GMM$Rho_FSMUi_T[,7,6], # mean(post_GMM$Rho_FSMUi_T[,7,6])
  Tru_CZ_above_CZ_below = post_GMM$Rho_FSMUi_T[,5,6], # mean(post_GMM$Rho_FSMUi_T[,5,6])
  
  # Australia & New Zealand
  # CZE control - AUS/NZE more freq. users 
  # CZE control - AUS/NZE less freq. users
  # AUS/NZE more vs. AUS/NZE less freq. users
  Tru_CZ_Orig_AUS_Above = post_GMM$Rho_FSMUi_T[,7,1], # mean(post_GMM$Rho_FSMUi_T[,7,1])
  Tru_CZ_Orig_AUS_Below = post_GMM$Rho_FSMUi_T[,7,2], # mean(post_GMM$Rho_FSMUi_T[,7,2])
  Tru_AUS_above_AUS_Below = post_GMM$Rho_FSMUi_T[,2,1], # mean(post_GMM$Rho_FSMUi_T[,2,1])
  
  # Turkey
  # CZE control - TUR more freq. users 
  # CZE control - TUR less freq. users
  # TUR more vs. TUR less freq. users
  Tru_CZ_Orig_TUR_Above = post_GMM$Rho_FSMUi_T[,7,10], # mean(post_GMM$Rho_FSMUi_T[,7,10])
  Tru_CZ_Orig_TUR_Below = post_GMM$Rho_FSMUi_T[,7,11], # mean(post_GMM$Rho_FSMUi_T[,7,11])
  Tru_TUR_above_TUR_Below = post_GMM$Rho_FSMUi_T[,10,11], # mean(post_GMM$Rho_FSMUi_T[,10,11])
  
  # Colombia 
  # CZE control - COL more freq. users 
  # CZE control - COL less freq. users
  # COL more vs. COL less freq. users
  Tru_CZ_Orig_COL_Above = post_GMM$Rho_FSMUi_T[,7,3], # mean(post_GMM$Rho_FSMUi_T[,7,3])
  Tru_CZ_Orig_COL_Below = post_GMM$Rho_FSMUi_T[,7,4], # mean(post_GMM$Rho_FSMUi_T[,7,4])
  Tru_COL_above_COL_Below = post_GMM$Rho_FSMUi_T[,3,4], # mean(post_GMM$Rho_FSMUi_T[,3,4])
  
  # Vietnam 
  # CZE control - VNM more freq. users 
  # CZE control - VNM less freq. users
  # VNM more vs. VNM less freq. users
  Tru_CZ_Orig_VNM_Above = post_GMM$Rho_FSMUi_T[,7,12], # mean(post_GMM$Rho_FSMUi_T[,7,12])
  Tru_CZ_Orig_VNM_Below = post_GMM$Rho_FSMUi_T[,7,13], # mean(post_GMM$Rho_FSMUi_T[,7,13])
  Tru_VNM_above_VNM_Below = post_GMM$Rho_FSMUi_T[,12,13], # mean(post_GMM$Rho_FSMUi_T[,12,13])
  
  # South Africa...
  # CZE control - ZAF more freq. users 
  # CZE control - ZAF less freq. users
  # ZAF more vs. ZAF less freq. users
  Tru_CZ_Orig_ZAF_Above = post_GMM$Rho_FSMUi_T[,7,8], # mean(post_GMM$Rho_FSMUi_T[,7,8])
  Tru_CZ_Orig_ZAF_Below = post_GMM$Rho_FSMUi_T[,7,9], # mean(post_GMM$Rho_FSMUi_T[,7,9])
  Tru_ZAF_above_ZAF_Below = post_GMM$Rho_FSMUi_T[,8,9] # mean(post_GMM$Rho_FSMUi_T[,8,9])
)


tiff("M2_Correlations_ATTR_model_with_GMM_04_02_25.tiff",width=30,height=20.5,units="cm",res=600,compression="lzw")
par(mar=c(1.7, 19.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(0,1),ylim=c(1,43),yaxt="n",bty="n", ylab="",xlab="", cex.axis=1)
segments(x0 = 0.4, y0 = c(1:3,
                          4.5:6.5,
                          8:10,
                          11.5:13.5,
                          15:17,
                          18.5:20.5,
                          23:25,
                          26.5:28.5,
                          30:32,
                          33.5:35.5,
                          37:39,
                          40.5:42.5), x1 = 1.0, y1 = c(1:3,
                                                       4.5:6.5,
                                                       8:10,
                                                       11.5:13.5,
                                                       15:17,
                                                       18.5:20.5,
                                                       23:25,
                                                       26.5:28.5,
                                                       30:32,
                                                       33.5:35.5,
                                                       37:39,
                                                       40.5:42.5), col = "#40808080", lwd = 1.5)

abline(v=c(seq(from=0.5, to=1, by=0.1)),col="#80808080",lty=2)
abline(v=c(0,0.5,1),col="darkgrey",lty=2, lwd=3)

# Empty graph layout 
cols<-c("#d1584f", 
        "#9e3931",
        "#825d5b", 
        
        "#83f287",
        "#52bf56",
        "#5f9c61", 
        
        "#719deb",
        "#5f7db3", 
        "#305391",
        
        "#eef58c",
        "#c6cf4c", 
        "#81854c",
        
        "#d1584f", 
        "#9e3931",
        "#825d5b", 
        
        "#83f287",
        "#52bf56",
        "#5f9c61", 
        
        "#719deb",
        "#5f7db3", 
        "#305391",
        
        "#eef58c",
        "#c6cf4c", 
        "#81854c",
        
        "#d1584f", 
        "#9e3931",
        "#825d5b", 
        
        "#83f287",
        "#52bf56",
        "#5f9c61", 
        
        "#719deb",
        "#5f7db3", 
        "#305391",
        
        "#eef58c",
        "#c6cf4c", 
        "#81854c"
) 

ys<-c(1:3,
      4.5:6.5,
      8:10,
      11.5:13.5,
      15:17,
      18.5:20.5,
      23:25,
      26.5:28.5,
      30:32,
      33.5:35.5,
      37:39,
      40.5:42.5)

ys<-rev(ys)

COLROW_names<- c("(A) CZE control - CZE more frequent users", 
                 "(A) CZE control - CZE less frequent users",
                 "(A) CZE more - CZE less frequent users",
                 
                 "(A) AUS/NZE more freq. users - CZE control",
                 "(A) AUS/NZE less freq. users - CZE control",
                 "(A) AUS/NZE more - AUS/NZE less freq. users",
                 
                 "(A) TUR more freq. users - CZE control", 
                 "(A) TUR less freq. users - CZE control",
                 "(A) TUR more - TUR less freq. users",
                 
                 "(A) COL more freq. users - CZE control", 
                 "(A) COL less freq. users - CZE control",
                 "(A) COL more vs. COL less freq. users",
                 
                 "(A) VNM more freq. users - CZE control", 
                 "(A) VNM less freq. users - CZE control",
                 "(A) VNM more - VNM less freq. users",
                 
                 "(A) ZAF more freq. users - CZE control", 
                 "(A) ZAF less freq. users - CZE control",
                 "(A) ZAF more - ZAF less freq. users",
                 
                 
                 "(T) CZE control - CZE more frequent users", 
                 "(T) CZE control - CZE less frequent users",
                 "(T) CZE more - CZE less frequent users",
                 
                 "(T) AUS/NZE more freq. users - CZE control",
                 "(T) AUS/NZE less freq. users - CZE control",
                 "(T) AUS/NZE more - AUS/NZE less freq. users",
                 
                 "(T) TUR more freq. users - CZE control", 
                 "(T) TUR less freq. users - CZE control",
                 "(T) TUR more - TUR less freq. users",
                 
                 "(T) COL more freq. users - CZE control", 
                 "(T) COL less freq. users - CZE control",
                 "(T) COL more - COL less freq. users",
                 
                 "(T) VNM more freq. users - CZE control", 
                 "(T) VNM less freq. users - CZE control",
                 "(T) VNM more - VNM less freq. users",
                 
                 "(T) ZAF more freq. users - CZE control", 
                 "(T) ZAF less freq. users - CZE control",
                 "(T) ZAF more - ZAF less freq. users"
)

Coefficients_check<-data.frame(Name=COLROW_names,Low_CI=rep(0,36),
                               Mean=rep(0,36),Up_CI=rep(0,36)) # Sorry, not in a mood to reinvent, how to .... do it aesthetically
for (i in 1:length(post_CorAT_plot)) {
  Coefficients_check$Mean[i]<-mean(post_CorAT_plot[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(post_CorAT_plot)) {
  Coefficients_check$Low_CI[i]<-PI(post_CorAT_plot[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(post_CorAT_plot)) {
  Coefficients_check$Up_CI[i]<-PI(post_CorAT_plot[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))

cex_t <- 1.05
pls <- 1.2
text("Mean", x=0.2, y=max(ys+pls), cex=cex_t, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.10, y=max(ys+pls), cex=cex_t, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=0.30, y=max(ys+pls), cex=cex_t, offset=0.5, xpd=NA, srt=0)

for(i in 1:length(post_CorAT_plot)){ 
  toplot<-post_CorAT_plot[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  textlab<-COLROW_names[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  axis(2, at=ycoord, labels=textlab, las=2, cex.axis=1.05, tick=F)
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=0.2, y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=0.1,y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=0.3,y=ys[i], cex=cex_t)
  for (i in 1:36) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.002)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  # lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

dev.off()


# Two tables - top, attractiveness, bottom, trustworthiness

labels_x <- c("AUS/NZE Freq", "AUS/NZE Infreq", "COL Freq","COL Infreq", "CZE Control","CZE Freq", "CZE Infreq","ZAF Freq", "ZAF Infreq",
              "TUR Freq", "TUR Infreq", "VNM Freq", "VNM Infreq")
labels_y <- labels_x

# Attr
str(post_GMM$Rho_FSMUi_A)


tiff("M2_Correlations_Heatmap_ATTR_model_WITH_GMM_04_02_25.tiff",width=30,height=25,units="cm",res=600,compression="lzw")
cor_table_mean <- apply(post_GMM$Rho_FSMUi_A,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(post_GMM$Rho_FSMUi_A,c(2,3),PI, prob=0.95)
cor_table_LOWCI <- cor_table_CI[1,,] 
cor_table_UPWCI <- cor_table_CI[2,,] 

font1 <- 15
font2 <- 18

# I want the coefficient to be printed above the axis, while the CIs should be below it: 
col_fun <- colorRamp2(c(0.5, 0.75, 1), c("salmon", "white", "lightblue"))
Heatmap((as.matrix(cor_table_mean)), column_title ="Attractiveness: All correlations (model with GMM)", cluster_rows=FALSE, 
        cluster_columns = FALSE, column_title_gp = gpar(fontsize=font1), column_names_gp = gpar(fontsize = font1), 
        row_names_gp = gpar(fontsize = font1), column_names_rot = 45, column_names_side = "top", name = "r", 
        col = col_fun,
        cell_fun = function(j, i, x, y, width, height, fill) {
          if (j>i) {c(grid.text(sprintf("%.3f", cor_table_mean[i, j]), x, y, gp = gpar(fontsize = font2),just="top"))}
          else if (j<i){c(grid.text(sprintf("%.3f", cor_table_LOWCI[i, j]), x, y, gp = gpar(fontsize = font1),just = c(0.65,-0.35)),
                          grid.text(sprintf("%.3f", cor_table_UPWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.45,0.95))
          )}
        },rect_gp = gpar(col = "white", lwd = 1), row_title = "95% Compatibility Interval")

dev.off()



tiff("M2_Correlations_Heatmap_TRUSTW_model_WITH_GMM_04_02_25.tiff",width=30,height=25,units="cm",res=600,compression="lzw")
cor_table_mean <- apply(post_GMM$Rho_FSMUi_T,c(2,3),mean)
cor_table_mean <- as.data.frame(cor_table_mean)
colnames(cor_table_mean) <- labels_y
rownames(cor_table_mean) <- labels_x
cor_table_CI <- apply(post_GMM$Rho_FSMUi_T,c(2,3),PI, prob=0.95)
cor_table_LOWCI <- cor_table_CI[1,,] 
cor_table_UPWCI <- cor_table_CI[2,,] 

# I want the coefficient to be printed above the axis, while the CIs should be below it: 
col_fun <- colorRamp2(c(0.5, 0.75, 1), c("salmon", "white", "lightblue"))
Heatmap((as.matrix(cor_table_mean)), column_title ="Trustworthiness: All correlations (model with GMM)", cluster_rows=FALSE, 
        cluster_columns = FALSE, column_title_gp = gpar(fontsize=font1), column_names_gp = gpar(fontsize = font1), 
        row_names_gp = gpar(fontsize = font1), column_names_rot = 45, column_names_side = "top", name = "r", 
        col = col_fun,
        cell_fun = function(j, i, x, y, width, height, fill) {
          if (j>i) {c(grid.text(sprintf("%.3f", cor_table_mean[i, j]), x, y, gp = gpar(fontsize = font2),just="top"))}
          else if (j<i){c(grid.text(sprintf("%.3f", cor_table_LOWCI[i, j]), x, y, gp = gpar(fontsize = font1),just = c(0.65,-0.35)),
                          grid.text(sprintf("%.3f", cor_table_UPWCI[i, j]), x, y, gp = gpar(fontsize = font1), just = c(0.45,0.95))
          )}
        },rect_gp = gpar(col = "white", lwd = 1), row_title = "95% Compatibility Interval")

dev.off()

#.-.-.-.--.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-.-.-.--.-.-.-.-.--.-.-.--.-.-.-.-.

# Morphometric predictors - fixed and varying effects: 

# 1 Attractiveness & Trustworthiness - fixed effects

str(post_GMM)

M2_posts_GMM_Fixed <- list(
  b_age_A = post_GMM$b_age_A,    # Age (A)
  b_L_A = post_GMM$b_L_A,        # Lightness (A)
  b_dist_A = post_GMM$b_dist_A,  # Distinctiveness (A)
  b_FA_A = post_GMM$b_FA_A,      # Asymmetry (A)
  b_sshd_A = post_GMM$b_sshd_A,  # SShD (A)
  b_age_T = post_GMM$b_age_T,    # Age (T)
  b_L_T = post_GMM$b_L_T,        # Lightness (T)
  b_dist_T = post_GMM$b_dist_T,  # Distinctiveness (T)
  b_FA_T = post_GMM$b_FA_T,      # Asymmetry (T)
  b_sshd_T = post_GMM$b_sshd_T   # SShD (T)
)

summary.data.frame(M2_posts_GMM_Fixed)
str(M2_posts_GMM_Fixed) # Need 10 rows 



tiff("M2_WITH_GMM_PREDICTORS_FIXED_05_02_2025.tiff",width=15,height=30,units="cm",res=600,compression="lzw")
par(mar=c(3.1, 14.1, 1.1, 2.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.5,0.25),ylim=c(1,11),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Linear predictors' fixed effects", side = 3, line = 0.20, adj = 2.5) # Adjust 'adj' for horizontal shifting

segments(x0 = -0.5, y0 = c(1:5,6.5:10.5), x1 = 0.2, y1 = c(1:5,6.5:10.5), col = "#40808080", lwd = 1.5)
abline(v=c(seq(from=-0.4, to=0.2, by=0.1)),col="#80808080",lty=2)
# Empty graph layout 

cols<-c("#d1584f", 
        "#825d5b", 
        "#83f287",
        "#5f9c61", 
        "#719deb",
                
        "#d1584f", 
        "#825d5b", 
        "#83f287",
        "#5f9c61", 
        "#719deb"
) 

ys<-c(seq(from=1,to=5, by=1),seq(from=6.5, to=10.5, by=1))
ys<-rev(ys)

labs<-c("Age (A)",
        "Lightness (A)",
        "Distinctiveness (A)",
        "Asymmetry (A)",
        "SexShapeDim (A)",
        "Age (T)",
        "Lightness (T)",
        "Distinctiveness (T)",
        "Asymmetry (T)",
        "SexShapeDim (T)"
)

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,10),
                               Mean=rep(0,10),Up_CI=rep(0,10)) 

for (i in 1:length(M2_posts_GMM_Fixed)) {
  Coefficients_check$Mean[i]<-mean(M2_posts_GMM_Fixed[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(M2_posts_GMM_Fixed)) {
  Coefficients_check$Low_CI[i]<-PI(M2_posts_GMM_Fixed[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(M2_posts_GMM_Fixed)) {
  Coefficients_check$Up_CI[i]<-PI(M2_posts_GMM_Fixed[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))

cex_t <- 1.25
cex_t2 <- 0.75
pls <- 0.4

text("Mean", x=0.8, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.6, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=1.0, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


for(i in 1:length(M2_posts_GMM_Fixed)){ 
  toplot<-M2_posts_GMM_Fixed[[i]]
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
  for (i in 1:13) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.001)+ycoord[i],border=T, lwd = 1, col=colpol))
  }
  #lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.1, x1 = ci[1], y1 = ycoord+0.1, lwd = 4, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.1, x1 = ci[2], y1 = ycoord+0.1, lwd = 4, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

abline(v=0.0, col="#999999", lwd=2, lty=1)

dev.off()


# 2 Attractiveness varying effects

str(post_GMM)
str(post_GMM$f_per_group_pr_A)

M2_posts_GMM_Attr_Varying <- list(
  Age_CZ_control = (post_GMM$b_age_A + post_GMM$f_per_group_pr_A[,7,1]),    # Age (A): CZE Control
  Age_CZ_Frequent = (post_GMM$b_age_A + post_GMM$f_per_group_pr_A[,5,1]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (post_GMM$b_age_A + post_GMM$f_per_group_pr_A[,6,1]),   # Age (A): CZE Less Freq. Users
  
  Age_AUS_Freq = (post_GMM$b_age_A + post_GMM$f_per_group_pr_A[,1,1]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (post_GMM$b_age_A + post_GMM$f_per_group_pr_A[,2,1]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_TUR_Freq = (post_GMM$b_age_A + post_GMM$f_per_group_pr_A[,10,1]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (post_GMM$b_age_A + post_GMM$f_per_group_pr_A[,11,1]),   # Age (A): TUR Less Freq. Users
  
  Age_COL_Freq = (post_GMM$b_age_A + post_GMM$f_per_group_pr_A[,3,1]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (post_GMM$b_age_A + post_GMM$f_per_group_pr_A[,4,1]),   # Age (A): COL Less Freq. Users
  
  Age_VNM_Freq = (post_GMM$b_age_A + post_GMM$f_per_group_pr_A[,12,1]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (post_GMM$b_age_A + post_GMM$f_per_group_pr_A[,13,1]),   # Age (A): COL Less Freq. Users
  
  Age_ZAF_Freq = (post_GMM$b_age_A + post_GMM$f_per_group_pr_A[,8,1]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (post_GMM$b_age_A + post_GMM$f_per_group_pr_A[,9,1]),   # Age (A): ZAF Less Freq. Users
  
  
  Dist_CZ_control = (post_GMM$b_dist_A + post_GMM$f_per_group_pr_A[,7,2]),    # Dist (A): CZE Control
  Dist_CZ_Frequent = (post_GMM$b_dist_A + post_GMM$f_per_group_pr_A[,5,2]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (post_GMM$b_dist_A + post_GMM$f_per_group_pr_A[,6,2]),   # Dist (A): CZE Less Freq. Users
  
  Dist_AUS_Freq = (post_GMM$b_dist_A + post_GMM$f_per_group_pr_A[,1,2]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (post_GMM$b_dist_A + post_GMM$f_per_group_pr_A[,2,2]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_TUR_Freq = (post_GMM$b_dist_A + post_GMM$f_per_group_pr_A[,10,2]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (post_GMM$b_dist_A + post_GMM$f_per_group_pr_A[,11,2]),   # Dist (A): TUR Less Freq. Users
  
  Dist_COL_Freq = (post_GMM$b_dist_A + post_GMM$f_per_group_pr_A[,3,2]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (post_GMM$b_dist_A + post_GMM$f_per_group_pr_A[,4,2]),   # Dist (A): COL Less Freq. Users
  
  Dist_VNM_Freq = (post_GMM$b_dist_A + post_GMM$f_per_group_pr_A[,12,2]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (post_GMM$b_dist_A + post_GMM$f_per_group_pr_A[,13,2]),   # Dist (A): COL Less Freq. Users
  
  Dist_ZAF_Freq = (post_GMM$b_dist_A + post_GMM$f_per_group_pr_A[,8,2]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (post_GMM$b_dist_A + post_GMM$f_per_group_pr_A[,9,2]),   # Dist (A): ZAF Less Freq. Users
  
  
  Asym_CZ_control = (post_GMM$b_FA_A + post_GMM$f_per_group_pr_A[,7,3]),    # Asym (A): CZE Control
  Asym_CZ_Frequent = (post_GMM$b_FA_A + post_GMM$f_per_group_pr_A[,5,3]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (post_GMM$b_FA_A + post_GMM$f_per_group_pr_A[,6,3]),   # Asym (A): CZE Less Freq. Users
  
  Asym_AUS_Freq = (post_GMM$b_FA_A + post_GMM$f_per_group_pr_A[,1,3]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (post_GMM$b_FA_A + post_GMM$f_per_group_pr_A[,2,3]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_TUR_Freq = (post_GMM$b_FA_A + post_GMM$f_per_group_pr_A[,10,3]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (post_GMM$b_FA_A + post_GMM$f_per_group_pr_A[,11,3]),   # Asym (A): TUR Less Freq. Users
  
  Asym_COL_Freq = (post_GMM$b_FA_A + post_GMM$f_per_group_pr_A[,3,3]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (post_GMM$b_FA_A + post_GMM$f_per_group_pr_A[,4,3]),   # Asym (A): COL Less Freq. Users
  
  Asym_VNM_Freq = (post_GMM$b_FA_A + post_GMM$f_per_group_pr_A[,12,3]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (post_GMM$b_FA_A + post_GMM$f_per_group_pr_A[,13,3]),   # Asym (A): COL Less Freq. Users
  
  Asym_ZAF_Freq = (post_GMM$b_FA_A + post_GMM$f_per_group_pr_A[,8,3]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (post_GMM$b_FA_A + post_GMM$f_per_group_pr_A[,9,3]),   # Asym (A): ZAF Less Freq. Users
  
  
  SexTyp_CZ_control = (post_GMM$b_sshd_A + post_GMM$f_per_group_pr_A[,7,4]),    # SexTyp (A): CZE Control
  SexTyp_CZ_Frequent = (post_GMM$b_sshd_A + post_GMM$f_per_group_pr_A[,5,4]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (post_GMM$b_sshd_A + post_GMM$f_per_group_pr_A[,6,4]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_AUS_Freq = (post_GMM$b_sshd_A + post_GMM$f_per_group_pr_A[,1,4]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (post_GMM$b_sshd_A + post_GMM$f_per_group_pr_A[,2,4]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_TUR_Freq = (post_GMM$b_sshd_A + post_GMM$f_per_group_pr_A[,10,4]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (post_GMM$b_sshd_A + post_GMM$f_per_group_pr_A[,11,4]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_COL_Freq = (post_GMM$b_sshd_A + post_GMM$f_per_group_pr_A[,3,4]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (post_GMM$b_sshd_A + post_GMM$f_per_group_pr_A[,4,4]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_VNM_Freq = (post_GMM$b_sshd_A + post_GMM$f_per_group_pr_A[,12,4]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (post_GMM$b_sshd_A + post_GMM$f_per_group_pr_A[,13,4]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_ZAF_Freq = (post_GMM$b_sshd_A + post_GMM$f_per_group_pr_A[,8,4]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (post_GMM$b_sshd_A + post_GMM$f_per_group_pr_A[,9,4]),   # SexTyp (A): ZAF Less Freq. Users
  
  
  L_CZ_control = (post_GMM$b_L_A + post_GMM$f_per_group_pr_A[,7,5]),    # L (A): CZE Control
  L_CZ_Frequent = (post_GMM$b_L_A + post_GMM$f_per_group_pr_A[,5,5]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (post_GMM$b_L_A + post_GMM$f_per_group_pr_A[,6,5]),   # L (A): CZE Less Freq. Users
  
  L_AUS_Freq = (post_GMM$b_L_A + post_GMM$f_per_group_pr_A[,1,5]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (post_GMM$b_L_A + post_GMM$f_per_group_pr_A[,2,5]),   # L (A): AUS/NZE Less Freq. Users
  
  L_TUR_Freq = (post_GMM$b_L_A + post_GMM$f_per_group_pr_A[,10,5]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (post_GMM$b_L_A + post_GMM$f_per_group_pr_A[,11,5]),   # L (A): TUR Less Freq. Users
  
  L_COL_Freq = (post_GMM$b_L_A + post_GMM$f_per_group_pr_A[,3,5]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (post_GMM$b_L_A + post_GMM$f_per_group_pr_A[,4,5]),   # L (A): COL Less Freq. Users
  
  L_VNM_Freq = (post_GMM$b_L_A + post_GMM$f_per_group_pr_A[,12,5]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (post_GMM$b_L_A + post_GMM$f_per_group_pr_A[,13,5]),   # L (A): COL Less Freq. Users
  
  L_ZAF_Freq = (post_GMM$b_L_A + post_GMM$f_per_group_pr_A[,8,5]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (post_GMM$b_L_A + post_GMM$f_per_group_pr_A[,9,5])   # L (A): ZAF Less Freq. Users
)


summary.data.frame(M2_posts_GMM_Attr_Varying)
str(M2_posts_GMM_Attr_Varying) # Need 65 rows 



tiff("M2_WITH_GMM_PREDICTORS_ATR_VARYING_05_02_2025.tiff",width=25,height=30,units="cm",res=600,compression="lzw")
par(mar=c(2.1, 14.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.5),ylim=c(1,66),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
#mtext("Linear predictors' varying effects", side = 3, line = 0.20, adj = 2.5) # Adjust 'adj' for horizontal shifting

segments(x0 = -0.25, y0 = c(1:13,14.5:26.5,28:40,41.5:53.5,55:67), x1 = 0.2, 
         y1 = c(1:13,14.5:26.5,28:40,41.5:53.5,55:67), col = "#40808080", lwd = 1.5)
abline(v=c(seq(from=-0.2, to=0.2, by=0.1)),col="#80808080",lty=2)
# Empty graph layout 

cols<-c("#c56043", "#e04b5c", "#de5353", "#c25c42", "#d85652", "#c8585d", "#de534b", "#cd5440", "#d35d5e", "#c3615a", "#d46655", "#cf5552", "#c65f5d",
        "#856554", "#896757", "#815b69", "#8f6756", "#86554d", "#8a675d", "#85586a", "#805158", "#855d52", "#8f4e50", "#8e5960", "#80685a", "#75584c",
        "#85b68f", "#77bf98", "#86b18f", "#85b589", "#7eB68d", "#8bB37a", "#80B491", "#65Bd7d", "#81B784", "#8bb883", "#8ebf92", "#7bBe8f", "#91bd79",
        "#6b9168", "#69925b", "#689d55", "#6ba261", "#5aaa6b", "#599866", "#519c58", "#55a755", "#5caa53", "#5c9c60", "#588f5b", "#5e9d6f", "#5aa757",
        "#64a9e6", "#79a5ee", "#809be9", "#6d9dfa", "#6899ec", "#6facde", "#77abed", "#63aaf0", "#63a4e4", "#74a5e3", "#79a9eb", "#6aaaf6", "#78a4dd"
        
) 

ys<-c(1:13,14.5:26.5,28:40,41.5:53.5,55:67)
ys<-rev(ys)

labs <- c("CZE Control",
  "CZE More Frequent U",
  "CZE Less Freqeunt U",
  
  "AUS/NZE More Frequent U",
  "AUS/NZE Less Frequent U",
  
  "TUR More Frequent U",
  "TUR Less Frequent U",
  
  "COL More Frequent U",
  "COL Less Frequent U",
  
  "COL More Frequent U",
  "COL Less Frequent U",
  
  "ZAF More Frequent U",
  "ZAF Less Frequent U")

labs <- rep(labs,5)

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,65),
                               Mean=rep(0,65),Up_CI=rep(0,65)) 

for (i in 1:length(M2_posts_GMM_Attr_Varying)) {
  Coefficients_check$Mean[i]<-mean(M2_posts_GMM_Attr_Varying[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(M2_posts_GMM_Attr_Varying)) {
  Coefficients_check$Low_CI[i]<-PI(M2_posts_GMM_Attr_Varying[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(M2_posts_GMM_Attr_Varying)) {
  Coefficients_check$Up_CI[i]<-PI(M2_posts_GMM_Attr_Varying[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))

cex_t <- 1.25
cex_t2 <- 1
pls <- 1

text("Mean", x=0.325, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.25, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=0.4, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


for(i in 1:length(M2_posts_GMM_Attr_Varying)){ 
  toplot<-M2_posts_GMM_Attr_Varying[[i]]
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

abline(v=0.0, col="#999999", lwd=2, lty=1)

dev.off()


# 3 TRUSTWORTHINESS varying effects

str(post_GMM)
str(post_GMM$f_per_group_pr_T)

M2_posts_GMM_TRUSTW_Varying <- list(
  Age_CZ_control = (post_GMM$b_age_T + post_GMM$f_per_group_pr_T[,7,1]),    # Age (A): CZE Control
  Age_CZ_Frequent = (post_GMM$b_age_T + post_GMM$f_per_group_pr_T[,5,1]),   # Age (A): CZE More Freq. Users
  Age_CZ_Infreq = (post_GMM$b_age_T + post_GMM$f_per_group_pr_T[,6,1]),   # Age (A): CZE Less Freq. Users
  
  Age_AUS_Freq = (post_GMM$b_age_T + post_GMM$f_per_group_pr_T[,1,1]),   # Age (A): AUS/NZE More Freq. Users
  Age_AUS_Infreq = (post_GMM$b_age_T + post_GMM$f_per_group_pr_T[,2,1]),   # Age (A): AUS/NZE Less Freq. Users
  
  Age_TUR_Freq = (post_GMM$b_age_T + post_GMM$f_per_group_pr_T[,10,1]),   # Age (A): TUR More Freq. Users
  Age_TUR_Infreq = (post_GMM$b_age_T + post_GMM$f_per_group_pr_T[,11,1]),   # Age (A): TUR Less Freq. Users
  
  Age_COL_Freq = (post_GMM$b_age_T + post_GMM$f_per_group_pr_T[,3,1]),   # Age (A): COL More Freq. Users
  Age_COL_Infreq = (post_GMM$b_age_T + post_GMM$f_per_group_pr_T[,4,1]),   # Age (A): COL Less Freq. Users
  
  Age_VNM_Freq = (post_GMM$b_age_T + post_GMM$f_per_group_pr_T[,12,1]),   # Age (A): COL More Freq. Users
  Age_VNM_Infreq = (post_GMM$b_age_T + post_GMM$f_per_group_pr_T[,13,1]),   # Age (A): COL Less Freq. Users
  
  Age_ZAF_Freq = (post_GMM$b_age_T + post_GMM$f_per_group_pr_T[,8,1]),   # Age (A): ZAF More Freq. Users
  Age_ZAF_Infreq = (post_GMM$b_age_T + post_GMM$f_per_group_pr_T[,9,1]),   # Age (A): ZAF Less Freq. Users
  
  
  Dist_CZ_control = (post_GMM$b_dist_T + post_GMM$f_per_group_pr_T[,7,2]),    # Dist (A): CZE Control
  Dist_CZ_Frequent = (post_GMM$b_dist_T + post_GMM$f_per_group_pr_T[,5,2]),   # Dist (A): CZE More Freq. Users
  Dist_CZ_Infreq = (post_GMM$b_dist_T + post_GMM$f_per_group_pr_T[,6,2]),   # Dist (A): CZE Less Freq. Users
  
  Dist_AUS_Freq = (post_GMM$b_dist_T + post_GMM$f_per_group_pr_T[,1,2]),   # Dist (A): AUS/NZE More Freq. Users
  Dist_AUS_Infreq = (post_GMM$b_dist_T + post_GMM$f_per_group_pr_T[,2,2]),   # Dist (A): AUS/NZE Less Freq. Users
  
  Dist_TUR_Freq = (post_GMM$b_dist_T + post_GMM$f_per_group_pr_T[,10,2]),   # Dist (A): TUR More Freq. Users
  Dist_TUR_Infreq = (post_GMM$b_dist_T + post_GMM$f_per_group_pr_T[,11,2]),   # Dist (A): TUR Less Freq. Users
  
  Dist_COL_Freq = (post_GMM$b_dist_T + post_GMM$f_per_group_pr_T[,3,2]),   # Dist (A): COL More Freq. Users
  Dist_COL_Infreq = (post_GMM$b_dist_T + post_GMM$f_per_group_pr_T[,4,2]),   # Dist (A): COL Less Freq. Users
  
  Dist_VNM_Freq = (post_GMM$b_dist_T + post_GMM$f_per_group_pr_T[,12,2]),   # Dist (A): COL More Freq. Users
  Dist_VNM_Infreq = (post_GMM$b_dist_T + post_GMM$f_per_group_pr_T[,13,2]),   # Dist (A): COL Less Freq. Users
  
  Dist_ZAF_Freq = (post_GMM$b_dist_T + post_GMM$f_per_group_pr_T[,8,2]),   # Dist (A): ZAF More Freq. Users
  Dist_ZAF_Infreq = (post_GMM$b_dist_T + post_GMM$f_per_group_pr_T[,9,2]),   # Dist (A): ZAF Less Freq. Users
  
  
  Asym_CZ_control = (post_GMM$b_FA_T + post_GMM$f_per_group_pr_T[,7,3]),    # Asym (A): CZE Control
  Asym_CZ_Frequent = (post_GMM$b_FA_T + post_GMM$f_per_group_pr_T[,5,3]),   # Asym (A): CZE More Freq. Users
  Asym_CZ_Infreq = (post_GMM$b_FA_T + post_GMM$f_per_group_pr_T[,6,3]),   # Asym (A): CZE Less Freq. Users
  
  Asym_AUS_Freq = (post_GMM$b_FA_T + post_GMM$f_per_group_pr_T[,1,3]),   # Asym (A): AUS/NZE More Freq. Users
  Asym_AUS_Infreq = (post_GMM$b_FA_T + post_GMM$f_per_group_pr_T[,2,3]),   # Asym (A): AUS/NZE Less Freq. Users
  
  Asym_TUR_Freq = (post_GMM$b_FA_T + post_GMM$f_per_group_pr_T[,10,3]),   # Asym (A): TUR More Freq. Users
  Asym_TUR_Infreq = (post_GMM$b_FA_T + post_GMM$f_per_group_pr_T[,11,3]),   # Asym (A): TUR Less Freq. Users
  
  Asym_COL_Freq = (post_GMM$b_FA_T + post_GMM$f_per_group_pr_T[,3,3]),   # Asym (A): COL More Freq. Users
  Asym_COL_Infreq = (post_GMM$b_FA_T + post_GMM$f_per_group_pr_T[,4,3]),   # Asym (A): COL Less Freq. Users
  
  Asym_VNM_Freq = (post_GMM$b_FA_T + post_GMM$f_per_group_pr_T[,12,3]),   # Asym (A): COL More Freq. Users
  Asym_VNM_Infreq = (post_GMM$b_FA_T + post_GMM$f_per_group_pr_T[,13,3]),   # Asym (A): COL Less Freq. Users
  
  Asym_ZAF_Freq = (post_GMM$b_FA_T + post_GMM$f_per_group_pr_T[,8,3]),   # Asym (A): ZAF More Freq. Users
  Asym_ZAF_Infreq = (post_GMM$b_FA_T + post_GMM$f_per_group_pr_T[,9,3]),   # Asym (A): ZAF Less Freq. Users
  
  
  SexTyp_CZ_control = (post_GMM$b_sshd_T + post_GMM$f_per_group_pr_T[,7,4]),    # SexTyp (A): CZE Control
  SexTyp_CZ_Frequent = (post_GMM$b_sshd_T + post_GMM$f_per_group_pr_T[,5,4]),   # SexTyp (A): CZE More Freq. Users
  SexTyp_CZ_Infreq = (post_GMM$b_sshd_T + post_GMM$f_per_group_pr_T[,6,4]),   # SexTyp (A): CZE Less Freq. Users
  
  SexTyp_AUS_Freq = (post_GMM$b_sshd_T + post_GMM$f_per_group_pr_T[,1,4]),   # SexTyp (A): AUS/NZE More Freq. Users
  SexTyp_AUS_Infreq = (post_GMM$b_sshd_T + post_GMM$f_per_group_pr_T[,2,4]),   # SexTyp (A): AUS/NZE Less Freq. Users
  
  SexTyp_TUR_Freq = (post_GMM$b_sshd_T + post_GMM$f_per_group_pr_T[,10,4]),   # SexTyp (A): TUR More Freq. Users
  SexTyp_TUR_Infreq = (post_GMM$b_sshd_T + post_GMM$f_per_group_pr_T[,11,4]),   # SexTyp (A): TUR Less Freq. Users
  
  SexTyp_COL_Freq = (post_GMM$b_sshd_T + post_GMM$f_per_group_pr_T[,3,4]),   # SexTyp (A): COL More Freq. Users
  SexTyp_COL_Infreq = (post_GMM$b_sshd_T + post_GMM$f_per_group_pr_T[,4,4]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_VNM_Freq = (post_GMM$b_sshd_T + post_GMM$f_per_group_pr_T[,12,4]),   # SexTyp (A): COL More Freq. Users
  SexTyp_VNM_Infreq = (post_GMM$b_sshd_T + post_GMM$f_per_group_pr_T[,13,4]),   # SexTyp (A): COL Less Freq. Users
  
  SexTyp_ZAF_Freq = (post_GMM$b_sshd_T + post_GMM$f_per_group_pr_T[,8,4]),   # SexTyp (A): ZAF More Freq. Users
  SexTyp_ZAF_Infreq = (post_GMM$b_sshd_T + post_GMM$f_per_group_pr_T[,9,4]),   # SexTyp (A): ZAF Less Freq. Users
  
  
  L_CZ_control = (post_GMM$b_L_T + post_GMM$f_per_group_pr_T[,7,5]),    # L (A): CZE Control
  L_CZ_Frequent = (post_GMM$b_L_T + post_GMM$f_per_group_pr_T[,5,5]),   # L (A): CZE More Freq. Users
  L_CZ_Infreq = (post_GMM$b_L_T + post_GMM$f_per_group_pr_T[,6,5]),   # L (A): CZE Less Freq. Users
  
  L_AUS_Freq = (post_GMM$b_L_T + post_GMM$f_per_group_pr_T[,1,5]),   # L (A): AUS/NZE More Freq. Users
  L_AUS_Infreq = (post_GMM$b_L_T + post_GMM$f_per_group_pr_T[,2,5]),   # L (A): AUS/NZE Less Freq. Users
  
  L_TUR_Freq = (post_GMM$b_L_T + post_GMM$f_per_group_pr_T[,10,5]),   # L (A): TUR More Freq. Users
  L_TUR_Infreq = (post_GMM$b_L_T + post_GMM$f_per_group_pr_T[,11,5]),   # L (A): TUR Less Freq. Users
  
  L_COL_Freq = (post_GMM$b_L_T + post_GMM$f_per_group_pr_T[,3,5]),   # L (A): COL More Freq. Users
  L_COL_Infreq = (post_GMM$b_L_T + post_GMM$f_per_group_pr_T[,4,5]),   # L (A): COL Less Freq. Users
  
  L_VNM_Freq = (post_GMM$b_L_T + post_GMM$f_per_group_pr_T[,12,5]),   # L (A): COL More Freq. Users
  L_VNM_Infreq = (post_GMM$b_L_T + post_GMM$f_per_group_pr_T[,13,5]),   # L (A): COL Less Freq. Users
  
  L_ZAF_Freq = (post_GMM$b_L_T + post_GMM$f_per_group_pr_T[,8,5]),   # L (A): ZAF More Freq. Users
  L_ZAF_Infreq = (post_GMM$b_L_T + post_GMM$f_per_group_pr_T[,9,5])   # L (A): ZAF Less Freq. Users
)


summary.data.frame(M2_posts_GMM_TRUSTW_Varying)
str(M2_posts_GMM_TRUSTW_Varying) # Need 65 rows 



tiff("M2_WITH_GMM_PREDICTORS_TRUSTW_VARYING_05_02_2025.tiff",width=25,height=30,units="cm",res=600,compression="lzw")
par(mar=c(2.1, 14.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.25,0.5),ylim=c(1,66),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
#mtext("Linear predictors' varying effects", side = 3, line = 0.20, adj = 2.5) # Adjust 'adj' for horizontal shifting

segments(x0 = -0.25, y0 = c(1:13,14.5:26.5,28:40,41.5:53.5,55:67), x1 = 0.2, 
         y1 = c(1:13,14.5:26.5,28:40,41.5:53.5,55:67), col = "#40808080", lwd = 1.5)
abline(v=c(seq(from=-0.2, to=0.2, by=0.1)),col="#80808080",lty=2)
# Empty graph layout 

cols<-c("#c56043", "#e04b5c", "#de5353", "#c25c42", "#d85652", "#c8585d", "#de534b", "#cd5440", "#d35d5e", "#c3615a", "#d46655", "#cf5552", "#c65f5d",
        "#856554", "#896757", "#815b69", "#8f6756", "#86554d", "#8a675d", "#85586a", "#805158", "#855d52", "#8f4e50", "#8e5960", "#80685a", "#75584c",
        "#85b68f", "#77bf98", "#86b18f", "#85b589", "#7eB68d", "#8bB37a", "#80B491", "#65Bd7d", "#81B784", "#8bb883", "#8ebf92", "#7bBe8f", "#91bd79",
        "#6b9168", "#69925b", "#689d55", "#6ba261", "#5aaa6b", "#599866", "#519c58", "#55a755", "#5caa53", "#5c9c60", "#588f5b", "#5e9d6f", "#5aa757",
        "#64a9e6", "#79a5ee", "#809be9", "#6d9dfa", "#6899ec", "#6facde", "#77abed", "#63aaf0", "#63a4e4", "#74a5e3", "#79a9eb", "#6aaaf6", "#78a4dd"
        
) 

ys<-c(1:13,14.5:26.5,28:40,41.5:53.5,55:67)
ys<-rev(ys)

labs <- c("CZE Control",
          "CZE More Frequent U",
          "CZE Less Freqeunt U",
          
          "AUS/NZE More Frequent U",
          "AUS/NZE Less Frequent U",
          
          "TUR More Frequent U",
          "TUR Less Frequent U",
          
          "COL More Frequent U",
          "COL Less Frequent U",
          
          "COL More Frequent U",
          "COL Less Frequent U",
          
          "ZAF More Frequent U",
          "ZAF Less Frequent U")

labs <- rep(labs,5)

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,65),
                               Mean=rep(0,65),Up_CI=rep(0,65)) 

for (i in 1:length(M2_posts_GMM_TRUSTW_Varying)) {
  Coefficients_check$Mean[i]<-mean(M2_posts_GMM_TRUSTW_Varying[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(M2_posts_GMM_TRUSTW_Varying)) {
  Coefficients_check$Low_CI[i]<-PI(M2_posts_GMM_TRUSTW_Varying[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(M2_posts_GMM_TRUSTW_Varying)) {
  Coefficients_check$Up_CI[i]<-PI(M2_posts_GMM_TRUSTW_Varying[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))

cex_t <- 1.25
cex_t2 <- 1
pls <- 1

text("Mean", x=0.325, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.25, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=0.4, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


for(i in 1:length(M2_posts_GMM_TRUSTW_Varying)){ 
  toplot<-M2_posts_GMM_TRUSTW_Varying[[i]]
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

abline(v=0.0, col="#999999", lwd=2, lty=1)

dev.off()


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

rm(list=ls())

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Estimates per culture - model 25 centre missing (not, the image contains separately-saved sampled posterior; that's why it works)
load("IMAGE_Mod_25_centre_gone.Rdata")

str(post_25)

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 3.1 How assigned attractiveness differs relatively between the samples: 

summary(as.factor(CZRSAUS_AT_TW_cut_25@data$FSMUi))



str(post_25$Rho_FSMUi_A) 


# BEWARE - CZ ORIG IS on a different position than before! 

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# Attractiveness

M1_posts_ATR <- list(
  # 1 CZECH ORIGINAL 
  CZ_orig = post_noGMM$a_A + post_noGMM$a_group_A[,7],
  # 2 CZECH - above - median users.
  CZ_Intensive_Users = post_noGMM$a_A + post_noGMM$a_group_A[,5],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = post_noGMM$a_A + post_noGMM$a_group_A[,6],
  # 4 Australia - above - median users  
  AUS_Intensive_User = post_noGMM$a_A + post_noGMM$a_group_A[,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = post_noGMM$a_A + post_noGMM$a_group_A[,2],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = post_noGMM$a_A + post_noGMM$a_group_A[,10],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = post_noGMM$a_A + post_noGMM$a_group_A[,11],
  # Colombia - above - median users 
  COL_Intensive_User = post_noGMM$a_A + post_noGMM$a_group_A[,3], 
  # COlombia - below - median users
  COL_Less_Intensive = post_noGMM$a_A + post_noGMM$a_group_A[,4],
  # Vietnam - above - median users 
  VN_Intensive_User = post_noGMM$a_A + post_noGMM$a_group_A[,12],
  # Vietnam - below - median users 
  VN_Less_Intensive = post_noGMM$a_A + post_noGMM$a_group_A[,13],
  # RSA - above - median users 
  RSA_Intenstive_User = post_noGMM$a_A + post_noGMM$a_group_A[,8],
  # RSA - below - median users 
  RSA_Less_Intensive = post_noGMM$a_A + post_noGMM$a_group_A[,9]
)

summary.data.frame(M1_posts_ATR)
str(M1_posts_ATR) # Need 13 rows 

tiff("M1_no_GMM_Attractiveness_04_02_2025.tiff",width=20,height=12,units="cm",res=600,compression="lzw")
par(mar=c(3.1, 18.1, 1.1, 2.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-1,1),ylim=c(1,13.5),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Czech Faces - Rated Attractiveness: Model without GMM", side = 3, line = 0.20, adj = 2.5) # Adjust 'adj' for horizontal shifting

segments(x0 = -1, y0 = 1:13, x1 = 0.5, y1 = 1:13, col = "#40808080", lwd = 1.5)
abline(v=c(seq(from=-1, to=0.5, by=0.25)),col="#80808080",lty=2)
# Empty graph layout 

cols<-c("#00FBFF", 
        "#8C00FF",
        "#FFADD0",
        "#09C9FF",
        "#00FF33",
        
        "#0FEA33",
        "#FFEA00",
        "#FFA600",
        "#00FF33",
        "#EEEA00",
        
        "#EEEDD0",
        "#FFFD0D",
        "#DDEE0D"
) 

ys<-seq(from=1,to=13, by=1)
ys<-rev(ys)

labs<-c("Czechs (Control group)",    
        "Czechs - More frequent users",
        "Czechs - Less frequent users",
        "Australia/NewZ - More freq. users",
        "Australia/NewZ - Less freq. users",
        "Turkey - More frequent users",
        "Turkey - Less frequent users",
        "Colombia - More frequent users",
        "Colombia - Less frequent users",
        "Vietnamese - More freq. users",
        "Vietnamese - Less freq. users",
        "South Africans - More freq. users",
        "South Africans - Less freq. users"
)

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,13),
                               Mean=rep(0,13),Up_CI=rep(0,13)) 

for (i in 1:length(M1_posts_ATR)) {
  Coefficients_check$Mean[i]<-mean(M1_posts_ATR[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(M1_posts_ATR)) {
  Coefficients_check$Low_CI[i]<-PI(M1_posts_ATR[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(M1_posts_ATR)) {
  Coefficients_check$Up_CI[i]<-PI(M1_posts_ATR[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))

cex_t <- 1.25
cex_t2 <- 0.75
pls <- 0.4

text("Mean", x=0.8, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.6, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=1.0, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


for(i in 1:length(M1_posts_ATR)){ 
  toplot<-M1_posts_ATR[[i]]
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
  axis(2, at=ycoord, labels=textlab, srt=45, las=2, cex.axis=1.2)
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=0.8, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=0.6, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=1.0, y=ys[i], cex=cex_t2)
  for (i in 1:13) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.01)+ycoord[i],border=T, lwd = 1, col=colpol))
  }
  #lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

abline(v=0.0, col="#999999", lwd=2, lty=1)

dev.off()

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 3.2. DTTO: Trustworthiness

M1_posts_TRU <- list(
  # 1 CZECH ORIGINAL 
  CZ_orig = post_noGMM$a_T + post_noGMM$a_group_T[,7],
  # 2 CZECH - above - median users.
  CZ_Intensive_Users = post_noGMM$a_T + post_noGMM$a_group_T[,5],
  # 3 CZECH - below - median users 
  CZ_Less_Intensive = post_noGMM$a_T + post_noGMM$a_group_T[,6],
  # 4 Australia - above - median users  
  AUS_Intensive_User = post_noGMM$a_T + post_noGMM$a_group_T[,1],
  # 5 AUstralia - below - median users 
  AUS_Less_Intensive = post_noGMM$a_T + post_noGMM$a_group_T[,2],
  # 6 Turkey - above - median users 
  TUR_Intensive_User = post_noGMM$a_T + post_noGMM$a_group_T[,10],
  # 7 TUrkey - below - media users 
  TUR_Less_Intensive = post_noGMM$a_T + post_noGMM$a_group_T[,11],
  # Colombia - above - median users 
  COL_Intensive_User = post_noGMM$a_T + post_noGMM$a_group_T[,3], 
  # COlombia - below - median users
  COL_Less_Intensive = post_noGMM$a_T + post_noGMM$a_group_T[,4],
  # Vietnam - above - median users 
  VN_Intensive_User = post_noGMM$a_T + post_noGMM$a_group_T[,12],
  # Vietnam - below - median users 
  VN_Less_Intensive = post_noGMM$a_T + post_noGMM$a_group_T[,13],
  # RSA - above - median users 
  RSA_Intenstive_User = post_noGMM$a_T + post_noGMM$a_group_T[,8],
  # RSA - below - median users 
  RSA_Less_Intensive = post_noGMM$a_T + post_noGMM$a_group_T[,9]
)

summary.data.frame(M1_posts_ATR)
str(M1_posts_ATR) # Need 13 rows 

tiff("M1_no_GMM_Trustworthiness_04_02_2025.tiff",width=20,height=12,units="cm",res=600,compression="lzw")
par(mar=c(3.1, 18.1, 1.1, 2.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-1,1),ylim=c(1,13.5),yaxt="n",bty="n", ylab="",xlab="slope estimate", cex.axis=1)
mtext("Czech Faces - Rated Trustworthiness: Model without GMM", side = 3, line = 0.20, adj = 2.5) # Adjust 'adj' for horizontal shifting

segments(x0 = -1, y0 = 1:13, x1 = 0.5, y1 = 1:13, col = "#40808080", lwd = 1.5)
abline(v=c(seq(from=-1, to=0.5, by=0.25)),col="#80808080",lty=2)
# Empty graph layout 

ys<-seq(from=1,to=13, by=1)
ys<-rev(ys)

Coefficients_check<-data.frame(Name=labs,Low_CI=rep(0,13),
                               Mean=rep(0,13),Up_CI=rep(0,13)) 

for (i in 1:length(M1_posts_TRU)) {
  Coefficients_check$Mean[i]<-mean(M1_posts_TRU[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(M1_posts_TRU)) {
  Coefficients_check$Low_CI[i]<-PI(M1_posts_TRU[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(M1_posts_TRU)) {
  Coefficients_check$Up_CI[i]<-PI(M1_posts_TRU[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))

cex_t <- 1.25
cex_t2 <- 0.75
pls <- 0.4

text("Mean", x=0.8, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=0.6, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=1.0, y=max(ys+pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


for(i in 1:length(M1_posts_TRU)){ 
  toplot<-M1_posts_TRU[[i]]
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
  axis(2, at=ycoord, labels=textlab, srt=45, las=2, cex.axis=1.2)
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=0.8, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=0.6, y=ys[i], cex=cex_t2)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=1.0, y=ys[i], cex=cex_t2)
  for (i in 1:13) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.01)+ycoord[i],border=T, lwd = 1, col=colpol))
  }
  #lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=3, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

abline(v=0.0, col="#999999", lwd=2, lty=1)

dev.off()

# Correlations - Model 25 centre missing

