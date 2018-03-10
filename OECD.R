#OECD 국가의 삶의 만족도 분석
#Data 출처 https://www.kaggle.com/jej13b/oecd-better-life-index
#Cluster 분석

library(doBy)
library(ggplot2)
library(corrplot)

#1.Data Handling
OECD<- data.frame(read.csv("OECD.csv")) #데이터 불러오기
str(OECD) #데이터 탐색
attach(OECD)
OECD1<-OECD[-36,] #36행의 각국 데이터 합계 행 제거
OECD2<-OECD1[,c(1,6,8,10,14,15,19,21,24)] # 모델링 데이터 추출

#Correlation Analysis
OECD2_cor<-round(cor(OECD2[,2:length(OECD2)], use = "complete.obs"),2)

#clusteting
set.seed(9999)
fit<-kmeans(OECD2[,2:length(OECD2)], center=3)
names(fit)
OECD2$cluster<-fit$cluster


#bivariate plot 
plot(OECD2[,c(2,8)])
plot(OECD2[,c(2,8)], col=OECD2$cluster+5, pch=20, cex=2)
plot(OECD2[,c(2,8)], col=OECD2$cluster+5, pch=20, cex=2)

#corrplot 
library(corrplot)
corrplot(OECD2_cor, method = "shade", shade.col = NA, tl.col = 'black', tl.srt = 45)

