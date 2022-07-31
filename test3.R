#データの読み込み
DF <- read.table("my_covid3.csv", 
                 sep = ",", 
                 header = TRUE, 
                 stringsAsFactors = FALSE)
head(DF)

#散布図の確認plot
plot(DF$感染者数1日, DF$平均気温)
plot(log(DF$感染者数1日), DF$平均気温)

#散布図の確認ggplot、ダミー変数ごとの違いを確認
library(ggplot2)
library(GGally)
ggpairs(DF[, c(2, 7)], 
        aes(colour=as.factor(DF$平日休日), 
            alpha=0.5))

ggpairs(DF[, c(2, 7)], 
        aes(colour=as.factor(DF$曜日), 
            alpha=0.5))
  

#箱ひげ図の確認
boxplot(感染者数1日 ~ 平日休日, data=DF)
boxplot(感染者数1日 ~ 曜日, data=DF)

#感染者数1日のhiストグラムによる分布把握
hist(DF$感染者数1日)
hist(log(DF$感染者数1日))
hist(log(DF$感染者数1日 + 1))






#線形回帰モデルの作成lm1
lm1 <- lm(感染者数1日 ~ 平日休日 + 平均気温, data = DF)
summary(lm1)

#分散分析（ほんとに差があるのか）aov, TukeyHSD
AOV1 <- aov(lm1)
summary(AOV1)
TukeyHSD(AOV1)

#オーバーフィッテイングの確認AIC（小さいほど良い）
AIC(lm1)

#BIC（単純なモデルを良く評価）
BIC(lm1)

#残差の分布
par(mfrow=c(2, 2))
plot(lm1)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm1)

#標準変化域係数の作成lm.beta
library(lm.beta)
LM1 <- lm.beta(lm1)
summary(LM1)

#効果量（実際にどの程度差があるか）コーエンのd
weekdaydata <- subset(DF, DF$平日休日=="平日", subset="感染者数1日")
head(weekdaydata)

weekenddata <- subset(DF, DF$平日休日=="休日", subset="感染者数1日")
head(weekenddata)

library(effsize)
cohen.d(weekdaydata$感染者数1日, weekenddata$感染者数1日,
        hedges.correction=F)


#効果量（実際にどの程度差があるか）コーエンのd
first_half <- subset(DF, 
                     DF$前半後半=="_週の前半")
head(first_half)

second_half <- subset(DF, 
                      DF$前半後半=="週の後半")
head(second_half)

first_half_vec <- first_half[, 2]
head(first_half_vec)

second_half_vec <- second_half[, 2]
head(second_half_vec)
str(second_half_vec)

library(effsize)
cohen.d(first_half_vec, 
        second_half_vec)



#線形回帰モデルの作成lm2
lm2 <- lm(感染者数1日 ~ 曜日 + 平均気温, data = DF)
summary(lm2)

#分散分析（ほんとに差があるのか）aov, TukeyHSD
AOV2 <- aov(感染者数1日 ~ 曜日 + 平均気温, data = DF)
summary(AOV2)
TukeyHSD(AOV2)

#オーバーフィッテイングの確認AIC（小さいほど良い）
AIC(lm1)
AIC(lm2)

#BIC（単純なモデルを良く評価）
BIC(lm1)
BIC(lm2)

#残差の分布
par(mfrow=c(2, 2))
plot(lm2)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm2)

#標準変化域係数の作成lm.beta
library(lm.beta)
LM2 <- lm.beta(lm2)
summary(LM2)

#効果量（実際にどの程度差があるか）コーエンのd
mondaydata <- subset(DF, DF$曜日=="月", subset="感染者数1日")
head(mondaydata)

tuesdaydata <- subset(DF, DF$曜日=="火", subset="感染者数1日")
head(tuesdaydata)

library(effsize)
cohen.d(mondaydata$感染者数1日, tuesdaydata$感染者数1日,
        hedges.correction=F)








#線形回帰モデルの作成lm3
lm3 <- lm(log(感染者数1日 + 1) ~ 
            平日休日 + 平均気温, data = DF)
summary(lm3)

#分散分析（ほんとに差があるのか）aov, TukeyHSD
AOV3 <- aov(lm3)
summary(AOV3)
TukeyHSD(AOV3)

#オーバーフィッテイングの確認AIC（小さいほど良い）
AIC(lm1)
AIC(lm2)
AIC(lm3)

#BIC（単純なモデルを良く評価）
BIC(lm1)
BIC(lm2)
BIC(lm3)

#残差の分布
par(mfrow=c(2, 2))
plot(lm3)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm3)

#標準変化域係数の作成lm.beta
library(lm.beta)
LM3 <- lm.beta(lm3)
summary(LM3)





#線形回帰モデルの作成lm4
lm4 <- lm(log(感染者数1日 + 1) ~ 
            曜日 + 平均気温, data = DF)
summary(lm4)

#分散分析（ほんとに差があるのか）aov, TukeyHSD
AOV4 <- aov(log(感染者数1日 + 1) ~ 
              曜日 + 平均気温, data = DF)
summary(AOV4)
TukeyHSD(AOV4)

#オーバーフィッテイングの確認AIC（小さいほど良い）
AIC(lm1)
AIC(lm2)
AIC(lm3)
AIC(lm4)

#BIC（単純なモデルを良く評価）
BIC(lm1)
BIC(lm2)
BIC(lm3)
BIC(lm4)

#残差の分布
par(mfrow=c(2, 2))
plot(lm4)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm4)

#標準変化域係数の作成lm.beta
library(lm.beta)
LM4 <- lm.beta(lm4)
summary(LM4)






head(DF)
#平行分析
library(psych)
result.prl1 <- fa.parallel(DF[, c(7, 15)], fm="ml")



