#データの読み込み
DF <- read.table("my_covid2.csv", 
                 sep = ",", 
                 header = TRUE, 
                 stringsAsFactors = FALSE)
head(DF)




#線形回帰モデルの作成lm
lm1 <- lm(感染者数1日 ~ S * I, data = DF)
summary(lm1)

#分散分析（ほんとに差があるのか）aov, TukeyHSD
AOV1 <- aov(lm1)






library(psych)
result.prl1 <- fa.parallel(DF[, c(7:14)], fm="ml")
summary(AOV1)
TukeyHSD(AOV1)

#オーバーフィッテイングの確認BIC, AIC
AIC(lm1)
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

#データの読み込み
DF2 <- read.table("my_covid3.csv", 
                 sep = ",", 
                 header = TRUE, 
                 stringsAsFactors = FALSE)
head(DF2)




#線形回帰モデルの作成lm2
lm2 <- lm(感染者数1日 ~ SI * urgent * 平均気温, data = DF2)
summary(lm2)

#分散分析（ほんとに差があるのか）aov, TukeyHSD
AOV2 <- aov(lm2)
summary(AOV2)
TukeyHSD(AOV2)

#オーバーフィッテイングの確認BIC, AIC
AIC(lm2)
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




#線形回帰モデルの作成lm3
lm3 <- lm(感染者数1日 ~ ラグ変数 + 平日休日 + 平均気温, data = DF2)
summary(lm3)

#分散分析（ほんとに差があるのか）aov, TukeyHSD
AOV3 <- aov(lm3)
summary(AOV3)
TukeyHSD(AOV3)

#オーバーフィッテイングの確認BIC, AIC
AIC(lm3)
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
lm4 <- lm(感染者数1日 ~ I(ラグ変数^2) + ラグ変数 + 
                平日休日 + 平均気温, data = DF2)
summary(lm4)

#分散分析（ほんとに差があるのか）aov, TukeyHSD
AOV4 <- aov(lm4)
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
LM3 <- lm.beta(lm4)
summary(LM4)




