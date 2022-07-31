#データの読み込み
DF <- read.table("my_covid5_for_dimension.csv", 
                 sep = ",", 
                 header = TRUE, 
                 stringsAsFactors = FALSE)
head(DF)

#平行分析
library(psych)
result.prl <- fa.parallel(DF[, (3:37)], fm="ml")

library(psych)
result.prl1 <- fa.parallel(DF[, (3:37)], fm="minres")

library(psych)
result.prl1 <- fa.parallel(DF[, (3:10)], fm="ml")

library(psych)
result.prl1 <- fa.parallel(DF[, (11:19)], fm="ml")

library(psych)
result.prl1 <- fa.parallel(DF[, (21:37)], fm="ml")

result.prl1 <- fa.parallel(DF[, (3:19)], fm="ml")

library(psych)
result.prl1 <- fa.parallel(DF[, (3:44)], fm="pa")

COR <- cor(DF[, (3:37)])
COR

#因子分析FA
resultFA1 <- fa(DF[, (19:37)], 
                fm = "ml",
                nfactors=4, 
                rotate = "varimax", 
                scores = "regression")

print(resultFA1, digits=2, srt=TRUE)

fa.diagram(resultFA1, 
           rsize=0.8, 
           e.size=0.1, 
           marg=c(.5, 5, .5, .5), 
           cex=.6
           )

head(resultFA1$scores)


DFfa1 <- as.data.frame(resultFA1$scores)

#因子のデータフレームを追加
DFnew <- cbind(DF, DFfa1)
head(DFnew)




#散布図の確認plot
plot(DF$感染者数1日, DFnew$ML1)
plot(log(DF$感染者数1日), DFnew$ML1)
plot(log(DF$感染者数1日), log(DFnew$ML1))

plot(DF$感染者数1日, DFnew$ML2)
plot(log(DF$感染者数1日), DFnew$ML2)
plot(log(DF$感染者数1日), log(DFnew$ML2))

plot(DF$感染者数1日, DFnew$ML3)
plot(log(DF$感染者数1日), DFnew$ML3)
plot(log(DF$感染者数1日), log(DFnew$ML3))

plot(DF$感染者数1日, DFnew$ML4)
plot(log(DF$感染者数1日), DFnew$ML4)
plot(log(DF$感染者数1日), log(DFnew$ML4))



#線形回帰モデルの作成lm1
lm1 <- lm(感染者数1日 ~ ML1 + ML2 + ML3 + ML4 + 平日休日, data = DFnew)
summary(lm1)

#分散分析（ほんとに差があるのか）aov, TukeyHSD
AOV1 <- aov(感染者数1日 ~ ML1 + ML2 + ML3 + ML4 + 平日休日, data = DFnew)
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








#線形回帰モデルの作成lm1
lm2 <- lm(log(感染者数1日 + 1) ~ 
            ML1 + ML2 + ML3 + ML4 + 平日休日, data = DFnew)
summary(lm2)

#分散分析（ほんとに差があるのか）aov, TukeyHSD
AOV2 <- aov(log(感染者数1日 + 1) ~ 
              log(ML1) + ML2 + ML3 + ML4 + 平日休日, data = DFnew)
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
