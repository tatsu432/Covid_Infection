DF <- read.table("covid19_tokyo_daily_last_modified.csv", 
                 sep = ",", 
                 header = TRUE, 
                 stringsAsFactors = FALSE, 
                 fileEncoding="UTF-8-BOM")

head(DF)



#lm1に平均湿度を加える線形回帰モデルの作成lm18
lm18 <- lm(感染者数1日 ~ 週の前半後半1 + 
                平均気温 + 平均湿度, data = DF)
summary(lm18)

#分散分析（ほんとに差があるのか
AOV18 <- aov(lm18)
summary(AOV18)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18)

#残差の分布
par(mfrow=c(2, 2))
plot(lm18)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm18)

#標準変化域係数の作成
library(lm.beta)
LM18 <- lm.beta(lm18)
summary(LM18)






#lm1に緊急事態宣言ダミーを加える線形回帰モデルの作成lm19
lm19 <- lm(感染者数1日 ~ 週の前半後半1 + 
                 平均気温 + 緊急事態宣言, data = DF)
summary(lm19)

#分散分析（ほんとに差があるのか）
AOV19 <- aov(lm19)
summary(AOV19)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19)

#残差の分布
par(mfrow=c(2, 2))
plot(lm19)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm19)

#標準変化域係数の作成
library(lm.beta)
LM19 <- lm.beta(lm19)
summary(LM19)










#lm1にワクチン接種者数を加える線形回帰モデルの作成lm20
lm20 <- lm(感染者数1日 ~ 週の前半後半1 + 平均気温 + 
                 ワクチン接種者数, 
               data = DF)
summary(lm20)

#分散分析
AOV20 <- aov(lm20)
summary(AOV20)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20)

#残差の分布
par(mfrow=c(2, 2))
plot(lm20)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm20)

#標準変化域係数の作成
library(lm.beta)
LM20 <- lm.beta(lm20)
summary(LM20)





#lm1に冬ダミーを加える線形回帰モデルの作成lm21
lm21 <- lm(感染者数1日 ~ 週の前半後半1 + 
                 平均気温 +冬ダミー, 
               data = DF)
summary(lm21)

#分散分析
AOV21 <- aov(lm21)
summary(AOV21)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21)


#残差の分布
par(mfrow=c(2, 2))
plot(lm21)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm21)

#標準変化域係数の作成
library(lm.beta)
LM21 <- lm.beta(lm21)
summary(LM21)








#lm1に平均湿度と緊急事態宣言ダミーと冬ダミーと
#ワクチン接種者数を加える線形回帰モデルの作成lm22
lm22 <- lm(感染者数1日 ~ 週の前半後半1 + 
                 平均気温 + 平均湿度 +
                 緊急事態宣言 + 冬ダミー + 
                 ワクチン接種者数, 
               data = DF)
summary(lm22)

#分散分析
AOV22 <- aov(lm22)
summary(AOV22)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22)

#残差の分布
par(mfrow=c(2, 2))
plot(lm22)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm22)

#標準変化域係数の作成
library(lm.beta)
LM22 <- lm.beta(lm22)
summary(LM22)





#lm1にラグ長期休暇ダミーを加える線形回帰モデルの作成lm23
lm23 <- lm(感染者数1日 ~ 週の前半後半1 + 
                 平均気温 +ラグ長期休暇ダミー, 
               data = DF)
summary(lm23)

#分散分析
AOV23 <- aov(lm23)
summary(AOV23)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23)

#残差の分布
par(mfrow=c(2, 2))
plot(lm23)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm23)

#標準変化域係数の作成
library(lm.beta)
LM23 <- lm.beta(lm23)
summary(LM23)



#lm1にラグ正月ダミーを加える線形回帰モデルの作成lm24
lm24 <- lm(感染者数1日 ~ 週の前半後半1 + 
                 平均気温 + ラグ正月ダミー, 
               data = DF)
summary(lm24)

#分散分析
AOV24 <- aov(lm24)
summary(AOV24)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24)

#残差の分布
par(mfrow=c(2, 2))
plot(lm24)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm24)

#標準変化域係数の作成
library(lm.beta)
LM24 <- lm.beta(lm24)
summary(LM24)







#lm1に平均湿度と冬ダミーと
#ワクチン接種者数と長期休暇ダミー
#を加える線形回帰モデルの作成lm25
lm25 <- lm(感染者数1日 ~ 週の前半後半1 + 
                 平均気温 + 平均湿度 + 冬ダミー + 
                 ワクチン接種者数 + 長期休暇ダミー, 
               data = DF)
summary(lm25)

#分散分析
AOV25 <- aov(lm25)
summary(AOV25)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25)

#残差の分布
par(mfrow=c(2, 2))
plot(lm25)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm25)

#標準変化域係数の作成
library(lm.beta)
LM25 <- lm.beta(lm25)
summary(LM25)







#lm1の平均気温をラグ平均気温として、
#ラグ平均湿度とラグ冬ダミーと
#ラグワクチン接種者数とラグ長期休暇ダミー
#を加える線形回帰モデルの作成lm26
lm26 <- lm(感染者数1日 ~ 週の前半後半1 + 
                 ラグ平均気温 + ラグ平均湿度 + ラグ冬ダミー + 
                 ラグワクチン接種者数 + ラグ長期休暇ダミー, 
               data = DF)
summary(lm26)

#分散分析（ほんとに差があるのか）
AOV26 <- aov(lm26)
summary(AOV26)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26)

#残差の分布
par(mfrow=c(2, 2))
plot(lm26)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm26)

#標準変化域係数の作成
library(lm.beta)
LM26 <- lm.beta(lm26)
summary(LM26)







#lm1に平均湿度と冬ダミーと
#ワクチン接種者数と長期休暇ダミーと
#PCRと抗原検査数を加える線形回帰モデルの作成lm27
lm27 <- lm(感染者数1日 ~ 週の前半後半1 + 
                 平均気温 + 平均湿度 + 冬ダミー + 
                 ワクチン接種者数 + 長期休暇ダミー + 
                 + PCRと抗原検査数, 
               data = DF)
summary(lm27)

#分散分析
AOV27 <- aov(lm27)
summary(AOV27)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27)

#残差の分布
par(mfrow=c(2, 2))
plot(lm27)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm27)

#標準変化域係数の作成
library(lm.beta)
LM27 <- lm.beta(lm27)
summary(LM27)






#lm1の平均気温をラグ平均気温として、
#ラグ平均湿度とラグ冬ダミーと
#ラグワクチン接種者数とラグ長期休暇ダミーと
#PCRと抗原検査数と緊急事態宣言と
#人動増減率前日比平均 * PCRと抗原検査数 
#を加える線形回帰モデルの作成lm28
lm28 <- lm(感染者数1日 ~ 週の前半後半1 + 
                 ラグ平均気温 + ラグ平均湿度 + ラグ冬ダミー + 
                 ラグワクチン接種者数 + ラグ長期休暇ダミー + 
                 + PCRと抗原検査数 + 緊急事態宣言 + 
                 人動増減率前日比平均 * PCRと抗原検査数 
                 , 
               data = DF)
summary(lm28)

#分散分析
AOV28 <- aov(lm28)
summary(AOV28)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28)

#残差の分布
#偏りがある
par(mfrow=c(2, 2))
plot(lm28)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm28)

#標準変化域係数の作成lm.beta
library(lm.beta)
LM28 <- lm.beta(lm28)
summary(LM28)





#lm1の平均気温をラグ平均気温として
#ラグ平均湿度とラグ冬ダミーと
#ラグワクチン接種者数とラグ長期休暇ダミーと
#PCRと抗原検査数とラグ緊急事態宣言と
#SIを加える線形回帰モデルの作成lm29
lm29 <- lm(感染者数1日 ~ 週の前半後半1 + 
                 ラグ平均気温 + ラグ平均湿度 + ラグ冬ダミー + 
                 ラグワクチン接種者数 + ラグ長期休暇ダミー + 
                 + PCRと抗原検査数 + ラグ緊急事態宣言 + SI
               , 
               data = DF)
summary(lm29)

#分散分析
AOV29 <- aov(lm29)
summary(AOV29)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29)

#残差の分布
par(mfrow=c(2, 2))
plot(lm29)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm29)

#標準変化域係数の作成lm.beta
library(lm.beta)
LM29 <- lm.beta(lm29)
summary(LM29)





#lm1の平均気温をラグ平均気温として、
#ラグ平均湿度とラグ冬ダミーと
#ラグ長期休暇ダミーと人動増減率前日比平均
#PCRと抗原検査数とラグ緊急事態宣言と
#SIを加える線形回帰モデルの作成lm30
lm30 <- lm(感染者数1日 ~ 週の前半後半1 + 
                 ラグ平均気温 + ラグ平均湿度 + 
                 ラグ冬ダミー + ラグ長期休暇ダミー + 
                 人動増減率前日比平均 + PCRと抗原検査数 + 
                 ラグ緊急事態宣言 + SI
               , 
               data = DF)
summary(lm30)

#分散分析
AOV30 <- aov(lm30)
summary(AOV30)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30)

#残差の分布
#偏りがみられる
par(mfrow=c(2, 2))
plot(lm30)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm30)

#標準変化域係数の作成
library(lm.beta)
LM30 <- lm.beta(lm30)
summary(LM30)




#lm1の平均気温をラグ平均気温として、
#ラグ平均湿度とラグ長期休暇ダミーと
#PCRと抗原検査数とラグ緊急事態宣言と
#SIとを加える線形回帰モデルの作成lm31
lm31 <- lm(感染者数1日 ~ 週の前半後半1 + 
                 ラグ平均気温 + ラグ平均湿度 + 
                 ラグ長期休暇ダミー + PCRと抗原検査数 + 
                 ラグ緊急事態宣言 + SI
               , 
               data = DF)
summary(lm31)

#分散分析
AOV31 <- aov(lm31)
summary(AOV31)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30, lm31)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30, lm31)

#残差の分布
par(mfrow=c(2, 2))
plot(lm31)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm31)

#標準変化域係数の作成
library(lm.beta)
LM31 <- lm.beta(lm31)
summary(LM31)





#lm1の平均気温をラグ平均気温として、
#ラグ平均湿度とラグ正月ダミーと
#PCRと抗原検査数とラグ緊急事態宣言と
#SIとラグ食料品店薬局の移動基準比
#を加える線形回帰モデルの作成lm32
lm32 <- lm(感染者数1日 ~ 週の前半後半1 + 
                   ラグ平均気温 + ラグ平均湿度 + 
                   ラグ正月ダミー + PCRと抗原検査数 + 
                   ラグ緊急事態宣言 + SI +　 
                   ラグ食料品店薬局の移動基準比
               , 
               data = DF)
summary(lm32)

#分散分析
AOV32 <- aov(lm32)
summary(AOV32)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30, lm31, lm32)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30, lm31, lm32)

#残差の分布
par(mfrow=c(2, 2))
plot(lm32)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm32)

#標準変化域係数の作成
library(lm.beta)
LM32 <- lm.beta(lm32)
summary(LM32)






#lm1の平均気温をラグ平均気温として、
#ラグ平均湿度とラグ冬ダミーと
#ラグ正月ダミーと
#PCRと抗原検査数とラグ緊急事態宣言と
#SIとラグ人の動き平均とラグ人の動き平均*ラグ冬ダミー
#を加える線形回帰モデルの作成lm33
lm33 <- lm(感染者数1日 ~ 週の前半後半1 + 
                   ラグ平均気温 + ラグ平均湿度 + 
                   ラグ冬ダミー + 
                   ラグ正月ダミー + PCRと抗原検査数+ 
                   ラグ緊急事態宣言 + SI + 
                   ラグ人の動き平均 + 
                   ラグ人の動き平均*ラグ冬ダミー
               , 
               data = DF)
summary(lm33)

#分散分析
AOV33 <- aov(lm33)
summary(AOV33)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30, lm31, lm32, lm33)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30, lm31, lm32, lm33)

#残差の分布
par(mfrow=c(2, 2))
plot(lm33)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm33)

#標準変化域係数の作成
library(lm.beta)
LM33 <- lm.beta(lm33)
summary(LM33)






#lm1の平均気温をラグ平均気温として、
#ラグ平均湿度とラグ冬ダミーと
#ラグ正月ダミーと
#PCRと抗原検査数とラグ緊急事態宣言と
#SIワクチン抜きとラグ人の動き平均と
#ラグ人の動き平均*ラグ冬ダミー
#を加える線形回帰モデルの作成lm34
lm34 <- lm(感染者数1日 ~ 週の前半後半1 + 
                   ラグ平均気温 + ラグ平均湿度 + 
                   ラグ冬ダミー + 
                   ラグ正月ダミー + PCRと抗原検査数+ 
                   ラグ緊急事態宣言 + SIワクチン抜き + 
                   ラグ人の動き平均*ラグ冬ダミー
               , 
               data = DF)
summary(lm34)

#分散分析
AOV34 <- aov(lm34)
summary(AOV34)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30, lm31, lm32, lm33, lm34)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30, lm31, lm32, lm33, lm34)

#残差の分布
par(mfrow=c(2, 2))
plot(lm34)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm34)

#標準変化域係数の作成
library(lm.beta)
LM34 <- lm.beta(lm34)
summary(LM34)





#lm1の平均気温をラグ平均気温として
#ラグ平均湿度と#ラグ正月ダミーと
#PCRと抗原検査数とラグ緊急事態宣言と
#SIワクチン抜きとラグ住宅の移動基準比と
#ラグ変異株陽性者数
#を加える線形回帰モデルの作成lm35
lm35 <- lm(感染者数1日 ~ 週の前半後半1 + 
                   ラグ平均気温 + ラグ平均湿度 +
                   ラグ正月ダミー + PCRと抗原検査数+ 
                   ラグ緊急事態宣言 + SIワクチン抜き + 
                   ラグ住宅の移動基準比 + 
                   ラグ変異株陽性者数
               , 
               data = DF)
summary(lm35)

#分散分析
AOV35 <- aov(lm35)
summary(AOV35)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30, lm31, lm32, lm33, lm34, lm35)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30, lm31, lm32, lm33, lm34, lm35)

#残差の分布
par(mfrow=c(2, 2))
plot(lm35)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm35)

#標準変化域係数の作成
library(lm.beta)
LM35 <- lm.beta(lm35)
summary(LM35)






#lm1の平均気温をラグ平均気温として
#ラグ平均湿度とラグ日照時間と
#ラグ正月ダミーと
#PCRと抗原検査数とラグ緊急事態宣言と
#SIワクチン抜きとラグ住宅の移動基準比と
#ラグ変異株陽性者数
#を加える線形回帰モデルの作成lm36
lm36 <- lm(感染者数1日 ~ 週の前半後半1 + 
                   ラグ平均気温 + ラグ平均湿度 + ラグ日照時間 + 
                   ラグ正月ダミー + PCRと抗原検査数+ 
                   ラグ緊急事態宣言 + SIワクチン抜き + 
                   ラグ住宅の移動基準比 + 
                   ラグ変異株陽性者数
               , 
               data = DF)
summary(lm36)

#分散分析
AOV36 <- aov(lm36)
summary(AOV36)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30, lm31, lm32, lm33, lm34, lm35, lm36)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30, lm31, lm32, lm33, lm34, lm35, lm36)

#残差の分布
par(mfrow=c(2, 2))
plot(lm36)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm36)

#標準変化域係数の作成lm.beta
library(lm.beta)
LM36 <- lm.beta(lm36)
summary(LM36)





#lm1の平均気温をラグ平均気温として
#ラグ平均湿度とラグ日照時間と
#ラグ正月ダミーと
#PCRと抗原検査数とラグ緊急事態宣言と
#SIワクチン抜きとラグ住宅の移動基準比と
#ラグアルファベータガンマ変異株陽性者数と
#ラグデルタカッパ変異株陽性者数
#を加える線形回帰モデルの作成lm37
lm37 <- lm(感染者数1日 ~ 週の前半後半1 + 
                   ラグ平均気温 + ラグ平均湿度 + ラグ日照時間 + 
                   ラグ正月ダミー + PCRと抗原検査数+ 
                   ラグ緊急事態宣言 + SIワクチン抜き + 
                   ラグ住宅の移動基準比 + 
                   ラグアルファベータガンマ変異株陽性者数 + 
                   ラグデルタカッパ変異株陽性者数
               , 
               data = DF)
summary(lm37)

#分散分析
AOV37 <- aov(lm37)
summary(AOV37)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30, lm31, lm32, lm33, lm34, lm35, lm36, 
    lm37)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30, lm31, lm32, lm33, lm34, lm35, lm36, 
    lm37)

#残差の分布
par(mfrow=c(2, 2))
plot(lm37)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm37)

#標準変化域係数の作成
library(lm.beta)
LM37 <- lm.beta(lm37)
summary(LM37)






#lm3の平均気温をラグ平均気温として、
#ラグ変数をラグ変数従来株として、
#ラグ正月ダミーと
#PCRと抗原検査数とラグ緊急事態宣言と
#ラグ冬ダミーとラグ住宅の移動基準比と
#ラグ変異株陽性者数
#を加える線形回帰モデルの作成lm38
lm38 <- lm(感染者数1日 ~ 週の前半後半1 + 
                   ラグ平均気温 + ラグ変数従来株 + 
                   ラグ正月ダミー + PCRと抗原検査数 + 
                   ラグ緊急事態宣言 + ラグ冬ダミー + 
                   ラグ住宅の移動基準比 + 
                   ラグ変異株陽性者数
               , 
               data = DF)
summary(lm38)

#分散分析
AOV38 <- aov(lm38)
summary(AOV38)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30, lm31, lm32, lm33, lm34, lm35, lm36, 
    lm37, lm38)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30, lm31, lm32, lm33, lm34, lm35, lm36, 
    lm37, lm38)

#残差の分布
par(mfrow=c(2, 2))
plot(lm38)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm38)

#標準変化域係数の作成
library(lm.beta)
LM38 <- lm.beta(lm38)
summary(LM38)





#lm3の平均気温をラグ平均気温として、
#ラグ変数をラグ変数従来株として、
#ラグ正月ダミーと
#PCRと抗原検査数とラグ緊急事態宣言とラグ冬ダミーと
#ラグ住宅の移動基準比と
#ラグアルファベータガンマ変異株陽性者数と
#ラグデルタカッパ変異株陽性者数
#を加える線形回帰モデルの作成lm39
lm39 <- lm(感染者数1日 ~ 週の前半後半1 + ラグ変数従来株 + 
                     ラグ平均気温 + ラグ平均湿度 + 
                     ラグ正月ダミー + PCRと抗原検査数+ 
                     ラグ緊急事態宣言 + ラグ冬ダミー + 
                     ラグ住宅の移動基準比 +
                     ラグアルファベータガンマ変異株陽性者数 + 
                     ラグデルタカッパ変異株陽性者数
                 , 
                 data = DF)
summary(lm39)

#分散分析
AOV39 <- aov(lm39)
summary(AOV39)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30, lm31, lm32, lm33, lm34, lm35, lm36, 
    lm37, lm38, lm39)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30, lm31, lm32, lm33, lm34, lm35, lm36, 
    lm37, lm38, lm39)

#残差の分布
par(mfrow=c(2, 2))
plot(lm39)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm39)

#標準変化域係数の作成
library(lm.beta)
LM39 <- lm.beta(lm39)
summary(LM39)




#lm3の平均気温をラグ平均気温として、
#ラグ変数をラグ変数従来株として、
#ラグ正月ダミーと
#PCRと抗原検査数とラグ緊急事態宣言と
#ラグ冬ダミーとラグ変異株陽性者数 と
#実質GDPを加える線形回帰モデルの作成lm40
lm40 <- lm(感染者数1日 ~ 週の前半後半1 + ラグ変数従来株 + 
                   ラグ平均気温 +
                   ラグ正月ダミー + PCRと抗原検査数+ 
                   ラグ緊急事態宣言 + ラグ冬ダミー + 
                   ラグ変異株陽性者数 + 
                   実質GDP
               , 
               data = DF)
summary(lm40)

#分散分析
AOV40 <- aov(lm40)
summary(AOV40)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30, lm31, lm32, lm33, lm34, lm35, lm36, 
    lm37, lm38, lm39, lm40)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17, lm18, lm19, lm20, 
    lm21, lm22, lm23, lm24, lm25, lm26, lm27, lm28, 
    lm29, lm30, lm31, lm32, lm33, lm34, lm35, lm36, 
    lm37, lm38, lm39, lm40)

#残差の分布
par(mfrow=c(2, 2))
plot(lm40)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm40)

#標準変化域係数の作成
library(lm.beta)
LM40 <- lm.beta(lm40)
summary(LM40)




#lm40を採用する
