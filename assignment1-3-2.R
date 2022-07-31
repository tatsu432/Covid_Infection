#データの読み込み
DF <- read.table("covid19_tokyo_daily_last_modified15.csv", 
                 sep = ",", 
                 header = TRUE, 
                 stringsAsFactors = FALSE, 
                 fileEncoding="UTF-8-BOM")

head(DF)

#相関行列の作成
COR1 <- cor(DF[, c(2, 4, 6, 25:26, 28:32)])
COR1

COR2 <- cor(DF[, c(2, 32:38)])
COR2

COR3 <- cor(DF[, c(2, 39:49)])
COR3

COR4 <- cor(DF[, c(50, 4, 6, 25:26, 28:32)])
COR4

COR5 <- cor(DF[, c(50, 32:38)])
COR5

COR6 <- cor(DF[, c(50, 39:49)])
COR6

#相関係数のグラフ表示
library(qgraph)
qgraph(COR1, 
       minimum=.20,  
       labels=colnames(COR1), 
       edge.labels=T, 
       label.scale=F, 
       label.cex=0.8,
       edge.label.cex=1.4
)

qgraph(COR2, 
       minimum=.20,  
       labels=colnames(COR2), 
       edge.labels=T, 
       label.scale=F, 
       label.cex=0.8,
       edge.label.cex=1.4
)

qgraph(COR3, 
       minimum=.20,  
       labels=colnames(COR3), 
       edge.labels=T, 
       label.scale=F, 
       label.cex=0.8,
       edge.label.cex=1.4
)

qgraph(COR4, 
       minimum=.20,  
       labels=colnames(COR4), 
       edge.labels=T, 
       label.scale=F, 
       label.cex=0.8,
       edge.label.cex=1.4
)

qgraph(COR5, 
       minimum=.20,  
       labels=colnames(COR5), 
       edge.labels=T, 
       label.scale=F, 
       label.cex=0.8,
       edge.label.cex=1.4
)

qgraph(COR6, 
       minimum=.20,  
       labels=colnames(COR6), 
       edge.labels=T, 
       label.scale=F, 
       label.cex=0.8,
       edge.label.cex=1.4
)


#箱ひげ図の確認
library(ggplot2)
library(GGally)
ggplot(DF) +
  geom_boxplot(aes(x=緊急事態宣言, 
                   y=感染者数1日))

ggplot(DF) +
  geom_boxplot(aes(x=緊急事態宣言, 
                   y=感染者数の増加前日比))

ggplot(DF) +
  geom_boxplot(aes(x=週の前半後半1, 
                   y=感染者数1日))

ggplot(DF) +
  geom_boxplot(aes(x=週の前半後半1, 
                   y=感染者数の増加前日比))
ggplot(DF) +
  geom_boxplot(aes(x=冬ダミー, 
                   y=感染者数の増加前日比))

#日付データに変換したDFdateを作成
DFdate <- DF
DFdate[, 1] <- as.Date(DF[, 1], 
                       "%Y/%m/%d")

head(DFdate)

#感染者数1日と平均湿度と平均気温の時系列データを比較
library(scales)

infection <- ggplot(DFdate) +
  geom_line(aes(x=日付, 
                y=感染者数1日, 
                group=1), 
            colour="blue",
            alpha=0.5) +
  scale_x_date(breaks = seq(as.Date("2020-1-16"), 
                            as.Date("2021-6-27"), 
                            by="1 month"), 
               labels=date_format("%m")) +
  labs(x="月")

humidity <- ggplot(DFdate) +
  geom_line(aes(x=日付, 
                y=平均湿度, 
                group=1), 
            colour="red",
            alpha=0.5) +
  scale_x_date(breaks = seq(as.Date("2020-1-16"), 
                            as.Date("2021-6-27"), 
                            by="1 month"), 
               labels=date_format("%m")) +
  labs(x="月")

temperature <- ggplot(DFdate) +
  geom_line(aes(x=日付, 
                y=平均気温, 
                group=1), 
            colour="red",
            alpha=0.5) +
  scale_x_date(breaks = seq(as.Date("2020-1-16"), 
                            as.Date("2021-6-27"), 
                            by="1 month"), 
               labels=date_format("%m")) +
  labs(x="月")

library(gridExtra)
gridExtra::grid.arrange(infection, humidity, ncol = 1)
gridExtra::grid.arrange(temperature, humidity, ncol = 1)







#平均湿度を加える線形回帰モデルの作成lm8
lm8 <- lm(感染者数1日 ~ SI + 週の前半後半1 + 
                平均気温 + 平均湿度, data = DF)
summary(lm8)

#分散分析（ほんとに差があるのか）aov, TukeyHSD
AOV8 <- aov(lm8)
summary(AOV8)
TukeyHSD(AOV8)

#オーバーフィッテイングの確認AIC（小さいほど良い）
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8)

#残差の分布
par(mfrow=c(2, 2))
plot(lm8)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm8)

#標準変化域係数の作成lm.beta
library(lm.beta)
LM8 <- lm.beta(lm8)
summary(LM8)




#平均湿度を加える線形回帰モデルの作成lm9
lm9 <- lm(感染者数1日 ~ ラグ変数 + 週の前半後半1 + 
                平均気温 + ラグ変数*週の前半後半1 + 
                平均湿度, data = DF)
summary(lm9)

#分散分析（ほんとに差があるのか）aov, TukeyHSD
AOV9 <- aov(lm9)
summary(AOV9)
TukeyHSD(AOV9)

#オーバーフィッテイングの確認AIC（小さいほど良い）
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, lm9)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, lm9)

#残差の分布
par(mfrow=c(2, 2))
plot(lm9)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm9)

#標準変化域係数の作成lm.beta
library(lm.beta)
LM9 <- lm.beta(lm9)
summary(LM9)


#緊急事態宣言ダミーを加える線形回帰モデルの作成lm10
lm10 <- lm(感染者数1日 ~ ラグ変数 + 週の前半後半1 + 
                 平均気温 + 緊急事態宣言, data = DF)
summary(lm10)

#分散分析（ほんとに差があるのか）aov, TukeyHSD
AOV10 <- aov(lm10)
summary(AOV10)
TukeyHSD(AOV10)

#オーバーフィッテイングの確認AIC（小さいほど良い）
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, lm9, lm10)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, lm9, lm10)

#残差の分布
par(mfrow=c(2, 2))
plot(lm10)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm10)

#標準変化域係数の作成lm.beta
library(lm.beta)
LM10 <- lm.beta(lm10)
summary(LM10)





#緊急事態宣言ダミーを加える線形回帰モデルの作成lm11
lm11 <- lm(感染者数1日 ~ SI*週の前半後半1 + 
                 平均気温 + 週の前半後半1 + 緊急事態宣言, 
               data = DF)
summary(lm11)

#分散分析（ほんとに差があるのか）aov, TukeyHSD
AOV11 <- aov(lm11)
summary(AOV11)
TukeyHSD(AOV11)

#オーバーフィッテイングの確認AIC（小さいほど良い）
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, lm9, lm10, lm11)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, lm9, lm10, lm11)

#残差の分布
par(mfrow=c(2, 2))
plot(lm11)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm11)

#標準変化域係数の作成lm.beta
library(lm.beta)
LM11 <- lm.beta(lm11)
summary(LM11)





#lm3にワクチン接種者数を加える線形回帰モデルの作成lm12
lm12 <- lm(感染者数1日 ~ 週の前半後半1 + 
                 平均気温 + ワクチン接種者数, 
               data = DF)
summary(lm12)

#分散分析（ほんとに差があるのか）aov, TukeyHSD
AOV11 <- aov(lm12)
summary(AOV12)
TukeyHSD(AOV12)

#オーバーフィッテイングの確認AIC（小さいほど良い）
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, lm9, lm10, lm11, lm12)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, lm9, lm10, lm11, lm12)

#残差の分布
par(mfrow=c(2, 2))
plot(lm12)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm12)

#標準変化域係数の作成lm.beta
library(lm.beta)
LM12 <- lm.beta(lm12)
summary(LM12)





#冬ダミーを加える線形回帰モデルの作成lm13
lm13 <- lm(感染者数1日 ~ SI*週の前半後半1 + 
                 平均気温 + SI*冬ダミー, 
               data = DF)
summary(lm13)

#分散分析（ほんとに差があるのか）aov, TukeyHSD
AOV13 <- aov(lm13)
summary(AOV13)
TukeyHSD(AOV13)

#オーバーフィッテイングの確認AIC（小さいほど良い）
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13)

#残差の分布
par(mfrow=c(2, 2))
plot(lm13)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm13)

#標準変化域係数の作成lm.beta
library(lm.beta)
LM13 <- lm.beta(lm13)
summary(LM13)