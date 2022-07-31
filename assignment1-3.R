#データの読み込み
DF <- read.table("covid19_tokyo_daily_last_modified.csv", 
                 sep = ",", 
                 header = TRUE, 
                 stringsAsFactors = FALSE, 
                 fileEncoding="UTF-8-BOM")

head(DF)

#相関行列の作成
#COR1, 2, 3は感染者数1日との相関を確かめる
#COR4, 5, 6は感染者数増加前日比との相関を確かめる
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

#人の動きと染者数増加関係を散布図で見る
library(ggplot2)
library(GGally)
ggpairs(DF[, c(49, 50)], 
        aes(colour=as.factor(DF$曜日), 
            alpha=0.5))


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
ggplot(DF) +
  geom_boxplot(aes(x=長期休暇ダミー, 
                   y=感染者数の増加前日比))
ggplot(DF) +
  geom_boxplot(aes(x=長期休暇ダミー, 
                   y=感染者数1日))
ggplot(DF) +
  geom_boxplot(aes(x=ラグ長期休暇ダミー, 
                   y=感染者数の増加前日比))
ggplot(DF) +
  geom_boxplot(aes(x=ラグ長期休暇ダミー, 
                   y=感染者数1日))

ggplot(DF) +
  geom_boxplot(aes(x=正月ダミー, 
                   y=感染者数の増加前日比))
ggplot(DF) +
  geom_boxplot(aes(x=正月ダミー, 
                   y=感染者数1日))
ggplot(DF) +
  geom_boxplot(aes(x=ラグ正月ダミー, 
                   y=感染者数の増加前日比))
ggplot(DF) +
  geom_boxplot(aes(x=ラグ正月ダミー, 
                   y=感染者数1日))

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







#lm3に平均湿度を加える線形回帰モデルの作成lm8
lm8 <- lm(感染者数1日 ~ 週の前半後半1 +平均気温 + 
                ラグ変数 + 平均湿度, data = DF)
summary(lm8)

#分散分析
AOV8 <- aov(lm8)
summary(AOV8)

#オーバーフィッテイングの確認AIC
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

#標準変化域係数の作成
library(lm.beta)
LM8 <- lm.beta(lm8)
summary(LM8)




#lm4に平均湿度を加える線形回帰モデルの作成lm9
lm9 <- lm(感染者数1日 ~ 週の前半後半1 + 平均気温 + 
                ラグ変数 + ラグ変数*週の前半後半1 + 
                平均湿度, data = DF)
summary(lm9)

#分散分析
AOV9 <- aov(lm9)
summary(AOV9)

#オーバーフィッテイングの確認AIC
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

#標準変化域係数の作成
library(lm.beta)
LM9 <- lm.beta(lm9)
summary(LM9)


#lm3に緊急事態宣言ダミーを加える線形回帰モデルの作成lm10
lm10 <- lm(感染者数1日 ~ ラグ変数 + 週の前半後半1 + 
                 平均気温 + 緊急事態宣言, data = DF)
summary(lm10)

#分散分析）
AOV10 <- aov(lm10)
summary(AOV10)

#オーバーフィッテイングの確認AIC
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

#標準変化域係数の作成
library(lm.beta)
LM10 <- lm.beta(lm10)
summary(LM10)





#lm4に緊急事態宣言ダミーと
#ラグ変数*緊急事態宣言を加える線形回帰モデルの作成lm11
lm11 <- lm(感染者数1日 ~ 週の前半後半1 + 平均気温 + 
                 ラグ変数 + ラグ変数*週の前半後半1 + 
                 緊急事態宣言 + ラグ変数*緊急事態宣言, 
               data = DF)
summary(lm11)

#分散分析
AOV11 <- aov(lm11)
summary(AOV11)

#オーバーフィッテイングの確認AIC
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





#lm4にワクチン接種者数とラグ変数*ワクチン接種者数
#を加える線形回帰モデルの作成lm12
lm12 <- lm(感染者数1日 ~ 週の前半後半1 + 平均気温 + 
                 ラグ変数 + ラグ変数*週の前半後半1 + 
                 ワクチン接種者数 + ラグ変数*ワクチン接種者数, 
               data = DF)
summary(lm12)

#分散分析
AOV12 <- aov(lm12)
summary(AOV12)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, lm9, lm10, lm11, lm12)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, lm9, lm10, lm11, lm12)

#残差の分布
par(mfrow=c(2, 2))
plot(lm12)
par(mfrow=c(1, 1))

#多重共線性の確認
#10以上のものがある
library(car)
vif(lm12)

#標準変化域係数の作成
library(lm.beta)
LM12 <- lm.beta(lm12)
summary(LM12)





#lm4に冬ダミーとラグ変数*冬ダミー
#を加える線形回帰モデルの作成lm13
lm13 <- lm(感染者数1日 ~ 週の前半後半1 + 平均気温 +　
                 ラグ変数 + ラグ変数*週の前半後半1 + 
                 冬ダミー + ラグ変数*冬ダミー, 
               data = DF)
summary(lm13)

#分散分析
AOV13 <- aov(lm13)
summary(AOV13)

#オーバーフィッテイングの確認AIC
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

#標準変化域係数の作成
library(lm.beta)
LM13 <- lm.beta(lm13)
summary(LM13)







#lm4に平均湿度と緊急事態宣言ダミーと
#ラグ変数*緊急事態宣言とを加える線形回帰モデルの作成lm14
lm14 <- lm(感染者数1日 ~ 週の前半後半1 + 平均気温 + 
                 ラグ変数 + ラグ変数*週の前半後半1 + 
                 平均湿度 + 緊急事態宣言 + 
                 ラグ変数*緊急事態宣言, 
               data = DF)
summary(lm14)

#分散分析
AOV14 <- aov(lm14)
summary(AOV14)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, lm14)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, lm14)

#残差の分布
par(mfrow=c(2, 2))
plot(lm14)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm14)

#標準変化域係数の作成lm.beta
library(lm.beta)
LM14 <- lm.beta(lm14)
summary(LM14)






#lm3に緊急事態宣言ダミーと平均湿度と冬ダミーと
#ワクチン接種者数を加える線形回帰モデルの作成lm15
lm15 <- lm(感染者数1日 ~ 週の前半後半1 + 平均気温 + 
                 ラグ変数 + 平均湿度 +
                 緊急事態宣言 + 冬ダミー + 
                 ワクチン接種者数, 
               data = DF)
summary(lm15)

#分散分析
AOV15 <- aov(lm15)
summary(AOV15)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15)

#残差の分布
par(mfrow=c(2, 2))
plot(lm15)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm15)

#標準変化域係数の作成
library(lm.beta)
LM15 <- lm.beta(lm15)
summary(LM15)





#lm4にラグ長期休暇ダミーとラグ変数*ラグ長期休暇ダミー
#を加える線形回帰モデルの作成lm16
lm16 <- lm(感染者数1日 ~ 週の前半後半1 + 平均気温 + 
                 ラグ変数 + ラグ変数*週の前半後半1 + 
                 ラグ長期休暇ダミー + 
                 ラグ変数*ラグ長期休暇ダミー, 
               data = DF)
summary(lm16)

#分散分析
AOV16 <- aov(lm16)
summary(AOV16)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16)

#残差の分布
par(mfrow=c(2, 2))
plot(lm16)
par(mfrow=c(1, 1))

#多重共線性の確認
#10以上のものあり
library(car)
vif(lm16)

#標準変化域係数の作成
library(lm.beta)
LM16 <- lm.beta(lm16)
summary(LM16)



#lm4にラグ正月ダミーを加える線形回帰モデルの作成lm17
lm17 <- lm(感染者数1日 ~ ラグ変数 + 週の前半後半1 + 
                 平均気温 + ラグ変数*週の前半後半1 + 
                 ラグ正月ダミー, 
               data = DF)
summary(lm17)

#分散分析
AOV17 <- aov(lm17)
summary(AOV17)

#オーバーフィッテイングの確認AIC
AIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17)

#BIC（単純なモデルを良く評価）
BIC(lm0, lm1, lm2, lm3, lm4, lm5, lm6, 
    lm7, lm8, lm9, lm10, lm11, lm12, lm13, 
    lm14, lm15, lm16, lm17)

#残差の分布
par(mfrow=c(2, 2))
plot(lm17)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm17)

#標準変化域係数の作成
library(lm.beta)
LM17 <- lm.beta(lm17)
summary(LM17)

