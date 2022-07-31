#データの読み込み
DF <- read.table("covid19_tokyo_daily_last_modified.csv", 
                 sep = ",", 
                 header = TRUE, 
                 stringsAsFactors = FALSE, 
                 fileEncoding="UTF-8-BOM")

head(DF)

#Sは感受性人口、Tは感染性人口を表している
#Rは治癒人口を表している
#SIはS*Iを示している
#従来株は感染者数1日から変異株を除いたものである
#ラグは感染者数1日と変異株と従来株は1日、
#その他の変数は一週間をラグとして置いている

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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成lm.beta
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
library(lm.beta)
LM17 <- lm.beta(lm17)
summary(LM17)







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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成lm.beta
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成lm.beta
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
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

#標準偏回帰係数の作成
library(lm.beta)
LM40 <- lm.beta(lm40)
summary(LM40)




#lm40を採用する

