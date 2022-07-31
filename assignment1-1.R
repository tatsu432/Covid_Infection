#データの読み込み
DF <- read.table("covid19_tokyo_daily_last_modified.csv", 
                 sep = ",", 
                 header = TRUE, 
                 stringsAsFactors = FALSE, 
                 fileEncoding="UTF-8-BOM")

#データ構造の確認
head(DF)

#日付データに変換したDFdateを作成
DFdate <- DF
DFdate[, 1] <- as.Date(DF[, 1], 
                  "%Y/%m/%d")

head(DFdate)

#感染者数1日と平均気温の時系列データを比較
library(scales)
library(ggplot2)
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
gridExtra::grid.arrange(infection, temperature, ncol = 1)

#散布図を用いて、カテゴリごとの違いを確認
library(ggplot2)
library(GGally)
#月火水木金土日で分ける場合
ggpairs(DF[, c(2, 6)], 
        aes(colour=as.factor(DF$曜日), 
            alpha=0.5))
#平日か休日かで分ける場合
ggpairs(DF[, c(2, 6)], 
        aes(colour=as.factor(DF$平日か休日かlast), 
            alpha=0.5))
#平日か休日か休日の前日かで分ける場合
ggpairs(DF[, c(2, 6)], 
        aes(colour=as.factor(DF$平日か休日か休日の前日かlast), 
            alpha=0.5))
#日月火水を週の前半、木金土を週の後半として分ける場合
ggpairs(DF[, c(2, 6)], 
        aes(colour=as.factor(DF$週の前半後半1), 
            alpha=0.5))
#月火水を週の前半、木金土日を週の後半として分ける場合
ggpairs(DF[, c(2, 6)], 
        aes(colour=as.factor(DF$週の前半後半2), 
            alpha=0.5))

#箱ひげ図を用いて、カテゴリごとの違いを確認

library(ggplot2)
library(GGally)
#月火水木金土日で分ける場合
ggplot(DF) +
  geom_boxplot(aes(x=曜日, 
                   y=感染者数1日))
#平日か休日かで分ける場合
ggplot(DF) +
  geom_boxplot(aes(x=平日か休日かlast, 
                   y=感染者数1日))
#平日か休日か休日の前日かで分ける場合
ggplot(DF) +
  geom_boxplot(aes(x=平日か休日か休日の前日かlast, 
                   y=感染者数1日))
#日月火水を週の前半、木金土を週の後半として分ける場合
ggplot(DF) +
  geom_boxplot(aes(x=週の前半後半1, 
                   y=感染者数1日))
#月火水を週の前半、木金土日を週の後半として分ける場合
ggplot(DF) +
  geom_boxplot(aes(x=週の前半後半2, 
                   y=感染者数1日))

#曜日と平均気温を説明変数とした
#線形回帰モデルの作成lm0
lm0 <- lm(感染者数1日 ~ 曜日 + 平均気温, data = DF)
summary(lm0)

#分散分析（本当に差があるのか）
AOV0 <- aov(lm0)
summary(AOV0)

#AIC（小さいほど良い）の確認
AIC(lm0)

#BIC（単純なモデルを良く評価）の確認
BIC(lm0)

#残差の分布
par(mfrow=c(2, 2))
plot(lm0)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm0)

#標準変化域係数の作成
library(lm.beta)
LM0 <- lm.beta(lm0)
summary(LM0)







#週の前半後半1と平均気温を説明変数とした
#線形回帰モデルの作成lm1
lm1 <- lm(感染者数1日 ~ 週の前半後半1 + 平均気温, data = DF)
summary(lm1)

#分散分析（本当に差があるのか）
AOV1 <- aov(lm1)
summary(AOV1)

#AIC（小さいほど良い）の確認
AIC(lm0, lm1)

#BIC（単純なモデルを良く評価）の確認
BIC(lm0, lm1)

#残差の分布
par(mfrow=c(2, 2))
plot(lm1)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm1)

#標準変化域係数の作成
library(lm.beta)
LM1 <- lm.beta(lm1)
summary(LM1)





#週の前半後半2と平均気温を説明変数とする
#線形回帰モデルの作成lm2
lm2 <- lm(感染者数1日 ~ 週の前半後半2 + 平均気温, data = DF)
summary(lm2)

#分散分析（本当に差があるのか）
AOV2 <- aov(lm2)
summary(AOV2)

#AIC（小さいほど良い）の確認
AIC(lm0, lm1, lm2)

#BIC（単純なモデルを良く評価）の確認
BIC(lm0, lm1, lm2)

#残差の分布
par(mfrow=c(2, 2))
plot(lm2)
par(mfrow=c(1, 1))

#多重共線性の確認
library(car)
vif(lm2)

#標準変化域係数の作成
library(lm.beta)
LM2 <- lm.beta(lm2)
summary(LM2)


#lm0, lm1, lm2の中で、残差の分布や
#多重共線性に大きな違いはなく、
#AIC, BICはlm1が優れており、有意差検定においても、
#lm1が一番差が認められ、
#平均気温が下がると新規感染者が増加
#週の後半となると新規感染者数が増加となり、
#それぞれの偏回帰係数の意味も明瞭なことから、
#lm1を採用する
