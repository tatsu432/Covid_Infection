DF <- read.table("covid19_tokyo_daily (1).csv", 
                 sep = ",", 
                 header = TRUE, 
                 stringsAsFactors = FALSE, 
                 fileEncoding="UTF-8-BOM",)

DF2 <- read.table("covid_modified.csv", 
                 sep = ",", 
                 header = TRUE, 
                 stringsAsFactors = FALSE, 
                 fileEncoding="UTF-8-BOM",)

head(DF)

hist(DF$感染者数1日)

hist(DF$平均気温)

summary(DF[-1])

library(ggplot2)

ggplot(DF)+
  geom_histogram(aes(感染者数1日), 
                 fill="skyblue", 
                 alpha=0.8)

pairs(DF[, -c(1)])

pairs(DF[, c(2, 3, 4, 5, 6)])

library(lattice)

splom(DF[, c(2, 3, 4, 5, 6)])

library(ggplot2)

library(GGally)

ggpairs(DF[, c(2, 3, 4, 5, 6)], 
        aes())

ggpairs(DF2[, c(2, 3, 4, 5, 6, 14)], 
        aes(colour=as.factor(曜日), 
            alpha=0.5), 
        )
ggpairs(DF2[, c(2, 3, 4, 5, 6, 14)], 
        )

head(DF2)

COR <- cor(DF2[, c(2, 3, 4, 5, 6)])
COR


lm1 <- lm(感染者数1日 ~ 平均気温, data=DF2)
summary(lm1)

lm2 <- lm(感染者数1日 ~ 曜日, data=DF2)
summary(lm2)

DF3 <- read.table("covid_modified2.csv", 
                  sep = ",", 
                  header = TRUE, 
                  stringsAsFactors = FALSE, 
                  fileEncoding="UTF-8-BOM",)

head(DF3)
ggpairs(DF2[, c(2, 6, 14)], 
        aes(colour=as.factor(曜日), 
            alpha=0.5)
)

ggpairs(DF3[, c(2, 3, 4, 5, 6, 15)],
        aes(colour=as.factor(平日休日), 
            alpha=0.5)
        )

lm2 <- lm(感染者数1日 ~ 平日休日, data=DF3)
summary(lm2)
AOV2 <- aov(感染者数1日 ~ 平日休日, data=DF3)
summary(AOV2)

lm3 <- lm(感染者数1日 ~ 曜日, data=DF3)
summary(lm3)
AOV3 <- aov(感染者数1日 ~ 曜日, data=DF3)
summary(AOV3)
TukeyHSD(AOV3)

boxplot(感染者数1日 ~ 平日休日, data=DF3, col="blue")
boxplot(感染者数1日 ~ 曜日, data=DF3, col="blue")



library(psych)
result.prl1 <- fa.parallel(DF[, c(7:14)], fm="ml")