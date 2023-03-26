#全体のデータ
#data <- read.csv("data.csv",fileEncoding="UTF-8")
#個々人のデータ
#data <- read.csv("dataEachPerson.csv",fileEncoding="UTF-8")
#滑りのみ，転がりのみ，滑りと転がりの3群のデータ
data <- read.csv("motionForJoint.csv",fileEncoding="UTF-8")

#一定速度，一定加速度(加速と減速間隔が同じ)，一定加速度(加速が減速より長い)，一定加速度(加速が減速より短い)
#，一定躍度(加速と減速間隔が同じ)，一定躍度(加速が減速より長い)，一定躍度(加速が減速より短い)
#data <- read.csv("motionForLegs.csv",fileEncoding="UTF-8")

#一つの画面にm行n列の図を行順に書く
par(mfrow=c(2,1))

#br<-seq(40,100,15)
barplot(data[,1],ylab = "Humanity", xlim = c(0,10), main = "Video1", col = "#ff00ff40")
barplot(data[,2],ylab = "Humanity", xlim = c(0,10), main = "Video2", col = "#0000ff40")
#barplot(data[,3],ylab = "Humanity", xlim = c(0,10), main = "Video3", col = "#0000ff40")
#barplot(data[,4],ylab = "Humanity", xlim = c(0,10), main = "Video4", col = "#0000ff40")
#barplot(data[,5],ylab = "Humanity", xlim = c(0,10), main = "Video5", col = "#0000ff40")
#barplot(data[,6],ylab = "Humanity", xlim = c(0,10), main = "Video6", col = "#0000ff40")
#barplot(data[,7],ylab = "Humanity", xlim = c(0,10), main = "Video7", col = "#0000ff40")
#barplot(data[,8],ylab = "Humanity", xlim = c(0,10), main = "Video8", col = "#0000ff40")
#barplot(data[,9],ylab = "Humanity", xlim = c(0,10), main = "Video9", col = "#0000ff40")
#barplot(data[,10],ylab = "Humanity", xlim = c(0,10), main = "Video10", col = "#0000ff40")

#for文
#for(i in 1:10){
#	plot(data[,i],ylab = "Humanity", xlim = c(0,10), main = "Video"+i, col = "#ff00ff40"))
#}

t.test(x=data[,1],y=data[,2],conf.level=0.95,var.equal=TRUE)

#F検定
var.test(x=data[,1],y=data[,2],conf.level=0.95)

#一元配置分散分析(ANOVA)
#ライブラリの追加
library(ggpubr) #分散分析のプロット
library(rstatix) #分散分析や事後テストの検定を行うため #検定結果をプロット上に表示させる
library(ggplot2)
library(gridExtra)

#正規性の確認
qqnorm(data$X1,ylab="Motion of joints")
qqline(data$X1)
shapiro_test <- shapiro.test(t(data$X1))

#Krushal-Wallis漸近検定，3群の場合
x1 <- c(8,6,1,3,5,3,7,3,2,5,3,3,1,5,3,3,2,7,7) #滑りのみ
x2 <- c(5,5,1,2,5,4,7,0,3,5,4,3,7,3,8,3,2,7,8) #転がりのみ
x3 <- c(10,4,1,5,4,2,6,3,3,5,1,8,3,7,4,3,3,7,6) #滑りと転がりの混合運動
dat <- c(x1,x2,x3)
# 各群内の順位和
r1<- sum(rk[1: 19])
r2<- sum(rk[20: 38])
r3<- sum(rk[39: 57])
# Kruskal-Wallis 検定統計量
h<- 12/(57*58)*(r1^2/19+r2^2/19+r3^2/19)-3*(57+1)
# カイ二乗分布を用いた p 値の算出
pchisq(h, 2, lower=F)

#Mann-Whitney U(Wilcoxon順位和)漸近検定，連続補正なし
wilcox.test(x1,x2,exact=F,correct=F)


v1 = [5,4,1,1,4,2,8,2,7,5,2,10,9,6,6,3,2,4,7]
a1 = [9,6,1,3,6,6,9,4,4,7,2,8,8,2,7,3,2,7,5]
a2 = [9,3,3,2,5,3,7,8,6,8,4,8,6,5,9,3,2,8,4]
a3 = [10,3,1,2,5,3,7,8,7,8,5,10,6,6,9,3,2,8,4]
j1 = [10,4,1,5,4,2,6,3,3,5,1,8,3,7,4,3,3,7,6]
j2 = [10,2,3,3,5,2,7,4,3,8,2,10,4,3,8,4,2,8,2]
j3 = [8,7,2,5,3,3,10,6,3,8,2,3,2,3,4,4,2,7,6]

data <- read.csv("motionForLegs.csv",fileEncoding="UTF-8")
#正規性の確認
qqnorm(data$X1,ylab="Motion of legs")
qqline(data$X1)
shapiro_test <- shapiro.test(t(data$X1))

%ヨー軸のデータの正規性を確認(シャピロ・ウィルク検定)
X1 <- c(10,4,1,5,4,2,6,3,3,5,1,8,3,7,4,3,3,7,6)
hist(X1, col = "green")
X2 <- c(9,3,1,5,4,3,3,8,7,7,2,7,8,9,9,4,3,7,4)
hist(X2,col = "green")
shapiro.test(X1)
shapiro.test(X2)

%有意差マークを半自動的につける(統計解析と図示)
%サンプルデータの取得
data <- read.csv("dataEachPerson.csv",fileEncoding="UTF-8")

#パッケージのインストールと読み込み
if (!require(dplyr))install.packages('dplyr')
library(dplyr)
data2 <- data
if (!require(ggplot2))install.packages('ggplot2')
library(ggplot2)
#ここで，気が付いた．有意差がでなかったなら，アスタリスク書けないじゃん
#でも，箱ひげ図は書ける

X1 <- c(10,4,1,5,4,2,6,3,3,5,1,8,3,7,4,3,3,7,6)
X2 <- c(9,3,1,5,4,3,3,8,7,7,2,7,8,9,9,4,3,7,4)
X3 <- c(8,6,1,3,5,3,7,3,2,5,3,3,1,5,3,3,2,7,7)
X4 <- c(5,5,1,2,5,4,7,0,3,5,4,3,7,3,8,3,2,7,8)
#X5 <- c(5,4,1,1,4,2,8,2,7,5,2,10,9,6,6,3,2,4,7)
#X6 <- c(9,6,1,3,6,6,9,4,4,7,2,8,8,2,7,3,2,7,5)
#X7 <- c(9,3,2,5,3,7,8,6,8,4,8,6,5,9,3,2,8,4)
#X8 <- c(10,3,1,2,5,3,7,8,7,8,5,10,6,6,9,3,2,8,4)
#X9 <- c(10,2,3,3,5,2,7,4,3,8,2,10,4,3,8,4,2,8,2)
#X10 <- c(8,7,2,5,3,3,10,6,3,8,2,3,2,3,4,4,2,7,6)
boxplot(X1,X2,X3,X4,las=1,cex.lab=1.5,xlab="条件",ylab="",cex.axis=1.2) 

tategaki <- function(x){
  x <- chartr("ー", "丨", x) # 長音符の処理
  x <- strsplit(split="", x)
  sapply(x, paste, collapse="\n")
}

mtext(tategaki("人らしさ"),
      side = 2, las = 1, ,cex.lab=1.5, line = 3)

memori <- c(1,2,3,4)
axis(side=1,　# X軸に挿入。Y軸はside=2 
     at=memori, # 上記で作成したxvaluesを利用。
     cex.axis=1.5) 

