#utf-8

setwd("F:/spssmoneny/T1801756")
data1 <- read.csv(file = '数据.csv',na.strings = " ")
str(data1)
data1$MS <- tolower(data1$MS)
str(data1)
data1$MS <- factor(data1$MS)
str(data1)
library(DMwR)
data1 <- knnImputation(data1)
str(data1)
#####num1 MS组的NLR与非MS组的NLR差异是否有统计学意义
a <- aov(data1$NLR~data1$MS)
summary(a)
library(gplots)
plotmeans(data1$NLR~data1$MS)

#########num2 Pearson检验银屑病患者血 NLR与PASI评分是否具有相关性
#corr1 <- cor(data1[,c(2,6)],method = 'pearson')
corr1 <- cor.test(data1$NLR,data1$PASI,method = 'pearson')
corr2 <- cor.test(data1$NLR,data1$PASI,method = 'spearman')

####检验过，相关系数不大


########num3 是否NLR越高，发生MS的可能性就越大

fit1 <- glm(MS ~ NLR , data = data1 , family = binomial(link = logit))
summary(fit1)
####显著，也可能是有离群点的原因
plot(fit1)
cdplot(MS ~ NLR , data = data1)

########num3 去掉离群点
low_nlr <- quantile(data1$NLR,probs = 0.05)
high_nlr <- quantile(data1$NLR,probs = 0.95)
data1_nlr <- data1[data1$NLR>=low_nlr&data1$NLR<=high_nlr,]
cdplot(MS ~ NLR , data = data1_nlr)


########num4  采用受试者工作特征曲线（ROC）检验了 NLR对银屑病患者MS发生预测价值
#####去掉离群值预测
#nlr_mean <- mean(data1_nlr$NLR)
data1_nlr$predict <- ifelse(data1_nlr$NLR > mean(data1_nlr$NLR),yes = 'yes',no = 'no')
data1_nlr$predict <- factor(data1_nlr$predict)
str(data1_nlr)
library(pROC)
plot.roc(as.numeric(data1_nlr$predict)~as.numeric(data1_nlr$MS),data = data1_nlr,col = 2)
