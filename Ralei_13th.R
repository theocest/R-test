setwd("E:/anaconda_r")

#github testing

#install.packages("ggplot2")
library(ggplot2)

#模拟生成一个正态分布，表示均值为7000，标准差为2000，数目为10000的男性毕业生收入
data1 <- rnorm(10000, mean = 7000, sd = 2000)
data1 <- as.data.frame(data1)
data1[,2] <- "male"
names(data1) <- c("income", "gender")
ggplot(data1, aes(income)) + geom_histogram(stat = "density")

#模拟生成一个正态分布，表示均值为5000，标准差为2000，数目为10000的女性毕业生收入
data2 <- rnorm(10000, mean = 5000, sd = 2000)
data2 <- as.data.frame(data2)
data2[,2] <- "female"
names(data2) <- c("income", "gender")
ggplot(data2, aes(income)) + geom_histogram(stat = "density")

#将男性数据与女性数据组合起来
newdata <- rbind(data1, data2)
View(newdata)

ggplot(newdata, aes(income)) + geom_histogram(stat = "density") + facet_grid(gender~.)



#检验正态分布的三种方法
#1.qq图
qqnorm(data1[,1])
qqline(data1[,1], col = "darkred")

#2.夏皮罗检验（shapiro.test）当w接近1，p>0.05时，说明数据符合正态分布
shapiro.test(sample(data1[,1], 5000))
save_data <- c()
for (i in 1:100){
  sample_data <- shapiro.test(sample(data1[,1], 5000))
  save_data <- append(save_data,sample_data[["p.value"]]) #因为sample_data是列表所以用两个方括
}
length(save_data[save_data<0.05])  #????

#保存p值



#3.KS检验，D接近0，且p>0.05时，说明数据符合正态分布
ks.test(
  data1[,1],
  rnorm(10000, mean = mean(data1[,1]), sd = sd(data1[,1]))
)


#关于正态分布的4个函数及其应用
#由已知的正态分布总体来分析个体
#已知某大学男性毕业生收入的均值为7000，标准差为2000，求：
#1.1 若甲同学的收入大于80%的人，那么他的收入是多少？（由概率求值问题）
qnorm(0.8,mean = 7000, sd = 2000)

#1.2 乙同学收入为8500左右的概率是多少？（点概率问题）
dnorm(8500, mean = 7000, sd = 2000)

#1.3 已知丙同学的收入为9000，他的收入会比百分之多少的人高？（区间概率问题）
pnorm(9000, mean = 7000, sd = 2000)
#是否函数会根据参数进行正态分布标准化处理？

#将一般正态分布转换成标准正态分布
zdata <- (data1[,1]-mean(data1[,1]))/sd(data1[,1]) #此处zdata是向量 
#？？？ggplot里的参数数据结构
ggplot(data = NULL, aes(zdata)) + geom_histogram(stat = "density")

#抽样分布
#样本均值的抽样分布

#中心极限定理
#1.当总体分布服从正态分布时，任意一个样本，无论样本容量多大，样本均值的抽样分布都服从正态分布
#每次抽样的样本容量
n <- 30

#统计量的统计函数
f <- mean

#抽样次数
times <- 1000

#保存抽样后的统计量，这里指的是均值
c_data <- c()

for (i in 1:times) {
  sampledata <- sample(data1[,1], n)
  c_data <- append(c_data, f(sampledata))
}

#绘出统计量的分布图形
ggplot(data = NULL, aes(c_data)) + geom_histogram(stat = "density")

shapiro.test(c_data)

#2.当总体分布不服从正态分布时，只要样本容量足够大，样本均值的抽样分布就会近似服从正态分布

#每次抽样的样本容量
n <- 30

#统计量的统计函数
f <- mean

#抽样次数
times <- 1000

#保存抽样后的统计量，这里指的是均值
c_data <- c()

data <- read.csv("areidata11.csv")
ggplot(data,aes(收入)) + geom_histogram(stat = "density")
for (i in 1:times) {
  sampledata <- sample(data[,3], n)
  c_data <- append(c_data, f(sampledata))
}

#绘出统计量的分布图形
ggplot(data = NULL, aes(c_data)) + geom_histogram(stat = "density")
shapiro.test(c_data)

#从均值和标准差来验证中心极限定理
#样本均值的抽样分布，均值等于总体均值，标准差（即标准误）等于总体标准差/根号n

#样本均值抽样分布的均值
xmean <- mean(data[,3])

#样本均值抽样分布的标准误
xsd <- sd(data[,3])/sqrt(n)

ks.test(
  c_data,
  rnorm(times, mean = xmean, sd = xsd)
)

save_data <- c()
for (i in 1:100) {
  sample_data <- ks.test(c_data, rnorm(times, mean = xmean, sd = xsd))
  save_data <- append(save_data, sample_data[["p.value"]])
}
length(save_data[save_data>0.05])