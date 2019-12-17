library(leaps)

###### P0 准备工作 ######

# 初始化数据
bd.tout <- read.table(file="housing.csv")
names(bd.tout) <- c('CRIM', 'ZN', 'INDUS', 'CHAS', 'NOX','RM', 'AGE', 'DIS', 'RAD', 'TAX', 'PTRATIO', 'B', 'LSTAT', 'MEDV')
head(bd.tout)

" 缩写解释:
CRIM：城镇人均犯罪率。
ZN：住宅用地超过 25000 sq.ft. 的比例。
INDUS：城镇非零售商用土地的比例。
CHAS：查理斯河空变量（如果边界是河流，则为1；否则为0）。---valeur qualitative
NOX：一氧化氮浓度。
RM：住宅平均房间数。
AGE：1940 年之前建成的自用房屋比例。
DIS：到波士顿五个中心区域的加权距离。
RAD：辐射性公路的接近指数。                             ---valeur qualitative
TAX：每 10000 美元的全值财产税率。
PTRATIO：城镇师生比例。
B：1000（Bk-0.63）^ 2，其中 Bk 指代城镇中黑人的比例。
LSTAT：人口中地位低下者的比例。
MEDV：自住房的平均房价，以千美元计。
"

# 删除valeur qualitative
bd.svq <- bd.tout[,-c(4,9)] # svq = sans valeurs qualitatives
head(bd.svq)
dim(bd.svq)

# 分割数据，前406条数据用来训练模型，后100条数据用来测试
bd.svq.modele <- bd.svq[1:406,]
bd.svq.controle <- bd.svq[407:506,]

# 模型筛选
reg.mul.svq <- lm(MEDV ~ ., data = bd.svq.modele)
summary(reg.mul.svq)
choix <- regsubsets(MEDV ~ ., data = bd.svq.modele, nvmax = dim(bd.svq.modele)[2]-1)
plot(choix, scale = "bic")
"
结果：
!!! 若使用全部506条数据，则结论为：
单变量线性回归的最佳模型的变量为LSTAT 
多变量线性回归的最佳模型的变量为ZN, NOX, RM, DIS, PTRATIO, B, LSTAT

!!! 若仅使用前406条数据，则结论为：
单变量线性回归的最佳模型的变量为RM
多变量线性回归的最佳模型的变量为CRIM, ZN, NOX, RM, DIS, PTRATIO, LSTAT

更多操作参见：https://www.cnblogs.com/runner-ljt/p/4856476.html
"

###### P1 第一部分 单变量线性回归 ######

# 建模
reg.sim.opt <- lm(MEDV ~ RM,  data = bd.svq.modele) #sim = simple, opt=optimal
summary(reg.sim.opt)

#### P1-1 线性回归图 ####
plot(bd.svq.modele$RM, bd.svq.modele$MEDV)
abline(reg.sim.opt, col='red')

#### P1-2 résidus/valeurs aberrantes ####
residu.sim <- rstudent(reg.sim.opt)

# residus分布图
n <- length(bd.svq.modele$MEDV)
plot(1:n, residu.sim, xlab = 'Index', ylab='Résidus studentisés', main = 'Valeurs aberrantes')
abline(-2, 0, lty = 2)
abline(2, 0, lty = 2)
IDval.ab <- (1:n)[abs(residu.sim)>2]
text(IDval.ab, residu.sim[IDval.ab], IDval.ab, pos = 4, col = 'blue')

# 标注异常值的数据图 !!!可与levier部分合并
plot(bd.svq.modele$RM, bd.svq.modele$MEDV, xlab = 'Nombre de chambres en moyenne', 
     ylab = 'Prix de maison (en 1000$)', main = 'Donnees avec valeurs aberrantes')
abline(coef(reg.sim.opt), col = 'green')
points(bd.svq.modele$RM[IDval.ab], bd.svq.modele$MEDV[IDval.ab], col = 'blue', pch = 16) 
text(bd.svq.modele$RM[IDval.ab], bd.svq.modele$MEDV[IDval.ab], IDval.ab, pos = 4, col = 'blue')

# residus正态分布分析
quant.residu <- qt((1:n)/n,n-3)
plot(quant.residu, sort(residu.sim), xlab = 'Student T(n-p-1)',
     ylab = 'Quantiles empiriques des résidus', main = 'QQ-plot des résidus')
abline(0, 1, col ='blue')

# ???是什么？意义？
residu.sim.std <- rstandard(reg.sim.opt)
plot(1:n, reg.sim.opt$residuals, main = 'Residus', xlab = 'Index', ylab = 'Residus')
points(1:n, residu.sim.std, pch = 4, col = 'blue')
points(1:n, residu.sim, pch = 5, col = 'red')
legend(0, 40, c('estime', 'standardise', 'studentise'),
       col = c('black', 'blue', 'red'), pch = c(1, 4, 5))

plot(1:n, residu.sim.std, pch = 4, col = 'blue', main = 'Residus', xlab = 'Index',
     ylab = 'Residus')
points(1:n, residu.sim, col = 'red')
legend(0, 6 , c('standardise', 'studentise'), col = c('blue', 'red'), pch = c(4, 1))

#### P1-3 point levier ####
levier.sim <- hatvalues(reg.sim.opt)

# point levier分布图
plot(1:n, levier.sim, xlab = 'Index', ylab = 'Poids h_ii', main = 'Points leviers')
p <- reg.sim.opt$rank
seuil1 <- 2*p/n
seuil2 <- 3*p/n
abline(seuil1,0,lty=2)
abline(seuil2,0,lty=3)
IDlev <- (1:n)[levier.sim>seuil2]
text(IDlev, levier.sim[IDlev], IDlev, pos = 4, col = 'blue')

# 标注residus和point levier的数据图
plot(bd.svq.modele$RM, bd.svq.modele$MEDV, xlab = 'Nombre de chambres en moyenne', 
     ylab = 'Prix de maison (en 1000$)', main = 'Donnees avec valeurs aberrantes et points leviers')
abline(coef(reg.sim.opt), col = 'green')
points(bd.svq.modele$RM[IDval.ab], bd.svq.modele$MEDV[IDval.ab], col = 'blue', pch = 16) 
text(bd.svq.modele$RM[IDval.ab], bd.svq.modele$MEDV[IDval.ab], IDval.ab, pos = 4, col = 'blue')
points(bd.svq.modele$RM[IDlev], bd.svq.modele$MEDV[IDlev], col = 'red', pch = 16) 
text(bd.svq.modele$RM[IDlev], bd.svq.modele$MEDV[IDlev], IDlev, pos = 4, col = 'red')
legend(3.5, 50, c('aberrante', 'levier'), col = c('blue','red'), pch = 16)

#### P1-4 distance de Cook ####
cook.sim <- cooks.distance(reg.sim.opt)

plot(1:n, cook.sim, xlab = 'Index', ylab = 'Distance de Cook', main = 'Distance de Cook',
     ylim = c(0,.15))
s1 <- qf(0.5, p, n-p)
s2 <- qf(0.1, p, n-p)
abline(s2, 0, lty = 2)
abline(s1, 0, lty = 3)

#### P1-5 intervalles de confiance ####
# 不知道在有controle组的时候是不是有不同的分析方法

###### P2 第二部分 多变量线性回归 ######

# 建模
bd.mul.opt <- bd.svq.modele[,c(1,2,4,5,7,9,11,12)]
head(bd.mul.opt)

reg.mul.opt <- lm(MEDV ~ ., data = bd.mul.opt)
summary(reg.mul.opt)

#### P2-1 线性回归图 ####

# 相关性图
cor(bd.mul.opt)
pairs(bd.mul.opt)

# 各变量的相关性图
par(mfrow=c(2,4))
coef.mul <- coef(reg.mul.opt)
plot(bd.mul.opt$MEDV ~ bd.mul.opt$CRIM)
abline(coef.mul[1], coef.mul[2])
plot(bd.mul.opt$MEDV ~ bd.mul.opt$ZN)
abline(coef.mul[1], coef.mul[3])
plot(bd.mul.opt$MEDV ~ bd.mul.opt$NOX)
abline(coef.mul[1], coef.mul[4])
plot(bd.mul.opt$MEDV ~ bd.mul.opt$RM)
abline(coef.mul[1], coef.mul[5])
plot(bd.mul.opt$MEDV ~ bd.mul.opt$DIS)
abline(coef.mul[1], coef.mul[6])
plot(bd.mul.opt$MEDV ~ bd.mul.opt$PTRATIO)
abline(coef.mul[1], coef.mul[7])
plot(bd.mul.opt$MEDV ~ bd.mul.opt$LSTAT)
abline(coef.mul[1], coef.mul[8])

#### P2-2 résidus/valeurs aberrantes ####

# ???是什么？意义？
plot(reg.mul.opt)

#### P2-3 Erreur quadratique moyenne ####

# 与不进行变量选择的模型的对比
medv1 <- predict(reg.mul.svq, bd.svq.controle[,-12])
medv2 <- predict(reg.mul.opt, bd.svq.controle[,c(1,2,4,5,7,9,11,12)])

eqm1 <- 0.01*sum((medv1-bd.svq.controle$MEDV)^2)
eqm2 <- 0.01*sum((medv2-bd.svq.controle$MEDV)^2)

###### P3 ######
bd.qua <- bd.tout[,c("MEDV","CHAS","RAD")]

#### P3-1 représentation des données ####
table(bd.qua$CHAS)
boxplot(MEDV~CHAS, data = bd.qua)

table()

#### P3-2 estimation des paramètres pour CHAS ####
reg.chas <- lm(MEDV~CHAS, data = bd.qua)
summary(reg.chas)
anova(reg.chas)

n <- nrow(bd.qua)
rs.chas <- rstudent(reg.chas)
plot(1:n, rs.chas)

df.chas <- data.frame(index=1:n,rs = rs.chas)
df.chas <- data.frame(df.chas,CHAS=bd.qua$CHAS)
head(df.chas)

par(mfrow = (c(1,2)))
plot(df.chas[df.chas$CHAS== 0, ]$index, df.chas[df.chas$CHAS == 0, ]$rs,main="CHAS = 0",xlab="Indice",ylab="Résidus studentisés")
plot(df.chas[df.chas$CHAS== 1, ]$index, df.chas[df.chas$CHAS == 1, ]$rs,main="CHAS = 1",xlab="Indice",ylab="Résidus studentisés")

par(mfrow = (c(1,1)))
boxplot(rs ~CHAS, df.chas)

#### P3-3 estimation des paramètres pour RAD ####
reg.rad <- lm(MEDV~RAD, data = bd.qua)
summary(reg.rad)
anova(reg.rad)

n <- nrow(bd.qua)
rs.rad <- rstudent(reg.rad)
plot(1:n, rs.rad)

df.rad <- data.frame(index=1:n,rs = rs.rad)
df.rad <- data.frame(df.rad, RAD=bd.qua$RAD)
head(df.rad)

par(mfrow = (c(1,2)))
plot(df.rad[df.rad$RAD== 0, ]$index, df.rad[df.rad$RAD == 0, ]$rs,main="RAD = 0",xlab="Indice",ylab="Résidus studentisés")
plot(df.rad[df.rad$RAD== 1, ]$index, df.rad[df.rad$RAD == 1, ]$rs,main="RAD = 1",xlab="Indice",ylab="Résidus studentisés")

par(mfrow = (c(1,1)))
boxplot(rs ~ RAD, df.rad)

##### TEST SENSIBLE #####
sem <- lm(MEDV ~ B,  data = bd.svq.modele) #sim = simple, opt=optimal
summary(sem)
anova(sem)
cor(bd.tout)

plot(bd.svq.modele$B, bd.svq.modele$MEDV)
abline(sem, col='red')

residu.sem <- rstudent(sem)

n <- length(bd.svq.modele$MEDV)
plot(1:n, residu.sem, xlab = 'Index', ylab='Résidus studentisés', main = 'Valeurs aberrantes')
abline(-2, 0, lty = 2)
abline(2, 0, lty = 2)
IDval.ab <- (1:n)[abs(residu.sem)>2]
text(IDval.ab, residu.sem[IDval.ab], IDval.ab, pos = 4, col = 'blue')

levier.sem <- hatvalues(sem)

plot(1:n, levier.sem, xlab = 'Index', ylab = 'Poids h_ii', main = 'Points leviers')
p <- sem$rank
seuil1 <- 2*p/n
seuil2 <- 3*p/n
abline(seuil1,0,lty=2)
abline(seuil2,0,lty=3)
IDlev <- (1:n)[levier.sem>seuil2]
text(IDlev, levier.sem[IDlev], IDlev, pos = 4, col = 'blue')

bd.sem.nonlev <- bd.svq.modele[-c(19,33,35,103,135,146,147,154,157,166,168,368,385),]
sem.nonlev <- lm(MEDV ~ B, data = bd.sem.nonlev)
summary(sem.nonlev)

plot(bd.sem.nonlev$B, bd.sem.nonlev$MEDV)
abline(sem, col='red')

cor(bd.sem.nonlev)