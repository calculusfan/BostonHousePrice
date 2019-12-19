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

# 分割数据，前406条数据用来训练模型，后100条数据用来测试
mdds.tout <- bd.tout[1:406,]   # mdds = modèles de données
ctrl.tout <- bd.tout[407:506,] # ctrl = contrôle

# 区分valeur qualitative与valeur quantitative
mdds.svq <- mdds.tout[,!names(mdds.tout) %in% c('CHAS','RAD')] # svq = sans valeurs qualitatives
mdds.qua <- mdds.tout[,c('CHAS','RAD','MEDV')]

# 分割数据，前406条数据用来训练模型，后100条数据用来测试
mdds.svq <- bd.svq[1:406,]
bd.svq.controle <- bd.svq[407:506,]

# 模型筛选
reg.mul.svq <- lm(MEDV ~ ., data = mdds.svq)
summary(reg.mul.svq)
choix <- regsubsets(MEDV ~ ., data = mdds.svq, nvmax = dim(mdds.svq)[2]-1)
plot(choix, scale = "bic")
"
结果：
!!! 若使用全部506条数据，则结论为：
单变量线性回归的最佳模型的变量为LSTAT 
多变量线性回归的最佳模型的变量为ZN, NOX, RM, DIS, PTRATIO, B, LSTAT

!!! 若仅使用前406条数据，则结论为：（使用此结果）
单变量线性回归的最佳模型的变量为RM
多变量线性回归的最佳模型的变量为CRIM, ZN, NOX, RM, DIS, PTRATIO, LSTAT

更多操作参见：https://www.cnblogs.com/runner-ljt/p/4856476.html
"

# 依模型将变量重新组合形成新数据库
mdds.sim.opt <- mdds.tout[c('RM','MEDV')]
mdds.mul.opt <- mdds.tout[c('CRIM', 'ZN', 'NOX', 'RM', 'DIS', 'PTRATIO', 'LSTAT', 'MEDV')]

###### P1 第一部分 单变量线性回归 ######

# 建模
reg.sim.opt <- lm(MEDV ~ RM,  data = mdds.sim.opt) #sim = simple, opt=optimal
summary(reg.sim.opt)

#### P1-1 线性回归图 ####
plot(mdds.sim.opt$RM, mdds.sim.opt$MEDV)
abline(reg.sim.opt, col='red')

#### P1-2 résidus/valeurs aberrantes ####
residu.sim <- rstudent(reg.sim.opt)

# residus分布图
n <- length(mdds.sim.opt$MEDV)
plot(1:n, residu.sim, xlab = 'Index', ylab='Résidus studentisés', main = 'Valeurs aberrantes')
abline(-2, 0, lty = 2)
abline(2, 0, lty = 2)
IDval.ab <- (1:n)[abs(residu.sim)>2]
text(IDval.ab, residu.sim[IDval.ab], IDval.ab, pos = 4, col = 'blue')

# 标注异常值的数据图 !!!可与levier部分合并
plot(mdds.sim.opt$RM, mdds.sim.opt$MEDV, xlab = 'Nombre de chambres en moyenne', 
     ylab = 'Prix de maison (en 1000$)', main = 'Donnees avec valeurs aberrantes')
abline(coef(reg.sim.opt), col = 'green')
points(mdds.svq$RM[IDval.ab], mdds.sim.opt$MEDV[IDval.ab], col = 'blue', pch = 16) 
text(mdds.sim.opt$RM[IDval.ab], mdds.sim.opt$MEDV[IDval.ab], IDval.ab, pos = 4, col = 'blue')

# residus正态分布分析
quant.residu <- qt((1:n)/n,n-3)
plot(quant.residu, sort(residu.sim), xlab = 'Student T(n-p-1)',
     ylab = 'Quantiles empiriques des résidus', main = 'QQ-plot des résidus')
abline(0, 1, col ='blue')

# ???是什么？意义？
rsdstd.sim <- rstandard(reg.sim.opt) # rsdstd = résidu stantard
plot(1:n, reg.sim.opt$residuals, main = 'Residus', xlab = 'Index', ylab = 'Residus')
points(1:n, rsdstd.sim, pch = 4, col = 'blue')
points(1:n, residu.sim, pch = 5, col = 'red')
legend(0, 40, c('estime', 'standardise', 'studentise'),
       col = c('black', 'blue', 'red'), pch = c(1, 4, 5))

plot(1:n, rsdstd.sim, pch = 4, col = 'blue', main = 'Residus', xlab = 'Index',
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
plot(mdds.sim.opt$RM, mdds.sim.opt$MEDV, xlab = 'Nombre de chambres en moyenne', 
     ylab = 'Prix de maison (en 1000$)', main = 'Donnees avec valeurs aberrantes et points leviers')
abline(coef(reg.sim.opt), col = 'green')
points(mdds.sim.opt$RM[IDval.ab], mdds.sim.opt$MEDV[IDval.ab], col = 'blue', pch = 16) 
text(mdds.sim.opt$RM[IDval.ab], mdds.sim.opt$MEDV[IDval.ab], IDval.ab, pos = 4, col = 'blue')
points(mdds.sim.opt$RM[IDlev], mdds.sim.opt$MEDV[IDlev], col = 'red', pch = 16) 
text(mdds.sim.opt$RM[IDlev], mdds.sim.opt$MEDV[IDlev], IDlev, pos = 4, col = 'red')
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
#### P1-6 预测对比 ####
# 待完成

###### P2 第二部分 多变量线性回归 ######

# 建模
reg.mul.opt <- lm(MEDV ~ ., data = mdds.mul.opt)
summary(reg.mul.opt)

#### P2-1 线性回归图 ####

# 相关性图
cor(mdds.mul.opt)
pairs(mdds.mul.opt)

# 各变量的相关性图
par(mfrow=c(2,4))
coef.mul <- coef(reg.mul.opt)
plot(mdds.mul.opt$MEDV ~ mdds.mul.opt$CRIM)
abline(coef.mul[1], coef.mul[2])
plot(mdds.mul.opt$MEDV ~ mdds.mul.opt$ZN)
abline(coef.mul[1], coef.mul[3])
plot(mdds.mul.opt$MEDV ~ mdds.mul.opt$NOX)
abline(coef.mul[1], coef.mul[4])
plot(mdds.mul.opt$MEDV ~ mdds.mul.opt$RM)
abline(coef.mul[1], coef.mul[5])
plot(mdds.mul.opt$MEDV ~ mdds.mul.opt$DIS)
abline(coef.mul[1], coef.mul[6])
plot(mdds.mul.opt$MEDV ~ mdds.mul.opt$PTRATIO)
abline(coef.mul[1], coef.mul[7])
plot(mdds.mul.opt$MEDV ~ mdds.mul.opt$LSTAT)
abline(coef.mul[1], coef.mul[8])

par(mfrow=c(1,1))

#### P2-2 résidus/valeurs aberrantes ####

# ???是什么？意义？
par(mfrow=c(2,2))
plot(reg.mul.opt)

par(mfrow=c(1,1))

#### P2-3 Erreur quadratique moyenne ####

# 与不进行变量选择的模型的对比 （是否保留？？）
medv1 <- predict(reg.mul.svq, bd.svq.controle[,-12])
medv2 <- predict(reg.mul.opt, bd.svq.controle[,c(1,2,4,5,7,9,11,12)])

eqm1 <- 0.01*sum((medv1-bd.svq.controle$MEDV)^2)
eqm2 <- 0.01*sum((medv2-bd.svq.controle$MEDV)^2)

#### P2-4 预测对比 ####
# 待完成

###### P3 ######

# 对于variable qualitative有设置对照组的意义吗？？

#### P3-1 représentation des données ####
table(mdds.qua$CHAS)
table(mdds.qua$RAD)
boxplot(MEDV~CHAS, data = mdds.qua)
boxplot(MEDV~RAD, data = mdds.qua)

#### P3-2 estimation des paramètres pour CHAS ####
reg.chas <- lm(MEDV~CHAS, data = mdds.qua)
summary(reg.chas)
anova(reg.chas)

n <- nrow(mdds.qua)
residu.chas <- rstudent(reg.chas)
plot(1:n, residu.chas)

df.chas <- data.frame(index=1:n,rs = residu.chas)
df.chas <- data.frame(df.chas,CHAS=mdds.qua$CHAS)
head(df.chas)

par(mfrow = (c(1,2)))
plot(df.chas[df.chas$CHAS== 0, ]$index, df.chas[df.chas$CHAS == 0, ]$rs,main="CHAS = 0",xlab="Indice",ylab="Résidus studentisés")
plot(df.chas[df.chas$CHAS== 1, ]$index, df.chas[df.chas$CHAS == 1, ]$rs,main="CHAS = 1",xlab="Indice",ylab="Résidus studentisés")

par(mfrow = (c(1,1)))
boxplot(rs ~ CHAS, df.chas)

#### P3-3 estimation des paramètres pour RAD ####
reg.rad <- lm(MEDV~RAD, data = mdds.qua)
summary(reg.rad)
anova(reg.rad)

n <- nrow(mdds.qua)
residu.rad <- rstudent(reg.rad)
plot(1:n, residu.rad)

df.rad <- data.frame(index=1:n,rs = residu.rad)
df.rad <- data.frame(df.rad, RAD=mdds.qua$RAD)
head(df.rad)

par(mfrow = (c(3,3)))
plot(df.rad[df.rad$RAD== 1, ]$index, df.rad[df.rad$RAD == 1, ]$rs,main="RAD = 1",xlab="Indice",ylab="Résidus studentisés")
plot(df.rad[df.rad$RAD== 2, ]$index, df.rad[df.rad$RAD == 2, ]$rs,main="RAD = 2",xlab="Indice",ylab="Résidus studentisés")
plot(df.rad[df.rad$RAD== 3, ]$index, df.rad[df.rad$RAD == 3, ]$rs,main="RAD = 3",xlab="Indice",ylab="Résidus studentisés")
plot(df.rad[df.rad$RAD== 4, ]$index, df.rad[df.rad$RAD == 4, ]$rs,main="RAD = 4",xlab="Indice",ylab="Résidus studentisés")
plot(df.rad[df.rad$RAD== 5, ]$index, df.rad[df.rad$RAD == 5, ]$rs,main="RAD = 5",xlab="Indice",ylab="Résidus studentisés")
plot(df.rad[df.rad$RAD== 6, ]$index, df.rad[df.rad$RAD == 6, ]$rs,main="RAD = 6",xlab="Indice",ylab="Résidus studentisés")
plot(df.rad[df.rad$RAD== 7, ]$index, df.rad[df.rad$RAD == 7, ]$rs,main="RAD = 7",xlab="Indice",ylab="Résidus studentisés")
plot(df.rad[df.rad$RAD== 8, ]$index, df.rad[df.rad$RAD == 8, ]$rs,main="RAD = 8",xlab="Indice",ylab="Résidus studentisés")
plot(df.rad[df.rad$RAD== 24, ]$index, df.rad[df.rad$RAD == 24, ]$rs,main="RAD = 24",xlab="Indice",ylab="Résidus studentisés")

par(mfrow = (c(1,1)))
boxplot(rs ~ RAD, df.rad)

#### P3-4 Modèle à deux facteurs ####
boxplot(MEDV~CHAS*RAD, data = mdds.qua, xlab='CHAS/RAD')

with(mdds.qua, interaction.plot(CHAS, RAD, MEDV))
with(mdds.qua, interaction.plot(RAD, CHAS, MEDV))

# MEDV ~ CHAS*RAD 与 MEDV ~ CHAS+RAD 的意义分别是什么？？
cmb1 <- lm(MEDV ~ CHAS*RAD, data = mdds.qua) # cmb = combine
summary(cmb1)
cmb2 <- lm(MEDV ~ CHAS+RAD, data = mdds.qua)
summary(cmb2)
anova(cmb1)
anova(cmb2)

##### TEST SENSIBLE #####
sem <- lm(MEDV ~ B,  data = mdds.svq) #sim = simple, opt=optimal
summary(sem)
anova(sem)
cor(bd.tout)

plot(mdds.svq$B, mdds.svq$MEDV)
abline(sem, col='red')

residu.sem <- rstudent(sem)

n <- length(mdds.svq$MEDV)
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

bd.sem.nonlev <- mdds.svq[-c(19,33,35,103,135,146,147,154,157,166,168,368,385),]
sem.nonlev <- lm(MEDV ~ B, data = bd.sem.nonlev)
summary(sem.nonlev)

plot(bd.sem.nonlev$B, bd.sem.nonlev$MEDV)
abline(sem, col='red')

cor(bd.sem.nonlev)