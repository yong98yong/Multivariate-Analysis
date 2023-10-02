fat=read.csv("C:/Users/yong/Desktop/다변량해석/bodyfat.csv", header=TRUE, stringsAsFactors=TRUE)

#fat=이상치를 제거하지 않은 원자료, boxplot
par(mfrow=c(1,7))
for (i in 1:7){
	boxplot(fat[,i], xlab=names(fat)[i], cex=1.5, cex.lab=1.8)
}

par(mfrow=c(1,8))
for (i in 8:15){
	boxplot(fat[,i], xlab=names(fat)[i], cex=1.5, cex.lab=1.8)
}

#outlier row index = 31, 39, 41, 42, 86, 216 제거
fat_age=fat[-c(31,39,41,42,86,216),]

#age 제외
fat=fat_age[,-3]

#age를 factor로
for (i in c(20,30,40,50,60)){
	fat_age[fat_age$Age>=i & fat_age$Age<(i+10), 3]=paste(i, 's', sep='')
	if (i==60){
		fat_age[fat_age$Age>=i, 3]=paste('over', i, sep='')
	}
}
fat_age$Age=as.factor(fat_age$Age)
str(fat_age)

#histogram
par(mfrow=c(2,7))
for (i in 1:14){
	hist(fat[,i])
}

#Q-Q plot
par(mfrow=c(1,4))
par(pty='s')
for (i in 1:4){
	qqnorm(fat[,i], main=paste('Q-Q plot of ', names(fat)[i]))
	qqline(fat[,i], col='red')
}
par(mfrow=c(1,4))
par(pty='s')
for (i in 5:8){
	qqnorm(fat[,i], main=paste('Q-Q plot of ', names(fat)[i]))
	qqline(fat[,i], col='red')
}
par(mfrow=c(1,3))
par(pty='s')
for (i in 9:11){
	qqnorm(fat[,i], main=paste('Q-Q plot of ', names(fat)[i]))
	qqline(fat[,i], col='red')
}
par(mfrow=c(1,3))
par(pty='s')
for (i in 12:14){
	qqnorm(fat[,i], main=paste('Q-Q plot of ', names(fat)[i]))
	qqline(fat[,i], col='red')
}

#heatmap
library(corrplot)
corrplot(cor(fat), method='number') 

#중심화 자료행렬 Y, 표준화 자료행렬 Z
X=fat
Y=X
for (i in 1:14){
	Y[,i]=X[,i]-apply(X, 2, mean)[i]
}
Z=scale(X)

#PCA
Z_pca=princomp(Z)
summary(Z_pca)
Z_pca$loadings
par(mfrow=c(1,1))
screeplot(Z_pca, type='l')

#주성분들의 독립성 확인
PC1=summary(Z_pca)$scores[,1]
PC2=summary(Z_pca)$scores[,2]
PC3=summary(Z_pca)$scores[,3]

par(mfrow=c(1,3))
par(pty='s')
plot(PC1, PC2)
plot(PC1, PC3)
plot(PC2, PC3)


#MANOVA
fat_sc <- Z_pca$scores[,1:3]
fat_m_sc <- manova(fat_sc ~ fat_age$Age)
summary(fat_m_sc)


##### Second step analysis ########

#20s and 30s
fat_age23 <- fat_age[fat_age$Age=='20s'|fat_age$Age=='30s',]
fat23 <- fat_age23[,-3]

fat23.pca <- princomp(fat23,cor=T)
fat23.sc <- fat23.pca$scores[,1:3]
fat23.sc.m <- manova(fat23.sc ~ fat_age23$Age)
summary(fat23.sc.m,test='Wilks')
summary.aov(fat23.sc.m)


#20s and 40s
fat_age24 <- fat_age[fat_age$Age=='20s'|fat_age$Age=='40s',]
fat24 <- fat_age24[,-3]

fat24.pca <- princomp(fat24,cor=T)
fat24.sc <- fat24.pca$scores[,1:3]
fat24.sc.m <- manova(fat24.sc ~ fat_age24$Age)
summary(fat24.sc.m,test='Wilks')
summary.aov(fat24.sc.m)


#20s and 50s
fat_age25 <- fat_age[fat_age$Age=='20s'|fat_age$Age=='50s',]
fat25 <- fat_age25[,-3]

fat25.pca <- princomp(fat25,cor=T)
fat25.sc <- fat25.pca$scores[,1:3]
fat25.sc.m <- manova(fat25.sc ~ fat_age25$Age)
summary(fat25.sc.m,test='Wilks')
summary.aov(fat25.sc.m)


#20s and over00
fat_age26 <- fat_age[fat_age$Age=='20s'|fat_age$Age=='over60',]
fat26 <- fat_age26[,-3]

fat26.pca <- princomp(fat26,cor=T)
fat26.sc <- fat26.pca$scores[,1:3]
fat26.sc.m <- manova(fat26.sc ~ fat_age26$Age)
summary(fat26.sc.m,test='Wilks')
summary.aov(fat26.sc.m)


#30s and 40s
fat_age34 <- fat_age[fat_age$Age=='30s'|fat_age$Age=='40s',]
fat34 <- fat_age34[,-3]

fat34.pca <- princomp(fat34,cor=T)
fat34.sc <- fat34.pca$scores[,1:3]
fat34.sc.m <- manova(fat34.sc ~ fat_age34$Age)
summary(fat34.sc.m,test='Wilks')
summary.aov(fat34.sc.m)


#30s and 50s
fat_age35 <- fat_age[fat_age$Age=='30s'|fat_age$Age=='50s',]
fat35 <- fat_age35[,-3]

fat35.pca <- princomp(fat35,cor=T)
fat35.sc <- fat35.pca$scores[,1:3]
fat35.sc.m <- manova(fat35.sc ~ fat_age35$Age)
summary(fat35.sc.m,test='Wilks')
summary.aov(fat35.sc.m)


#30s and over60
fat_age36 <- fat_age[fat_age$Age=='30s'|fat_age$Age=='over60',]
fat36 <- fat_age36[,-3]

fat36.pca <- princomp(fat36,cor=T)
fat36.sc <- fat36.pca$scores[,1:3]
fat36.sc.m <- manova(fat36.sc ~ fat_age36$Age)
summary(fat36.sc.m,test='Wilks')
summary.aov(fat36.sc.m)


#40s and 50s
fat_age45 <- fat_age[fat_age$Age=='40s'|fat_age$Age=='50s',]
fat45 <- fat_age45[,-3]

fat45.pca <- princomp(fat45,cor=T)
fat45.sc <- fat45.pca$scores[,1:3]
fat45.sc.m <- manova(fat45.sc ~ fat_age45$Age)
summary(fat45.sc.m,test='Wilks')
summary.aov(fat45.sc.m)


#40s and over60
fat_age46 <- fat_age[fat_age$Age=='40s'|fat_age$Age=='over60',]
fat46 <- fat_age46[,-3]

fat46.pca <- princomp(fat46,cor=T)
fat46.sc <- fat46.pca$scores[,1:3]
fat46.sc.m <- manova(fat46.sc ~ fat_age46$Age)
summary(fat46.sc.m,test='Wilks')
summary.aov(fat46.sc.m)


#50s and over60
fat_age56 <- fat_age[fat_age$Age=='50s'|fat_age$Age=='over60',]
fat56 <- fat_age56[,-3]

fat56.pca <- princomp(fat56,cor=T)
fat56.sc <- fat56.pca$scores[,1:3]
fat56.sc.m <- manova(fat56.sc ~ fat_age56$Age)
summary(fat56.sc.m,test='Wilks')
summary.aov(fat56.sc.m)
