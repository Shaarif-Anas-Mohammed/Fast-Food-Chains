library(car)
library(tidyverse)
library(factoextra)

fastfoods = read.csv(file.choose(),header = TRUE)

## MULTIVARIATE ANALYSIS OF VARIANCE //**

model1 <- lm(cbind(calories,cal_fat,total_fat,sat_fat,trans_fat,cholesterol,sodium,total_carb,fiber,sugar,protein,vit_a,vit_c,calcium)~factor(restaurant),data=fastfoods)
fit1.manova <- Manova(model1)
summary(fit1.manova)

##PRINCIPAL COMPONENT ANALYSIS//**

pca.fastfoods <-prcomp(fastfoods[,3:16],scale=TRUE)
summary(pca.fastfoods)

## Eigen values of the correlation matrix //**

R <- cor(fastfoods[,3:16])
lambda <- eigen(R)$values
lambda


##Scree Plot //**

ggplot(NULL,aes(x=1:14,y=lambda))+geom_point()+geom_line()+
     scale_x_continuous(breaks=1:14)+
     xlab("Principal Component")+ylab(expression("Eigenvalue"~lambda))

## Correlation between each of the 4 Principal components and variables **//

PC1 <- pca.fastfoods$rotation[,1]
corr.z1.y <- PC1*sqrt(lambda[1])
corr.z1.y

PC2 <- pca.fastfoods$rotation[,2]
corr.z2.y <- PC2*sqrt(lambda[2])
corr.z2.y

PC3 <- pca.fastfoods$rotation[,3]
corr.z3.y <- PC3*sqrt(lambda[3])
corr.z3.y

PC4 <- pca.fastfoods$rotation[,4]
corr.z4.y <- PC4*sqrt(lambda[4])
corr.z4.y

## making a new dataset with principal components replacing the variables //**

PCs = pca.fastfoods$x[,1:4]
food_item = fastfoods[,1:2]
Pc.data = cbind(food_item,PCs)
head(Pc.data)

## CLUSTER ANALYSIS //**
  ##Deciding number of clusters //**
 
fviz_nbclust(scale(Pc.data[,3:6],kmeans,method="wss"))
               
## cluster analysis //**

km.fastfood <- kmeans(Pc.data[,3:6],5,nstart=25)
cluster = km.fastfood$cluster
cluster.data = cbind(fastfoods,cluster)
head(cluster.data)
               