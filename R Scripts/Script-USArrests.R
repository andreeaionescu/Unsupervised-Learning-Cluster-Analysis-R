source("Classif.R")

#Testing model using ANOVA
setDate = read.table("USArrests.csv",header=T,sep=",")
n = ncol(setDate)
k=list()

names(setDate)
tabel.manova1 <- manova( cbind(State, Murder, Assault, Rape) ~ as.factor(UrbanPop), data=setDate)
summary(tabel.manova1)
s = summary(manovaT,test="Wilks")
	sFrame = data.frame(
		c("Wilks","F","P-Value"),
		c(s$stats[1,"Wilks"],s$stats[1,"approx F"],s$stats[1,"Pr(>F)"])
	)
	colnames(sFrame)=c("Indicatori","Valori")
	write.csv(sFrame,paste("USArrests","TestModel.csv",sep="_"),row.names=FALSE)


Cluster(setLda[1:(n-1)],k,"USArrests")

