#----------------- Hierarchical Clustering --------------------
#table - used table
#k - array with non-numeric variables
#tableT - test table

Cluster = function(table,k,name)
{
tryCatch(
{
	if(!"package:MASS"%in%search()){
		library(MASS)
	}
	for(var in k){
		table[,var] = as.numeric(as.factor(table[,var]))
		tableT[,var] = as.numeric(as.factor(tableT[,var]))
	}

#Calculating distances
	d = dist(as.matrix(table))
	write.csv(as.matrix(d),paste("Hclust",name,"Dist.csv",sep="_"))
	clust = hclust(dist(table))
	labels=rownames(table)
	plot(clust,labels=rownames(table))

#Evolution of the classification
	index=clust$merge
	m=nrow(index)
	H=character(m)
	for(i in 1:m){
		stanga=""
		if(index[i,1]<0){
			stanga=labels[-index[i,1]]
		} else {
			stanga=H[index[i,1]]
		}
		dreapta=""
		if(index[i,2]<0){
			dreapta=labels[-index[i,2]]
		} else {
			dreapta=H[index[i,2]]
		}		
		H[i]=paste("{",stanga,",",dreapta,"}",sep="")
	}
	frameIerarhie=data.frame(paste("Cluster",1:m),clust$height,H)
	colnames(frameIerarhie)=c("","Distanta","Structura cluster")
	write.csv(frameIerarhie,paste("Hclust",name,"Hierarchy.csv",sep="_"),row.names=FALSE)
},
error=function(ex){
	print(ex)
}
)
}


#----------------- Bayesian Discriminant --------------------
#table - used table
#k - array with non-numeric variables
#dVar - discriminant variable
#tableT - test table

BayesK = function(table,k,dVar,tableT,name)
{
tryCatch(
{
	if(!"package:MASS"%in%search()){
		library(MASS)
	}
	if(!"package:klaR"%in%search()){
		library(klaR)
	}
	for(var in k){
		table[,var] = as.numeric(as.factor(table[,var]))
		tableT[,var] = as.numeric(as.factor(tableT[,var]))
	}
	numeG=levels(dVar)
	p=ncol(table);q=length(numeG);n=nrow(table)
#Testarea modelului prin manova
	manovaT = manova(as.matrix(table)~dVar)
	s = summary(manovaT,test="Wilks")
	sFrame = data.frame(
		c("Wilks","F","P-Value"),
		c(s$stats[1,"Wilks"],s$stats[1,"approx F"],s$stats[1,"Pr(>F)"])
	)
	colnames(sFrame)=c("Indicatori","Valori")
	write.csv(sFrame,paste("NBayes",name,"TestModel.csv",sep="_"),row.names=FALSE)
#Rulare model
	nbrez = NaiveBayes(table,dVar)
	classrez=predict(nbrez,table)
	class=classrez$class
	setClassif1=data.frame(rownames(table),class)
	colnames(setClassif1)=c("Instanta","Grupa")
	write.csv(setClassif1,paste("NBayes",name,"ClassifB.csv",sep="_"),row.names=FALSE)
	write.csv(round(classrez$posterior),paste("NBayes",name,"APosterioriP.csv",sep="_"))
	confMatrix = table(Original=dVar,Predicted=factor(class))
	gAcc = array(0,c(q,q+1))
	sum=0
	for(i in 1:q){
		for(j in 1:q){
			gAcc[i,j]=confMatrix[i,j]
		}
		gAcc[i,q+1]=confMatrix[i,i]*100/sum(confMatrix[i,])
		sum=sum+confMatrix[i,i]
	}
	rownames(gAcc)=numeG
	colnames(gAcc)=c(numeG,"Acuratete grupe")
	write.csv(gAcc,paste("NBayes",name,"ConfusionMatrix.csv",sep="_"))
	classTrez=predict(nbrez,tableT)
	classT=classTrez$class
	setClassif2=data.frame(rownames(tableT),classT)
	colnames(setClassif2)=c("Instanta","Grupa")
	write.csv(setClassif2,paste("NBayes",name,"ClassifT.csv",sep="_"),row.names=FALSE)	
},
error=function(ex){
	print(ex)
}
)
}

BayesE = function(table,k,dVar,tableT,name)
{
tryCatch(
{
	if(!"package:MASS"%in%search()){
		library(MASS)
	}
	if(!"package:e1071"%in%search()){
		library(e1071)
	}
	for(var in k){
		table[,var] = as.numeric(as.factor(table[,var]))
		tableT[,var] = as.numeric(as.factor(tableT[,var]))
	}
	numeG=levels(dVar)
	p=ncol(table);q=length(numeG);n=nrow(table)
#Testing model using MANOVA
	manovaT = manova(as.matrix(table)~dVar)
	s = summary(manovaT,test="Wilks")
	sFrame = data.frame(
		c("Wilks","F","P-Value"),
		c(s$stats[1,"Wilks"],s$stats[1,"approx F"],s$stats[1,"Pr(>F)"])
	)
	colnames(sFrame)=c("Indicatori","Valori")
	write.csv(sFrame,paste("NBayesE",name,"TestModel.csv",sep="_"),row.names=FALSE)
#Apply on model
	nbrez = naiveBayes(table,dVar)
	class=predict(nbrez,table)
	setClassif1=data.frame(rownames(table),class)
	colnames(setClassif1)=c("Instanta","Grupa")
	write.csv(setClassif1,paste("NBayesE",name,"ClassifB.csv",sep="_"),row.names=FALSE)
	confMatrix = table(Original=dVar,Predicted=factor(class))
	gAcc = array(0,c(q,q+1))
	sum=0
	for(i in 1:q){
		for(j in 1:q){
			gAcc[i,j]=confMatrix[i,j]
		}
		gAcc[i,q+1]=confMatrix[i,i]*100/sum(confMatrix[i,])
		sum=sum+confMatrix[i,i]
	}
	rownames(gAcc)=numeG
	colnames(gAcc)=c(numeG,"Acuratete grupe")
	write.csv(gAcc,paste("NBayesE",name,"ConfusionMatrix.csv",sep="_"))
	classT=predict(nbrez,tableT)
	setClassif2=data.frame(rownames(tableT),classT)
	colnames(setClassif2)=c("Instanta","Grupa")
	write.csv(setClassif2,paste("NBayesE",name,"ClassifT.csv",sep="_"),row.names=FALSE)	
},
error=function(ex){
	print(ex)
}
)
}






