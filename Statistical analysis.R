source("functions.R")
#R packages
library("gplots")
library("survival")
library("mediation")
library("variancePartition")
library("reshape2")
library("ggpattern")
#load data
outcome.d<-read.table(file="outcome.txt",sep="\t",quot="",fill=T,head=T,row.names=1)###factor
meta.d.1<-read.table(file="metabolites.txt",sep="\t",quot="",fill=T,head=T,row.names=1)###log
clinic.d.1<-read.table(file="clinic.txt",sep="\t",quot="",fill=T,head=T,row.names=1)
meta.d<-log(meta.d.1[rownames(outcome.d),])
clinic.d<-clinic.d.1[rownames(outcome.d),]

###part1
#SC calculation
SCresult<-SC(clinic.d,meta.d)
SCmatrix<-SCresult[[1]]
pvalues<-SCresult[[2]]
write.csv(SCmatrix,"SCmatrix.csv")
write.csv(pvalues,"SCpmatrix.csv")
#correlation heatmap
pdf("SC.pdf")
heatmap.2(SCmatrix,col=bluered(75),key=TRUE, symkey=FALSE,trace="none",cexRow=0.8,margins=c(20,20))
dev.off()

###part2
#form
form1<-case~meta+age+as.factor(sex)+CP8+as.factor(CP9)+as.factor(CP10)+CP13+as.factor(CP11)+as.factor(CP12)+CP14+CP1+CP2+CP3+CP6+strata(match)
#OR calculation
ORresult<-OR(form1)
write.csv(ORresult,"OR.csv")
#set training dataset and validation set
split.d<-split_dataset(1,0.5)
t_ORresult<-OR(form1,split.d[[1]],split.d[[2]],split.d[[3]])
write.csv(t_ORresult,"train_OR.csv")
v_ORresult<-OR(form1,split.d[[4]],split.d[[5]],split.d[[6]])
write.csv(v_ORresult,"valid_OR.csv")
#select validation dataset randomly
random_ORresult<-random_OR(200,0.8)
write.csv(random_ORresult,"randomdataset_OR_200times_80.csv")

###Part3
#form:metabolites-clinical factors-disease
form.a<-clinic~meta #M=aX+e2
form.b<-case~meta+clinic #Y=c'X+bM+e3
form.c<-case~meta #Y=cX+e1
#mediation analysis
CMresult<-CM(form.a,form.b,form.c)
write.csv(CMresult,"causal mediation_seed01_boots1000.csv")

###Part4
#form
form1<-meta~age+sex+CP8+CP9+CP10+CP13+CP11+CP12+CP14+CP1+CP2+CP3+CP6
#VP calculation
VPresult<-VP(form1)
vars<-VPresult[[1]]
write.csv(vars,"VP_var.csv")
corr_type<-VPresult[[2]]
write.csv(corr_type,"VP_corrtype.csv")
#max VP
name_max=array(,c(nrow(vars),1))
var_max=array(,c(nrow(vars),1))
corr_type_max=array(,c(nrow(vars),1))
for(i in 1:nrow(vars)){
  k<-which.max(vars[i,])
  name_max[i,1]=colnames(vars)[k]
  var_max[i,1]=vars[i,k]
  corr_type_max[i,1]=corr_type[i,k]
}
rownames(name_max)<-rownames(var_max)<-rownames(corr_type_max)<-rownames(vars)
colnames(name_max)="name"
colnames(var_max)="var"
colnames(corr_type_max)="corr_type"
#plot
pdf("VPplot.pdf")
VPplot(var_max,corr_type_max)
dev.off()

