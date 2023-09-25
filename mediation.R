##Blood
BLOOD<-read.csv("BLOOD.csv")
BLOOD<-na.omit(BLOOD)
w<-which(is.infinite(BLOOD$LMR))
BLOOD<-BLOOD[-w,]
eid<-BLOOD$eid
BLOOD_new<-apply(BLOOD[,-1],2,scale)
BLOOD<-cbind(eid,BLOOD_new)
BLOOD<-as.data.frame(BLOOD)
##MHO
MHO_S<-read.csv("MHO_survival.csv")
##BC
BC_S<-read.csv("BC_survival.csv")
##merge
merge_M<-merge(BLOOD,MHO_S,by="eid")
merge_B<-merge(BLOOD,BC_S,by="eid")

#------Brain-disorders---
library("survival")
library("mediation")

###BC vs. dementia
merge_B_dementia<-merge_B[!is.na(merge_B$dementia_status),]
fit.dv<-glm(dementia_status~CRP+BC_ra+age+sex+edu_level+eth_white+TDI+smoking+alcohol+TPA+TV+fruit+vegetable+oilyfish+processed_meat+red_meat,family=binomial(link=logit),merge_B_dementia)
M2<-coef(summary(fit.dv))[2,]
fit.mediator<-lm(CRP~BC_ra+age+sex+edu_level+eth_white+TDI+smoking+alcohol+TPA+TV+fruit+vegetable+oilyfish+processed_meat+red_meat,merge_B_dementia)
M1<-coef(summary(fit.mediator))[2,]
results<-mediate(fit.mediator,fit.dv,treat="BC_ra",mediator="CRP",sims=1000)
SUM<-summary(results)
ACME<-c(SUM$d.avg,SUM$d.avg.ci,SUM$d.avg.p)
ADE<-c(SUM$z.avg,SUM$z.avg.ci,SUM$z.avg.p)
Prop<-c(SUM$n.avg,SUM$n.avg.ci,SUM$n.avg.p)
Total<-c(SUM$tau.coef,SUM$tau.ci,SUM$tau.p)
res<-rbind(ACME,ADE,Prop,Total)
res
results<-rbind(M1,M2,res)
colnames(results)<-c("Estimate","SE/95L","Z/95U","P")
write.csv(results,paste(outpath,"CRP_BC_dementia.csv",sep=''),row.names=T,quote=F)

###BC vs. anxiety
merge_B_anxiety<-merge_B[!is.na(merge_B$anxiety_status),]
fit.dv<-glm(anxiety_status~CRP+BC_ra+age+sex+edu_level+eth_white+TDI+smoking+alcohol+TPA+TV+fruit+vegetable+oilyfish+processed_meat+red_meat,family=binomial(link=logit),merge_B_anxiety)
M2<-coef(summary(fit.dv))[2,]
fit.mediator<-lm(CRP~BC_ra+age+sex+edu_level+eth_white+TDI+smoking+alcohol+TPA+TV+fruit+vegetable+oilyfish+processed_meat+red_meat,merge_B_anxiety)
M1<-coef(summary(fit.mediator))[2,]
results<-mediate(fit.mediator,fit.dv,treat="BC_ra",mediator="CRP",sims=1000)
SUM<-summary(results)
ACME<-c(SUM$d.avg,SUM$d.avg.ci,SUM$d.avg.p)
ADE<-c(SUM$z.avg,SUM$z.avg.ci,SUM$z.avg.p)
Prop<-c(SUM$n.avg,SUM$n.avg.ci,SUM$n.avg.p)
Total<-c(SUM$tau.coef,SUM$tau.ci,SUM$tau.p)
res<-rbind(ACME,ADE,Prop,Total)
res
results<-rbind(M1,M2,res)
colnames(results)<-c("Estimate","SE/95L","Z/95U","P")
write.csv(results,paste(outpath,"CRP_BC_anxiety.csv",sep=''),row.names=T,quote=F)

###BC vs. sleep disorder
merge_B_sleepdisorder<-merge_B[!is.na(merge_B$sleepdisorder_status),]
fit.dv<-glm(sleepdisorder_status~Monocytecount+BC_ra+age+sex+edu_level+eth_white+TDI+smoking+alcohol+TPA+TV+fruit+vegetable+oilyfish+processed_meat+red_meat,family=binomial(link=logit),merge_B_sleepdisorder)
M2<-coef(summary(fit.dv))[2,]
fit.mediator<-lm(Monocytecount~BC_ra+age+sex+edu_level+eth_white+TDI+smoking+alcohol+TPA+TV+fruit+vegetable+oilyfish+processed_meat+red_meat,merge_B_sleepdisorder)
M1<-coef(summary(fit.mediator))[2,]
results<-mediate(fit.mediator,fit.dv,treat="BC_ra",mediator="Monocytecount",sims=1000)
SUM<-summary(results)
ACME<-c(SUM$d.avg,SUM$d.avg.ci,SUM$d.avg.p)
ADE<-c(SUM$z.avg,SUM$z.avg.ci,SUM$z.avg.p)
Prop<-c(SUM$n.avg,SUM$n.avg.ci,SUM$n.avg.p)
Total<-c(SUM$tau.coef,SUM$tau.ci,SUM$tau.p)
res<-rbind(ACME,ADE,Prop,Total)
res
results<-rbind(M1,M2,res)
colnames(results)<-c("Estimate","SE/95L","Z/95U","P")
write.csv(results,paste(outpath,"Monocytecount_BC_sleepdisorder.csv",sep=''),row.names=T,quote=F)