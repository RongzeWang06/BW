library(survival)
library(survminer)
library(rms)
library(ggplot2)
####-- Stroke/PD/Anxiety/Depression/Sleep disorders --####
#- BMI non-linear -#
dd <- datadist(data1)
options(datadist="dd")
fit <- cph(Surv(years,status)~rcs(BMI,4)+age+sex+edu_level+eth_white+TDI+smoking+alcohol+TPA+TV+fruit+vegetable+oilyfish+meat_processed+red_meat,data = data1)
dd$limits$BMI[2] <- 30
fit <- update(fit)
anova(fit)
HR <- Predict(fit,BMI,fun=exp,ref.zero = TRUE)
P1 <- ggplot()+geom_hline(yintercept = 1,linetype=2,size=0.5)+geom_line(data = HR,aes(BMI,yhat),linetype="solid",size=1,alpha=0.7,colour="purple")+geom_ribbon(data = HR,aes(BMI,ymin=lower,ymax=upper),alpha=0.1,fill="purple")+geom_vline(xintercept = 18.5,linetype=2,size=0.5,colour="purple")+geom_vline(xintercept = 25,linetype=2,size=0.5,colour="purple")+geom_vline(xintercept = 30,linetype=2,size=0.5,colour="purple")+theme_classic()
P1
#- BMI categories -#
cox <- coxph(Surv(years,status)~BMI_cat+age+sex+edu_level+eth_white+TDI+smoking+alcohol+TPA+TV+fruit+vegetable+oilyfish+meat_processed+red_meat,data = data1)
summary(cox)
cox.zph(cox)
#- BMI change -#
cox <- coxph(Surv(years,status)~BC_ra+X21001.0.0+age+sex+edu_level+eth_white+TDI+smoking+alcohol+TPA+TV+fruit+vegetable+oilyfish+meat_processed+red_meat,data = data2)
cox <- coxph(Surv(years,status)~BC_ra_level+X21001.0.0+age+sex+edu_level+eth_white+TDI+smoking+alcohol+TPA+TV+fruit+vegetable+oilyfish+meat_processed+red_meat,data = data2)
cox <- coxph(Surv(years,status)~BC+age+sex+edu_level+eth_white+TDI+smoking+alcohol+TPA+TV+fruit+vegetable+oilyfish+meat_processed+red_meat,data = data2)
summary(cox)
cox.zph(cox)
#- BMI-metabolic health status -#
cox <- coxph(Surv(years,status)~MHO_3+age+sex+edu_level+eth_white+TDI+smoking+alcohol+TPA+TV+fruit+vegetable+oilyfish+meat_processed+red_meat,data = data3)
summary(cox)
cox.zph(cox)


####-- Dementia --####
#- BMI non-linear -#
dd <- datadist(data1)
options(datadist="dd")
fit <- cph(Surv(years,status)~rcs(BMI,4)+age+sex+edu_level+APOE4+eth_white+TDI+smoking+alcohol+TPA+TV+fruit+vegetable+oilyfish+meat_processed+red_meat,data = data1)
dd$limits$BMI[2] <- 30
fit <- update(fit)
anova(fit)
HR <- Predict(fit,BMI,fun=exp,ref.zero = TRUE)
P1 <- ggplot()+geom_hline(yintercept = 1,linetype=2,size=0.5)+geom_line(data = HR,aes(BMI,yhat),linetype="solid",size=1,alpha=0.7,colour="purple")+geom_ribbon(data = HR,aes(BMI,ymin=lower,ymax=upper),alpha=0.1,fill="purple")+geom_vline(xintercept = 18.5,linetype=2,size=0.5,colour="purple")+geom_vline(xintercept = 25,linetype=2,size=0.5,colour="purple")+geom_vline(xintercept = 30,linetype=2,size=0.5,colour="purple")+theme_classic()
P1
#- BMI categories -#
cox <- coxph(Surv(years,status)~BMI_cat+age+sex+edu_level+APOE4+eth_white+TDI+smoking+alcohol+TPA+TV+fruit+vegetable+oilyfish+meat_processed+red_meat,data = data1)
summary(cox)
cox.zph(cox)
#- BMI change -#
cox <- coxph(Surv(years,status)~BC_ra+X21001.0.0+age+sex+edu_level+APOE4+eth_white+TDI+smoking+alcohol+TPA+TV+fruit+vegetable+oilyfish+meat_processed+red_meat,data = data2)
cox <- coxph(Surv(years,status)~BC_ra_level+X21001.0.0+age+sex+edu_level+APOE4+eth_white+TDI+smoking+alcohol+TPA+TV+fruit+vegetable+oilyfish+meat_processed+red_meat,data = data2)
cox <- coxph(Surv(years,status)~BC+age+sex+edu_level+APOE4+eth_white+TDI+smoking+alcohol+TPA+TV+fruit+vegetable+oilyfish+meat_processed+red_meat,data = data2)
summary(cox)
cox.zph(cox)
#- BMI-metabolic health status -#
cox <- coxph(Surv(years,status)~MHO_3+age+sex+edu_level+APOE4+eth_white+TDI+smoking+alcohol+TPA+TV+fruit+vegetable+oilyfish+meat_processed+red_meat,data = data3)
summary(cox)
cox.zph(cox)