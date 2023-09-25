meal<-read.csv("MHO_meal.csv",header = T)

f<-lm(cooked_vegetable~factor(MHO)+age+sex+edu_level+eth_white+TDI+smoking+alcohol,meal)
 summary(f)
