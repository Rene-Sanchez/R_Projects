library(foreign)
library(lavaan)

library(semPlot)

pdr2<-read.spss("C:\\Users\\Sylvia\\Downloads\\lsaylong.sav", to.data.frame=T)
data.frame(pdr2$mth7,pdr2$mth8,pdr2$mth9,pdr2$mth10)

cov_matrix = cov(data)

cov_matrix = cov(data,use='pairwise')

 model.cov = lav_matrix_lower2full(c(
  +     104.01586,			
  +     93.52333,	121.50807,		
  +     102.97885,	120.32902,	158.871,	
  +     109.15183,	123.613, 154.5343,  186.2955))

model.mean = c(102.4174675,121.8166967,156.70265,186.2955)

names(model.mean) = rownames(model.cov) = colnames(model.cov) = c("t1","t2","t3","t4")

model1 = 
'intercept =~ 1*t1 + 1*t2 +1*t3 + 1*t4
slope =~ 0*t1 + 1*t2 +2*t3 + 3*t4
t1 ~~ r*t1
t2 ~~ r*t2
t3 ~~ r*t3
t4 ~~ r*t4'

order = c("df","chisq","rmsea","cfi","srmr")

model1.fit = growth(model1,sample.cov = model.cov,sample.mean = model.mean, sample.nobs = 3116)
fitmeasures(model1.fit,fit.measures = order)
summary(model1.fit)

fit1 <- growth(model1, data=data)

