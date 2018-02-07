---
title: "R_project"
author: "RRRP-4"
date: "December 11, 2017"
output: word_document
---

library(foreign)
library(lavaan)


original<-read.spss("lsayshort.sav", to.data.frame=T)
data = data.frame(original$math7,original$math8,original$math9,original$math10)

data = na.omit(data)
cov_matrix = cov(data)

model.mean = c(88.3492575,93.9559875,105.37993,117.291)


model1 = 
'intercept =~ 1*original.math7 + 1*original.math8 +1*original.math9 + 1*original.math10
slope =~ 0*original.math7 + 1*original.math8 +2*original.math9 + 3*original.math10
intercept ~~ slope
intercept ~ original.math7
intercept ~ original.math8
intercept ~ original.math9
intercept ~ original.math10
slope ~ original.math7
slope ~ original.math8
slope ~ original.math9
slope ~ original.math10
original.math7 ~~ r*original.math8
original.math8 ~~ r*original.math9
original.math9 ~~ r*original.math10'

order = c("df","chisq","rmsea","cfi","srmr")

model1.fit = growth(model1,sample.cov = cov_matrix,sample.mean = model.mean, sample.nobs = 1994)
fitmeasures(model1.fit,fit.measures = order)
summary(model1.fit)
