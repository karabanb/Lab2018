
setwd("xxx")
load("KrukUWr2018.RData")



# A glance at the data

summary(cases)
summary(events)

Cases <- data.table(cases)
Events <- data.table(events)


Cases <- Cases[sample((1:dim(Cases)[1]), 0.3*dim(Cases)[1])]

#1  Decoding variables

Cases[,CreditCard := ifelse(Product=="Credit card",1,0)]
Cases[,Female := ifelse(Gender=="FEMALE",1,0)]



# Handling missing data

Variables = c(         "LoanAmount",
                       "TOA",
                       "Principal",
                       "Interest",
                       "Other",
                       "D_ContractDateToImportDate",
                       "DPD",
                       "PopulationInCity",
                       "Age",
                       "LastPaymentAmount",
                       "M_LastPaymentToImportDate",
                       "GDPPerCapita",
                       "MeanSalary",
                       "CreditCard",
                       "Female",
                       "Bailiff",
                       "ClosedExecution"
                       )

nullCounts <- lapply(Cases[,.SD,.SDcols=Variables], function(x) sum(is.na(x)))



# Imputation with avg

variables <- c(        "LoanAmount",
                       "TOA",
                       "Principal",
                       "Interest",
                       "Other",
                       "D_ContractDateToImportDate",
                       "DPD",
                       "PopulationInCity",
                       "Age",
                       "LastPaymentAmount",
                       "M_LastPaymentToImportDate",
                       "GDPPerCapita",
                       "MeanSalary"
                       )
                                    
for (variable in Variables) {      ## variable = 'Age'
    if (eval(parse(text=paste("nullCounts$",variable,sep=""))) > 0) {
          avg <- eval(parse(text=paste("mean(Cases[,",variable,"],na.rm=TRUE)",sep="")))
          eval(parse(text=paste("Cases[is.na(",variable,"), ",variable,":=avg]",sep="")))
    }           
}



# Other imputation

Cases[is.na(Female),Female:= ifelse(runif(1,0,1)<Cases[,mean(Female,na.rm=TRUE)],1,0)]
Cases[is.na(Bailiff),Bailiff:= ifelse(runif(1,0,1)<Cases[,mean(Bailiff,na.rm=TRUE)],1,0)]

Cases[is.na(ClosedExecution) & Bailiff==0, ClosedExecution:= 0]
Cases[is.na(ClosedExecution), ClosedExecution:= ifelse(runif(1,0,1)<Cases[,mean(ClosedExecution,na.rm=TRUE)],1,0)]



# Adding payment data from events

setkey(Cases,CaseId)
setkey(Events,CaseId)

Payments <- Events[Month <= 12,.(Payments12M = sum(ifelse(is.na(PaymentAmount),0,PaymentAmount))),by=.(CaseId)]
setkey(Payments,CaseId)

Cases <- Cases[Payments,nomatch=0][,SR12M := Payments12M*1.0/TOA]
Cases <- Cases[SR12M >= 0,] 

Cases[,Client := 'B'][Payments12M > 0, Client := 'G']



# Proportion of tail data to be removed from the dataset

summary(Cases)

Proportion = 0.001

Cases <- Cases[LoanAmount<quantile(Cases[,LoanAmount], probs=1-Proportion),]
Cases <- Cases[DPD<quantile(Cases[,DPD], probs=1-Proportion),]
Cases <- Cases[LastPaymentAmount<quantile(Cases[,LastPaymentAmount], probs=1-Proportion),]
Cases <- Cases[SR12M<quantile(Cases[,SR12M], probs=1-Proportion),]



# Correlation analysis

Variables = c(         "LoanAmount",
                       "TOA",
                       "Principal",
                       "Interest",
                       "Other",
                       "D_ContractDateToImportDate",
                       "DPD",
                       "PopulationInCity",
                       "Age",
                       "LastPaymentAmount",
                       "M_LastPaymentToImportDate",
                       "GDPPerCapita",
                       "MeanSalary",
                       "CreditCard",
                       "Female",
                       "Bailiff",
                       "ClosedExecution"
                       )


library(corrplot)
corrplot(cor(Cases[,.SD,.SDcols = Variables]), order = "hclust", tl.col='black', tl.cex=.75)



# Matrix of R2 for quadratic relationship

options(scipen=999)

R2matrix <- matrix(nrow=dim(Cases[,.SD,.SDcols = Variables])[2], ncol=dim(Cases[,.SD,.SDcols = Variables])[2])
colnames(R2matrix) <- Variables
rownames(R2matrix) <- Variables

for (i in 1:dim(Cases[,.SD,.SDcols = Variables])[2]) {
    for (j in 1:dim(Cases[,.SD,.SDcols = Variables])[2]) {                   
      if (i != j) {model_lm <- lm(paste(Variables[i],paste(Variables[j],"+",Variables[j],"^2"),sep="~"), data = Cases)
                   R2matrix[i,j] <- summary(model_lm)$adj.r.squared
      }
      if (i==j) R2matrix[i,j] <- 1
    }
}
R2matrix


# VIF quick analysis

vif <- data.table()
for (i in 1:dim(Cases[,.SD,.SDcols = Variables])[2]) {

         model_lm <- lm(paste(Variables[i],paste(Variables[-i], collapse="+"),sep="~"), data = Cases)
         vif <- rbind(vif ,data.table(Variable = Variables[i], AdjR2 = summary(model_lm)$adj.r.squared, VIF = 1/(1-summary(model_lm)$adj.r.squared)))}       

print(vif)




# 2.1 GAM modelling SR12M

library(gam)


         #  Training and test sets
      
n = dim(Cases)[1]                           
index.learn = sample(1:n, n/2)

CasesTrn = Cases[index.learn,]
CasesTst = Cases[-index.learn,]



         #  Model

model_SR12M <- gam(as.formula(paste("SR12M~",paste(Variables,collapse="+"))), family=gaussian(link=identity), data = CasesTrn)
                                       
summary(model_SR12M)  


         #  Stepwise selection

Variables_Discrete = c("CreditCard","Female","Bailiff","ClosedExecution")

Lista <- c()
for (i in 1:length(Variables)) {
                           
    if (sum(Variables[i] == Variables_Discrete) == 0) {
       Lista[[i]] = as.formula(paste("~1+", Variables[i], 
                  "+s(", Variables[i], ",2)", "+s(", 
                  Variables[i], ",3)", sep = ""))
                  }
    else {
                Lista[[i]] <- as.formula(paste("~1+", Variables[i], 
                  sep = ""))
    }
}

step.model_SR12M <-step.Gam(model_SR12M, scope=Lista, dir='both')

summary(step.model_SR12M)


         #  Final model

model_SR12M <- gam(
                   SR12M~
                          s(LoanAmount,3) +                    
                          s(TOA,3) +                            
                          s(Principal,3) +                       
                          s(D_ContractDateToImportDate,3) +     
                          s(DPD,3) +                             
                          PopulationInCity +                                                         
                          s(Age,3) +                             
                          s(LastPaymentAmount,3) +               
                          s(M_LastPaymentToImportDate,3) +       
                          GDPPerCapita +                                                             
                          MeanSalary +                                                               
                          Female                              
, family=gaussian(link=identity), data = CasesTrn)

summary(model_SR12M)


         #  Partial prediction plots

plot.Gam(model_SR12M,ask=TRUE)


         #  Payments 12M Forecast

sum(predict.Gam(model_SR12M, newdata=CasesTst, type='response')*CasesTst[,TOA])
sum(CasesTst[,Payments12M])

plot(predict.Gam(model_SR12M, newdata=CasesTst, type='response'),CasesTst[,SR12M], xlab="Forecasts", ylab="Actuals", ylim=c(0,1.5), xlim=c(0,1.5))



# 2.2 GAM modelling SR12M  - binary response variable



Cases[,Good := ifelse(Client == 'B',0,1)]

         #  Training and test sets

n = dim(Cases)[1]
index.learn = sample(1:n, n/2)


CasesTrn = Cases[index.learn,]
CasesTst = Cases[-index.learn,]




         #  Model

model_SR12M <- gam(as.formula(paste("Good~",paste(Variables,collapse="+"))), family=binomial(link = "logit"), data = CasesTrn)

summary(model_SR12M)


         #  Stepwise selection

Variables_Discrete = c("CreditCard","Female","Bailiff","ClosedExecution")

Lista <- c()
for (i in 1:length(Variables)) {

    if (sum(Variables[i] == Variables_Discrete) == 0) {
       Lista[[i]] = as.formula(paste("~1+", Variables[i],
                  "+s(", Variables[i], ",2)", "+s(",
                  Variables[i], ",3)", sep = ""))
                  }
    else {
                Lista[[i]] <- as.formula(paste("~1+", Variables[i],
                  sep = ""))
    }
}

step.model_SR12M <-step.gam(model_SR12M, scope=Lista, dir='both')

summary(step.model_SR12M)


         #  Final model

model_SR12M <- gam(
                   Good ~
                        LoanAmount +
                        Interest +
                        s(Other, 3) +
                        s(D_ContractDateToImportDate, 3) +
                        s(DPD, 3) +
                        s(Age, 3) +
                        s(LastPaymentAmount, 3) +
                        s(M_LastPaymentToImportDate, 3) +
                        s(GDPPerCapita, 3) +
                        s(MeanSalary, 3) +
                        CreditCard +
                        Female
, family=binomial(link = "logit"), data = CasesTrn)

summary(model_SR12M)


         #  Partial prediction plots

plot.Gam(model_SR12M,ask=TRUE)


         #  Payments 12M Forecast

CasesTrn[,Forecast := predict.Gam(model_SR12M, newdata=CasesTrn, type='response')]
CasesTst[,Forecast := predict.Gam(model_SR12M, newdata=CasesTst, type='response')]
Cases[,Forecast := predict.Gam(model_SR12M, newdata=Cases, type='response')]

Cases[,Band := cut(Forecast, breaks = 5)]
CasesTrn[,Band := Cases[index.learn,Band]]
CasesTst[,Band := Cases[-index.learn,Band]]

Payments_Group <- tapply(CasesTrn[,Payments12M],CasesTrn[,Band],sum)
TOA_Group <- tapply(CasesTrn[,TOA],CasesTrn[,Band],sum)
SR_Group <- Payments_Group/TOA_Group

TOA_Group_Tst <- tapply(CasesTst[,TOA],CasesTst[,Band],sum)

sum(SR_Group*TOA_Group_Tst)
sum(CasesTst[,Payments12M])


         #  Gini

scores <- predict.Gam(model_SR12M, CasesTst, type = "response")

library(pROC)
plot(roc(CasesTst[,Good], scores, direction="<"), col="yellow", lwd=3, main="ROC_BinaryModel")


         #  Concurvity

library(mgcv)

model_SR12M <- gam(
                   Good ~
                        LoanAmount +
                        Interest +
                        s(Other) +
                        s(D_ContractDateToImportDate) +
                        s(DPD) +
                        s(Age) +
                        s(LastPaymentAmount) +
                        s(M_LastPaymentToImportDate) +
                        s(GDPPerCapita) +
                        s(MeanSalary) +
                        CreditCard +
                        Female
, family=binomial(link = "logit"), data = CasesTrn)


summary(model_SR12M)
plot(model_SR12M)

concurvity(model_SR12M,full=TRUE)
concurvity(model_SR12M,full=FALSE)



vis.concurvity(model_SR12M)



vis.concurvity <- function(b, type="estimate"){
   cc <- concurvity(b, full=FALSE)[[type]]

   diag(cc) <- NA
   cc[lower.tri(cc)]<-NA

   layout(matrix(1:2, ncol=2), widths=c(5,1))
   opar <- par(mar=c(5, 6, 5, 0) + 0.1)
   # main plot
   image(z=cc, x=1:ncol(cc), y=1:nrow(cc), ylab="", xlab="",
         axes=FALSE, asp=1, zlim=c(0,1))
   axis(1, at=1:ncol(cc), labels = colnames(cc), las=2)
   axis(2, at=1:nrow(cc), labels = rownames(cc), las=2)
   # legend
   opar <- par(mar=c(5, 0, 4, 3) + 0.1)
   image(t(matrix(rep(seq(0, 1, len=100), 2), ncol=2)),
         x=1:3, y=1:101, zlim=c(0,1), axes=FALSE, xlab="", ylab="")
   axis(4, at=seq(1,101,len=5), labels = round(seq(0,1,len=5),1), las=2)
   par(opar)
}


