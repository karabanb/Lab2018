
setwd("xxx")
load("KrukUWr2018.RData")
library(data.table)


#1 A glance at the data

summary(cases)
summary(events)

Cases <- data.table(cases)
Events <- data.table(events)



#  Decoding variables

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
                                    
for (variable in variables) {      ## variable = 'Age'
    if (eval(parse(text=paste("nullCounts$",variable,sep=""))) > 0) {
          avg <- eval(parse(text=paste("mean(Cases[,",variable,"],na.rm=TRUE)",sep="")))
          eval(parse(text=paste("Cases[is.na(",variable,"), ",variable,":=avg]",sep="")))
    }           
}



#  Other imputation

Cases[is.na(Female),Female:= ifelse(runif(1,0,1)<Cases[,mean(Female,na.rm=TRUE)],1,0)]
Cases[is.na(Bailiff),Bailiff:= ifelse(runif(1,0,1)<Cases[,mean(Bailiff,na.rm=TRUE)],1,0)]

Cases[is.na(ClosedExecution) & Bailiff==0, ClosedExecution:= 0]
Cases[is.na(ClosedExecution), ClosedExecution:= ifelse(runif(1,0,1)<Cases[,mean(ClosedExecution,na.rm=TRUE)],1,0)]



#  Proportion of tail data to be removed from the dataset

summary(Cases)

Proportion = 0.001

Cases <- Cases[LoanAmount<quantile(Cases[,LoanAmount], probs=1-Proportion),]
Cases <- Cases[DPD<quantile(Cases[,DPD], probs=1-Proportion),]
Cases <- Cases[LastPaymentAmount<quantile(Cases[,LastPaymentAmount], probs=1-Proportion),]



#  Correlation analysis

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

cor(Cases[,.SD,.SDcols = Variables], use="pairwise.complete.obs", method="pearson")
cor(Cases[,.SD,.SDcols = Variables], use="pairwise.complete.obs", method="spearman")


#2  Adding payment data from events

setkey(Cases,CaseId)
setkey(Events,CaseId)

Payments <- Events[Month <= 6,.(P6M = sum(ifelse(is.na(PaymentAmount),0,PaymentAmount)), Qty6M = sum(ifelse(is.na(PaymentAmount),0,1))),by=.(CaseId)]
setkey(Payments,CaseId)

Cases <- Cases[Payments[,.(CaseId,P6M,Qty6M)],nomatch=0][,Client := 'B']
Cases[P6M*1.0/TOA > 0.005 | Qty6M >= 3, Client := 'G']



#3 Correlation analysis

library(Hmisc)
rcorr(as.matrix(Cases[,.SD,.SDcols = Variables]), type="spearman")

library(corrgram)
corrgram(Cases[,.SD,.SDcols = Variables], lower.panel=panel.shade,
   upper.panel=panel.pie, text.panel=panel.txt,
   main="ApplicationData")


library(corrplot)
corrplot(cor(Cases[,.SD,.SDcols = Variables]), order = "hclust", tl.col='black', tl.cex=.75)


#4  VIF quick analysis
vif <- data.table()

for (i in 1:length(Variables)) {
model_lm <- lm(paste(Variables[i],paste(Variables[-i], collapse="+"),sep="~"), data = Cases)
         
         vif = rbind(vif,data.table(Variable = Variables[i], AdjR2 = summary(model_lm)$adj.r.squared, VIF = 1/(1-summary(model_lm)$adj.r.squared)))

}


#Rozgrzewka

library(MASS)
example1 <- mvrnorm(n = 1000, mu = c(0,0), Sigma = matrix(c(1,0.6,0.6,1),2,2), empirical = FALSE)
plot(example1, main="Direction in Data", xlab="x ", ylab="y", pch=19, asp=1)
pca <- prcomp(example1, center = TRUE, scale = TRUE)
eig <- eigen(cor(example1))

segments(-4*pca$rotation[1,1],-4*pca$rotation[2,1],4*pca$rotation[1,1],4*pca$rotation[2,1])
segments(-4*pca$rotation[1,2],-4*pca$rotation[2,2],4*pca$rotation[1,2],4*pca$rotation[2,2])



#5  Standardization of variables

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
                       "MeanSalary"
                       #"CreditCard",
                       #"Female",
                       #"Bailiff",
                       #"ClosedExecution"
                       )


CasesStd <- scale(Cases[,.SD,.SDcols = Variables])

summary(CasesStd)



#6   Principal Components

eigen(cor(CasesStd))
pca <- prcomp(CasesStd, center = FALSE, scale = FALSE)

summary(pca)


#7  Principal components

options(scipen=999)
CasesPCA <- data.table(CasesStd %*% pca$rotation)
CasesPCA[,Client := Cases[,Client]]

summary(CasesPCA)


#8  Variation explained graph

plot(pca$sdev^2/sum(pca$sdev^2))
barplot(pca$sdev^2/sum(pca$sdev^2), names.arg = 1:13, main="Variance explained", xlab="PC")


#9  Correlation between PCAs and Variables

CorMatrix <- pca$rotation %*% diag(pca$sdev)


#10  Graph - correlation of the original std variables with the PCs

library(plotrix)

plot(CorMatrix[,c(1,2)], xlim=c(-1.2,1.2), ylim=c(-1.2,1.2), xlab="PC1", ylab="PC2", asp=1)
draw.circle(0, 0, radius = 1)
abline(v=0)
abline(h=0)
text(CorMatrix[,c(1,2)], labels = Variables)


#11  Plot PC1-PC2 by groups

library(ggplot2)

qplot(PC3,PC4,data=CasesPCA, colour = Client, label=Client, xlab = "PC3", ylab = "PC4",size=I(1))


library(scatterplot3d)
library(rgl)
library(car)
scatter3d(x = CasesPCA[,PC3], y = CasesPCA[,PC4], z = CasesPCA[,PC2], groups = as.factor(CasesPCA[,Client]), surface=FALSE)


#12  Simple DA in the space of PCs

library(pROC)

CasesPCA[,Good := ifelse(Client=="G",1,0)]
pairs <- combn(1:7,2)
auc <- list()

for (i in (1:dim(pairs)[2])) {   # i=1

       n <- dim(CasesPCA)[1]
       index.learn <- sample(1:n, n*0.5)

       DaneTst <- CasesPCA[index.learn,]
       DaneTrn <- CasesPCA[-index.learn,]

       model <- glm(formula =
         paste0("Good ~PC",pairs[1,i],"+PC",pairs[2,i])
      , data = DaneTrn, family = binomial(link = "logit"))

      scores <- predict.glm(model, DaneTst, type = "response")
      r <- roc(DaneTst$Good, scores, direction="<")

      #plot(roc(DaneTst$Good, scores, direction="<"), col="yellow", lwd=3, main="ROC")
      auc[[i]] <- r$auc

}








