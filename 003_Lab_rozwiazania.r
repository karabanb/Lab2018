
setwd("")
load("KrukUWr2018.RData")



#1 A glance at the data

summary(cases)
summary(events)

Cases <- data.table(cases)
Events <- data.table(events)



#2  Decoding variables

Cases[,CreditCard := ifelse(Product=="Credit card",1,0)]
Cases[,Female := ifelse(Gender=="FEMALE",1,0)]



#3 Handling missing data

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


#3 Imputation with avg

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



# Other imputation

Cases[is.na(Female),Female:= ifelse(runif(1,0,1)<Cases[,mean(Female,na.rm=TRUE)],1,0)]
Cases[is.na(Bailiff),Bailiff:= ifelse(runif(1,0,1)<Cases[,mean(Bailiff,na.rm=TRUE)],1,0)]

Cases[is.na(ClosedExecution) & Bailiff==0, ClosedExecution:= 0]
Cases[is.na(ClosedExecution), ClosedExecution:= ifelse(runif(1,0,1)<Cases[,mean(ClosedExecution,na.rm=TRUE)],1,0)]



#4  Proportion of tail data to be removed from the dataset

summary(Cases)

Proportion = 0.001

Cases <- Cases[LoanAmount<quantile(Cases[,LoanAmount], probs=1-Proportion),]
Cases <- Cases[DPD<quantile(Cases[,DPD], probs=1-Proportion),]
Cases <- Cases[LastPaymentAmount<quantile(Cases[,LastPaymentAmount], probs=1-Proportion),]



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


CasesStd <- data.table(cbind(CaseId=Cases[,CaseId],scale(Cases[,.SD,.SDcols = Variables])))

summary(CasesStd)



#6   Adding payment data from events

setkey(CasesStd,CaseId)
setkey(Events,CaseId)

Payments <- Events[Month <= 6,.(Payments6M = sum(ifelse(is.na(PaymentAmount),0,PaymentAmount))),by=.(CaseId)]
setkey(Payments,CaseId)

CasesStd <- CasesStd[Payments[,.(CaseId,Payments6M)],nomatch=0][,Client := 'B']
CasesStd[Payments6M > 0, Client := 'G']


# Proportion of goods
G_prior <- CasesStd[Client == "G", .N]/CasesStd[, .N]


#7   Cluster analysis - Good vs Bad Clients                     

summary(CasesStd)

Variables <- c("TOA","M_LastPaymentToImportDate")

k <- 5
kCluster <- kmeans(x=CasesStd[, .SD, .SDcols=Variables],
  centers=k, iter.max=100, nstart=10)

# Assigning clusters
CasesStd[, kClass:=kCluster$cluster]


# Graph 
CasesStd[, colClass:=rainbow(k)[kCluster$cluster]]

plot(CasesStd[Client == "G", ][["TOA"]],
  CasesStd[Client == "G", ][["M_LastPaymentToImportDate"]],
  pch=1, col=CasesStd[Client == "G", ]$colClass, cex=0.7,
  xlab="", ylab="", main="",
  xlim = 1.05*range(CasesStd[["TOA"]]),
  ylim = 1.05*range(CasesStd[["M_LastPaymentToImportDate"]]))
points(CasesStd[Client == "B", ][["TOA"]],
  CasesStd[Client == "B", ][["M_LastPaymentToImportDate"]],
  pch=6, col=CasesStd[Client == "B", ]$colClass, cex=0.7)
points(kCluster$centers, pch=18, cex=2)


#8 Classification matrix
#             | Predicted Good | Predicted Bad  |
# Actual Good |      TP        |       FN       | = P
# Actual Bad  |      FP        |       TN       | = N
#
# sensitivity = true positive rate = TP/P = TP/(TP + FN)
# specificity = true negative rate = TN/N = TN/(FP + TN)

tmp <- CasesStd[, .(.N,
  B_count=sum(ifelse(Client == "B", 1,0)),
  G_count=sum(ifelse(Client == "G", 1,0)),
  G_percent=sum(ifelse(Client == "G", 1,0))/.N), by=.(kClass)][order(kClass)]



# Forecast od Payment6M
tmp[, G_predict:=ifelse(G_percent > G_prior, 1, 0)]

confMatrix <- matrix(
  c(tmp[G_predict == 1, sum(G_count)], tmp[G_predict == 0, sum(G_count)],
    tmp[G_predict == 1, sum(B_count)], tmp[G_predict == 0, sum(B_count)]),
  nrow=2, ncol=2, byrow=TRUE)
colnames(confMatrix) <- paste0("Predicted: ", c("G", "B"))
rownames(confMatrix) <- paste0("Actual: ", c("G", "B"))
confMatrix

#The same using INNER JOIN
setkey(tmp,kClass)
setkey(CasesStd,kClass)
CasesStd <- CasesStd[tmp[,.(kClass,G_predict)],nomatch=0]
CasesStd[, GClient:= ifelse(Client == "B",0,1)]
table(CasesStd[,GClient],CasesStd[,G_predict])


#9 Classification quality
classSuccess <- sum(diag(confMatrix))/sum(confMatrix)


#10 Classification graph

library(ggplot2)

qplot(TOA,M_LastPaymentToImportDate,data=CasesStd, colour = Client, label=Client, xlab = "TOA", ylab = "M_LastPaymentToImportDate",size=I(1))

library(scatterplot3d)
library(rgl)
library(car)
scatter3d(x = CasesStd[,TOA], y = CasesStd[,M_LastPaymentToImportDate], z = CasesStd[,kClass], groups = as.factor(CasesStd[,Client]), surface=FALSE)



#11  Cluster analysis - Good vs Bad Clients   More variables

summary(CasesStd)

Variables <- c(        "LoanAmount",
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

k <- 5
kCluster <- kmeans(x=CasesStd[, .SD, .SDcols=Variables],
  centers=k, iter.max=100, nstart=10)

# Assigning clusters
CasesStd[, kClass:=kCluster$cluster]

# Proportion of goods
(G_prior <- CasesStd[Client == "G", .N]/CasesStd[, .N])


# Classification matrix
#             | Predicted Good | Predicted Bad  |
# Actual Good |      TP        |       FN       | = P
# Actual Bad  |      FP        |       TN       | = N
#
# sensitivity = true positive rate = TP/P = TP/(TP + FN)
# specificity = tru negative rate = TN/N = TN/(FP + TN)

tmp <- CasesStd[, .(.N,
  B_count=sum(ifelse(Client == "B", 1,0)),
  G_count=sum(ifelse(Client == "G", 1,0)),
  G_percent=sum(ifelse(Client == "G", 1,0))/.N), by=kClass][order(kClass)]

# Forecast od Payment6M
tmp[, G_predict:=ifelse(G_percent > G_prior, 1, 0)]

confMatrix <- matrix(
  c(tmp[G_predict == 1, sum(G_count)], tmp[G_predict == 0, sum(G_count)],
    tmp[G_predict == 1, sum(B_count)], tmp[G_predict == 0, sum(B_count)]),
  nrow=2, ncol=2, byrow=TRUE)
colnames(confMatrix) <- paste0("Pred: ", c("G", "B"))
rownames(confMatrix) <- paste0("Real: ", c("G", "B"))
confMatrix

# Classification quality
(classSuccess <- sum(diag(confMatrix))/sum(confMatrix))



#12 The choice of k and its effect on classification quality

(G_prior <- CasesStd[Client == "G", .N]/CasesStd[, .N])

kClassSuccess <- data.table()
for (k in 2:30) {
  kCluster <- kmeans(x=CasesStd[, .SD, .SDcols=Variables],
    centers=k, iter.max=100, nstart=10)

  CasesStd[, kClass:=kCluster$cluster]

tmp <- CasesStd[, .(.N,
  B_count=sum(ifelse(Client == "B", 1,0)),
  G_count=sum(ifelse(Client == "G", 1,0)),
  G_percent=sum(ifelse(Client == "G", 1,0))/.N), by=kClass][order(kClass)]
  tmp[, G_predict:=ifelse(G_percent > G_prior, 1, 0)]

confMatrix <- matrix(
  c(tmp[G_predict == 1, sum(G_count)], tmp[G_predict == 0, sum(G_count)],
    tmp[G_predict == 1, sum(B_count)], tmp[G_predict == 0, sum(B_count)]),
  nrow=2, ncol=2, byrow=TRUE)

  kClassSuccess <- rbindlist(list(kClassSuccess,
    data.table(K=k, kClassSuccess=sum(diag(confMatrix))/sum(confMatrix))))
}

plot(kClassSuccess)



################################################################################################################################################################
####################################################################################################################
##################################################13 knn - k nearest neighbours

k <- 5
Variables <- c("TOA","M_LastPaymentToImportDate")



kNearest <- class::knn(
  train=CasesStd[, .SD, .SDcols=Variables],
  test=CasesStd[, .SD, .SDcols=Variables],
  cl=CasesStd$Client,
  k=k, use.all=FALSE)

cfMatrix <- table(kNearest, CasesStd$Client)
sum(diag(cfMatrix))/sum(cfMatrix)

knnClassSuccess <- data.table()
for (k in seq(from=1, to=29, by=2)) {
  kNearest <- class::knn(
    train=CasesStd[, .SD, .SDcols=Variables],
    test=CasesStd[, .SD, .SDcols=Variables],
    cl=CasesStd$Client,
    k=k, use.all=FALSE)

  cfMatrix <- table(kNearest, CasesStd$Client)

  knnClassSuccess <- rbindlist(list(knnClassSuccess,
    data.table(K=k, ClassSuccess=sum(diag(cfMatrix))/sum(cfMatrix))))
}




################################################## knn - k nearest neighbours - more variables
k <- 5
Variables <- c(        "LoanAmount",
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



kNearest <- class::knn(
  train=CasesStd[, .SD, .SDcols=Variables],
  test=CasesStd[, .SD, .SDcols=Variables],
  cl=CasesStd$Client,
  k=k, use.all=FALSE)

cfMatrix <- table(kNearest, CasesStd$Client)
sum(diag(cfMatrix))/sum(cfMatrix)

knnClassSuccess <- data.table()
for (k in seq(from=1, to=29, by=2)) {
  kNearest <- class::knn(
    train=CasesStd[, .SD, .SDcols=Variables],
    test=CasesStd[, .SD, .SDcols=Variables],
    cl=CasesStd$Client,
    k=k, use.all=FALSE)

  cfMatrix <- table(kNearest, Test$Client)

  knnClassSuccess <- rbindlist(list(knnClassSuccess,
    data.table(K=k, ClassSuccess=sum(diag(cfMatrix))/sum(cfMatrix))))
}


#14 Train and Test data

indeks.train <- sample(1:dim(CasesStd)[1],0.7*dim(CasesStd)[1])

Train <- CasesStd[indeks.train,]
Test <- CasesStd[-indeks.train,]

knnClassSuccess <- data.table()         
for (k in seq(from=1, to=30, by=5)) {
  kNearest <- class::knn(
    train=Train[, .SD, .SDcols=Variables],
    test=Test[, .SD, .SDcols=Variables],
    cl=Train$Client,
    k=k, use.all=FALSE)

  cfMatrix <- table(kNearest, Test$Client)

  knnClassSuccess <- rbindlist(list(knnClassSuccess,
    data.table(K=k, ClassSuccess=sum(diag(cfMatrix))/sum(cfMatrix))))
}







