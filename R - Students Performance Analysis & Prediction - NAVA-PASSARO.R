setwd("C:/Users/Asus S512JF-EJ014T/Downloads/Data Analysis")
DataSet <- read.csv("DataSet.csv")
View(DataSet)

# EDA --> EXPLORATORY DATA ANALYSIS ####

# BARPLOT: Gender Distribution
color = c('seashell1', 'palegreen4')
table(DataSet$sex)
barplot(table(DataSet$sex), main='Difference between genders', col=color, legend=c('Female', 'Male'), ylim = c(0,250), xlim = c(0,5), width = 1 )

# PIE CHART: Parent Status
color1 = c('seashell1', 'palegreen4')
table(DataSet$Pstatus)
pie(table(DataSet$Pstatus), main='Parent Status', col=color1, labels = c('Apart', 'Together'))

# 3D PIE CHART: Parents Occupation
library(plotrix)
library(RColorBrewer)
color2 = c(brewer.pal(n = 5, name = 'Greens'))
lbls <- c('1 - Teacher', '2 - Health', '3 - Services', '4 - Home', '5 - Other')
table(DataSet$Fjob)
table(DataSet$Mjob)
pie3D(table(DataSet$Fjob), main='Father Occupations', col=color2, labels=lbls,labelcex = 1 )
pie3D(table(DataSet$Mjob), main='Mother Occupations', col=color2, labels=lbls, labelcex = 1)

freqT <- table(DataSet$Fjob, DataSet$Mjob)
marginF <- c(marginSums(freqT[1,]), marginSums(freqT[2,]), marginSums(freqT[3,]), marginSums(freqT[4,]), marginSums(freqT[5,]))
marginM <- c(marginSums(freqT[,1]), marginSums(freqT[,2]), marginSums(freqT[,3]), marginSums(freqT[,4]), marginSums(freqT[,5]))
vector <- c(marginF, marginM)
margins <- matrix(vector,  nrow=2, ncol=5, byrow=TRUE)
colnames(margins) = c('1 - Teacher', '2 - Health', '3 - Services', '4 - Home', '5 - Other')
rownames(margins) = c('Father', 'Mother')
margins
barplot(margins, main = 'Father vs Mother Occupation', beside = TRUE, col=c('darkgreen', 'lightgreen'),ylim = c(0,300),xlim = c(0,15), width = 1, legend.text = c('Father', 'Mother'))

# BARPLOT: Quality of Family Relations
color3 = c(brewer.pal(n = 5, name = 'Greens'))
table(DataSet$famrel)
barplot(table(DataSet$famrel), main ='Family Relations', col = color3, ylim = c(0,300), xlim = c(0,7), width = 1)
legend(x=0.5, y=250, legend = c('1 - Very Bad', '2 - Bad', '3 - Moderate', '4 - Good', '5 - Excellent'),col = color3, pch = 15)

# HISTOGRAM:  Parent Education Level per Job
library(ggplot2)
qplot(Medu, main = "Mother Job Distribution per Education", data = DataSet, geom = "histogram", fill = Mjob, binwidth = 0.35)
qplot(Fedu, main = "Father Job Distribution per Education", data = DataSet, geom = "histogram", fill = Fjob, binwidth = 0.35)

par(mfrow=c(1,2))
boxplot(DataSet$Medu, xlab = "Mothers Education", ylab = "Levels", ylim=c(-1 ,5))
boxplot(DataSet$Fedu, xlab = "Fathers Education", ylim=c(-1,5))
par(mfrow=c(1,1))

# BARPLOT: Family Support 
color4 = c('grey', 'tan1')
table(DataSet$famsup)
barplot(table(DataSet$famsup), col = color4, main='Family Support', ylim = c(0,250), ylab = 'Frequency', xlim = c(0,7), width = 1)
table(DataSet[,c(17,15)])

# BARPLOT: Weekely Studytime
color5 = c(brewer.pal(n = 5, name = 'Oranges'))
table(DataSet$studytime)
barplot(table(DataSet$studytime), main='Weekly studytime', col=color5, ylim=c(0,200), xlim = c(0,7), width = 1, legend.text = c('1 - <2hours', '2 - 2 to 5 hours', '3 - 5 to 10 hours', '4 - >10 hours'))

# BARPLOT: Failures 
color6 = c(brewer.pal(n = 4, name = 'Oranges'))
table(DataSet$failures)
barplot(table(DataSet$failures), main= 'Count of Past Failures', col= color6, ylim = c(0,350), xlim = c(0,7), width = 1)

# HISTOGRAMS: G1, G2, G3
hist(DataSet$G1, main = 'Grade 1 Distribution', ylim = c(0,100), xlab = 'G1', xlim=c(0,20), col = c('wheat1'))
hist(DataSet$G2, main = 'Grade 2 Distribution', ylim = c(0,100), xlab = 'G2', xlim=c(0,20), col = c('tan1'))
hist(DataSet$G3, main = 'Grade 3 Distribution', ylim = c(0,100), xlab = 'G3', xlim=c(0,20), col = c('sienna2'))

summary(DataSet$G1)
summary(DataSet$G2)
summary(DataSet$G3)


# CORRELATION ####

# Correlation Matrix Graph
## Graph that represents the correlation matrix between the variables of a dataset
subfam <- subset(DataSet, select=c(sex, Pstatus, Medu, Fedu, Mjob, Fjob, famsup, famrel, studytime, failures, schoolsup, romantic, freetime, G1, G2, G3)) 
head(subfam)
install.packages("corrgram")
library(corrgram)

# Pearson
corrgram(subfam, order=TRUE, lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt, cor.method = "pearson", main = "Correlation Matrix Graph - Pearson")
# Spearman
corrgram(subfam, order=TRUE, lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt, cor.method = "spearman", main = "Correlation Matrix Graph - Spearman")
# Kendall
corrgram(subfam, order=TRUE, lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt, cor.method = "kendall", main = "Correlation Matrix Graph - Kendall")



# CLUSTERING ####

# Clustering requires numeric variables
subset.clus <- subset(DataSet,select = c(Medu, Fedu, famrel, G1, G2, G3))
subset.sc <- scale(subset.clus)
subset.dist <- dist(subset.sc)

# Hierarchical Clustering
subset.hc <- hclust(subset.dist, method = "complete")
str(subset.hc)
subset.hc

plot(subset.hc, labels = NULL, hang = 0.1, check = TRUE, axes = TRUE, frame.plot = FALSE, ann = TRUE, main = "Cluster Dendrogram", sub = NULL, xlab = NULL, ylab = "Height")
rect.hclust(subset.hc, k=2)

## The cutree() function returns the partitionin of dataset for a given level
subset.hc.2 <- cutree(subset.hc,2)
## To see the partitioning of first 2 Clusters
subset.hc.2

# Isolate a cluster and see its characteristics
subset.clus[subset.hc.2==1,]   ###first cluster, all columns
summary(subset.clus[subset.hc.2==1,])

table(subset.clus$G1, subset.hc.2)
table(subset.clus$G2, subset.hc.2)
table(subset.clus$G3, subset.hc.2)


# Distribition of  and Grades in different Clusters
# Cluster 1 
hist(subset.clus$G1[subset.hc.2==1], ylim = c(0,100), xlim=c(0,20), col = "seashell1", main = "Distribution of Grades 1 in Cluster 1", xlab = "G1")
hist(subset.clus$G2[subset.hc.2==1], ylim = c(0,100), xlim=c(0,20), col = "palegreen2", main = "Distribution of Grades 2 in Cluster 1", xlab = "G2")
hist(subset.clus$G3[subset.hc.2==1], ylim = c(0,100), xlim=c(0,20), col = "palegreen3", main = "Distribution of Grades 3 in Cluster 1", xlab = "G3")

# Cluster 2
hist(subset.clus$G1[subset.hc.2==2], ylim = c(0,100), xlim=c(0,20), col = "seashell1", main = "Distribution of Grades 1 in Cluster 2", xlab = "G1")
hist(subset.clus$G2[subset.hc.2==2], ylim = c(0,100), xlim=c(0,20), col = "palegreen2", main = "Distribution of Grades 2 in Cluster 2", xlab = "G2")
hist(subset.clus$G3[subset.hc.2==2], ylim = c(0,100), xlim=c(0,20), col = "palegreen3", main = "Distribution of Grades 3 in Cluster 2", xlab = "G3")




# SUBSET for PCA, Linear Regression and SVM ####
## Transforming categorical variables into numerical ones
sex_bin <- ifelse(DataSet$sex == "M", 1, 0)
Pstatus_bin <- ifelse(DataSet$Pstatus == "T", 1, 0)
famsup_bin <- ifelse(DataSet$famsup == "yes", 1, 0)
schoolsup_bin <- ifelse(DataSet$schoolsup == "yes", 1, 0)
romantic_bin <- ifelse(DataSet$romantic == "yes", 1, 0)

Subset = data.frame(sex_bin, Pstatus_bin, DataSet$Medu, DataSet$Fedu, famsup_bin, DataSet$famrel, DataSet$studytime, DataSet$failures, schoolsup_bin, romantic_bin, DataSet$freetime, DataSet$Dalc, DataSet$Walc, DataSet$absences, DataSet$G1, DataSet$G2, DataSet$G3)

## Replacing zeros in G2 and G3 with NA
Subset$G2 <- ifelse(Subset$G2==0, NA, Subset$G2)
Subset$G3 <- ifelse(Subset$G3==0, NA, Subset$G3)
## Omitting NAs
Subset <- na.omit(Subset)

summary(Subset)
View(Subset)



# PCA --> PRINCIPAL COMPONENT ANALYSIS ####

## prcomp() to run the PCA. scale = TRUE to normalize the data
## Data standardization can help compare variables with different units of measurement
pca = prcomp(Subset, scale = TRUE)
summary(pca)

# Plot of PCA 
install.packages("FactoMineR")
library(FactoMineR)
PCA = PCA(Subset[,])



# LINEAR REGRESSION ####
# Linear Regression on all the features
lm0 <- lm(DataSet.G3 ~ ., data=Subset)
summary(lm0)

# Linear Regression on grades G1 and G2
lm1 <- lm(DataSet.G3 ~ DataSet.G1 + DataSet.G2, data=Subset)
summary(lm1)

# Linear Regression on family variables
lm2 <- lm(DataSet.G3 ~ Pstatus_bin + DataSet.Fedu + DataSet.Medu + famsup_bin + DataSet.famrel, data=Subset)
summary(lm2)

# Linear Regression on studytime and failures
lm3 <- lm(DataSet.G3 ~ DataSet.studytime + DataSet.failures, data=Subset)
summary(lm3)


# SVM --> SUPPORT VECTOR MACHINE ####

## Setting train and test partitions from a sample of subset
sample.svm <- sample(nrow(Subset), nrow(Subset)*0.8)

train.svm <- Subset[sample.svm,]
train.svm <- data.frame(train.svm)

test.svm <- Subset[-sample.svm,]
test.svm <- data.frame(test.svm)

attach(train.svm)
attach(test.svm)

library(e1071)

## Appling SVM Model Function
subset.lsvm <- svm(DataSet.G3 ~., data = train.svm, type = "C-classification", kernel = "linear")

# SVM Model Results and Table of Predictions from test set
subset.lsvm

pred <- predict(subset.lsvm, test.svm)
table(pred, DataSet.G3)

# SVM: FAM Subset 
head(Subset)
fam.lsvm <- svm(DataSet.G3 ~ Pstatus_bin + DataSet.Medu + DataSet.Fedu + DataSet.famrel + famsup_bin + schoolsup_bin, data = train.svm, type = "C-classification", kernel = "linear")

# FAM SVM Model Results and Table of Predictions from test set
fam.lsvm

fam.pred <- predict(fam.lsvm, test.svm)
table(fam.pred, DataSet.G3)


# SVM: Student Features
stu.lsvm <- svm(DataSet.G3 ~ DataSet.studytime + DataSet.failures + DataSet.freetime + DataSet.Dalc + DataSet.Walc + DataSet.absences, data = train.svm, type = "C-classification", kernel = "linear")
stu.pred <- predict(stu.lsvm, test.svm)
table(stu.pred, DataSet.G3)

# SVM: Final Grade based on Other Grades 
grade.lsvm <- svm(DataSet.G3 ~ DataSet.failures + DataSet.G1 + DataSet.G2, data = train.svm, type = "C-classification", kernel = "linear")
grade.pred <- predict(stu.lsvm, test.svm)
table(grade.pred, DataSet.G3)

par(mfrow=c(2,2))

# Plot of Predicted Values vs Real Values
plot(x = DataSet.G3, y = pred, main = "Predicted G3 vs Real G3", xlab = "Real G3", ylab = "Pred G3")

# Plot of Predicted Values vs Real Values : FAM SVM
plot(x = DataSet.G3, y = fam.pred, main = "Predicted G3 vs Real G3 : FAM SVM", xlab = "Real G3", ylab = "Pred G3")

# Plot of Predicted G3 : STU SVM
plot(x = DataSet.G3, y = stu.pred, main = "Predicted G3 vs Real G3 : STU SVM", xlab = "Real G3", ylab = "Pred G3")

# Plot of Predicted G3 : GRADES SVM
plot(x = DataSet.G3, y = grade.pred, main = "Predicted G3 vs Real G3 : GRADE SVM", xlab = "Real G3", ylab = "Pred G3")

par(mfrow=c(1,1))



# MODEL PERFORMANCE EVALUATION ####
install.packages("Metrics")
library(Metrics)

# lm1 vs grade.lsvm
pred.lm1 <- predict(lm1, newdata = test.svm)
pred.grade.lsvm <- predict(grade.lsvm, newdata = test.svm)

rmse_lm1 <- rmse(DataSet.G3, pred.lm1)
rmse_grade.lsvm <- rmse(DataSet.G3, as.numeric(pred.grade.lsvm))

rmse_lm1
rmse_grade.lsvm

# lm2 vs fam.lsvm
pred.lm2 <- predict(lm2, newdata = test.svm)
pred.fam.lsvm <- predict(fam.lsvm, newdata = test.svm)

rmse_lm2 <- rmse(DataSet.G3, pred.lm2)
rmse_fam.lsvm <- rmse(DataSet.G3, as.numeric(pred.fam.lsvm))

rmse_lm2
rmse_fam.lsvm

# lm0 vs subset.lsvm
pred.lm0 <- predict(lm0, newdata = test.svm)
pred.subset.lsvm <- predict(subset.lsvm, newdata = test.svm)


rmse_lm0 <- rmse(DataSet.G3, pred.lm1)
rmse_subset.lsvm <- rmse(DataSet.G3, as.numeric(pred.grade.lsvm))

rmse_lm0
rmse_subset.lsvm



print("The End :)")

