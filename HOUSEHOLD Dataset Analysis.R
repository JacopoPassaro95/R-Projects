### HOUSEHOLD DATA SET 
setwd("C:/Users/Asus S512JF-EJ014T/Downloads") #set directory
household = read.table("household.txt", header=T) #read and import txt dataset
household
attach(household) #now it is possible to recall variables without typing household$...

plot(household) #general plots 
summary(household)
by(household, household$gender, summary) #summary by gender
str(household)
View(household)

## HISTOGRAMS BY VARIABLES 

#op <- par(mfrow = c(2, 2),           # 2 x 2 pictures on one plot
#pty = "s")                           # square plotting region,
                                      # independent of device size
## At end of plotting, reset to previous settings:
#  par(op)

?par
{par(mfrow = c(2,2))
hist(household$housing, col = "white")
hist(household$food, col = "grey")
hist(household$service, col = "blue")
hist(household$goods, col = "pink")
}

#DATASET DIVIDED BY GENDER

#from data frame to tables to work with data for males and females
dfm <- as.matrix(household); dft <- as.table(dfm) 
#doing this here if we need the whole set of data in the future

###Conditional View, For EX 

household$food[household$gender == "female"]
household$gender[household$food >= 250]

household_female <- household[household$gender=="female",]
household_female
females <- as.matrix(household_female[,1:4]) 
female_exp<-as.table(females)
#done this series of assignments in order to work properly with numeric 
#values and using directly labels
females
female_exp



household_male <- household[household$gender=="male",]
household_male
males <- as.matrix(household_male[,1:4]); male_exp<-as.table(males) 
#done this series of assignments in order to work properly with 
#numeric values and using directly labels
male_exp


#Pie charts for males and females in order to see a "practical"(or more handy) 
#distribution of their expenses

?pie    #par can be used to set or query graphical parameters

#pie(x, labels = names(x), edges = 200, radius = 0.8,
#clockwise = FALSE, init.angle = if(clockwise) 90 else 0,
#density = NULL, angle = 45, col = NULL, border = NULL,
#lty = NULL, main = NULL, ...)

par(mfcol=c(1,2))
pie(colSums(female_exp),main = "Female expenses") # main title for the pie-chart 
pie(colSums(male_exp),main = "Male expenses")
#We can easily detect some good information by the graphical representation above


#But who spends more money between men and women in total?

total_female_exp <-sum(colSums(female_exp))
total_female_exp

total_male_exp<-sum(colSums((male_exp)))
total_male_exp

if (total_female_exp > total_male_exp) {
  print("Females spend more money than males in this data set! Their expenses are equal to : ",quote = F)
  print(total_female_exp)
}else {
  print("Males spend more money than females in this data set! Their expenses are equal to (in HK$): ",quote = F)
  print(total_male_exp)
  print("Females spend instead (in HK$) : ", quote = F)
  print(total_female_exp)
}

#Averages

# tapply function takes the chosen variable, splits it according to gender, 
# and computes the mean for each group

tapply(household$housing, household$gender, mean)
tapply(household$food, household$gender, mean)
tapply(household$goods, household$gender, mean)
tapply(household$service, household$gender, mean)


###How much money does every woman of the set spend on average in general 
#and for each different cause?

tot_avg_female <-mean(female_exp) #general
tot_avg_female

housingf<-female_exp[,"housing"]
hf <-mean(housingf)#avg housing expenses

foodf<-female_exp[,"food"]
foodf
ff<-mean(foodf)#avg food expenses

goodsf<-female_exp[,"goods"]
gf<-mean(goodsf)#avg goods expenses
gf

servicef<-female_exp[,"service"]
sf<-mean(servicef)#avg service expenses

labels_avg <- c("avg_housing","avg_food","avg_goods","avg_service") #labels for the averages
labels_avg
avgf <- c(hf,ff,gf,sf)
female_avg_exp <- rbind(labels_avg,avgf)
female_avg_exp #binding together outputs to get a clear picture of the values
 

###How much money does every man of the set spend on average in general 
#and for each different cause?

tot_avg_male <- mean(male_exp)#general
tot_avg_male

housingm <- male_exp[,"housing"]
hm <- mean(housingm) #avg housing expenses

foodm<-male_exp[,"food"]
fm<-mean(foodm) #avg food expenses

goodsm <- male_exp[,"goods"]
gm <- mean(goodsm) #avg goods expenses

servicem <- male_exp[,"service"]
sm <- mean(servicem) #avg service expenses

avgm <- c(hm,fm,gm,sm)
male_avg_exp<-rbind(labels_avg,avgm)
male_avg_exp #binding together outputs to get a clear picture of the values

#So now we have verified that the conclusions we did before are true (by using the means of the categories)

###HYPOTESIS TESTING

#State null and alternative hypothesis

# Ho: There is no difference in mean of male and female housing expenditure
# H1: There is difference in mean of male and female housing expenditure

#load ggplot2 - its great for making graphs
library(ggplot2)
# Graph the data
# boxplot works great
ggplot(household,aes(x= gender, y= housing)) + geom_boxplot() + theme_classic()
?ggplot



#compare graphs by gender
par(mfrow=c(2,2))
boxplot(service ~ gender, data=household)
boxplot(housing ~ gender, data=household)
boxplot(food ~ gender, data=household)
boxplot( goods~ gender, data=household)

#Hypothesis testing

?t.test()
#t.test(x, y = NULL,
#alternative = c("two.sided", "less", "greater"),
#mu = 0, paired = FALSE, var.equal = FALSE,
#conf.level = 0.95, ...)

t.test(housing ~ gender, data = household, var.equal = T, paired = F)
# t= -2.1119 , df = 38
# p value = 0.04132
# Conclusion: p value is 0.04132, therefore WE REJECT NULL HYPOTHESIS H0 
#and we can conclude that there is difference in mean of male and female housing expenditure.
#mu1 = female's mu
#mu2 = male's mu

t.test(food ~ gender, data = household, var.equal = T, paired = F) #Reject Ho
t.test(service ~ gender, data = household, var.equal = T, paired = F) #Reject Ho
t.test(goods ~ gender, data = household, var.equal = T, paired = F) #Don't Reject Ho
par(mfrow = c(1, 1)) ; boxplot(goods ~ gender, data = household)
# H0: mu1 = mu2
# H1: mu1 != mu2


hist(females)
qqnorm(females)#normality check
summary(females)
boxplot(females)

hist(males)
qqnorm(males)#normality check
summary(males)
boxplot(males)

t.test(female_exp,male_exp) #rejecting null hypo, alpha = 5%
#H1: mu1>mu2

t.test(female_exp, male_exp, alternative = "greater") #alpha is 5% don't reject Ho

#H1: mu1<mu2

t.test(female_exp, male_exp, alternative = "less")#alpha is 5% Reject Ho
 
###USING REGRESSION MODELLING 
# y = b0 + b1*x

library(car)
#package already installed on my computer, so I do not wrote a line of code do install it

?lm #lm is used to fit linear models. It can be used to carry out regression, 
    #single stratum analysis of variance and analysis of covariance

mod <- lm(housing  ~  . - gender , data = household)
avPlots(mod)


#Calculate the correlation between the variables to identify which 
#variables are correlated and how.

cor(household[-5]) #provides a table of correlations among variables 

#The most high correlations are: 
               #service-housing(0.85), 
               #service-goods(0.88),
               #housing-goods(0.72)
?plot
plot(goods, service)
plot(housing, service)
plot(housing, goods)
plot(goods, housing)
plot(service, housing)

plot(household$goods,household$service)
plot(household$housing,household$service)
plot(household$housing,household$goods)

#Now we can produce some simple regression models between the variables.
reg1 = lm(housing ~ goods, data = household) 
plot(household$goods, household$housing)
abline(reg1, col = "red")
summary(reg1)
summary(reg1)$adj.r.squared
#The value of the adjusted r.squared isn't an ideal value to consider this one a good model. 
#Moreover, the plot represent the same result.

#housing = a+b*service
reg2 = lm(housing ~ service, data=household)
plot(household$service,household$housing)
abline(reg2, col = "red")
summary(reg2)
summary(reg2)$adj.r.squared
#This is a better model than the previous one.


reg3 = lm(service ~ goods, data=household) 
plot(household$goods, household$service)
abline(reg3, col = "red")
summary(reg3)
summary(reg3)$adj.r.squared
#This is the most efficient model because the value 
# of the adjusted r.squared is more high that the others one.

# Now, we have chosen the model called reg3. 
# We can calculate the fitted values.

cbind(reg3$fitted.values,
      cbind(1, household$goods) %*% summary(reg3)$coefficients[,1]) #first column is estimate
reg3$fitted.values

#We can see the linear model and the fitted values in a plot.
plot(household$goods, household$service, xlim = c(0,3000))
abline(reg3, col = "red")
points(household$goods, reg3$fitted.values, col = "red")

#The residuals of the model can be seen in the plot.
cbind(residuals(reg3), household$service - reg3$fitted.values) #or
residuals(reg3) 
?segments
segments(household$goods, reg3$fitted.values,   #points FROM which to draw
         household$goods, household$service,    # coordinates for points TO which to draw
         lty = 3) 
#We can see that for the points more far from the other points (outliers), 
# the value of the residual is higher. 

#We can calculate the predicted value of the variable service 
# if for example the value of the variable goods is equal to 500. 

reg3$coefficients[1] + 500 * reg3$coefficients[2] # (y = a + x*b)
?predict #is a generic function for predictions from the results of various model fitting functions
predict(reg3, newdata = rbind(household, c(NA,NA, 500,NA,NA))[nrow(household)+1,]) #by adding a row
points(500, predict(reg3, newdata = rbind(household, c(NA,NA, 500,NA,NA))[nrow(household)+1,]),
       col = "blue", xlim = c(0,1000), ylim = c(0,500))

# T test
summary(reg3)$coefficients
summary(reg3)$coefficients[,1] / summary(reg3)$coefficients[,2] #the t-values
2 * pt(abs(summary(reg3)$coefficients[,3]), 
       nrow(household) - length(reg3$coefficients),lower.tail = F) #the probabilities

### BY FOCUSING ON MODELS DEFINED BY GENDER
#Only the variable food can be explained by the variable gender.
{
  reg = lm(food ~ gender, data = household)
summary(reg)
}

summary(lm(goods ~ gender, data=household))
summary(lm(service ~ gender, data=household))
summary(lm(housing ~ gender, data=household))

### MULTIPLE REGRESSION

reg4 = lm(service ~ goods + gender) 
 
reg5 = update(reg4, . ~ . + housing) #will update and by default refit a model
reg6 = update(reg5, . ~ . + food)

summary(reg4)$adj.r.squared

summary(reg5)$adj.r.squared #the previous model reg4 can be improved by the model reg5 
                            # by adding the variable housing.

summary(reg6)$adj.r.squared #after adding the variable food in the model reg6, 
                            #the adj.r squared increase only by 0.0001 and the '***' 
summary(reg4)
summary(reg5)
summary(reg6)

reg7 = lm(housing ~ goods + gender + service + food)
reg8 = lm(goods ~ housing + gender + service)
reg9 = lm(goods ~ gender + service)

summary(reg7)
summary(reg8)
summary(reg9)



ls(reg5)
reg5$coefficients
reg5$fitted.values

e.y = reg5$residuals

#avPlots
install.packages("car")
library(car)
?avPlot()
#These functions construct added-variable, also called partial-regression, 
#    plots for linear and generalized linear models.

avPlots(reg5)

#avPlots for service and goods 

goods.others = update(reg5, .~. - household$goods)
pi.goods.others = lm(household$goods ~ household$gender + household$housing)
goods.mod = lm(goods.others$residuals ~ pi.goods.others$residuals)
goods.mod
plot(pi.goods.others$residuals, goods.others$residuals)
abline(goods.mod, col="blue")

###CHI-SQUARE TEST 
#Start with the function found in the first part

female_avg = avgf
male_avg = avgm


#create a new tab for testing
household.tab = rbind(female_avg,male_avg)
colnames(household.tab) <- c("housing","food","goods","service")
rownames(household.tab) <- c("Female", "Male")
household.tab

#graphically
female_male_barplot <- barplot(prop.table(t(household.tab), 2), beside= T,
        legend.text = colnames(household.tab),
        col = c("black","grey80","grey50","white"))

#If the argument is a matrix, then barplot creates by default a
#"stacked barplot", where the columns are partitioned according to the 
#contributions from different rows of the table. If you want to place the
#contributions from different rows beside each other instead, you can use 
#the argument beside=T.




chisq.test(household.tab) #reject hypothesis of statistical independence
X2_test <- chisq.test(household.tab)

ls(X2_test)

# Observed counts
X2_test$observed

# Expected counts
X2_test$expected

# demonstration

nij_exp <- matrix(NA, 2, 4)
ni <- rowSums(household.tab)
nj <- colSums(household.tab)
n <- sum(household.tab)


# For loop

for(i in 1:nrow(household.tab)){
  for(j in 1:ncol(household.tab)){
    nij_exp[i,j] <- ni[i]*nj[j]/n  
  }
}

# Chi-squared test statistic
X2_test$statistic
sum((X2_test$observed - X2_test$expected)^2/X2_test$expected)

# Degrees of freedom
X2_test$parameter
prod(dim(household.tab)-1)

# p-value
X2_test$p.value # reject 
pchisq(X2_test$statistic, df = 3, lower.tail = F)

x <- seq(0, 350, 0.01)
plot(x, dchisq(x, df = prod(dim(household.tab)-1)), type = "l") 
abline(h = 0) # horizontal line in zero
segments(qchisq(0.95, 3),0,
         qchisq(0.95, 3), dchisq(qchisq(0.95, 3), 3), col = "red") # rejection region
points(X2_test$statistic, 0, col = "red")

#proportions
tab.h = table(household$gender)
tab.h
table(household$gender)/sum(table(household$gender))
prop.table(tab.h)
perc.h = round(prop.table(tab.h), digits = 2) * 100
perc.h
perc.hn= paste(names(perc.h), perc.h, "%") #Concatenate vectors after converting to character.
perc.hn

 
?cut #divides the range of x into intervals and codes the values
fac = cut(household$service, breaks = c(0,500,501,1000,1001,2100))
fac = factor(fac, ordered = T)
levels(fac) = c("few","medium","many")
fac
table(household$gender,fac)

fac2 = cut(food, breaks = c(0,100,101,500,501,2000))
sort(food)
fac2 = factor(fac2, ordered = T)
levels(fac2)= c("few","medium","many")
fac2

fac2<-cut(household$food, breaks = c(0,100,101,500,501,2000))
fac2<-factor(fac2, ordered = T)
levels(fac2)<-c("few","medium","many")
fac2
table(household$gender,fac2)
TAB <- table(household$gender,fac2)
TAB
chisq.test(table(household$gender,fac2)) #APPROXIMATION MAY BE INCORRECT