mydata<- read.csv("C:\\Users\\lixin\\Desktop\\appliedstats\\intro to statistical learning data sets\\College.csv")

summary(mydata)
fix(mydata) #data is presented in a spreadsheet-like way, have to close the window in order for later commands to run


# If You need NA count of all --- table(is.na(z)) 
# If you need NA count Column wise -- sapply(z, function(x) sum(is.na(x)))
# If you need NA count Row wise --- rowSums(is.na(z))

#check for missing values

table(is.na(mydata))
f<- function(x){
  sum(is.na(x))
}
sapply(mydata, f)
#      X     Private        Apps      Accept      Enroll   Top10perc   Top25perc F.Undergrad P.Undergrad 
# 0           0           0           0           0           0           0           0           0 
# Outstate  Room.Board       Books    Personal         PhD    Terminal   S.F.Ratio perc.alumni      Expend 
# 0           0           0           0           0           0           0           0           0 
# Grad.Rate 
# 0

#no missing values for any column

rowSums(is.na(mydata))


rownames(mydata)= mydata[,1] # add a column called row.names to the data set

#now eliminate the original column that stores the rownames to avoid R to do calculations on it
mydata<- mydata[,-1]
#now the first column was eliminated and the row names column is converted into another column that's not
# part of the data but kinda as a label for each row


# Private : Public/private indicator
# . Apps : Number of applications received
# . Accept : Number of applicants accepted
# . Enroll : Number of new students enrolled
# . Top10perc : New students from top 10% of high school class
# . Top25perc : New students from top 25% of high school class
# . F.Undergrad : Number of full-time undergraduates
# . P.Undergrad : Number of part-time undergraduates
# . Outstate : Out-of-state tuition
# . Room.Board : Room and board costs
# . Books : Estimated book costs
# . Personal : Estimated personal spending
# . PhD : Percent of faculty with Ph.D.'s
# . Terminal : Percent of faculty with terminal degree
# . S.F.Ratio : Student/faculty ratio
# . perc.alumni : Percent of alumni who donate
# . Expend : Instructional expenditure per student
# . Grad.Rate : Graduation rate
#create scatter plot matrix for certain columns of the data
pairs(mydata[,2:5])

#side-by-side boxplots of outstate vs private
boxplot(mydata$Outstate~mydata$Private, col=rainbow(2), main="boxplot of Outstate tuition by type of school: public vs private")
# so on average, private schools have more outstate students than public schools

#now create a variable called Elite
Elite =rep ("No",nrow(mydata ))

Elite [mydata$Top10perc >50]=" Yes"   #i don't understand how it works here?! the expression in the bracket is a vector of booleans

Elite =as.factor (Elite)
mydata =data.frame( mydata ,Elite)

#boxplot of outstate vs elite
boxplot(mydata$Outstate~mydata$Elite, col=rainbow(2), main="boxplot of Outstate vs Elite")


#a few histograms
par(mfrow=c(2,2))
h<-hist(mydata$Outstate, main="histogram of outstate tuition", col="red")
xfit<-seq(min(mydata$Outstate),max(mydata$Outstate),length=40) 
yfit<-dnorm(xfit,mean=mean(mydata$Outstate),sd=sd(mydata$Outstate)) 
yfit <- yfit*diff(h$mids[1:2])*length(mydata$Outstate)  #mids is a vector of all the mid points of the ranges of breaks in histogram
#n_tuition<- density(rnorm(1000, mean(mydata$Outstate), sd(mydata$Outstate)))
lines(xfit, yfit, col="blue", lwd=2)
hist(mydata$S.F.Ratio, main="histogram of student faculty ratio", col="blue")
hist(mydata$Grad.Rate, main="histogram of graduation rate", col="green")
hist(mydata$perc.alumni, main="histogram of percent of alumni donate", col="black")

#check for normality
qqnorm(mydata$Outstate)
qqnorm(mydata$Grad.Rate)
qqnorm(mydata$S.F.Ratio)
qqnorm(mydata$perc.alumni)

#compare distribution of the four variables above by private vs public school
install.packages("lattice")
library(lattice)

histogram( ~ Grad.Rate | Private, data=mydata, main="histograms of graduation rate for public and private school") #
histogram( ~ Outstate | Private, data=mydata, main="histograms of outstate tuition for public and private school") #
histogram( ~ S.F.Ratio | Private, data=mydata, main="histograms of student faculty ratio for public and private school") #
histogram( ~ perc.alumni | Private, data=mydata, main="histograms of perc of alum donation for public and private school") #


# par(mfrow=c(2,1))
# if(mydata$Private==2) hist(mydata$Grad.Rate, main="histogram of graduation rate for private schools", col="green")
# if(mydata$Private==1) hist(mydata$Grad.Rate, main="histogram of graduation rate for public schools", col="green")
# the above comments didn't work


# look at acceptance rate
Accept.rate<- mydata$Accept/mydata$Apps
Enroll.rate<- mydata$Enroll/mydata$Accept
mydata<- data.frame(mydata, Accept.rate, Enroll.rate)

var1<-mydata$Grad.Rate
by1<- mydata$Private
# func1<- function(x){
#   mean(x)
#   var(x)
#   
# }
mean.comparison <- aggregate(mydata[,-1], 
                            list(category = by1),
                            mean)
mean.comparison

std.comparison <- aggregate(mydata[,-1], 
                             list(category = by1),
                            sd )
std.comparison


#look at correlations of some key variables
cor(mydata[,c(13,15,17,18,20,21)], use="complete.obs") 
simple_fit1<- lm(mydata$Grad.Rate ~ mydata$PhD + mydata$Expend + mydata$S.F.Ratio + mydata$Accept.rate + mydata$Enroll.rate)
summary(simple_fit1)

Instructional.expend_tuition_ratio<- mydata$Expend/mydata$Outstate
mydata<- data.frame(mydata, Instructional.expend_tuition_ratio)
 
simple_fit2<- lm(mydata$Grad.Rate ~ mydata$PhD + mydata$Expend + mydata$Instructional.expend_tuition_ratio+
                   mydata$S.F.Ratio + mydata$Accept.rate + mydata$Enroll.rate)
summary(simple_fit2)



##############################################################################
######### predict private vs public schools given other varaibles ############
##############################################################################
data_train<- mydata[1:500,-19]  #remove Elite column
data_test<- mydata[501:777,-19] #remove Elite column

#OLS regression (this is probably not a good model since a lot of the regressors are endogenous, just first trial of the data)
data_train$Private<- as.numeric(data_train$Private)
data_train$Private[data_train$Private==1] <- 0
data_train$Private[data_train$Private==2] <- 1

model_ols1<- lm(Private ~ Apps + Accept + Accept.rate + Enroll + Enroll.rate + 
                 Top10perc+ Top25perc + F.Undergrad + P.Undergrad + Outstate + 
                 Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + 
                 perc.alumni + Expend + Grad.Rate + Instructional.expend_tuition_ratio,
                 data=data_train)
model_ols2<- lm(Private ~. ,
               data=data_train)
summary(model_ols1)
summary(model_ols2)
plot(model_ols2$residuals)
points(model_ols2$fitted.values, col="blue")
points(data_train$Private, col="red")

#use logistic regression for classification
model_logit1 <- glm(Private ~.,family=binomial(link='logit'),data=data_train)
summary(model_logit1)
p<- predict(model_logit1, newdata=data_train, type="response") #model seems makes predictions just fine

p.private<- round(p)

install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)
confusionMatrix(p.private, data_train$Private)
#Accuracy : 0.964  

#now use the test data set
p_test<- predict(model_logit1, newdata= data_test, type="response") 
p.private_test<- round(p_test)
data_test$Private<- as.numeric(data_test$Private)
data_test$Private[data_test$Private==1] <- 0
data_test$Private[data_test$Private==2] <- 1
confusionMatrix(p.private_test, data_test$Private)
#out of sample
#Accuracy : 0.917   



#KNN #cv
#since we use a distance function in KNN, it's always a better practice to normalize/standardize the predictors 
# before running the model
#normalization: x=(x-min)/(max-min)
#standardization: x=(x-mean)/std

normalization<- function(x){
   return( (x-min(x))/ (max(x)-min(x)))
}
for(i in 2:21){
  data_train[,i]= normalization(data_train[,i])
  data_test[,i]= normalization(data_test[,i])
}

data_train_n<- lapply(data_train[2:21], normalization)
data_test_n<- lapply(data_test[2:21], normalization)


#choose k using k fold CV

#let k=10 for now
install.packages("class")
library(class)

model_knn1<- knn(data_train, data_test, cl=data_train[,1], k=2)  #tried k=2~10, results didn't change!
table(data_test[,1], model_knn1)

#LDA : assume the k classes have different means but share the same covariance matrix

#QDA: the k class have different mean and different covariance matrices
install.packages("MASS")
library(MASS)
model_qda<- qda(data_train[,2:21], data_train[,1])
p.qda<- predict(model_qda, data_test[,2:21])$class
table(data_test[,1], p.qda)

#p.qda
#    0   1
# 0  93  24
# 1  17 143

##########################
#try different specifications for each model, not use all the other variables 
#########################