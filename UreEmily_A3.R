install.packages("ISLR")
library(ISLR)

summary(Weekly)

pairs(Weekly)

cor(Weekly[,-9])

attach(Weekly)

fix(Weekly)



glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Volume,data=Weekly,family=binomial)

summary(glm.fits)

glm.probs=predict(glm.fits,type="response")

glm.probs[1:10]

contrasts(Direction)



glm.pred=rep("Down",1089)

glm.pred[glm.probs>.5]="Up"


table(glm.pred,Direction)

mean(glm.pred==Direction)

View(Weekly)

attach(Weekly)

#(training dataset)
# We will then use this vector to create a held out data set of observations

train=(Year<2010)

# create the TEST data

Weekly.2010=Weekly[!train,]
dim(Weekly.2010)

#create the DV in the test data - - [!train] only the observations for which train is FALSE
Direction.2010=Direction[!train]

#run logistic regression in the TRAINING dataset,  using the subset=train argument
glm.fits2=glm(Direction~Lag2,data=Weekly,family=binomial, subset=train)

#predicted probabilities of the stock market going up for each of the weeks in our TEST set-that is, for the weeks in 2010
glm.probs=predict(glm.fits2,Weekly.2010,type="response")

#change the probabilities to value (>0.5 is Up while <=0.5 is Down), 52 is the number of observations in the TEST data
glm.pred=rep("Down",52)
glm.pred[glm.probs>.5]="Up"

#compare the predicted value of DV with the true value of DV in the test dataset

table(glm.pred,Direction.2010)

# calculate the accuracy rate (how much percentage of correct predictions)
mean(glm.pred==Direction.2010)




#KNN
library(class)

# knn() forms predictions using a single command. The function requires four inputs.
# 1. A matrix containing the predictors associated with the training data, labeled train.X below.
# 2. A matrix containing the predictors associated with the data for which we wish to make predictions, labeled test.X below.
# 3. A vector containing the class labels for the training observations, labeled train.Direction below.
# 4. A value for K, the number of nearest neighbors to be used by the classifier.

train=(Year<2010)
test=Weekly.2010

train.X=cbind(Lag2)[train,]
test.X=cbind(Lag2)[!train,]

# a vector containing the class labels (DV) for the training observations
train.Direction=Direction[train]

# K=1
# We set a random seed before we apply knn() because if several observations are tied as nearest neighbors, then R will randomly break the tie. Therefore, a seed must be set in order to ensure reproducibility of results
set.seed(1)

# Now the knn() function can be used to predict the market's movement for the dates in 2005.


knn.pred=knn(data.frame(train.x),data.frame(test.x),(train.Direction),k=1)
table(knn.pred,Direction.2010)
mean(knn.pred==Direction.2010)

# K=10

knn.pred=knn(data.frame(train.x),data.frame(test.x),(train.Direction),k=10)
table(knn.pred,Direction.2010)
mean(knn.pred==Direction.2010)
