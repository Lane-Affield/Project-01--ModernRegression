##NAME: Lane Affield
##PROF: Herath Wiranthe
#DATE: 03/23/23
#TITLE: PROJECT 01
#DATASET: FISH MARKET
#DATASET LINK: 
getwd()
Fish <- read.csv("Fish.csv") #Load Amazon Dataset
attach(Fish) #attach it


#EXPLORING DATASET
dim(Fish)
head(Fish)
View(Fish)
plot(Fish, col = "blue")
summary(Fish)

library(corrplot)
Species

dummies <- model.matrix(~ Species, data = Fish) #creates dummies for species
fish <- cbind(Fish, dummies) #adds to dataset 
fish <- fish[,-1] #removes the original species column from dataset
fish
correlations<-cor(Fish[-1])#cor matrix
correlations
corrplot(correlations, "circle") #correlation plot

#LM
lm.fit1 <- lm(Weight~Length1)
summary(lm.fit1)
par(mfrow = c(2,2)) #divides plotting region
plot(lm.fit1)
plot(predict(lm.fit1), residuals(lm.fit1))
plot(Weight~Length1)
abline(lm.fit1, col = "green")
confint(lm.fit1)#get confints
abline(lm.fit)

plot(lm.fit)

lm.fit2 <- lm(Weight ~ Length1 + Species)
summary(lm.fit2)
par(mfrow = c(2,2)) #divides plotting region
plot(lm.fit2)
confint(lm.fit2)


#LR
train<-1:112
test<-Fish.new[113:159,]
lr.fit.train<-glm(Weight01 ~ Length1 + Species, data = Fish, family = binomial, subset = train)
summary(lm.fit2)

lr.prob.test <- predict(lr.fit.train, test, type="response")
lr.pred.test <- rep(0, 78)
lr.pred.test [lr.prob.test > .5] <- 1


table(lr.pred.test, test.target)
mean(lr.pred.test == test.target)


Weight01 <- ifelse(Weight > median(Weight), yes=1, no =0)#change to binary values
Fish.new<-data.frame(Weight01, fish)
lr.fit<-glm(Weight01 ~ Length1 + Species)
summary(lr.fit)
plot(lr.fit)
confint(lr.fit)#get confints

train<-1:112
test<-Fish.new[113:159,]
lr.fit.train<-glm(Weight01 ~ Length1 + SpeciesParkki + SpeciesPerch + SpeciesRoach + SpeciesSmelt +SpeciesWhitefish, data = Fish.new, family = binomial, subset = train)
summary(lr.fit.train)

lr.prob.test <- predict(lr.fit.train, test, type="response")
lr.pred.test <- rep(0, 78)
lr.pred.test [lr.prob.test > .5] <- 1


table(lr.pred.test, test.target)
mean(lr.pred.test == test.target)

