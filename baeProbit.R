bayesData <- read.csv(file="gistStuff.csv", h=T) #Formerly known as `final'
set.seed(12) #reproducibility
bayesData$Treatment <- as.factor(bayesData$Treatment)

library(Zelig)
bayesianOProbit <- zelig(Likelihood~Treatment+HorseRaceExposure+PollTrust+PresentationPreference+StatsClass, model="oprobit.bayes", data=bayesData)
#Check for convergence
heidel.diag(bayesianOProbit$result$coefficients) #Yay
raftery.diag(bayesianOProbit$result$coefficients) #Yay
#Summarize results
summary(bayesianOProbit)

#Simpler model to test stats class surprise
bayesianSimple <- zelig(Likelihood~Treatment+StatsClass, model="oprobit.bayes", data=bayesData)
#Check for convergence
heidel.diag(bayesianSimple$result$coefficients) #Yay
raftery.diag(bayesianSimple$result$coefficients) #Yay
#Summarize results
summary(bayesianSimple)