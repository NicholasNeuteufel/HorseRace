#Not used in the paper as of Nov. 2015

bayesData <- read.csv(file="gistStuff.csv", h=T) #Formerly known as `final'
set.seed(12) #reproducibility
bayesData$Treatment <- as.factor(bayesData$Treatment)

#Narrow down bayesData
library(dplyr)
dplyr::select(bayesData, StatsClass, HorseRaceExposure, 
       PollTrust, PresentationPreference,
       Treatment, Likelihood)
bayesData$Likelihood <- as.character(bayesData$Likelihood)
bayesData$Likelihood[bayesData$Likelihood=="a"] <- 1
bayesData$Likelihood[bayesData$Likelihood=="b"] <- 2
bayesData$Likelihood[bayesData$Likelihood=="c"] <- 3
bayesData$Likelihood <- as.numeric(bayesData$Likelihood)

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_data <- list(N = 215,
                  K = 4, 
                  J = 3, 
                  y = bayesData$Likelihood, 
                  x = bayesData)

mcmc_oprobit <- stan(file="oprobit.stan", 
                     data=stan_data)
