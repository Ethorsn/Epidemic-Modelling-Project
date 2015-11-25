
rm(list = ls())

load("Data/alldata.RData")
load("Models/NegBinModel.RData")
load("Models/hhh4Model.RData")

alldata$PredCases <- model.as.ba.Smooth$fitted.values

# Get aggregated data, (not using PopSmooth)
alldata <- alldata %>% group_by(date, week) %>%
  summarise(Cases = sum(Cases), pop = sum(Population), 
            o104wk = max(o104wk), PredCases = sum(PredCases))

# Add hhh4 model
alldata$hhh4PredCases <- NA
alldata$hhh4PredCases[-1] <- rowSums(hhh4Model4.wo.strat.od$fitted.values)
