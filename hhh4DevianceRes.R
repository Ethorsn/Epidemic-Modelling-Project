library("stats")
library("dplyr")
library("reshape2")
library("ggplot2")
#Load models
load("Models/hhh4Model.RData")

# extracting residuals
Deviance.resid.ASIS <- residuals(hhh4Model4.wo.strat.od,"deviance")
colNam <- colnames(Deviance.resid.ASIS)
# Transforming AsIs object into list
Deviance.resid.ASIS <- lapply(1:16, function(i) Deviance.resid.ASIS[, i])
Deviance.resid.ASIS <- do.call("cbind", Deviance.resid.ASIS) %>% as.data.frame()

# Colnames for melt function.
colnames(Deviance.resid.ASIS) <- colNam
# To long format
resid.long <- melt(Deviance.resid.ASIS)
# use the variable name to extract factors.
trans <- as.character(resid.long$variable) %>%
  strsplit("_") 
trans <- do.call("rbind",trans)
resid.long$Sex <- trans[,1]
resid.long$Age <- trans[,2]
#resid.long$Type <- as.factor("Deviance") Only plotting deviance.
# index.
resid.long$index <- 1:(dim(resid.long)[1]/2)

ggplot(resid.long) + 
  geom_point(aes(x=index, y=value, color=Age)) + 
  facet_grid(~Sex) +
  ylab("Deviance residual") +
  scale_color_brewer(palette=15)
  