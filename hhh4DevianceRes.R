library("stats")
library("dplyr")
library("reshape2")
library("ggplot2")
library("surveillance")
#Load models
load("Models/hhh4Model.RData")

# extracting residuals
Deviance.resid.ASIS <- residuals(hhh4Model4.wo.strat.od,"deviance")
#H%*%melt(hhh4Model4.wo.strat.od$stsObj@observed)$value - fitted.values(Deviance.resid.ASIS)
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
resid.long$Type <- as.factor("Deviance")
# index used for plotting.
resid.long$index <- 1:(dim(resid.long)[1]/2)

###############################
# Anscombe residuals (EXPERIMENTAL!)
# Data.frame to get model.matrix

#pdf("Figures/AnscombeResidExperimental.pdf", width = 6, height = 4, paper = 'special')
#ggplot(data = plot.data, aes(y = res, x = 1:length(res), col = Age:Sex)) +
#  geom_point() +
#  xlab("Observation") + 
#  ylab("Anscombe residuals") +
#  theme_bw()
#dev.off()




min(H%*%y - mu)
