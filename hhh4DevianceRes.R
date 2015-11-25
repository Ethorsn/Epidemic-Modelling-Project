library("stats")
library("dplyr")
library("reshape2")
library("ggplot2")
library("surveillance")
library("gridExtra")
library("cowplot")

#Load models
load("Models/hhh4Model.RData")
load("Data/AnscombeResid.Rdata")
# extracting residuals
Deviance.resid.ASIS <- residuals(hhh4Model4.wo.strat.od,"deviance")
colNam <- colnames(Deviance.resid.ASIS)

PearsonResids <- function(x){
  # function which calculates pearson residuals for negative binomial
  # assuming that the overdisp is not stratified.
  
  # Calculated according to Table A.9 "Pearson residuals" from 
  # "Generalized Linear Models and Extensions, Second Edition"
  
  # x: hhh4 object of NegBin family and non-stratified overdispersion.  
  overdisp <- summary(x)$fixef["overdisp","Estimate"]
  # Remove first row which is used for AR fitting
  Observed <- x$stsObj@observed[-1,] 
  fitted <- x$fitted.values
  # Save colnames
  Colnames <- colnames(Observed) 
  pearson <- (Observed - fitted)/(fitted+overdisp*fitted^2)^(1/2)
  colnames(pearson) <- Colnames
  return(pearson)
}
#Pearson.resid.ASIS <- PearsonResids(hhh4Model4.wo.strat.od)

# Transforming AsIs object into list
Deviance.resid.ASIS <- lapply(1:16, function(i) Deviance.resid.ASIS[, i])
Deviance.resid.ASIS <- do.call("cbind", Deviance.resid.ASIS) %>% as.data.frame()
# For Pearson
#Pearson.resid.ASIS <- lapply(1:16, function(i) Pearson.resid.ASIS[, i])
#Pearson.resid.ASIS <- do.call("cbind", Pearson.resid.ASIS) %>% as.data.frame()
# Colnames for melt function.
colnames(Deviance.resid.ASIS) <- colNam
# Same for Pearson 
#colnames(Pearson.resid.ASIS) <- colNam

# To long format
resid.long <- melt(Deviance.resid.ASIS,value.name = "res")
#resid.long2 <- melt(Pearson.resid.ASIS,value.name = "res")
# use the variable name to extract factors.
trans <- as.character(resid.long$variable) %>%
  strsplit("_") 
trans <- do.call("rbind",trans)

names(ans.resid) <- NULL


# append and remove unneccesary
resid.long$Sex <- trans[,1]
resid.long$Age <- trans[,2]
resid.long$Type <- as.factor("Deviance")
resid.long$variable <- NULL


#index used for plotting.
resid.long$index <- 1:(dim(resid.long)[1]/2)
###############################
# Anscombe residuals
# append the Values. 
resid.long2 <- data.frame('res'=ans.resid)
resid.long2$Sex <- trans[,1]
resid.long2$Age <- trans[,2]
resid.long2$Type <- as.factor("Anscombe")
resid.long2$index <- 1:(dim(resid.long)[1]/2)

#plot.data.anscombe$Type <- as.factor("Anscombe")
#plot.data.anscombe <- plot.data.anscombe %>% 
#  arrange(desc(Sex))
#plot.data.anscombe$index <- seq(1,dim(plot.data.anscombe)[1]/2,by=1)

p1 <- ggplot(resid.long) +
  geom_point(aes(y=res,x=index,color=Age))+
  xlab("Observation") +
  ylab("Residuals") +
  theme_bw() +
  facet_grid(.~Sex, scales = "free_y") +
  labs(title="Deviance") +
  scale_color_brewer("Age", palette = 2, type = "div") +
  theme(legend.position="none")

# Not using these anscombe residuals. 
p2 <- ggplot(resid.long2) +
  geom_point(aes(y=res,x=index,color=Age))+
  xlab("Observation") +
  ylab("Residuals") +
  theme_bw() +
  facet_grid(.~Sex, scales = "free_y") +
  labs(title="Anscombe") +
  scale_color_brewer("Age", palette = 2, type = "div")+
  theme(legend.position="bottom", legend.text=element_text(size=7))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p2)

pdf("Figures/Residuals.pdf",width=7, height=5, paper="special")
grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                         p2 + theme(legend.position="none"),
                         nrow=1),
             mylegend, nrow=2,heights=c(10, 1))
dev.off()

##################################################
# Check normality.
pNorm <- ggplot(resid.long2) + 
  geom_histogram(aes(x=res,y=..density.., fill=TRUE),alpha=0.8, binwidth=0.3) +
  stat_function(fun = dnorm, colour = "red", size=0.8) +
  scale_fill_brewer(type="qual",palette = 3) +
  theme_bw() +
  theme(legend.position="none")
# ok for anscombe?
pQQ <- qplot(sample=resid.long2$res, stat="qq") + 
  geom_abline(slope=1, intercept=0)  +
  theme_bw() 

pdf("Figures/AnscombeHistQQ.pdf",width=7, height=5, paper="special")
grid.arrange(arrangeGrob(pNorm,pQQ,nrow=1), nrow=2,heights=c(10, 1))
dev.off()
