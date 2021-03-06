# Load packages
library("ggplot2")
library("dplyr")

# Load data
load("Data/alldata.RData")

# Get aggregated data 
#alldata <- alldata %>% group_by(date, week) %>%
#  summarise(Cases = sum(Cases), pop = sum(Population), o104wk = max(o104wk))

# Plot whole time series
pdf("Figures/StratifiedTimeSeries.pdf", width=6, height=4, paper='special') 
ggplot(data = alldata) +
  geom_bar(aes(x = date, y = Cases, fill=factor(o104wk)),stat="identity") +
  xlab("Time (week)") + ylab("No. cases reported")+
  scale_y_continuous(expand = c(0,0), limits = c(0, 3000)) +
  theme_bw() +
  #geom_vline(xintercept = as.numeric(alldata$date[which(c(0, diff(alldata$o104wk)) != 0) - c(1,0)]),
  #           linetype = "dashed", colour = "red") +
  facet_grid(Sex~Age) +
  scale_fill_brewer(type="qual",palette=2) +
  guides(fill=FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
dev.off()



