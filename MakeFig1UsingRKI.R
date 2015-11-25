# Load packages
library("data.table")
library("ggplot2")
library("dplyr")

# Read data
alldata <- fread("Data/alldata.csv", data.table = FALSE)

# Remove first column
alldata$V1 <- NULL

# Get aggregated data 
alldata <- alldata %>% group_by(season, week) %>%
  summarise(y = sum(Cases), pop = sum(Population), o104wk = max(o104wk))

# Get incidence (per 100,000 population)
alldata$incidence <- alldata$y*100000/alldata$pop

# Fix ymin, ymax and ymed
alldata.tmp <- alldata %>% filter(season != 2010) %>% group_by(week) %>%
  summarise(ymax = max(incidence), ymin = min(incidence), ymed = median(incidence))

# Join to whole data
alldata <- left_join(alldata, alldata.tmp)

# Filter out and use season 2010
alldata2010 <- alldata %>% filter(season == 2010)

# Some annoying ugly things to get tick marks for all weeks. 
# (Sugestions for doing this nicer are very welcome)
breaks <- 1:52
ind <- which(breaks %in% c(1, 11, 21, 33, 43, 52))
labels <- vector(length = length(breaks))
labels[ind] <- paste0("W", c(31, 41, 51, 11, 21, 30))
labels[-ind] <- ""

# Do the plotting in way to many lines
# ADD LEGEND! Colours might be nice. Maybe write "Outbreak period" and
# "Sprout warning issued".
 
