#This script uses the matched GSA/Census data to create a barplot
#describing the percentage of each state's population covered by .gov websites

setwd("/home/markus/Dropbox/4_RA/govWebsites")

load("data/govWebsitesVerifiedCensus.Rdata")

#population estimates for cities covered by GSA list
statesGSA <- data9[,c(2:9,30)]
#aggregate to state level
statesGSA <- aggregate(statesGSA[,1:8], by=list(statesGSA$STNAME), FUN = sum)
#read in Census data
statesCensus <- read.csv("data/sub-est2015_all.csv")
#keep only states
statesCensus <- subset(statesCensus, SUMLEV==40)
#merge
states <- merge(statesGSA,statesCensus,by.x="Group.1",by.y="STNAME")
states$coverage <- states$POPESTIMATE2015.x/states$POPESTIMATE2015.y

require(ggplot2)
require(dplyr)

#create barplot with states
g <- ggplot(states, aes(x = Group.1, y = coverage*100)) +
  geom_bar(stat="identity", width = 0.7) +
  labs(title = "", x = "", y = "Percent covered") +
  theme_minimal(base_size = 14) + 
  ylim(0,100) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(states$Group.1)))

#aggregate to country level
country <- states[,c(2:10,19:26)]
country <- apply(country,2,as.character)
country <- apply(country,2,as.numeric)
country <- aggregate(country, by=list(states$SUMLEV), FUN = sum)
country$Name <- "United States"
country$coverage <- country$POPESTIMATE2015.x/country$POPESTIMATE2015.y

#create barplot (i.e. just a single bar) for U.S. as a whole
g1 <- ggplot(country, aes(x = Name, y = coverage*100)) +
  geom_bar(stat="identity", width = 0.7) +
  labs(title = "", x = "", y = "") +
  theme_minimal(base_size = 14) + 
  ylim(0,100) + 
  theme(axis.text.x = element_blank(), plot.margin = unit(c(1,0,-2,0), "lines")) +
  coord_flip()

#use cowplot's plot_grid() to put the two together
require(cowplot)
plot_grid(g1,g, ncol=1, rel_heights=c(0.06, 1), rel_widths=c(1, 1), align = 'v')

ggsave("paper/figures/coverage_states.pdf", width = 7, height = 10)
