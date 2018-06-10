rm(list = ls())
load("rfiles/stmSession_sim.rdata")
rm(list = ls()[!ls()%in%c("stmFit", "prep")])

#population
varname <- "B01001_001E"

prep2 <- summary(prep)
pvalues <- list()
pointestimates <- list()
for(i in 1:length(prep2$tables)){
  pvalues[[i]] <- prep2$tables[[i]][which(rownames(prep2$tables[[i]])==varname),][4]
  pointestimates[[i]] <- prep2$tables[[i]][which(rownames(prep2$tables[[i]])==varname),][1]
}
sigtopics <- which(do.call(c, pvalues)<0.001)
bigeffects <- do.call(c, pointestimates)
bigeffects <- data.frame(coef = bigeffects, coefAbs = abs(bigeffects), topic = 1:length(bigeffects))
bigeffects <- bigeffects[order(bigeffects$coefAbs, decreasing = T),]
bigeffects <- bigeffects[bigeffects$topic%in%sigtopics,]
bigeffects <- bigeffects$topic[1:5]

pdf(file = "paper/figures/stm_effect_pop.pdf")
plot(prep, varname, model = stmFit, method = "continuous", topics = bigeffects, xlab = "Population", labeltype = "frex", n = 5)
dev.off()

# topWords <- labelTopics(stmFit, n = 5)
# topWords <- topWords$frex
# topWords[bigeffects,]

#income
varname <- "B19013_001E"

pvalues <- list()
pointestimates <- list()
for(i in 1:length(prep2$tables)){
  pvalues[[i]] <- prep2$tables[[i]][which(rownames(prep2$tables[[i]])==varname),][4]
  pointestimates[[i]] <- prep2$tables[[i]][which(rownames(prep2$tables[[i]])==varname),][1]
}
sigtopics <- which(do.call(c, pvalues)<0.001)
bigeffects <- do.call(c, pointestimates)
bigeffects <- data.frame(coef = bigeffects, coefAbs = abs(bigeffects), topic = 1:length(bigeffects))
bigeffects <- bigeffects[order(bigeffects$coefAbs, decreasing = T),]
bigeffects <- bigeffects[bigeffects$topic%in%sigtopics,]
bigeffects <- bigeffects$topic[1:5]

pdf(file = "paper/figures/stm_effect_income.pdf")
plot(prep, varname, model = stmFit, method = "continuous", topics = bigeffects, xlab = "Median income", labeltype = "frex", n = 5)
dev.off()