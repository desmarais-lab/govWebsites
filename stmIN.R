library('stm')
library('tidyr')
library('ggplot2')
library('scales')
library('stringr')

load("rfiles/d.Rdata")

#get the data ready for preprocessing
docsIN <- d$doc
meta <- subset(d, select = c('Name', 'winner'))
processed <- textProcessor(docsIN, metadata = meta)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

#the stm package has its own preprocessing function
out <- prepDocuments(processed$documents, processed$vocab,
                     processed$meta, lower.thresh = 15)

#Number of topics
numtopics <- 60

#Train the model
stmFit <- stm(documents = out$documents, vocab = out$vocab,
              K = numtopics, prevalence =~ winner,
              max.em.its = 75, data = out$meta,
              init.type = "Spectral")

#Estimate effects from the model outputs
sims <- 1000

#re-estimate the effects, this time do 1000 draws from the posterior
prep <- estimateEffect(formula = 1:numtopics ~ winner, 
                       stmobj = stmFit,
                       meta = out$meta, 
                       uncertainty = "Global",
                       nsims = sims)

#save the results, since training takes some time
#the results are so large, we only retain the
#objects necessary for the rest of the analysis
rm(list = ls()[!ls()%in% c("stmFit", "prep", "out", "numtopics")])
save.image('rfiles/stmSessionIN_Party.RData')
load('rfiles/stmSessionIN_Party.RData')

#make a dataframe to store the results in
df <- data.frame(coef = rep(0, numtopics), sig = rep(FALSE, numtopics),
                 lower = rep(0, numtopics), upper = rep(0, numtopics),
                 topic = 1:numtopics)

conf_level <- 1 #in percent
conf_level <- conf_level/100 #as a fraction
conf_level <- conf_level/2 #two-sided

conf_lower <- 0 + conf_level
conf_upper <- 1 - conf_level

for(j in 1:numtopics){
  
  coefs <- list()
  
  for(i in 1:sims){
    coefs[[i]] <- prep$parameters[[j]][[i]]$est[2]
  }
  
  coefs <- do.call(c, coefs)
  coefs <- quantile(coefs, c(conf_lower, conf_upper))
  
  df$lower[j] <- coefs[1]
  df$upper[j] <- coefs[2]
  df$sig[j] <- all(coefs>0) | all(coefs<0)
  df$coef[j] <- mean(coefs)
  
}

#number of significant topics
sum(df$sig)

#sort data frame by coefficient size
df <- df[order(df$coef, decreasing = T),]

#get the 5 topics with the highest/lowest significant coefficients
repTopics <- df$topic[df$sig==T][1:5]
demTopics <- rev(df$topic[df$sig==T][(nrow(df[df$sig==T,])-4):nrow(df[df$sig==T,])])

#sort data frame by topic number
df <- df[order(df$topic, decreasing = F),]

#number of top words to be shown
nTopWords <- 10

#topicLabels
repWords <- labelTopics(stmFit, n = nTopWords)
repWords <- repWords$prob[repTopics,]
repWords <- c(apply(repWords, 1, c))

demWords <- labelTopics(stmFit, n = nTopWords)
demWords <- demWords$prob[demTopics,]
demWords <- c(apply(demWords, 1, c))

#create data frame for xtable
xt <- data.frame(demTopic = rep(demTopics, each = nTopWords),
                 demTopicCoeff = round(rep(df$coef[demTopics], each = nTopWords), 3),
                 Democratic = demWords,
                 repTopic = rep(repTopics, each = nTopWords),
                 repTopicCoeff = round(rep(df$coef[repTopics], each = nTopWords), 3),
                 Republican = repWords)

#save results
save.image('rfiles/stmIN.RData')

library("xtable")

strCaption <- "Top 50 Democratic and Republican words (Indiana), according to STM. 
             The words are the top words for the most Democratic/Republican topic, determined
             by the size (and significance) of the coefficient of the party covariate."

xt <- print(xtable(xt, digits = 3,#digits = c(0, 3, 0, 3, 1, 0, 6), # first zero "represents" row numbers which we skip later
                   align = "lccr|ccr",  # align and put a vertical line (first "l" again represents column of row numbers)
                   caption = strCaption, label = "tabSTMIN"),
            size = "footnotesize", #Change size; useful for bigger tables "normalsize" "footnotesize"
            include.rownames = FALSE, #Don't print rownames
            include.colnames = FALSE, #We create them ourselves
            caption.placement = "top", #"top", NULL
            hline.after=NULL, #We don't need hline; we use booktabs
            floating=TRUE, # whether \begin{Table} should be created (TRUE) or not (FALSE)
            sanitize.text.function = force, # Important to treat content of first column as latex function
            add.to.row = list(pos = list(-1,
                                         seq(nTopWords, (nrow(xt)-nTopWords), by = nTopWords),
                                         nrow(xt)),
                              command = c(paste("\\toprule \n",  # NEW row
                                                #"\\multicolumn{2}{c}{} & \\multicolumn{2}{c}{\\textbf{colLabel1}} & \\multicolumn{2}{c}{colLabel2} \\\\\n",
                                                "\\multicolumn{3}{c}{\\textbf{Democratic}} & \\multicolumn{3}{c}{\\textbf{Republican}} \\\\\n",
                                                "\\cmidrule(l){1-3} \\cmidrule(l){4-6}\n",
                                                " Topic & Coefficient & Word & Topic & Coefficient & Word \\\\\n", # NEW row 
                                                "\\midrule \n"
                              ),
                              paste("\\cmidrule(l){1-3} \\cmidrule(l){4-6}\n" # we may also use 'pos' and 'command' to add a midrule
                              ),
                              paste("\\bottomrule \n"  # paste is used as it is more flexible regarding adding lines
                              )
                              )
            )
)

writeLines(xt, 
           con = 'paper/tables/stmTopWordsIN.tex')
