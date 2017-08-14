library('ggplot2')
library('cowplot')
source('functions/mallet_helper_functions.R')

doc.topics <- mallet.doc.topics(topic.model, smoothed = F, normalized = F)
republican <- doc.topics[which(d$winner == "Republican"),]
democratic <- doc.topics[which(d$winner == "Democratic"),]
republican <- as.tibble(republican)
democratic <- as.tibble(democratic)

topic.words <- mallet.topic.words(topic.model, smoothed = T, normalized = T)

diff <- colMeans(democratic)-colMeans(republican)
dem.topics <- which((apply(democratic, 2, mean)-apply(republican, 2, mean))>0)
rep.topics <- which((apply(democratic, 2, mean)-apply(republican, 2, mean))<0)

prop.dem <- colMeans(democratic)/colMeans(republican)
prop.rep <- colMeans(republican)/colMeans(democratic)
props <- prop.dem
props[prop.dem<1] <- prop.rep[prop.dem<1]
props <- tibble(topic = 1:length(props), prop.diff = props)
topicnumbers <- props$topic[order(props$prop.diff, decreasing = T)][1:10]

##################################################

df.words <- lapply(topicnumbers, returnWTP, nwords = 8, topic.word.matrix = topic.words)
df.words <- do.call(rbind, df.words)

df.words$party <- NA
df.words$party[as.numeric(str_replace(df.words$topic, "Topic ", "")) %in% dem.topics] <- "Democratic"
df.words$party[as.numeric(str_replace(df.words$topic, "Topic ", "")) %in% rep.topics] <- "Republican"

df.words$topic.num <- str_replace(df.words$topic, "Topic ", "")

df.words <- merge(df.words, props, by.x = "topic.num", by.y = "topic", all.x = T)

df.words$topic <- str_c(df.words$topic, " (", round(df.words$prop.diff, 2), " times more)")

# Word-topic-probability plot
partisanTopics <- df.words %>% ggplot(aes(words, weights, group = factor(topic), fill = party)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 2) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
  coord_flip() +
  ylab("") + xlab("") +
  scale_fill_manual(values = c("dodgerblue", "indianred"))
partisanTopics

ggsave(plotWTP_partisan, file = "paper/figures/partisanTopics.pdf", width = 8, height = 9)
