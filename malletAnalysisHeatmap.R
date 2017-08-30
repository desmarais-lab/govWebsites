library('reshape2')
library('ggplot2')
library('cowplot')

doc.topics <- mallet.doc.topics(topic.model, smoothed = T, normalized = T)
republican <- doc.topics[which(d$winner == "Republican"),]
democratic <- doc.topics[which(d$winner == "Democratic"),]
republican <- as.tibble(republican)
democratic <- as.tibble(democratic)

democratic.melted <- melt(as.matrix(democratic))
names(democratic.melted) <- c("Document", "Topic", "Weight")
heatmap_democratic <- ggplot(democratic.melted, aes(x = Topic, y = Document, fill = Weight)) + 
  geom_tile() + 
  scale_y_reverse() +
  theme(axis.text.x = element_blank()) +
  ggtitle("Democratic")

republican.melted <- melt(as.matrix(republican))
names(republican.melted) <- c("Document", "Topic", "Weight")
heatmap_republican <- ggplot(republican.melted, aes(x = Topic, y = Document, fill = Weight)) + 
  geom_tile() + 
  scale_y_reverse() +
  theme(axis.text.x = element_blank()) +
  ggtitle("Republican")

heatmaps_g <- plot_grid(heatmap_democratic, heatmap_republican, nrow = 2)
heatmaps_g

ggsave(heatmaps_g, file = str_c("./paper/figures/heatmaps_weights.png"), width=5, height=8)
