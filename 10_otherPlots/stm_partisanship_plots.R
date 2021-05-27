library(stm)
library(ggplot2)

load("../07_topicModel/stmSession_sim_60.rdata")

topwords <- stm::labelTopics(stmFit, topics = NULL, n = 7, frexweight = 0.5)
topwords <- topwords$frex

# Republican
plt_data <- plot(prep, covariate = "party",
                 topics = c(20, 51, 2, 47, 19),
                 model = stmFit,
                 method = "difference",
                 cov.value1 = "Republican",
                 cov.value2 = "Democratic",
                 xlim = c(-.002, .008),
                 labeltype = "custom",
                 custom.labels = c('Energy (Topic 20)',
                                   'Fire protection (Topic 51)',
                                   'Vaccination (Topic 2)',
                                   'Sanitation (Topic 47)',
                                   'Zoning (Topic 19)'),
                 omit.plot = T)

df_plt_R <- data.frame(topic = plt_data$topics,
                       mean = unlist(plt_data$means),
                       lower = do.call(rbind, plt_data$cis)[,1],
                       upper = do.call(rbind, plt_data$cis)[,2],
                       labels = plt_data$labels,
                       topwords = apply(topwords[plt_data$topics,], 1, paste, collapse = " "))
paletteR <- c("red3")
ggplot(df_plt_R, 
       aes(y = mean, x = nrow(df_plt_R):1, color = "red")) + 
  geom_point(size=3) +
  geom_hline(yintercept=0, color = "grey92", linetype = 2, size = .7) +
  geom_errorbar(mapping=aes(ymin=lower, ymax=upper), size=1, width=0) + 
  coord_flip() +
  labs(y = "Estimated topic proportion for a Republican city", x = "", color = "Policy Area", shape = "Policy Area") +
  theme_classic() + 
  theme(legend.position = "none") +
  geom_text(aes(label = labels), size = 3.5, color = "black", hjust=-0.3) +
  geom_text(aes(label = topwords), size = 3.5, color = "black", alpha = 0.4, hjust=1.1) +
  scale_colour_manual(values=paletteR) +
  ylim(-0.011, 0.011) + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank())
ggsave("../paper/figures/Republican_topics.pdf", width = 9, height = 5)

# Democratic
plt_data_D <- plot(prep, covariate = "party",
                   topics = c(52, 6, 10, 29, 58, 45, 35, 55, 39),
                   model = stmFit,
                   method = "difference",
                   cov.value1 = "Republican",
                   cov.value2 = "Democratic",
                   xlim = c(-.002, .008),
                   labeltype = "custom",
                   custom.labels = c('Affordable housing (Topic 52)',
                                     'Race and gender (Topic 6)',
                                     'Employee benefits (Topic 10)',
                                     'Employee rights (Topic 29)',
                                     'Public finances (Topic 58)',
                                     'Public finances (Topic 45)',
                                     'Public finances (Topic 35)',
                                     'Public finances (Topic 55)',
                                     'Mixed-use zoning  (Topic 39)'),
                   omit.plot = T)

df_plt_D <- data.frame(topic = plt_data_D$topics,
                       mean = unlist(plt_data_D$means),
                       lower = do.call(rbind, plt_data_D$cis)[,1],
                       upper = do.call(rbind, plt_data_D$cis)[,2],
                       labels = plt_data_D$labels,
                       topwords = apply(topwords[plt_data_D$topics,], 1, paste, collapse = " "))

paletteD <- c("#0072B2")
ggplot(df_plt_D, 
       aes(y = mean, x = nrow(df_plt_D):1, color = "blue")) + 
  geom_point(size=3) +
  geom_hline(yintercept=0, color = "grey92", linetype = 2, size = .7) +
  geom_errorbar(mapping=aes(ymin=lower, ymax=upper), size=1, width=0) + 
  coord_flip() +
  labs(y = "Estimated topic proportion for a Republican city", x = "", color = "Policy Area", shape = "Policy Area") +
  theme_classic() + 
  theme(legend.position = "none") +
  geom_text(aes(label = labels), size = 3.5, color = "black", hjust=1.2) +
  geom_text(aes(label = topwords), size = 3.5, color = "black", alpha = 0.4, hjust=-0.1) +
  scale_colour_manual(values=paletteD) +
  ylim(-0.011, 0.011) + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank())
ggsave("../paper/figures/Democratic_topics.pdf", width = 9, height = 5)

# Both parties together
palette_combined <- c("#0072B2", "red3")
df_plt_combined <- rbind(df_plt_D, df_plt_R)
df_plt_combined$party <- c(rep("D", nrow(df_plt_D)), rep("R", nrow(df_plt_R)))
df_plt_combined$topwords_hjust <- c(rep(-0.1, nrow(df_plt_D)), rep(1.1, nrow(df_plt_R)))
df_plt_combined$labels_hjust <- c(rep(1.2, nrow(df_plt_D)), rep(-0.3, nrow(df_plt_R)))

ggplot(df_plt_combined, 
       aes(y = mean, x = nrow(df_plt_combined):1, color = party)) + 
  geom_point(size=3) +
  geom_hline(yintercept=0, color = "grey92", linetype = 2, size = .7) +
  geom_errorbar(mapping=aes(ymin=lower, ymax=upper, color = party), size=1, width=0) + 
  coord_flip() +
  labs(y = "Estimated topic proportion for a Republican city", x = "", color = "Policy Area", shape = "Policy Area") +
  theme_classic() + 
  theme(legend.position = "none") +
  geom_text(aes(label = labels, hjust=labels_hjust), size = 3.5, color = "black") +
  geom_text(aes(label = topwords, hjust=topwords_hjust), size = 3.5, color = "black", alpha = 0.4) +
  scale_colour_manual(values=palette_combined) +
  ylim(-0.011, 0.011) + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank())
ggsave("../paper/figures/Democratic_Republican_topics.pdf", width = 9, height = 10)
