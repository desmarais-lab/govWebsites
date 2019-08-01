library(stm)
library(data.table)
library(ggplot2)

set.seed(1)

load("out/stmSession_sim_60.rdata")

coh <- semanticCoherence(model = stmFit, documents = d_stm$documents, M = 10)
exc <- exclusivity(model = stmFit)

results <- data.table(topic = 1:numtopics,
                      coherence = coh,
                      exclusivity = exc)

ggplot(results, aes(coherence, exclusivity, label = topic)) + 
  geom_text() +
  theme_bw() +
  labs(x = "Semantic Coherence", y = "Exclusivity")
ggsave("../paper/figures/stm_60_coherence_exclusivity.pdf", width = 6, height = 4)
