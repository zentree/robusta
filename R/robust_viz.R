library(forcats)

## Visualization of rankings for top-n selections
n_top_candidates <- 30

## Subset top-n trees from candidates_rank and use sperm plot
id_top_candidates <- candidates_choices$Ortet[1:n_top_candidates]

ranks_top_candidates <- candidates_rank[rownames(candidates_rank) %in% id_top_candidates,]

ranking_distrib <- data.frame(ortet = rep(rownames(ranks_top_candidates), each = nsim),
                              rankings = as.vector(t(ranks_top_candidates)))
ranking_distrib$ortet <- with(ranking_distrib, fct_relevel(ortet, id_top_candidates))

# Plot of top candidates, including full distribution of results
ggplot(ranking_distrib, aes(x = ortet, y = rankings)) +
  geom_point(size = 0.5, alpha = 0.2) +
  stat_summary(fun.y = 'median', geom = 'point', size = 1, colour = 'red') +
  geom_hline(yintercept = threshold, colour = 'blue') +
  scale_y_reverse() + theme_minimal() +
  labs(x = 'Ortet code', y = 'Rankings over 1,000 scenarios') +
  theme(axis.text.x=element_text(angle=-90))

ggsave('./reports/top-ranking.pdf')

# Zooming in top half of distribution for top candidates
ggplot(ranking_distrib, aes(x = ortet, y = rankings)) +
  geom_point(size = 0.5, alpha = 0.2) +
  stat_summary(fun.y = 'median', geom = 'point', size = 1, colour = 'red') +
  scale_y_reverse(limits = c(threshold/2, 0)) + theme_minimal() +
  labs(x = 'Ortet code', y = 'Rankings over 1,000 scenarios') +
  theme(axis.text.x=element_text(angle=-90))

ggsave('./reports/top-ranking-zoomed.pdf')
