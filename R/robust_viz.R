## Visualization of rankings for top-n selections
n_top_candidates <- 50

## Subset top-n trees from candidates_rank and use sperm plot
id_top_candidates <- candidates_choices$Ortet[1:n_top_candidates]

ranks_top_candidates <- candidates_rank[candidates_choices$Ortet %in% id_top_candidates,]
ranking_distrib <- data.frame(Ortet = rep(id_top_candidates, each = nsim),
                              rankings = matrix(ranks_top_candidates,
                                                nrow = nsim*n_top_candidates, ncol = 1, byrow = TRUE))

ggplot(ranking_distrib, aes(x = Ortet, y = rankings)) + geom_point(size = 0.5, alpha = 0.2)
