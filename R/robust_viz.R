## Visualization of rankings for top-n selections
n_top_candidates <- 50
non_robust <- TRUE

## Subset top-n trees from candidates_rank and use sperm plot
candidates_choices %>%
  select(Ortet) %>%
  filter(row_number() <= n_top_candidates) -> id_top_candidates
