

word_pairs_pos <- df_pos %>% 
  pairwise_count(word, Name, sort = TRUE)

word_pairs_neg <- df_neg %>%
  pairwise_count(word, Name, sort = TRUE)

set.seed(611)

pairs_plot_pos <- word_pairs_pos %>%
  filter(n >= 140)               %>%
  graph_from_data_frame()        %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "steelblue") +
  ggtitle("Positive word pairs") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  my_theme() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

pairs_plot_neg <- word_pairs_neg %>%
  filter(n >= 80)                %>%
  graph_from_data_frame()        %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "indianred") +
  ggtitle("Negative word pairs") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  my_theme() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

grid.arrange(pairs_plot_pos, pairs_plot_neg, ncol = 2)