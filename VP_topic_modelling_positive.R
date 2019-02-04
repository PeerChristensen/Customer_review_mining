# Project: Text mining VitaePro reviews
# Author: Peer Christensen
# Date: December, 2018
# Task: Topic Modelling - positive

########## CONTENTS #######################################

# 1. Prepare data
# 2. Build models
# 3. Evaluation
# 4. Plot Final model

library(tidyverse)
library(tidytext)
library(magrittr)
library(tm)
library(ggrepel)
library(stm)
library(furrr)
library(ggrepel)
library(RColorBrewer)
library(gganimate)

########## 1. Prepare data ################################

df <- read_csv("vitaepro_dataCombined.csv")

my_stopwords <- c("så","vitaepro","pro","vita","1","d","2","3","venlig","danmark","vitae","vitapro", "vita", "kan",
                  tm::stopwords("danish"))

df %<>%
  filter(rating >= 4) %>%
  unnest_tokens(word, review) %>%
  mutate(word = removeWords(word,my_stopwords)) %>%
  add_count(word)                          %>%
  filter(n > 1,word != "") %>%                
  select(-n, -sentiment,-time,-company,-rating)

########## 2. Build models ################################

dfSparse <- df             %>%
  count(id, word)          %>%
  cast_sparse(id, word, n)

plan("default")
start_time_stm <- Sys.time()

nTopics <- seq(2,15)

many_models_stm <- data_frame(K = nTopics) %>%
  mutate(topic_model = future_map(K, ~stm(dfSparse, K = ., verbose = TRUE)))

end_time_stm <- Sys.time() # 6.25 mins

########## 2. Evaluate models #############################

heldout <- make.heldout(dfSparse)

k_result <- many_models_stm %>%
  mutate(exclusivity        = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, dfSparse),
         eval_heldout       = map(topic_model, eval.heldout, heldout$missing),
         residual           = map(topic_model, checkResiduals, dfSparse),
         bound              = map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact              = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound             = bound + lfact,
         iterations         = map_dbl(topic_model, function(x) length(x$convergence$bound))) %>%
  mutate(mean_semantic_coherence = map(semantic_coherence,mean) %>% unlist(),
         mean_exclusivity = map(exclusivity,mean) %>% unlist())

# DIAGNOSTIC PLOTS

k_result %>%
  transmute(K,
            `Lower bound`         = lbound,
            Residuals             = map_dbl(residual, "dispersion"),
            `Semantic coherence`  = map_dbl(semantic_coherence, mean),
            Exclusivity           = map_dbl(exclusivity, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x        = "K (number of topics)",
       y        = NULL)

# SEMANTIC COHERENCE AND EXCLUSIVITY

excl_sem_plot <- k_result                    %>%
  select(K, exclusivity, semantic_coherence) %>%
  #filter(K %in% seq(2,15))                   %>%
  unnest()                                   %>%
  mutate(K = as.factor(K))                   %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 5, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity") 

excl_sem_plot

# ANIMATED 

anim_plot <- excl_sem_plot +
  labs(title = 'K: {round(frame_time,0)}') +
  transition_time(as.numeric(K)) +
  ease_aes('linear')
animate(anim_plot, nframes = 14, fps = 0.5)

k_result %>% ggplot(aes(x=mean_semantic_coherence, y = mean_exclusivity,
                        label=K)) +
  geom_point(size=3) +
  geom_text_repel(size=5) +
  geom_smooth()

########## 1. Plot final model ############################

# SELECT MODEL

topic_model_stm <- k_result %>% 
  filter(K ==6)             %>% 
  pull(topic_model)         %>% 
  .[[1]]

topic_model_stm

# EXPLORE MODEL

cols = brewer.pal(9, "Greens")[4:9]
cols = colorRampPalette(cols)(10)

# BETA PLOT

td_beta <- tidy(topic_model_stm)

top_terms <- td_beta %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(order = rev(row_number()))

top_terms %>%
  ggplot(aes(order, beta,fill = rev(factor(topic)))) +
  #ggtitle("Positive review topics") +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(
    breaks = top_terms$order,
    labels = top_terms$term,
    expand = c(0,0)) +
  facet_wrap(~ topic,scales="free") +
  coord_flip(ylim=c(0,max(top_terms$beta))) +
  labs(x="",y=expression(beta)) +
  theme(axis.title=element_blank()) + 
  theme_minimal() + 
  theme(axis.text  = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 30,b=10)),
        axis.title.y = element_text(margin = margin(r = 30,l=10)),
        panel.grid = element_blank(),
        strip.text.x = element_text(size=16)) +
  scale_fill_manual(values=cols)

ggsave("beta_pos_plotUpdate.png")

# BETA + GAMMA

top_terms <- td_beta  %>%
  arrange(beta)       %>%
  group_by(topic)     %>%
  top_n(8, beta)      %>%
  arrange(-beta)      %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

td_gamma <- tidy(topic_model_stm, matrix = "gamma",
                 document_names = rownames(dfSparse))

gamma_terms <- td_gamma              %>%
  group_by(topic)                    %>%
  summarise(gamma = mean(gamma))     %>%
  arrange(desc(gamma))               %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  #top_n(15, gamma)      %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  #geom_text(hjust = 1.05, vjust=0, size = 3, family = "Helvetica") +
  geom_text(hjust = 0, nudge_y = 0.0100, size = 6,
            family = "Helvetica") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.6),
                     labels = scales::percent_format()) +
  labs(x = NULL, y = expression(gamma)) +
  theme_minimal() + 
  theme(axis.text  = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 30,b=10)),
        axis.title.y = element_text(margin = margin(r = 30,l=10)),
        panel.grid = element_blank()) +
  scale_fill_manual(values=cols)

ggsave("beta_gamma_pos_plot.png")
