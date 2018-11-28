# TOPIC MODELLING OF VITAEPRO
# Peer Christensen
# Oktober 2018

# Sentiment analysis
# How are ratings distributed?
# how are sentiments distributed
# How are sentiment and rating distributed over time?
# relationship sentiment ~ rating

# topic modelling

library(trustpilotR)
library(tidyverse)
library(happyorsad)
library(tidytext)
library(tm)
library(topicmodels)
library(widyr)
library(ggraph)
library(igraph)
library(gridExtra)
library(lubridate)
library(magrittr)
library(ggridges)
library(jtools)

#df <- get_reviews("https://dk.trustpilot.com/review/vitaepro.dk",company = "VitaePro")

#write_csv(df, "vitaepro_data.csv")

#NB! 
# Sys.setlocale(locale = 'en_US.UTF-8')

my_theme <- function() {
  theme_apa(legend.pos = "none") +
    theme(
      panel.background = element_rect(fill = "black", colour = "black"),
      plot.background = element_rect(fill = "black", colour = "black"),
      plot.margin = margin(1, 1, 1, 1, "cm"),
      panel.border = element_blank(),            # facet border
      strip.background = element_blank(),        # facet title background
      text = element_text(colour="white"),
      axis.text = element_text(colour="white", size=12),
      plot.title = element_text(size=24)
    ) 
}

########## LOAD, CLEAN, ADD #################

df <- read_csv("vitaepro_data.csv")

# too few data points before july 2017, subset
df %<>% 
  filter(date(time) >= "2017-07-01")

# lower case, add sentiments
df %<>%
  mutate(review = tolower(review)) %>%              
  mutate(sentiment = map_int(df$review,happyorsad,"da"))

# check for errors
df %>% 
  filter(rating <= 2, sentiment > 1) %>%
  write_csv("evt_fejlratings.csv")

# remove stopwords
df %<>%
  mutate(review = removeWords(review, c("så","vitaepro","vitae","vitapro", "vita", "kan","få","får","fik", stopwords("danish"))))

######### RATINGS #####################

# ratings
df %>% 
  group_by(rating) %>%
  count() %>%
  ggplot(aes(x=factor(rating),y=n,fill=factor(rating))) + 
  geom_col() +
  scale_fill_manual(values = c("#FF3722","#FF8622","#FFCE00","#73CF11","#00B67A"))

# rating over time: points
#df %>% 
#  ggplot(aes(time,rating)) + 
#  geom_jitter() + 
#  geom_smooth()

# aggregated by month: line

df %>%
  group_by(month=floor_date(time, "month")) %>% 
  summarise(`avg. rating` = mean(rating)) %>%
  ggplot(aes(month,`avg. rating`)) +
  geom_line() +
  geom_point(aes(month,`avg. rating`,colour=`avg. rating`), size = 3) +
  scale_colour_gradient(low="#FFCE00", high = "#00B67A") +
  ylim(0,5) +
  geom_smooth(colour="snow", alpha=.3)

# stacked area plot, count

df %>% 
  group_by(quarter = floor_date(time, "quarter")) %>%
  count(rating) %>%
  ungroup() %>%
  complete(quarter, nesting(rating), fill = list(n =0)) %>%
  mutate(order = rev(row_number())) %>%
  ggplot(aes(x=quarter, y = n, fill = factor(rating), group=reorder(rating,order))) +
  geom_area() +
  scale_fill_manual(values = c("#FF3722","#FF8622","#FFCE00","#73CF11","#00B67A"))


# stacked area plot, prop

df %>% 
  group_by(quarter = floor_date(time, "quarter")) %>%
  count(rating) %>%
  ungroup() %>%
  complete(quarter, nesting(rating), fill = list(n =0)) %>%
  group_by(quarter) %>%
  mutate(prop = n / sum(n)) %>%
  mutate(order = rev(row_number())) %>%
  ggplot(aes(x=quarter, y = prop, fill = factor(rating), group=reorder(rating,order))) +
  geom_area() +
  scale_fill_manual(values = c("#FF3722","#FF8622","#FFCE00","#73CF11","#00B67A"))

######## SENTIMENTS #############

# eksempler:
cat("Det er et fantastisk produkt."," - score:",
    happyorsad("Det er et fantastisk produkt"))
cat("Elendig service, og pillerne smager dårligt."," - score:",
    happyorsad("Elendig service, og pillerne smager dårligt."))

# density plot
df %>% 
  ggplot(aes(x=sentiment)) + 
  geom_density(size=1) +
  geom_vline(xintercept = median(df$sentiment), 
             colour = "indianred", linetype = "dashed", size =1) +
  ggplot2::annotate("text", x = 15, y = 0.06, 
                    label = paste("median = ", median(df$sentiment)), colour = "indianred") +
  xlim(-20,20)

df %>%
  ggplot(aes(x=sentiment,y=factor(rating),fill=factor(rating))) +
  geom_density_ridges(rel_min_height = 0.005) +
  scale_fill_manual(values = c("#FF3722","#FF8622","#FFCE00","#73CF11","#00B67A"))

# sentiments
df %>% ggplot(aes(sentiment)) + 
  geom_histogram(stat="count") +
  my_theme()

# sentiments over time: points
#df %>% ggplot(aes(time,sentiment)) + 
#  geom_jitter() + 
#  geom_smooth()

# aggregated by month: line
monthly_avg <- df %>%
  group_by(month=floor_date(time, "month")) %>% 
  summarise(`avg. sentiment` = mean(sentiment)) 

monthly_avg %>% ggplot(aes(month,`avg. sentiment`)) +
  geom_line() +
  ylim(0,5) +
  geom_smooth()

##################################################################

# TOPIC MODELLING

# positive
df_pos <- df %>%
  filter(rating > 3, sentiment > 1) %>%
  select(-sentiment,-company,-time,-rating) %>%
  unnest_tokens(word, review)

# dtm
words_pos <- df_pos %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

reviewDTM_pos <- words_pos %>%
  cast_dtm(id, word, n)

#reviewDTM_pos <- removeSparseTerms(reviewDTM_pos, 0.95)

reviewLDA_pos <- LDA(reviewDTM_pos, k = 4, control = list(seed = 347))

topics_pos <- tidy(reviewLDA_pos, matrix = "beta")

topTerms_pos <- topics_pos %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(order = rev(row_number())) 

plot_pos <- topTerms_pos %>%
  ggplot(aes(order, beta)) +
  ggtitle("Positive review topics") +
  geom_col(show.legend = FALSE, fill = "steelblue") +
  scale_x_continuous(
    breaks = topTerms_pos$order,
    labels = topTerms_pos$term,
    expand = c(0,0))+
  facet_wrap(~ topic,scales="free") +
  coord_flip(ylim=c(0,max(topTerms_pos$beta))) +
  theme(axis.title=element_blank()) + 
  my_theme()
  
plot_pos

ggsave("plot_pos.png")

# negative
df_neg <- df %>%
  filter(rating <= 3, sentiment < 0) %>%
  select(-sentiment,-company,-time,-rating) %>%
  unnest_tokens(word, review)

# dtm
words_neg <- df_neg %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

reviewDTM_neg <- words_neg %>%
  cast_dtm(id, word, n)

reviewLDA_neg <- LDA(reviewDTM_neg, k = 4, control = list(seed = 347))

topics_neg <- tidy(reviewLDA_neg, matrix = "beta")

topTerms_neg <- topics_neg %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(order = rev(row_number())) 

plot_neg <- topTerms_neg %>%
  ggplot(aes(order, beta)) +
  ggtitle("Negative review topics") +
  geom_col(show.legend = FALSE, fill = "indianred") +
  scale_x_continuous(
    breaks = topTerms_neg$order,
    labels = topTerms_neg$term,
    expand = c(0,0))+
  facet_wrap(~ topic,scales="free") +
  coord_flip(ylim=c(0,max(topTerms_neg$beta))) +
  theme(axis.title=element_blank()) +
  my_theme()

plot_neg
ggsave("plot_neg.png")

word_pairs_pos <- df_pos %>% 
  pairwise_count(word, id, sort = TRUE)

word_pairs_neg <- df_neg %>%
  pairwise_count(word, id, sort = TRUE)

set.seed(611)

pairs_plot_pos <- word_pairs_pos %>%
  filter(n >= 6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = .9, edge_width = n), edge_colour = "steelblue") +
  ggtitle("Positive word pairs") +
  geom_node_point(colour="snow",size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines"),colour="white") +
  my_theme() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

pairs_plot_neg <- word_pairs_neg %>%
  filter(n >= 2) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = .9, edge_width = n), edge_colour = "indianred") +
  ggtitle("Negative word pairs") +
  geom_node_point(colour="snow",size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines"),colour="white") +
  my_theme() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

grid.arrange(pairs_plot_pos, pairs_plot_neg, ncol = 2)
