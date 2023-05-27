library(tidyverse)
library(tidytext)
library(quanteda)
library(quanteda.dictionaries)
library(quanteda.sentiment)



data_tidy =
  read_rds("data/balanced_score5.rds")   %>% 
  unnest(comments)  %>% 
  # filter(score_comment > 5) %>% 
  select(-score_comment) %>% 
  filter(body != "") %>% 
  group_by(subreddit) %>% 
  mutate(id = row_number()) %>%
  mutate(id = paste(subreddit, id, sep = "_")) %>% 
  ungroup() 


data_tidy = 
  data_tidy %>% 
  select(subreddit, body)



sentiment1 = 
  liwcalike(data_tidy$body, dictionary = data_dictionary_NRC)


table_sentiment1 = 
  bind_cols(data_tidy, sentiment1) %>% 
  group_by(subreddit) %>% 
  summarise(across(anger:trust, mean)) %>% 
  pivot_longer(cols = anger:trust, names_to = "sentiment", values_to = "value") %>% 
  group_by(sentiment) %>% 
  mutate(value = scale(value)) %>% 
  rename_with(~c("Subreddit", "Sentiment", "Value"))



table_sentiment1 %>% 
  ggplot() +
  geom_col(aes(Sentiment, Value)) + 
  facet_wrap(~Subreddit) +
  coord_flip()


table_sentiment1 %>% 
  ggplot() +
  geom_col(aes(Subreddit, Value, fill = Subreddit), show.legend = FALSE) + 
  facet_wrap(~Sentiment) +
  coord_flip() +
  labs(title = "Sentiment Analysis of Comments.") +
  theme_minimal()+
  theme(legend.key.size = unit(3, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=18), #change legend title font size
        legend.text = element_text(size=12), #change legend text font size
        plot.title = element_text(hjust = .5, size = 14))+
  ylab("") +
  xlab("")


ggsave("plots/sentiment_body.png", dpi = 320,
       width = 13, height = 8, units = "in")

