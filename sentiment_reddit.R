library(tidyverse)
library(tidytext)
library(quanteda)
library(quanteda.dictionaries)
library(quanteda.sentiment)



data_feel =
  data_tidy %>% 
  select(subreddit, title, selftext) %>% 
  unique() %>% 
  mutate(op = str_c(title, selftext, sep = " ")) %>% 
  mutate(op = str_to_lower(op))

sentiment1 = 
  liwcalike(data_feel$op, dictionary = data_dictionary_NRC)

sentiment2 =
  liwcalike(data_feel$op, dictionary = data_dictionary_ANEW)


table_sentiment1 = 
  bind_cols(data_feel, sentiment1) %>% 
  group_by(subreddit) %>% 
  summarise(across(anger:trust, mean)) %>% 
  pivot_longer(cols = anger:trust, names_to = "sentiment", values_to = "value") %>% 
  group_by(sentiment) %>% 
  mutate(value = scale(value)) %>% 
  rename_with(~c("Subreddit", "Sentiment", "Value"))


table_sentiment2 = 
  bind_cols(data_feel, sentiment2) %>% 
  group_by(subreddit) %>% 
  summarise(across(pleasure:dominance, mean)) %>% 
  pivot_longer(cols = pleasure:dominance, names_to = "sentiment", values_to = "value") %>% 
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
  labs(title = "Sentiment Analysis of Original Posts.") +
  theme_minimal()+
  theme(legend.key.size = unit(3, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=18), #change legend title font size
        legend.text = element_text(size=12), #change legend text font size
        plot.title = element_text(hjust = .5, size = 14))+
  ylab("") +
  xlab("")


ggsave("plots/sentiment_op.png", dpi = 320,
       width = 13, height = 8, units = "in")



table_sentiment2 %>% 
  ggplot() +
  geom_col(aes(Sentiment, Value, fill = Subreddit)) + 
  facet_wrap(~Subreddit) +
  coord_flip()


table_sentiment2 %>% 
  ggplot() +
  geom_col(aes(Subreddit, Value)) + 
  facet_wrap(~Sentiment) +
  coord_flip()

