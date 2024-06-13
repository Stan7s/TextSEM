# library(tidyverse)
library(dplyr)
library(tidytext)
library(tm)
library(topicmodels)
library(RAMpath)
library(DiagrammeR)

data(prof1000)

prof.nest <- prof1000 %>% group_by(profid) %>%
  summarise(comments = paste(comments, collapse = " "),
            rating = mean(rating), difficulty=mean(difficulty),
            book = mean(book), grade=mean(grade))

prof.tm <- unnest_tokens(prof.nest, word, comments)

data(stopwords)
prof.tm <- prof.tm %>% anti_join(filter(stopwords, lexicon == "evaluation"))

prof.tm$word <- SnowballC::wordStem(prof.tm$word)
prof.tm <- prof.tm %>%
  filter(!grepl("[[:digit:]]", word))

prof.dtm <- prof.tm %>%
  count(profid, word) %>%    ## word frequency
  cast_dtm(profid, word, n)  ## convert to dtm matrix

prof.dtm <- removeSparseTerms(prof.dtm, .995)

prof.lda <- topicmodels::LDA(prof.dtm, k = 6, control=list(seed=20240305))

comments.prob <- tidy(prof.lda, matrix = "gamma")
comments.prob <- comments.prob %>%
  spread(key = topic, value = gamma, sep = '')

## combine the data with the topic probabilities
comments.prob$document <- as.numeric(comments.prob$document)
sem.data <- left_join(prof.nest, comments.prob, by=join_by(profid==document))

model <- ' rating ~ book + difficulty +
                  topic1 + topic2 + topic3 + topic4 + topic5
         '

res <- lavaan::sem(model = model, data = sem.data)
summary(res, fit=TRUE)

plot.res <- lavaan2ram(res, ram.out = F)
plot.res.path <- ramPathBridge(plot.res, F, F)
plot(plot.res.path, 'ex2', output.type='dot')

grViz('ex2.dot')


prof.topics <- tidy(prof.lda, matrix = "beta")
prof.topics

## terms & topics
prof.terms <- prof.topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

prof.terms %>% print(n=60)

reorder_within <- function (x, by, within, fun = mean, sep = "___", ...)
{
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

## plot the topics and terms
prof.terms %>%
  mutate(topic=as.factor(topic), term = reorder_within(term, beta, topic, sep="")) %>%
  ggplot(aes(term, beta, fill = topic)) +
  geom_col(show.legend = FALSE) + facet_wrap(~topic, scales = "free", labeller = "label_both") +
  xlab("Terms") + ylab("Topics") + coord_flip() + scale_x_reordered() + scale_fill_grey()+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12))

