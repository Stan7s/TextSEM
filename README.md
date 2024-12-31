
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TextSEM

<!-- badges: start -->
<!-- badges: end -->

The goal of TextSEM is to incorporate SEM with text data.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("Stan7s/TextSEM")
```

## Example

``` r
library(TextSEM)
library(RAMpath)
library(DiagrammeR)
```

When using it for the first time, run:

``` r
textsem_install() 
```

At the beginning of each session, run:

``` r
textsem_init()
```

## Dataset

For illustration, we will a set of student evaluation of teaching data.
The data were scraped from an online website conforming to its site
requirement. In total, we have 11,873,668 teaching evaluations on
275,522 instructors from students in 3,168 colleges. The evaluations
were conducted from October 1999 to September 2018.

For each evaluation, we have information on the overall numerical rating
of the teaching of the instructor, how difficult the class was, whether
the student took the class for credit or not, whether the class was an
online class or not, whether a textbook was used or not, the grade the
student received, and a short text comment regarding the teaching of the
instructor. There is also a “tag” variable that kind of summarizes the
evaluation.

For demonstration, we mainly use the data from 1,000 professors with a
little bit over 38,000 evaluations.

``` r
data(prof1000)
str(prof1000)
```

### Sentiment Analysis

In this example, the overall sentiment of the text was extracted and
used as a mediator. The function `sem.text` can be used to estimated the
model.

Using dictionary-based sentiment analysis:

``` r
model <- 'teaching =~ rating + comments + tags
          teaching ~ book + difficulty + gender
         '
res <- sem.sentiment(model = model,
                df = prof1000,
                text_var = c('comments', 'tags'), 
                method = 'sentimentr')
res$model
res$estimate
lavaan::summary(res$estimates, fit = TRUE)
```

Using AI-based sentiment analysis:

``` r
# Outcome variable: rating, grade
# Predictor: book, attendance, difficulty
# Mediator: comments

model <- ' rating ~ book + attendance + difficulty + comments
           grade ~ book + attendance + difficulty + comments
           comments ~ book + attendance + difficulty
         '

res <- sem.sentiment(model = model,
                df = prof1000,
                text_var = c('comments'),
                method = "sentiment.ai")

lavaan::summary(res$estimates, fit = TRUE)
```

Multiple text variables:

``` r
# Outcome variable: rating, grade
# Predictor: book, attendance, difficulty
# Mediator: comments, tags

model <- ' rating ~ book + attendance + difficulty + comments
           grade ~ book + attendance + difficulty + comments
           comments ~ book + attendance + difficulty + tags
         '
res <- sem.sentiment(model = model,
                df = prof1000,
                text_var = c('comments', 'tags'),
                method = 'sentimentr')

res$model
summary(res$estimates, fit = TRUE)
```

The path diagram is given below. From the results, the text is a
mediator between whether a book is required and the grade and rating.

``` r
plot.res <- lavaan2ram(res$estimates, ram.out = F)
# Filter out intercepts by removing rows where the predictor is missing or equals "1" (common in RAM formats for intercepts)
# plot.res <- plot.res[plot.res$lhs != "1", ]
plot.res.path <- ramPathBridge(plot.res, F, F)
plot(plot.res.path, 'sentiment', output.type='dot')

grViz('sentiment.dot')
```

### Topic Modeling

For the aspect-based sentiment analysis, the aspects are determined
before the sentiment analysis. If the aspects are not known or not easy
to identify, we can use topic models. Topic models can be used to
identify the topics and associated words in a text. Latent Dirichlet
allocation (LDA) is a widely used method for topic modeling that
facilitates the explanations of the observed words using latent topics.

To combine SEM with topic models, we can first analyze the data to
extract the topics and then use the topics in SEM.

The following code processes the prof1000 dataset by grouping the data
by professor’s id (`profid`) and then summarizing the grouped data. It
concatenates all comments for each professor into a single string and
calculates the mean of the rating, difficulty, book, and grade
variables, handling any missing values by excluding them from the
calculation. The resulting summarized dataset is stored in the prof.nest
data frame.

``` r
prof.nest <- prof1000 %>% group_by(profid) %>%
summarise(comments = paste(comments, collapse = " "),
          tags = paste(tags, collapse = ";"),
          rating = mean(rating, na.rm = TRUE), 
          difficulty=mean(difficulty, na.rm = TRUE),
          book = mean(book, na.rm = TRUE), 
          grade=mean(grade, na.rm = TRUE))
# The nested dataset is also stored in this package:
# data(prof.nest)

str(prof.nest)
```

As an example, we combine the comments for each professor to conduct the
topic modeling. Previous analysis of the data has found 6 topics in the
data \[@jacobucci2020\] based on cross validation. Here, we directly fit
the model 6 topics.

``` r
model <- ' rating ~ book + difficulty + comments'
res <- sem.topic(model = model, 
                 data = prof.nest, 
                 text_var = c('comments'), 
                 n_topics = c(6))
summary(res$estimates, fit=TRUE)
```

Plot the top-frequency terms of each topic:

``` r
sem.topic.plot(res$lda$comments)
```

Plot the path diagram:

``` r
plot.res <- lavaan2ram(res$estimates, ram.out = F)
plot.res.path <- ramPathBridge(plot.res, F, F)
plot(plot.res.path, 'topic', output.type='dot')

graph <- grViz('topic.dot')

svg_graph <- DiagrammeRsvg::export_svg(graph)
rsvg::rsvg_pdf(charToRaw(svg_graph), file = "output/topic_path.pdf")
```

Multiple text variables:

``` r
model <- ' rating ~ book + difficulty + comments + tags'
res <- sem.topic(model = model, 
                 data = prof.nest, 
                 text_var = c('comments', 'tags'), 
                 n_topics = c(6, 3))
summary(res$estimates, fit=TRUE)
```

### Sentence Embedding

The `sem.encode` function generates sentence embeddings using specified
pre-trained models from the SentenceBERT or OpenAI GPT series. It allows
for reduction of dimensionality using either Singular Value
Decomposition (SVD) or Principal Component Analysis (PCA).

The `sem.emb` function integrates sentence embeddings into SEM.

Example 1: SentenceBERT embeddings + PCA

``` r
prof.head <- head(prof1000, 30)
sem_model <- ' rating ~ book + difficulty + comments'
reduced_embeddings <- sem.encode(prof.head$comments, encoder = "all-mpnet-base-v2", reduce_method = "PCA")
result <- sem.emb(sem_model = sem_model, data = prof.head, text_var = "comments", encoder = "all-mpnet-base-v2", reduce_method = "PCA", reduce_dim = 5)
lavaan::summary(result$estimates, fit=TRUE)
```

Example 2: GPT embeddings + SVD

``` r
Sys.setenv(OPENAI_API_KEY = "Your API key") # Setting up OpenAI api key in your system

prof.head <- head(prof1000, 30)
sem_model <- ' rating ~ book + difficulty + comments'
reduced_embeddings <- sem.encode(prof.head$comments, encoder = "text-embedding-3-small", reduce_method = "SVD")
result <- sem.emb(sem_model = sem_model, data = prof.head, text_var = "comments", encoder = "text-embedding-3-small", reduce_method = "SVD", reduce_dim = 5)
lavaan::summary(result$estimates, fit=TRUE)
```
