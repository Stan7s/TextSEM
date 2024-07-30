
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

``` r
# Outcome variable: rating, grade
# Predictor: book, attendance, difficulty
# Mediator: comments

model <- ' rating ~ book + attendance + difficulty + comments + tags
           grade ~ book + attendance + difficulty + comments
           comments ~ book + attendance + difficulty
         '
res <- sem.sentiment(model = model,
                data = prof1000,
                text_var=c('tags', 'comments'))

res$model
summary(res$estimates, fit = TRUE)
```

The path diagram is given below. From the results, the text is a
mediator between whether a book is required and the grade and rating.

``` r
plot.res <- lavaan2ram(res$estimates, ram.out = F)
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

grViz('topic.dot')
```

Note that multiple text variables can be included:

``` r
model <- ' rating ~ book + difficulty + comments + tags'
res <- sem.topic(model = model, 
                 data = prof.nest, 
                 text_var = c('comments', 'tags'), 
                 n_topics = c(6, 3))
summary(res$estimates, fit=TRUE)
```

### Sentence Embedding

Sentence embeddings can be retrived from pre-trained models, most of
which are implemented in Python. To use them here, we can leverage the
`reticulate` R package:

``` r
library(reticulate)

## First time set-up
# virtualenv_create("r-reticulate")
# py_install("pandas")
# py_install("transformers")
# py_install("torch")
# py_install("sentence_transformers")

## Call virtual environment
use_virtualenv("r-reticulate")

# Track error
# py_last_error()
```

R Markdown and knitr support code chucks in Python.

``` python
## Set environment variables
import os
os.environ["TOKENIZERS_PARALLELISM"] = "false"

## Load data
import pandas as pd
df = pd.read_csv("data-raw/prof1000.original.csv")
df = df.head()
print(df)
```

One option is to get text embedding of a BERT model from
[Huggingface](https://huggingface.co/):

``` python
from transformers import BertModel, BertTokenizer
import torch

model_name = 'bert-base-uncased'
tokenizer = BertTokenizer.from_pretrained(model_name)
model = BertModel.from_pretrained(model_name)

def get_embedding(text):
    inputs = tokenizer(text, return_tensors='pt', padding=True, truncation=True, max_length=128)
    with torch.no_grad():
        outputs = model(**inputs)
        embedding = outputs.last_hidden_state[:, 0, :].squeeze().numpy()
    return embedding
df['emb_comments'] = df['comments'].apply(get_embedding)

print(df)
```

Another is from [sentence transformers](https://sbert.net/):

``` python
from sentence_transformers import SentenceTransformer

model = SentenceTransformer("all-mpnet-base-v2")
df['emb_comments'] = df['comments'].apply(model.encode)

# Or get the npy array directly:
# embeddings = model.encode(df['comments'])
# embeddings
```

Alternatively, we can simply call the `TextSEM::sem.encode()` function,
which is a wrapper of sentence transformers:

``` r
embeddings <- sem.encode(prof.nest$comments)
dim(embeddings)

save(embeddings, file="../data/prof.nest.emb.rda")
```

In the following example, we can use the text data to predict the
numerical rating. With the embedded vectors, we can fit a regression
model as

$$
rating_i = \beta_0 + \sum_{j=1}^{n}\beta_j v_{ij} + e_i
$$

where n is the length of the embedding of each text (here, n = 768). We
can use a regression model to estimate the model:

``` r
# Retrieve the saved word embeddings
load("../data/prof.nest.emb.rda")

## Select and organize the data
reg.data <- cbind(prof.nest$rating, embeddings)
reg.data <- as.data.frame(reg.data)
names(reg.data)[1] <- 'rating'

reg.model <- lm(rating ~ ., data = reg.data)

## Calculate R2
reg.est <- summary(reg.model)
reg.est$r.squared

## Visualize the coefficients and its significance
plot(reg.est$coefficients[-1, 1], type='h', ylab='Coefficients')
plot(reg.est$coefficients[-1, 4], type='h', ylab='p-value')
abline(h=.05)

## Number of significant predictors
sum(reg.est$coefficients[-1, 4] < .05)
```

Or, use `TextSEM::sem.emb()`:

(This is extremely slow, probably because lavaan cannot take too many
variables. I took the first 10 items for test use.)

``` r
model <- ' rating ~ book + difficulty + comments'

res <- sem.emb(prof.nest, text_var = "comments", sem_model = model)

## Alternatively, load embeddings through files: 
sem.emb(prof.nest, text_var = "comments", sem_model = model, emb_file_path = "../data/prof.nest.emb.rda")

summary(res, fit=TRUE)
```

Plot the path diagram:

``` r
plot.res <- lavaan2ram(res, ram.out = F)
plot.res.path <- ramPathBridge(plot.res, F, F)
plot(plot.res.path, 'embedding', output.type='dot')

grViz('embedding.dot')
```
