
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TextSEM

<!-- badges: start -->
<!-- badges: end -->

The goal of TextSEM is to incorporate SEM with text data.

## Installation

You can install the development version of TextSEM from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Stan7s/TextSEM")
```

## Example

``` r
library(TextSEM)
```

### Sentiment Analysis

### Topic Modeling

``` r
data(prof.nest)
str(prof.nest)

model <- ' rating ~ book + difficulty +
                  topic1 + topic2 + topic3 + topic4 + topic5
         '
res <- sem.topic(prof.nest, id_var = 'profid', text_var = 'comments', model = model)
sem.topic.path(res$model)
sem.topic.plot(res$lda)
```

### Sentence Embedding
