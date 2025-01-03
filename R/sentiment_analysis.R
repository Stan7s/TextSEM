#' Structural Equation Modeling with Sentiment Analysis
#'
#' This function integrates sentiment analysis into a structural equation model (SEM) by calculating sentiment scores for specified text variables and incorporating them as additional variables in the SEM.
#'
#' @param model The structural equation model specified as a character string.
#' @param df A data frame containing the input data.
#' @param text_vars A character vector of text variable names in the data frame for which sentiment analysis should be performed.
#' @param text_stats A character vector of text sentiment statistics to be added to the SEM. Currently supports only 'OverallSenti' (overall sentiment).
#' @param polarity_dt A data table for polarity lexicon to be used for sentiment analysis. Defaults to `lexicon::hash_sentiment_jockers_rinker`.
#' @param valence_shifters_dt A data table for valence shifters to be used for sentiment analysis. Defaults to `lexicon::hash_valence_shifters`.
#' @param missing The method for handling missing data in the SEM. Defaults to 'ML' (maximum likelihood).
#' @param fixed.x Logical. If `TRUE`, the exogenous variables are treated as fixed. Defaults to `FALSE`.
#' @param ... Additional arguments passed to the `sentiment_by` function from the `sentimentr` package and to the `sem` function from the `lavaan` package.
#'
#' @return A list containing three items:
#' \item{model}{A character string representing the modified SEM with added sentiment variables.}
#' \item{data}{A data frame with added text sentiment statistics.}
#' \item{estimates}{The fitted SEM model object.}
#' @importFrom lavaan lavParseModelString lavaanify sem
#' @importFrom sentimentr sentiment_by
#' @importFrom data.table rbindlist
#' @importFrom sentiment.ai sentiment_score
#' @export
#'
sem.sentiment <- function(model,
                     df,
                     text_vars,
                     method="sentimentr",
                     text_stats=c('sentiment'),
                     polarity_dt = lexicon::hash_sentiment_jockers_rinker,
                     valence_shifters_dt = lexicon::hash_valence_shifters,
                     missing = 'ML',
                     fixed.x = FALSE,
                     ...){

  ## parse the model
  model_info <- lavParseModelString(model)
  model_var <- unique(c(model_info$lhs, model_info$rhs))

  ## get the list of text variables in the model
  text_vars <- text_vars[text_vars %in% model_var]
  # print("text_vars")
  # print(text_vars)

  N <- length(text_vars) # Number of text variables
  if (N > 0){
    ## now get the sentiment score of the text
    text_scores <- list()

    batch_sentiment <- function(text, batch_size = 200, ...) {
      if(method == "sentimentr"){
        text_batches <- split(text, ceiling(seq_along(text) / batch_size))
        scores <- data.table::rbindlist(lapply(text_batches, sentiment_by))$ave_sentiment
      }else if(method == "sentiment.ai"){
        scores <- unname(sentiment_score(text))
      }
      return(scores)
    }

    for(i in 1:N){
      sentiment_result <- batch_sentiment(df[, text_vars[i]]) # Compute sentiment scores
      text_scores[[i]] <- sentiment_result
    }
    names(text_scores) <- text_vars
    # print("text_score")
    # print(as.data.frame(text_score))

    #print("456")
    data_new <- cbind(df, as.data.frame(text_scores))
    names(data_new) <- c(names(df), paste0(rep(text_vars, each = length(text_stats)), '.', text_stats))
    #print("data_new")
    #print(names(data_new))

    model_lavaanify <- lavaanify(model)
    model_user <- model_lavaanify[model_lavaanify$user==1, ]
    # print("model_user")
    # print(model_user)

    model_new <- c()
    for(i in 1:nrow(model_user)){
      row <- model_user[i,]
      # print(row)
      if((row['lhs'] %in% text_vars) && (row['rhs'] %in% text_vars)){
        model_new <- c(model_new, paste0(rep(paste0(row['lhs'], '.', text_stats), each = length(text_stats)),
                                         ' ', row['op'], ' ', rep(paste0(row['rhs'], '.', text_stats), length(text_stats))))
      } else if(row['lhs'] %in% text_vars){
        model_new <- c(model_new, paste0(row['lhs'], '.', text_stats, ' ', row['op'], ' ', row['rhs']))
      } else if(row['rhs'] %in% text_vars){
        model_new <- c(model_new, paste0(row['lhs'], ' ', row['op'], ' ', row['rhs'], '.', text_stats))
      } else{
        model_new <- c(model_new, paste0(row['lhs'],  ' ', row['op'], ' ', row['rhs']))
      }
    }
    # print(model_new)
    # model_new <- paste0(model_new, collapse = '\n')
  }
  model_res <- sem(model=model_new, data=data_new,
                   missing = missing, fixed.x = fixed.x)

  return(list(model=model_new, data=data_new, estimates=model_res))
}
