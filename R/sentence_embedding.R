#' Generate Sentence Embeddings using Sentence Transformers
#'
#' This function generates sentence embeddings for a given vector of text using a specified pre-trained model from the `sentence_transformers` Python package.
#'
#' @param text_vector A character vector containing the text data to be embedded.
#' @param encoder A character string specifying the name of the pre-trained model to be used for generating embeddings.
#' @param reduce_method Dimension reduction method for embeddings. Can be either "SVD" or "PCA".
#' @param reduce_dim An integer denoting the size of embedding after reduction.
#'
#' @return A matrix of sentence embeddings with each row corresponding to a sentence from `text_vector` and each column representing a dimension of the embedding space.
#' @import reticulate
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' text_vector <- c("This is a sentence.", "This is another sentence.")
#' model_name <- "paraphrase-MiniLM-L6-v2"
#' embeddings <- sem.emb(text_vector, model_name)
#' print(embeddings)
#' }
sem.encode <- function(text_vector, encoder = "all-mpnet-base-v2", reduce_method = "SVD", reduce_dim = 5){
  models.sbert = c("all-mpnet-base-v2", "paraphrase-MiniLM-L6-v2")
  models.gpt = c("text-embedding-3-small", "text-embedding-3-large", "text-embedding-ada-002")

  normalize_l2 <- function(vector) {
    norm <- norm(vector, type = "2")
    if (norm == 0) norm <- 1  # Avoid division by zero
    return(vector / norm)
  }

  generate_emb <- function(text){
    response = openai$embeddings$create(input=text, model=encoder, encoding_format="float")
    emb = response$data[[1]]$embedding
    normalized_emb = normalize_l2(emb)
    return(normalized_emb)
  }

  if(encoder %in% models.sbert){
    sbert <- import("sentence_transformers")
    model <- sbert$SentenceTransformer(encoder)
    embeddings <- model$encode(text_vector)
  } else if(encoder %in% models.gpt){
    openai <- import("openai")
    openai$api_key <- Sys.getenv("OPENAI_API_KEY")
    embs <- lapply(text_vector, generate_emb)
    embeddings <- do.call(rbind, embs)
  } else {
    stop("Encoder not supported.")
  }

  if(reduce_method == "SVD"){
    svd_result <- svd(embeddings)
    U <- svd_result$u
    D <- diag(svd_result$d[1:reduce_dim])
    reduced_emb <- U[, 1:reduce_dim] %*% D
    print(dim(reduced_emb))  # Should be n x reduce_dim
    print(reduced_emb)
  } else if(reduce_method == 'PCA'){
    pca_result <- prcomp(embeddings, scale. = TRUE, center = TRUE)
    reduced_emb <- pca_result$x[, 1:reduce_dim]
    # print(dim(reduced_emb))  # Should be n x reduce_dim
    # print(reduced_emb)
  }

  # Rename the columns
  colnames(reduced_emb) <- paste0('v', 1:ncol(reduced_emb))
  rownames(reduced_emb) <- 1:nrow(reduced_emb)
  return(as.matrix(reduced_emb))
}

#' Structural Equation Modeling with Embeddings
#'
#' This function performs Structural Equation Modeling (SEM) using text embeddings. It checks if the specified `.rda` file with embeddings exists. If the file exists, it loads the embeddings; otherwise, it generates the embeddings using the `sem.encode` function. The embeddings are then incorporated into the SEM model.
#'
#' @param sem_model A character string specifying the SEM model.
#' @param data A data frame containing the input data.
#' @param text_var A character string specifying the name of the text variable in the data frame.
#' @param pca_dim A integer specifying reduced dimension through PCA.
#' @param encoder A character string specifying the encoder model to be used for generating embeddings. Defaults to "all-mpnet-base-v2".
#' @param reduce_dim An integer denoting the size of embedding after reduction.
#' @param emb_filepath A character string specifying the path to the `.rda` file containing the embeddings. If `NULL`, embeddings are generated using `sem.encode`.
#'
#' @return The result of the `lavaan::sem` function, which is an object of class `lavaan`.
#' @importFrom lavaan sem
#' @importFrom stats prcomp
#' @export
#'
#' @examples
#' \dontrun{
#' sem_model <- 'rating ~ book + difficulty + comments'
#' res <- sem.emb(sem_model = sem_model, data = prof.nest, text_var = "comments",
#'                 pca_dim = 10, emb_model = "all-mpnet-base-v2")
#' summary(res, fit=TRUE)
#' }
#'
sem.emb <- function(sem_model, data, text_var, encoder = "all-mpnet-base-v2", emb_filepath = NULL, reduce_method = "SVD", reduce_dim = 5){

  df <- data

  # Check if the file path ends with .rda
  load_flag = FALSE
  if (!is.null(emb_filepath)) {
    if (grepl("\\.rda$", emb_filepath)) {
      if (file.exists(emb_filepath)) {
        # Load the file
        print("Loading embeddings from file...")
        embeddings <- get(load(emb_filepath))
        if (is.matrix(embeddings)) {
          if (nrow(embeddings) == nrow(df)) {
            print("Success.")
            load_flag = TRUE
          } else {
            print("Incorrect dimension.")
          }
        } else {
          print("Loaded object is not a matrix.")
        }
      } else {
        print("File doesn't exist.")
      }
    } else {
      stop("The specified file is not an `.rda` file.")
    }
  }

  if (!load_flag){
    print("Generating embeddings, this might take a while...")
    embeddings <- TextSEM::sem.encode(df[[text_var]], reduce_method = reduce_method, reduce_dim = reduce_dim)
    print("Success.")
  }
  replace_vars_in_model <- function(model, var_old, var_new) {
    replacement <- paste(var_new, collapse = " + ")
    updated_model <- gsub(paste0("\\b", var_old, "\\b"), replacement, model)
    return(updated_model)
  }

  colnames(embeddings) <- paste0(text_var, '.', colnames(embeddings))
  model_new <- replace_vars_in_model(sem_model, text_var, colnames(embeddings))

  df <- cbind(df, embeddings)
  df <- as.data.frame(df)
  estimates <- lavaan::sem(model = model_new, data = df)

  list(model = model_new, data = df, estimates = estimates)
}
