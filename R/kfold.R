# Create tibble with k-fold train and test sets
# This is the crossv_kfold function from the modelr
# package but does not require data to be a dataframe
# and does not use references; data is copied to the tibble.
 
#' Create K-fold Train and Test Sets
#'
#' Based on the function from the modelr package
#'
#' @param data A matrix or dataframe
#' @param k An integer: The number of folds.
#' @param id 
#' @return A list of tibbles with train and test columns.
#' @export
kfold <- function(data, k = 5, id = ".id") {
  n <- nrow(data)
  folds <- sample(rep(1:k, length.out = n))

  idx <- seq_len(n)
  fold_idx <- split(idx, folds)

  fold <- function(test) {
    list(
      train = data[setdiff(idx, test), ],
      test = data[test, ]
    )
  }
  cols <- purrr::transpose(purrr::map(fold_idx, fold))
  cols[[id]] <- id(k)

  tibble::as_data_frame(cols)
}
