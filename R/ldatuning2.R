# Modification of ldatuning function
# Griffiths2004 method works only with Gibbs sampling based models

#' score_models
#'
#' Replaces FindTopicsNumber from ldatuning package. Takes
#' topicmodels LDA output models and topic count and returns
#' several scores of the clustering fit.
#'
#' @param dtm An object of class "DocumentTermMatrix" with
#'   term-frequency weighting or an object coercible to a
#'   "simple_triplet_matrix" with integer entries.
#' @param topics Vvector with number of topics to compare different models.
#' @param metrics String or vector of possible metrics: "Griffiths2004",
#'   "CaoJuan2009", "Arun2010", "Deveaud2014".
#' @param mc.cores Integer; The number of CPU cores to processes models
#'   simultaneously.
#' @param verbose Logical; Print status updates.
#' @return Data-frame with one or more metrics.  numbers of topics and
#'   corresponding values of metric. Can be directly used by
#'   FindTopicsNumber_plot to draw a plot.
#'
#' @export
#' @import topicmodels
score_models <- function(models, dtm,
                         topics = seq(10, 40, by = 10),
                         metrics = c("CaoJuan2009", "Arun2010", "Deveaud2014"),
                         verbose = FALSE) {

  # calculate metrics
  if (verbose) cat("calculate metrics:\n")
  result <- data.frame(topics)
  for (m in metrics) {
    if (verbose) cat(sprintf("  %s...", m))
    if (!m %in% c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014")) {
      cat(" unknown!\n")
    } else {
      result[m] <- switch(m,
        "Griffiths2004" = Griffiths2004(models, control),
        "CaoJuan2009" = CaoJuan2009(models),
        "Arun2010" = Arun2010(models, dtm),
        "Deveaud2014" = Deveaud2014(models),
        NaN
      )
      if (verbose) cat(" done.\n")
    }
  }
  return(result)
}


Griffiths2004 <- function(models, control) {
  # log-likelihoods (remove first burning stage)
  burnin <- ifelse("burnin" %in% names(control), control$burnin, 0)
  logLiks <- lapply(models, function(model) {
    utils::tail(model@logLiks, n = length(model@logLiks) - burnin / control$keep)
    # model@logLiks[-(1 : (control$burnin/control$keep))]
  })
  # harmonic means for every model
  metrics <- sapply(logLiks, function(x) {
    # code is a little tricky, see explanation in [Ponweiser2012 p. 36]
    # ToDo: add variant without "Rmpfr"
    llMed <- stats::median(x)
    metric <- as.double(
      llMed - log(Rmpfr::mean(exp(-Rmpfr::mpfr(x, prec = 2000L) + llMed)))
    )
    return(metric)
  })
  return(metrics)
}

CaoJuan2009 <- function(models) {
  metrics <- sapply(models, function(model) {
    # topic-word matrix
    m1 <- exp(model@beta)
    # pair-wise cosine distance
    pairs <- utils::combn(nrow(m1), 2)
    cos.dist <- apply(pairs, 2, function(pair) {
      x <- m1[pair[1], ]
      y <- m1[pair[2], ]
      # dist <- lsa::cosine(x, y)
      dist <- crossprod(x, y) / sqrt(crossprod(x) * crossprod(y))
      return(dist)
    })
    # metric
    metric <- sum(cos.dist) / (model@k * (model@k - 1) / 2)
    return(metric)
  })
  return(metrics)
}

Arun2010 <- function(models, dtm) {
  # length of documents (count of words)
  len <- slam::row_sums(dtm)
  # evaluate metrics
  metrics <- sapply(models, FUN = function(model) {
    # matrix M1 topic-word
    m1 <- exp(model@beta) # rowSums(m1) == 1
    m1.svd <- svd(m1)
    cm1 <- as.matrix(m1.svd$d)
    # matrix M2 document-topic
    m2 <- model@gamma # rowSums(m2) == 1
    cm2 <- len %*% m2 # crossprod(len, m2)
    norm <- norm(as.matrix(len), type = "m")
    cm2 <- as.vector(cm2 / norm)
    # symmetric Kullback-Leibler divergence
    divergence <- sum(cm1 * log(cm1 / cm2)) + sum(cm2 * log(cm2 / cm1))
    return(divergence)
  })
  return(metrics)
}

Deveaud2014 <- function(models) {
  metrics <- sapply(models, function(model) {
    ### original version
    # topic-word matrix
    m1 <- exp(model@beta)
    # prevent NaN
    if (any(m1 == 0)) {
      m1 <- m1 + .Machine$double.xmin
    }
    # pair-wise Jensen-Shannon divergence
    pairs <- utils::combn(nrow(m1), 2)
    jsd <- apply(pairs, 2, function(pair) {
      x <- m1[pair[1], ]
      y <- m1[pair[2], ]
      ### standard Jensen-Shannon divergence
      # m <- (x + y) / 2
      # jsd <- 0.5 * sum(x*log(x/m)) + 0.5 * sum(y*log(y/m))
      ### divergence by Deveaud2014
      jsd <- 0.5 * sum(x * log(x / y)) + 0.5 * sum(y * log(y / x))
      return(jsd)
    })

    # metric
    metric <- sum(jsd) / (model@k * (model@k - 1))
    return(metric)
  })
  return(metrics)
}


#' FindTopicsNumber_plot
#'
#' Support function to analyze optimal topic number. Use output of the
#' score_models function. I have changed the original
#'
#' @param values Data-frame with first column named `topics` and other columns
#'   are values of metrics.
#'
#' @examples
#' \dontrun{
#'
#' library(topicmodels)
#' data("AssociatedPress", package="topicmodels")
#' dtm <- AssociatedPress[1:10, ]
#' optimal.topics <- FindTopicsNumber(dtm, topics = 2:10,
#'   metrics = c("Arun2010", "CaoJuan2009", "Griffiths2004")
#' )
#' FindTopicsNumber_plot(optimal.topics)
#' }
#'
#' @export
#' @import ggplot2
plot_lda_scores <- function(values, pal = NULL) {
  # normalize to [0,1]
  columns <- base::subset(values, select = 2:ncol(values))
  values <- base::data.frame(
    values["topics"],
    base::apply(columns, 2, function(column) {
      scales::rescale(column, to = c(0, 1), from = range(column))
    })
  )

  values$Deveaud2014 <- 1 - values$Deveaud2014
  # Check for existence of columns
  # values$Griffiths2004 <- 1- values$Griffiths2004

  if (is.null(pal)) pal <- pal21()
  # melt
  values <- reshape2::melt(values, id.vars = "topics", na.rm = TRUE)
  names(values) <- c("Topics", "Score", "value")
  # standart plot
  p <- ggplot(values, aes_string(x = "Topics", y = "value", group = "Score"))
  p <- p + geom_line(aes_string(color = "Score"))
  p <- p + geom_point(aes_string(shape = "Score", color = "Score"), size = 2)
  p <- p + scale_x_continuous(breaks = values$Topics)
  p <- p + scale_color_manual(values = pal21()[1:5])
  p <- p + labs(
    x = "Number of topics",
    y = NULL,
    title = "Topic model scores",
    subtitle = "All scores rescaled to (0,1), lower is better"
  )

  p <- p + theme_scatter("#f8f8f8")
  p
  # return(p)
}

