#' Multiclass Partial Dependence Plots for Predictors
#' @importFrom pdp partial
#' @importFrom dplyr select bind_rows rename all_of
#' @importFrom tidyr everything
#' @importFrom randomForest randomForest
#' @importFrom ggh4x facet_grid2
#' @importFrom rlang .data
#' @description Computes partial dependence plots (PDPs) for multiclass
#' classification models across one or more predictor variables and returns
#' individual and combined visualizations.
#'
#' For each predictor variable, PDPs are computed for each class of the response
#' using \code{pdp::partial()} with class-specific probability estimates.
#' The function produces:
#' \itemize{
#'   \item Class-specific PDP plots (color-coded and faceted)
#'   \item Combined small-multiple plots (color-coded and faceted) across
#'     predictor variables
#'   \item A unified dataset containing all PDP values
#' }
#'
#' @param object A fitted multiclass classification model supporting
#' \code{pdp::partial()}.
#' @param pred.data A data frame containing the training data used for PDP
#'   estimation. Must include both predictors and the response variable.
#' @param response A character string specifying the name of the response variable
#'   in \code{pred.data}. Associated response must be a factor for multiclass
#'   classification.
#' @param pred.vars A character vector specifying one or more predictor
#'   variables for which partial dependence should be computed.
#' @param prob Logical indicating whether or not partial dependence for
#'   classification problems should be returned on the probability scale,
#'   rather than the centered logit. If FALSE, the partial dependence
#'   function is on a scale similar to the logit. Default is TRUE.
#' @param ... Additional arguments to be passed onto \link{partial}.
#'
#' @return A named list containing:
#' \describe{
#'   \item{data}{A data frame containing all PDP values across predictors and
#'     classes}
#'   \item{<var>_color}{ggplot object showing PDPs colored by class for
#'     predictor <var>}
#'   \item{<var>_facet}{ggplot object showing PDPs faceted by class for
#'     predictor <var>}
#'   \item{combo_color}{Combined PDPs colored by class and faceted by
#'   predictor variable for comparison}
#'   \item{combo_facet}{Combined PDPs with class-by-predictor faceting for
#'   comparison}
#' }
#'
#' @details
#' This function assumes that the provided model supports class probability
#' estimation and that \code{pdp::partial()} can be applied with the
#' \code{which.class} argument. The PDPs are computed independently for each
#' class and predictor.
#'
#' @examples
#' \dontrun{
#' library(randomForest)
#' data(iris)
#'
#' rf <- randomForest(Species ~ ., data = iris, probability = TRUE)
#'
#' pd <- pdp_multiclass(
#'   object = rf,
#'   pred.data = iris,
#'   response = "Species",
#'   pred.vars = c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width")
#' )
#'
#' pd$combo_color
#' pd$combo_facet
#' }
#'
#' @export

pdp_multiclass <- function(object, pred.data, response, pred.vars,
                           prob = TRUE, ...) {

  ## -------------------------
  ## Input checks
  ## -------------------------
  if (!response %in% names(pred.data)) {
    stop("`response` is not found in pred.data, or is mistyped.")
  }

  if (!all(pred.vars %in% names(pred.data))) {
    stop("Some `pred.vars` are not in pred.data.")
  }

  res <- pred.data[[response]]

  if (!is.factor(res)) {
    stop("`response` must be a factor for multiclass PDP.")
  }

  classes <- levels(res)

  if (any(!res %in% classes)) {
    stop("Response contains values not represented in factor levels.")
  }

  ## output containers
  out <- list()

  all_p <- vector("list", length(pred.vars))

  ## -------------------------
  ## Main loop over predictors
  ## -------------------------
  for (j in seq_along(pred.vars)) {

    v <- pred.vars[j]

    p_list <- vector("list", length(classes))

    ## -------------------------
    ## PDP per class
    ## -------------------------
    for (i in seq_along(classes)) {

      pd <- pdp::partial(
        object = object,
        train = pred.data,
        pred.var = v,
        which.class = classes[i],
        prob = prob,
        ...
      )

      if (!"yhat" %in% names(pd)) {
        stop("Expected 'yhat' in pdp output. Model interface may be incompatible.")
      }

      pd$yhat.id <- classes[i]
      p_list[[i]] <- pd
    }

    p <- dplyr::bind_rows(p_list)

    ## -------------------------
    ## Standardize x-axis column
    ## -------------------------
    stopifnot(v %in% names(p))
    p <- dplyr::rename(p, x = dplyr::all_of(v))

    ## add predictor label
    p$pred.var <- v

    p <- p |> dplyr::select(.data$pred.var, tidyr::everything())
    ## store for global plot
    all_p[[j]] <- p

    ## -------------------------
    ## per-variable plots
    ## -------------------------
    g <- ggplot2::ggplot(
      p,
      ggplot2::aes(x = .data$x, y = .data$yhat, color = .data$yhat.id)
    ) +
      ggplot2::geom_line() +
      ggplot2::xlab(v) +
      ggplot2::theme(legend.title = ggplot2::element_blank())

    g1 <- ggplot2::ggplot(
      p,
      ggplot2::aes(x = .data$x, y = .data$yhat)
    ) +
      ggplot2::geom_line() +
      ggplot2::xlab(v) +
      ggplot2::facet_wrap(~ .data$yhat.id)

    out[[paste0(v, "_color")]] <- g
    out[[paste0(v, "_facet")]] <- g1
  }

  ## -------------------------
  ## Combine all predictors
  ## -------------------------
  all_p <- dplyr::bind_rows(all_p)

  ## -------------------------
  ## Combined faceted plot
  ## -------------------------
  combo_color <- ggplot2::ggplot(
    all_p,
    ggplot2::aes(x = .data$x, y = .data$yhat, color = .data$yhat.id)
  ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ .data$pred.var, scales = "free_x") +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank())

  combo_facet <- ggplot2::ggplot(
    all_p,
    ggplot2::aes(x = .data$x, y = .data$yhat)
  ) +
    ggplot2::geom_line() +
    ggh4x::facet_grid2(.data$yhat.id ~ .data$pred.var, scales = "free_x") +
    ggplot2::theme(axis.title.x = ggplot2::element_blank())

  ## -------------------------
  ## return
  ## -------------------------
  out$combo_color <- combo_color
  out$combo_facet <- combo_facet
  out$data <- all_p

  out
}

# examples
# p = pdp_multiclass(object = randomForest::randomForest(Species ~ .,
#                    data = datasets::iris, probability = TRUE),
#                    pred.data = datasets::iris, response = "Species",
#                    pred.vars = c("Petal.Length", "Sepal.Length",
#                    "Petal.Width", "Sepal.Width"))
# p$combo_color
# p$combo_facet
#
# object = randomForest(Species ~ ., data = datasets::iris, probability = TRUE)
# pred.data = datasets::iris
# response = "Species"
# pred.vars = c("Petal.Length", "Sepal.Length", "Petal.Width", "Sepal.Width"))

# set.seed(1143)  # for reproducibility

# library(randomForest)
# library(ggplot2)
# library(pdp)
# library(ggplot2)
#library(ranger)

# p = mat.or.vec(0, 0)
#
# for(i in seq_len(length(levels(iris$Species)))){
#   p1 <- partial(object = rfo1, train = iris, pred.var = "Petal.Width",
#                 which.class = i, prob = T)
#   p1$yhat.id = levels(iris$Species)[i]
#   p = rbind(p, p1)
# }
# p
#
# ggplot(p, aes(Petal.Width, yhat, color = yhat.id)) +
#   geom_line() +
#   theme(legend.title = element_blank())
#
# ggplot(p, aes(Petal.Width, yhat)) +
#   geom_line() +
#   facet_wrap(~ yhat.id, nrow = length(levels(iris$Species)))
# Glass
# glass = read.csv("misc/glass.csv")
# glass$GlassType <- as.factor(glass$GlassType)
# rfg <- randomForest(GlassType ~ ., data = glass, probability = TRUE)
# rfg$confusion
# rfg$importance
#
# p = mat.or.vec(0, 0)
# for(i in seq_len(length(levels(glass$GlassType)))){
#   p1 <- partial(object = rfg, train = glass, pred.var = "Refindex",
#                 which.class = i, prob = T)
#   p1$yhat.id = levels(glass$GlassType)[i]
#   p = rbind(p, p1)
# }
# p
# class(p$yhat.id)
#
# ggplot(p, aes(Refindex, yhat, color = yhat.id)) +
#   geom_line() +
#   theme(legend.title = element_blank())
#
# ggplot(p, aes(Refindex, yhat)) +
#   geom_line() +
#   facet_wrap(~ yhat.id, nrow = length(levels(glass$GlassType))/2)
#
#
# m = mat.or.vec(0, 0)
# for(i in seq_len(length(levels(glass$GlassType)))){
#   p1 <- partial(object = rfg, train = glass, pred.var = "Magnesium",
#                 which.class = i, prob = T)
#   p1$yhat.id = levels(glass$GlassType)[i]
#   m = rbind(m, p1)
# }
# m
#
# ggplot(m, aes(Magnesium, yhat, color = yhat.id)) +
#   geom_line() +
#   theme(legend.title = element_blank())
#
# ggplot(m, aes(Magnesium, yhat)) +
#   geom_line() +
#   facet_wrap(~ yhat.id, nrow = length(levels(glass$GlassType))/2)


# check that it works with a variety of objects

# p = pdp_multiclass(rfg, glass, "GlassType", pred.var = "Refindex")
# p$data
# p$color + geom_line(linewidth = 1)
# p$facet

# this works
# rfo1 = randomForest::randomForest(Species ~ ., data = iris,
#                                   probability = TRUE)
# p = pdp_multiclass(rfo1, iris, "Species", pred.var = "Petal.Length")
# p$data
# p$color
# p$facet
