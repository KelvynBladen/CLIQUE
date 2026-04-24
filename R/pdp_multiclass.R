# Main function
#' pdp_multiclass
#' @importFrom ggplot2 ggplot aes geom_line xlab theme element_blank facet_wrap
#' @importFrom pdp partial
#' @importFrom randomForest randomForest
#' @description words.
#' @param object words.
#' @param pred.data words.
#' @param response words.
#' @param pred.var words.
#'
#' @return A list containing:
#'   \item{data}{A data frame of values for the plots.}
#'   \item{color}{Colored plot.}
#'   \item{facet}{Faceted plot.}
#' @export
#'
#' @examples
#' p = pdp_multiclass(randomForest::randomForest(Species ~ .,
#'                    data = datasets::iris,
#'                    probability = TRUE), datasets::iris, "Species",
#'                    pred.var = "Petal.Length")
#' p$color
#' p$facet

pdp_multiclass <- function(object = randomForest(Species ~ .,
                                                 data = datasets::iris,
                                                 probability = TRUE),
                           pred.data = datasets::iris,
                           response = "Species",
                           pred.var = c("Petal.Length")){

  res = pred.data[, response]
  l = list()

  #for(j in seq_len(length(pred.var))){
  p = mat.or.vec(0, 0)

  for(i in seq_len(length(levels(res)))){

    p1 <- pdp::partial(object = object, train = pred.data,
                       pred.var = pred.var, which.class = i, prob = T)

    p1$yhat.id = levels(res)[i]

    p = rbind(p, p1)

  }
  g = ggplot(p, aes(p[,1], .data$yhat, color = .data$yhat.id#, group = yhat.id # for factors
  )) +
    geom_line() +
    xlab(pred.var) +
    theme(legend.title = element_blank())

  g1 = ggplot(p, aes(p[,1], .data$yhat#, group = yhat.id # for factors
  )) +
    geom_line() +
    xlab(pred.var) +
    facet_wrap(~ .data$yhat.id, nrow = floor(length(levels(res))/2))


  #l[[j]] = p
  #}

  l$data = p
  l$color = g
  l$facet = g1
  l
  #l
}

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


# make function be able to accept multiple pred.vars
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

