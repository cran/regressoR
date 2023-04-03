#' calibrate_boosting
#' 
#' @description helps to get the maximum of n.minobsinnode and bag.fraction values with which no error is generated in the model.
#'
#' @param data the name of the learning data.
#' 
#' @seealso \code{\link[gbm]{gbm}}
#'
#' @export
#'
#' @examples
#' calibrate_boosting(iris)
#' 
calibrate_boosting <- function(data){
  nr <- nrow(data)
  for(i in 10:1){
    for (j in seq(0.5, 1, 0.1)) {
      if(nr * j > i*2 + 1){
        return(list(n.minobsinnode = i, bag.fraction = j))
      }
    }
  }
  return(NULL)
}


b_ntree_values <- function(model) {
  ntree  = c(1:model$n.trees)
  b_rmse = sqrt(model$train.error)
  best_ntree = ntree[which.min(b_rmse)]
  # find overfitting, underfitting, and "best"" ntree
  fit_status = ifelse(ntree < best_ntree, "Over", ifelse(ntree == best_ntree, "Best", "Under"))
  b_results = data.frame(
    ntree,
    round(b_rmse, 10),
    fit_status
  )
  colnames(b_results) = c("ntree", "RMSE", "Fit?")
  return(b_results)
}
summary.gbm <- function (object, cBars = length(object$var.names), n.trees = object$n.trees, 
          plotit = TRUE, order = TRUE, method = relative.influence, 
          normalize = TRUE, ...) 
{
  if (n.trees < 1) {
    stop("n.trees must be greater than 0.")
  }
  if (n.trees > object$n.trees) {
    warning("Exceeded total number of GBM terms. Results use n.trees=", 
            object$n.trees, " terms.\n")
    n.trees <- object$n.trees
  }
  rel.inf <- method(object, n.trees)
  rel.inf[rel.inf < 0] <- 0
  if (order) {
    i <- order(-rel.inf)
  }
  else {
    i <- 1:length(rel.inf)
  }
  if (cBars == 0) 
    cBars <- min(10, length(object$var.names))
  if (cBars > length(object$var.names)) 
    cBars <- length(object$var.names)
  if (normalize) 
    rel.inf <- 100 * rel.inf/sum(rel.inf)
  return(data.frame(var = object$var.names[i], rel.inf = rel.inf[i]))
}

relative.influence <- function (object, n.trees, scale. = FALSE, sort. = FALSE) 
{
  if (object$distribution == "multinomial") {
    n.trees <- n.trees * object$num.classes
  }
  get.rel.inf <- function(obj) {
    lapply(split(obj[[6]], obj[[1]]), sum)
  }
  temp <- unlist(lapply(object$trees[1:n.trees], get.rel.inf))
  rel.inf.compact <- unlist(lapply(split(temp, names(temp)), 
                                   sum))
  rel.inf.compact <- rel.inf.compact[names(rel.inf.compact) != 
                                       "-1"]
  rel.inf <- rep(0, length(object$var.names))
  i <- as.numeric(names(rel.inf.compact)) + 1
  rel.inf[i] <- rel.inf.compact
  names(rel.inf) <- object$var.names
  if (scale.) {
    rel.inf <- rel.inf/max(rel.inf)
  }
  if (sort.) {
    rel.inf <- rev(sort(rel.inf))
  }
  return(rel.inf = rel.inf)
}
