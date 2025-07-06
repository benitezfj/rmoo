#' Calculation of Modified Crowding Distance
#'
#' A Crowded-comparison approach.
#'
#' The crowded-comparison operator maintain diversity in the Pareto front
#' during multi-objective optimization. This version uses a reference point-based
#' normalization and preference distance strategy.
#'
#' @param object An object of class 'rnsga2', typically from a call to r-nsga2.
#'               Must contain fitness, population, fronts, popSize, and reference_points.
#' @param epsilon Minimum allowed distance between solutions to avoid duplicates.
#' @param weights A numeric vector of weights for preference distance (default is equal weights).
#' @param normalization Type of normalization to apply: `"ever"`, `"front"`, or `"no"`.
#' @param extreme_points_as_ref_dirs Logical; whether to use extreme points as reference directions.
#'
#' @author Francisco Benitez
#'
#' @references Kalyanmoy Deb and J. Sundar (2006). GECCO '06. doi:10.1145/1143997.1144112
#'
#' @seealso [rnsga2()]
#'
#' @return A list with:
#' \describe{
#'   \item{survivors}{Indices of selected individuals}
#'   \item{indexmin}{Index of individuals with minimum scalarizing value (optional)}
#'   \item{reference_points}{Updated reference points matrix}
#' }
#' @export
modifiedCrowdingDistance <- function(object,
                                     epsilon,
                                     weights = NULL,
                                     normalization = "front",
                                     extreme_points_as_ref_dirs = FALSE) {
  fitness <- object@fitness
  population <- object@population
  nObj <- ncol(fitness)
  fronts <- object@f
  nFront <- length(fronts)
  popSize <- object@popSize
  reference_points <- object@reference_points

  # if (is.null(weights)) {
  #   weights <- rep((1/nObj),nObj)
  # }
  weights <- weights %||% rep(1 / nObj, nObj)

  normalize_range <- function(mat, idx) {
    apply(mat[idx, , drop = FALSE], 2, range)
  }

  ideal_point <- rep(Inf, nObj)
  nadir_point <- rep(-Inf, nObj)

  #Normalization
  if (normalization == "ever") {
    ideal_point <- apply(rbind(ideal_point,fitness), 2, min)
    nadir_point <- apply(rbind(ideal_point,fitness), 2, max)
  } else if (normalization == "front" && length(fronts[[1]]) > 1) {
    ideal_point <- apply(fitness[fronts[[1]],], 2, min)
    nadir_point <- apply(fitness[fronts[[1]],], 2, max)
  } else if (normalization == "no") {
    ideal_point <- rep(1, nObj)
    nadir_point <- rep(0, nObj)
  }

  if (extreme_points_as_ref_dirs){
    ps <- PerformScalarizing(population = population[unlist(fronts), ],
                             fitness = fitness[unlist(fronts), ],
                             smin = object@smin,
                             extreme_points = reference_points,
                             ideal_point = ideal_point)
    reference_points <- rbind(reference_points, ps$extremepoint)
    smin <-  ps$indexmin
  } else{
    smin <- NULL
  }

  n_remaining <- popSize
  survivors <- c()

  distance_to_ref_points <- calc_norm_pref_distance(fitness=fitness,
                                                    ref_points=reference_points,
                                                    weight=weights,
                                                    ideal_point=ideal_point,
                                                    nadir_point=nadir_point
                                                    )

  for (i in seq_len(nFront)) {
    #cat(i, " Iter: ", object@iter, "\n")
    n_remaining <- popSize - length(survivors)
    if(n_remaining==0) break
    if(length(fronts[[i]]) > 1){
      # rank_by_distance <- apply(apply(distance_to_ref_points[fronts[[i]],], 2, order), 2, order)
      rank_by_distance <- apply(apply(as.matrix(distance_to_ref_points[fronts[[i]],]), 2, order), 2, order) #We use as.matrix in the case when the distance ob to the reference points has one dimension
      ref_point_of_best_rank <- apply(rank_by_distance, 1, which.min)
    }else{
      rank_by_distance <-  order(order(distance_to_ref_points[fronts[[i]],]))
      rank_by_distance <- t(rank_by_distance)
      ref_point_of_best_rank <- which.min(rank_by_distance)
    }
    ranking <- diag(rank_by_distance[seq_len(length(fronts[[i]])), ref_point_of_best_rank])

    if (length(fronts[[i]]) < n_remaining){
      crowding <- ranking
      I <- seq_len(length(fronts[[i]]))
    } else{
      dist_to_others <- calc_norm_pref_distance(fitness=fitness[fronts[[i]],],
                                                ref_points=fitness[fronts[[i]],],
                                                weight=weights,
                                                ideal_point=ideal_point,
                                                nadir_point=nadir_point)
      diag(dist_to_others) <- Inf
      crowding <- rep(NA_real_, length(fronts[[i]]))
      not_selected <- order(ranking)

      while (length(not_selected) > 0) {
        idx <- not_selected[1]
        crowding[idx] <- ranking[idx]
        to_remove <- c(idx)

        dist <- dist_to_others[idx, not_selected]

        group <- not_selected[which(dist < epsilon)][1]

        if (!is.na(group)){
          if (length(group)){
            crowding[group] <- ranking[group] + round(length(fronts[[i]]) / 2)

            # remove group from not_selected array
            to_remove <- c(to_remove, group)


          }
        }
        not_selected <- not_selected[which(!(not_selected %in% to_remove))]

      }
      I <- order(crowding)[1:n_remaining]

    }
    survivors <- c(survivors,fronts[[i]][I])

  }
  out <- list(survivors = survivors,
              indexmin = smin,
              reference_points = reference_points)
  return(out)
}

#' Calculate Normalized Preference Distance

#' Computes the weighted normalized Euclidean distance between a set of fitness
#' vectors and a set of reference points.

#' @param fitness A matrix of fitness values.
#' @param ref_points A matrix of reference points.
#' @param weight A numeric vector of weights for each objective.
#' @param ideal_point A numeric vector of ideal point values.
#' @param nadir_point A numeric vector of nadir point values.
#'
#' @return A matrix of distances where element (i, j) is the distance from
#'        fitness to ref_points.
#' @export
calc_norm_pref_distance <- function(fitness, ref_points, weight, ideal_point, nadir_point) {
  fitness <- as.matrix(fitness)
  ref_points <- as.matrix(ref_points)
  # if(!is.matrix(ref_points)){
  #   ref_points <- t(ref_points)
  # }
  # if(!is.matrix(fitness)){
  #   fitness <- t(fitness)
  # }

  # Calculate the difference between fitness and ref_points
  D <- matrix(rep(fitness, each = nrow(ref_points)),
              ncol = ncol(ref_points), byrow = FALSE) -
       matrix(rep(t(ref_points), nrow(fitness)),
              ncol = ncol(fitness), byrow = TRUE)

  # Calculate the denominator
  denom <- nadir_point - ideal_point # New
  denom[denom == 0] <- 1e-12
  # denom[which(denom == 0)] <- 1e-12

  # Calculate the normalized preference distance
  N <- ((sweep(D, 2, denom, FUN = "/"))^2) * weight
  N <- sqrt(rowSums(N) * length(weight))
  # N <- sqrt(apply(N, 1, sum) * length(weight))

  matrix(N, nrow = nrow(fitness), ncol = nrow(ref_points), byrow = TRUE)
  # return(matrix(N, nrow(fitness), nrow(ref_points), byrow = TRUE))

}
