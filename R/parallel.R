#' Start Parallel Backend for rmoo Package
#'
#' This function sets up parallel computing using the `parallel` and `doParallel` packages.
#' It supports both "snow" (PSOCK) and "multicore" backends depending on the OS.
#'
#' @param parallel Logical, numeric, character, or a cluster object. If `TRUE`, uses all detected cores.
#'   If numeric, it specifies the number of cores. If character, should be `"snow"` or `"multicore"`.
#'   If a cluster object, it will use that cluster.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class `logical` with attributes:
#'   - `type`: cluster type ("snow" or "multicore")
#'   - `cores`: number of cores used
#'   - `cluster`: the cluster object created or passed
#'
#' @examples
#' \dontrun{
#'   cl <- startParallel(TRUE)
#'   stopParallel(attr(cl, "cluster"))
#' }
#'
#' @export
startParallel <- function(parallel = TRUE, ...)
{
  # Start parallel computing for rmoo package

  # check availability of parallel and doParallel (their dependencies, i.e.
  # foreach and iterators, are specified as Depends on package DESCRIPTION file)
  if(!all(requireNamespace("parallel", quietly = TRUE),
          requireNamespace("doParallel", quietly = TRUE)))
    stop("packages 'parallel' and 'doParallel' required for parallelization!")

  # if a cluster is provided as input argument use that cluster and exit
  if(any(class(parallel) == "cluster")) {
    cl <- parallel
    parallel <- TRUE
    attr(parallel, "type") <- foreach::getDoParName()
    attr(parallel, "cores") <- foreach::getDoParWorkers()
    attr(parallel, "cluster") <- cl
    return(parallel)
  }

  # set default parallel functionality depending on system OS:
  # - snow functionality on Windows OS
  # - multicore functionality on Unix-like systems (Unix/Linux & Mac OSX)
  parallelType <- if (.Platform$OS.type == "windows") "snow" else "multicore"

  # get the current number of cores available
  numCores <- parallel::detectCores()

  # set parameters for parallelization
  if (is.numeric(parallel)) {
    numCores <- as.integer(parallel)
    parallel <- TRUE
  } else if (is.character(parallel)) {
    parallelType <- parallel
    parallel <- TRUE
  } else if (!is.logical(parallel)) {
    parallel <- FALSE
  }

  attr(parallel, "type") <- parallelType
  attr(parallel, "cores") <- numCores

  # start "parallel backend" if needed
  if(parallel) {
    if (parallelType == "snow") {
      # snow functionality on Unix-like systems & Windows
      cl <- parallel::makeCluster(numCores, type = "PSOCK")
      attr(parallel, "cluster") <- cl
      # export parent environment

      # varlist <- ls(envir = parent.frame(), all.names = TRUE)
      # varlist <- varlist[varlist != "..."]
      # parallel::clusterExport(cl, varlist = varlist, envir = parent.frame()) # envir = parent.env(environment())

      parallel::clusterExport(cl, varlist = setdiff(ls(parent.frame(), all.names = TRUE), "..."), envir = parent.frame())
      # export global environment (workspace)
      parallel::clusterExport(cl, varlist = ls(envir = globalenv(), all.names = TRUE), envir = globalenv())
      # load current packages in workers
      # pkgs <- .packages()
      lapply(.packages(), function(pkg)
        parallel::clusterCall(cl, library, package = pkg, character.only = TRUE))
      #
      doParallel::registerDoParallel(cl, cores = numCores)
    }
    else if (parallelType == "multicore") {
      # multicore functionality on Unix-like systems
      cl <- parallel::makeCluster(numCores, type = "FORK")
      doParallel::registerDoParallel(cl, cores = numCores)
      attr(parallel, "cluster") <- cl
    }
    else
    { stop("Only 'snow' and 'multicore' clusters allowed!") }
  }

  return(parallel)
}


#' Stop Parallel Backend
#'
#' Stops the parallel backend and reverts to sequential execution.
#'
#' @param cluster A cluster object, typically retrieved from `attr(parallel, "cluster")`
#' @param ... Additional arguments (currently unused).
#'
#' @return `invisible(NULL)`, used for side effects.
#'
#' @examples
#' \dontrun{
#'   cl <- startParallel()
#'   stopParallel(attr(cl, "cluster"))
#' }
#'
#' @export
stopParallel <- function(cluster, ...) {
  if (!inherits(cluster, "cluster")) {
    warning("No valid cluster provided to stopParallel().")
    return(invisible(NULL))
  }
  # Stop parallel computing
  parallel::stopCluster(cluster)
  foreach::registerDoSEQ()
  invisible(NULL)
}
