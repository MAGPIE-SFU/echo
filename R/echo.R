#' ECHO
#' 
#' `echo` will estimate the number of cryptic cases related to a phylogenetic tree. 
#' 
#' @param tree The phylogenetic tree(s) for which ECHO will estimate cryptic case counts.
#' Can be an object of class `phylo`, `treedata`, a string indicating the filepath of a Newick file, or a numeric value equal to the total branch length of the tree.
#' If you want `echo` to process a posterior sample of trees, input the sample either as a list or a vector.
#' @param n_samples The number of samples. This should be equal to the number of tips in the phylogeny.
#' @param latent_duration The average duration of the latent period in days.
#' @param infectious_duration The average duration of the infectious period in days.
#' @param Re The effective reproduction number. If unknown, provide an `NA` value; in this case ECHO-C will not be computed.
#' @param time_scale String indicating the time scale that `tree` is measured in. 
#' Can be either `"days"` or `"years"`. Default value is `"years"`.
#' @param CI The desired coverage probability of the confidence intervals. Must be a single value between 0 and 1. Default value is 0.95.
#' 
#' @return An object of class `echo`. Contains the following elements:
#' - `n_trees`: The number of input trees.
#' - `n_samples`: The number of samples, equal to the number of tips.
#' - `A`: A list containing the point estimate(s) and confidence interval(s) corresponding to ECHO-A (see Details).
#' - `B`: A list containing the point estimate(s) and confidence interval(s) corresponding to ECHO-B (see Details).
#' - `C`: A list containing the point estimate(s) and confidence interval(s) corresponding to ECHO-C (see Details).
#' 
#' @examples
#' tree <- ape::read.tree("../data-raw/example_tree.nwk")
#' result <- echo(tree, n_samples=31, latent_duration=10, infectious_duration=8, Re=2, time_scale="days")
#' 
#' result
#' 
#' summary(result)
#' 
#' @export
echo <- function(tree, n_samples, latent_duration, infectious_duration, Re, time_scale="years", CI=0.95) {
  ## PROCESS INPUTS
  
  # All epi parameters must be positive numbers
  if(!is.numeric(latent_duration) || is.na(latent_duration) || latent_duration <= 0)
    stop("latent_duration must be positive.")
  
  if(!is.numeric(infectious_duration) || is.na(infectious_duration) || infectious_duration <= 0)
    stop("infectious_duration must be positive.")
  
  if(!is.numeric(Re) || any(Re <= 0))
    stop("Re must be positive.")
  
  if(!(time_scale=="days" || time_scale=="years"))
    stop("time_scale must be 'days' or 'years'.")
  
  # Check tree input type and check if multiple trees
  if(is.numeric(tree)) {
    n_trees <- length(tree)
  } else{
    if(inherits(tree, c("phylo", "treedata"))) {
      n_trees <- 1
    } else if(inherits(tree, "multiPhylo")) {
      n_trees <- length(tree)
    } else if(is.character(tree)) {
      tree <- read_newick(tree)
      n_trees <- 1
    } else{
      stop("Input tree not valid type. Please see documentation.")
    }
  }
  
  if(length(Re) < n_trees)
    Re <- rep(Re, n_trees)
  
  # Check how the tree was input
  if(n_trees == 1) {
    if(is.numeric(tree)) {
      L <- ifelse(time_scale=="days", tree, tree*362.25)
    } else if(inherits(tree, "phylo")) {
      L <- length_from_phylo(tree, time_scale)
    } else if(inherits(tree, "echo_tree")) {
      L <- ifelse(time_scale=="days", sum(tree$edge$length), sum(tree$edge$length)*365.25)
    } else {
      L <- length_from_treedata(tree, time_scale)
    }
  }
  else {
    if(is.numeric(tree)) {
      if(time_scale=="days") {
        L <- tree
      } else {
        L <- lapply(tree, `*`, 365.25)
      }
    } else if(inherits(tree, "multiPhylo")) {
      L <- lapply(tree, length_from_phylo, time_scale=time_scale)
    } else {
      L <- lapply(tree, length_from_treedata, time_scale=time_scale)
    }
  }
  
  ## RUN ECHO
  if(n_trees==1) {
    A_est <- MLE_A(L, latent_duration, infectious_duration)
    B_est <- MLE_B(L, n_samples, latent_duration, infectious_duration)
    C_est <- MLE_C(L, n_samples, latent_duration, infectious_duration, Re)
    
    res <- list(A=list(point=A_est, CI=CI_A(A_est, 1-CI)), 
                B=list(point=B_est, CI=CI_B(B_est, n_samples, 1-CI)),
                C=list(point=C_est, CI=CI_C(C_est, n_samples, 1-CI)))
  } else {
    A_list <- sapply(L, MLE_A, thetaL=latent_duration, thetaI=infectious_duration)
    B_list <- sapply(L, MLE_B, n_obs=n_samples, thetaL=latent_duration, thetaI=infectious_duration)
    C_list <- sapply(seq_along(L), function(i) MLE_C(L[[i]], n_obs=n_samples, thetaL=latent_duration, thetaI=infectious_duration, Re=Re[i]))
    
    A_int <- sapply(A_list, CI_A, alpha=1-CI)
    B_int <- sapply(B_list, CI_B, n_obs=n_samples, alpha=1-CI)
    C_int <- sapply(C_list, CI_C, n_obs=n_samples, alpha=1-CI)
    
    res <- list(A=list(point=A_list, CI=A_int),
                B=list(point=B_list, CI=B_int),
                C=list(point=C_list, CI=C_int))
  }
  
  ## OUTPUT
  res$n_trees <- n_trees
  res$n_samples <- n_samples
  class(res) <- "echo"
  
  return(res)
}

# Extract total branch length from an object of class "phylo"
length_from_phylo <- function(tree, time_scale) {
  L <- sum(tree$edge.length)
  if(time_scale == "years") {
    return(L * 365.25)
  } else{
    return(L)
  }
}

# Extract total branch length from an object of class "treedata"
length_from_treedata <- function(tree, time_scale) {
  L <- sum(tree@phylo$edge.length)
  if(time_scale == "years") {
    return(L * 365.25)
  } else{
    return(L)
  }
}


# Estimator A -- latent only
MLE_A <- function(total_length, thetaL, thetaI) {
  L <- total_length * thetaL / (thetaI + thetaL)
  m0 <- max(L/thetaL, 0)
  res <- stats::optim(m0, function(m) -stats::dgamma(L, shape=m, scale=thetaL, log=T), lower=1, method="L-BFGS-B")
  return(res$par)
}


# Estimator B -- latent + infectious
MLE_B <- function(total_length, n_obs, thetaL, thetaI) {
  theta <- (thetaL + thetaI) / 2
  res <- stats::optim(1, function(m) -stats::dgamma(total_length, shape=2*m, scale=theta, log=T), lower=1, method="L-BFGS-B")
  return(res$par)
}


# Estimator C -- infectious only
MLE_C <- function(total_length, n_obs, thetaL, thetaI, Re) {
  if(is.na(Re))
    return(NA)
  L <- total_length * thetaI / (thetaL + thetaI)
  theta <- thetaI / (1 + Re)
  m0 <- max(L/theta, 1)
  res <- stats::optim(m0, function(m) -stats::dgamma(L, shape=m, scale=theta, log=T), lower=1, method="L-BFGS-B")
  return(res$par-n_obs)
}

# 
CI_A <- function(n, alpha) n + stats::qnorm(1-alpha/2) / sqrt(trigamma(n)) * c(-1, 1)
CI_B <- function(N, n_obs, alpha) N + stats::qnorm(1-alpha/2) / sqrt(trigamma(N+n_obs+(N-n_obs)/2)) * c(-1, 1)
CI_C <- function(n, n_obs, alpha){
  if(is.na(n))
    return(c(NA, NA))
  return(n + stats::qnorm(1-alpha/2) / sqrt(trigamma(n+n_obs)) * c(-1, 1))
}