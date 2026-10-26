#' Estimate infection ascertainment using ECHO
#' 
#' This function is a preliminary implementation of the ascertainment estimator that is currently in development.
#' It has not been peer-reviewed yet. Supporting technical documents will be added soon.
#' 
#' @param echo_results An object of class `echo`.
#' @param heights The height of the input tree(s) in years. If multiple trees are represented, should be stored in a vector.
#' @param toi_distribution List containing the TOI distribution and its required inputs.
#' - `fn` is a functional form, must be of the format `fn(t, pars)`. `fn` must be vectorized with respect to `t`. 
#' - `pars` can be any format as long as it is compatible with `fn`.
#' @param offspring_distribution List containing the mean (`Re`) and dispersion (`k`) parameters of a negative binomial offspring (secondary case count) distribution.
#' @param generation_interval List containing the parameters of the generation interval distribution. 
#' This is a gamma distribution and can be specified with either `shape` and `scale` or `mean` and `var`. 
#' If both are provided, `shape` and `scale` will be used.
#' @param sampling_delay List containing the parameters of the sampling delay distribution.
#' This is a gamma distribution and can be specified with either `shape` and `scale` or `mean` and `var`. 
#' If both are provided, `shape` and `scale` will be used.
#' 
#' @export
estimate_ascertainment <- function(echo_results,
                                   heights,
                                   toi_distributions, 
                                   offspring_distribution,
                                   generation_interval,
                                   sampling_delay,
                                   n_steps=500) {
  # echo_fit <- echo(total_length, n_seq, pars$thetaL, pars$thetaI, pars$Re)
  n_seq <- echo_results$n_samples
  pE <- sapply(echo_results$A$point, function(x) n_seq / x)
  # pE <- n_seq / echo_fit$A$point
  
  # compute derived quantities for the ascertainment estimation:
  if (!all(c("shape", "scale") %in% names(generation_interval))) {
    if (!all(c("mean", "var") %in% names(generation_interval)))
      stop("generation_interval must contain either shape and scale or mean and var.")
    generation_interval$shape <- generation_interval$mean^2 / generation_interval$var
    generation_interval$scale <- generation_interval$var / generation_interval$mean
  }
  
  if (!all(c("shape", "scale") %in% names(sampling_delay))) {
    if (!all(c("mean", "var") %in% names(sampling_delay)))
      stop("sampling_delay must contain either shape and scale or mean and var.")
    sampling_delay$shape <- sampling_delay$mean^2 / sampling_delay$var
    sampling_delay$scale <- sampling_delay$var / sampling_delay$mean
  }
  
  fits <- future.apply::future_mapply(ascertain_one,
                                      heights, pE, toi_distributions,
                                      MoreArgs = list(offspring = offspring_distribution,
                                                      generation = generation_interval,
                                                      sampling = sampling_delay,
                                                      n_steps = n_steps),
                                      SIMPLIFY = FALSE)
  pi <- sapply(fits, `[[`, "pi")
  mean_pi <- sapply(fits, `[[`, "Epi")
  N <- sapply(mean_pi, function(x) n_seq / x)

  return(data.frame(N=N, Epi=mean_pi, pi=pi, pE=pE))
}

ascertain_one <- function(height, pE, toi, offspring, generation, sampling, n_steps) {
  pi <- solve_Epi(height, pE, toi, offspring, generation, sampling, n_steps)
  list(pi = pi, Epi = Epi(pi, height, toi, sampling))
}

# PGF for the negative binomial distribution with mean Re and dispersion parameter k
G <- Vectorize(function(p, Re, k) {
  return((1 + (Re/k)*(1-p))^-k)
})


# function to approximate E[p0] throughout time for a given value of pi
Ep0t <- Vectorize(function(pi, height, toi, generation, offspring, sampling, n_steps) {
  # initialize vectors for omega (p0(t)) and omegabar (the integral approximation)
  omega <- rep(NA, n_steps+1); omega[1] <- 1
  omegabar <- rep(NA, n_steps+1); omegabar[1] <- 1
  
  # the size of the time steps
  dt <- height / n_steps
  
  # the coefficients for the trapezoid rule
  coef <- c(0.5, rep(1,n_steps-1), 0.5)
  
  # pre-compute the generation distribution density at each time step
  dgammastore <- stats::dgamma(dt*(1:n_steps), shape=generation$shape, scale=generation$scale)
  
  # compute the time density as the exponential grown function divided by a normalization constant
  # f_eval <- exp(r*(depth - dt*(1:n_steps))) / ((exp(r*depth)-1)/r)
  f_eval <- toi$fn(dt*(0:n_steps), toi$pars)
  
  # initialize the expectation vector -- E[p0(0)] = 1*dt*ft
  # E <- dt*f_eval[1]
  
  # compute the value of p0(t) for each time step
  for(i in 1:n_steps) {
    omegabar[i+1] <- min(1, sum(coef[1:i]*dgammastore[seq(i,1,-1)]*omega[1:i]*dt)+1-stats::pgamma(i*dt,shape=generation$shape,scale=generation$scale))
    omega[i+1] <- (1-pi*stats::pgamma(i*dt,shape=sampling$shape,scale=sampling$scale)) * G(omegabar[i+1], offspring$Re, offspring$k)
    # E <- E + (omega[i+1]*f_eval[i+1]+omega[i]*f_eval[i])/2*dt
  }
  
  return(sum(coef*omega*f_eval*dt))
}, vectorize.args="pi")


# computes E[pi(t)] wrt. infection time
# pi: pi0 (time-limit of pi(t))
# ft: density of the time of infection
# t_max: total depth of the tree in days
Epi <- Vectorize(function(pi, height, toi, sampling) {
  f <- function(t) toi$fn(t, toi$pars) * stats::pgamma(t, shape=sampling$shape, scale=sampling$scale)
  return(pi * stats::integrate(f, lower=0, upper=height, subdivisions=1000)$value)
}, vectorize.args = c("pi", "height"))


# solves for pi by solving the equation E[pi] = pE(1-E[p0])
solve_Epi <- Vectorize(function(height, pE, toi, offspring, generation, sampling, n_steps) {
  f <- function(pi) {
    LHS = Epi(pi, height, toi, sampling)
    RHS = pE*(1-Ep0t(pi, height, toi, generation, offspring, sampling, n_steps))
    return(LHS - RHS)
  }
  
  tryCatch(stats::uniroot(f, lower=0.001, upper=0.999)$root,
           error = function(e) {
             warning("uniroot failed to find a solution for pi (height = ", height,
                     ", pE = ", pE, "); returning NA. Original error: ",
                     conditionMessage(e), call. = FALSE)
             NA_real_
           })
}, vectorize.args = "height")
