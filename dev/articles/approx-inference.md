# Approximate Bayesian inference in epidist

## 1 Background

The `epidist` package uses Bayesian inference to estimate delay
distributions and other quantities. Doing Bayesian inference amounts to
approximating the posterior distribution of each parameter in the
statistical model[¹](#fn1). A range of methods exist to perform this
approximation.

By default, `epidist` uses the No-U-Turn Sampler \[NUTS; Hoffman,
Gelman, et al. ([2014](#ref-hoffman2014no))\] Hamiltonian Monte Carlo
(HMC) algorithm. NUTS is an example of a broader class of Markov chain
Monte Carlo (MCMC) methods. These methods work by simulating from a
Markov chain with the target posterior distribution as its stationary
distribution. When MCMC algorithms are run for sufficiently many
iterations, and have reached convergence, the samples can be treated as
being drawn from the posterior distribution. Relevant posterior
quantities such as expectations may then be computed using these
samples. A drawback of MCMC methods, like NUTS, is that simulations can
be quite computational intensive, especially for complex models or large
data.

The `epidist` package is built using `brms` ([Bürkner 2017](#ref-brms)),
which stands for “Bayesian Regression Models using Stan”, where Stan
([Carpenter et al. 2017](#ref-carpenter2017stan)) is a probabilistic
programming language. Although NUTS is the primary inference algorithm
used in Stan, additional options are available. These additional
inference algorithms are also available in `epidist` due to its
dependence on `brms`.

In this vignette, we first briefly describe the alternative algorithms
available (Section [2](#other)) as well as directing you to more
detailed resources. Then (Section [3](#demo)) we demonstrate their
application to fitting simulated data, before extracting and comparing
posterior distributions. By comparing the resulting inferences to those
from NUTS, we hope to help you make informed decisions about which
algorithm to use in your applied problem.

## 2 Alternative approximate inference algorithms

Here we describe three alternative approximate Bayesian inference
algorithms that are available to use in `brms`, and therefore also
available in `epidist`. It’s worth noting that further inference
algorithms may have become available since this vignette was last
updated. Please check `brms` package updates if interested!

### 2.1 Laplace method

The Laplace method approximates a posterior distribution by a Gaussian
distribution centered (by default) at the posterior mode. In Stan, the
Gaussian approximation is constructed on the unconstrained parameter
space (as the domain of a Gaussian distribution is the real line).
Samples from the Gaussian approximation may then be transformed to the
constrained parameter space. To access the Laplace method, specify
`algorithm = "laplace"` within
[`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html). See
the section [Laplace
Sampling](https://mc-stan.org/docs/cmdstan-guide/laplace_sample_config.html)
of the `CmdStan` User’s Guide for more information.

### 2.2 Variational inference using ADVI

Automatic differentiation variational inference \[ADVI; Kucukelbir et
al. ([2017](#ref-kucukelbir2017advi))\] is a type of variational
inference \[VI; Blei, Kucukelbir, and McAuliffe
([2017](#ref-blei2017variational))\] algorithm. VI works by restricting
to a family of distributions, and then selecting the member of that
family which is the most similar to the posterior distribution. Most
commonly, and in Stan, (dis-)similarity is measured using the
Kullback–Leibler (KL) divergence. There are two options for the family
of distributions, either a fully factorised Gaussian with
`algorithm = "meanfield"` or a Gaussian with a full-rank covariance
matrix with `algorithm = "fullrank"`. See the section [Variational
Inference using
ADVI](https://mc-stan.org/docs/cmdstan-guide/variational_config.html) of
the `CmdStan` User’s Guide for more information.

### 2.3 Pathfinder

Pathfinder is a method closely related to variational inference, which
has been more recently developed by Zhang et al.
([2022](#ref-zhang2022pathfinder)). It works by generating Gaussian
approximations along each step of an iterative optimisation algorithm
(such as L-BFGS). The KL divergence from each approximation to the
posterior is measured, with the best approximation chosen. Pareto
smoothed importance sampling \[PSIS; Vehtari et al.
([2015](#ref-vehtari2015pareto))\] is optionally used to resample draws
from the chosen Gaussian distribution. When multiple paths are specified
(using `num_paths`) then the Pathfinder algorithm is run multiple times,
initialising the optimisation at different points. The resulting
approximation is a mixture of Gaussian distributions, rather than a
single Gaussian distribution. See the section [Pathfinder Method for
Approximate Bayesian
Inference](https://mc-stan.org/docs/cmdstan-guide/pathfinder_config.html)
of the `CmdStan` User’s Guide for more information.

## 3 Demonstration

In this demonstration, we use the following packages:

``` r
library(epidist)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(tidybayes)
library(cmdstanr) # nolint
```

To access the approximate inference methods used in this vignette we
will need to use the `cmdstanr` backend for `brms` (we generally
recommend using this backend for fitting models). To do this, we first
need to install CmdStan (see the README for more details). We can check
we have everything we need as follows:

``` r
cmdstanr::cmdstan_version()
```

We can simulate data to use for fitting models. The example data
simulation process follows that used in the [Getting started with
epidist](https://epidist.epinowcast.org/articles/epidist.html#data)
vignette, so we will not detail exactly what is happening here, but
please consult that vignette if interested:

Click to expand for data simulation code

``` r
meanlog <- 1.8
sdlog <- 0.5
obs_time <- 25
sample_size <- 200

obs_cens_trunc_samp <- simulate_gillespie(seed = 101) |>
  simulate_secondary(
    meanlog = meanlog,
    sdlog = sdlog
  ) |>
  mutate(
    ptime_lwr = floor(.data$ptime),
    ptime_upr = .data$ptime_lwr + 1,
    stime_lwr = floor(.data$stime),
    stime_upr = .data$stime_lwr + 1,
    obs_time = obs_time
  ) |>
  filter(.data$stime_upr <= .data$obs_time) |>
  slice_sample(n = sample_size, replace = FALSE)
```

We now prepare the data for fitting with the marginal model, and perform
inference with HMC:

``` r
linelist_data <- as_epidist_linelist_data(
  obs_cens_trunc_samp$ptime_lwr,
  obs_cens_trunc_samp$ptime_upr,
  obs_cens_trunc_samp$stime_lwr,
  obs_cens_trunc_samp$stime_upr,
  obs_time = obs_cens_trunc_samp$obs_time
)

data <- as_epidist_marginal_model(linelist_data)

t <- proc.time()
fit_hmc <- epidist(data = data, algorithm = "sampling", backend = "cmdstanr")
time_hmc <- proc.time() - t
```

Note that for clarity above we specify `algorithm = "sampling"`, but if
you were to call `epidist(data = data)` the result would be the same
since `"sampling"` (i.e. HMC) is the default value for the `algorithm`
argument.

Now, we fit[²](#fn2) the same marginal model using each method in
Section [2](#other). To match the four Markov chains of length 1000 in
HMC above, we then draw 4000 samples from each approximate posterior.

``` r
t <- proc.time()
fit_laplace <- epidist(
  data = data, algorithm = "laplace", draws = 4000, backend = "cmdstanr"
)
time_laplace <- proc.time() - t

t <- proc.time()
fit_advi <- epidist(
  data = data, algorithm = "meanfield", draws = 4000, backend = "cmdstanr"
)
time_advi <- proc.time() - t
```

For the Pathfinder algorithm we will set `num_paths = 1`. Although both
the Laplace and ADVI methods ran without problems in all cases during
testing, we found that Pathfinder often produced the error message
“Error evaluating model log probability: Non-finite gradient.” Although
a `save_single_paths` option is available, which may have allowed
recovery of individual Pathfinder paths (and therefore removing faulty
paths), it does not appear to be working currently[³](#fn3).

``` r
t <- proc.time()
fit_pathfinder <- epidist(
  data = data, algorithm = "pathfinder", draws = 4000, chains = 1,
  backend = "cmdstanr"
)
time_pathfinder <- proc.time() - t
```

We now extract the posterior distribution for the delay parameters from
the fitted model for each inference method. Thankfully, each algorithm
is implemented to sample draws from the posterior distribution, making
post-processing straightforward.

Click to expand for code to extract posterior draws

``` r
fits <- list(
  HMC = fit_hmc,
  Laplace = fit_laplace,
  ADVI = fit_advi,
  Pathfinder = fit_pathfinder
)

draws <- imap(fits, function(fit, name) {
  draws <- fit |>
    predict_delay_parameters() |>
    as.data.frame() |>
    pivot_longer(
      cols = c("mu", "sigma", "mean", "sd"),
      names_to = "parameter",
      values_to = "value"
    ) |>
    filter(parameter %in% c("mu", "sigma")) |>
    mutate(method = as.factor(name))
  return(draws)
})

draws <- bind_rows(draws)
```

### 3.1 Comparison of parameter posterior distributions

The mean estimated value of each parameter, from each method, is as
follows.

``` r
pars <- draws |>
  group_by(method, parameter) |>
  summarise(value = mean(value)) |>
  pivot_wider(names_from = parameter, values_from = value) |>
  ungroup()

pars
```

More comprehensively, the estimated posterior distributions are shown in
Figure [**??**](#fig:posterior).

Click to expand for code to create posterior distribution plot

``` r
p_posterior <- draws |>
  ggplot(aes(x = value, col = method)) +
  stat_slabinterval(density = "histogram", breaks = 30, alpha = 0.8) +
  scale_colour_manual(values = c("#56B4E9", "#009E73", "#E69F00", "#CC79A7")) +
  facet_grid(method ~ parameter, scales = "free_x") +
  theme_minimal() +
  guides(fill = "none") +
  labs(x = "", y = "", col = "Method") +
  theme(legend.position = "bottom")
```

``` r
p_posterior
```

### 3.2 Comparison of resulting delay distributions

Figure [**??**](#fig:delay-pdf) shows how the different `mu` and `sigma`
posterior mean estimates from each inference method alter an estimated
delay distribution.

Click to expand for code to create delay PDF plot

``` r
p_delay_pdf <- pmap_df(
  filter(pars), ~ tibble(
    x = seq(0, 25, by = 0.1),
    method = ..1, density = dlnorm(x, ..2, ..3)
  )
) |>
  ggplot(aes(x = x, y = density, col = method)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#56B4E9", "#009E73", "#E69F00", "#CC79A7")) +
  theme_minimal() +
  labs(x = "", y = "", col = "Method") +
  theme(legend.position = "bottom")
```

``` r
p_delay_pdf
```

### 3.3 Comparison of time taken

In this example, HMC took a longer time to run than the other methods
and Pathfinder was the fastest running method. That said, even for HMC
the computation time in this case is unlikely to be prohibitive.

``` r
times <- list(
  HMC = time_hmc,
  Laplace = time_laplace,
  ADVI = time_advi,
  Pathfinder = time_pathfinder
)

times
```

## 4 Conclusion

The range of alternative approximation algorithms available, and their
ease of use, is an attractive feature of `brms`. We found that these
algorithms do produce reasonable approximations in far less time than
HMC. Of course, this vignette only includes one example, and a more
thorough investigation would be required to make specific
recommendations. That said, currently we do not recommend use of the
Pathfinder algorithm due to its unstable performance in our testing and
early stage software implementation.

### References

Blei, David M, Alp Kucukelbir, and Jon D McAuliffe. 2017. “Variational
Inference: A Review for Statisticians.” *Journal of the American
Statistical Association* 112 (518): 859–77.

Bürkner, Paul-Christian. 2017. “brms: An R Package for Bayesian
Multilevel Models Using Stan.” *Journal of Statistical Software* 80 (1):
1–28. <https://doi.org/10.18637/jss.v080.i01>.

Carpenter, Bob, Andrew Gelman, Matthew D Hoffman, Daniel Lee, Ben
Goodrich, Michael Betancourt, Marcus A Brubaker, Jiqiang Guo, Peter Li,
and Allen Riddell. 2017. “Stan: A Probabilistic Programming Language.”
*Journal of Statistical Software* 76.

Hoffman, Matthew D, Andrew Gelman, et al. 2014. “The No-u-Turn Sampler:
Adaptively Setting Path Lengths in Hamiltonian Monte Carlo.” *J. Mach.
Learn. Res.* 15 (1): 1593–623.

Kucukelbir, Alp, Dustin Tran, Rajesh Ranganath, Andrew Gelman, and David
M. Blei. 2017. “Automatic Differentiation Variational Inference.”
*Journal of Machine Learning Research* 18 (14): 1–45.
[http://jmlr.org/papers/v18/16-107.html](http://jmlr.org/papers/v18/16-107.md).

Vehtari, Aki, Daniel Simpson, Andrew Gelman, Yuling Yao, and Jonah
Gabry. 2015. “Pareto Smoothed Importance Sampling.” *arXiv Preprint
arXiv:1507.02646*.

Zhang, Lu, Bob Carpenter, Andrew Gelman, and Aki Vehtari. 2022.
“Pathfinder: Parallel Quasi-Newton Variational Inference.” *Journal of
Machine Learning Research* 23 (306): 1–49.
[http://jmlr.org/papers/v23/21-0889.html](http://jmlr.org/papers/v23/21-0889.md).

------------------------------------------------------------------------

1.  We are currently developing a vignette explaining the statistical
    model in more detail!

2.  Note that in this section, and above for the MCMC, the output of the
    call is hidden, but if you were to call these functions yourself
    they would display information about the fitting procedure as it
    occurs

3.  See <https://github.com/stan-dev/cmdstanr/issues/878>
