# Guide to the statistical models implemented in epidist

The `epidist` package enables users to estimate delay distributions
while accounting for common reporting biases. This vignette first
introduces the background ([Park et al. 2024](#ref-park2024estimating))
required to understand the models implemented in `epidist`. It then goes
on to explain each model in turn.

## 1 Background

Estimating a delay distribution may appear to be simple: one could
imagine fitting a probability distribution to a set of observed delays.
However, observed delays are often biased during an ongoing outbreak. We
begin by presenting a formalism for characterizing delay distributions
as well as two main biases (truncation and censoring) affecting them. We
then present statistical models for correcting for these biases.

Any epidemiological delay requires a primary (starting) and a secondary
(ending) event. For example, the incubation period measures the time
between infection (primary event) and symptom onset (secondary event).
Here, we use \\p\\ to denote the time of the primary event, and \\s\\ to
denote the time of the secondary event.

### 1.1 Forwards and backwards distributions

We can measure any delay distribution in two different ways. First, we
can measure the forward distribution \\f_p(\tau)\\, starting from a
cohort of individuals who experienced the primary event at the same time
\\p\\ and looking at when they experienced their secondary event.
Second, we can measure the backward distribution \\b_s(\tau)\\, starting
from a cohort of individuals who experienced the secondary event at the
same time \\s\\ and looking at when they experienced their primary
event. While the length of each individual delay \\\tau = p-s\\ remains
constant whether we look at it forward or backwards, the shape of the
distribution is affected by the differences in perspectives due to the
differences in cohort composition.

To illustrate their differences, let’s assume that primary and secondary
events occur at rates, or equivalently incidences, \\\mathcal{P}(p)\\
and \\\mathcal{S}(s)\\, respectively. Then, the total density
\\\mathcal{T}(p, s)\\ of individuals with a primary event at time \\p\\
and secondary event at time \\s\\ can be expressed equivalently in terms
of both forward and backward distributions: \\ \mathcal{T}(p, s) =
\mathcal{P}(p) f_p(s - p) = \mathcal{S}(s) b_s(s - p). \tag{1.1} \\
Rearranging Equation [(1.1)](#eq:forwards-backwards) gives \\ b_s(\tau)
= \frac{\mathcal{P}(s - \tau) f\_{s - \tau}(\tau)}{\mathcal{S}(s)}.
\tag{1.2} \\ The denominator of Equation
[(1.2)](#eq:forwards-backwards2), which corresponds to the incidence of
secondary events, may be expressed as the integral over all possible
delays \\ \mathcal{S}(s) = \int\_{-\infty}^\infty \mathcal{P}(s - \tau)
f\_{s - \tau}(\tau) \text{d} \tau, \\ such that \\ b_s(\tau) =
\frac{\mathcal{P}(s - \tau) f\_{s - \tau}(\tau)}{\int\_{-\infty}^\infty
\mathcal{P}(s - \tau) f\_{s - \tau}(\tau) \text{d} \tau}. \\ Here, we
see that \\b_s(\tau)\\ depends not only on \\f\_{s - \tau}(\tau)\\ but
also on \\\mathcal{P}(s - \tau)\\, meaning that past changes in the
incidence pattern will affect the shape of the distribution.
Particularly, when an epidemic is growing, we are more likely to observe
shorter delays, causing an underestimation of the mean delay. Therefore,
we always want to characterize epidemiological delays from the forward
perspective and estimate the forward distribution. For this reason, our
current methodology focuses on biases that affect the estimation of the
forward distribution.

### 1.2 Right truncation

One key bias that affects the forward distribution is right truncation.
Right truncation refers to the bias arising from the inability to
observe future events and occurs when we observe data based on the
secondary events. For example, assume the data are right truncated and
we don’t observe secondary events past time \\T\\. Then, we will only
observe delays whose secondary events occurred before time \\T\\,
causing us to underestimate the mean of the distribution as these delays
will on average be shorter.

Bias from right truncation is greater when events are more likely to be
more recent. A common example of severely right truncated data is data
collected during outbreaks when growth in incidence is exponential (so
you are much more likely to have a recent event). On the other hand, if
data collection is continued until the end of an outbreak then many
fewer events are likely to be more recent and so there will be little
right truncation in general.

Mathematically right truncation can be described as follows. Let \\P\\
and \\S\\ be random variables. Let \\F_p\\ be the forward cumulative
distribution. Then, the probability of observing a delay of length
\\\tau\\ given that the primary event occurred at time \\p\\ and a
truncation at time \\T\\ can be written as: \\ \begin{aligned}
\mathbb{P}(S = P + \tau \\ \| \\ P = p, S \< T) &= \frac{\mathbb{P}(S =
P + \tau, P = p, S \< T)}{\mathbb{P}(P = p, S \< T)} \\ &=
\frac{\mathbb{P}(S = P + \tau \< T \\ \| \\ P = p)\mathbb{P}(P =
p)}{\mathbb{P}(S \< T \\ \| \\ P = p)\mathbb{P}(P = p)} \\ &=
\frac{\mathbb{P}(S = P + \tau \< T \\ \| \\ P = p)}{\mathbb{P}(S \< T \\
\| \\ P = p)} \\ &= \frac{f_p(\tau)}{\int_0^{T - p} f_p(x) \text{d}x} =
\frac{f_p(\tau)}{F_p(T - p)}, \quad p + \tau \< T. \end{aligned} \\

Examining this equation illustrates that (right) truncation renormalises
the density by the values which are possible. For example, if the
distribution \\x \sim \text{Unif}(0, 1)\\ were right truncated by \\T =
0.5\\ then \\x \sim \text{Unif}(0, 0.5)\\.

### 1.3 Interval censoring

The exact timing of epidemiological events is often unknown. Instead, we
may only know that the event happened within a certain interval. We
refer to this as interval censoring. A very common example of interval
censoring in epidemiology is date censoring, where we only know, or are
using, data to the day of an event rather than the precise time. Other
forms of interval censoring, like weekly or monthly interval censoring,
are also common. When both primary and secondary events are interval
censored, this is referred to as double censoring.

Mathematically single interval censoring is defined as follows. Assume
the secondary event \\S\\ is censored and so we don’t know when the
event exactly happened. Instead, we only know that the secondary event
happened between \\S_L\\ and \\S_R\\. Then, \\ \begin{aligned}
\mathbb{P}(S_L \< S \< S_R \\ \| \\ P = p) &= \int\_{S_L}^{S_R} f_p(y-p)
dy\\ &= F_p(S_R-p) - F_p(S_L-p) \end{aligned} \\

Similarly, double interval censoring is defined as follows. Now, assume
that both the primary \\P\\ and secondary \\S\\ events are truncated. We
only know that the primary event happened between \\P_L\\ and \\P_R\\
and the secondary event happened between \\S_L\\ and \\S_R\\. We now
write \\g_P\\ to denote the unconditional distribution of primary
events. Then, \\ \begin{aligned} \mathbb{P}(S_L \< S \< S_R \\ \| \\ P_L
\< P \< P_R) &= \mathbb{P}(P_L \< P \< P_R, S_L \< S \< S_R \\ \| \\ P_L
\< P \< P_R)\\ &= \frac{\mathbb{P}(P_L \< P \< P_R, S_L \< S \<
S_R)}{\mathbb{P}(P_L \< P \< P_R)}\\ &= \frac{\int\_{P_L}^{P_R}
\int\_{S_L}^{S_R} g_P(x) f_x(y-x) \\dy\\ dx}{\int\_{P_L}^{P_R} g_P(z)\\
dz }\\ &= \int\_{P_L}^{P_R} \int\_{S_L}^{S_R} g_P(x\\\|\\P_L,P_R)
f_x(y-x) \\dy\\ dx \end{aligned} \\ where \\g_P(x\\\|\\P_L,P_R)\\
represents the conditional distribution of primary event given lower
\\P_L\\ and upper \\P_R\\ bounds.

## 2 The naive model

The simplest approach to modelling epidemiological delay distributions
is ignoring truncation and censoring biases and simply treating the
delays as continuous fully observed data. Then, the likelihood of
observing a delay \\\mathbf{Y}\_i\\ given parameter \\\mathbf{\theta}\\
is straightforward: \\ \mathcal{L}(\mathbf{Y}\_i \\ \| \\
\mathbf{\theta}) = f(y_i - x_i). \\ where \\y_i\\ and \\x_i\\ are the
observed primary and secondary event times.

As shown in ([Park et al. 2024](#ref-park2024estimating)) when the data
is double censored this modelling approach biases the mean by
approximately a day as well as the standard deviation. Where right
truncation is also present biases can be more severe with plausible
simulated scenarios leading to biased means that were \>30% shorter than
the true distributions.

## 3 The latent model

This approach aims to account for the right truncation and double
censoring using a generative modelling approach. For each event, a
latent variable is used to represent the exact time of the event. This
then allows the modelling of the continuous distribution, adjusted for
the right truncation. Whilst this is an approximation ([Park et al.
2024](#ref-park2024estimating)) showed good recovery of simulated
distributions in a range of settings. However, the use of two latent
variables per observed delay means that this approach may scale poorly
with larger datasets. That being said this approach has been used
successfully in multiple real-world outbreak settings (([Ward et al.
2022](#ref-ward2022transmission))). If using the latent model, please
cite Park et al. ([2024](#ref-park2024estimating)) in addition to
`epidist`.

Mathematically this model is described as follows. We look at the
conditional probability that the secondary event \\S\\ falls between
\\S_L\\ and \\S_R\\, given that the primary event \\P\\ falls between
\\P_L\\ and \\P_R\\ and that the secondary event \\S\\ occurs before the
truncation time \\T\\: \\ \begin{aligned} &\mathbb{P}(S_L \< S \< S_R \\
\| \\ P_L \< P \< P_R, S\<T)\\ &= \mathbb{P}(P_L \< P \< P_R, S_L \< S
\< S_R, S\<T \\ \| \\ P_L \< P \< P_R, S\<T)\\ &= \frac{\mathbb{P}(P_L
\< P \< P_R, S_L \< S \< S_R, S\<T)}{\mathbb{P}(P_L \< P \< P_R,
S\<T)}\\ &= \frac{\int\_{P_L}^{P_R} \int\_{S_L}^{S_R} g_P(x) f_x(y-x)
\\dy\\ dx}{\int\_{P_L}^{P_R} \int\_{z}^T g_P(z) f_z(w-z) \\ dz \\dw }\\
&= \frac{\int\_{P_L}^{P_R} \int\_{S_L}^{S_R} g_P(x) f_x(y-x) \\dy\\
dx}{\int\_{P_L}^{P_R} g_P(z) F_z(T-z) \\dw }\\ &=
\frac{\int\_{P_L}^{P_R} \int\_{S_L}^{S_R} g_P(x\|P_L, P_R) f_x(y-x)
\\dy\\ dx}{\int\_{P_L}^{P_R} g_P(z\|P_L, P_R) F_z(T-z) \\dw }\\
\end{aligned} \\ Using latent variables, we can now rewrite the
observation likelihood as: \\ \begin{aligned} x_i &\sim g_P(x_i \\ \| \\
p\_{L, i}, p\_{R, i}) \\ y_i &\sim \text{Unif}(s\_{L, i}, s\_{R, i}) \\
\mathcal{L}(\mathbf{Y} \\ \| \\ \mathbf{\theta}) &= \prod_i \left\[
\frac{f\_{x_i}(y_i - x_i)}{\int\_{P\_{L, i}}^{P\_{R, i}} g_P(z \\ \| \\
p\_{L, i}, p\_{R, i}) F_z(T - z) \text{d}z} \right\]. \end{aligned} \\
As before, \\g_P(z \\ \| \\ p\_{L, i}, p\_{R, i})\\ represents the
conditional distribution of the primary event given lower \\P_L\\ and
upper \\P_R\\ bounds; this is equivalent to modelling the incidence in
primary events.

### 3.1 Bounding the latent primary event time

`epidist` samples both latent offsets on the unit scale. Write \\w\_{P,
i} = p\_{R, i} - p\_{L, i}\\ and \\w\_{S, i} = s\_{R, i} - s\_{L, i}\\
for the two censoring window widths. Then \\ \tilde{p}\_i \sim
\text{Unif}(0, 1), \qquad \tilde{s}\_i \sim \text{Unif}(0, 1), \qquad
s_i = w\_{S, i}\\ \tilde{s}\_i, \\ and the primary event offset is
ordinarily \\ p_i = w\_{P, i}\\ \tilde{p}\_i . \\

When the two censoring windows overlap the delay must still be
non-negative. The primary event therefore has to precede the sampled
secondary event. The offset is then bounded by the sampled secondary
offset rather than by the window width. \\ p_i = s_i\\ \tilde{p}\_i . \\

That upper bound is itself a parameter. The map \\\tilde{p}\_i \mapsto
p_i\\ therefore has Jacobian determinant \\ \left\| \frac{\partial
p_i}{\partial \tilde{p}\_i} \right\| = s_i , \\ Stan does not add this
term for a transformation written this way. See the [Stan user’s guide
on changes of
variables](https://mc-stan.org/docs/stan-users-guide/reparameterization.html#changes-of-variables).
The log density of these observations needs it explicitly. \\ \log
\mathcal{L}\_i \\\longmapsto\\ \log \mathcal{L}\_i + \log s_i . \\ The
non-overlapping case needs no such term because \\w\_{P, i}\\ is data,
so its Jacobian is a constant that cancels. The primary event density is
used unnormalised here. A constant normalising it over \\\[0, s_i\]\\
would itself depend on \\s_i\\.

### 3.2 A non-uniform primary event

Incidence is not flat within a reporting window during rapid growth or
decline. Setting `primary = "expgrowth"` replaces the flat primary event
with an exponentially tilted one at rate \\r\\.

The tilt is placed on the unit scale offset, with the rate scaled by
that offset’s bound \\b_i\\, which is \\w\_{P, i}\\ ordinarily and
\\s_i\\ where the windows overlap. \\ \tilde{p}\_i \sim
\text{ExpGrowth}(0, 1, r_i b_i). \\ Unlike the flat case, this density
has to be normalised. Its constant depends on \\r_i\\, which is
estimated, so dropping it would let the likelihood grow without bound in
\\r_i\\.

\\r\\ is a distributional parameter, so it takes a formula and a prior
like any other. This allows the rate to vary by covariate. The delays
carry little information about \\r\\. It is normally taken from a
separate estimate of epidemic growth and given an informative prior
centred on that value, as in Brand et al.
([2026](#ref-brand2026scalable)), rather than learned from the delays.

``` r

epidist(data, formula = bf(mu ~ 1, pgrowth ~ 1 + region))
```

This follows the implementation in Brand et al.
([2026](#ref-brand2026scalable)).

The latent model formulation follows Ward et al.
([2022](#ref-ward2022transmission)). The need for this adjustment was
identified in Funk and Abbott ([2026](#ref-bdbvlinelist)) and derived in
Brand et al. ([2026](#ref-brand2026scalable)).

## 4 The marginal model

The marginal model corrects for the same biases as the latent model but
integrates out the exact event times numerically, or analytically where
closed-form solutions exist, rather than sampling latent variables. This
approach uses the primary event censored distribution implemented in the
[`primarycensored`](https://primarycensored.epinowcast.org/) package
([Abbott et al. 2025](#ref-primarycensored)). If using the marginal
model, please cite `primarycensored` in addition to `epidist`.

Under the assumption that the forward distribution does not change
within the censoring interval (i.e. \\f_x = f\\ for \\x \in \[P_L,
P_R\]\\), the double censoring probability from Section
[1.3](#interval-censoring) simplifies to \\ \mathbb{P}(S_L \< S \< S_R
\mid P_L \< P \< P_R) = \int\_{P_L}^{P_R} g_P(x \mid P_L, P_R)
\left\[F(S_R - x) - F(S_L - x)\right\] \text{d}x. \\ For common delay
and primary event distributions, such as gamma or lognormal delays with
uniform primary events, `primarycensored` provides closed-form
analytical solutions to this integral. For other combinations, numerical
integration is used.

Right truncation at time \\T\\ is handled by normalising the likelihood
as in the latent model: \\ \mathcal{L}(\mathbf{Y} \mid \mathbf{\theta})
= \prod_i \frac{\mathbb{P}(S\_{L,i} \< S_i \< S\_{R,i} \mid P\_{L,i} \<
P_i \< P\_{R,i})}{\int\_{P\_{L,i}}^{P\_{R,i}} g_P(z \mid p\_{L,i},
p\_{R,i}) F(T - z) \\ \text{d}z}. \\

Removing the latent variables reduces the number of parameters that must
be sampled, and where analytical solutions exist the likelihood can be
evaluated without numerical integration. In addition, identical
observations can be aggregated and the likelihood computed once per
unique combination of delay, censoring windows, and covariates. Together
these make the marginal model substantially more efficient than the
latent model, particularly for larger datasets with daily-censored data
where many observations share the same structure.

For the mathematical details of primary event censored distributions,
including the survival function derivation and closed-form solutions for
specific distributions, see
[`vignette("why-it-works", package = "primarycensored")`](https://primarycensored.epinowcast.org/articles/why-it-works.html)
and
[`vignette("analytic-solutions", package = "primarycensored")`](https://primarycensored.epinowcast.org/articles/analytic-solutions.html).

## 5 The meta model

Published delay estimates are usually summary statistics, and the
estimation procedure behind them is itself a source of bias ([Charniga
et al. 2024](#ref-charniga2024best); [Park et al.
2024](#ref-park2024estimating)). The meta model fits one delay
distribution to a mix of individual level data and published summaries.
Each summary is fitted to what the study’s own procedure would have
converged to given the delay distribution, with sampling uncertainty
from the study sample size. Section [5.1](#sampling-likelihoods) gives
the likelihood of each kind of summary in terms of the moments,
distribution function and quantiles of a delay distribution. Section
[5.3](#the-biased-estimands) says which distribution those are taken
from for each study. Study level heterogeneity, for example
`mu ~ 1 + (1 | study)`, is specified through the `brms` formula as for
the other models. If using the meta model, please cite `primarycensored`
in addition to `epidist`. The meta model is experimental and its
interface may still change.

### 5.1 Sampling likelihoods

Write \\m_1\\, \\\sigma\\, \\\mu_3\\ and \\\mu_4\\ for the mean,
standard deviation and third and fourth central moments of a delay
distribution, \\\kappa = \mu_4 / \sigma^4\\ for its kurtosis, \\G\\ for
its distribution function and \\Q_p\\ for its quantile at probability
\\p\\. For a study that estimated the delay without bias these are those
of the forward delay distribution of Section [1](#maths). Section
[5.3](#the-biased-estimands) replaces them with those of the
distribution the study’s procedure targeted, and the likelihoods below
are unchanged. A study reports a value \\y\\ computed from \\n\\ delays.

#### 5.1.1 A reported mean

A reported mean is normal, \\ y \sim \text{Normal}\left(m_1, \\
\text{se}\_{m_1} \right), \tag{5.1} \\ with \\\text{se}\_{m_1}\\ the
reported standard error where given and \\\sigma / \sqrt{n}\\ otherwise.

#### 5.1.2 A reported standard deviation

A reported standard deviation is normal with the kurtosis based standard
error of a sample standard deviation, \\ y \sim
\text{Normal}\left(\sigma, \\ \text{se}\_\sigma \right), \quad
\text{se}\_\sigma = \sigma \sqrt{\frac{\kappa - 1}{4 n}}, \tag{5.2} \\
which follows from the asymptotic variance \\(\mu_4 - \sigma^4)/n\\ of
the sample variance by the delta method ([Cramér
1946](#ref-cramer1946)). The normal theory expression \\\sigma /
\sqrt{2(n-1)}\\ is the case \\\kappa = 3\\ and is two to four times too
narrow for the right skewed distributions delays follow. Equation
[(5.2)](#eq:meta-sd-lik), and Equation [(5.5)](#eq:meta-moment-pair)
built on it, should not be trusted once \\\sqrt{(\kappa - 1) / (4 n)}\\
exceeds about a quarter. For a lognormal with kurtosis 9 that is \\n\\
below about 30.
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
warns when a reported mean and standard deviation imply this under a
lognormal delay.

#### 5.1.3 A reported quantile

A reported quantile at probability \\p\\ is fitted on the probability
scale, which avoids inverting \\G\\, \\ p \sim \text{Normal}\left(G(y),
\\ \text{se}\_p\right), \tag{5.3} \\ with \\\text{se}\_p =
\sqrt{p(1-p)/n}\\ the binomial standard error of an empirical
distribution function. A quantile supplied with a standard error
\\\text{se}\_y\\ on the delay scale is fitted on that scale instead, \\
y \sim \text{Normal}\left(Q_p, \\ \text{se}\_y\right), \tag{5.4} \\
since the density that would carry \\\text{se}\_y\\ onto the probability
scale vanishes far from the implied quantile.

#### 5.1.4 Summaries from the same study

Summaries that one study computed from the same delays are correlated,
so they are fitted jointly. Two summaries are fitted together when they
agree on every field other than the summary itself, so a linear
predictor cannot vary within the group. A summary supplied with its own
standard error is fitted on its own.

A mean and a standard deviation from one study are given the asymptotic
bivariate normal of the pair, \\ \begin{pmatrix} y\_{m} \\ y\_{\sigma}
\end{pmatrix} \sim \text{Normal}\left( \begin{pmatrix} m_1 \\ \sigma
\end{pmatrix}, \\ \frac{1}{n}\begin{pmatrix} \sigma^2 & \mu_3 / (2
\sigma) \\ \mu_3 / (2 \sigma) & \sigma^2 (\kappa - 1) / 4 \end{pmatrix}
\right), \tag{5.5} \\ whose off diagonal is \\\text{Cov}(\bar{x}, s^2) =
\mu_3 / n\\ carried onto the standard deviation scale by the delta
method ([Cramér 1946](#ref-cramer1946)). The correlation is \\\gamma_1 /
\sqrt{\kappa - 1}\\ with \\\gamma_1 = \mu_3 / \sigma^3\\ the skewness,
which every distribution keeps inside \\\[-1, 1\]\\. Moments taken from
a grid or quadrature can sit just outside, so the correlation is
clipped.

Several quantiles from one study at probabilities \\p_1 \< \dots \<
p_k\\ with values \\y_1 \le \dots \le y_k\\ cut the delay axis into
\\k + 1\\ cells, and the counts in them are multinomial, \\ (c_1, \dots,
c\_{k+1}) \sim \text{Multinomial}\left(n, \\ \left(G(y_1), \\ G(y_2) -
G(y_1), \\ \dots, \\ 1 - G(y_k)\right)\right), \tag{5.6} \\ with \\c_j =
\text{round}(n p_j) - \text{round}(n p\_{j-1})\\ and \\c\_{k+1} = n -
\text{round}(n p_k)\\. A single quantile reduces this to the binomial of
which Equation [(5.3)](#eq:meta-quantile-lik) is the normal
approximation, so quantile rows of a continuous estimand use Equation
[(5.6)](#eq:meta-quantile-set) unless a standard error is supplied. Two
quantiles reported at the same value are merged into one cell. A cell
whose probability underflows to zero while the study saw delays in it is
floored at \\10^{-300}\\.

A single quantile of integer day delays is a discrete statistic. “The
median is 5 days” says that the empirical distribution function crossed
one half between 4 and 5 days, that is \\N\_{\le y - w_s} \< \lceil n p
\rceil \le N\_{\le y}\\ with \\N\_{\le y}\\ the number of delays at or
below \\y\\. It is fitted as the probability of that event, \\ P(N\_{\le
y} \ge k) - P(N\_{\le y - w_s} \ge k), \quad k = \lceil n p \rceil,
\quad N\_{\le y} \sim \text{Binomial}\left(n, G_0(y)\right), \tag{5.7}
\\ where \\w_s\\ is the reporting resolution and \\G_0\\ the step
distribution function of the discrete estimand of Section
[5.3](#the-biased-estimands), before continuity correction. The
information this carries saturates as \\n\\ grows. Several quantiles of
integer day delays use Equation [(5.6)](#eq:meta-quantile-set) on the
continuity corrected \\G\\, which is calibrated at around thirty delays
but claims a standard error five times too small at a thousand, so
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
warns about such studies.

Summaries of different kinds from one study, such as a mean and a
median, are treated as independent, which understates their joint
uncertainty. The normal approximations degrade for small study sample
sizes.

#### 5.1.5 A vector of summaries with a covariance

A study that fitted a distribution to its delays can publish draws of
the fitted parameters, which
[`as_epidist_multivariate()`](https://epidist.epinowcast.org/reference/as_epidist_multivariate.md)
summarises by their mean and covariance and
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
pushes through to the summaries the fitted distribution implies. This is
the reporting format we recommend, because it keeps the correlation
between the reported quantities. With \\y\\ the reported vector and
\\\Sigma\\ the covariance over it, \\ y \sim
\text{Normal}\left(m(\theta), \\ \Sigma\right), \tag{5.8} \\ where
\\m(\theta)\\ holds \\m_1\\ for a mean, \\\sigma\\ for a standard
deviation and \\Q_p\\ for a quantile. Summaries of a \\k\\ parameter fit
are functions of \\k\\ numbers, so at most \\k\\ may be reported with a
covariance or standard errors, and asking for more is an error.

#### 5.1.6 Reported distribution parameters

A study that published the parameters of a fitted distribution
\\\hat{F}\\ has them converted to summaries by
[`epidist_estimates_parameters()`](https://epidist.epinowcast.org/reference/epidist_estimates_parameters.md),
so the family it fitted need not match the family fitted to it. The
summaries are taken over the range of delays the study could have seen,
conditioning \\\hat{F}\\ on \\(L, D\]\\, \\ \hat{F}\_{L,D}(y) =
\frac{\hat{F}(y) - \hat{F}(L)}{\hat{F}(D) - \hat{F}(L)}, \quad L \< y
\le D, \tag{5.9} \\ with \\L\\ the smallest delay it counted and \\D\\
its observation time, or \\D = \infty\\ for a study that adjusted for
right truncation. Without this a study that did not correct for right
truncation is charged with tail spread its data never had, which can
reach tens of percent on a standard deviation. Reported parameter
standard errors are carried onto the summaries by the delta method, as
\\J V J^\top\\ with \\V\\ the diagonal matrix of squared standard errors
and \\J\\ the Jacobian of the map from parameters to summaries, and
fitted jointly through Equation [(5.8)](#eq:meta-mvn). For \\k\\
summaries of a \\k\\ parameter fit this gives back the curvature
\\V^{-1}\\, where fitting each with its own standard error would claim
standard errors 1.4 to 1.5 times too wide for a lognormal mean and
standard deviation. A full parameter covariance is better passed as
draws to
[`as_epidist_multivariate()`](https://epidist.epinowcast.org/reference/as_epidist_multivariate.md).
A study with no parameter uncertainty falls back to the sample size
likelihoods above, and any number of its summaries may be reported. This
route assumes the reported distribution has the shape of the study’s
estimand, which holds only approximately where a continuous family was
fitted to integer date differences. There, quantiles in the body of the
distribution are more reliable than a standard deviation, which depends
on a tail the study never saw.

#### 5.1.7 Individual level records

Individual level rows use the marginal model likelihood of Section
[4](#the-marginal-model) unchanged, with \\\theta\\ shared with the
summary rows. Their primary event distribution is set with `primary`, so
the tilted primary event of Section [3.2](#primary-tilt) is available to
them. Summary rows take the growth rate given for each study as a known
quantity rather than the estimated `pgrowth`, a limitation tracked in
[epinowcast/epidist#678](https://github.com/epinowcast/epidist/issues/678).
The joint likelihood is the product of the sampling likelihoods above
over the summary rows and the marginal model likelihood over the
individual level rows.

### 5.2 What we need from each study

[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
takes, for each summary:

- how the study adjusted for interval censoring, a code from 0 to 4
  defined in Section [5.3](#the-biased-estimands),
- whether it adjusted for right truncation, and if not its observation
  time, how collection stopped and the growth rate of primary events
  over the study period,
- its censoring windows \\w_p\\ and \\w_s\\, the widths of the intervals
  its primary and secondary events were observed in,
- its sample size, or a standard error or covariance in its place,
- the smallest delay it counted.

Both truncation designs assume the study sampled a cohort of primary
events, so a study that sampled on the secondary event reports a
backward distribution of Section
[1.1](#forwards-and-backwards-distributions), which is not represented
here. Systematic reviews rarely record this metadata, so it is usually
the analyst’s judgement and should be stated and varied in a sensitivity
analysis. The checklist of Charniga et al.
([2024](#ref-charniga2024best)) covers it. Where it is missing entirely,
a covariate for the phase of the outbreak makes the `brms` formula a
meta-regression that estimates the residual bias instead.

### 5.3 The biased estimands

Let \\f(\tau; \theta)\\ and \\F(\tau; \theta)\\ be the forward density
and distribution function of Section [1](#maths), and \\g_p\\ the
distribution of a primary event within its window of width \\w_p\\,
uniform under constant incidence and exponentially tilted towards more
recent times otherwise. Write \\ F\_{pc}(\tau; \theta) =
\mathbb{P}(\tau^\star + U \le \tau), \quad \tau^\star \sim f(\cdot \\ ;
\theta), \\ U \sim g_p, \\ for the primary event censored distribution
function of `primarycensored` ([Abbott et al.
2025](#ref-primarycensored)). Each censoring code below defines a
distribution, right truncated at the observation time \\D\\, with \\D =
\infty\\ for a study that adjusted for right truncation itself. Its
moments, distribution function \\G\\ and quantiles \\Q_p\\ replace those
of the delay in the likelihoods of Section [5.1](#sampling-likelihoods).
Four moments are needed, because the kurtosis sets the sampling error of
a reported standard deviation and the skewness its correlation with a
reported mean.

#### 5.3.1 Censoring adjustment

Code 0, no adjustment, summarised integer date differences directly, so
its estimand is discrete. With \\q_j\\ the probability on the \\j\\th
secondary window, \\ q_j = \frac{F\_{pc}(j w_s; \theta) - F\_{pc}((j-1)
w_s; \theta)}{F\_{pc}(w_s \lfloor D / w_s \rfloor; \theta)}, \quad j =
1, \dots, \lfloor D / w_s \rfloor, \tag{5.10} \\ where bin \\j\\ carries
the delay \\(j-1) w_s\\ and the truncation point is discretised to the
last full grid boundary. This is the doubly interval censored, right
truncated probability mass function the marginal model of Section
[4](#the-marginal-model) uses for individual observations. Summing over
the grid gives the moments, and its cumulative sum gives \\G_0\\. For
quantile rows \\G\\ is continuity corrected by interpolating \\G_0\\
linearly through the mid points of its cells, because a reported
quantile of day resolution data otherwise lands on a jump. A single
reported quantile uses Equation [(5.7)](#eq:meta-quantile-crossing) on
\\G_0\\. The reported value is itself rounded to the grid, so a bias
remains that does not shrink with \\n\\. It stays under 4% on the mean
and 9% on the standard deviation once the reported quantiles sit twenty
five or more cells above the smallest delay counted, and reaches tens of
percent on both within ten, so a coarsely resolved delay is better
fitted through its mean and standard deviation where reported.

Code 1, full adjustment, reported the moments of \\f(\cdot \\ ;
\theta)\\ itself, right truncated at \\D\\, \\ \mathbb{E}\[\tau^k \mid
\tau \le D\] = \frac{\int_0^D k t^{k-1} \left(F(D; \theta) - F(t;
\theta)\right) \text{d}t}{F(D; \theta)}, \quad k = 1, \dots, 4,
\tag{5.11} \\ which reduces to the family moments when \\D = \infty\\.
Its distribution function is \\F(y; \theta) / F(D; \theta)\\.

Code 2, the uniform single interval approximation, left the primary
interval uncorrected and so observed \\\tau^\star + U\\, with the
moments of \\F\_{pc}\\ right truncated at \\D\\, \\
\mathbb{E}\[(\tau^\star + U)^k \mid \tau^\star + U \le D\] =
\frac{\int_0^D k t^{k-1} \left(F\_{pc}(D; \theta) - F\_{pc}(t;
\theta)\right) \text{d}t}{F\_{pc}(D; \theta)}, \quad k = 1, \dots, 4.
\tag{5.12} \\ Its distribution function is \\F\_{pc}(y; \theta) /
F\_{pc}(D; \theta)\\. Equations [(5.11)](#eq:meta-trunc-moments) and
[(5.12)](#eq:meta-uniform-moments) are evaluated by Simpson’s rule.
Where \\D = \infty\\ and the primary event is uniform the convolution is
exact, \\ \mu\_{pc} = \mu + \frac{w_p}{2}, \quad \sigma\_{pc}^2 =
\sigma^2 + \frac{w_p^2}{12}, \quad \mu\_{4,pc} = \mu_4 + 6 \sigma^2
\frac{w_p^2}{12} + \frac{w_p^4}{80}, \tag{5.13} \\ with \\\mu\\,
\\\sigma^2\\ and \\\mu_4\\ the mean, variance and fourth central moment
of \\f(\cdot \\ ; \theta)\\.

Code 3, midpoint imputation, assigned each delay to the centre of its
interval, so its estimand is the grid of Equation
[(5.10)](#eq:meta-grid) moved up by \\w_s / 2\\. A single reported
quantile uses Equation [(5.7)](#eq:meta-quantile-crossing), as for code
0. Code 4, midpoint imputation with a uniform interval, placed the
primary event at the midpoint of its window and integrated the secondary
interval, so its estimand is that of code 2 moved down by \\w_p / 2\\. A
shift changes the mean alone and moves the distribution function and
quantiles with it. Code 4 therefore has the mean of code 1 and the
variance of code 2 before truncation, because midpointing removes the
mean of \\U\\ but not its spread. The mirror reading, a midpointed
secondary event and an integrated primary interval, has variance
\\\sigma^2 + w_s^2 / 12\\ and is not used, because the literature
midpoints the wide exposure window of the primary event. A study that
midpointed the secondary interval and left the primary alone is code 3.
Each code integrates the interval it did not midpoint rather than
drawing a random position in it, which would add \\w^2 / 6\\ to the
variance.

#### 5.3.2 Right truncation

The truncation above conditions on the delay falling below one cutoff,
which is what a cohort followed for a common observation time gives.
This is truncation rather than right censoring, where a case is known to
exist and contributes a survival term rather than being absent from the
study. Right censoring is not yet supported.

A study that accrued primary events over a window of length \\A\\ and
stopped at its calendar end saw a delay \\d\\ only for primary events at
least \\d\\ before the stop. With primary events arriving at a rate
proportional to \\\exp(r t)\\ the follow up available is \\ w(d) =
\int_0^{A - d} \exp(r t) \\ \text{d}t = \frac{\exp(r (A - d)) - 1}{r},
\quad 0 \le d \le A, \tag{5.14} \\ which tends to \\A - d\\ as \\r\\
tends to zero and, for a long window and a growing epidemic, to an
exponential tilt by \\\exp(-r d)\\. This is the dynamical bias of Park
et al. ([2024](#ref-park2024estimating)). The estimand is \\f(d; \theta)
w(d)\\ renormalised over \\\[0, A\]\\, so \\A\\ replaces the cohort
cutoff \\D\\. Applying both at once double counts, so a study is one or
the other. The weight multiplies the quadrature for Equations
[(5.11)](#eq:meta-trunc-moments) and [(5.12)](#eq:meta-uniform-moments)
at each node and renormalises. For Equation
[(5.12)](#eq:meta-uniform-moments) it is evaluated at \\d - w_p / 2\\,
to average the primary offset back out. The correction is only as good
as the growth rate supplied.

On the grid of Equation [(5.10)](#eq:meta-grid) the follow up is a step
function, \\w(w_p \lfloor x / w_p \rfloor)\\ for a delay of \\x\\ from
the start of the primary window, because the primary event is known only
to its window. Each cell of the grid is cut at the multiples of \\w_p\\
inside it, each piece is weighted by the follow up at the primary window
it starts in, and the pieces are summed back before renormalising.
Weighting each cell at its lower edge instead is exact only when \\w_p =
w_s\\, and for a daily primary and weekly secondary window puts the mean
36% low at \\r = 0.2\\ where the piecewise weight is within 0.2%.

Two residuals remain. The piecewise weight treats the last primary
window as complete, so when \\A\\ is not a multiple of \\w_p\\ the
primary events it cuts short are overweighted, by 5% on the mean for a
weekly primary and daily secondary window with \\A = 30\\ at \\r =
0.2\\. Use a collection window that is a multiple of \\w_p\\ where the
study allows it. Equation [(5.12)](#eq:meta-uniform-moments) keeps the
smooth weight, because the follow up available to a primary event is
only known to within its window, and at \\w_p = 7\\ its mean is 2.6%
high at \\r = 0.2\\. Both residuals grow with \\r\\ and as \\w_p\\ grows
towards \\A\\.

#### 5.3.3 Left truncation

A study that only counted delays of at least \\L\\ reported summaries
conditioned on \\\tau \> L\\, the left truncation of survival analysis
([Klein and Moeschberger 2003](#ref-klein2003)). Each expression above
reduces to its earlier form when \\L = 0\\. On the grid of Equation
[(5.10)](#eq:meta-grid) the cells below \\L\\ are dropped and the rest
renormalised by their mass, which is \\F\_{pc}(D) - F\_{pc}(L)\\ when
\\L\\ falls on a grid boundary. The truncated moments of Equation
[(5.11)](#eq:meta-trunc-moments) pick up a boundary term, \\
\mathbb{E}\[\tau^k \mid L \< \tau \le D\] = \frac{L^k \left(F(D;
\theta) - F(L; \theta)\right) + \int_L^D k t^{k-1} \left(F(D; \theta) -
F(t; \theta)\right) \text{d}t} {F(D; \theta) - F(L; \theta)}, \quad k =
1, \dots, 4, \tag{5.15} \\ and Equation
[(5.12)](#eq:meta-uniform-moments) likewise with \\F\_{pc}\\ in place of
\\F\\. The distribution function becomes \\ G(y) = \frac{F(y; \theta) -
F(L; \theta)}{F(D; \theta) - F(L; \theta)}, \quad L \< y \le D,
\tag{5.16} \\ zero at or below \\L\\ and one above \\D\\. The accrual
weight of Equation [(5.14)](#eq:meta-accrual) is unchanged, since the
cells and nodes it multiplies now start at \\L\\. Individual level rows
pass \\L\\ to `primarycensored` as their left truncation point, as the
marginal model does.

#### 5.3.4 Quantiles of the estimands

The quantile \\Q_p\\ of an estimand is read off by inverse linear
interpolation between the two points of \\G\\ that bracket \\p\\. On the
grid of Equation [(5.10)](#eq:meta-grid) that interpolant is the
continuity corrected quantile, so it is exact. For a continuous estimand
the chord is refined, exactly through the family quantile function for a
lognormal or weibull delay under code 1, and otherwise by two Newton
steps using the closed form distribution function and density of the
estimand. An accrual estimand is defined by the interpolation between
its nodes and keeps its chord. Both keep \\Q_p\\ differentiable in
\\\theta\\.

### References

Abbott, Sam, Sam Brand, James Mba Azam, Carl Pearson, Sebastian Funk,
and Kelly Charniga. 2025. *Primarycensored: Primary Event Censored
Distributions*. <https://doi.org/10.5281/zenodo.13632839>.

Brand, Samuel P. C., Barbora Nemcova, Carl A. B. Pearson, et al. 2026.
“A Scalable Marginalisation Approach for Double Interval Censored
Epidemiological Delays.” Unpublished manuscript.

Charniga, Kelly, Sang Woo Park, Andrei R. Akhmetzhanov, et al. 2024.
“Best Practices for Estimating and Reporting Epidemiological Delay
Distributions of Infectious Diseases.” *PLOS Computational Biology* 20
(10): 1–21. <https://doi.org/10.1371/journal.pcbi.1012520>.

Cramér, Harald. 1946. *Mathematical Methods of Statistics*. Vol. 9.
Princeton Mathematical Series. Princeton University Press.

Funk, Sebastian, and Sam Abbott. 2026. *Bayesian Re-Analysis of the 2012
Isiro Bundibugyo Ebola Virus Line List*. GitHub repository.
<https://github.com/epiforecasts/bdbv-linelist-analysis>.

Klein, John P., and Melvin L. Moeschberger. 2003. *Survival Analysis:
Techniques for Censored and Truncated Data*. 2nd ed. Springer.
<https://doi.org/10.1007/b97377>.

Park, Sang Woo, Andrei R. Akhmetzhanov, Kelly Charniga, et al. 2024.
“Estimating Epidemiological Delay Distributions for Infectious
Diseases.” *medRxiv*, ahead of print.
<https://doi.org/10.1101/2024.01.12.24301247>.

Ward, Thomas, Rachel Christie, Robert S Paton, Fergus Cumming, and
Christopher E Overton. 2022. “Transmission Dynamics of Monkeypox in the
United Kingdom: Contact Tracing Study.” *BMJ* 379.
<https://doi.org/10.1136/bmj-2022-073153>.
