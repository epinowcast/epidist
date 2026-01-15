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
observe future events and ocurs when we observe data based on the
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
2022](#ref-ward2022transmission))).

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

### References

Park, Sang Woo, Andrei R. Akhmetzhanov, Kelly Charniga, Anne Cori,
Nicholas G. Davies, Jonathan Dushoff, Sebastian Funk, et al. 2024.
“Estimating Epidemiological Delay Distributions for Infectious
Diseases.” *medRxiv*. <https://doi.org/10.1101/2024.01.12.24301247>.

Ward, Thomas, Rachel Christie, Robert S Paton, Fergus Cumming, and
Christopher E Overton. 2022. “Transmission Dynamics of Monkeypox in the
United Kingdom: Contact Tracing Study.” *BMJ* 379.
<https://doi.org/10.1136/bmj-2022-073153>.
