
# Adjusting for common biases in infectious disease data when estimating distributions.

## A simple example

First load required packages and functions.

``` r
library(data.table)
library(purrr, quietly = TRUE)
library(here)
library(brms)
functions <- list.files(here("R"), full.names = TRUE)
walk(functions, source)
```

Now simulate data from an outbreak.

``` r
outbreak <- simulate_gillespie(seed=101)
```

Simulate an observation process during the growth phase for a secondary
event using a lognormal distribution, and finally simulate observing
this event.

``` r
truncated_obs <- outbreak |>
  simulate_secondary(
    meanlog = 1.8,
    sdlog = 0.5
  ) |>
  observe_process() |>
  filter_obs_by_obs_time(obs_time = 25) |>
    DT(sample(1:.N, 100, replace = FALSE))
```

First fit a naive lognormal model with no adjustment.

``` r
naive_fit <- naive_delay(data = truncated_obs, cores = 4, refresh = 0)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 finished in 0.2 seconds.
#> Chain 2 finished in 0.2 seconds.
#> Chain 3 finished in 0.2 seconds.
#> Chain 4 finished in 0.2 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 0.2 seconds.
#> Total execution time: 0.4 seconds.
summary(naive_fit)
#>  Family: lognormal 
#>   Links: mu = identity; sigma = log 
#> Formula: delay_daily ~ 1 
#>          sigma ~ 1
#>    Data: data (Number of observations: 100) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Population-Level Effects: 
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept           1.59      0.05     1.50     1.68 1.00     3596     2655
#> sigma_Intercept    -0.78      0.07    -0.91    -0.64 1.00     3792     2563
#> 
#> Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

Adjust for date censoring.

``` r
censored_fit <- censoring_adjusted_delay(
  data = truncated_obs, cores = 4, refresh = 0
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 finished in 0.7 seconds.
#> Chain 2 finished in 0.7 seconds.
#> Chain 3 finished in 0.7 seconds.
#> Chain 4 finished in 0.7 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 0.7 seconds.
#> Total execution time: 0.8 seconds.
summary(censored_fit)
#>  Family: lognormal 
#>   Links: mu = identity; sigma = log 
#> Formula: delay_lwr | cens(censored, delay_upr) ~ 1 
#>          sigma ~ 1
#>    Data: data (Number of observations: 100) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Population-Level Effects: 
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept           1.60      0.05     1.51     1.69 1.00     2920     2533
#> sigma_Intercept    -0.85      0.08    -1.00    -0.69 1.00     2781     2816
#> 
#> Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

Adjust for right truncation.

``` r
truncation_fit <- truncation_adjusted_delay(
  data = truncated_obs, cores = 4, refresh = 0
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 2 finished in 1.0 seconds.
#> Chain 3 finished in 1.0 seconds.
#> Chain 1 finished in 1.1 seconds.
#> Chain 4 finished in 1.1 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 1.0 seconds.
#> Total execution time: 1.3 seconds.
summary(truncation_fit)
#>  Family: lognormal 
#>   Links: mu = identity; sigma = log 
#> Formula: delay_daily | trunc(lb = 0.001, ub = censored_obs_time) ~ 1 
#>          sigma ~ 1
#>    Data: data (Number of observations: 100) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Population-Level Effects: 
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept           1.88      0.12     1.71     2.11 1.01     1028      770
#> sigma_Intercept    -0.62      0.11    -0.81    -0.38 1.01     1001      819
#> 
#> Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

Adjust for right truncation and date censoring.

``` r
truncation_censoring_fit <- truncation_censoring_adjusted_delay(
  data = truncated_obs, cores = 4, refresh = 0
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 finished in 1.5 seconds.
#> Chain 2 finished in 1.5 seconds.
#> Chain 4 finished in 1.5 seconds.
#> Chain 3 finished in 1.5 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 1.5 seconds.
#> Total execution time: 1.7 seconds.
summary(truncation_censoring_fit)
#>  Family: lognormal 
#>   Links: mu = identity; sigma = log 
#> Formula: delay_lwr | cens(censored, delay_upr) + trunc(lb = 0.001, ub = censored_obs_time) ~ 1 
#>          sigma ~ 1
#>    Data: data (Number of observations: 100) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Population-Level Effects: 
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept           1.84      0.08     1.70     2.03 1.00     1667     1645
#> sigma_Intercept    -0.74      0.11    -0.95    -0.51 1.00     1452     1881
#> 
#> Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

Adjust for right truncation and date censoring using a latent variable
approach.

``` r
latent_truncation_censoring_fit <- latent_truncation_censoring_adjusted_delay(
  data = truncated_obs, cores = 4, refresh = 0
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 finished in 5.1 seconds.
#> Chain 2 finished in 5.5 seconds.
#> Chain 3 finished in 5.4 seconds.
#> Chain 4 finished in 5.4 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 5.4 seconds.
#> Total execution time: 5.6 seconds.
summary(latent_truncation_censoring_fit)
#>  Family: latent_lognormal 
#>   Links: mu = identity; sigma = log; pwindow = identity; swindow = identity 
#> Formula: ptime | vreal(stime, obs_at) ~ 1 
#>          sigma ~ 1
#>          pwindow ~ 0 + id
#>          swindow ~ 0 + id
#>    Data: data (Number of observations: 100) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Population-Level Effects: 
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept           1.88      0.10     1.73     2.11 1.00     4503     2185
#> sigma_Intercept    -0.73      0.11    -0.93    -0.49 1.00     4590     2663
#> pwindow_id1         0.53      0.29     0.03     0.98 1.00    11383     2263
#> pwindow_id2         0.44      0.28     0.02     0.96 1.00     7938     1717
#> pwindow_id3         0.53      0.29     0.03     0.98 1.00     9697     2145
#> pwindow_id4         0.52      0.29     0.02     0.98 1.00     9794     2123
#> pwindow_id5         0.50      0.30     0.02     0.98 1.00     8512     2239
#> pwindow_id6         0.53      0.29     0.03     0.98 1.00     8048     2535
#> pwindow_id7         0.53      0.29     0.03     0.98 1.00     9489     2274
#> pwindow_id8         0.42      0.28     0.02     0.96 1.00     9200     2223
#> pwindow_id9         0.52      0.29     0.03     0.98 1.00    10184     2258
#> pwindow_id10        0.51      0.28     0.03     0.97 1.00     9242     2307
#> pwindow_id11        0.52      0.29     0.03     0.98 1.00     9859     2128
#> pwindow_id12        0.50      0.29     0.02     0.98 1.00     8820     2484
#> pwindow_id13        0.52      0.28     0.03     0.98 1.00     8570     2643
#> pwindow_id14        0.37      0.27     0.01     0.94 1.00     9756     2487
#> pwindow_id15        0.52      0.29     0.03     0.98 1.00     8019     2182
#> pwindow_id16        0.52      0.29     0.03     0.98 1.00    11137     2412
#> pwindow_id17        0.48      0.29     0.02     0.97 1.00     8907     2263
#> pwindow_id18        0.52      0.29     0.03     0.97 1.00     8590     2408
#> pwindow_id19        0.45      0.29     0.02     0.97 1.00     8652     2366
#> pwindow_id20        0.55      0.28     0.04     0.98 1.00     9890     2613
#> pwindow_id21        0.49      0.29     0.03     0.97 1.00     8886     2815
#> pwindow_id22        0.50      0.29     0.02     0.98 1.00     8885     2426
#> pwindow_id23        0.50      0.29     0.03     0.97 1.00     9380     2477
#> pwindow_id24        0.47      0.28     0.03     0.96 1.00     9205     2195
#> pwindow_id25        0.54      0.28     0.03     0.98 1.01     8135     2424
#> pwindow_id26        0.52      0.28     0.03     0.97 1.00     9756     2289
#> pwindow_id27        0.52      0.29     0.03     0.98 1.00    11359     2539
#> pwindow_id28        0.49      0.29     0.02     0.98 1.00     6756     2351
#> pwindow_id29        0.50      0.28     0.03     0.97 1.00     9458     2435
#> pwindow_id30        0.42      0.28     0.01     0.96 1.00     7748     2772
#> pwindow_id31        0.53      0.29     0.03     0.98 1.01     9567     2186
#> pwindow_id32        0.49      0.29     0.03     0.97 1.00     8766     2546
#> pwindow_id33        0.52      0.29     0.03     0.98 1.00     9038     1878
#> pwindow_id34        0.52      0.29     0.02     0.98 1.01    10833     2274
#> pwindow_id35        0.50      0.29     0.02     0.98 1.00     6577     2485
#> pwindow_id36        0.50      0.29     0.02     0.97 1.00    11005     2190
#> pwindow_id37        0.46      0.29     0.02     0.96 1.00     9326     2253
#> pwindow_id38        0.53      0.29     0.03     0.98 1.00     9864     2318
#> pwindow_id39        0.49      0.29     0.03     0.97 1.00     8763     2593
#> pwindow_id40        0.52      0.28     0.04     0.98 1.00     7684     2618
#> pwindow_id41        0.52      0.28     0.03     0.98 1.00     8842     2486
#> pwindow_id42        0.48      0.29     0.02     0.97 1.00     6922     2232
#> pwindow_id43        0.53      0.29     0.03     0.98 1.00     8423     2512
#> pwindow_id44        0.51      0.28     0.03     0.97 1.00    10662     2775
#> pwindow_id45        0.54      0.29     0.02     0.98 1.00     9797     2450
#> pwindow_id46        0.51      0.29     0.03     0.98 1.00     9060     2215
#> pwindow_id47        0.53      0.28     0.04     0.97 1.00     8313     2382
#> pwindow_id48        0.53      0.29     0.03     0.98 1.00     9478     2387
#> pwindow_id49        0.52      0.29     0.03     0.98 1.00     8418     2415
#> pwindow_id50        0.32      0.24     0.01     0.89 1.00     9361     2265
#> pwindow_id51        0.52      0.29     0.02     0.98 1.00     8842     2216
#> pwindow_id52        0.52      0.29     0.03     0.97 1.00     9498     2383
#> pwindow_id53        0.53      0.29     0.03     0.98 1.00     8840     2060
#> pwindow_id54        0.45      0.28     0.03     0.96 1.00    10164     2819
#> pwindow_id55        0.49      0.29     0.02     0.98 1.00     8635     2310
#> pwindow_id56        0.51      0.28     0.04     0.97 1.00     9059     2528
#> pwindow_id57        0.49      0.29     0.02     0.97 1.00     9668     2817
#> pwindow_id58        0.45      0.28     0.02     0.97 1.00     8873     1874
#> pwindow_id59        0.53      0.29     0.03     0.98 1.00     8948     2546
#> pwindow_id60        0.51      0.29     0.02     0.98 1.00     8948     2176
#> pwindow_id61        0.53      0.29     0.03     0.98 1.00     8142     2838
#> pwindow_id62        0.53      0.28     0.03     0.98 1.00     8174     2070
#> pwindow_id63        0.46      0.29     0.02     0.97 1.00     7819     2065
#> pwindow_id64        0.52      0.29     0.03     0.98 1.00    10689     2211
#> pwindow_id65        0.53      0.29     0.03     0.98 1.00    11289     2390
#> pwindow_id66        0.50      0.29     0.02     0.97 1.00     9960     2294
#> pwindow_id67        0.46      0.28     0.03     0.96 1.00     9740     2735
#> pwindow_id68        0.44      0.28     0.02     0.96 1.00    10596     2519
#> pwindow_id69        0.53      0.29     0.03     0.98 1.00     8248     2260
#> pwindow_id70        0.53      0.29     0.03     0.98 1.01     8753     2316
#> pwindow_id71        0.49      0.29     0.02     0.97 1.00     8742     2600
#> pwindow_id72        0.49      0.29     0.02     0.98 1.00    10022     2419
#> pwindow_id73        0.53      0.29     0.03     0.98 1.00    10070     2688
#> pwindow_id74        0.54      0.28     0.03     0.98 1.00     8623     2384
#> pwindow_id75        0.47      0.28     0.03     0.96 1.00     9337     2510
#> pwindow_id76        0.43      0.28     0.02     0.96 1.00     8367     2341
#> pwindow_id77        0.51      0.29     0.02     0.98 1.00     8031     2154
#> pwindow_id78        0.52      0.28     0.03     0.98 1.01     8912     2235
#> pwindow_id79        0.51      0.29     0.03     0.98 1.00    11862     2174
#> pwindow_id80        0.51      0.29     0.02     0.98 1.00     8641     2549
#> pwindow_id81        0.58      0.28     0.04     0.99 1.00     7718     2213
#> pwindow_id82        0.52      0.29     0.03     0.98 1.00    10850     2000
#> pwindow_id83        0.52      0.29     0.03     0.98 1.00     8689     2157
#> pwindow_id84        0.52      0.28     0.03     0.97 1.00     8132     2591
#> pwindow_id85        0.52      0.29     0.03     0.98 1.00     8534     2535
#> pwindow_id86        0.53      0.29     0.03     0.98 1.00     9683     2369
#> pwindow_id87        0.51      0.29     0.02     0.98 1.00     9592     2189
#> pwindow_id88        0.53      0.29     0.03     0.98 1.00     8094     2824
#> pwindow_id89        0.52      0.29     0.03     0.98 1.00     8472     2166
#> pwindow_id90        0.50      0.28     0.03     0.97 1.00     8131     2344
#> pwindow_id91        0.51      0.29     0.03     0.97 1.00     8707     2817
#> pwindow_id92        0.51      0.29     0.03     0.97 1.00     9577     2635
#> pwindow_id93        0.52      0.29     0.02     0.98 1.00    10772     2306
#> pwindow_id94        0.51      0.29     0.03     0.98 1.00     8018     2331
#> pwindow_id95        0.33      0.25     0.01     0.90 1.00     7173     2223
#> pwindow_id96        0.49      0.29     0.02     0.97 1.00     8396     2224
#> pwindow_id97        0.52      0.29     0.02     0.98 1.00     8780     2185
#> pwindow_id98        0.41      0.27     0.02     0.95 1.00     8865     2205
#> pwindow_id99        0.51      0.29     0.03     0.98 1.00     9267     2216
#> pwindow_id100       0.50      0.29     0.02     0.97 1.00     9893     2534
#> swindow_id1         0.50      0.29     0.03     0.97 1.00     9746     2401
#> swindow_id2         0.58      0.28     0.05     0.98 1.00     9452     2373
#> swindow_id3         0.57      0.28     0.05     0.98 1.00     9896     2499
#> swindow_id4         0.47      0.28     0.02     0.97 1.00     8832     2708
#> swindow_id5         0.51      0.29     0.02     0.98 1.00     7884     2395
#> swindow_id6         0.47      0.29     0.02     0.97 1.00    10025     2072
#> swindow_id7         0.49      0.29     0.03     0.97 1.00     8611     2387
#> swindow_id8         0.58      0.28     0.05     0.98 1.00     9954     2573
#> swindow_id9         0.48      0.29     0.02     0.96 1.00     7764     2534
#> swindow_id10        0.50      0.29     0.02     0.98 1.00    10445     1999
#> swindow_id11        0.48      0.28     0.02     0.97 1.00     9982     1791
#> swindow_id12        0.53      0.29     0.03     0.98 1.00     8305     2055
#> swindow_id13        0.47      0.29     0.02     0.98 1.00     8473     2267
#> swindow_id14        0.65      0.26     0.08     0.99 1.00     8436     2108
#> swindow_id15        0.48      0.29     0.02     0.98 1.00     9952     2256
#> swindow_id16        0.54      0.29     0.04     0.98 1.00    10998     2071
#> swindow_id17        0.54      0.29     0.03     0.98 1.00    10029     2303
#> swindow_id18        0.49      0.28     0.02     0.97 1.00     8759     2140
#> swindow_id19        0.55      0.28     0.04     0.98 1.00     8580     2145
#> swindow_id20        0.52      0.29     0.03     0.98 1.00     9974     2423
#> swindow_id21        0.51      0.29     0.02     0.97 1.00    11254     2452
#> swindow_id22        0.50      0.29     0.03     0.97 1.00     9050     2420
#> swindow_id23        0.54      0.28     0.03     0.98 1.00     8262     2091
#> swindow_id24        0.54      0.28     0.04     0.98 1.00     8651     2221
#> swindow_id25        0.49      0.29     0.02     0.97 1.00     8425     2609
#> swindow_id26        0.48      0.29     0.02     0.97 1.00    10385     2350
#> swindow_id27        0.49      0.29     0.02     0.97 1.00    10484     2302
#> swindow_id28        0.52      0.29     0.03     0.98 1.00    10188     2283
#> swindow_id29        0.50      0.29     0.02     0.98 1.00     8767     2066
#> swindow_id30        0.58      0.28     0.05     0.98 1.00     9119     2325
#> swindow_id31        0.49      0.29     0.03     0.97 1.00     8190     2337
#> swindow_id32        0.52      0.29     0.03     0.98 1.00    10119     2688
#> swindow_id33        0.49      0.29     0.02     0.97 1.00     8722     2508
#> swindow_id34        0.49      0.29     0.03     0.97 1.00     9353     2420
#> swindow_id35        0.50      0.28     0.03     0.97 1.00    10909     2265
#> swindow_id36        0.50      0.29     0.03     0.98 1.00     8834     2239
#> swindow_id37        0.55      0.28     0.04     0.98 1.00     8532     2242
#> swindow_id38        0.50      0.29     0.03     0.97 1.00    10239     2494
#> swindow_id39        0.52      0.29     0.02     0.99 1.00     8918     2026
#> swindow_id40        0.50      0.28     0.03     0.97 1.00     9002     2624
#> swindow_id41        0.49      0.29     0.02     0.97 1.00     7735     1934
#> swindow_id42        0.53      0.28     0.04     0.98 1.00     8965     2433
#> swindow_id43        0.50      0.29     0.02     0.97 1.00     7609     2641
#> swindow_id44        0.50      0.28     0.03     0.97 1.00     8618     2502
#> swindow_id45        0.50      0.29     0.02     0.98 1.00     9941     2128
#> swindow_id46        0.49      0.28     0.03     0.97 1.00     8515     2690
#> swindow_id47        0.48      0.29     0.02     0.97 1.00     9503     2347
#> swindow_id48        0.48      0.29     0.02     0.97 1.00     7682     2129
#> swindow_id49        0.48      0.29     0.02     0.97 1.00     9062     1924
#> swindow_id50        0.72      0.23     0.17     0.99 1.00     7756     2758
#> swindow_id51        0.49      0.29     0.02     0.97 1.00     7783     2683
#> swindow_id52        0.50      0.29     0.03     0.98 1.00     9480     2263
#> swindow_id53        0.48      0.29     0.02     0.97 1.00     6989     2257
#> swindow_id54        0.58      0.28     0.04     0.98 1.00    10110     2189
#> swindow_id55        0.53      0.29     0.03     0.98 1.00     9614     2605
#> swindow_id56        0.49      0.29     0.02     0.98 1.01     8704     2073
#> swindow_id57        0.52      0.29     0.03     0.98 1.00     9600     2004
#> swindow_id58        0.67      0.25     0.10     0.99 1.00     7966     2092
#> swindow_id59        0.66      0.26     0.08     0.99 1.00     7810     2325
#> swindow_id60        0.50      0.29     0.02     0.98 1.00     9336     2108
#> swindow_id61        0.48      0.29     0.02     0.98 1.00     8907     2539
#> swindow_id62        0.48      0.29     0.02     0.97 1.00     9337     2459
#> swindow_id63        0.54      0.29     0.03     0.98 1.00    10687     2191
#> swindow_id64        0.48      0.28     0.03     0.97 1.00    10543     2731
#> swindow_id65        0.49      0.29     0.02     0.97 1.00     9227     1936
#> swindow_id66        0.57      0.29     0.03     0.99 1.00     9785     2074
#> swindow_id67        0.55      0.28     0.05     0.98 1.00     8594     2552
#> swindow_id68        0.56      0.28     0.04     0.98 1.00     9345     2266
#> swindow_id69        0.50      0.28     0.03     0.97 1.00     9188     2701
#> swindow_id70        0.49      0.28     0.03     0.97 1.00     7922     2225
#> swindow_id71        0.51      0.29     0.03     0.98 1.00    10194     2121
#> swindow_id72        0.52      0.29     0.03     0.98 1.00     9030     2339
#> swindow_id73        0.49      0.29     0.03     0.97 1.00     8594     2306
#> swindow_id74        0.53      0.29     0.03     0.98 1.00     8470     2691
#> swindow_id75        0.54      0.29     0.04     0.98 1.00     9883     2364
#> swindow_id76        0.61      0.28     0.05     0.99 1.00     9231     2082
#> swindow_id77        0.51      0.29     0.03     0.98 1.00     8680     2347
#> swindow_id78        0.48      0.29     0.02     0.97 1.00     7062     1995
#> swindow_id79        0.50      0.29     0.02     0.98 1.00     9088     1988
#> swindow_id80        0.50      0.29     0.03     0.97 1.00    10873     2455
#> swindow_id81        0.58      0.27     0.05     0.98 1.00    10456     2275
#> swindow_id82        0.52      0.29     0.02     0.98 1.00     8601     2326
#> swindow_id83        0.49      0.29     0.02     0.97 1.00     9197     2457
#> swindow_id84        0.53      0.29     0.03     0.98 1.00     8216     2045
#> swindow_id85        0.48      0.29     0.03     0.97 1.00    11938     2032
#> swindow_id86        0.51      0.29     0.02     0.98 1.01    10407     2183
#> swindow_id87        0.49      0.29     0.02     0.97 1.00     8982     2361
#> swindow_id88        0.48      0.29     0.02     0.97 1.00     8211     2429
#> swindow_id89        0.47      0.29     0.02     0.97 1.00     8335     2529
#> swindow_id90        0.50      0.29     0.02     0.97 1.00     9381     2117
#> swindow_id91        0.50      0.29     0.02     0.98 1.00     7054     2254
#> swindow_id92        0.49      0.29     0.03     0.98 1.00     8867     2464
#> swindow_id93        0.49      0.28     0.02     0.97 1.01     8886     2179
#> swindow_id94        0.54      0.28     0.03     0.98 1.00     8164     2165
#> swindow_id95        0.67      0.25     0.10     0.99 1.00     8922     2642
#> swindow_id96        0.52      0.28     0.03     0.98 1.00     9458     2562
#> swindow_id97        0.49      0.29     0.02     0.98 1.00     8438     1994
#> swindow_id98        0.59      0.28     0.04     0.99 1.00     8871     2464
#> swindow_id99        0.54      0.28     0.04     0.98 1.00     7713     2343
#> swindow_id100       0.51      0.30     0.02     0.98 1.00     8808     2124
#> 
#> Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

## Analyses

This analysis in this repository has been implemented using the
[`targets`](https://docs.ropensci.org/targets/) package and associated
packages. The workflow is defined in
[`_targets.md`](https://github.com/parksw3/dynamicaltruncation/blob/main/_targets.md)
and can be explored interactively using
[`_targets.Rmd`](https://github.com/parksw3/dynamicaltruncation/blob/main/_targets.Rmd)
`Rmarkdown` document. The workflow can be visualised as the following
graph.

This complete analysis can be recreated using the following (note this
may take quite some time even with a fairly large amount of available
compute),

``` bash
bash bin/update-targets.sh
```

Alternative the following `targets` functions may be used to
interactively explore the workflow:

-   Run the workflow sequentially.

``` r
targets::tar_make()
```

-   Run the workflow using all available workers.

``` r
targets::tar_make_future(workers = future::availableCores())
```

-   Explore a graph of the workflow.

``` r
targets::tar_visnetwork(targets_only = TRUE)
```

Watch the workflow as it runs in a `shiny` app.

``` r
targets::tar_watch(targets_only = TRUE)
```

To use our archived version of the interim results (and so avoid long
run times) use the following to download it. Note that this process has
not been rigorously tested across environments and so may not work
seamlessly).

``` r
source(here::here("R", "targets-archive.R"))
get_targets_archive()
```
