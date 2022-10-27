
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
    sdlog = 0.3
  ) |>
  observe_process() |>
  filter_obs_by_obs_time(obs_time = 25)
```

First fit a naive lognormal model with no adjustment.

``` r
naive_fit <- naive_delay(data = truncated_obs, cores = 4, refresh = 0)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 finished in 0.6 seconds.
#> Chain 2 finished in 0.6 seconds.
#> Chain 3 finished in 0.6 seconds.
#> Chain 4 finished in 0.5 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 0.6 seconds.
#> Total execution time: 0.8 seconds.
summary(naive_fit)
#>  Family: lognormal 
#>   Links: mu = identity; sigma = log 
#> Formula: delay_daily ~ 1 
#>          sigma ~ 1
#>    Data: data (Number of observations: 461) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Population-Level Effects: 
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept           1.65      0.02     1.63     1.69 1.00     3071     2449
#> sigma_Intercept    -1.12      0.03    -1.18    -1.06 1.00     3295     2115
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
#> Chain 1 finished in 2.3 seconds.
#> Chain 4 finished in 2.3 seconds.
#> Chain 2 finished in 2.5 seconds.
#> Chain 3 finished in 2.5 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 2.4 seconds.
#> Total execution time: 2.6 seconds.
summary(censored_fit)
#>  Family: lognormal 
#>   Links: mu = identity; sigma = log 
#> Formula: delay_lwr | cens(censored, delay_upr) ~ 1 
#>          sigma ~ 1
#>    Data: data (Number of observations: 461) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Population-Level Effects: 
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept           1.66      0.02     1.63     1.69 1.00     3109     2357
#> sigma_Intercept    -1.20      0.04    -1.28    -1.13 1.00     3136     2553
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
#> Chain 1 finished in 1.8 seconds.
#> Chain 2 finished in 1.8 seconds.
#> Chain 4 finished in 1.8 seconds.
#> Chain 3 finished in 2.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 1.8 seconds.
#> Total execution time: 2.1 seconds.
summary(truncation_fit)
#>  Family: lognormal 
#>   Links: mu = identity; sigma = log 
#> Formula: delay_daily | trunc(lb = 0, ub = censored_obs_time) ~ 1 
#>          sigma ~ 1
#>    Data: data (Number of observations: 461) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Population-Level Effects: 
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept           1.70      0.02     1.67     1.74 1.00     3025     2715
#> sigma_Intercept    -1.07      0.04    -1.14    -1.00 1.00     2576     2290
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
#> Chain 1 finished in 3.6 seconds.
#> Chain 4 finished in 3.5 seconds.
#> Chain 2 finished in 3.7 seconds.
#> Chain 3 finished in 3.8 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 3.7 seconds.
#> Total execution time: 3.9 seconds.
summary(truncation_censoring_fit)
#>  Family: lognormal 
#>   Links: mu = identity; sigma = log 
#> Formula: delay_lwr | cens(censored, delay_upr) + trunc(lb = 0, ub = censored_obs_time) ~ 1 
#>          sigma ~ 1
#>    Data: data (Number of observations: 461) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Population-Level Effects: 
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept           1.71      0.02     1.67     1.74 1.00     3466     2190
#> sigma_Intercept    -1.16      0.04    -1.24    -1.08 1.00     3307     2568
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
#> Chain 2 finished in 60.5 seconds.
#> Chain 1 finished in 61.0 seconds.
#> Chain 3 finished in 61.5 seconds.
#> Chain 4 finished in 62.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 61.3 seconds.
#> Total execution time: 62.2 seconds.
summary(latent_truncation_censoring_fit)
#>  Family: latent_lognormal 
#>   Links: mu = identity; sigma = log; pwindow = identity; swindow = identity 
#> Formula: ptime | vreal(stime, obs_at) ~ 1 
#>          sigma ~ 1
#>          pwindow ~ 0 + id
#>          swindow ~ 0 + id
#>    Data: data (Number of observations: 461) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Population-Level Effects: 
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept           1.75      0.01     1.72     1.78 1.00     4526     2880
#> sigma_Intercept    -1.26      0.04    -1.33    -1.19 1.00     4345     2816
#> pwindow_id1         0.53      0.28     0.03     0.97 1.00     4680     2330
#> pwindow_id2         0.55      0.28     0.03     0.98 1.00     3608     2150
#> pwindow_id3         0.52      0.29     0.03     0.98 1.00     4173     2349
#> pwindow_id4         0.57      0.28     0.04     0.98 1.00     5058     2580
#> pwindow_id5         0.52      0.28     0.04     0.97 1.00     4366     2503
#> pwindow_id6         0.44      0.29     0.02     0.97 1.00     4073     2128
#> pwindow_id7         0.53      0.28     0.03     0.98 1.00     4162     2334
#> pwindow_id8         0.45      0.28     0.02     0.96 1.00     4321     2209
#> pwindow_id9         0.55      0.29     0.03     0.98 1.00     5086     2327
#> pwindow_id10        0.50      0.28     0.03     0.97 1.00     3961     2340
#> pwindow_id11        0.54      0.28     0.03     0.98 1.00     5123     2457
#> pwindow_id12        0.54      0.28     0.04     0.98 1.00     4606     2152
#> pwindow_id13        0.51      0.29     0.03     0.97 1.00     4154     2283
#> pwindow_id14        0.56      0.29     0.04     0.99 1.00     4806     2475
#> pwindow_id15        0.52      0.29     0.03     0.98 1.00     4310     2210
#> pwindow_id16        0.54      0.29     0.03     0.98 1.00     4233     2386
#> pwindow_id17        0.55      0.28     0.04     0.98 1.00     4556     2349
#> pwindow_id18        0.38      0.27     0.02     0.94 1.00     4609     2478
#> pwindow_id19        0.53      0.29     0.03     0.98 1.00     4760     2507
#> pwindow_id20        0.52      0.29     0.03     0.97 1.00     5040     2575
#> pwindow_id21        0.51      0.29     0.03     0.98 1.00     4436     2482
#> pwindow_id22        0.47      0.28     0.02     0.96 1.00     5239     2378
#> pwindow_id23        0.41      0.28     0.02     0.96 1.00     4603     2187
#> pwindow_id24        0.54      0.29     0.03     0.98 1.00     4216     2441
#> pwindow_id25        0.54      0.29     0.03     0.98 1.00     5019     2465
#> pwindow_id26        0.53      0.29     0.03     0.98 1.00     5262     1908
#> pwindow_id27        0.56      0.29     0.03     0.98 1.00     5875     2316
#> pwindow_id28        0.40      0.28     0.02     0.96 1.00     4631     2685
#> pwindow_id29        0.55      0.29     0.03     0.98 1.00     3820     2138
#> pwindow_id30        0.55      0.28     0.04     0.98 1.00     4407     2148
#> pwindow_id31        0.55      0.28     0.03     0.98 1.00     5016     2255
#> pwindow_id32        0.51      0.28     0.03     0.97 1.00     5970     2634
#> pwindow_id33        0.55      0.29     0.03     0.98 1.00     5276     2229
#> pwindow_id34        0.55      0.28     0.05     0.98 1.00     4608     2315
#> pwindow_id35        0.50      0.29     0.03     0.97 1.00     4170     2125
#> pwindow_id36        0.50      0.30     0.02     0.97 1.00     5140     2162
#> pwindow_id37        0.45      0.28     0.02     0.96 1.00     4385     2411
#> pwindow_id38        0.56      0.29     0.04     0.98 1.00     3537     2234
#> pwindow_id39        0.52      0.29     0.03     0.97 1.00     4642     2285
#> pwindow_id40        0.56      0.28     0.05     0.98 1.00     4788     2471
#> pwindow_id41        0.52      0.29     0.03     0.98 1.00     5335     1812
#> pwindow_id42        0.56      0.28     0.03     0.98 1.00     4317     1953
#> pwindow_id43        0.46      0.28     0.02     0.97 1.00     4059     2389
#> pwindow_id44        0.50      0.29     0.02     0.98 1.00     5017     2333
#> pwindow_id45        0.56      0.28     0.04     0.98 1.00     5089     2239
#> pwindow_id46        0.50      0.29     0.02     0.97 1.00     4651     2049
#> pwindow_id47        0.54      0.29     0.03     0.98 1.00     5051     2393
#> pwindow_id48        0.52      0.28     0.03     0.98 1.00     4266     2299
#> pwindow_id49        0.55      0.28     0.04     0.98 1.00     4376     2486
#> pwindow_id50        0.50      0.29     0.03     0.97 1.00     6378     2602
#> pwindow_id51        0.57      0.28     0.04     0.98 1.00     4435     2085
#> pwindow_id52        0.45      0.28     0.02     0.97 1.00     5313     2441
#> pwindow_id53        0.56      0.28     0.05     0.98 1.00     4756     2737
#> pwindow_id54        0.47      0.28     0.02     0.97 1.00     4908     2130
#> pwindow_id55        0.49      0.29     0.03     0.97 1.00     5034     2708
#> pwindow_id56        0.52      0.29     0.03     0.98 1.00     3858     2117
#> pwindow_id57        0.51      0.28     0.03     0.97 1.00     4951     2419
#> pwindow_id58        0.54      0.29     0.03     0.98 1.00     5161     2408
#> pwindow_id59        0.56      0.29     0.03     0.99 1.00     4110     2276
#> pwindow_id60        0.52      0.28     0.03     0.97 1.00     4729     2501
#> pwindow_id61        0.45      0.28     0.02     0.96 1.00     3973     1878
#> pwindow_id62        0.47      0.29     0.02     0.97 1.00     3983     2073
#> pwindow_id63        0.53      0.29     0.03     0.98 1.00     5611     2353
#> pwindow_id64        0.53      0.28     0.04     0.98 1.00     4237     2077
#> pwindow_id65        0.56      0.29     0.04     0.98 1.00     5399     2253
#> pwindow_id66        0.56      0.29     0.04     0.98 1.00     4306     2323
#> pwindow_id67        0.47      0.29     0.02     0.97 1.01     5121     2205
#> pwindow_id68        0.54      0.29     0.03     0.98 1.00     5219     2208
#> pwindow_id69        0.49      0.29     0.03     0.97 1.00     4080     2303
#> pwindow_id70        0.54      0.29     0.04     0.98 1.00     4080     2243
#> pwindow_id71        0.45      0.28     0.02     0.96 1.00     4647     2356
#> pwindow_id72        0.39      0.27     0.02     0.95 1.00     4705     2144
#> pwindow_id73        0.55      0.28     0.03     0.98 1.00     4187     2224
#> pwindow_id74        0.51      0.28     0.03     0.97 1.00     4270     2041
#> pwindow_id75        0.49      0.29     0.03     0.97 1.00     4089     1819
#> pwindow_id76        0.44      0.28     0.02     0.96 1.00     5041     2279
#> pwindow_id77        0.52      0.28     0.03     0.98 1.00     5576     2149
#> pwindow_id78        0.57      0.28     0.04     0.98 1.00     5600     2371
#> pwindow_id79        0.44      0.28     0.02     0.96 1.00     4690     2457
#> pwindow_id80        0.57      0.28     0.04     0.99 1.00     4396     2413
#> pwindow_id81        0.53      0.28     0.03     0.97 1.00     5862     2539
#> pwindow_id82        0.49      0.29     0.03     0.97 1.00     4411     2481
#> pwindow_id83        0.46      0.29     0.02     0.97 1.00     4707     2479
#> pwindow_id84        0.38      0.27     0.02     0.93 1.00     5397     2380
#> pwindow_id85        0.49      0.29     0.02     0.98 1.00     4413     2458
#> pwindow_id86        0.55      0.29     0.03     0.98 1.00     5106     2381
#> pwindow_id87        0.47      0.29     0.02     0.98 1.00     3844     2021
#> pwindow_id88        0.51      0.28     0.03     0.97 1.00     5294     1921
#> pwindow_id89        0.49      0.28     0.03     0.97 1.00     4852     2577
#> pwindow_id90        0.54      0.28     0.04     0.98 1.00     4471     2019
#> pwindow_id91        0.52      0.29     0.03     0.98 1.00     4513     2269
#> pwindow_id92        0.42      0.27     0.02     0.95 1.00     4083     2426
#> pwindow_id93        0.53      0.29     0.03     0.98 1.00     5201     2226
#> pwindow_id94        0.39      0.27     0.01     0.94 1.00     3808     2141
#> pwindow_id95        0.54      0.29     0.02     0.98 1.00     4735     2131
#> pwindow_id96        0.54      0.29     0.03     0.98 1.00     4444     2038
#> pwindow_id97        0.52      0.28     0.04     0.97 1.00     5020     2597
#> pwindow_id98        0.45      0.28     0.02     0.96 1.00     4686     2522
#> pwindow_id99        0.55      0.29     0.03     0.98 1.00     4892     2587
#> pwindow_id100       0.53      0.28     0.03     0.98 1.00     4695     2418
#> pwindow_id101       0.53      0.29     0.03     0.98 1.00     3937     2320
#> pwindow_id102       0.44      0.28     0.02     0.96 1.00     4110     2296
#> pwindow_id103       0.55      0.28     0.03     0.98 1.00     4316     2290
#> pwindow_id104       0.54      0.29     0.04     0.98 1.00     4372     2216
#> pwindow_id105       0.54      0.28     0.04     0.97 1.00     4452     2643
#> pwindow_id106       0.51      0.29     0.03     0.98 1.00     4615     2249
#> pwindow_id107       0.54      0.28     0.04     0.98 1.00     4331     2389
#> pwindow_id108       0.50      0.28     0.03     0.97 1.00     4298     2279
#> pwindow_id109       0.57      0.28     0.04     0.98 1.00     4465     2390
#> pwindow_id110       0.56      0.29     0.03     0.98 1.00     4351     2065
#> pwindow_id111       0.49      0.28     0.03     0.97 1.00     3801     2274
#> pwindow_id112       0.53      0.28     0.03     0.98 1.00     3620     2123
#> pwindow_id113       0.55      0.29     0.03     0.99 1.00     4729     2285
#> pwindow_id114       0.49      0.29     0.02     0.97 1.00     3620     2118
#> pwindow_id115       0.57      0.28     0.04     0.98 1.00     4278     2114
#> pwindow_id116       0.56      0.29     0.04     0.98 1.00     5160     2093
#> pwindow_id117       0.57      0.28     0.05     0.98 1.00     3771     2108
#> pwindow_id118       0.54      0.29     0.03     0.98 1.00     4791     1831
#> pwindow_id119       0.53      0.28     0.04     0.98 1.00     4170     2309
#> pwindow_id120       0.57      0.28     0.03     0.98 1.00     5161     2676
#> pwindow_id121       0.53      0.29     0.03     0.98 1.00     4943     2296
#> pwindow_id122       0.54      0.28     0.04     0.98 1.00     4876     2377
#> pwindow_id123       0.56      0.29     0.04     0.99 1.00     4909     2354
#> pwindow_id124       0.52      0.29     0.03     0.97 1.00     4381     2257
#> pwindow_id125       0.50      0.29     0.03     0.97 1.00     4979     2581
#> pwindow_id126       0.57      0.28     0.05     0.98 1.00     4920     2401
#> pwindow_id127       0.47      0.28     0.02     0.97 1.00     4832     1885
#> pwindow_id128       0.58      0.28     0.04     0.98 1.00     5217     2524
#> pwindow_id129       0.49      0.29     0.02     0.97 1.00     5594     2408
#> pwindow_id130       0.54      0.29     0.03     0.98 1.00     4632     2042
#> pwindow_id131       0.56      0.28     0.03     0.98 1.00     4039     2004
#> pwindow_id132       0.50      0.28     0.03     0.97 1.00     4935     2183
#> pwindow_id133       0.50      0.29     0.03     0.97 1.00     4072     2106
#> pwindow_id134       0.51      0.29     0.02     0.98 1.00     4652     2195
#> pwindow_id135       0.49      0.29     0.03     0.97 1.00     4574     2416
#> pwindow_id136       0.55      0.29     0.03     0.98 1.00     5228     2567
#> pwindow_id137       0.55      0.28     0.04     0.98 1.00     4792     2420
#> pwindow_id138       0.47      0.29     0.02     0.98 1.00     4028     2015
#> pwindow_id139       0.57      0.28     0.04     0.98 1.00     4454     2713
#> pwindow_id140       0.48      0.29     0.02     0.97 1.00     4655     2581
#> pwindow_id141       0.50      0.29     0.02     0.98 1.00     5003     2166
#> pwindow_id142       0.51      0.29     0.02     0.98 1.00     3949     2255
#> pwindow_id143       0.49      0.29     0.03     0.97 1.00     4570     2114
#> pwindow_id144       0.53      0.29     0.03     0.98 1.00     3557     1912
#> pwindow_id145       0.54      0.29     0.03     0.98 1.00     4719     2325
#> pwindow_id146       0.37      0.26     0.01     0.93 1.00     4765     2556
#> pwindow_id147       0.55      0.29     0.03     0.98 1.00     4531     2322
#> pwindow_id148       0.52      0.29     0.03     0.98 1.00     4781     2392
#> pwindow_id149       0.50      0.28     0.02     0.97 1.00     4198     2430
#> pwindow_id150       0.53      0.28     0.03     0.98 1.00     4857     2484
#> pwindow_id151       0.49      0.29     0.03     0.97 1.00     3953     2016
#> pwindow_id152       0.51      0.29     0.03     0.97 1.00     5195     2221
#> pwindow_id153       0.49      0.29     0.02     0.98 1.00     5342     2192
#> pwindow_id154       0.51      0.29     0.02     0.98 1.00     4191     1786
#> pwindow_id155       0.50      0.29     0.03     0.98 1.00     4773     2109
#> pwindow_id156       0.51      0.29     0.03     0.98 1.00     4202     2188
#> pwindow_id157       0.53      0.28     0.04     0.97 1.00     4092     2264
#> pwindow_id158       0.52      0.29     0.02     0.98 1.00     4750     2238
#> pwindow_id159       0.47      0.28     0.02     0.97 1.00     4557     2283
#> pwindow_id160       0.56      0.29     0.04     0.98 1.00     5178     2264
#> pwindow_id161       0.51      0.28     0.03     0.97 1.00     4439     2001
#> pwindow_id162       0.46      0.28     0.02     0.97 1.00     4225     2384
#> pwindow_id163       0.49      0.29     0.02     0.97 1.00     5427     2383
#> pwindow_id164       0.52      0.29     0.03     0.98 1.00     4784     2381
#> pwindow_id165       0.54      0.29     0.03     0.98 1.00     4656     2480
#> pwindow_id166       0.57      0.28     0.04     0.98 1.00     4940     2358
#> pwindow_id167       0.55      0.28     0.04     0.98 1.00     4538     2485
#> pwindow_id168       0.55      0.28     0.04     0.98 1.00     4281     2372
#> pwindow_id169       0.54      0.29     0.03     0.98 1.00     5599     2636
#> pwindow_id170       0.51      0.29     0.03     0.98 1.00     6161     2572
#> pwindow_id171       0.52      0.29     0.02     0.98 1.00     4369     2126
#> pwindow_id172       0.56      0.28     0.04     0.98 1.00     3675     2128
#> pwindow_id173       0.52      0.28     0.03     0.98 1.00     4255     2534
#> pwindow_id174       0.39      0.27     0.02     0.94 1.00     5291     2329
#> pwindow_id175       0.53      0.28     0.03     0.98 1.00     5932     2247
#> pwindow_id176       0.54      0.29     0.03     0.98 1.00     4666     2672
#> pwindow_id177       0.51      0.29     0.03     0.98 1.00     3724     1996
#> pwindow_id178       0.45      0.29     0.02     0.97 1.00     4699     2502
#> pwindow_id179       0.54      0.28     0.03     0.98 1.01     4901     2096
#> pwindow_id180       0.56      0.29     0.04     0.98 1.00     3628     2204
#> pwindow_id181       0.50      0.28     0.03     0.98 1.00     4948     2267
#> pwindow_id182       0.50      0.28     0.02     0.97 1.00     4954     2251
#> pwindow_id183       0.51      0.28     0.03     0.98 1.00     5241     2289
#> pwindow_id184       0.53      0.28     0.03     0.98 1.00     4600     2234
#> pwindow_id185       0.55      0.28     0.04     0.98 1.00     4694     2451
#> pwindow_id186       0.51      0.29     0.02     0.98 1.00     5043     2352
#> pwindow_id187       0.49      0.29     0.02     0.97 1.00     4050     2096
#> pwindow_id188       0.49      0.28     0.03     0.97 1.00     5497     2375
#> pwindow_id189       0.55      0.29     0.04     0.98 1.00     5181     2920
#> pwindow_id190       0.54      0.28     0.03     0.98 1.00     4700     2371
#> pwindow_id191       0.41      0.27     0.02     0.95 1.00     4906     2415
#> pwindow_id192       0.53      0.29     0.03     0.98 1.00     4766     2291
#> pwindow_id193       0.53      0.29     0.03     0.98 1.00     4502     2030
#> pwindow_id194       0.48      0.29     0.03     0.97 1.00     4902     2262
#> pwindow_id195       0.54      0.29     0.03     0.98 1.00     5329     2172
#> pwindow_id196       0.52      0.28     0.03     0.98 1.00     4720     2114
#> pwindow_id197       0.52      0.29     0.03     0.98 1.00     3981     2233
#> pwindow_id198       0.55      0.29     0.04     0.98 1.00     5084     2768
#> pwindow_id199       0.56      0.29     0.04     0.98 1.00     4035     2303
#> pwindow_id200       0.56      0.28     0.04     0.98 1.00     3688     2219
#> pwindow_id201       0.56      0.28     0.03     0.98 1.00     5270     2275
#> pwindow_id202       0.47      0.28     0.03     0.97 1.00     5582     2468
#> pwindow_id203       0.53      0.29     0.03     0.98 1.00     4933     2641
#> pwindow_id204       0.53      0.28     0.04     0.98 1.00     4653     2520
#> pwindow_id205       0.53      0.29     0.03     0.98 1.00     4476     2496
#> pwindow_id206       0.47      0.28     0.02     0.97 1.00     5428     2548
#> pwindow_id207       0.47      0.29     0.02     0.97 1.00     4628     2400
#> pwindow_id208       0.50      0.29     0.03     0.98 1.00     3918     2132
#> pwindow_id209       0.47      0.29     0.02     0.97 1.00     5798     2739
#> pwindow_id210       0.51      0.29     0.02     0.98 1.00     4913     2410
#> pwindow_id211       0.49      0.29     0.03     0.97 1.00     4932     2470
#> pwindow_id212       0.48      0.28     0.02     0.97 1.00     3747     2485
#> pwindow_id213       0.50      0.28     0.03     0.97 1.00     3934     2388
#> pwindow_id214       0.52      0.29     0.03     0.98 1.00     4721     2196
#> pwindow_id215       0.43      0.28     0.01     0.95 1.00     5069     2128
#> pwindow_id216       0.52      0.28     0.03     0.98 1.00     4183     1675
#> pwindow_id217       0.52      0.28     0.03     0.98 1.00     4953     2542
#> pwindow_id218       0.56      0.29     0.04     0.98 1.00     4648     2309
#> pwindow_id219       0.53      0.28     0.03     0.98 1.00     4139     2396
#> pwindow_id220       0.55      0.29     0.03     0.98 1.00     4561     2300
#> pwindow_id221       0.55      0.28     0.03     0.98 1.00     4136     2385
#> pwindow_id222       0.53      0.28     0.03     0.98 1.00     4825     2038
#> pwindow_id223       0.50      0.29     0.02     0.98 1.00     4328     2275
#> pwindow_id224       0.53      0.29     0.02     0.98 1.00     4446     2470
#> pwindow_id225       0.52      0.29     0.03     0.98 1.00     4820     2366
#> pwindow_id226       0.47      0.28     0.03     0.96 1.00     5225     2324
#> pwindow_id227       0.55      0.29     0.03     0.98 1.00     4238     2382
#> pwindow_id228       0.41      0.27     0.02     0.95 1.00     4331     2156
#> pwindow_id229       0.55      0.29     0.04     0.98 1.00     4278     2119
#> pwindow_id230       0.51      0.29     0.02     0.97 1.00     4224     2276
#> pwindow_id231       0.52      0.28     0.03     0.97 1.00     4311     2255
#> pwindow_id232       0.54      0.28     0.04     0.97 1.00     4505     2479
#> pwindow_id233       0.45      0.28     0.02     0.96 1.00     3862     2329
#> pwindow_id234       0.35      0.27     0.01     0.93 1.00     3958     2149
#> pwindow_id235       0.53      0.29     0.03     0.98 1.00     4425     2373
#> pwindow_id236       0.23      0.20     0.01     0.75 1.00     4042     2082
#> pwindow_id237       0.46      0.29     0.02     0.96 1.00     4792     2384
#> pwindow_id238       0.51      0.29     0.03     0.97 1.00     4996     2245
#> pwindow_id239       0.56      0.29     0.04     0.98 1.00     4809     2546
#> pwindow_id240       0.51      0.29     0.03     0.97 1.00     5221     2634
#> pwindow_id241       0.55      0.28     0.03     0.98 1.00     4559     2273
#> pwindow_id242       0.54      0.28     0.04     0.98 1.00     5628     2652
#> pwindow_id243       0.50      0.29     0.03     0.98 1.00     4772     2240
#> pwindow_id244       0.50      0.28     0.03     0.97 1.00     4505     2151
#> pwindow_id245       0.56      0.29     0.03     0.99 1.00     4449     1651
#> pwindow_id246       0.37      0.27     0.01     0.94 1.00     4609     2061
#> pwindow_id247       0.46      0.28     0.02     0.97 1.01     3984     2092
#> pwindow_id248       0.49      0.30     0.02     0.98 1.00     4794     2451
#> pwindow_id249       0.53      0.29     0.03     0.97 1.00     4567     2230
#> pwindow_id250       0.55      0.29     0.03     0.98 1.00     4707     2661
#> pwindow_id251       0.56      0.29     0.03     0.98 1.00     3503     2390
#> pwindow_id252       0.32      0.24     0.01     0.88 1.00     4796     2496
#> pwindow_id253       0.53      0.29     0.03     0.98 1.00     5390     2299
#> pwindow_id254       0.49      0.28     0.03     0.97 1.00     4838     2425
#> pwindow_id255       0.37      0.27     0.01     0.94 1.00     3749     2187
#> pwindow_id256       0.36      0.26     0.01     0.94 1.00     3963     1935
#> pwindow_id257       0.53      0.29     0.03     0.98 1.00     5154     2282
#> pwindow_id258       0.49      0.29     0.03     0.97 1.00     5439     2127
#> pwindow_id259       0.51      0.28     0.03     0.98 1.00     4195     2099
#> pwindow_id260       0.52      0.29     0.04     0.97 1.00     5568     2722
#> pwindow_id261       0.53      0.29     0.03     0.98 1.00     4780     2260
#> pwindow_id262       0.52      0.29     0.03     0.98 1.00     4747     2280
#> pwindow_id263       0.54      0.28     0.03     0.97 1.00     4263     2198
#> pwindow_id264       0.54      0.28     0.04     0.97 1.00     4253     2566
#> pwindow_id265       0.45      0.29     0.02     0.96 1.00     4093     2113
#> pwindow_id266       0.51      0.28     0.03     0.97 1.00     4989     2446
#> pwindow_id267       0.44      0.28     0.02     0.96 1.00     3813     1536
#> pwindow_id268       0.55      0.29     0.04     0.98 1.00     4925     2252
#> pwindow_id269       0.51      0.29     0.02     0.98 1.00     3136     1748
#> pwindow_id270       0.53      0.29     0.03     0.98 1.00     4559     2012
#> pwindow_id271       0.56      0.29     0.04     0.98 1.00     4575     2020
#> pwindow_id272       0.46      0.28     0.02     0.96 1.00     6323     2289
#> pwindow_id273       0.46      0.28     0.02     0.96 1.00     4816     2297
#> pwindow_id274       0.49      0.29     0.03     0.97 1.00     4197     2246
#> pwindow_id275       0.52      0.29     0.03     0.98 1.00     4403     2461
#> pwindow_id276       0.48      0.29     0.02     0.98 1.00     4332     2410
#> pwindow_id277       0.53      0.29     0.02     0.99 1.00     3798     2083
#> pwindow_id278       0.49      0.28     0.02     0.97 1.00     4088     2115
#> pwindow_id279       0.53      0.29     0.03     0.98 1.00     3999     2284
#> pwindow_id280       0.54      0.28     0.04     0.97 1.00     5287     2465
#> pwindow_id281       0.45      0.28     0.02     0.96 1.00     4204     2220
#> pwindow_id282       0.53      0.29     0.03     0.98 1.00     5478     2599
#> pwindow_id283       0.48      0.29     0.02     0.97 1.00     4552     2223
#> pwindow_id284       0.45      0.28     0.02     0.97 1.00     4189     2111
#> pwindow_id285       0.50      0.29     0.02     0.98 1.00     4536     2714
#> pwindow_id286       0.49      0.29     0.02     0.97 1.00     5047     2092
#> pwindow_id287       0.52      0.29     0.03     0.98 1.00     4582     1974
#> pwindow_id288       0.54      0.29     0.04     0.98 1.00     4886     2559
#> pwindow_id289       0.56      0.28     0.05     0.97 1.00     5564     2930
#> pwindow_id290       0.55      0.28     0.05     0.98 1.00     4763     2275
#> pwindow_id291       0.48      0.29     0.02     0.97 1.00     4811     2344
#> pwindow_id292       0.55      0.29     0.03     0.98 1.00     3705     2344
#> pwindow_id293       0.46      0.28     0.02     0.97 1.00     3666     2245
#> pwindow_id294       0.45      0.28     0.02     0.96 1.00     5067     2273
#> pwindow_id295       0.50      0.29     0.02     0.98 1.00     4118     2254
#> pwindow_id296       0.41      0.28     0.01     0.95 1.00     3772     2258
#> pwindow_id297       0.52      0.29     0.03     0.98 1.00     5888     2241
#> pwindow_id298       0.54      0.28     0.04     0.97 1.00     4089     2468
#> pwindow_id299       0.44      0.27     0.02     0.95 1.00     4625     1827
#> pwindow_id300       0.56      0.29     0.03     0.98 1.00     4469     2316
#> pwindow_id301       0.53      0.28     0.03     0.98 1.00     3815     2474
#> pwindow_id302       0.56      0.28     0.04     0.98 1.00     4673     2406
#> pwindow_id303       0.57      0.28     0.04     0.98 1.00     3840     1881
#> pwindow_id304       0.50      0.28     0.03     0.97 1.00     4344     2139
#> pwindow_id305       0.45      0.29     0.01     0.98 1.00     3594     1900
#> pwindow_id306       0.53      0.29     0.03     0.98 1.00     4636     2596
#> pwindow_id307       0.51      0.29     0.02     0.98 1.00     5262     2303
#> pwindow_id308       0.42      0.27     0.02     0.95 1.00     4363     1693
#> pwindow_id309       0.50      0.29     0.03     0.98 1.00     4114     2293
#> pwindow_id310       0.54      0.29     0.03     0.98 1.00     4678     2335
#> pwindow_id311       0.47      0.28     0.03     0.96 1.00     5158     2474
#> pwindow_id312       0.51      0.29     0.03     0.97 1.00     3885     2316
#> pwindow_id313       0.49      0.28     0.03     0.97 1.00     4755     2369
#> pwindow_id314       0.55      0.28     0.04     0.98 1.00     4484     2295
#> pwindow_id315       0.53      0.28     0.03     0.98 1.00     4370     2341
#> pwindow_id316       0.46      0.29     0.02     0.97 1.00     5275     2685
#> pwindow_id317       0.52      0.29     0.02     0.98 1.00     4311     2431
#> pwindow_id318       0.55      0.28     0.03     0.98 1.00     4804     1992
#> pwindow_id319       0.56      0.29     0.03     0.98 1.00     4097     2114
#> pwindow_id320       0.56      0.29     0.03     0.98 1.00     3629     2355
#> pwindow_id321       0.55      0.29     0.04     0.98 1.00     3835     1987
#> pwindow_id322       0.55      0.29     0.04     0.98 1.00     5598     2397
#> pwindow_id323       0.43      0.28     0.02     0.97 1.00     4140     2139
#> pwindow_id324       0.55      0.28     0.03     0.98 1.00     4368     2359
#> pwindow_id325       0.53      0.29     0.03     0.98 1.00     3869     2319
#> pwindow_id326       0.56      0.28     0.04     0.98 1.00     5341     2124
#> pwindow_id327       0.53      0.29     0.03     0.98 1.00     3795     2096
#> pwindow_id328       0.44      0.28     0.02     0.96 1.00     4468     2317
#> pwindow_id329       0.55      0.28     0.04     0.98 1.00     4045     2328
#> pwindow_id330       0.55      0.28     0.04     0.98 1.00     3766     2215
#> pwindow_id331       0.35      0.26     0.01     0.92 1.01     4701     2441
#> pwindow_id332       0.49      0.29     0.03     0.97 1.00     5082     2398
#> pwindow_id333       0.55      0.29     0.03     0.98 1.00     4165     2208
#> pwindow_id334       0.50      0.29     0.03     0.97 1.00     4873     2067
#> pwindow_id335       0.50      0.29     0.03     0.97 1.00     5013     2099
#> pwindow_id336       0.50      0.28     0.03     0.96 1.00     4791     2327
#> pwindow_id337       0.28      0.23     0.01     0.84 1.00     4584     2274
#> pwindow_id338       0.55      0.29     0.03     0.98 1.00     3908     2051
#> pwindow_id339       0.36      0.26     0.01     0.94 1.00     4735     2227
#> pwindow_id340       0.48      0.28     0.03     0.97 1.00     3333     1962
#> pwindow_id341       0.52      0.29     0.03     0.97 1.00     3860     2527
#> pwindow_id342       0.54      0.29     0.04     0.98 1.00     5308     2422
#> pwindow_id343       0.42      0.28     0.02     0.96 1.00     4091     2316
#> pwindow_id344       0.49      0.29     0.03     0.97 1.00     4151     2511
#> pwindow_id345       0.55      0.28     0.03     0.98 1.00     5107     2196
#> pwindow_id346       0.51      0.28     0.02     0.98 1.00     4670     2465
#> pwindow_id347       0.47      0.29     0.02     0.97 1.00     4013     2069
#> pwindow_id348       0.46      0.28     0.02     0.97 1.00     4556     2410
#> pwindow_id349       0.53      0.29     0.03     0.97 1.00     4369     2161
#> pwindow_id350       0.51      0.30     0.02     0.98 1.00     4127     2024
#> pwindow_id351       0.51      0.29     0.02     0.97 1.00     4929     2215
#> pwindow_id352       0.51      0.29     0.03     0.97 1.00     4384     2558
#> pwindow_id353       0.54      0.29     0.03     0.98 1.00     5591     2485
#> pwindow_id354       0.53      0.29     0.03     0.98 1.00     4874     2751
#> pwindow_id355       0.49      0.28     0.03     0.97 1.00     5029     2336
#> pwindow_id356       0.43      0.29     0.01     0.96 1.00     4643     2019
#> pwindow_id357       0.44      0.28     0.02     0.97 1.00     4047     2283
#> pwindow_id358       0.50      0.29     0.02     0.97 1.00     3510     1730
#> pwindow_id359       0.51      0.29     0.03     0.97 1.00     4332     2372
#> pwindow_id360       0.36      0.26     0.01     0.92 1.00     4046     2275
#> pwindow_id361       0.47      0.29     0.02     0.97 1.00     5356     2589
#> pwindow_id362       0.49      0.29     0.02     0.98 1.00     4455     2258
#> pwindow_id363       0.53      0.29     0.03     0.98 1.00     4100     2418
#> pwindow_id364       0.46      0.28     0.02     0.96 1.00     5106     2334
#> pwindow_id365       0.50      0.29     0.02     0.97 1.00     4844     2282
#> pwindow_id366       0.51      0.29     0.03     0.98 1.00     5784     2344
#> pwindow_id367       0.49      0.29     0.03     0.97 1.00     5349     2288
#> pwindow_id368       0.54      0.29     0.03     0.98 1.00     4825     2246
#> pwindow_id369       0.54      0.28     0.04     0.98 1.00     4224     2505
#> pwindow_id370       0.51      0.29     0.02     0.98 1.00     4021     2402
#> pwindow_id371       0.49      0.29     0.02     0.97 1.00     4098     2172
#> pwindow_id372       0.39      0.27     0.02     0.94 1.00     4482     2124
#> pwindow_id373       0.48      0.28     0.03     0.97 1.00     4433     2327
#> pwindow_id374       0.48      0.29     0.02     0.97 1.00     3466     1993
#> pwindow_id375       0.55      0.28     0.04     0.98 1.00     5224     2595
#> pwindow_id376       0.39      0.27     0.02     0.94 1.00     5255     2415
#> pwindow_id377       0.52      0.28     0.03     0.98 1.00     5254     2556
#> pwindow_id378       0.45      0.28     0.02     0.96 1.00     4330     2192
#> pwindow_id379       0.53      0.29     0.03     0.98 1.00     4730     2236
#> pwindow_id380       0.39      0.27     0.02     0.94 1.00     4062     2300
#> pwindow_id381       0.55      0.28     0.04     0.98 1.00     5571     2788
#> pwindow_id382       0.41      0.27     0.02     0.95 1.00     5102     2720
#> pwindow_id383       0.50      0.28     0.03     0.97 1.00     4886     2432
#> pwindow_id384       0.54      0.28     0.03     0.98 1.00     5411     2623
#> pwindow_id385       0.51      0.29     0.03     0.97 1.00     3675     2296
#> pwindow_id386       0.51      0.28     0.03     0.97 1.00     3944     2131
#> pwindow_id387       0.54      0.28     0.03     0.98 1.00     3985     2272
#> pwindow_id388       0.54      0.29     0.03     0.98 1.00     4755     2335
#> pwindow_id389       0.53      0.28     0.03     0.98 1.00     4605     2127
#> pwindow_id390       0.39      0.27     0.02     0.94 1.00     4628     2345
#> pwindow_id391       0.53      0.29     0.03     0.98 1.00     4511     2429
#> pwindow_id392       0.51      0.29     0.03     0.98 1.00     3606     2167
#> pwindow_id393       0.53      0.28     0.03     0.98 1.00     5571     2497
#> pwindow_id394       0.52      0.29     0.03     0.97 1.00     4418     1968
#> pwindow_id395       0.54      0.29     0.02     0.98 1.00     4073     2205
#> pwindow_id396       0.48      0.28     0.02     0.97 1.00     4931     2247
#> pwindow_id397       0.42      0.27     0.02     0.95 1.00     3945     2053
#> pwindow_id398       0.48      0.28     0.03     0.97 1.00     4816     2005
#> pwindow_id399       0.50      0.29     0.02     0.97 1.00     5016     2046
#> pwindow_id400       0.54      0.29     0.04     0.98 1.00     4429     2483
#> pwindow_id401       0.54      0.30     0.02     0.98 1.00     5248     2347
#> pwindow_id402       0.47      0.28     0.02     0.97 1.00     3896     2367
#> pwindow_id403       0.54      0.28     0.03     0.98 1.00     4640     2204
#> pwindow_id404       0.50      0.28     0.03     0.97 1.00     4466     2479
#> pwindow_id405       0.44      0.28     0.02     0.96 1.00     4066     2040
#> pwindow_id406       0.54      0.28     0.03     0.97 1.00     4081     2410
#> pwindow_id407       0.52      0.28     0.03     0.98 1.00     4615     2285
#> pwindow_id408       0.43      0.27     0.02     0.95 1.00     4240     2188
#> pwindow_id409       0.46      0.29     0.02     0.97 1.00     5142     2480
#> pwindow_id410       0.48      0.29     0.03     0.97 1.00     5098     2421
#> pwindow_id411       0.36      0.26     0.01     0.93 1.00     4688     2289
#> pwindow_id412       0.52      0.29     0.03     0.98 1.00     4232     2270
#> pwindow_id413       0.53      0.28     0.03     0.98 1.00     4870     2567
#> pwindow_id414       0.48      0.28     0.03     0.97 1.00     5339     2608
#> pwindow_id415       0.53      0.29     0.03     0.98 1.00     4539     2178
#> pwindow_id416       0.51      0.29     0.02     0.98 1.00     3480     2026
#> pwindow_id417       0.51      0.29     0.02     0.98 1.00     3813     2051
#> pwindow_id418       0.54      0.28     0.03     0.98 1.00     4419     2458
#> pwindow_id419       0.47      0.29     0.02     0.96 1.00     5339     2584
#> pwindow_id420       0.52      0.28     0.04     0.97 1.00     4675     2273
#> pwindow_id421       0.46      0.28     0.03     0.96 1.00     4490     2233
#> pwindow_id422       0.50      0.28     0.03     0.97 1.00     4926     2542
#> pwindow_id423       0.36      0.26     0.01     0.92 1.00     5120     2420
#> pwindow_id424       0.43      0.28     0.02     0.96 1.00     4582     2265
#> pwindow_id425       0.49      0.29     0.02     0.97 1.00     4483     2560
#> pwindow_id426       0.53      0.29     0.03     0.98 1.00     4935     2772
#> pwindow_id427       0.45      0.28     0.02     0.96 1.00     5178     2397
#> pwindow_id428       0.47      0.28     0.03     0.97 1.00     4010     1895
#> pwindow_id429       0.47      0.28     0.02     0.97 1.00     4233     2606
#> pwindow_id430       0.39      0.27     0.01     0.94 1.00     3958     2481
#> pwindow_id431       0.44      0.28     0.02     0.96 1.00     4139     2470
#> pwindow_id432       0.37      0.27     0.01     0.93 1.00     4326     2495
#> pwindow_id433       0.41      0.28     0.02     0.94 1.00     4275     2284
#> pwindow_id434       0.51      0.29     0.03     0.97 1.00     4107     2284
#> pwindow_id435       0.49      0.29     0.03     0.98 1.00     4901     2492
#> pwindow_id436       0.48      0.29     0.02     0.96 1.00     4725     2549
#> pwindow_id437       0.46      0.29     0.02     0.97 1.00     4462     2327
#> pwindow_id438       0.40      0.27     0.02     0.94 1.00     4990     2217
#> pwindow_id439       0.46      0.28     0.02     0.96 1.00     4155     2347
#> pwindow_id440       0.38      0.27     0.01     0.93 1.00     4584     2343
#> pwindow_id441       0.50      0.29     0.03     0.98 1.00     5067     2318
#> pwindow_id442       0.49      0.28     0.03     0.96 1.00     4378     2545
#> pwindow_id443       0.51      0.29     0.02     0.98 1.00     3544     1961
#> pwindow_id444       0.38      0.26     0.02     0.93 1.00     4706     2776
#> pwindow_id445       0.42      0.28     0.02     0.96 1.00     4413     2215
#> pwindow_id446       0.48      0.29     0.03     0.97 1.00     4868     2222
#> pwindow_id447       0.43      0.28     0.02     0.95 1.00     5194     2218
#> pwindow_id448       0.50      0.29     0.03     0.98 1.00     3589     2150
#> pwindow_id449       0.30      0.23     0.01     0.84 1.00     4965     2272
#> pwindow_id450       0.48      0.29     0.02     0.97 1.00     4583     1958
#> pwindow_id451       0.43      0.28     0.02     0.96 1.00     4551     2254
#> pwindow_id452       0.44      0.28     0.02     0.96 1.00     4204     2241
#> pwindow_id453       0.47      0.28     0.02     0.97 1.00     4960     1979
#> pwindow_id454       0.46      0.29     0.02     0.97 1.00     4085     2102
#> pwindow_id455       0.42      0.28     0.02     0.95 1.00     4660     2242
#> pwindow_id456       0.43      0.28     0.02     0.96 1.00     4897     2471
#> pwindow_id457       0.45      0.28     0.02     0.96 1.00     3934     2291
#> pwindow_id458       0.44      0.28     0.02     0.96 1.00     4528     2481
#> pwindow_id459       0.37      0.27     0.01     0.93 1.00     4818     2164
#> pwindow_id460       0.42      0.28     0.02     0.95 1.00     4519     2251
#> pwindow_id461       0.41      0.27     0.02     0.95 1.00     4501     2069
#> swindow_id1         0.47      0.28     0.02     0.96 1.00     4433     2266
#> swindow_id2         0.44      0.28     0.02     0.95 1.00     4802     2355
#> swindow_id3         0.47      0.29     0.02     0.97 1.00     5291     2353
#> swindow_id4         0.43      0.29     0.02     0.96 1.00     4452     2216
#> swindow_id5         0.49      0.29     0.02     0.97 1.00     4269     2173
#> swindow_id6         0.56      0.27     0.05     0.98 1.00     4804     2428
#> swindow_id7         0.47      0.29     0.02     0.97 1.00     5566     2193
#> swindow_id8         0.55      0.28     0.03     0.98 1.00     4462     2048
#> swindow_id9         0.44      0.28     0.02     0.96 1.00     5064     2522
#> swindow_id10        0.51      0.29     0.02     0.98 1.00     4368     1988
#> swindow_id11        0.47      0.28     0.03     0.97 1.00     4426     2052
#> swindow_id12        0.46      0.29     0.02     0.96 1.00     4736     2521
#> swindow_id13        0.48      0.29     0.02     0.97 1.00     5095     2548
#> swindow_id14        0.45      0.28     0.02     0.96 1.00     5007     2512
#> swindow_id15        0.49      0.28     0.02     0.97 1.00     4412     2461
#> swindow_id16        0.45      0.28     0.02     0.96 1.00     5229     2396
#> swindow_id17        0.46      0.28     0.02     0.97 1.00     5574     2406
#> swindow_id18        0.62      0.26     0.07     0.98 1.00     3525     2058
#> swindow_id19        0.46      0.28     0.02     0.97 1.00     5006     2751
#> swindow_id20        0.48      0.29     0.02     0.97 1.00     4358     2147
#> swindow_id21        0.49      0.28     0.02     0.97 1.00     4880     2426
#> swindow_id22        0.54      0.29     0.04     0.98 1.00     5463     2470
#> swindow_id23        0.59      0.28     0.04     0.99 1.00     5201     2417
#> swindow_id24        0.47      0.28     0.02     0.97 1.00     3784     1943
#> swindow_id25        0.47      0.29     0.02     0.97 1.00     5407     2439
#> swindow_id26        0.46      0.29     0.02     0.97 1.00     4194     2005
#> swindow_id27        0.44      0.28     0.02     0.96 1.00     4424     2206
#> swindow_id28        0.60      0.27     0.06     0.98 1.00     3720     2251
#> swindow_id29        0.45      0.28     0.02     0.96 1.00     4707     2365
#> swindow_id30        0.45      0.28     0.02     0.96 1.00     4580     2392
#> swindow_id31        0.44      0.28     0.02     0.96 1.00     4849     2409
#> swindow_id32        0.49      0.28     0.03     0.97 1.00     4791     2559
#> swindow_id33        0.47      0.29     0.02     0.97 1.00     3956     2195
#> swindow_id34        0.45      0.28     0.02     0.96 1.00     4516     2661
#> swindow_id35        0.51      0.28     0.03     0.97 1.00     5348     2530
#> swindow_id36        0.49      0.28     0.03     0.97 1.00     4971     2449
#> swindow_id37        0.54      0.28     0.04     0.98 1.00     3782     2244
#> swindow_id38        0.44      0.28     0.02     0.97 1.00     4751     2336
#> swindow_id39        0.48      0.29     0.02     0.98 1.00     5002     2568
#> swindow_id40        0.44      0.29     0.02     0.96 1.00     4491     2635
#> swindow_id41        0.49      0.29     0.02     0.98 1.00     4335     2218
#> swindow_id42        0.44      0.28     0.02     0.97 1.00     4118     2029
#> swindow_id43        0.54      0.29     0.03     0.98 1.00     4789     2154
#> swindow_id44        0.50      0.29     0.02     0.98 1.00     5155     2505
#> swindow_id45        0.45      0.29     0.02     0.97 1.00     5308     2208
#> swindow_id46        0.50      0.29     0.03     0.98 1.00     4520     2477
#> swindow_id47        0.47      0.29     0.02     0.97 1.00     4142     2531
#> swindow_id48        0.48      0.29     0.02     0.97 1.00     4340     2200
#> swindow_id49        0.46      0.29     0.02     0.96 1.00     4042     2503
#> swindow_id50        0.50      0.29     0.03     0.97 1.00     4614     2345
#> swindow_id51        0.44      0.28     0.02     0.96 1.00     4790     2658
#> swindow_id52        0.54      0.28     0.03     0.98 1.00     4490     2129
#> swindow_id53        0.44      0.29     0.01     0.96 1.00     4424     2122
#> swindow_id54        0.52      0.29     0.02     0.98 1.00     4464     2155
#> swindow_id55        0.51      0.29     0.03     0.97 1.00     4251     2636
#> swindow_id56        0.47      0.29     0.02     0.97 1.00     4022     2208
#> swindow_id57        0.48      0.28     0.02     0.97 1.01     4190     2352
#> swindow_id58        0.45      0.29     0.02     0.97 1.00     4098     2229
#> swindow_id59        0.44      0.29     0.01     0.97 1.00     4833     2151
#> swindow_id60        0.47      0.29     0.02     0.97 1.00     4182     2283
#> swindow_id61        0.55      0.29     0.03     0.98 1.00     4169     2044
#> swindow_id62        0.52      0.29     0.03     0.98 1.00     4576     2471
#> swindow_id63        0.48      0.29     0.02     0.97 1.00     4787     2734
#> swindow_id64        0.47      0.28     0.02     0.97 1.00     4898     2548
#> swindow_id65        0.44      0.29     0.02     0.97 1.00     4352     2461
#> swindow_id66        0.46      0.29     0.02     0.97 1.00     4785     2407
#> swindow_id67        0.53      0.29     0.03     0.98 1.00     5494     2920
#> swindow_id68        0.46      0.29     0.02     0.97 1.00     3509     2133
#> swindow_id69        0.51      0.28     0.03     0.97 1.00     3873     1907
#> swindow_id70        0.47      0.29     0.02     0.97 1.00     4433     2412
#> swindow_id71        0.56      0.28     0.05     0.98 1.00     5007     2378
#> swindow_id72        0.62      0.27     0.05     0.99 1.00     4515     2404
#> swindow_id73        0.44      0.29     0.02     0.96 1.00     4199     2311
#> swindow_id74        0.50      0.29     0.02     0.98 1.00     4100     2276
#> swindow_id75        0.51      0.29     0.03     0.98 1.00     5273     2359
#> swindow_id76        0.56      0.28     0.04     0.98 1.00     5599     1968
#> swindow_id77        0.47      0.28     0.03     0.97 1.00     3863     2330
#> swindow_id78        0.44      0.28     0.02     0.96 1.00     4567     2151
#> swindow_id79        0.57      0.27     0.04     0.98 1.00     4441     2430
#> swindow_id80        0.44      0.28     0.02     0.95 1.00     4081     2357
#> swindow_id81        0.46      0.29     0.02     0.97 1.00     4216     2365
#> swindow_id82        0.50      0.29     0.03     0.97 1.00     4221     2196
#> swindow_id83        0.54      0.28     0.03     0.98 1.00     4428     2247
#> swindow_id84        0.62      0.27     0.06     0.99 1.00     4540     2196
#> swindow_id85        0.51      0.29     0.02     0.97 1.00     3613     1708
#> swindow_id86        0.45      0.29     0.02     0.97 1.00     4500     2200
#> swindow_id87        0.53      0.28     0.04     0.98 1.00     4967     2276
#> swindow_id88        0.48      0.29     0.03     0.97 1.00     5393     2082
#> swindow_id89        0.51      0.29     0.03     0.98 1.00     4011     2060
#> swindow_id90        0.45      0.29     0.02     0.97 1.00     3112     2054
#> swindow_id91        0.49      0.29     0.02     0.97 1.00     4278     1744
#> swindow_id92        0.58      0.28     0.04     0.98 1.00     3933     2106
#> swindow_id93        0.48      0.28     0.02     0.97 1.00     4499     2374
#> swindow_id94        0.60      0.26     0.06     0.98 1.00     4674     2435
#> swindow_id95        0.48      0.29     0.02     0.97 1.00     4194     2033
#> swindow_id96        0.45      0.28     0.02     0.97 1.00     3954     2061
#> swindow_id97        0.48      0.29     0.02     0.97 1.00     4870     2349
#> swindow_id98        0.55      0.27     0.05     0.98 1.00     4809     2396
#> swindow_id99        0.45      0.29     0.02     0.96 1.00     6088     2561
#> swindow_id100       0.47      0.29     0.02     0.97 1.00     3785     2197
#> swindow_id101       0.47      0.28     0.02     0.97 1.00     4228     2137
#> swindow_id102       0.57      0.28     0.05     0.98 1.00     4264     2229
#> swindow_id103       0.44      0.28     0.02     0.96 1.00     4915     2491
#> swindow_id104       0.46      0.28     0.03     0.97 1.00     4579     2277
#> swindow_id105       0.47      0.29     0.02     0.97 1.00     5959     2330
#> swindow_id106       0.48      0.29     0.02     0.97 1.00     4030     2175
#> swindow_id107       0.46      0.29     0.01     0.97 1.00     4909     2182
#> swindow_id108       0.49      0.29     0.03     0.97 1.00     4561     2097
#> swindow_id109       0.43      0.28     0.02     0.96 1.00     4014     2297
#> swindow_id110       0.44      0.29     0.02     0.96 1.00     5065     2223
#> swindow_id111       0.52      0.28     0.03     0.98 1.00     4406     2274
#> swindow_id112       0.47      0.29     0.02     0.97 1.00     3933     2177
#> swindow_id113       0.45      0.28     0.02     0.96 1.00     4309     2296
#> swindow_id114       0.50      0.28     0.03     0.97 1.00     5849     2672
#> swindow_id115       0.43      0.28     0.02     0.97 1.00     4019     2341
#> swindow_id116       0.44      0.29     0.02     0.97 1.00     5472     2406
#> swindow_id117       0.43      0.28     0.02     0.96 1.00     5131     2647
#> swindow_id118       0.46      0.29     0.02     0.98 1.00     3447     1666
#> swindow_id119       0.47      0.29     0.02     0.97 1.00     3933     2205
#> swindow_id120       0.44      0.29     0.02     0.97 1.00     5516     2409
#> swindow_id121       0.46      0.28     0.03     0.97 1.00     5588     2480
#> swindow_id122       0.45      0.28     0.02     0.95 1.00     4668     2206
#> swindow_id123       0.44      0.28     0.02     0.96 1.00     5200     2056
#> swindow_id124       0.47      0.29     0.02     0.96 1.00     5100     2245
#> swindow_id125       0.50      0.28     0.03     0.97 1.00     4562     2268
#> swindow_id126       0.44      0.29     0.02     0.96 1.00     4493     2204
#> swindow_id127       0.53      0.29     0.03     0.98 1.00     3687     2199
#> swindow_id128       0.43      0.29     0.02     0.97 1.00     4128     2154
#> swindow_id129       0.52      0.29     0.03     0.98 1.00     4353     2060
#> swindow_id130       0.47      0.29     0.02     0.97 1.00     5009     2372
#> swindow_id131       0.43      0.28     0.02     0.96 1.00     5235     2788
#> swindow_id132       0.50      0.28     0.03     0.97 1.00     4776     2601
#> swindow_id133       0.50      0.28     0.03     0.97 1.00     4176     2255
#> swindow_id134       0.49      0.29     0.03     0.97 1.00     4828     2658
#> swindow_id135       0.51      0.28     0.03     0.97 1.00     4949     2643
#> swindow_id136       0.46      0.28     0.02     0.96 1.00     4624     2633
#> swindow_id137       0.45      0.29     0.02     0.97 1.00     3925     2313
#> swindow_id138       0.52      0.29     0.03     0.98 1.00     3523     2419
#> swindow_id139       0.43      0.28     0.02     0.96 1.00     3524     2220
#> swindow_id140       0.52      0.28     0.03     0.98 1.00     5782     2335
#> swindow_id141       0.50      0.29     0.02     0.98 1.00     4672     2418
#> swindow_id142       0.49      0.28     0.03     0.96 1.00     4707     2647
#> swindow_id143       0.51      0.28     0.03     0.98 1.00     5332     2754
#> swindow_id144       0.48      0.28     0.02     0.97 1.00     4435     2189
#> swindow_id145       0.46      0.29     0.02     0.97 1.00     5337     2399
#> swindow_id146       0.63      0.26     0.08     0.98 1.00     4009     2373
#> swindow_id147       0.45      0.28     0.02     0.96 1.00     5074     2432
#> swindow_id148       0.48      0.29     0.02     0.98 1.00     3969     2540
#> swindow_id149       0.50      0.29     0.02     0.98 1.00     3512     2071
#> swindow_id150       0.47      0.28     0.02     0.97 1.00     4552     2574
#> swindow_id151       0.52      0.29     0.03     0.98 1.00     3866     2403
#> swindow_id152       0.49      0.29     0.02     0.97 1.00     5004     2241
#> swindow_id153       0.51      0.29     0.03     0.98 1.00     4861     2423
#> swindow_id154       0.49      0.29     0.02     0.97 1.00     5114     2206
#> swindow_id155       0.50      0.28     0.03     0.97 1.00     4250     1958
#> swindow_id156       0.49      0.28     0.03     0.97 1.00     4777     2509
#> swindow_id157       0.47      0.28     0.02     0.96 1.00     4246     2371
#> swindow_id158       0.48      0.29     0.03     0.97 1.00     5088     2304
#> swindow_id159       0.52      0.29     0.02     0.98 1.00     4295     2005
#> swindow_id160       0.44      0.28     0.02     0.96 1.00     4473     2249
#> swindow_id161       0.49      0.29     0.03     0.98 1.01     3310     2023
#> swindow_id162       0.54      0.28     0.04     0.98 1.00     4588     1801
#> swindow_id163       0.51      0.29     0.03     0.98 1.00     4398     2437
#> swindow_id164       0.47      0.29     0.02     0.97 1.00     4729     2418
#> swindow_id165       0.46      0.28     0.02     0.97 1.00     4681     2330
#> swindow_id166       0.43      0.29     0.02     0.95 1.00     5061     2451
#> swindow_id167       0.46      0.29     0.01     0.97 1.00     4275     2043
#> swindow_id168       0.45      0.28     0.02     0.96 1.00     4923     2316
#> swindow_id169       0.45      0.28     0.02     0.96 1.00     4536     2016
#> swindow_id170       0.48      0.28     0.02     0.97 1.00     4244     2289
#> swindow_id171       0.48      0.29     0.02     0.97 1.00     4504     2371
#> swindow_id172       0.44      0.29     0.02     0.97 1.00     4186     2346
#> swindow_id173       0.48      0.29     0.02     0.97 1.00     4850     2091
#> swindow_id174       0.61      0.27     0.06     0.99 1.00     5335     2419
#> swindow_id175       0.47      0.29     0.02     0.97 1.00     4295     2057
#> swindow_id176       0.45      0.28     0.02     0.97 1.00     4793     2490
#> swindow_id177       0.49      0.29     0.03     0.97 1.00     5135     2538
#> swindow_id178       0.55      0.28     0.04     0.98 1.00     4012     2202
#> swindow_id179       0.46      0.28     0.02     0.97 1.00     4108     2336
#> swindow_id180       0.43      0.28     0.02     0.95 1.00     4466     2499
#> swindow_id181       0.49      0.29     0.03     0.97 1.00     4587     2534
#> swindow_id182       0.50      0.28     0.02     0.98 1.00     4419     2402
#> swindow_id183       0.49      0.29     0.03     0.97 1.00     5067     2551
#> swindow_id184       0.47      0.28     0.02     0.97 1.00     4891     2168
#> swindow_id185       0.46      0.29     0.02     0.97 1.00     4924     2352
#> swindow_id186       0.49      0.28     0.03     0.97 1.00     4027     2223
#> swindow_id187       0.52      0.29     0.03     0.98 1.00     4350     2222
#> swindow_id188       0.51      0.29     0.03     0.98 1.00     4431     2169
#> swindow_id189       0.44      0.28     0.02     0.96 1.00     4186     2074
#> swindow_id190       0.47      0.28     0.02     0.97 1.00     5574     2192
#> swindow_id191       0.59      0.27     0.05     0.98 1.01     5050     1914
#> swindow_id192       0.48      0.29     0.02     0.97 1.00     4773     2303
#> swindow_id193       0.47      0.29     0.02     0.97 1.00     3838     2295
#> swindow_id194       0.52      0.29     0.02     0.98 1.00     6070     2268
#> swindow_id195       0.46      0.29     0.02     0.97 1.00     4061     2311
#> swindow_id196       0.48      0.28     0.02     0.97 1.00     4356     2135
#> swindow_id197       0.49      0.29     0.02     0.97 1.00     4618     2230
#> swindow_id198       0.45      0.28     0.02     0.96 1.00     5695     2508
#> swindow_id199       0.44      0.28     0.02     0.96 1.00     4167     2355
#> swindow_id200       0.44      0.29     0.02     0.96 1.00     4532     2146
#> swindow_id201       0.45      0.29     0.02     0.97 1.00     4608     2418
#> swindow_id202       0.52      0.29     0.03     0.98 1.00     4761     2314
#> swindow_id203       0.46      0.29     0.02     0.97 1.00     4544     2450
#> swindow_id204       0.47      0.28     0.02     0.97 1.00     5838     2533
#> swindow_id205       0.47      0.28     0.02     0.96 1.00     3821     2253
#> swindow_id206       0.54      0.28     0.04     0.97 1.00     5573     2549
#> swindow_id207       0.53      0.28     0.03     0.98 1.00     4925     2351
#> swindow_id208       0.49      0.28     0.03     0.97 1.00     4283     2279
#> swindow_id209       0.53      0.28     0.03     0.98 1.00     3936     2219
#> swindow_id210       0.49      0.28     0.02     0.97 1.00     5406     2105
#> swindow_id211       0.51      0.29     0.02     0.98 1.00     4673     1975
#> swindow_id212       0.51      0.29     0.03     0.98 1.00     4615     2137
#> swindow_id213       0.51      0.29     0.02     0.98 1.00     4233     2242
#> swindow_id214       0.48      0.28     0.03     0.97 1.00     4621     2019
#> swindow_id215       0.57      0.27     0.05     0.98 1.00     5942     2268
#> swindow_id216       0.48      0.29     0.02     0.97 1.00     4470     2049
#> swindow_id217       0.48      0.29     0.03     0.97 1.00     4882     2715
#> swindow_id218       0.44      0.28     0.02     0.95 1.00     4762     2262
#> swindow_id219       0.47      0.29     0.02     0.97 1.00     4944     2562
#> swindow_id220       0.45      0.28     0.02     0.97 1.00     4584     1963
#> swindow_id221       0.44      0.28     0.02     0.96 1.00     4963     2446
#> swindow_id222       0.48      0.28     0.03     0.97 1.00     5707     2498
#> swindow_id223       0.50      0.29     0.03     0.98 1.00     3358     1790
#> swindow_id224       0.47      0.29     0.02     0.97 1.00     3599     2560
#> swindow_id225       0.48      0.29     0.02     0.97 1.00     4379     2186
#> swindow_id226       0.53      0.28     0.03     0.98 1.00     3895     2422
#> swindow_id227       0.45      0.28     0.02     0.96 1.00     4886     2558
#> swindow_id228       0.59      0.28     0.05     0.99 1.00     3435     2106
#> swindow_id229       0.44      0.28     0.02     0.97 1.00     4859     2458
#> swindow_id230       0.50      0.29     0.02     0.98 1.00     5085     2042
#> swindow_id231       0.48      0.29     0.03     0.97 1.00     4612     2384
#> swindow_id232       0.47      0.28     0.02     0.97 1.00     4867     2366
#> swindow_id233       0.56      0.28     0.04     0.98 1.00     4549     2309
#> swindow_id234       0.64      0.27     0.08     0.99 1.00     3812     2143
#> swindow_id235       0.47      0.29     0.02     0.97 1.00     4676     2173
#> swindow_id236       0.77      0.19     0.28     0.99 1.00     4678     1959
#> swindow_id237       0.54      0.28     0.03     0.98 1.00     4049     2118
#> swindow_id238       0.49      0.29     0.02     0.97 1.00     4362     2425
#> swindow_id239       0.45      0.29     0.02     0.97 1.00     4096     2098
#> swindow_id240       0.49      0.28     0.02     0.97 1.00     4345     2255
#> swindow_id241       0.45      0.29     0.02     0.97 1.00     4117     2388
#> swindow_id242       0.46      0.29     0.02     0.97 1.00     5013     2371
#> swindow_id243       0.51      0.29     0.04     0.97 1.00     4369     2476
#> swindow_id244       0.51      0.29     0.03     0.98 1.00     4650     2437
#> swindow_id245       0.43      0.28     0.02     0.96 1.00     4629     2441
#> swindow_id246       0.62      0.26     0.07     0.98 1.00     4990     2272
#> swindow_id247       0.54      0.28     0.04     0.98 1.00     3986     2660
#> swindow_id248       0.53      0.28     0.03     0.98 1.00     4681     2462
#> swindow_id249       0.46      0.28     0.02     0.96 1.00     5560     2414
#> swindow_id250       0.45      0.29     0.02     0.96 1.00     5168     2432
#> swindow_id251       0.44      0.28     0.02     0.96 1.00     5070     2586
#> swindow_id252       0.68      0.24     0.11     0.99 1.00     4892     2474
#> swindow_id253       0.47      0.29     0.03     0.98 1.00     3701     2143
#> swindow_id254       0.52      0.28     0.03     0.97 1.00     4404     2159
#> swindow_id255       0.63      0.26     0.06     0.99 1.00     4921     2366
#> swindow_id256       0.64      0.26     0.08     0.99 1.00     5585     1982
#> swindow_id257       0.46      0.29     0.02     0.97 1.00     4963     2366
#> swindow_id258       0.52      0.29     0.03     0.98 1.00     5278     2612
#> swindow_id259       0.49      0.29     0.03     0.97 1.00     4900     2153
#> swindow_id260       0.48      0.28     0.02     0.97 1.00     4771     2113
#> swindow_id261       0.47      0.29     0.02     0.97 1.00     3786     2395
#> swindow_id262       0.48      0.30     0.02     0.98 1.01     4334     2248
#> swindow_id263       0.46      0.29     0.02     0.97 1.00     4522     1806
#> swindow_id264       0.46      0.29     0.02     0.97 1.00     4574     2394
#> swindow_id265       0.55      0.28     0.04     0.98 1.00     4943     2413
#> swindow_id266       0.49      0.29     0.02     0.98 1.00     3555     1931
#> swindow_id267       0.56      0.29     0.04     0.98 1.00     3853     2091
#> swindow_id268       0.47      0.29     0.02     0.97 1.00     4714     2579
#> swindow_id269       0.49      0.29     0.02     0.98 1.00     4626     2344
#> swindow_id270       0.47      0.30     0.02     0.97 1.00     3565     2026
#> swindow_id271       0.44      0.28     0.02     0.96 1.00     4700     2215
#> swindow_id272       0.54      0.29     0.03     0.98 1.00     4473     2532
#> swindow_id273       0.53      0.29     0.03     0.98 1.00     6323     2523
#> swindow_id274       0.50      0.29     0.03     0.98 1.00     4799     2467
#> swindow_id275       0.48      0.29     0.02     0.97 1.00     4601     2472
#> swindow_id276       0.52      0.29     0.02     0.98 1.00     4619     2018
#> swindow_id277       0.48      0.28     0.02     0.97 1.00     4419     2333
#> swindow_id278       0.50      0.29     0.02     0.97 1.00     3325     2136
#> swindow_id279       0.47      0.29     0.03     0.97 1.00     4538     2374
#> swindow_id280       0.47      0.29     0.02     0.97 1.00     3358     2091
#> swindow_id281       0.55      0.28     0.04     0.98 1.00     4035     2170
#> swindow_id282       0.46      0.29     0.02     0.97 1.01     4688     2239
#> swindow_id283       0.52      0.29     0.02     0.98 1.00     5005     2389
#> swindow_id284       0.56      0.28     0.04     0.98 1.00     4126     2090
#> swindow_id285       0.50      0.28     0.03     0.98 1.00     4961     1818
#> swindow_id286       0.50      0.28     0.03     0.97 1.00     3883     2058
#> swindow_id287       0.48      0.29     0.02     0.97 1.00     4802     2277
#> swindow_id288       0.46      0.28     0.02     0.97 1.00     4364     2272
#> swindow_id289       0.44      0.29     0.02     0.96 1.00     4980     2095
#> swindow_id290       0.45      0.29     0.02     0.97 1.00     4618     2664
#> swindow_id291       0.53      0.28     0.04     0.98 1.00     5155     2344
#> swindow_id292       0.45      0.28     0.02     0.96 1.00     5879     2351
#> swindow_id293       0.55      0.29     0.03     0.98 1.00     4026     2522
#> swindow_id294       0.55      0.29     0.03     0.98 1.00     3793     1836
#> swindow_id295       0.51      0.28     0.04     0.98 1.00     5244     2306
#> swindow_id296       0.59      0.28     0.04     0.99 1.00     3695     1959
#> swindow_id297       0.48      0.29     0.02     0.97 1.00     5406     1909
#> swindow_id298       0.45      0.28     0.02     0.96 1.00     4367     2619
#> swindow_id299       0.56      0.28     0.03     0.98 1.00     4411     2353
#> swindow_id300       0.44      0.28     0.02     0.96 1.00     3792     2280
#> swindow_id301       0.46      0.28     0.02     0.97 1.00     4923     2233
#> swindow_id302       0.43      0.29     0.02     0.96 1.00     4925     2289
#> swindow_id303       0.44      0.28     0.02     0.96 1.00     4834     2565
#> swindow_id304       0.50      0.28     0.02     0.98 1.00     4486     2452
#> swindow_id305       0.55      0.29     0.03     0.98 1.00     5115     2331
#> swindow_id306       0.47      0.29     0.02     0.96 1.00     4106     2335
#> swindow_id307       0.48      0.28     0.02     0.97 1.00     4253     2076
#> swindow_id308       0.58      0.28     0.05     0.98 1.00     4960     2534
#> swindow_id309       0.50      0.29     0.03     0.98 1.00     4440     1915
#> swindow_id310       0.46      0.28     0.02     0.97 1.00     4554     2363
#> swindow_id311       0.53      0.28     0.03     0.98 1.00     5313     2321
#> swindow_id312       0.49      0.29     0.03     0.97 1.00     4809     2479
#> swindow_id313       0.51      0.29     0.02     0.98 1.00     4161     1878
#> swindow_id314       0.45      0.28     0.02     0.96 1.00     4824     2509
#> swindow_id315       0.46      0.29     0.02     0.97 1.00     4286     2392
#> swindow_id316       0.54      0.28     0.03     0.98 1.00     4389     2384
#> swindow_id317       0.49      0.29     0.02     0.97 1.00     4177     2511
#> swindow_id318       0.45      0.29     0.02     0.97 1.00     4573     2354
#> swindow_id319       0.44      0.29     0.01     0.97 1.00     4361     2603
#> swindow_id320       0.45      0.28     0.02     0.96 1.00     5345     2575
#> swindow_id321       0.45      0.29     0.02     0.97 1.00     4764     2468
#> swindow_id322       0.44      0.28     0.02     0.96 1.00     4480     2465
#> swindow_id323       0.57      0.28     0.04     0.98 1.00     3650     2096
#> swindow_id324       0.45      0.29     0.02     0.96 1.00     5987     2450
#> swindow_id325       0.48      0.29     0.02     0.97 1.00     4846     2511
#> swindow_id326       0.44      0.28     0.02     0.96 1.00     4073     2099
#> swindow_id327       0.46      0.28     0.02     0.96 1.00     4986     2369
#> swindow_id328       0.56      0.28     0.04     0.98 1.00     4799     2104
#> swindow_id329       0.45      0.29     0.02     0.97 1.00     4909     2307
#> swindow_id330       0.45      0.28     0.03     0.96 1.00     4525     2459
#> swindow_id331       0.65      0.26     0.09     0.99 1.00     5199     2379
#> swindow_id332       0.50      0.29     0.02     0.98 1.00     4997     2420
#> swindow_id333       0.44      0.28     0.02     0.96 1.00     4934     2165
#> swindow_id334       0.50      0.28     0.02     0.97 1.00     3630     2112
#> swindow_id335       0.50      0.29     0.03     0.98 1.00     4464     2421
#> swindow_id336       0.50      0.29     0.03     0.97 1.00     4297     2614
#> swindow_id337       0.72      0.23     0.15     0.99 1.00     4449     2115
#> swindow_id338       0.45      0.29     0.02     0.96 1.01     5626     2618
#> swindow_id339       0.65      0.25     0.08     0.99 1.00     4610     2427
#> swindow_id340       0.52      0.29     0.02     0.98 1.00     4381     2075
#> swindow_id341       0.48      0.29     0.02     0.97 1.00     5691     2290
#> swindow_id342       0.46      0.29     0.02     0.97 1.00     3859     2039
#> swindow_id343       0.58      0.27     0.04     0.98 1.00     4753     2085
#> swindow_id344       0.51      0.29     0.02     0.97 1.00     4375     2287
#> swindow_id345       0.45      0.29     0.02     0.97 1.00     5152     2210
#> swindow_id346       0.49      0.28     0.02     0.97 1.00     4146     2139
#> swindow_id347       0.53      0.29     0.03     0.98 1.00     5122     2549
#> swindow_id348       0.54      0.28     0.03     0.98 1.00     4748     2109
#> swindow_id349       0.47      0.28     0.03     0.97 1.00     4110     2567
#> swindow_id350       0.49      0.29     0.02     0.97 1.00     4351     2470
#> swindow_id351       0.48      0.29     0.02     0.97 1.00     4718     2442
#> swindow_id352       0.48      0.29     0.03     0.98 1.01     4750     1995
#> swindow_id353       0.46      0.29     0.01     0.97 1.00     5054     2156
#> swindow_id354       0.46      0.29     0.02     0.97 1.01     5047     2023
#> swindow_id355       0.52      0.29     0.03     0.98 1.00     4341     2278
#> swindow_id356       0.57      0.28     0.04     0.98 1.00     4715     2500
#> swindow_id357       0.55      0.28     0.04     0.98 1.00     4787     2669
#> swindow_id358       0.50      0.28     0.03     0.97 1.00     2972     1865
#> swindow_id359       0.50      0.29     0.02     0.98 1.00     3901     1866
#> swindow_id360       0.64      0.26     0.08     0.99 1.00     4191     2445
#> swindow_id361       0.54      0.29     0.03     0.98 1.00     5310     2125
#> swindow_id362       0.52      0.29     0.03     0.98 1.00     4325     2307
#> swindow_id363       0.46      0.29     0.02     0.97 1.00     4274     2282
#> swindow_id364       0.53      0.29     0.03     0.97 1.01     4816     2279
#> swindow_id365       0.51      0.28     0.04     0.97 1.00     4947     2538
#> swindow_id366       0.48      0.28     0.02     0.97 1.00     4129     2508
#> swindow_id367       0.50      0.28     0.03     0.97 1.00     5336     2299
#> swindow_id368       0.46      0.29     0.02     0.97 1.00     3930     2107
#> swindow_id369       0.46      0.29     0.02     0.97 1.00     4219     2258
#> swindow_id370       0.48      0.29     0.02     0.97 1.00     4743     2690
#> swindow_id371       0.51      0.28     0.02     0.98 1.00     4364     2226
#> swindow_id372       0.62      0.27     0.06     0.98 1.00     4301     2300
#> swindow_id373       0.52      0.28     0.04     0.98 1.00     4312     2448
#> swindow_id374       0.52      0.28     0.03     0.97 1.00     3917     2280
#> swindow_id375       0.45      0.28     0.02     0.96 1.00     4640     2388
#> swindow_id376       0.60      0.27     0.05     0.98 1.00     3071     1859
#> swindow_id377       0.48      0.29     0.02     0.97 1.00     4705     2186
#> swindow_id378       0.57      0.28     0.04     0.98 1.00     3727     2019
#> swindow_id379       0.48      0.30     0.02     0.97 1.00     4654     2306
#> swindow_id380       0.61      0.27     0.05     0.99 1.00     4294     1996
#> swindow_id381       0.46      0.29     0.02     0.97 1.00     5594     2429
#> swindow_id382       0.59      0.28     0.05     0.98 1.00     4205     2284
#> swindow_id383       0.50      0.28     0.03     0.97 1.00     4576     2387
#> swindow_id384       0.46      0.29     0.02     0.97 1.00     4531     2474
#> swindow_id385       0.48      0.29     0.02     0.98 1.00     3451     1987
#> swindow_id386       0.49      0.29     0.02     0.97 1.01     3786     1744
#> swindow_id387       0.46      0.28     0.02     0.96 1.00     4710     2107
#> swindow_id388       0.47      0.28     0.02     0.96 1.00     4770     2608
#> swindow_id389       0.47      0.28     0.02     0.97 1.00     4996     2500
#> swindow_id390       0.61      0.27     0.07     0.98 1.00     5237     2295
#> swindow_id391       0.48      0.29     0.02     0.97 1.00     4230     2440
#> swindow_id392       0.49      0.28     0.03     0.97 1.00     5363     2298
#> swindow_id393       0.47      0.29     0.02     0.97 1.00     4955     2385
#> swindow_id394       0.48      0.29     0.03     0.97 1.00     4748     2718
#> swindow_id395       0.46      0.29     0.02     0.97 1.00     5175     2437
#> swindow_id396       0.52      0.29     0.03     0.98 1.00     4729     2128
#> swindow_id397       0.58      0.27     0.05     0.98 1.00     4238     2038
#> swindow_id398       0.52      0.28     0.03     0.97 1.00     4741     2350
#> swindow_id399       0.50      0.28     0.02     0.97 1.00     5117     2495
#> swindow_id400       0.46      0.28     0.02     0.97 1.00     4639     2689
#> swindow_id401       0.46      0.28     0.02     0.96 1.00     5157     2444
#> swindow_id402       0.54      0.28     0.03     0.98 1.00     3929     2175
#> swindow_id403       0.47      0.29     0.02     0.97 1.00     5082     2401
#> swindow_id404       0.50      0.29     0.03     0.98 1.01     4987     2233
#> swindow_id405       0.54      0.28     0.03     0.98 1.00     3912     2400
#> swindow_id406       0.46      0.29     0.02     0.97 1.00     4058     2179
#> swindow_id407       0.50      0.29     0.03     0.97 1.00     4372     2672
#> swindow_id408       0.57      0.28     0.04     0.98 1.00     4586     2153
#> swindow_id409       0.54      0.28     0.03     0.98 1.00     4186     2284
#> swindow_id410       0.51      0.28     0.04     0.98 1.01     4815     2431
#> swindow_id411       0.64      0.26     0.06     0.99 1.00     5142     1902
#> swindow_id412       0.49      0.29     0.02     0.97 1.00     5136     2466
#> swindow_id413       0.47      0.29     0.02     0.97 1.00     4214     1935
#> swindow_id414       0.52      0.29     0.03     0.97 1.00     4823     2357
#> swindow_id415       0.47      0.29     0.02     0.97 1.00     4441     2463
#> swindow_id416       0.49      0.29     0.03     0.97 1.00     4898     2170
#> swindow_id417       0.49      0.29     0.02     0.97 1.00     4170     2180
#> swindow_id418       0.47      0.29     0.02     0.97 1.00     5049     2309
#> swindow_id419       0.54      0.28     0.04     0.98 1.00     4485     2296
#> swindow_id420       0.48      0.29     0.03     0.97 1.00     4738     2663
#> swindow_id421       0.55      0.29     0.03     0.98 1.00     3559     1498
#> swindow_id422       0.50      0.29     0.03     0.98 1.00     5294     2185
#> swindow_id423       0.63      0.26     0.07     0.99 1.00     3613     2243
#> swindow_id424       0.57      0.28     0.03     0.98 1.00     5416     2344
#> swindow_id425       0.51      0.28     0.03     0.97 1.00     5318     2414
#> swindow_id426       0.47      0.29     0.02     0.98 1.00     4186     1817
#> swindow_id427       0.54      0.28     0.03     0.98 1.00     4762     2291
#> swindow_id428       0.52      0.29     0.03     0.97 1.00     4832     2542
#> swindow_id429       0.54      0.28     0.03     0.98 1.00     3972     2202
#> swindow_id430       0.60      0.27     0.07     0.98 1.00     4861     2442
#> swindow_id431       0.56      0.28     0.04     0.98 1.00     3654     1821
#> swindow_id432       0.62      0.27     0.05     0.99 1.00     4418     2061
#> swindow_id433       0.59      0.28     0.04     0.98 1.00     3388     1616
#> swindow_id434       0.50      0.29     0.03     0.97 1.00     5673     2702
#> swindow_id435       0.51      0.28     0.03     0.97 1.00     4372     2581
#> swindow_id436       0.52      0.28     0.03     0.97 1.00     4529     2566
#> swindow_id437       0.54      0.28     0.04     0.98 1.00     5563     2552
#> swindow_id438       0.59      0.28     0.04     0.98 1.00     4319     2282
#> swindow_id439       0.54      0.28     0.03     0.98 1.00     4476     2354
#> swindow_id440       0.62      0.26     0.07     0.98 1.00     4795     2437
#> swindow_id441       0.50      0.28     0.03     0.97 1.00     4641     2146
#> swindow_id442       0.51      0.29     0.03     0.97 1.00     5470     2462
#> swindow_id443       0.50      0.29     0.03     0.97 1.00     4288     2139
#> swindow_id444       0.64      0.25     0.09     0.98 1.00     3895     2287
#> swindow_id445       0.59      0.27     0.05     0.98 1.00     4176     2251
#> swindow_id446       0.51      0.28     0.03     0.98 1.00     3966     2122
#> swindow_id447       0.57      0.28     0.05     0.98 1.00     4350     2233
#> swindow_id448       0.50      0.29     0.03     0.98 1.00     4351     2492
#> swindow_id449       0.70      0.24     0.13     0.99 1.00     3353     1920
#> swindow_id450       0.52      0.28     0.03     0.98 1.00     4539     2275
#> swindow_id451       0.57      0.28     0.04     0.98 1.00     3258     1579
#> swindow_id452       0.54      0.28     0.04     0.98 1.00     4833     2215
#> swindow_id453       0.53      0.28     0.03     0.97 1.00     4416     2300
#> swindow_id454       0.55      0.28     0.03     0.98 1.00     3613     1900
#> swindow_id455       0.58      0.28     0.05     0.98 1.00     4610     2547
#> swindow_id456       0.57      0.29     0.03     0.99 1.00     5050     2478
#> swindow_id457       0.55      0.28     0.04     0.98 1.00     4042     2304
#> swindow_id458       0.55      0.29     0.03     0.98 1.00     5604     2379
#> swindow_id459       0.63      0.27     0.06     0.99 1.00     4446     2357
#> swindow_id460       0.57      0.28     0.04     0.98 1.00     3985     1901
#> swindow_id461       0.59      0.28     0.05     0.98 1.00     4927     2445
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
