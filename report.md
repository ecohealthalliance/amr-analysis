---
title: "AMR Country Hotspots"
output:
  rmdformats::html_clean:
    lightbox: TRUE
    gallery: TRUE
    thumbnails: FALSE
    keep_md: TRUE
---



```
##  Family: zero_inflated_poisson 
##   Links: mu = log; zi = logit 
## Formula: n_amr_events ~ log(livestock_consumption_kg_per_pcu) + log(livestock_pcu) + log(migrant_pop_perc) + ab_export_perc + health_expend_perc + human_consumption_ddd + log(gdp_dollars) + offset(log(population)) 
##          zi ~ log(pubs_sum + 0.1) + log(gdp_dollars) + log(population)
##    Data: country_mice (Number of observations: 200) 
## Samples: 120 chains, each with iter = 4000; warmup = 2000; thin = 1;
##          total post-warmup samples = 240000
## 
## Population-Level Effects: 
##                                     Estimate Est.Error l-95% CI u-95% CI
## Intercept                              -0.59      1.16    -2.73     1.76
## zi_Intercept                           33.16      5.93    22.26    45.50
## loglivestock_consumption_kg_per_pcu     0.25      0.07     0.12     0.39
## loglivestock_pcu                       -0.24      0.05    -0.32    -0.14
## logmigrant_pop_perc                     0.15      0.02     0.10     0.19
## ab_export_perc                          5.75      0.74     4.27     7.17
## health_expend_perc                      0.16      0.01     0.14     0.18
## human_consumption_ddd                   0.06      0.01     0.04     0.08
## loggdp_dollars                         -0.30      0.05    -0.40    -0.21
## zi_logpubs_sumP0.1                     -0.03      0.18    -0.39     0.32
## zi_loggdp_dollars                      -1.47      0.27    -2.03    -0.99
## zi_logpopulation                        0.27      0.23    -0.17     0.72
##                                     Eff.Sample Rhat
## Intercept                                  147 1.31
## zi_Intercept                            206314 1.00
## loglivestock_consumption_kg_per_pcu        156 1.28
## loglivestock_pcu                           136 1.34
## logmigrant_pop_perc                        339 1.11
## ab_export_perc                             528 1.06
## health_expend_perc                         837 1.04
## human_consumption_ddd                      159 1.27
## loggdp_dollars                             137 1.34
## zi_logpubs_sumP0.1                      223897 1.00
## zi_loggdp_dollars                       202061 1.00
## zi_logpopulation                        231927 1.00
## 
## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
## is a crude measure of effective sample size, and Rhat is the potential 
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

![](report_files/figure-html/diagnostics-1.png)<!-- -->![](report_files/figure-html/diagnostics-2.png)<!-- -->![](report_files/figure-html/diagnostics-3.png)<!-- -->![](report_files/figure-html/diagnostics-4.png)<!-- -->![](report_files/figure-html/diagnostics-5.png)<!-- -->

![](report_files/figure-html/plots-1.png)<!-- -->![](report_files/figure-html/plots-2.png)<!-- -->![](report_files/figure-html/plots-3.png)<!-- -->

