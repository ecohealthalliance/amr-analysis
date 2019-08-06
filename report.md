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
## Formula: n_amr_events ~ log(livestock_consumption_kg_per_pcu) + log(livestock_pcu) + migrant_pop_perc + ab_export_perc + health_expend_perc + human_consumption_ddd + log(gdp_dollars) + offset(log(population)) 
##          zi ~ pubs_sum + log(gdp_dollars) + log(population)
##    Data: country_mice (Number of observations: 200) 
## Samples: 120 chains, each with iter = 4000; warmup = 2000; thin = 1;
##          total post-warmup samples = 240000
## 
## Population-Level Effects: 
##                                     Estimate Est.Error l-95% CI u-95% CI
## Intercept                              -1.71      1.26    -3.98     0.90
## zi_Intercept                           32.68      5.25    23.04    43.63
## loglivestock_consumption_kg_per_pcu     0.16      0.07     0.02     0.30
## loglivestock_pcu                       -0.32      0.05    -0.41    -0.20
## migrant_pop_perc                        0.00      0.00    -0.00     0.01
## ab_export_perc                          6.57      0.73     5.10     7.97
## health_expend_perc                      0.19      0.01     0.17     0.21
## human_consumption_ddd                   0.08      0.01     0.06     0.10
## loggdp_dollars                         -0.24      0.06    -0.36    -0.14
## zi_pubs_sum                            -0.00      0.00    -0.00     0.00
## zi_loggdp_dollars                      -1.46      0.26    -2.00    -0.99
## zi_logpopulation                        0.27      0.21    -0.13     0.70
##                                     Eff.Sample Rhat
## Intercept                                  124 1.40
## zi_Intercept                            144047 1.00
## loglivestock_consumption_kg_per_pcu        132 1.36
## loglivestock_pcu                           103 1.55
## migrant_pop_perc                           337 1.11
## ab_export_perc                             526 1.07
## health_expend_perc                         280 1.13
## human_consumption_ddd                      145 1.31
## loggdp_dollars                             111 1.48
## zi_pubs_sum                             100054 1.00
## zi_loggdp_dollars                       118074 1.00
## zi_logpopulation                        136640 1.00
## 
## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
## is a crude measure of effective sample size, and Rhat is the potential 
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

![](report_files/figure-html/diagnostics-1.png)<!-- -->![](report_files/figure-html/diagnostics-2.png)<!-- -->![](report_files/figure-html/diagnostics-3.png)<!-- -->![](report_files/figure-html/diagnostics-4.png)<!-- -->![](report_files/figure-html/diagnostics-5.png)<!-- -->

![](report_files/figure-html/plots-1.png)<!-- -->![](report_files/figure-html/plots-2.png)<!-- -->![](report_files/figure-html/plots-3.png)<!-- -->

