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
## Formula: n_amr_events ~ ln_livestock_consumption_kg_per_pcu + ln_livestock_pcu + ln_migrant_pop_perc + ab_export_perc + health_expend_perc + human_consumption_ddd + ln_gdp_per_capita + offset(ln_population) 
##          zi ~ ln_pubs_sum_per_capita + ln_gdp_per_capita + ln_population
##    Data: data[[i]] (Number of observations: 200) 
## Samples: 120 chains, each with iter = 2000; warmup = 1000; thin = 1;
##          total post-warmup samples = 120000
## 
## Population-Level Effects: 
##                                     Estimate Est.Error l-95% CI u-95% CI
## Intercept                             -10.17      1.06   -12.20    -8.14
## zi_Intercept                           30.59      5.97    19.55    43.03
## ln_livestock_consumption_kg_per_pcu    -0.13      0.05    -0.23    -0.03
## ln_livestock_pcu                       -0.42      0.03    -0.47    -0.36
## ln_migrant_pop_perc                     0.01      0.03    -0.05     0.07
## ab_export_perc                          5.28      0.71     3.85     6.66
## health_expend_perc                      0.11      0.01     0.08     0.13
## human_consumption_ddd                   0.09      0.01     0.08     0.11
## ln_gdp_per_capita                       0.23      0.06     0.12     0.34
## zi_ln_pubs_sum_per_capita              -0.14      0.20    -0.54     0.24
## zi_ln_gdp_per_capita                   -1.39      0.26    -1.93    -0.90
## zi_ln_population                       -1.20      0.21    -1.64    -0.82
##                                     Eff.Sample Rhat
## Intercept                                  114 1.46
## zi_Intercept                             84798 1.00
## ln_livestock_consumption_kg_per_pcu        222 1.17
## ln_livestock_pcu                            92 1.70
## ln_migrant_pop_perc                        173 1.25
## ab_export_perc                            6797 1.01
## health_expend_perc                         119 1.42
## human_consumption_ddd                      178 1.23
## ln_gdp_per_capita                          147 1.31
## zi_ln_pubs_sum_per_capita               115532 1.00
## zi_ln_gdp_per_capita                     96029 1.00
## zi_ln_population                         84592 1.00
## 
## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
## is a crude measure of effective sample size, and Rhat is the potential 
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

![](report_files/figure-html/diagnostics-1.png)<!-- -->![](report_files/figure-html/diagnostics-2.png)<!-- -->![](report_files/figure-html/diagnostics-3.png)<!-- -->![](report_files/figure-html/diagnostics-4.png)<!-- -->![](report_files/figure-html/diagnostics-5.png)<!-- -->

![](report_files/figure-html/plots-1.png)<!-- -->![](report_files/figure-html/plots-2.png)<!-- -->![](report_files/figure-html/plots-3.png)<!-- -->

