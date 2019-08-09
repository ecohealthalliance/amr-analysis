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
## Formula: n_amr_events ~ ln_livestock_consumption_kg_per_pcu + ln_livestock_pcu + ln_migrant_pop_perc + ab_export_perc + health_expend_perc + human_consumption_ddd + ln_gdp_dollars + offset(ln_population) 
##          zi ~ ln_pubs_sum + ln_gdp_dollars + ln_population
##    Data: data[[i]] (Number of observations: 200) 
## Samples: 120 chains, each with iter = 2000; warmup = 1000; thin = 1;
##          total post-warmup samples = 120000
## 
## Population-Level Effects: 
##                                     Estimate Est.Error l-95% CI u-95% CI
## Intercept                              -0.58      1.16    -2.74     1.75
## zi_Intercept                           33.23      5.90    22.45    45.57
## ln_livestock_consumption_kg_per_pcu     0.25      0.07     0.12     0.39
## ln_livestock_pcu                       -0.24      0.05    -0.33    -0.14
## ln_migrant_pop_perc                     0.15      0.02     0.10     0.19
## ab_export_perc                          5.77      0.74     4.28     7.19
## health_expend_perc                      0.16      0.01     0.14     0.18
## human_consumption_ddd                   0.06      0.01     0.04     0.08
## ln_gdp_dollars                         -0.30      0.05    -0.40    -0.21
## zi_ln_pubs_sum                         -0.03      0.18    -0.38     0.33
## zi_ln_gdp_dollars                      -1.48      0.27    -2.04    -0.99
## zi_ln_population                        0.27      0.23    -0.17     0.72
##                                     Eff.Sample Rhat
## Intercept                                  147 1.31
## zi_Intercept                             97688 1.00
## ln_livestock_consumption_kg_per_pcu        155 1.28
## ln_livestock_pcu                           119 1.42
## ln_migrant_pop_perc                        319 1.12
## ab_export_perc                             524 1.07
## health_expend_perc                        1220 1.03
## human_consumption_ddd                      162 1.26
## ln_gdp_dollars                             124 1.40
## zi_ln_pubs_sum                          105336 1.00
## zi_ln_gdp_dollars                        95030 1.00
## zi_ln_population                        110161 1.00
## 
## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
## is a crude measure of effective sample size, and Rhat is the potential 
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

![](report_files/figure-html/diagnostics-1.png)<!-- -->![](report_files/figure-html/diagnostics-2.png)<!-- -->![](report_files/figure-html/diagnostics-3.png)<!-- -->![](report_files/figure-html/diagnostics-4.png)<!-- -->![](report_files/figure-html/diagnostics-5.png)<!-- -->

![](report_files/figure-html/plots-1.png)<!-- -->![](report_files/figure-html/plots-2.png)<!-- -->![](report_files/figure-html/plots-3.png)<!-- -->

