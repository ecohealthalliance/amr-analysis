exploratory\_analysis
================
emmamendelsohn
2019-07-11

—————–View Data—————–

![](01-country-analysis_files/figure-gfm/r%20plots-1.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20plots-2.png)<!-- -->

—————–Fit GAM—————–

    ## 
    ## Family: quasipoisson 
    ## Link function: log 
    ## 
    ## Formula:
    ## n_amr_events ~ s(gdp_billion_log, k = 4) + s(population_log, 
    ##     k = 10) + s(migrant_pop_perc, k = 3) + s(health_expend_perc, 
    ##     k = 10) + s(ab_export_perc, k = 3) + s(pubs_sum, k = 10) + 
    ##     s(ab_consumption_ddd, k = 10) + s(ag_land_perc, k = 10) + 
    ##     s(manure_soils_kg_per_km2, k = 3) + english_spoken
    ## 
    ## Parametric coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          2.2520     0.1588  14.182   <2e-16 ***
    ## english_spokenTRUE   0.3698     0.2581   1.433    0.159    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                              edf Ref.df      F  p-value    
    ## s(gdp_billion_log)         1.000  1.000  0.401 0.529662    
    ## s(population_log)          2.515  3.069  5.965 0.001465 ** 
    ## s(migrant_pop_perc)        1.000  1.000  0.902 0.347281    
    ## s(health_expend_perc)      2.405  3.041  4.926 0.004579 ** 
    ## s(ab_export_perc)          1.000  1.000  2.074 0.156660    
    ## s(pubs_sum)                1.000  1.000  1.433 0.237416    
    ## s(ab_consumption_ddd)      1.784  2.232 10.026 0.000166 ***
    ## s(ag_land_perc)            3.349  4.088  4.071 0.006180 ** 
    ## s(manure_soils_kg_per_km2) 1.000  1.000  0.243 0.624356    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.862   Deviance explained = 81.3%
    ## -REML = 112.25  Scale est. = 8.2384    n = 62

    ##               para s(gdp_billion_log) s(population_log)
    ## worst    0.8749652          0.9913633         0.9927653
    ## observed 0.8749652          0.9839572         0.9695050
    ## estimate 0.8749652          0.9861254         0.9347583
    ##          s(migrant_pop_perc) s(health_expend_perc) s(ab_export_perc)
    ## worst              0.9102837             0.9999750         0.8960028
    ## observed           0.8979236             0.9416345         0.8951820
    ## estimate           0.8958291             0.9360546         0.8931218
    ##          s(pubs_sum) s(ab_consumption_ddd) s(ag_land_perc)
    ## worst      0.9999757             0.9734763       0.9646690
    ## observed   0.9933393             0.7914903       0.7143527
    ## estimate   0.9965503             0.7441085       0.7103518
    ##          s(manure_soils_kg_per_km2)
    ## worst                     0.9397263
    ## observed                  0.8440649
    ## estimate                  0.8461450

![](01-country-analysis_files/figure-gfm/r%20mod-gam-1.png)<!-- -->

    ## 
    ## Method: REML   Optimizer: outer newton
    ## full convergence after 11 iterations.
    ## Gradient range [-1.962972e-05,1.998476e-05]
    ## (score 112.2496 & scale 8.238359).
    ## Hessian positive definite, eigenvalue range [4.585919e-07,25.59512].
    ## Model rank =  56 / 56 
    ## 
    ## Basis dimension (k) checking results. Low p-value (k-index<1) may
    ## indicate that k is too low, especially if edf is close to k'.
    ## 
    ##                              k'  edf k-index p-value  
    ## s(gdp_billion_log)         3.00 1.00    0.82    0.09 .
    ## s(population_log)          9.00 2.52    0.91    0.30  
    ## s(migrant_pop_perc)        2.00 1.00    1.15    0.94  
    ## s(health_expend_perc)      9.00 2.40    1.00    0.56  
    ## s(ab_export_perc)          2.00 1.00    0.83    0.12  
    ## s(pubs_sum)                9.00 1.00    0.82    0.10  
    ## s(ab_consumption_ddd)      9.00 1.78    1.14    0.95  
    ## s(ag_land_perc)            9.00 3.35    0.93    0.33  
    ## s(manure_soils_kg_per_km2) 2.00 1.00    1.07    0.81  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

![](01-country-analysis_files/figure-gfm/r%20mod-gam-2.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-gam-3.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-gam-4.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-gam-5.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-gam-6.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-gam-7.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-gam-8.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-gam-9.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-gam-10.png)<!-- -->

—————–Fit BART—————–

    ## 
    ## Running BART with numeric y
    ## 
    ## number of trees: 200
    ## number of chains: 1, number of threads 1
    ## Prior:
    ##  k: 2.000000
    ##  degrees of freedom in sigma prior: 3.000000
    ##  quantile in sigma prior: 0.900000
    ##  scale in sigma prior: 0.005321
    ##  power and base for tree prior: 2.000000 0.950000
    ##  use quantiles for rule cut points: false
    ## data:
    ##  number of training observations: 62
    ##  number of test observations: 0
    ##  number of explanatory variables: 10
    ##  init sigma: 20.495039, curr sigma: 20.495039
    ## 
    ## Cutoff rules c in x<=c vs x>c
    ## Number of cutoffs: (var: number of possible c):
    ## (1: 100) (2: 100) (3: 100) (4: 100) (5: 100) 
    ## (6: 100) (7: 100) (8: 100) (9: 100) (10: 100) 
    ## 
    ## Running mcmc loop:
    ## iteration: 100 (of 1000)
    ## iteration: 200 (of 1000)
    ## iteration: 300 (of 1000)
    ## iteration: 400 (of 1000)
    ## iteration: 500 (of 1000)
    ## iteration: 600 (of 1000)
    ## iteration: 700 (of 1000)
    ## iteration: 800 (of 1000)
    ## iteration: 900 (of 1000)
    ## iteration: 1000 (of 1000)
    ## total seconds in loop: 1.148766
    ## 
    ## Tree sizes, last iteration:
    ## [1] 2 2 3 2 2 2 2 4 3 3 2 2 2 3 2 3 3 5 
    ## 1 2 3 2 2 2 2 3 2 3 2 2 2 3 2 2 3 3 2 2 
    ## 2 2 2 3 2 2 2 2 1 2 3 2 2 2 4 2 2 3 5 2 
    ## 4 3 2 2 3 2 2 2 2 2 2 2 2 2 3 3 2 2 2 2 
    ## 3 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 3 2 
    ## 2 2 3 2 2 2 2 2 3 2 2 1 1 2 2 3 2 3 3 2 
    ## 3 3 3 3 3 3 2 2 2 1 2 2 2 2 2 3 2 2 3 2 
    ## 2 2 3 1 2 2 2 3 3 3 2 4 2 2 3 3 2 2 2 2 
    ## 3 1 3 3 2 3 3 2 2 3 3 2 2 2 2 2 2 3 2 3 
    ## 2 3 2 1 2 2 2 3 2 2 3 2 2 2 1 3 3 2 3 3 
    ## 2 3 
    ## 
    ## Variable Usage, last iteration (var:count):
    ## (1: 23) (2: 30) (3: 33) (4: 25) (5: 25) 
    ## (6: 28) (7: 27) (8: 29) (9: 24) (10: 19) 
    ## 
    ## DONE BART

—————–Compare models—————–

    ## # A tibble: 2 x 2
    ##   model mean_residuals
    ##   <chr>          <dbl>
    ## 1 bart            9.61
    ## 2 gam             6.23

    ##              n_amr_events       gam      bart
    ## n_amr_events    1.0000000 0.9485646 0.9183702
    ## gam             0.9485646 1.0000000 0.9625171
    ## bart            0.9183702 0.9625171 1.0000000

—————–Residuals—————–

—————–ICE (Individual Conditional Expectation) Plots—————–

![](01-country-analysis_files/figure-gfm/r%20ice-1.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20ice-2.png)<!-- -->
