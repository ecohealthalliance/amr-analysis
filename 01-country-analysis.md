exploratory\_analysis
================
emmamendelsohn
2019-06-25

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
    ## (Intercept)          2.3270     0.1562  14.896   <2e-16 ***
    ## english_spokenTRUE   0.3048     0.2554   1.194    0.239    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                              edf Ref.df      F  p-value    
    ## s(gdp_billion_log)         1.000  1.000  0.490 0.487466    
    ## s(population_log)          2.655  3.235  6.505 0.000709 ***
    ## s(migrant_pop_perc)        1.000  1.000  0.541 0.465965    
    ## s(health_expend_perc)      2.748  3.469  4.859 0.003692 ** 
    ## s(ab_export_perc)          1.000  1.000  1.614 0.210272    
    ## s(pubs_sum)                1.000  1.000  1.025 0.316751    
    ## s(ab_consumption_ddd)      1.599  1.980 11.348 0.000155 ***
    ## s(ag_land_perc)            3.509  4.274  3.955 0.006450 ** 
    ## s(manure_soils_kg_per_km2) 1.000  1.000  0.249 0.620216    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.908   Deviance explained = 85.3%
    ## -REML = 115.13  Scale est. = 8.3472    n = 63

    ##               para s(gdp_billion_log) s(population_log)
    ## worst    0.8646279          0.9927685         0.9939882
    ## observed 0.8646279          0.9848385         0.9750414
    ## estimate 0.8646279          0.9869881         0.9398782
    ##          s(migrant_pop_perc) s(health_expend_perc) s(ab_export_perc)
    ## worst              0.9133507             0.9999752         0.8931092
    ## observed           0.9024449             0.9381688         0.8924536
    ## estimate           0.9003194             0.9364886         0.8905195
    ##          s(pubs_sum) s(ab_consumption_ddd) s(ag_land_perc)
    ## worst      0.9999758             0.9752864       0.9647925
    ## observed   0.9935137             0.7915560       0.7208799
    ## estimate   0.9966077             0.7501272       0.7158863
    ##          s(manure_soils_kg_per_km2)
    ## worst                     0.9355783
    ## observed                  0.8413939
    ## estimate                  0.8434514

![](01-country-analysis_files/figure-gfm/r%20mod-gam-1.png)<!-- -->

    ## 
    ## Method: REML   Optimizer: outer newton
    ## full convergence after 9 iterations.
    ## Gradient range [-1.085192e-05,8.757071e-06]
    ## (score 115.127 & scale 8.347167).
    ## Hessian positive definite, eigenvalue range [4.702e-07,26.11237].
    ## Model rank =  56 / 56 
    ## 
    ## Basis dimension (k) checking results. Low p-value (k-index<1) may
    ## indicate that k is too low, especially if edf is close to k'.
    ## 
    ##                              k'  edf k-index p-value  
    ## s(gdp_billion_log)         3.00 1.00    0.80   0.060 .
    ## s(population_log)          9.00 2.66    0.92   0.370  
    ## s(migrant_pop_perc)        2.00 1.00    1.04   0.685  
    ## s(health_expend_perc)      9.00 2.75    1.01   0.640  
    ## s(ab_export_perc)          2.00 1.00    0.81   0.060 .
    ## s(pubs_sum)                9.00 1.00    0.81   0.085 .
    ## s(ab_consumption_ddd)      9.00 1.60    1.11   0.835  
    ## s(ag_land_perc)            9.00 3.51    0.92   0.335  
    ## s(manure_soils_kg_per_km2) 2.00 1.00    1.07   0.760  
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
    ##  scale in sigma prior: 0.005576
    ##  power and base for tree prior: 2.000000 0.950000
    ##  use quantiles for rule cut points: false
    ## data:
    ##  number of training observations: 63
    ##  number of test observations: 0
    ##  number of explanatory variables: 10
    ##  init sigma: 25.039913, curr sigma: 25.039913
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
    ## total seconds in loop: 1.206430
    ## 
    ## Tree sizes, last iteration:
    ## [1] 2 2 2 3 2 2 2 2 2 3 2 2 2 4 2 3 1 3 
    ## 2 3 3 2 2 2 2 3 1 1 3 2 4 2 2 2 2 2 2 2 
    ## 3 3 3 2 2 3 1 3 2 2 3 2 1 5 2 2 2 3 2 2 
    ## 2 3 2 5 2 2 3 3 2 2 3 2 2 2 2 2 3 2 3 4 
    ## 2 2 3 4 2 2 2 2 2 1 2 3 2 3 4 3 2 3 2 2 
    ## 1 2 2 3 2 2 3 2 2 2 2 2 2 2 3 3 2 2 3 4 
    ## 2 2 2 3 2 3 3 2 2 2 3 2 2 2 2 3 2 2 2 3 
    ## 2 2 2 2 2 1 3 2 2 2 1 1 2 2 2 2 2 2 2 2 
    ## 2 2 2 2 3 2 2 3 2 2 2 3 2 3 2 1 3 2 2 4 
    ## 1 2 1 2 2 2 1 2 2 3 2 2 3 3 2 2 3 2 3 2 
    ## 2 2 
    ## 
    ## Variable Usage, last iteration (var:count):
    ## (1: 18) (2: 31) (3: 32) (4: 23) (5: 29) 
    ## (6: 36) (7: 17) (8: 26) (9: 24) (10: 18) 
    ## 
    ## DONE BART

—————–Compare models—————–

    ## # A tibble: 2 x 2
    ##   model mean_residuals
    ##   <chr>          <dbl>
    ## 1 bart            9.18
    ## 2 gam             6.37

    ##              n_amr_events       gam      bart
    ## n_amr_events    1.0000000 0.9660339 0.9554191
    ## gam             0.9660339 1.0000000 0.9806920
    ## bart            0.9554191 0.9806920 1.0000000

—————–Residuals—————–

—————–ICE (Individual Conditional Expectation) Plots—————–

![](01-country-analysis_files/figure-gfm/r%20ice-1.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20ice-2.png)<!-- -->
