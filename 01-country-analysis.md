exploratory\_analysis
================
emmamendelsohn
2019-06-25

—————–View Data—————–

![](01-country-analysis_files/figure-gfm/r%20plots-1.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20plots-2.png)<!-- -->

—————–Fit GAM—————– using ziP family- for zero inflated Poisson data,
when the zero inflation rate depends simply on the Poisson mean. From
mgcv: This sort of model is really only appropriate when none of your
covariates help to explain the zeroes in your data. If your covariates
predict which observations are likely to have zero mean then adding a
zero inflated model on top of this is likely to lead to identifiability
problems. Identifiability problems may lead to fit failures, or absurd
values for the linear predictor or predicted values.

    ## [1] "full convergence"

    ## 
    ## Family: Zero inflated Poisson(-1.016,0.615) 
    ## Link function: identity 
    ## 
    ## Formula:
    ## n_amr_events ~ s(gdp_billion_log) + s(population_log) + s(pubs_sum) + 
    ##     s(migrant_pop_perc, k = 5) + s(health_expend_perc) + s(ab_export_perc, 
    ##     k = 5) + s(ab_consumption_ddd) + s(ag_land_perc, k = 5) + 
    ##     s(manure_soils_kg_per_km2, k = 5) + english_spoken
    ## 
    ## Parametric coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         2.41457    0.07606  31.746   <2e-16 ***
    ## english_spokenTRUE  0.12162    0.16610   0.732    0.464    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                              edf Ref.df Chi.sq  p-value    
    ## s(gdp_billion_log)         1.000  1.000  6.005   0.0143 *  
    ## s(population_log)          6.998  7.907 86.316 2.82e-15 ***
    ## s(pubs_sum)                5.437  6.100 28.194 9.49e-05 ***
    ## s(migrant_pop_perc)        1.000  1.000  0.081   0.7765    
    ## s(health_expend_perc)      6.972  7.633 81.913 1.10e-14 ***
    ## s(ab_export_perc)          1.000  1.000  1.116   0.2907    
    ## s(ab_consumption_ddd)      2.219  2.717 31.400 2.38e-05 ***
    ## s(ag_land_perc)            3.813  3.951 35.367 2.09e-07 ***
    ## s(manure_soils_kg_per_km2) 3.885  3.969 39.231 5.70e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Deviance explained = 99.3%
    ## -REML = 229.43  Scale est. = 1         n = 63

    ##               para s(gdp_billion_log) s(population_log) s(pubs_sum)
    ## worst    0.9999543          1.0000000         0.9999999   1.0000000
    ## observed 0.9999543          0.9895073         0.9669500   0.9948870
    ## estimate 0.9999543          0.9865408         0.9791193   0.9990749
    ##          s(migrant_pop_perc) s(health_expend_perc) s(ab_export_perc)
    ## worst              0.9999996             1.0000000         0.9999997
    ## observed           0.9537991             0.9658247         0.9415446
    ## estimate           0.9382690             0.9645454         0.9440537
    ##          s(ab_consumption_ddd) s(ag_land_perc) s(manure_soils_kg_per_km2)
    ## worst                0.9999994       0.9999944                  0.9999986
    ## observed             0.8708048       0.9168492                  0.9278401
    ## estimate             0.8944577       0.9834974                  0.9664509

![](01-country-analysis_files/figure-gfm/r%20mod-gam-1.png)<!-- -->

    ## 
    ## Method: REML   Optimizer: outer newton
    ## full convergence after 16 iterations.
    ## Gradient range [-7.595193e-05,2.043814e-05]
    ## (score 229.4294 & scale 1).
    ## Hessian positive definite, eigenvalue range [1.598647e-06,73.585].
    ## Model rank =  63 / 63 
    ## 
    ## Basis dimension (k) checking results. Low p-value (k-index<1) may
    ## indicate that k is too low, especially if edf is close to k'.
    ## 
    ##                              k'  edf k-index p-value   
    ## s(gdp_billion_log)         9.00 1.00    0.72   0.030 * 
    ## s(population_log)          9.00 7.00    1.14   0.930   
    ## s(pubs_sum)                9.00 5.44    0.65   0.005 **
    ## s(migrant_pop_perc)        4.00 1.00    1.09   0.860   
    ## s(health_expend_perc)      9.00 6.97    0.86   0.215   
    ## s(ab_export_perc)          4.00 1.00    1.06   0.800   
    ## s(ab_consumption_ddd)      9.00 2.22    1.22   1.000   
    ## s(ag_land_perc)            4.00 3.81    0.80   0.095 . 
    ## s(manure_soils_kg_per_km2) 4.00 3.89    0.88   0.255   
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
    ## total seconds in loop: 0.893443
    ## 
    ## Tree sizes, last iteration:
    ## [1] 2 2 2 2 2 2 2 1 2 3 2 2 2 3 2 3 2 2 
    ## 2 2 4 2 2 3 2 1 3 2 2 2 2 3 2 2 2 2 3 2 
    ## 2 2 2 2 3 3 2 2 3 2 2 3 2 2 3 2 2 2 3 2 
    ## 2 2 3 2 2 2 3 2 2 3 2 2 2 2 3 3 2 3 2 2 
    ## 4 2 2 5 3 2 2 2 2 3 2 2 2 3 2 2 2 2 2 2 
    ## 2 2 3 4 2 2 2 1 3 2 2 2 2 2 2 2 3 2 4 4 
    ## 1 2 2 2 2 2 2 2 2 2 2 2 3 3 2 1 2 2 3 2 
    ## 2 3 2 3 2 4 2 2 3 3 2 2 2 2 2 2 2 2 3 2 
    ## 3 3 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 3 3 
    ## 2 2 2 4 2 2 2 2 2 2 2 2 2 2 3 3 2 2 2 2 
    ## 2 1 
    ## 
    ## Variable Usage, last iteration (var:count):
    ## (1: 24) (2: 27) (3: 26) (4: 30) (5: 18) 
    ## (6: 30) (7: 28) (8: 20) (9: 22) (10: 24) 
    ## 
    ## DONE BART

—————–Compare models—————–

—————–Residuals—————–

—————–ICE (Individual Conditional Expectation) Plots—————–

![](01-country-analysis_files/figure-gfm/r%20ice-1.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20ice-2.png)<!-- -->
