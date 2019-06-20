exploratory\_analysis
================
emmamendelsohn
2019-06-20

—————–View Data—————–

![](01-country-analysis_files/figure-gfm/r%20plots-1.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20plots-2.png)<!-- -->

—————–Fit GAM—————–

    ## 
    ## Family: quasipoisson 
    ## Link function: log 
    ## 
    ## Formula:
    ## n_amr_events ~ s(wb_gdp_billion_log) + s(wb_population_log) + 
    ##     s(wb_migrant_pop_perc) + s(oec_ab_export_log) + s(pubs_sum, 
    ##     k = 15) + s(im_ab_consumption)
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    2.538      0.164   15.48   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                          edf Ref.df     F p-value   
    ## s(wb_gdp_billion_log)  1.000  1.000 8.973 0.00414 **
    ## s(wb_population_log)   2.429  2.985 3.191 0.02741 * 
    ## s(wb_migrant_pop_perc) 1.000  1.000 2.156 0.14798   
    ## s(oec_ab_export_log)   1.000  1.000 0.006 0.93792   
    ## s(pubs_sum)            1.000  1.000 1.510 0.22461   
    ## s(im_ab_consumption)   2.457  3.112 4.796 0.00440 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.771   Deviance explained = 71.3%
    ## -REML = 118.44  Scale est. = 15.071    n = 62

    ##                  para s(wb_gdp_billion_log) s(wb_population_log)
    ## worst    6.096106e-19             0.9999979            0.9926887
    ## observed 6.096106e-19             0.9896498            0.9309583
    ## estimate 6.096106e-19             0.9801104            0.9463854
    ##          s(wb_migrant_pop_perc) s(oec_ab_export_log) s(pubs_sum)
    ## worst                 0.9906507            0.9978626   0.9999979
    ## observed              0.8966376            0.9554636   0.9967357
    ## estimate              0.8822877            0.9134950   0.9958611
    ##          s(im_ab_consumption)
    ## worst               0.9906867
    ## observed            0.8471317
    ## estimate            0.8376967

![](01-country-analysis_files/figure-gfm/r%20mod-gam-1.png)<!-- -->

    ## 
    ## Method: REML   Optimizer: outer newton
    ## full convergence after 11 iterations.
    ## Gradient range [-1.930995e-05,1.581121e-05]
    ## (score 118.4421 & scale 15.07093).
    ## Hessian positive definite, eigenvalue range [3.450205e-06,27.53492].
    ## Model rank =  60 / 60 
    ## 
    ## Basis dimension (k) checking results. Low p-value (k-index<1) may
    ## indicate that k is too low, especially if edf is close to k'.
    ## 
    ##                           k'   edf k-index p-value    
    ## s(wb_gdp_billion_log)   9.00  1.00    0.82    0.16    
    ## s(wb_population_log)    9.00  2.43    0.85    0.14    
    ## s(wb_migrant_pop_perc)  9.00  1.00    1.01    0.65    
    ## s(oec_ab_export_log)    9.00  1.00    0.99    0.57    
    ## s(pubs_sum)            14.00  1.00    0.72  <2e-16 ***
    ## s(im_ab_consumption)    9.00  2.46    0.93    0.27    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

![](01-country-analysis_files/figure-gfm/r%20mod-gam-2.png)<!-- -->

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
    ##  scale in sigma prior: 0.004052
    ##  power and base for tree prior: 2.000000 0.950000
    ##  use quantiles for rule cut points: false
    ## data:
    ##  number of training observations: 62
    ##  number of test observations: 0
    ##  number of explanatory variables: 27
    ##  init sigma: 21.344568, curr sigma: 21.344568
    ## 
    ## Cutoff rules c in x<=c vs x>c
    ## Number of cutoffs: (var: number of possible c):
    ## (1: 100) (2: 100) (3: 100) (4: 100) (5: 100) 
    ## (6: 100) (7: 100) (8: 100) (9: 100) (10: 100) 
    ## (11: 100) (12: 100) (13: 100) (14: 100) (15: 100) 
    ## (16: 100) (17: 100) (18: 100) (19: 100) (20: 100) 
    ## (21: 100) (22: 100) (23: 100) (24: 100) (25: 100) 
    ## (26: 100) (27: 100) 
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
    ## total seconds in loop: 1.479496
    ## 
    ## Tree sizes, last iteration:
    ## [1] 3 1 1 3 4 1 3 2 3 2 3 1 2 2 3 2 2 2 
    ## 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 3 2 
    ## 2 2 3 2 1 2 2 2 2 2 2 1 3 2 2 3 1 2 1 2 
    ## 2 2 3 3 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 
    ## 3 2 2 2 2 3 2 2 2 2 4 2 2 3 2 3 3 2 2 2 
    ## 3 2 2 2 2 2 2 2 1 4 2 3 3 3 2 3 2 2 3 2 
    ## 2 3 2 2 2 3 3 2 1 2 3 2 2 1 2 2 2 3 2 2 
    ## 4 2 2 2 2 2 1 2 2 2 2 2 2 3 3 3 2 2 2 2 
    ## 1 2 2 2 3 2 2 2 2 2 2 2 2 2 2 3 4 2 2 2 
    ## 3 2 3 3 2 2 3 1 2 3 2 2 2 3 3 2 1 2 2 2 
    ## 1 2 
    ## 
    ## Variable Usage, last iteration (var:count):
    ## (1: 8) (2: 7) (3: 10) (4: 9) (5: 11) 
    ## (6: 6) (7: 12) (8: 13) (9: 12) (10: 5) 
    ## (11: 10) (12: 10) (13: 9) (14: 11) (15: 5) 
    ## (16: 7) (17: 5) (18: 9) (19: 10) (20: 4) 
    ## (21: 7) (22: 11) (23: 13) (24: 11) (25: 5) 
    ## (26: 7) (27: 10) 
    ## DONE BART

—————–Compare models—————–

    ## # A tibble: 2 x 2
    ##   model mean_residuals
    ##   <chr>          <dbl>
    ## 1 bart            10.1
    ## 2 gam             10.1

    ##              n_amr_events       gam      bart
    ## n_amr_events    1.0000000 0.8969653 0.9381280
    ## gam             0.8969653 1.0000000 0.9242313
    ## bart            0.9381280 0.9242313 1.0000000

—————–Residuals—————–

![](01-country-analysis_files/figure-gfm/r%20resids-1.png)<!-- -->

—————–ICE (Individual Conditional Expectation) Plots—————–

![](01-country-analysis_files/figure-gfm/r%20ice-1.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20ice-2.png)<!-- -->
