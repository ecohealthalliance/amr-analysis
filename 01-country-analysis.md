exploratory\_analysis
================
emmamendelsohn
Mon Mar 18 08:38:24 2019

—————–View
Data—————–

![](01-country-analysis_files/figure-gfm/r%20plots-1.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20plots-2.png)<!-- -->

—————–Fit GAM—————–

    ## 
    ## Family: quasipoisson 
    ## Link function: log 
    ## 
    ## Formula:
    ## n_amr_events ~ s(wb_gdp_billion_log) + s(wb_population_log) + 
    ##     s(wb_ag_land_perc) + s(oec_ab_export_log) + s(pubs_sum)
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   1.3811     0.2829   4.881 2.71e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                         edf Ref.df     F p-value  
    ## s(wb_gdp_billion_log) 1.000  1.000 6.561  0.0114 *
    ## s(wb_population_log)  1.000  1.000 2.355  0.1270  
    ## s(wb_ag_land_perc)    1.000  1.000 0.151  0.6982  
    ## s(oec_ab_export_log)  1.000  1.000 1.518  0.2198  
    ## s(pubs_sum)           1.716  2.018 0.508  0.6037  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =   0.65   Deviance explained = 62.1%
    ## -REML = 298.49  Scale est. = 32.59     n = 154

    ##                  para s(wb_gdp_billion_log) s(wb_population_log)
    ## worst    6.171808e-22             0.9820172            0.7928571
    ## observed 6.171808e-22             0.9110801            0.7864109
    ## estimate 6.171808e-22             0.8307339            0.6932861
    ##          s(wb_ag_land_perc) s(oec_ab_export_log) s(pubs_sum)
    ## worst             0.4963853            0.9067014   0.9837666
    ## observed          0.3496542            0.6845594   0.9264111
    ## estimate          0.3410067            0.6222398   0.9161905

![](01-country-analysis_files/figure-gfm/r%20mod-gam-1.png)<!-- -->

    ## 
    ## Method: REML   Optimizer: outer newton
    ## full convergence after 9 iterations.
    ## Gradient range [-7.084472e-05,3.712677e-05]
    ## (score 298.4862 & scale 32.59028).
    ## Hessian positive definite, eigenvalue range [6.554219e-06,74.00157].
    ## Model rank =  46 / 46 
    ## 
    ## Basis dimension (k) checking results. Low p-value (k-index<1) may
    ## indicate that k is too low, especially if edf is close to k'.
    ## 
    ##                         k'  edf k-index p-value   
    ## s(wb_gdp_billion_log) 9.00 1.00    0.96   0.630   
    ## s(wb_population_log)  9.00 1.00    1.00   0.795   
    ## s(wb_ag_land_perc)    9.00 1.00    0.73   0.005 **
    ## s(oec_ab_export_log)  9.00 1.00    0.84   0.070 . 
    ## s(pubs_sum)           9.00 1.72    0.88   0.185   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

—————–Fit BART—————–

    ## 
    ## Running BART with numeric y
    ## 
    ## number of trees: 200
    ## Prior:
    ##  k: 2.000000
    ##  degrees of freedom in sigma prior: 3.000000
    ##  quantile in sigma prior: 0.900000
    ##  scale in sigma prior: 0.002294
    ##  power and base for tree prior: 2.000000 0.950000
    ##  use quantiles for rule cut points: false
    ## data:
    ##  number of training observations: 154
    ##  number of test observations: 0
    ##  number of explanatory variables: 28
    ##  init sigma: 21.053375, curr sigma: 21.053375
    ## 
    ## Cutoff rules c in x<=c vs x>c
    ## Number of cutoffs: (var: number of possible c):
    ## (1: 100) (2: 100) (3: 100) (4: 100) (5: 100) 
    ## (6: 100) (7: 100) (8: 100) (9: 100) (10: 100) 
    ## (11: 100) (12: 100) (13: 100) (14: 100) (15: 100) 
    ## (16: 100) (17: 100) (18: 100) (19: 100) (20: 100) 
    ## (21: 100) (22: 100) (23: 100) (24: 100) (25: 100) 
    ## (26: 100) (27: 100) (28: 100) 
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
    ## total seconds in loop: 1.111204
    ## 
    ## Tree sizes, last iteration:
    ## [1] 2 2 2 2 2 1 2 2 2 3 2 2 2 2 3 2 2 3 
    ## 3 3 3 2 2 3 2 3 2 2 2 1 2 2 2 4 1 2 2 2 
    ## 2 2 2 2 2 2 1 2 2 4 2 2 2 2 2 2 2 1 2 2 
    ## 2 2 2 2 3 2 2 2 2 2 3 2 2 1 2 2 4 2 2 2 
    ## 2 2 3 2 2 2 2 2 3 2 2 2 2 3 2 2 2 2 2 1 
    ## 1 5 3 1 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 
    ## 3 2 2 2 2 2 2 2 5 3 2 2 2 2 2 2 2 2 2 2 
    ## 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 3 2 3 2 2 
    ## 2 2 2 1 1 2 2 4 2 2 2 2 2 2 3 3 2 3 2 3 
    ## 2 2 1 2 2 2 3 1 3 2 2 2 4 2 2 2 2 3 2 2 
    ## 2 3 
    ## 
    ## Variable Usage, last iteration (var:count):
    ## (1: 8) (2: 12) (3: 11) (4: 5) (5: 10) 
    ## (6: 8) (7: 9) (8: 15) (9: 5) (10: 10) 
    ## (11: 4) (12: 8) (13: 8) (14: 10) (15: 7) 
    ## (16: 7) (17: 7) (18: 8) (19: 8) (20: 7) 
    ## (21: 10) (22: 13) (23: 8) (24: 8) (25: 7) 
    ## (26: 5) (27: 4) (28: 9) 
    ## DONE BART

    ##                 Length Class         Mode   
    ## call                 5 -none-        call   
    ## first.sigma        100 -none-        numeric
    ## sigma             1000 -none-        numeric
    ## sigest               1 -none-        numeric
    ## yhat.train      154000 -none-        numeric
    ## yhat.train.mean    154 -none-        numeric
    ## yhat.test            0 -none-        NULL   
    ## yhat.test.mean       0 -none-        NULL   
    ## varcount         28000 -none-        numeric
    ## y                  154 -none-        numeric
    ## fit                  1 dbartsSampler S4

—————–Compare models—————–

    ## # A tibble: 2 x 2
    ##   model mean_residuals
    ##   <chr>          <dbl>
    ## 1 bart            8.24
    ## 2 gam             9.47

    ##              n_amr_events       gam      bart
    ## n_amr_events    1.0000000 0.8142246 0.9073768
    ## gam             0.8142246 1.0000000 0.9216600
    ## bart            0.9073768 0.9216600 1.0000000

![](01-country-analysis_files/figure-gfm/r%20mod-comp-1.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-comp-2.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-comp-3.png)<!-- -->
