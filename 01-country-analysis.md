exploratory\_analysis
================
emmamendelsohn
Thu Mar 14 11:06:17 2019

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
    ## (Intercept)   3.3100     0.1472   22.49   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                         edf Ref.df     F p-value  
    ## s(wb_gdp_billion_log) 1.478  1.828 0.377  0.5889  
    ## s(wb_population_log)  1.000  1.000 4.672  0.0353 *
    ## s(wb_ag_land_perc)    1.000  1.000 0.758  0.3880  
    ## s(oec_ab_export_log)  1.000  1.000 0.627  0.4323  
    ## s(pubs_sum)           1.000  1.000 0.282  0.5978  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =   0.53   Deviance explained = 45.3%
    ## -REML = 126.22  Scale est. = 28.942    n = 57

    ##                  para s(wb_gdp_billion_log) s(wb_population_log)
    ## worst    2.856221e-21             0.9998359            0.9314973
    ## observed 2.856221e-21             0.9775111            0.8171150
    ## estimate 2.856221e-21             0.9410362            0.7886507
    ##          s(wb_ag_land_perc) s(oec_ab_export_log) s(pubs_sum)
    ## worst             0.9305826             0.971521   0.9998344
    ## observed          0.4697191             0.854018   0.9915412
    ## estimate          0.5066002             0.835728   0.9906753

![](01-country-analysis_files/figure-gfm/r%20mod-gam-1.png)<!-- -->

    ## 
    ## Method: REML   Optimizer: outer newton
    ## full convergence after 12 iterations.
    ## Gradient range [-1.536826e-05,4.236904e-05]
    ## (score 126.2167 & scale 28.94186).
    ## Hessian positive definite, eigenvalue range [2.814424e-06,25.50274].
    ## Model rank =  46 / 46 
    ## 
    ## Basis dimension (k) checking results. Low p-value (k-index<1) may
    ## indicate that k is too low, especially if edf is close to k'.
    ## 
    ##                         k'  edf k-index p-value  
    ## s(wb_gdp_billion_log) 9.00 1.48    1.14   0.850  
    ## s(wb_population_log)  9.00 1.00    1.06   0.635  
    ## s(wb_ag_land_perc)    9.00 1.00    0.83   0.085 .
    ## s(oec_ab_export_log)  9.00 1.00    0.93   0.310  
    ## s(pubs_sum)           9.00 1.00    0.88   0.150  
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
    ##  scale in sigma prior: 0.004738
    ##  power and base for tree prior: 2.000000 0.950000
    ##  use quantiles for rule cut points: false
    ## data:
    ##  number of training observations: 57
    ##  number of test observations: 0
    ##  number of explanatory variables: 26
    ##  init sigma: 30.099537, curr sigma: 30.099537
    ## 
    ## Cutoff rules c in x<=c vs x>c
    ## Number of cutoffs: (var: number of possible c):
    ## (1: 100) (2: 100) (3: 100) (4: 100) (5: 100) 
    ## (6: 100) (7: 100) (8: 100) (9: 100) (10: 100) 
    ## (11: 100) (12: 100) (13: 100) (14: 100) (15: 100) 
    ## (16: 100) (17: 100) (18: 100) (19: 100) (20: 100) 
    ## (21: 100) (22: 100) (23: 100) (24: 100) (25: 100) 
    ## (26: 100) 
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
    ## total seconds in loop: 0.855961
    ## 
    ## Tree sizes, last iteration:
    ## [1] 2 3 3 3 2 2 2 2 2 2 1 2 2 2 2 2 2 4 
    ## 2 2 2 2 2 2 2 2 2 1 3 2 2 2 2 3 1 2 2 2 
    ## 1 3 2 3 2 2 2 2 2 2 2 2 2 2 3 2 2 3 2 1 
    ## 3 2 2 2 2 2 2 3 3 3 3 3 2 2 2 2 3 3 2 2 
    ## 2 2 2 2 2 2 2 2 3 2 1 2 2 3 2 2 2 2 2 2 
    ## 2 4 2 2 2 3 2 2 2 3 2 2 2 2 2 2 2 3 3 3 
    ## 3 3 1 3 2 2 2 2 2 2 2 2 2 3 2 2 2 3 3 2 
    ## 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 3 2 2 2 1 
    ## 2 2 2 3 2 3 2 3 2 2 2 1 2 4 2 3 2 2 2 2 
    ## 2 3 2 2 2 2 2 2 2 1 3 2 2 2 2 2 2 1 2 2 
    ## 2 2 
    ## 
    ## Variable Usage, last iteration (var:count):
    ## (1: 12) (2: 12) (3: 8) (4: 11) (5: 4) 
    ## (6: 12) (7: 11) (8: 10) (9: 9) (10: 9) 
    ## (11: 6) (12: 12) (13: 10) (14: 14) (15: 9) 
    ## (16: 6) (17: 10) (18: 8) (19: 10) (20: 11) 
    ## (21: 7) (22: 7) (23: 3) (24: 5) (25: 6) 
    ## (26: 11) 
    ## DONE BART

    ##                 Length Class         Mode   
    ## call                5  -none-        call   
    ## first.sigma       100  -none-        numeric
    ## sigma            1000  -none-        numeric
    ## sigest              1  -none-        numeric
    ## yhat.train      57000  -none-        numeric
    ## yhat.train.mean    57  -none-        numeric
    ## yhat.test           0  -none-        NULL   
    ## yhat.test.mean      0  -none-        NULL   
    ## varcount        26000  -none-        numeric
    ## y                  57  -none-        numeric
    ## fit                 1  dbartsSampler S4

—————–Compare models—————–

    ## # A tibble: 2 x 2
    ##   model mean_residuals
    ##   <chr>          <dbl>
    ## 1 bart            18.3
    ## 2 gam             20.4

    ##              n_amr_events       gam      bart
    ## n_amr_events    1.0000000 0.7605294 0.8802326
    ## gam             0.7605294 1.0000000 0.8770936
    ## bart            0.8802326 0.8770936 1.0000000

![](01-country-analysis_files/figure-gfm/r%20mod-comp-1.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-comp-2.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-comp-3.png)<!-- -->
