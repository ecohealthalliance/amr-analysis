exploratory\_analysis
================
emmamendelsohn
2019-03-22

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
    ##     s(wb_ag_land_perc, k = 15) + s(oec_ab_export_log) + s(pubs_sum) + 
    ##     english_spoken
    ## 
    ## Parametric coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          1.2569     0.3166   3.970 0.000114 ***
    ## english_spokenTRUE   0.1081     0.3364   0.321 0.748333    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                         edf Ref.df     F p-value   
    ## s(wb_gdp_billion_log) 1.000  1.000 8.113 0.00503 **
    ## s(wb_population_log)  1.000  1.000 2.465 0.11863   
    ## s(wb_ag_land_perc)    1.000  1.000 0.335 0.56349   
    ## s(oec_ab_export_log)  1.000  1.000 0.924 0.33802   
    ## s(pubs_sum)           1.819  2.109 0.812 0.44974   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =   0.66   Deviance explained = 64.3%
    ## -REML = 287.88  Scale est. = 31.813    n = 150

    ##               para s(wb_gdp_billion_log) s(wb_population_log)
    ## worst    0.3744737             0.9835878            0.8157457
    ## observed 0.3744737             0.9234531            0.8094742
    ## estimate 0.3744737             0.8516632            0.7273815
    ##          s(wb_ag_land_perc) s(oec_ab_export_log) s(pubs_sum)
    ## worst             0.5468113            0.9132171   0.9853237
    ## observed          0.3822698            0.7029563   0.9327204
    ## estimate          0.3659928            0.6481939   0.9225047

![](01-country-analysis_files/figure-gfm/r%20mod-gam-1.png)<!-- -->

    ## 
    ## Method: REML   Optimizer: outer newton
    ## full convergence after 11 iterations.
    ## Gradient range [-5.503191e-05,3.253661e-05]
    ## (score 287.8797 & scale 31.8128).
    ## Hessian positive definite, eigenvalue range [1.040834e-06,71.50209].
    ## Model rank =  52 / 52 
    ## 
    ## Basis dimension (k) checking results. Low p-value (k-index<1) may
    ## indicate that k is too low, especially if edf is close to k'.
    ## 
    ##                          k'   edf k-index p-value    
    ## s(wb_gdp_billion_log)  9.00  1.00    0.93    0.43    
    ## s(wb_population_log)   9.00  1.00    1.06    0.97    
    ## s(wb_ag_land_perc)    14.00  1.00    0.69  <2e-16 ***
    ## s(oec_ab_export_log)   9.00  1.00    0.85    0.12    
    ## s(pubs_sum)            9.00  1.82    0.87    0.20    
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
    ##  scale in sigma prior: 0.002374
    ##  power and base for tree prior: 2.000000 0.950000
    ##  use quantiles for rule cut points: false
    ## data:
    ##  number of training observations: 150
    ##  number of test observations: 0
    ##  number of explanatory variables: 29
    ##  init sigma: 21.416361, curr sigma: 21.416361
    ## 
    ## Cutoff rules c in x<=c vs x>c
    ## Number of cutoffs: (var: number of possible c):
    ## (1: 100) (2: 100) (3: 100) (4: 100) (5: 100) 
    ## (6: 100) (7: 100) (8: 100) (9: 100) (10: 100) 
    ## (11: 100) (12: 100) (13: 100) (14: 100) (15: 100) 
    ## (16: 100) (17: 100) (18: 100) (19: 100) (20: 100) 
    ## (21: 100) (22: 100) (23: 100) (24: 100) (25: 100) 
    ## (26: 100) (27: 100) (28: 100) (29: 100) 
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
    ## total seconds in loop: 1.176679
    ## 
    ## Tree sizes, last iteration:
    ## [1] 4 2 2 2 2 2 2 2 1 2 3 3 2 2 3 2 2 2 
    ## 2 2 2 1 3 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 
    ## 2 1 2 2 2 1 1 2 2 1 2 2 2 3 4 2 2 2 2 3 
    ## 2 2 2 2 2 2 2 2 2 3 1 3 2 2 1 2 2 4 2 2 
    ## 1 2 3 2 2 1 2 2 2 2 2 2 2 2 2 1 2 2 3 2 
    ## 2 3 2 3 2 2 3 2 4 2 3 3 2 2 2 2 3 2 2 4 
    ## 2 2 3 2 2 2 3 2 2 3 2 2 2 2 2 1 2 4 2 2 
    ## 3 2 3 2 2 2 4 2 1 2 2 3 2 2 3 3 1 4 3 3 
    ## 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 
    ## 2 2 1 3 2 1 2 2 2 2 1 2 3 3 3 2 2 2 3 2 
    ## 2 2 
    ## 
    ## Variable Usage, last iteration (var:count):
    ## (1: 11) (2: 7) (3: 1) (4: 10) (5: 13) 
    ## (6: 15) (7: 11) (8: 8) (9: 3) (10: 10) 
    ## (11: 8) (12: 2) (13: 9) (14: 7) (15: 6) 
    ## (16: 5) (17: 10) (18: 4) (19: 4) (20: 13) 
    ## (21: 9) (22: 11) (23: 3) (24: 6) (25: 4) 
    ## (26: 11) (27: 13) (28: 7) (29: 7) 
    ## DONE BART

—————–Compare models—————–

    ## # A tibble: 2 x 2
    ##   model mean_residuals
    ##   <chr>          <dbl>
    ## 1 bart            8.27
    ## 2 gam             9.28

    ##              n_amr_events       gam      bart
    ## n_amr_events    1.0000000 0.8219701 0.9091044
    ## gam             0.8219701 1.0000000 0.9286271
    ## bart            0.9091044 0.9286271 1.0000000

—————–Residuals—————–

![](01-country-analysis_files/figure-gfm/r%20resids-1.png)<!-- -->

—————–ICE (Individual Conditional Expectation)
Plots—————–

![](01-country-analysis_files/figure-gfm/r%20ice-1.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20ice-2.png)<!-- -->
