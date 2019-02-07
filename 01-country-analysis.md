exploratory\_analysis
================
emmamendelsohn
Thu Feb 7 12:22:19 2019

—————–View
Data—————–

![](01-country-analysis_files/figure-gfm/r%20plots-1.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20plots-2.png)<!-- -->

—————–Fit GAM—————–

    ## 
    ## Family: quasipoisson 
    ## Link function: log 
    ## 
    ## Formula:
    ## n ~ s(NY.GDP.MKTP.CD.Billion.log) + continent + s(SP.POP.TOTL.log) + 
    ##     s(pubs_sum)
    ## 
    ## Parametric coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         3.1875     0.4432   7.192 2.84e-09 ***
    ## continentAmericas   0.1220     0.5556   0.220    0.827    
    ## continentAsia       0.2400     0.4770   0.503    0.617    
    ## continentEurope     0.1235     0.5679   0.217    0.829    
    ## continentOceania   -0.7974     1.1509  -0.693    0.492    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                                 edf Ref.df     F p-value
    ## s(NY.GDP.MKTP.CD.Billion.log) 1.462  1.802 1.148   0.271
    ## s(SP.POP.TOTL.log)            1.000  1.000 1.532   0.222
    ## s(pubs_sum)                   1.000  1.001 0.176   0.677
    ## 
    ## R-sq.(adj) =  0.503   Deviance explained = 45.9%
    ## -REML = 127.02  Scale est. = 27.673    n = 59

    ##               para s(NY.GDP.MKTP.CD.Billion.log) s(SP.POP.TOTL.log)
    ## worst    0.9217435                     0.9995730          0.8112508
    ## observed 0.9217435                     0.9484228          0.7792228
    ## estimate 0.9217435                     0.8830085          0.7046101
    ##          s(pubs_sum)
    ## worst      0.9995748
    ## observed   0.9822554
    ## estimate   0.9766782

![](01-country-analysis_files/figure-gfm/r%20mod-gam-1.png)<!-- -->

    ## 
    ## Method: REML   Optimizer: outer newton
    ## full convergence after 8 iterations.
    ## Gradient range [-5.566873e-05,0.0001164018]
    ## (score 127.0163 & scale 27.67271).
    ## Hessian positive definite, eigenvalue range [3.297272e-06,25.5025].
    ## Model rank =  32 / 32 
    ## 
    ## Basis dimension (k) checking results. Low p-value (k-index<1) may
    ## indicate that k is too low, especially if edf is close to k'.
    ## 
    ##                                 k'  edf k-index p-value
    ## s(NY.GDP.MKTP.CD.Billion.log) 9.00 1.46    1.05    0.64
    ## s(SP.POP.TOTL.log)            9.00 1.00    1.05    0.73
    ## s(pubs_sum)                   9.00 1.00    0.87    0.14

—————–Fit BART—————–

    ## 
    ## Running BART with numeric y
    ## 
    ## number of trees: 200
    ## Prior:
    ##  k: 2.000000
    ##  degrees of freedom in sigma prior: 3.000000
    ##  quantile in sigma prior: 0.900000
    ##  scale in sigma prior: 0.006352
    ##  power and base for tree prior: 2.000000 0.950000
    ##  use quantiles for rule cut points: false
    ## data:
    ##  number of training observations: 59
    ##  number of test observations: 0
    ##  number of explanatory variables: 8
    ##  init sigma: 34.852854, curr sigma: 34.852854
    ## 
    ## Cutoff rules c in x<=c vs x>c
    ## Number of cutoffs: (var: number of possible c):
    ## (1: 100) (2: 100) (3: 100) (4: 100) (5: 100) 
    ## (6: 100) (7: 100) (8: 100) 
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
    ## total seconds in loop: 0.800770
    ## 
    ## Tree sizes, last iteration:
    ## [1] 3 2 1 2 2 2 1 3 2 2 2 3 3 2 2 2 2 2 
    ## 2 2 4 2 3 2 2 2 3 2 2 3 2 2 2 2 2 2 2 2 
    ## 3 2 2 4 2 2 2 3 2 2 2 2 2 4 2 3 2 2 1 3 
    ## 2 2 4 3 2 2 2 4 1 1 2 2 2 3 3 3 3 4 2 2 
    ## 2 3 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 3 2 
    ## 2 2 3 2 2 1 2 2 3 1 2 1 3 1 2 3 2 2 3 2 
    ## 2 2 2 3 1 3 2 3 2 2 3 3 2 3 2 2 2 3 2 2 
    ## 2 3 2 2 3 2 3 2 2 2 2 2 1 2 2 3 2 2 2 2 
    ## 2 2 2 3 2 2 2 2 2 2 3 3 2 2 2 2 2 2 2 1 
    ## 4 2 2 2 3 2 2 3 2 2 2 2 2 2 2 2 2 2 2 3 
    ## 2 2 
    ## 
    ## Variable Usage, last iteration (var:count):
    ## (1: 30) (2: 27) (3: 20) (4: 27) (5: 26) 
    ## (6: 38) (7: 34) (8: 41) 
    ## DONE BART

    ##                 Length Class         Mode   
    ## call                5  -none-        call   
    ## first.sigma       100  -none-        numeric
    ## sigma            1000  -none-        numeric
    ## sigest              1  -none-        numeric
    ## yhat.train      59000  -none-        numeric
    ## yhat.train.mean    59  -none-        numeric
    ## yhat.test           0  -none-        NULL   
    ## yhat.test.mean      0  -none-        NULL   
    ## varcount         8000  -none-        numeric
    ## y                  59  -none-        numeric
    ## fit                 1  dbartsSampler S4

—————–Compare models—————–

    ## # A tibble: 2 x 2
    ##   model mean_residuals
    ##   <chr>          <dbl>
    ## 1 bart            21.6
    ## 2 gam             20.7

    ##              n       gam      bart
    ## n    1.0000000 0.7541112 0.7677855
    ## gam  0.7541112 1.0000000 0.9694526
    ## bart 0.7677855 0.9694526 1.0000000

![](01-country-analysis_files/figure-gfm/r%20mod-comp-1.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-comp-2.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-comp-3.png)<!-- -->
