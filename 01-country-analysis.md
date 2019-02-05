exploratory\_analysis
================
emmamendelsohn
Tue Feb 5 07:50:59 2019

—————–View Data—————–

![](01-country-analysis_files/figure-gfm/r%20plots-1.png)<!-- -->

—————–Fit GAM—————–

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## n ~ +s(log(NY.GDP.MKTP.CD.Billion)) + continent + offset(log(SP.POP.TOTL))
    ## 
    ## Parametric coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)         16.945      9.241   1.834   0.0724 .
    ## continentAmericas   -3.501     12.503  -0.280   0.7806  
    ## continentAsia       -1.871     11.075  -0.169   0.8665  
    ## continentEurope     -4.928     10.881  -0.453   0.6525  
    ## continentOceania   -19.670     17.389  -1.131   0.2632  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                                  edf Ref.df     F  p-value    
    ## s(log(NY.GDP.MKTP.CD.Billion)) 3.781  4.661 16.85 1.18e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.589   Deviance explained = 62.7%
    ## -REML = 266.45  Scale est. = 638.91    n = 61

    ##               para s(log(NY.GDP.MKTP.CD.Billion))
    ## worst    0.8798633                     0.25583262
    ## observed 0.8798633                     0.05650447
    ## estimate 0.8798633                     0.07486232

![](01-country-analysis_files/figure-gfm/r%20mod-gam-1.png)<!-- -->

    ## 
    ## Method: REML   Optimizer: outer newton
    ## full convergence after 5 iterations.
    ## Gradient range [-2.803122e-06,1.696311e-06]
    ## (score 266.4469 & scale 638.9059).
    ## Hessian positive definite, eigenvalue range [0.9768977,27.5729].
    ## Model rank =  14 / 14 
    ## 
    ## Basis dimension (k) checking results. Low p-value (k-index<1) may
    ## indicate that k is too low, especially if edf is close to k'.
    ## 
    ##                                  k'  edf k-index p-value
    ## s(log(NY.GDP.MKTP.CD.Billion)) 9.00 3.78    1.01    0.47

—————–Fit BART—————–

    ## 
    ## Running BART with numeric y
    ## 
    ## number of trees: 200
    ## Prior:
    ##  k: 2.000000
    ##  degrees of freedom in sigma prior: 3.000000
    ##  quantile in sigma prior: 0.900000
    ##  scale in sigma prior: 0.003174
    ##  power and base for tree prior: 2.000000 0.950000
    ##  use quantiles for rule cut points: false
    ## data:
    ##  number of training observations: 61
    ##  number of test observations: 0
    ##  number of explanatory variables: 7
    ##  init sigma: 23.361497, curr sigma: 23.361497
    ## 
    ## Cutoff rules c in x<=c vs x>c
    ## Number of cutoffs: (var: number of possible c):
    ## (1: 100) (2: 100) (3: 100) (4: 100) (5: 100) 
    ## (6: 100) (7: 100) 
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
    ## total seconds in loop: 0.874078
    ## 
    ## Tree sizes, last iteration:
    ## [1] 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 3 
    ## 2 2 2 3 2 3 2 2 2 2 2 2 4 2 2 1 2 2 2 2 
    ## 2 2 2 2 3 2 2 2 2 2 2 3 2 3 2 3 2 2 2 1 
    ## 2 2 2 2 2 2 1 4 2 2 2 2 2 1 2 2 2 2 3 3 
    ## 2 2 3 2 2 2 3 3 2 2 3 2 2 2 2 2 2 3 3 3 
    ## 2 3 3 2 2 2 2 2 3 2 3 3 2 2 3 2 2 2 2 2 
    ## 2 3 1 2 2 3 3 2 3 3 3 3 2 2 2 1 2 2 1 2 
    ## 1 2 2 2 2 2 2 2 2 2 2 2 2 3 3 2 2 2 2 2 
    ## 2 2 2 2 4 2 2 2 3 1 2 2 2 2 2 3 2 2 2 4 
    ## 2 2 1 2 3 2 2 2 3 1 2 2 2 2 2 2 2 3 2 2 
    ## 2 1 
    ## 
    ## Variable Usage, last iteration (var:count):
    ## (1: 40) (2: 24) (3: 28) (4: 36) (5: 31) 
    ## (6: 37) (7: 37) 
    ## DONE BART

    ##                 Length Class         Mode   
    ## call                5  -none-        call   
    ## first.sigma       100  -none-        numeric
    ## sigma            1000  -none-        numeric
    ## sigest              1  -none-        numeric
    ## yhat.train      61000  -none-        numeric
    ## yhat.train.mean    61  -none-        numeric
    ## yhat.test           0  -none-        NULL   
    ## yhat.test.mean      0  -none-        NULL   
    ## varcount         7000  -none-        numeric
    ## y                  61  -none-        numeric
    ## fit                 1  dbartsSampler S4

—————–Compare models—————–

    ## # A tibble: 2 x 2
    ##   model mean_residuals
    ##   <chr>          <dbl>
    ## 1 bart            16.8
    ## 2 gam             17.2

    ##              n       gam     bart
    ## n    1.0000000 0.8017514 0.866129
    ## gam  0.8017514 1.0000000 0.929888
    ## bart 0.8661290 0.9298880 1.000000

![](01-country-analysis_files/figure-gfm/r%20mod-comp-1.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-comp-2.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-comp-3.png)<!-- -->
