exploratory\_analysis
================
emmamendelsohn
Tue Feb 5 18:31:14 2019

—————–View
Data—————–

![](01-country-analysis_files/figure-gfm/r%20plots-1.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20plots-2.png)<!-- -->

—————–Fit GAM—————–

    ## 
    ## Family: quasipoisson 
    ## Link function: log 
    ## 
    ## Formula:
    ## n ~ s(log(NY.GDP.MKTP.CD.Billion)) + continent + s(log(SP.POP.TOTL))
    ## 
    ## Parametric coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         2.9522     0.3819   7.729 5.24e-10 ***
    ## continentAmericas  -0.5860     0.5241  -1.118    0.269    
    ## continentAsia       0.5135     0.3703   1.387    0.172    
    ## continentEurope     0.3737     0.4623   0.808    0.423    
    ## continentOceania   -0.1469     0.6966  -0.211    0.834    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                                  edf Ref.df     F p-value  
    ## s(log(NY.GDP.MKTP.CD.Billion)) 4.421  5.377 2.562  0.0315 *
    ## s(log(SP.POP.TOTL))            1.000  1.000 0.204  0.6532  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.243   Deviance explained = 48.4%
    ## -REML = 132.79  Scale est. = 0.059738  n = 59

    ##               para s(log(NY.GDP.MKTP.CD.Billion)) s(log(SP.POP.TOTL))
    ## worst    0.9182468                      0.7565561           0.7607283
    ## observed 0.9182468                      0.4874842           0.7289115
    ## estimate 0.9182468                      0.5749283           0.6096503

![](01-country-analysis_files/figure-gfm/r%20mod-gam-1.png)<!-- -->

    ## 
    ## Method: REML   Optimizer: outer newton
    ## full convergence after 9 iterations.
    ## Gradient range [-6.36985e-05,5.565263e-05]
    ## (score 132.7913 & scale 0.0597381).
    ## Hessian positive definite, eigenvalue range [6.369498e-05,26.10989].
    ## Model rank =  23 / 23 
    ## 
    ## Basis dimension (k) checking results. Low p-value (k-index<1) may
    ## indicate that k is too low, especially if edf is close to k'.
    ## 
    ##                                  k'  edf k-index p-value
    ## s(log(NY.GDP.MKTP.CD.Billion)) 9.00 4.42    1.25    0.97
    ## s(log(SP.POP.TOTL))            9.00 1.00    1.11    0.84

—————–Fit BART—————–

    ## 
    ## Running BART with numeric y
    ## 
    ## number of trees: 200
    ## Prior:
    ##  k: 2.000000
    ##  degrees of freedom in sigma prior: 3.000000
    ##  quantile in sigma prior: 0.900000
    ##  scale in sigma prior: 0.004758
    ##  power and base for tree prior: 2.000000 0.950000
    ##  use quantiles for rule cut points: false
    ## data:
    ##  number of training observations: 59
    ##  number of test observations: 0
    ##  number of explanatory variables: 8
    ##  init sigma: 30.163861, curr sigma: 30.163861
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
    ## total seconds in loop: 0.847501
    ## 
    ## Tree sizes, last iteration:
    ## [1] 3 2 2 2 2 2 2 2 2 3 1 2 2 4 2 2 2 2 
    ## 2 3 2 2 2 2 2 2 2 2 2 2 3 3 3 2 2 2 3 2 
    ## 2 3 3 2 2 2 2 2 3 2 2 3 2 2 2 2 2 2 2 2 
    ## 3 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 3 3 4 2 
    ## 3 3 3 3 2 2 3 2 2 2 2 2 2 2 3 2 2 1 4 3 
    ## 4 2 4 3 2 2 2 2 2 2 2 3 2 3 4 2 2 3 1 1 
    ## 2 2 2 2 3 2 3 2 2 2 2 2 3 3 2 3 2 4 2 3 
    ## 3 2 2 2 2 4 2 2 1 3 2 4 2 2 2 2 2 2 2 2 
    ## 2 2 2 2 2 2 3 2 2 3 2 2 2 1 2 1 2 2 2 3 
    ## 2 3 2 2 2 3 3 3 3 2 2 3 3 3 2 2 2 2 2 4 
    ## 1 2 
    ## 
    ## Variable Usage, last iteration (var:count):
    ## (1: 42) (2: 29) (3: 35) (4: 35) (5: 30) 
    ## (6: 24) (7: 36) (8: 26) 
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
    ## 1 bart            21.4
    ## 2 gam             21.7

    ##              n       gam      bart
    ## n    1.0000000 0.6802686 0.7908896
    ## gam  0.6802686 1.0000000 0.8577997
    ## bart 0.7908896 0.8577997 1.0000000

![](01-country-analysis_files/figure-gfm/r%20mod-comp-1.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-comp-2.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-comp-3.png)<!-- -->
