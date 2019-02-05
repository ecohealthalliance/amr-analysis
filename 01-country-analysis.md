exploratory\_analysis
================
emmamendelsohn
Tue Feb 5 14:59:10 2019

—————–View Data—————–

![](01-country-analysis_files/figure-gfm/r%20plots-1.png)<!-- -->

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
    ## (Intercept)         3.1546     0.4348   7.255 1.71e-09 ***
    ## continentAmericas   0.1848     0.5374   0.344    0.732    
    ## continentAsia       0.1848     0.4622   0.400    0.691    
    ## continentEurope     0.1495     0.5439   0.275    0.784    
    ## continentOceania   -0.7720     1.1350  -0.680    0.499    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                                 edf Ref.df     F p-value  
    ## s(log(NY.GDP.MKTP.CD.Billion)) 1.73  2.157 2.580   0.079 .
    ## s(log(SP.POP.TOTL))            1.00  1.000 2.122   0.151  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.521   Deviance explained = 46.5%
    ## -REML =  129.6  Scale est. = 27.163    n = 61

    ##               para s(log(NY.GDP.MKTP.CD.Billion)) s(log(SP.POP.TOTL))
    ## worst    0.9184685                      0.7531111           0.7515322
    ## observed 0.9184685                      0.6850108           0.7064940
    ## estimate 0.9184685                      0.5503579           0.5959985

—————–Fit BART—————–

    ## 
    ## Running BART with numeric y
    ## 
    ## number of trees: 200
    ## Prior:
    ##  k: 2.000000
    ##  degrees of freedom in sigma prior: 3.000000
    ##  quantile in sigma prior: 0.900000
    ##  scale in sigma prior: 0.004605
    ##  power and base for tree prior: 2.000000 0.950000
    ##  use quantiles for rule cut points: false
    ## data:
    ##  number of training observations: 61
    ##  number of test observations: 0
    ##  number of explanatory variables: 7
    ##  init sigma: 29.673385, curr sigma: 29.673385
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
    ## total seconds in loop: 0.827340
    ## 
    ## Tree sizes, last iteration:
    ## [1] 2 2 3 1 2 2 2 3 3 2 2 2 2 2 2 2 2 3 
    ## 2 2 2 2 2 2 2 2 2 2 2 2 3 4 2 3 2 2 2 2 
    ## 3 2 2 3 2 2 2 2 3 2 2 2 2 2 2 2 1 1 2 1 
    ## 2 2 2 3 3 2 1 2 3 2 2 2 2 2 3 2 2 3 1 2 
    ## 3 2 2 3 2 3 2 2 3 2 2 2 2 2 3 2 1 2 2 2 
    ## 3 2 2 2 2 2 3 2 3 2 2 4 2 2 2 2 2 1 3 2 
    ## 3 2 2 2 2 2 2 2 4 2 3 2 3 2 2 2 3 1 2 3 
    ## 3 3 3 2 1 2 2 2 2 2 2 1 2 2 2 2 2 1 2 2 
    ## 2 2 2 2 2 2 2 2 2 2 2 3 3 2 2 2 3 2 2 2 
    ## 2 3 2 2 2 2 3 2 2 2 2 2 3 2 2 2 2 2 2 2 
    ## 2 4 
    ## 
    ## Variable Usage, last iteration (var:count):
    ## (1: 46) (2: 33) (3: 29) (4: 27) (5: 25) 
    ## (6: 35) (7: 38) 
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
    ## 1 bart            21.4
    ## 2 gam             20.1

    ##              n       gam      bart
    ## n    1.0000000 0.7597727 0.7818966
    ## gam  0.7597727 1.0000000 0.9773458
    ## bart 0.7818966 0.9773458 1.0000000

![](01-country-analysis_files/figure-gfm/r%20mod-comp-1.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-comp-2.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-comp-3.png)<!-- -->
