exploratory\_analysis
================
emmamendelsohn
Mon Feb 4 07:12:19 2019

—————–Visualize—————–

![](01-country-analysis_files/figure-gfm/r%20plots-1.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20plots-2.png)<!-- -->

—————–Fit GAM—————–

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## n ~ +s(log(NY.GDP.MKTP.CD.Billion)) + continent + s(log(SP.POP.TOTL))
    ## 
    ## Parametric coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)         32.749      9.650   3.394  0.00138 **
    ## continentAmericas    2.931     12.783   0.229  0.81959   
    ## continentAsia       -5.087     10.722  -0.474  0.63729   
    ## continentEurope     -1.691     12.343  -0.137  0.89157   
    ## continentOceania   -17.274     17.309  -0.998  0.32321   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                                  edf Ref.df     F  p-value    
    ## s(log(NY.GDP.MKTP.CD.Billion)) 3.514  4.356 6.928 0.000103 ***
    ## s(log(SP.POP.TOTL))            3.759  4.641 3.144 0.014343 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.667   Deviance explained =   73%
    ## -REML = 260.08  Scale est. = 516.98    n = 61

    ##               para s(log(NY.GDP.MKTP.CD.Billion)) s(log(SP.POP.TOTL))
    ## worst    0.9184685                      0.7531111           0.7515322
    ## observed 0.9184685                      0.6147109           0.4280242
    ## estimate 0.9184685                      0.5503579           0.5959985

![](01-country-analysis_files/figure-gfm/r%20mod-gam-1.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20mod-gam-2.png)<!-- -->

    ## 
    ## Method: REML   Optimizer: outer newton
    ## full convergence after 5 iterations.
    ## Gradient range [-2.669008e-08,5.800586e-09]
    ## (score 260.0781 & scale 516.977).
    ## Hessian positive definite, eigenvalue range [0.6854646,27.1326].
    ## Model rank =  23 / 23 
    ## 
    ## Basis dimension (k) checking results. Low p-value (k-index<1) may
    ## indicate that k is too low, especially if edf is close to k'.
    ## 
    ##                                  k'  edf k-index p-value
    ## s(log(NY.GDP.MKTP.CD.Billion)) 9.00 3.51    1.08    0.65
    ## s(log(SP.POP.TOTL))            9.00 3.76    1.11    0.76
