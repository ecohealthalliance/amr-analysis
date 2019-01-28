exploratory\_analysis
================
emmamendelsohn
Mon Jan 28 14:47:07 2019

—————–Visualize—————–

    ## # A tibble: 61 x 6
    ##     date country       SP.POP.TOTL     n continent NY.GDP.MKTP.CD.Billion
    ##    <dbl> <chr>               <dbl> <dbl> <chr>                      <dbl>
    ##  1  2015 china          1371220000   184 Asia                      11065.
    ##  2  2015 united states   321039839   167 Americas                  18121.
    ##  3  2015 india          1309053980   137 Asia                       2102.
    ##  4  2015 spain            46444832   108 Europe                     1198.
    ##  5  2015 canada           35832513   101 Americas                   1560.
    ##  6  2015 japan           127141000    88 Asia                       4395.
    ##  7  2015 france           66593366    83 Europe                     2438.
    ##  8  2015 brazil          205962108    60 Americas                   1802.
    ##  9  2015 south korea      51014947    54 Asia                       1383.
    ## 10  2015 italy            60730582    51 Europe                     1833.
    ## # … with 51 more rows

![](01-country-analysis_files/figure-gfm/r%20plots-1.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20plots-2.png)<!-- -->![](01-country-analysis_files/figure-gfm/r%20plots-3.png)<!-- -->

—————–Fit quasipoisson—————–

    ## # A tibble: 4 x 5
    ##   term                                estimate std.error statistic  p.value
    ##   <chr>                                  <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)                         2.97e+ 0  1.46e- 1     20.3  9.00e-28
    ## 2 NY.GDP.MKTP.CD.Billion              1.28e- 4  2.95e- 5      4.35 5.75e- 5
    ## 3 SP.POP.TOTL                         1.48e- 9  4.13e-10      3.59 6.84e- 4
    ## 4 NY.GDP.MKTP.CD.Billion:SP.POP.TOTL -8.34e-14  5.73e-14     -1.46 1.50e- 1

    ## # A tibble: 1 x 7
    ##   null.deviance df.null logLik   AIC   BIC deviance df.residual
    ##           <dbl>   <int>  <dbl> <dbl> <dbl>    <dbl>       <int>
    ## 1         2281.      60     NA    NA    NA    1187.          57
