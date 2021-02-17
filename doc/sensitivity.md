---
title: "AMR Imputation Sensitivity Analysis"
output: 
  html_document:
    keep_md: true
---






This document presents the results of a sensitivity analysis of missing value imputation for human and animal antimicrobial consumption. We evaluate four subsets of the data with varying degrees of imputation.

1) Countries with values for human AND animal antimicrobial consumption data, i.e., no imputation (n = 36)
2) Countries with values for human OR animal antimicrobial consumption data, i.e., one or the other is imputed (n = 73)
3) Countries with GDPs within the range of GDPs of countries that have both human and animal antimicrobial consumption data (n = 88). In this scenario, both human and animal consumption values can be imputed if the country GDP is within range. The figure below shows the distribution of GDPs for countries in this subset.
![](sensitivity_files/figure-html/gdp-1.png)<!-- -->
4) All countries in the dataset, i.e., full imputation of missing values (n = 189)

All imputations in this sensitivity analysis were performed using `MICE`. To compare results across the four scenarios, we present model coefficients, marginal effect plots for three variables of interest (human antimicrobial consumption, livestock antimicrobial consumption, and the interaction between livestock antimicrobial consumption and GDP), and maps showing reported and predicted AMR emergence counts and human/livestock antimicrobial consumption imputations by country.

### Model coefficients with in-depth focus on human and livestock antimicrobial consumption effects



##### 1. Countries with values for human AND animal antimicrobial consumption data
![](sensitivity_files/figure-html/p1-1.png)<!-- -->

##### 2. Countries with values for human OR animal antimicrobial consumption data
![](sensitivity_files/figure-html/p2-1.png)<!-- -->

##### 3. Countries with GDP values within the range of countries that have values for human\nand animal antimicrobial consumption
![](sensitivity_files/figure-html/p3-1.png)<!-- -->

##### 4. All countries in dataset
![](sensitivity_files/figure-html/p4-1.png)<!-- -->

### Maps
As desribed above, scenario 1 is limited to countries from high GDP regions. With increasing amounts of imputation (scenarios 2 through 4), more LMIC countries are incorporated into the analysis. We believe that scenario 3 brings in geographic coverage without relying too heavily on imputation.


##### 1. Countries with values for human AND animal antimicrobial consumption data
![](sensitivity_files/figure-html/m1-1.png)<!-- -->

##### 2. Countries with values for human OR animal antimicrobial consumption data
![](sensitivity_files/figure-html/m2-1.png)<!-- -->

##### 3. Countries with GDP values within the range of countries that have values for human\nand animal antimicrobial consumption
![](sensitivity_files/figure-html/m3-1.png)<!-- -->

##### 4. All countries in dataset
![](sensitivity_files/figure-html/m4-1.png)<!-- -->
