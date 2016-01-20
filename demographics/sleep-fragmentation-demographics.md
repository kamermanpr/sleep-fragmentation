Demographics
============

### Authors: Stella Iacovides & Peter Kamerman

**Date: December 10, 2015**

------------------------------------------------------------------------

Load required packages and set chunk options
--------------------------------------------

``` r
# Load packages
library(knitr)
library(pander)
library(ggplot2)
library(scales)
library(grid)
library(cowplot)
library(readr)
library(dplyr)
library(tidyr)

# knitr chunk options
opts_chunk$set(echo = FALSE,
               warning = FALSE,
               message = FALSE,
               fig.path = './figures/',
               fig.width = 7,
               fig.height = 6,
               dev = c('png', 'pdf'),
               tidy = TRUE, 
               tidy.opts = list(width.cutoff = 65))
```

Load data
---------

``` r
demo <- read_csv("./data/demographics.csv", col_names = T)
```

Quick look
----------

    ## Observations: 11
    ## Variables: 12
    ## $ id                 (chr) "A", "B", "C", "D", "E", "F", "G", "H", "I"...
    ## $ age_years          (int) 21, 21, 21, 21, 21, 22, 24, 21, 21, 21, 23
    ## $ weight_kg          (int) 65, 55, 52, 70, 78, 53, 88, 60, 66, 63, 52
    ## $ height_m           (dbl) 1.65, 1.57, 1.56, 1.56, 1.50, 1.56, 1.65, 1...
    ## $ bmi_kg.m           (dbl) 23.8, 22.3, 22.4, 28.7, 34.6, 21.7, 32.3, 2...
    ## $ menstrual.pain_vas (int) 9, 24, 30, 11, 20, 16, 25, 10, 9, 9, 7
    ## $ ghq_score          (int) 0, 5, 6, 4, 4, 0, 4, 1, 0, 4, 3
    ## $ psi_score          (int) 3, 5, 5, 5, 5, 2, 4, 2, 4, 2, 4
    ## $ menarche.age_years (int) 13, 15, 15, 15, 13, 11, 12, 14, 12, 14, 15
    ## $ menstruation_years (int) 8, 6, 6, 6, 8, 11, 12, 7, 9, 7, 8
    ## $ cycle.length_days  (int) 28, 28, 28, 28, 30, 28, 28, 28, 28, 28, 28
    ## $ menses.length_days (int) 5, 4, 5, 5, 5, 6, 5, 5, 5, 5, 4

    ##       id              age_years       weight_kg        height_m    
    ##  Length:11          Min.   :21.00   Min.   :52.00   Min.   :1.500  
    ##  Class :character   1st Qu.:21.00   1st Qu.:54.00   1st Qu.:1.560  
    ##  Mode  :character   Median :21.00   Median :63.00   Median :1.570  
    ##                     Mean   :21.55   Mean   :63.82   Mean   :1.597  
    ##                     3rd Qu.:21.50   3rd Qu.:68.00   3rd Qu.:1.650  
    ##                     Max.   :24.00   Max.   :88.00   Max.   :1.710  
    ##     bmi_kg.m     menstrual.pain_vas   ghq_score       psi_score    
    ##  Min.   :20.30   Min.   : 7.00      Min.   :0.000   Min.   :2.000  
    ##  1st Qu.:22.00   1st Qu.: 9.00      1st Qu.:0.500   1st Qu.:2.500  
    ##  Median :22.80   Median :11.00      Median :4.000   Median :4.000  
    ##  Mean   :25.15   Mean   :15.45      Mean   :2.818   Mean   :3.727  
    ##  3rd Qu.:27.50   3rd Qu.:22.00      3rd Qu.:4.000   3rd Qu.:5.000  
    ##  Max.   :34.60   Max.   :30.00      Max.   :6.000   Max.   :5.000  
    ##  menarche.age_years menstruation_years cycle.length_days
    ##  Min.   :11.00      Min.   : 6.0       Min.   :28.00    
    ##  1st Qu.:12.50      1st Qu.: 6.5       1st Qu.:28.00    
    ##  Median :14.00      Median : 8.0       Median :28.00    
    ##  Mean   :13.55      Mean   : 8.0       Mean   :28.18    
    ##  3rd Qu.:15.00      3rd Qu.: 8.5       3rd Qu.:28.00    
    ##  Max.   :15.00      Max.   :12.0       Max.   :30.00    
    ##  menses.length_days
    ##  Min.   :4.000     
    ##  1st Qu.:5.000     
    ##  Median :5.000     
    ##  Mean   :4.909     
    ##  3rd Qu.:5.000     
    ##  Max.   :6.000

Summary plots
-------------

### Age

<table>
<caption>Five-number summary of age (years)</caption>
<colgroup>
<col width="9%" />
<col width="13%" />
<col width="12%" />
<col width="9%" />
<col width="13%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Min.</th>
<th align="center">1st Qu.</th>
<th align="center">Median</th>
<th align="center">Mean</th>
<th align="center">3rd Qu.</th>
<th align="center">Max.</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">21</td>
<td align="center">21</td>
<td align="center">21</td>
<td align="center">21.55</td>
<td align="center">21.5</td>
<td align="center">24</td>
</tr>
</tbody>
</table>

![](./figures/Age-1.png)

### Weight

<table>
<caption>Five-number summary of weight (kg)</caption>
<colgroup>
<col width="9%" />
<col width="13%" />
<col width="12%" />
<col width="9%" />
<col width="13%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Min.</th>
<th align="center">1st Qu.</th>
<th align="center">Median</th>
<th align="center">Mean</th>
<th align="center">3rd Qu.</th>
<th align="center">Max.</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">52</td>
<td align="center">54</td>
<td align="center">63</td>
<td align="center">63.82</td>
<td align="center">68</td>
<td align="center">88</td>
</tr>
</tbody>
</table>

![](./figures/weight-1.png)

### Height

<table>
<caption>Five-number summary of height (m)</caption>
<colgroup>
<col width="9%" />
<col width="13%" />
<col width="12%" />
<col width="9%" />
<col width="13%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Min.</th>
<th align="center">1st Qu.</th>
<th align="center">Median</th>
<th align="center">Mean</th>
<th align="center">3rd Qu.</th>
<th align="center">Max.</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">1.5</td>
<td align="center">1.56</td>
<td align="center">1.57</td>
<td align="center">1.597</td>
<td align="center">1.65</td>
<td align="center">1.71</td>
</tr>
</tbody>
</table>

![](./figures/height-1.png)

### Body mass index (BMI)

<table>
<caption>Five-number summary of body mass index (BMI; m/kg^2)</caption>
<colgroup>
<col width="9%" />
<col width="13%" />
<col width="12%" />
<col width="9%" />
<col width="13%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Min.</th>
<th align="center">1st Qu.</th>
<th align="center">Median</th>
<th align="center">Mean</th>
<th align="center">3rd Qu.</th>
<th align="center">Max.</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">1.5</td>
<td align="center">1.56</td>
<td align="center">1.57</td>
<td align="center">1.597</td>
<td align="center">1.65</td>
<td align="center">1.71</td>
</tr>
</tbody>
</table>

![](./figures/bmi-1.png)

### Intensity of menstrual pain

<table>
<caption>Five-number summary of menstrual pain ratings (0-100mm VAS)</caption>
<colgroup>
<col width="9%" />
<col width="13%" />
<col width="12%" />
<col width="9%" />
<col width="13%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Min.</th>
<th align="center">1st Qu.</th>
<th align="center">Median</th>
<th align="center">Mean</th>
<th align="center">3rd Qu.</th>
<th align="center">Max.</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">7</td>
<td align="center">9</td>
<td align="center">11</td>
<td align="center">15.45</td>
<td align="center">22</td>
<td align="center">30</td>
</tr>
</tbody>
</table>

![](./figures/menstrualPain-1.png)

### General Health Questionniare

<table>
<caption>Five-number summary of General Health Questionniare (GHQ) scores</caption>
<colgroup>
<col width="9%" />
<col width="13%" />
<col width="12%" />
<col width="9%" />
<col width="13%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Min.</th>
<th align="center">1st Qu.</th>
<th align="center">Median</th>
<th align="center">Mean</th>
<th align="center">3rd Qu.</th>
<th align="center">Max.</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">0</td>
<td align="center">0.5</td>
<td align="center">4</td>
<td align="center">2.818</td>
<td align="center">4</td>
<td align="center">6</td>
</tr>
</tbody>
</table>

![](./figures/ghqScore-1.png)

### Pittsburg Sleep Quality Index (PSQI)

<table>
<caption>Five-number summary of Pittsburg Sleep Quality Index (PSQI) scores</caption>
<colgroup>
<col width="9%" />
<col width="13%" />
<col width="12%" />
<col width="9%" />
<col width="13%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Min.</th>
<th align="center">1st Qu.</th>
<th align="center">Median</th>
<th align="center">Mean</th>
<th align="center">3rd Qu.</th>
<th align="center">Max.</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">2</td>
<td align="center">2.5</td>
<td align="center">4</td>
<td align="center">3.727</td>
<td align="center">5</td>
<td align="center">5</td>
</tr>
</tbody>
</table>

![](./figures/psqiScore-1.png)

### Age at menarche

<table>
<caption>Five-number summary of age at menarche (years)</caption>
<colgroup>
<col width="9%" />
<col width="13%" />
<col width="12%" />
<col width="9%" />
<col width="13%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Min.</th>
<th align="center">1st Qu.</th>
<th align="center">Median</th>
<th align="center">Mean</th>
<th align="center">3rd Qu.</th>
<th align="center">Max.</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">11</td>
<td align="center">12.5</td>
<td align="center">14</td>
<td align="center">13.55</td>
<td align="center">15</td>
<td align="center">15</td>
</tr>
</tbody>
</table>

![](./figures/menarcheAge-1.png)

### Time since menarche

<table>
<caption>Five-number summary of time since menarche (years)</caption>
<colgroup>
<col width="9%" />
<col width="13%" />
<col width="12%" />
<col width="9%" />
<col width="13%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Min.</th>
<th align="center">1st Qu.</th>
<th align="center">Median</th>
<th align="center">Mean</th>
<th align="center">3rd Qu.</th>
<th align="center">Max.</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">6</td>
<td align="center">6.5</td>
<td align="center">8</td>
<td align="center">8</td>
<td align="center">8.5</td>
<td align="center">12</td>
</tr>
</tbody>
</table>

![](./figures/timeMenarche-1.png)

### Average menstrual cycle length

<table>
<caption>Five-number summary of menstrual cycle length (days)</caption>
<colgroup>
<col width="9%" />
<col width="13%" />
<col width="12%" />
<col width="9%" />
<col width="13%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Min.</th>
<th align="center">1st Qu.</th>
<th align="center">Median</th>
<th align="center">Mean</th>
<th align="center">3rd Qu.</th>
<th align="center">Max.</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">28</td>
<td align="center">28</td>
<td align="center">28</td>
<td align="center">28.18</td>
<td align="center">28</td>
<td align="center">30</td>
</tr>
</tbody>
</table>

![](./figures/cycleLength-1.png)

### Duration of menstrual period

<table>
<caption>Five-number summary of duration of menstrual period (days)</caption>
<colgroup>
<col width="9%" />
<col width="13%" />
<col width="12%" />
<col width="9%" />
<col width="13%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Min.</th>
<th align="center">1st Qu.</th>
<th align="center">Median</th>
<th align="center">Mean</th>
<th align="center">3rd Qu.</th>
<th align="center">Max.</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">4</td>
<td align="center">5</td>
<td align="center">5</td>
<td align="center">4.909</td>
<td align="center">5</td>
<td align="center">6</td>
</tr>
</tbody>
</table>

![](./figures/periodLength-1.png)

Session information
-------------------

    ## R version 3.2.2 (2015-08-14)
    ## Platform: x86_64-apple-darwin13.4.0 (64-bit)
    ## Running under: OS X 10.11.1 (El Capitan)
    ## 
    ## locale:
    ## [1] C
    ## 
    ## attached base packages:
    ## [1] grid      stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ## [1] tidyr_0.3.1   dplyr_0.4.3   readr_0.2.2   cowplot_0.5.0 scales_0.3.0 
    ## [6] ggplot2_1.0.1 pander_0.6.0  knitr_1.11   
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_0.12.2      magrittr_1.5     MASS_7.3-45      munsell_0.4.2   
    ##  [5] colorspace_1.2-6 R6_2.1.1         stringr_1.0.0    plyr_1.8.3      
    ##  [9] tools_3.2.2      parallel_3.2.2   gtable_0.1.2     DBI_0.3.1       
    ## [13] htmltools_0.2.6  yaml_2.1.13      digest_0.6.8     assertthat_0.1  
    ## [17] reshape2_1.4.1   formatR_1.2.1    evaluate_0.8     rmarkdown_0.8.1 
    ## [21] labeling_0.3     stringi_1.0-1    proto_0.3-10
