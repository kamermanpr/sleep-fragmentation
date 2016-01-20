## Regression

This repository contains the analytical [data](./data/PainRegression.csv), [data analysis scripts](SleepFragmentation_PainRegression.Rmd), and the [markdown document](SleepFragmentation_PainRegression.md) and [figure](./figures/) outputs produced by the analysis scripts for the modelling of changes in pain intensity over a ten-minute period following induction of ischaemic pain (tourniquet and hand dynamometer). Participants rated their pain at ischaemic tolerance, and the every minute thereafter for ten minutes, on a 100mm visual analogue pain scale (anchored at: 0 = no pain, 100 = worst pain ever experienced). The intervention took place on awakening from the baseline night (control - no sleep disruption), and each of the succesive sleep fragmentation nights. 

The modelling followed a three-step process. In all cases the raw VAS scores in millimetres were converted to a proportion.  

1. Linear mixed modelling of the untransformed VAS scores _**(mod1)**_. Three models were generated: i) null (intercept only), ii) basic (effect of time, but not intervention), and iii) full (effect of time and intervention; interaction was not assessed). In all three models, the slope (time), and the intercept (participant) were allowed to vary.   

2. Linear mixed modelling of the arcsine-square root transformed VAS scores _**(mod2)**_. This transform is the traditional method of dealing proportions, stabilizing variance and normalizing the data. The same three models were generated as described under point 1 above. 

3. General additive linear mixed modelling of untransformed VAS scores _**(mod3)**_. GAM allows modelling using the beta distribution, which describes [0, 1] bounded continuous probability distributions. In this case, we used the inflated beta family, which allows values from 0 to 1. The same three models were generated as described under point 1 above. 

For each of the three modelling approaches used, standard diagnostic plots were generated for the best model (the full model in each case). The linear mixed model regression on untransformed VAS data showed reducing residual variance at greater VAS scores (i.e., not homoskedastic), and the model was deemed inappropriate. Using the transformed data stabilised the residual variance, and the distribution of the residuals were reasonably close being normally distributed. This model was deemed satisfactory. The best model was that produced by the GAM, there was homoskedasticity of residuals, and the distribution of residuals was closer to a normal distribution than the transformed data. Therefore, we decided to use the results from the GAM. 

## Code book
[PainRegression.csv](./data/PainRegression.csv)  

|Key    |Label                                                                                                                                                                                                     |
|:------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|id     |Participant identification number                                                                                                                                                                         |
|period |Baseline night (no sleep fragmentation), Fragmentation nights 1 and 2 (8 awakenings each night for 2 successive nights). _The order of the baseline and fragmentation intervention was randomised_        |
|X0-10  |Pain intensity rating (0-100mm visual analogue scale (anchored at: 0 = no pain, 100 = worst pain ever experienced), measured every 60 seconds from 0 to 10 minutes after ischaemic tolerance was reached. |