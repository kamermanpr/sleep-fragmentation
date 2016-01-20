## Demographic information

This repository contains the analytical [data](./data/demographics.csv), [data analysis scripts](sleep-fragmentation-demographics.Rmd), and the [markdown document](sleep-fragmentation-demographics.md) and [figure](./figures/) outputs produced by the analysis scripts for summaries of basic demographic information, and information on participant menstrual cycles, sleep quality, and general health. 

Summary data include five-number summary statistics and a plot for each variable. All plots include a box-and-whisker summary plot with a superimposed scatterplot of individual data points.  

## Code book
|Key                |Label |
|:------------------|:-----|
|id                 |Participant identification number    |
|age_years          |Age in years    |
|weight_kg          |Weight in kilograms (kg)   |
|height_m           |Height in metres (m)   |
|bmi_kg.m           |Body mass index in kg/m^2   |
|menstrual.pain_vas |Intenisty of any pain experienced during menstruation (to exclude dysmenorrhoeic participants)    |
|ghq_score          |General Health Questionnaire score _(scores of 6 or below indicate good health; shown by dotted red line on the figure)_)    |
|psi_score          |Pittsburg Sleep Quality Index _(scores of 5 or below indicate good sleep quality; shown by dotted red line on the figure)_    |
|menarche.age_years |Age when menarche occurred (years)    |
|menstruation_years |Number of years menstruating (years)    |
|cycle.length_days  |Average duration of menstrual cycle (days)    |
|menses.length_days |Average duration of menstrual period (days)   |


