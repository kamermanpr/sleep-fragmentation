# Demographics table
####################
## Authors: Peter Kamerman, Fiona Baker, and Stella Iacovides
## Contains script used to generate the demographics table used in the manuscript

### Load required packages
library(readr)
library(dplyr)
library(tidyr)
### Load data
demoTable <- read_csv('./data/demographics.csv', col_names = T)
### Quick look
glimpse(demoTable)
### Process data
Mean <- as.numeric(summarise_each(demoTable, funs(mean), 2:12))
SD <- as.numeric(summarise_each(demoTable, funs(sd), 2:12))
Median <- as.numeric(summarise_each(demoTable, funs(median), 2:12))
q25 <- as.numeric(summarise_each(demoTable, funs(quantile(., probs = 0.25)), 2:12))
q75 <- as.numeric(summarise_each(demoTable, funs(quantile(., probs = 0.75)), 2:12))
Variable <- c('Age', 'Weight', 'Height',
              'BMI', 'Pain_during_menstruation',
              'GHQ', 'PSI', 'Age_of_menarche',
              'Menstruation', 'Cycle_length',
              'Period_length')
demoTable2 <- as.data.frame(
    cbind(Variable, as.numeric(Mean), as.numeric(SD),
          as.numeric(Median), as.numeric(q25),
          as.numeric(q75)))
demoTable3 <- demoTable2 %>%
    mutate(Value = paste0(round(Median, 2),
                          ' (',
                          round(q25, 2),
                          ' - ',
                          round(q75, 2),
                          ')')) %>%
    select(Variable, Value)
### Write to csv for conversion into a piublication-ready table in Google Docs
write_csv(demoTable3, './data/demographics-table.csv')
