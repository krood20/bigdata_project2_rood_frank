library(here)
library(GGally)

source("predictive-functions.R")

path <- here("data", "adult.small")
header <- c('age', 'workclass', 'fnlwgt', 'education', 'education_num',
            'marital_status', 'occupation', 'relationship', 'race', 'sex',
            'capital_gain', 'capital_loss', 'hours_per_week', 'native_cournty',
            'income')
census.table <- file_to_table(path, header)
smallds <- subset(census.table, select=c('age', 'race', 'sex', 'hours_per_week', 'income'))
smallds <- subset(census.table, select=c('age', 'income'))
ggpairs(smallds)
pairs(smallds)

#census.labeled <- label_data(census.table)
#census.no_lab <- file_to_table(path)
#census.table$age

#pairs(race ~ hours_per_week, data = census.table[, c('hours_per_week', 'race')])
#plot(hours_per_week ~ race.factor, data =census.labeled[, c('hours_per_week', 'race')])

#trans.factor <- factor(rep(c("t0", "t1", "t2"), c(4, 1, 2)))
#trans.factor

#plot(trans.factor)