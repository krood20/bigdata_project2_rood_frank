library(here)

source("predictive-functions.R")

path <- here("data", "adult.small")
header <- c('age', 'workclass', 'fnlwgt', 'education', 'education_num',
            'marital_status', 'occupation', 'relationship', 'race', 'sex',
            'capital_gain', 'capital_loss', 'hours_per_week', 'native_cournty',
            'income')
census.table <- file_to_table(path, header)
cenus.labeled <- label_data(census.table)

pairs(cenus.labeled[, c('hours_per_week', 'race')])