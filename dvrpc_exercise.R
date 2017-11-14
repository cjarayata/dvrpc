## task: indentify census tracts with most low-income and/or non-white, 2015

setwd("~/Desktop/DVRPC/DVRPCExercise")

data <- read.csv("states/alabama.csv", header = T)

# for each state, trim dataset
## parse GEO.display.label into state, county, tract number
## keep raw tract identifier

# poverty level cols, race cols
## format is HC01_EST / HC01_MOE, then _VC[number]
## 18 is white, 19 through 25 is what you want to add
## 56 is poverty level 200% and below

# choosing columns to keep
cols <- c("HC01_EST", "HC01_MOE")
vars <- c(18:25, 56) # variable to keep
vars2 <- paste0("_VC", vars)
variables <- as.vector(outer(cols, vars2, paste0, sep=""))
trim <- data[, colnames(data) %in% variables]

# now the dataset is ready for one state, ish
trim <- cbind(data[, 2:3], trim)

# give useful variable names

# will i need to read in 50 datasets, one for each state?
# there's gotta be a streamlined way to do this



# sort and rank by poverty, by race (two rankings)
## could have a 'combined rank' e.g. #1 poverty + #1 race = 2, which is "best" ranking you can have?

# also do national rankings! can ddply handle this?

# bonus VZ stuff? commuting by type?



