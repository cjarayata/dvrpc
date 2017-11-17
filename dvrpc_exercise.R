## task: indentify census tracts with most low-income and/or non-white, 2015

pacman::p_load(reshape2)

setwd("~/Desktop/DVRPC/DVRPCExercise")

data <- read.csv("states/alabama.csv", header = T)

data2 <- read.csv("states/alaska.csv", header = T)


# looks like gary does not know a streamlined way to download all 50 states info

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
state <- cbind(data[, 2:3], trim)


state.process <- data.frame(matrix(ncol = 8, nrow = nrow(state)))
colnames(state.process) <- c("geo.id", "tract", "white.est", "white.err",
                             "nonwhite.est", "nonwhite.err", "lowincome.est", "lowincome.err")

state.process[,c(1:4)] <- state[,c(1:4)] # IDs plus white estimate and white error
state.process["nonwhite.est"] <- state[,5] + # black
                                  state[,7] + # american indian
                                  state[,9] + # asian
                                  state[,11] + # hawaiian
                                  state[,13] + #some other
                                  state[,15] # two or more
state.process["nonwhite.err"] <- state[,6] + # black
                                  state[,8] + # american indian
                                  state[,10] + # asian
                                  state[,12] + # hawaiian
                                  state[,14] + #some other
                                  state[,16] # two or more
state.process["lowincome.est"] <- state[17]
state.process["lowincome.err"] <- state[18]


# parse tract
location <- colsplit(state.process$tract, ",", c("tract", "county", "state"))

state.process <- cbind(state.process[1],
                       location,
                       state.process[3:length(state.process)])

  
# sort and rank by poverty, by race (two rankings)
## could have a 'combined rank' e.g. #1 poverty + #1 race = 2, which is "best" ranking you can have?

# need to time it as well

# also do national rankings! can ddply handle this?

# once i write shiny to cut the data a bunch of different ways
# will need to convert the census tract information to lat/long boundaries
## leaflet package
## mr data to convert to json object
## use json object to query out available API info from certain URLs
## use for bonus VZ stuff? commuting by type?

# testing some changes to make sure remote to github works.

