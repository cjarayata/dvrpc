## task: indentify census tracts with most low-income and/or non-white, 2015

pacman::p_load(reshape2)

setwd("~/Desktop/DVRPC/DVRPCExercise")

data <- read.csv("data/alabama.csv", header = T)

data2 <- read.csv("data/alaska.csv", header = T)


# poverty level cols, race cols
## format is HC01_EST / HC01_MOE [header column 01, estimate / margin of error]
## then _VC[number]
## 18 is white, 19 through 25 is what you want to add
## 56 is poverty level 200% and below

# choosing columns to keep
cols <- c("HC01_EST", "HC01_MOE")
vars <- c(18:25, 56) # numbered list
vars2 <- paste0("_VC", vars) # pasted list eg. "_VC18"
variables <- as.vector(outer(cols, vars2, paste0, sep="")) # all permutations
trim <- data[, colnames(data) %in% variables] # cut dataframe

# add geo identifiers
state <- cbind(data[, 2:3], trim)

# initialize processed dataframe
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

# obtaining rankings for nonwhite and poverty rankings
state.process$nonwhite.rank <- NA
state.process$nonwhite.rank[order(-state.process$nonwhite.est)] <- 1:nrow(state.process)

state.process$poverty.rank <- NA
state.process$poverty.rank[order(-state.process$lowincome.est)] <- 1:nrow(state.process)

# sum ranks to find overlapping tracts
state.process$combo.rank <- state.process$nonwhite.rank + state.process$poverty.rank

# the finale
top.tract.state <- state.process[state.process$nonwhite.rank <=10 | # top 10
                                   state.process$poverty.rank <= 10 | # top 10
                                   state.process$combo.rank <= 50, ] # any tracts with combo rank of 50 or less

rownames(top.tract.state) <- NULL


############### LOOP
# now i am going to need to process every single file, and then rbind it together


# list files
list <- list.files(path = "data")

# variables to keep
cols <- c("HC01_EST", "HC01_MOE")
vars <- c(18:25, 56) # numbered list
vars2 <- paste0("_VC", vars) # pasted list eg. "_VC18"
variables <- as.vector(outer(cols, vars2, paste0, sep="")) # all permutations

# initialize blank top.tract.state
top.tract <- data.frame(matrix(ncol = 13))
colnames(top.tract) <- c("geo.id", "tract", "county", "state", "white.est", "white.err",
                         "nonwhite.est", "nonwhite.err", "lowincome.est", "lowincome.err",
                         "nonwhite.rank", "poverty.rank", "combo.rank")


## this loop *works* but because i downloaded the data in chunks, it does not do the state parsing correctly

ptm <- proc.time() # start timer

for(i in 1:length(list)){
  data <- read.csv(paste0("data/", list[i]), header = T)
  trim <- data[, colnames(data) %in% variables]
  state <- cbind(data[, 2:3], trim)
  
  # initialize processed dataframe
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
  
  # obtaining rankings for nonwhite and poverty rankings
  state.process$nonwhite.rank <- NA
  state.process$nonwhite.rank[order(-state.process$nonwhite.est)] <- 1:nrow(state.process)
  
  state.process$poverty.rank <- NA
  state.process$poverty.rank[order(-state.process$lowincome.est)] <- 1:nrow(state.process)
  
  # sum ranks to find overlapping tracts
  state.process$combo.rank <- state.process$nonwhite.rank + state.process$poverty.rank
  
  # the finale
  top <- state.process[state.process$nonwhite.rank <=10 | # top 10
                                     state.process$poverty.rank <= 10 | # top 10
                                     state.process$combo.rank <= 50, ] # any tracts with combo rank of 50 or less
  
  rownames(top) <- NULL
  top.tract <- rbind(top.tract, top)
}


as.numeric((proc.time() - ptm)[3]) # stop timer

# once national dataset is together, do national rankings

# need to time it as well

# make sure you have all 50 states


# markdown for processing pipeline; be descriptive

# SHINY
## have a viewer that will show ten:
## nationwide [default view]
## by state (drop down to pick a state)


# markdown or shiny
# VZ bonus material:
## convert the census tract information to lat/long boundaries
## leaflet package
## mr data to convert to json object
## use json object to query out available API info from certain URLs
## commuting by type? amt money spent on infrastructure per tract?
## number of injuries that occur in each tract? (but i'd need to download a ton more data for this)


