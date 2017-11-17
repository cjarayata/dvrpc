## task: indentify census tracts with most low-income and/or non-white, 2015

pacman::p_load(reshape2)

setwd("~/Desktop/DVRPC/DVRPCExercise")

data <- head(read.csv("data/alabama.csv", header = T))

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


# this nested loop will be able to handle files with only one state as well as
## files that have more than one state added in

# initialize "final" dataframe we will rbind to
top.tracts <- data.frame(matrix(ncol = 13))
colnames(top.tracts) <- c("geo.id", "tract", "county", "state", "white.est", "white.err",
                         "nonwhite.est", "nonwhite.err", "lowincome.est", "lowincome.err",
                         "nonwhite.rank", "poverty.rank", "combo.rank")

ptm <- proc.time() # start timer

for(i in 1:length(list)){
  data <- read.csv(paste0("data/", list[i]), header = T)
  trim <- data[, colnames(data) %in% variables]
  file <- cbind(data[, 2:3], trim)
  
  # initialize processed dataframe
  process <- data.frame(matrix(ncol = 8, nrow = nrow(file)))
  colnames(process) <- c("geo.id", "tract", "white.est", "white.err",
                          "nonwhite.est", "nonwhite.err", "lowincome.est", "lowincome.err")
  
  process[,c(1:4)] <- file[,c(1:4)] # IDs plus white estimate and white error
  process["nonwhite.est"] <- file[,5] + # black
    file[,7] + # american indian
    file[,9] + # asian
    file[,11] + # hawaiian
    file[,13] + # some other
    file[,15] # two or more
  process["nonwhite.err"] <- file[,6] + # black
    file[,8] + # american indian
    file[,10] + # asian
    file[,12] + # hawaiian
    file[,14] + #some other
    file[,16] # two or more
  process["lowincome.est"] <- file[17]
  process["lowincome.err"] <- file[18]
  
  
  # parse tract
  location <- colsplit(process$tract, ",", c("tract", "county", "state"))
  
  process <- cbind(process[1],
                         location,
                         process[3:length(process)])
  
  # obtaining rankings for nonwhite and poverty rankings per each state in combined datafiles
  states <- unique(process$state) # make a list of the states in each file
  
  for (j in 1:length(states)){
    subset <- process[process$state == states[j], ]
    subset$nonwhite.rank <- NA
    subset$nonwhite.rank[order(-subset$nonwhite.est)] <- 1:nrow(subset)
    
    subset$poverty.rank <- NA
    subset$poverty.rank[order(-subset$lowincome.est)] <- 1:nrow(subset)
    
    subset$combo.rank <- subset$nonwhite.rank + subset$poverty.rank
    
    temp <- subset[subset$nonwhite.rank <= 10 | # top 10
                     subset$poverty.rank <= 10 | # top 10
                     subset$combo.rank <= 50, ] # any tracts with combo rank of 50 or less
    rownames(temp) <- NULL
    top.tracts <- rbind(top.tracts, temp)
  }
}

as.numeric((proc.time() - ptm)[3]) # approx 13 seconds

top.tracts <- top.tracts[-1, ]
rownames(top.tracts) <- NULL

# sanity check, make sure all 50 states represented

save(top.tracts, file = "top.tracts")

load("top.tracts")

# do national rankings



# markdown for processing pipeline; be descriptive

# SHINY
## have a viewer that will show ten:
## nationwide [default view]
## by state (drop down to pick a state)


# markdown or shiny
# VZ bonus material:
## convert the census tract information to lat/long boundaries (i might need to take the first geo.id instead)
## leaflet package
## mr data to convert to json object
## use json object to query out available API info from certain URLs
## commuting by type? amt money spent on infrastructure per tract?
## number of injuries that occur in each tract? (but i'd need to download a ton more data for this)


