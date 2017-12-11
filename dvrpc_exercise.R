####################
# initial task:
# identify census tracts with most low-income and/or non-white, 2015
####################

# load packages
pacman::p_load(reshape2, magrittr, ggplot2, plotly)

setwd("~/Desktop/DVRPC/DVRPCExercise")


# # reading in manually to poke around the data
# data <- head(read.csv("data/alabama.csv", header = T))
# 
# data2 <- read.csv("data/alaska.csv", header = T)


# list files in data directory
list <- list.files(path = "data")

# variables to keep
# poverty level cols, race cols
## format is HC01_EST / HC01_MOE [header column 01, estimate / margin of error]
## then _VC[number]
## 18 is white, 19 through 25 is what you want to add up
## 56 is poverty level 200% and below

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
    
    # combo rank will give us a *rough* idea of top nonwhite AND lowincome tracts
    subset$combo.rank <- subset$nonwhite.rank + subset$poverty.rank
    
    # for each state, cut down to top 10 NW, top 10 low-income, and any tracts w/ combo rank 30 or less
    temp <- subset[subset$nonwhite.rank <= 10 |
                     subset$poverty.rank <= 10 |
                     subset$combo.rank <= 30, ]
    rownames(temp) <- NULL
    top.tracts <- rbind(top.tracts, temp)
  }
}

as.numeric((proc.time() - ptm)[3]) # approx 15 seconds

# clean a bit
top.tracts <- top.tracts[-1, ]
rownames(top.tracts) <- NULL

# sanity check, make sure all 50 states (plus DC) represented
length(unique(top.tracts$state)) # 51

# do national rankings in same way as state rankings

top.tracts$nw.nat.rank[order(-top.tracts$nonwhite.est)] <- 1:nrow(top.tracts)
top.tracts$pov.nat.rank[order(-top.tracts$lowincome.est)] <- 1:nrow(top.tracts)
top.tracts$combo.nat.rank <- top.tracts$nw.nat.rank + top.tracts$pov.nat.rank


# make a descriptive name for ggplot labels
## format tract ##, county, state
tract.clean <- gsub(pattern = "Census Tract ", "#", top.tracts$tract)
county.clean <- gsub(pattern = " County", "", top.tracts$county)
tract.label <- paste0(tract.clean, ",", county.clean, ",", top.tracts$state)

top.tracts <- cbind(tract.label, top.tracts)

# done!
save(top.tracts, file = "top.tracts")

########
# plot testing
########

load("top.tracts")
library(ggplot2)

nw.one <- top.tracts[top.tracts$nonwhite.rank == 1, ]

ggplot(nw.one, aes(x = state, y = nonwhite.est)) +
  geom_bar(position="dodge", stat="identity", fill = "#FF6666") +
  geom_errorbar(aes(ymin = nonwhite.est - nonwhite.err,
                    ymax = nonwhite.est + nonwhite.err,
                    width = 0.5)) +
  ylab("Non-white Estimate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
  # scale_y_continuous(expand = c(0, 0, 1000, 0))


#################
# PA commuting stuff!
## table S0801 - commuting characteristics by sex - just for PA by tracts
#################

pa.commuting <- read.csv("commuting/pa_commuting.csv", header = T)

# what are the variables i need?
## GEO.id2, GEO.display.label
## HC01_EST_VC01 - total workers
## HC02, HC03 - male/female split
## EST_VC03 - car/truck/van percentage
### need male/female splits
## EST_VC10 - public
## VC11 - walked
## VC12 - cycle
## VC13 - other (taxi/uber?, motorcycle, other)
## VC55 - mean time to work
# multiply total by the %age to get rough estimate

cols <- c("HC01_EST", "HC02_EST", "HC03_EST")
vars <- c("01", "03", 10:13, "55") # numbered list
vars2 <- paste0("_VC", vars) # pasted list eg. "_VC18"
variables <- as.vector(outer(cols, vars2, paste0, sep="")) # all permutations


commute.trim <- pa.commuting[, colnames(pa.commuting) %in% variables]
commute <- cbind(pa.commuting[, 2:3], commute.trim)

pa <- top.tracts[top.tracts$state == " Pennsylvania", ]

pa.commute <- commute[commute$GEO.id2 %in% pa$geo.id, ]

# rename columns
columns <- colnames(pa.commute)

columns %<>%
  gsub("HC01_", "total", .) %>%
  gsub("HC02_", "male", .) %>%
  gsub("HC03_", "female", .) %>%
  gsub("EST_", "", .) %>% 
  gsub("VC01", ".workers", .) %>% 
  gsub("VC03", ".privatevehicle", .) %>% 
  gsub("VC10", ".publictrans", .) %>% 
  gsub("VC11", ".walk", .) %>% 
  gsub("VC12", ".bicycle", .) %>% 
  gsub("VC13", ".taxiuberother", .) %>% 
  gsub("VC55", ".meantraveltime", .)

colnames(pa.commute) <- columns

# plot testing
ggplot(pa.commute, aes(x = as.factor(GEO.id2), y = total.meantraveltime)) +
  geom_bar(position="dodge", stat="identity", fill = "#FF6666") +
  xlab("Tract ID") +
  ylab("Mean Travel Time (minutes)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

# i think i need to melt to make this useful for shiny / ggplot

# save the casted version first
save(pa.commute, file = "commuting/pa.commute")
load(file = "commuting/pa.commute")

pa.melt <- melt(pa.commute, id.vars = columns[c(1:5, 21:23)])

# string split
split <- colsplit(as.character(pa.melt$variable), "[.]", c("group", "type"))

pa.commute.melt <- cbind(pa.melt[1:8],
                 split,
                 pa.melt[10])


# both casted and melted versions; done!
save(pa.commute, pa.commute.melt, file = "dvrpcexercise/pa.commute.data")


########
# plot testing
########

load("dvrpcexercise/pa.commute.data")

test <- pa.melt[pa.melt$GEO.id2 == "42101004102", ]

ggplot(test, aes(x = factor(type, levels = unique(type)), y = value,
                 fill = factor(group, levels = unique(group)))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = paste0(test$GEO.display.label[1],
                      "\nTotal Workers: ", test$total.workers[1],
                      " [Male: ", test$male.workers[1],
                      ", Female: ", test$female.workers[1], "]",
                      "\nMean Travel Time: ", test$total.meantraveltime,
                      " minutes")) +
  xlab("Commute Type") +
  ylab("Percent Mode Share") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

########
# mapping stuff
########

pacman::p_load(maptools, sf, rgdal, leaflet)

# read in shape file from census website
pa <- readOGR("dvrpcexercise/cb_2015_42_tract_500k/cb_2015_42_tract_500k.shp")

head(pa@data)

load("dvrpcexercise/pa.commute.data")

pa.tract.ids <- pa.commute$GEO.id2[!duplicated(pa.commute$GEO.id2)]

top.pa.shape <- pa[pa@data$GEOID %in% pa.tract.ids, ] # now i have a shape file for 19 pa tracts i want

save(top.pa.shape, file = "dvrpcexercise/pa.topshape.data")

load("dvrpcexercise/pa.topshape.data")

map <- leaflet() %>%
  setView(lat = 39.9526, lng = -75.1652, zoom = 11) %>% # center on philadelphia
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data = top.pa.shape,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
              label = paste0("Tract #", top.pa.shape@data$NAME)) # display label on hover

map
