library(dplyr)
library(igraph)

### Read in All Games from 2017-18 Season
x <- read.csv("NCAA_Hoops_Results_4_6_2018.csv", as.is = T)
x$scorediff <- x$teamscore - x$oppscore

### Get Teams that beat Villanova
trans_champs <- vector()
next_teams <- filter(x, team == "Villanova", scorediff < 0) %>% pull(opponent)
degrees_of_seperation <- list()
degrees_of_seperation[[1]] <- next_teams
k <- 2

### Find Transitive National Champions
while(length(next_teams) > 0) {
  trans_champs <- c(trans_champs, unique(next_teams))
  tmp <- vector()
  for(i in 1:length(next_teams)) {
    tmp <- c(tmp, filter(x, team == next_teams[i], scorediff < 0) %>% pull(opponent))
  }
  next_teams <- unique(setdiff(tmp, trans_champs))
  degrees_of_seperation[[k]] <- next_teams
  k <- k+1
}

### Another Approach (Directed Graph)
y <- filter(x, scorediff > 0) %>% select(team, opponent)
win_network <- graph.data.frame(y, directed = T)
is.connected(win_network, mode = "strong")
E(win_network)$arrow.size <- 0.01
cols <- c(rep("orange", 328), "navy", rep("orange", 32))
plot(win_network, vertex.label.cex = 0.3, vertex.size = 5, vertex.color = cols)

### Degress of Seperation from Villanova
plot(unlist(lapply(degrees_of_seperation, length)[1:6]), type = "l", lwd = 3, col = "orange",
     ylab = "Transitive National Champions", xlab = "Degrees of Seperation from Villanova",
     main = "2017-18 Transitive National Champions")
