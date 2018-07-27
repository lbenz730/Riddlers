### Luke Benz
### Ridler Classic
### 7/27/2018

### Get Coordinates for potential move
move_coord <- function(move_type, x, y) {
  coordinates <- list("x" = vector(), "y" = vector())
  if(move_type == "left") {
    coordinates$x <- x - 1:3
    coordinates$y <- rep(y, 3)
  }
  else if(move_type == "right") {
    coordinates$x <- x + 1:3
    coordinates$y <- rep(y, 3)
  }
  else if(move_type == "up") {
    coordinates$x <- rep(x, 3)
    coordinates$y <- y + 1:3
  }
  else if(move_type == "down") {
    coordinates$x <- rep(x, 3)
    coordinates$y <- y - 1:3
  }
  else if(move_type == "up_right") {
    coordinates$x <- x + 1:2
    coordinates$y <- y + 1:2
  }
  else if(move_type == "up_left") {
    coordinates$x <- x - 1:2
    coordinates$y <- y + 1:2
  }
  else if(move_type == "down_right") {
    coordinates$x <- x + 1:2
    coordinates$y <- y - 1:2
  }  
  else if(move_type == "down_left") {
    coordinates$x <- x - 1:2
    coordinates$y <- y - 1:2
  }
  return(coordinates)
}

### Test if move is valid
valid_move <- function(grid, coordinates) {
  n <- nrow(grid)
  ### Wrap-Around
  if(any(coordinates$y < 1) | any(coordinates$x < 1)) {
    return(F)
  }
  
  if(any(coordinates$y > n) | any(coordinates$x > n)) {
    return(F)
  }
  
  ### Already Visited
  for(k in 1:length(coordinates$x)) {
    if(grid[coordinates$y[k], coordinates$x[k]]) {
      return(F)
    }
  }
  
  return(T)
}

### Build Grid
reset_grid <- function(n) {
  matrix(F, nrow = n, ncol = n) 
}

xmoves <- c()
ymoves <- c()
moves_used <- c()
blank <- c()
fails <- 0

### Square Size
n <- 5

moves <- c("left", "right", "up", "down", "up_right", 
           "up_left", "down_right", "down_left")
grid <- reset_grid(n)

x <- sample(1:n, 1)
y <- sample(1:n, 1)
grid[y,x] <- T

while(any(!grid)) {
  xmoves <- c(xmoves, x)
  ymoves <- c(ymoves, y)
  
  
  ### Get Set of all Valid Moves
  z <- sapply(moves, move_coord, x, y) 
  good_moves <- vector() 
  for(i in 1:length(moves)) {
    if(valid_move(grid, list("x" = z[1,i][[1]], "y" = z[2,i][[1]]))) {
      good_moves <- c(good_moves, moves[i])
    }
  }
  
  ### Make Move, or Reset
  if(length(good_moves) >= 1) {
    move <- sample(good_moves, 1)
    moves_used <- c(moves_used, move)
    coordinates <- move_coord(move, x, y)
    for(k in 1:length(coordinates$x)) {
      grid[coordinates$y[k], coordinates$x[k]] <- T
    }
    x <- coordinates$x[k]
    y <- coordinates$y[k]
  }
  else{
    fails <- fails + 1
    if(sum(!grid) == 4) {
      break
    }
    blank <- c(blank, sum(!grid))
    if(fails %% 100 == 0) {
      print(paste("Fails:", fails))
    }
    xmoves <- c()
    ymoves <- c()
    moves_used <- c()
    grid <- reset_grid(n)
    x <- sample(1:n, 1)
    y <- sample(1:n, 1)
    grid[y,x] <- T
    
  }
}

### Min Legally Covered
table(blank)
n^2 - max(blank)

### Max Legally Covered
n^2 - min(blank)

frame <- expand.grid("x" = 1:n, "y" = 1:n)
frame$move <- 0
frame$covered <- F
frame$covered[frame$x == xmoves[1] &frame$y == ymoves[1]] <- T
all_frames <- frame
for(i in 1:length(moves)) {
  frame$move <- i
  coordinates <- move_coord(moves_used[i], xmoves[i], ymoves[i])
  for(k in 1:length(coordinates$x)) {
    frame$covered[frame$x == coordinates$x[k] & 
                    frame$y == coordinates$y[k]] <- T
  }
  
  all_frames <- rbind(all_frames, frame)
}

library(ggplot2)
library(dplyr)

pdf("riddler.pdf")
for(i in 0:8) {
  p <- ggplot(filter(all_frames, move == i), aes(x = x, y = y, fill = covered)) + 
    geom_raster() + scale_fill_manual("Visited", values = c("white", "aquamarine1")) + 
    geom_vline(xintercept = 0.5 + 1:4, color = "black", size = 1.2) + 
    geom_hline(yintercept = 0.5 + 1:4, color = "black", size = 1.2) +
    labs(x = "X", y = "Y", title = paste("Move:", i))
  print(p)
}
dev.off()