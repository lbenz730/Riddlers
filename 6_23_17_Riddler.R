drink_size <- seq(0.01, 0.99, 0.01)
coffee_taken <- rep(0, length(drink_size))

for(j in 1:length(drink_size)) {
  print(j)
  for(i in 1:50000) {
    left_in_pot <- runif(1)
    if(drink_size[j] < left_in_pot) {
      coffee_taken[j] <- coffee_taken[j] + drink_size[j]
    }
  }
}

plot(coffee_taken ~ drink_size, col = "black", pch = 19,
     xlab = "Drink Size", ylab = "Gallons Taken", 
     main = "Coffee Taken in 50000 Simulated Trips")

par(new = T)
func <- function(x){50000*(x - x^2)}
curve(func, 0, 1, lty = 1, lwd = 3, col = "red", axes = F,
      xlab = "", ylab = "")
text(0.8, 12500, paste("Coffee Per Trip Max for Drink Size = ", 
                       optimize(func, c(0,1), maximum = T)$maximum, sep = ""))
       
       
    