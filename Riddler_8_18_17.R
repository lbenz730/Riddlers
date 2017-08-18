### Luke Benz
### Riddler 8/18/2017

### Start String
iterations <- 100000
num_string <- c(3,3,3,2)
ratio <- 3

### Iterate + Build
for(i in 2:iterations) {
  num_string <- c(num_string, rep(3, num_string[i]), 2)
  ratio <- c(ratio, round(sum(num_string == 3)/sum(num_string == 2), 4))
}

### Get Final Ratio
print(paste("Final 3:2 Ratio:", ratio[iterations], sep = " "))

### Plot
par(mfrow = c(2,2))
plot(ratio[1:100], type = "l", col = "Red", lwd = 3, 
     xlab = "Number of 2's in String", 
     ylab = "3:2 Ratio",
     main  = "First 100 2s")
abline(h = ratio[iterations], col = "blue", lty = 3, lwd = 3)
text(100 * 0.8, ratio[iterations] + 0.1, paste("3:2 Ratio Limit:", ratio[iterations], sep = " "))

plot(ratio[1:250], type = "l", col = "Red", lwd = 3, 
     xlab = "Number of 2's in String", 
     ylab = "3:2 Ratio",
     main  = "First 250 2s")
abline(h = ratio[iterations], col = "blue", lty = 3, lwd = 3)
text(250 * 0.8, ratio[iterations] + 0.1, paste("3:2 Ratio Limit:", ratio[iterations], sep = " "))

plot(ratio[1:500], type = "l", col = "Red", lwd = 3, 
     xlab = "Number of 2's in String", 
     ylab = "3:2 Ratio",
     main  = "First 500 2s")
abline(h = ratio[iterations], col = "blue", lty = 3, lwd = 3)
text(500 * 0.8, ratio[iterations] + 0.1, paste("3:2 Ratio Limit:", ratio[iterations], sep = " "))

plot(ratio[1:1000], type = "l", col = "Red", lwd = 3, 
     xlab = "Number of 2's in String", 
     ylab = "3:2 Ratio",
     main  = "First 1000 2s")
abline(h = ratio[iterations], col = "blue", lty = 3, lwd = 3)
text(1000 * 0.8, ratio[iterations] + 0.1, paste("3:2 Ratio Limit:", ratio[iterations], sep = " "))
