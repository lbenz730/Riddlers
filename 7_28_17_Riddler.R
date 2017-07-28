### Luke Benz
### FiveThirtyEight Riddler, 7/28/17

is_factor <- function(a, b) {
  return(b/a == round(b/a))
}

is_multiple <- function(a, b) {
  return(a/b == round(a/b))
}

sims <- 10000000
lens <- rep(NA, sims)
for(i in 1:sims) {
  print(i)
  
  ### Start Chain with Random Number
  nums <- 1:100
  chain <- sample(nums, 1)
  nums <- nums[-chain]
  len <- 1
  
  ### Build Chain
  while(sum(is_factor(chain[len], nums) | is_multiple(chain[len], nums)) > 0) {
    tmp <- nums[is_factor(chain[len], nums) | is_multiple(chain[len], nums)]
    if(length(tmp) > 1) {
      chain <- c(chain, sample(tmp, 1))
    }else{
      chain <- c(chain, tmp)
    }
    nums <- nums[!is.element(nums, chain)]
    len <- length(chain)
  }
  
  if(i == 1) {
    max_chain <- chain
    max_len <- length(chain)
  }
  
  ### Replace Max Chain if necessary
  if(len > length(max_chain)) {
    max_chain <- chain
    max_len <- length(max_chain)
  }
  
  lens[i] <- max_len
}  

plot(lens, type = "l", col = "red", lwd = 3, xlab = "Iteration", ylab = "Max Chain Length Found", main = "Iterations to Find Max Chain Length")

