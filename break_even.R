require(microbenchmark)

#One ticket, one play
f <- function(prize){
     
     n <- rhyper(1, 6, 53, 6)     
     return(c(n, prize[n + 1]))
     
}
 
#One ticket, keep playing if n = 2
g <- function(prize){
     
     S <- 0
     while(TRUE){
         
         x <- f(prize)
         S <- S + x[2]
         if(x[1] != 2){break}
         
     }
     
     return(S)
     
 }

#Play n times
simulate <- function(n){

	hits <- rhyper(n, 6, 53, 6)
	cost <- 2 * n
	prize <- c(0, 0, 2, 30, 140, 1750, 10^6) 

	final <- which(hits != 2)
	S <- sum(prize[hits[final] + 1])

	hits <- hits[-final]
	S <- sum(hits) + sum(sapply(hits, function(x){g(prize)}))

	return(S - cost)
	
}

#Simulation will run for approximately 1min
runtime <- microbenchmark(simulate(7696))

mean_runtime <- mean(runtime$time) / 10^9
n_runs <- round(60 / 0.004739061)

print(paste("Running ", n_runs, " times.", sep = ""), quote = FALSE)

sims <- replicate(n_runs, simulate(7696))
p_break_even <- sum(sims >= 0) / length(sims)

print(paste("P(Break even) = ", p_break_even, sep = ""), quote = FALSE)
