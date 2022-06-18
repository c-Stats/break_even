require(microbenchmark)

#Play n times
simulate <- function(n){

	hits <- rhyper(n, 6, 53, 6)
	cost <- 2 * n
	prize <- c(0, 0, 2, 30, 140, 1750, 10^6) 
	gains <- 0

	gains <- gains + sum(prize[hits + 1])
	#Replay when hit == 2
	n <- sum(hits == 2)

	while(n != 0){

		hits <- rhyper(n, 6, 53, 6)
		gains <- gains + sum(prize[hits + 1])
		n <- sum(hits == 2)

	}

	return(gains - cost)
	
}

#Simulation will run for approximately n mins
n <- 10
runtime <- microbenchmark(simulate(7696))

mean_runtime <- mean(runtime$time) / 10^9
n_runs <- round(n * 60 / 0.004739061)

print(paste("Runtime: ", n, " minutes.", sep = ""), quote = FALSE)
print(paste("Running ", n_runs, " times.", sep = ""), quote = FALSE)

sims <- replicate(n_runs, simulate(7696))
p_break_even <- sum(sims >= 0) / length(sims)

print(paste("P(Break even) = ", p_break_even, sep = ""), quote = FALSE)

