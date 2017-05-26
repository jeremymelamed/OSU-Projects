# Oh Deer! Game Simulation
# SMILE Systems Thinking Feedback Loop Lesson

# Initialize variables
deer <- 4
habitat <- 12
iterations <- 15
n <- deer + habitat

# Initialize deer and habitat arrays
ds <- numeric(iterations)
hs <- numeric(iterations)

# Store initial values
ds[1] <- deer
hs[1] <- habitat

for (i in 2:iterations) {
        
        # Simulate deer and habitat component selection using uniform random draw and floor division
        deer_decisions <- runif(n = deer, min = 1, max = 4) %/% 1 
        habitat_decisions <- runif(n = habitat, min = 1, max = 4) %/% 1
        
        # Difference between total deer and habitat for each choice
        c1 <- length(which(deer_decisions == 1)) - length(which(habitat_decisions == 1)) 
        c2 <- length(which(deer_decisions == 2)) - length(which(habitat_decisions == 2)) 
        c3 <- length(which(deer_decisions == 3)) - length(which(habitat_decisions == 3)) 
        
        # Determine how many deer die in round
        dead_deer <- ifelse(c1 > 0, c1, 0) + ifelse(c2 > 0, c2, 0) + ifelse(c3 > 0, c3, 0)
        
        # Adjust number of deer and habitat components for the next round.
        # Double deer who catch habitat
        deer <- 2 * (deer - dead_deer)
        habitat <- n - deer
        
        # Store population results
        ds[i] <- deer
        hs[i] <- habitat
        
}

# Plot results
plot(ds, type = 'b', main = 'Deer and Habitat Population Totals Over Time', 
     ylim = c(0, n + 5), xlab = 'Round', ylab = 'Population Totals')
points(hs, col='red') # Add habitat points
lines(hs, col = 'red') # Add habitat trend line
legend(12, n + 4, c('Deer', 'Habitat'), col = c('black', 'red'), lty = 1) # Add legend





