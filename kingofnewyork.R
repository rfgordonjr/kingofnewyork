library(dplyr)
library(ggplot2)

# mean(rbinom(n = 100000, size = 6, prob = 1/6)) # note mean = np = 6x(1/6) = 1
simulateRolls <- function(iter, players=... ){
  # iter <- 500000;
  results <- rep(0, 7)

  for (i in 1:iter){
    # Initialize/reset:
    x <- 0; y <- 0; z <- 0; u <- 0
  
    # Simulate first roll: 
    x <- rbinom(1, 6, 1/6)
  
    # Simulate 2nd roll: 
    y <- rbinom(1, 6-x, 1/6)
  
    # simulate 3rd roll:
    z <- rbinom(1, 6-(x+y), 1/6)
  
    #Add total number
    u <- x + y + z
  
    # Update results
    results[u+1] <- results[u+1] + 1
  }
  mass <- results/iter
  distribution <- cumsum(results/iter)
  survival <- 1 - distribution
  
  return(data.frame(mass, distribution, survival))
}


results <- simulateRolls(iter = 500000)
results %>% ggplot(.) + 
  geom_bar(aes(x = 0:6, y = mass), stat = "identity") +
  geom_text(aes(x = 0:6, y=mass+0.01, label=round(mass, 3))) +
  labs(x = "Number of Claws", y = "Probability") +
  scale_x_discrete(limits = 0:6)
results %>% ggplot(.) + geom_bar(aes(x = 0:6, y = distribution), stat = "identity")
results %>% ggplot(.) + geom_bar(aes(x = 0:6, y = survival), stat = "identity")

results$mass


par(mfrow = c(1, 3))
# Mass function
massPlot <- barplot(results$mass,
        names.arg = 0:6,
        xlab = "# of claws", 
        ylab = "Probability",
        ylim = c(0, max(results$mass + 0.05)),
        main = "Probability Mass\n(Probability that # claws = x)")
text(x = massPlot, y = results$mass + 0.01, labels = round(results$mass, 3)); 

# Distribution function
distributionPlot <- barplot(distribution,
                       names.arg = 0:6,
                       xlab = "# of claws", 
                       ylab = "Probability",
                       ylim = c(0, max(distribution + 0.05)),
                       main = "Probability Distribution\n(Probability that # claws <= x)")
text(x = distributionPlot, y = distribution + 0.02, labels = round(distribution, 3)); 

# Survival function
survivalPlot <- barplot(survival,
                        names.arg = 0:6,
                        xlab = "# of claws",
                        ylim = c(0, max(survival) + 0.05),
                        main = "Survival Function\n(Probability that # claws > x)")
text(x = survivalPlot, y = survival + 0.02, labels = round(survival, 3))
