library(dplyr)
library(ggplot2)

# Make up some data
stratum <- factor(rep(c("A", "B"),  each = 500))
set.seed(1)
value <- c(rnorm(500, 50, 10), rnorm(500, 80, 10))
population <- data_frame(stratum, value)

# Plot
population %>% 
  ggplot(aes(x = value, color = stratum)) + geom_density()

# What happens with SRS?
population %>% 
  sample_n(size = 10) %>% 
  group_by(stratum) %>% 
  summarize(n=n(), mean=mean(value))

# What happens with Stratified?
population %>% 
  group_by(stratum) %>% 
  sample_n(size = 5) %>% 
  summarize(n=n(), mean=mean(value))


# Larger number of SRS simulations
srs_sims <- replicate(1000, 
                      population %>% 
                        sample_n(size = 10),
                      simplify = FALSE)
                  
srs_means <- sapply(srs_sims, function(x) mean(x$value))

# Stratified
strat_sims <- replicate(1000, 
                        population %>%
                          group_by(stratum) %>% 
                          sample_n(size = 5),
                        simplify = FALSE)

strat_means <- sapply(strat_sims, 
                      function(x) (5*mean(x$value[1:5]) + 5*mean(x$value[6:10]))/10)

# Compare

all_means <- rbind(data_frame(scheme="srs", means=srs_means),
                   data_frame(scheme="strat", means=strat_means))

all_means %>% 
  ggplot(aes(x=means, color=scheme)) + geom_density()

all_means %>%
  group_by(scheme) %>% 
  summarize(mean=mean(means), var=var(means), sd=sd(means))
