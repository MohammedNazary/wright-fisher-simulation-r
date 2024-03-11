library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

wf_df <- data.frame()
sizes <- 1000
starting_p <- .7
n_gen <- 100
n_reps <- 50
alpha1m <- c(.0001, .001, .01, .1, .3)
alpha2m <- c(.0001, .001, .01, .1, .3)


for(alpha1 in alpha1m){
  for(alpha2 in alpha2m){
    N <- sizes
    p <- starting_p
    p0 <- p
    for(j in 1:n_gen){
      X <- rbinom(n_reps, 2*N, p)
      p <- (X / (2*N)) * (1-alpha1) + (1 - (X / (2*N))) * alpha2
      rows <- data.frame(replicate = 1:n_reps, alpha1 = rep(alpha1, n_reps),
                         gen = rep(j, n_reps), alpha2 = rep(alpha2, n_reps),
                         p = p)
      wf_df <- bind_rows(wf_df, rows)
    }
  }
}

p <- ggplot(wf_df, aes(x = gen, y = p, group = replicate)) +
     geom_path(alpha = .5) + facet_grid(alpha1 ~ alpha2) + guides(colour=FALSE)
p