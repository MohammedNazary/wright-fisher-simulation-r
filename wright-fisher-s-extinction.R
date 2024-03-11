library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

wf_df <- data.frame()
sizes <- c(10, 50, 100, 1000, 5000, 15000)
starting_p <- c(.01, .1, .3, .5, .7, .9, .99)
n_gen <- 100
n_reps <- 50

for(N in sizes){
  for(p in starting_p){
    p0 <- p
    for(j in 1:n_gen){
      X <- rbinom(n_reps, 2*N, p)
      p <- X / (2*N)
      rows <- data.frame(replicate = 1:n_reps, N = rep(N, n_reps),
                         gen = rep(j, n_reps), p0 = rep(p0, n_reps),
                         p = p)
      wf_df <- bind_rows(wf_df, rows)
    }
  }
}

p <- ggplot(wf_df, aes(x = gen, y = p, group = replicate)) +
     geom_path(alpha = .5) + facet_grid(N ~ p0) + guides(colour=FALSE)
p