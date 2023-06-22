## load packages
library(raster)
library(ggplot2)
library(gridExtra)

## source functions
setwd("W:/PYWELL_SHARED/Pywell Projects/BRC/Rob Boyd/UK-SCAPE/resolution_vs_bias/R")

source("./coarsen.R")
source("./createCorrelatedVariable.R")
source("./simulateSample.R")
source("./simulateDistribution.R")
source("./mvrn.R")

## now, for each virtual species, create its distribution, create a sample and coarsen it to see what happens 

# rare clustered species 
rareClustered <- simulateDistribution(prevalence = 0.01, gridSize = 80, phi = 0.1)

rareClusteredSamp <- lapply(rep(0.1,10),
                            simulateSample,
                            rho = 0.067, 
                            distribution = rareClustered)

rareClusteredStats <- lapply(c(1,2,4,8,16),
                             FUN = coarsen,
                             distribution = rareClustered,
                             sample = rareClusteredSamp,
                             id = paste0("clustered_", round(cellStats(rareClustered, stat = "mean"),2)))

rareClusteredStats <- do.call("rbind", rareClusteredStats); rareClusteredStats

rareClusteredStats$ddc[is.na(rareClusteredStats$ddc)] <- 0

rareClusteredStatsMeans <- means_df <- aggregate(. ~ id + fact, data = rareClusteredStats, FUN = mean); rareClusteredStatsMeans

# rare dispersed species
rareDispersed <- simulateDistribution(prevalence = 0.01, gridSize = 80, phi = 0.6)

rareDispersedSamp <- lapply(rep(0.1,10),
                            simulateSample,
                            rho = 0.0675, 
                            distribution = rareDispersed)

rareDispersedStats <- lapply(c(1,2,4,8,16),
                             FUN = coarsen,
                             distribution = rareDispersed,
                             sample = rareDispersedSamp,
                             id = paste0("dispersed_", round(cellStats(rareDispersed, stat = "mean"),2)))

rareDispersedStats <- do.call("rbind", rareDispersedStats); rareDispersedStats

rareDispersedStats$ddc[is.na(rareDispersedStats$ddc)] <- 0

rareDispersedStatsMeans <- means_df <- aggregate(. ~ id + fact, data = rareDispersedStats, FUN = mean); rareDispersedStatsMeans

# medium clustered species
medClustered <- simulateDistribution(prevalence = 0.25, gridSize = 80, phi = 0.1)

medClusteredSamp <- lapply(rep(0.1,10),
                            simulateSample,
                            rho = 0.07, 
                            distribution = medClustered)

medClusteredStats <- lapply(c(1,2,4,8,16),
                             FUN = coarsen,
                             distribution = medClustered,
                             sample = medClusteredSamp,
                             id = paste0("clustered_", round(cellStats(medClustered, stat = "mean"),2)))

medClusteredStats <- do.call("rbind", medClusteredStats); medClusteredStats

medClusteredStats$ddc[is.na(medClusteredStats$ddc)] <- 0

medClusteredStatsMeans <- means_df <- aggregate(. ~ id + fact, data = medClusteredStats, FUN = mean); medClusteredStatsMeans

# medium dispersed species
medDispersed <- simulateDistribution(prevalence = 0.25, gridSize = 80, phi = 0.6)

medDispersedSamp <- lapply(rep(0.1,10),
                            simulateSample,
                            rho = 0.07, 
                            distribution = medDispersed)

medDispersedStats <- lapply(c(1,2,4,8,16),
                            FUN = coarsen,
                            distribution = medDispersed,
                            sample = medDispersedSamp,
                            id = paste0("dispersed_", round(cellStats(medDispersed, stat = "mean"),2)))

medDispersedStats <- do.call("rbind", medDispersedStats); medDispersedStats

medDispersedStats$ddc[is.na(medDispersedStats$ddc)] <- 0

medDispersedStatsMeans <- means_df <- aggregate(. ~ id + fact, data = medDispersedStats, FUN = mean); medDispersedStatsMeans

# common clustered species
commonClustered <- simulateDistribution(prevalence = 0.5, gridSize = 80, phi = 0.1)

commonClusteredSamp <- lapply(rep(0.1,10),
                            simulateSample,
                            rho = 0.07, 
                            distribution = commonClustered)

commonClusteredStats <- lapply(c(1,2,4,8,16),
                            FUN = coarsen,
                            distribution = commonClustered,
                            sample = commonClusteredSamp,
                            id = paste0("clustered_", round(cellStats(commonClustered, stat = "mean"),2)))

commonClusteredStats <- do.call("rbind", commonClusteredStats); commonClusteredStats

commonClusteredStats$ddc[is.na(commonClusteredStats$ddc)] <- 0

commonClusteredStatsMeans <- means_df <- aggregate(. ~ id + fact, data = commonClusteredStats, FUN = mean); commonClusteredStatsMeans

# common dispersed species
commonDispersed <- simulateDistribution(prevalence = 0.5, gridSize = 80, phi = 0.6)

commonDispersedSamp <- lapply(rep(0.1,10),
                            simulateSample,
                            rho = 0.07, 
                            distribution = commonDispersed)

commonDispersedStats <- lapply(c(1,2,4,8,16),
                            FUN = coarsen,
                            distribution = commonDispersed,
                            sample = commonDispersedSamp,
                            id = paste0("dispersed_", round(cellStats(commonDispersed, stat = "mean"),2)))

commonDispersedStats <- do.call("rbind", commonDispersedStats); commonDispersedStats

commonDispersedStats$ddc[is.na(commonDispersedStats$ddc)] <- 0

commonDispersedStatsMeans <- means_df <- aggregate(. ~ id + fact, data = commonDispersedStats, FUN = mean); commonDispersedStatsMeans

## combine results
stats <- rbind(rareClusteredStatsMeans,
               rareDispersedStatsMeans,
               medClusteredStatsMeans,
               medDispersedStatsMeans,
               commonClusteredStatsMeans,
               commonDispersedStatsMeans)

#png("occ-area.png", width = 5, height = 5, units = "in", res = 500)
p_occ <- ggplot(data = stats, aes(x = fact, y = p_dist, colour = id)) +
  geom_line() +
  theme_linedraw() +
  geom_point() +
  labs(x = "Grain size",
       y = expression(paste(bar(Y)[N])),
       colour = "") +
  theme(legend.position = "none") +
  ggtitle("(A) Mean occupancy") +
  scale_x_continuous(breaks = c(1,2,4,8,16)) 
#dev.off()

#png("f-area.png", width = 5, height = 5, units = "in", res = 500)
p_f <- ggplot(data = stats, aes(x = fact, y = p_sample, colour = id)) +
  geom_line() +
  theme_linedraw() +
  geom_point() +
  labs(x = "Grain size",
       y = "f",
       colour = "") +
  theme(legend.position = "none") +
  ggtitle("(B) Sampling rate")+
  scale_x_continuous(breaks = c(1,2,4,8,16)) 
#dev.off()

#png("sigma-area.png", width = 5, height = 5, units = "in", res = 500)
p_sigma <- ggplot(data = stats, aes(x = fact, y = sd_dist, colour = id)) +
  geom_line() +
  theme_linedraw() +
  geom_point() +
  labs(x = "Grain size",
       y = expression(paste(sigma[N])),
       colour = "") +
  theme(legend.position = "none") +
  ggtitle("(C) Problem difficulty")+
  scale_x_continuous(breaks = c(1,2,4,8,16)) 

#dev.off()

#png("error-area.png", width = 5, height = 5, units = "in", res = 500)
p_error <- ggplot(data = stats, aes(x = fact, y = error, colour = id)) +
  geom_line() +
  theme_linedraw() +
  geom_point() +
  labs(x = "Grain size",
       y = expression(paste("Y"[n], "-", "Y"[N])),
       colour = "") +

    theme(legend.position = "none") +
  ggtitle("(E) Absolute error") +
  scale_x_continuous(breaks = c(1,2,4,8,16)) 
#dev.off()

p_relError <- ggplot(data = stats, aes(x = fact, y = rel_error, colour = id)) +
  geom_line() +
  theme_linedraw() +
  geom_point() +
  labs(x = "Grain size",
       y = "%",
       colour = "") +
  
  theme(legend.position = "none") +
  ggtitle("(F) Relative error") +
  scale_x_continuous(breaks = c(1,2,4,8,16))

p_ddc <- ggplot(data = stats, aes(x = fact, y = ddc, colour = id)) +
  geom_line() +
  theme_linedraw() +
  geom_point() +
  labs(x = "Grain size",
       y = expression(paste(rho, "(R,Y)")),
       colour = "") +
  theme(legend.position = "none") +
  ggtitle("(D) Data quality") +
  scale_x_continuous(breaks = c(1,2,4,8,16))

png("all.png", width = 8, height = 5, units = "in", res = 500)
grid.arrange(p_occ,p_f,p_sigma,p_ddc,p_error,p_relError,ncol=3)
dev.off()

