
library(deSolve)
library(ggplot2)
library(tidyr)
library(dplyr)

ses_fishery <- function(t, state, parms) {
  with(as.list(c(state, parms)), {
    
    C <- min(max(C, 0.0001), N)  # avoid division by zero
    S <- max(S, 0)
    
    D <- N - C
    
#Open-access escapement
    S_hat <- c / (p * q)
    
#Cooperative harvest (quota)
    Q <- theta * S
    Hc <- Q * (C/N)  # total cooperative harvest
    
#implied effort (not directly used in biomass eq)
    eC <- theta / C
    
#Defector harvest (open access)
    if (S > S_hat && D > 0) {
      Hd <- D*(S - S_hat)/N 
      eD <- Hd / (D * S)
    } else {
      Hd <- 0
      eD <- 0
    }
    
#Ecological dynamics
    dS <- r * S * (1 - S/K) * (S/A - 1) -
      Hc - Hd
    
#Social dynamics 
    dC <- a * C * (N - C) -
      b * C * (eD / (eC + 1e-8))
    
    list(c(dC, dS))
  })
}

parms <- c(
  r = 0.3, #growthrate
  K = 100, #carrying capacity
  A = 10, #allee threshold
  theta = 0.15, #percent stock quota
  a = 0.5, #Social pressure (to conform)
  b = 0.9, #temptation to defect
  N = 100, #Total amount of fishers
  p = 10, #price of fish
  q = 0.05, #catchability coefficient
  c = 0.001 #cost of fishing
)
    
# Initial state
state <- c(
  C = 99,
  S = 50
)

# Time
times <- seq(0, 10, by = 0.1)

# Run model
out <- ode(state, times, ses_fishery, parms)
out <- as.data.frame(out)

# Add defectors
out$D <- parms["N"] - out$C

out_long <- out %>%
  select(time, C, D, S) %>%
  pivot_longer(cols = c(C, D, S),
               names_to = "Variable",
               values_to = "Value")

ggplot(out_long, aes(x = time, y = Value, color = Variable)) +
  geom_line(size = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Social–Ecological Fishery Dynamics",
    x = "Time",
    y = "Population / Biomass",
    color = ""
  ) +
  scale_color_manual(values = c(
    "C" = "#1b9e77",
    "D" = "#d95f02",
    "S" = "#7570b3"
  )) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )
