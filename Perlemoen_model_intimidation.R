perlemoen_fishery <- function(t, state, parms) {
  with(as.list(c(state, parms)), {
    
    C <- min(max(C, 0.0001), N)  
    S <- max(S, 0)
    D <- N - C

    #harvest 
    Hc <- eC * C * S
    Hd <- eD * D * S
    
    #Ecological dynamics
    dS <- r * S * (1 - S/K) * (S/A - 1) -
      Hc - Hd
    
    #Social dynamics 
    dC <- a * C * (D) - #*(1-S/K), could be fun
          i * C * (D) -
          b * C * (eD / (eC + 1e-8))
    
    list(c(dC, dS))
  })
}

parms <- c(
  r = 0.44, #growthrate
  K = 100, #carrying capacity
  A = 10, #allee threshold (should be around 10% of K)
  eC = 0.002, #effort of cooperators (per cooperator)
  eD = 0.01, #effort of poachers (per defector)
  a = 0.06, #Social pressure (to conform)
  i = 0.061, #intimidation (social pressure to defect)
  b = 0.09, #temptation to defect
  N = 100 #Total amount of fishers
)

# Initial state
state <- c(
  C = 99,
  S = 50
)

# Time
times <- seq(0, 10, by = 0.1)

# Run model
out <- ode(state, times, perlemoen_fishery, parms)
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
    title = "H. midae Fishery Dynamics",
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
