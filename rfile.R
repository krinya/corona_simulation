library(data.table)
library(tidyr)
library(ggplot2)
library(gganimate)
library(ggrepel)
theme_set(theme_bw())

#initial parameters

# Number of days
t_max <- 100
# To create 1000 steps? So 10 for each day? To make the curve more smooth?
dt <- 0.1
#
t <- seq(from = 0, to = t_max, by = dt)

#alpha: infection rate (0-1)
#beta: contact rate (persons)
#gamma: recovery rate (0-1)
#rho: social distancing factor (0 everyone, 1 base situation)
#dr: death rate

alpha = 0.2
beta = 1.75
gamma = 0.5
rho = 0.0
dr <- 0.05

N <- 10000

# Normalized population values
S_0 <- 1-1/N
E_0 <- 1/N
I_0 <- 0
R_0 <- 0
D_0 <- 0

create_data <- function(){
  # Create a data.table with t0 values
  dataSimulation <- data.table(t = t[1],
                             S = S_0,
                             E = E_0,
                             I = I_0,
                             R = R_0,
                             D = D_0)

  for (i in t[2:length(t)]){
    
    print(i)
    
    last_S <- dataSimulation[nrow(dataSimulation), S]
    last_E <- dataSimulation[nrow(dataSimulation), E]
    last_I <- dataSimulation[nrow(dataSimulation), I]
    last_R <- dataSimulation[nrow(dataSimulation), R]
    last_D <- dataSimulation[nrow(dataSimulation), D]
    
    if(rho == 0){
      
      next_S <- last_S - (beta * last_S * last_I) * dt
      next_E <- last_E + (beta * last_S * last_I - alpha * last_E) * dt
      
    } else {
      
      next_S <- last_S - (rho * beta * last_S * last_I) * dt
      next_E <- last_E + (rho * beta * last_S * last_I - alpha*last_E) * dt
    }
    
    next_I <- last_I + (alpha * last_E - gamma * last_I - dr * last_I) * dt
    next_R <- last_R + (gamma * last_I) * dt
    next_D <- last_D + (dr * last_I) * dt
    
    insertRow <- data.table(t = i, S = next_S, E = next_E, I = next_I, R = next_R, D = next_D)
    
    l = list(dataSimulation, insertRow)
    
    dataSimulation <- rbindlist(l, fill = T)
    
    #check
    dataSimulation[, population:= S + E + I + R + D]
    
  }
  
  return(dataSimulation)
  
}

###
dataSimulation <- create_data()

### Plot values
longData <- gather(dataSimulation, type, value, S:population, factor_key=TRUE)
setDT(longData)

ggplot(longData, aes(x = t, y = value)) +
  geom_line(aes(color = type), size = 1) +
  geom_text_repel(data = longData[t == 50], aes(x = t , y = value ,label = round(value, 2)))

ggplot(longData, aes(x = t, y = value)) +
  geom_line(aes(color = type), size = 1) +
  transition_reveal(t)


