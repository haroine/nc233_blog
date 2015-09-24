
## Constants
g <<- 9.81
m_bike <<- 8 ## In kg
Temp <<- 20 ## In °C
p <<- 1013 ## In hPa
phi <<- 0.50 ## In percentage
SCx <<- 0.35 ## 0.25 -> 0.40
v_wind <<- 10 ## In kph
Cr <<- 0.008 ## 0.004 -> 0.010
Cf <<- 0.0027 ## 0.0024 -> 0.0030
  
## Compute power needed to move at speed v
## given various parameters
p_gravity <- function(m, slope, v) {
  return( (m+m_bike)*g*slope/100*v )
}

p_air <- function(m, v, v_wind, rho) {
  
  va <- v + v_wind
  
  return( 0.5*rho*SCx*va**2*v )
}

p_friction <- function(m, slope, v, v_wind) {
  
  va <- v + v_wind
  
#   return( ((m+m_bike)*g*Cr*cos(atan(pi/2*slope/100)) + Cf*va**2)*v  )
  return( ((m+m_bike)*g*Cr*sqrt(1-(slope/100)**2) + Cf*va**2)*v  )
  
}

# Temp in °C
# p in hPa
# phi in percentage
air_density <- function(Temp, p, phi) {
  
  return( 1/(287.06*(Temp+273.15))*(p-230.617*phi*exp((17.5045*Temp)/(241.2+Temp))) )
  
}

## v, v_wind in kph
p_total <- function(v_kph, m, slope, v_wind_kph) {
  
  rho <- air_density(Temp, p*100, phi)
  v <- v_kph/3.6
  v_wind <- v_wind_kph/3.6
  
  return( p_gravity(m, slope,v) + p_air(m, v, v_wind, rho) + p_friction(m, slope, v, v_wind) )
  
}

## Inverse problem : compute speed corresponding to
## given power, given parameters

m <<- 70
m1 <<- 57
m2 <<- 80
slope <<- 1 ## gradient, in percentage points
v_wind_kph <<- 18

v_power <- function(power, m, slope) {
  
  inverse <- function(y){(p_total(y,m,slope,v_wind_kph) - power)}
  
  return(uniroot(inverse, c(0,100))$root)
}

## H1 : If P/m is considered constant (constant denoted Wk)
## H2 : if (P/m)**(3/4) is considered constant (constant denoted Wk34)

power_H1 <- function(Wk, m) {
  return(Wk*m)
}

power_H2 <- function(Wk34, m) {
  return(Wk34*m**0.75)
}

## TODO : compute Wk et Wk34 based on real data:
## Robert Gesink, wall of Huy: 557W, 70 kg

Wk_computed <<- 557/70
Wk34_computed <<- (557/70)**(0.75)

## Amateur rider: 250W, 70 kg

# Wk_computed <<- 250/70
# Wk34_computed <<- (250/70)**(0.75)


## Under hypotheses H1 and H2, which rider is faster:
## the heavy one or the light one?

p_solve_H1 <- function(slope) {
  
  Wk <- Wk_computed
  
  return( v_power(power_H1(Wk,m1),m1,slope) -  v_power(power_H1(Wk,m2),m2,slope))
}

p_solve_H2 <- function(slope) {
  
  Wk34 <- Wk34_computed
  
  return( v_power(power_H2(Wk34,m1),m1,slope) -  v_power(power_H2(Wk34,m2),m2,slope))
}

## Result: limit slope does not exist under H1
## Under H2, it is roughly equal to 6.1 % for pros
## and 4.9 % for amateurs

uniroot(p_solve_H1, c(0,100))
limit_slope <<- uniroot(p_solve_H2, c(0,100))$root

## Graphs

require("ggplot2")

grad.x <- seq(0,100,0.1)
data_H1 <- sapply(grad.x, p_solve_H1)
data_H2 <- sapply(grad.x, p_solve_H2)


dfBike <- data.frame(cbind(grad.x, data_H1, data_H2))

plot_bike <- ggplot(dfBike, aes(x=grad.x)) +
            geom_line(aes(y=data_H1, colour="H1")) + geom_line(aes(y=data_H2, colour="H2")) +
            geom_hline(yintercept=0, colour="blue", linetype="dotted") +
            scale_x_continuous(limits = c(0, 100), breaks=c(0,5,10,15,25,50,75,100)) + xlab("Gradient") + ylab("Speed advantage for lighter rider") +
            scale_colour_manual(values=c("H1" = "#1E7FCB","H2"="#960018"), name="Hypotheses") +
            theme_bw()
print(plot_bike)
