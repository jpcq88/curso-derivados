## -----------------------------------------------------------------------------
#
# Taller 4: Derivados Financieros
# Juan Pablo Calle Quintero
# jpcalleq@unal.edu.co
# Última modificación: 27 de noviembre de 2015
# 
# Puede descragar este código en el siguiente enlace:
# https://github.com/jpcq88/curso-derivados/tree/master/taller4
#
## -----------------------------------------------------------------------------

## Librerías requeridas --------------------------------------------------------

library(fOptions) # valorar opciones
library(rootSolve) # hallar la reíces de una función


## Definición de funciones -----------------------------------------------------

# Gráfico en blanco, a veces útil para usar con la función grid.arrange()
blankPlot <- ggplot() + geom_blank(aes(1, 1)) +
  theme(
    plot.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  )

valor_futuro <- function(vp, tasa, tiempo) {
  # Calcula el valor futuro con interés continuo
  # vp: valor presente
  # tasa: tasa de interés anual de composición continua
  # tiempo: tiempo en años
  
  vf <- vp * exp(tasa * tiempo)
  
  return(vf)
}

valor_presente <- function(vf, tasa, tiempo) {
  # Calcula el valor presente con interés continuo
  # vf: valor futuro
  # tasa: tasa de interés anual de composición continua
  # tiempo: tiempo en años
  
  vp <- vf * exp(-tasa * tiempo)
  
  return(vp)
}

tasa_int_c <- function(vp, vf, tiempo) {
  # Calcula la tasa de interés compuesta continuamente
  # vp: valor presente
  # vf: valor futuro
  # tiempo: tiempo en años
  
  tasa <- log(vf / vp) / tiempo
  
  return(tasa)
}


## Punto 1 ---------------------------------------------------------------------

# a)

Principal <- 100
tpo <- c(0.5, 1, 1.5, 2)
cupon <- c(0, 0, 6.2, 8)
Precio <- c(98, 95, 101, 104)

tasa_cero <- tasa_int_c(Precio, Principal, tpo) # solo es válido para 0 cupón

tasa_cero[3] <- log((Precio[3] - (cupon[3] / 2) * exp(-tasa_cero[1] * tpo[1]) -
                       (cupon[3] / 2) * exp(-tasa_cero[2] * tpo[2])) /
                      (Principal + (cupon[3] / 2))) / -tpo[3]

tasa_cero[4] <- log((Precio[4] - (cupon[4] / 2) * exp(-tasa_cero[1] * tpo[1]) -
                       (cupon[4] / 2) * exp(-tasa_cero[2] * tpo[2]) -
                       (cupon[4] / 2) * exp(-tasa_cero[3] * tpo[3])) /
                      (Principal + (cupon[4] / 2))) / -tpo[4]

# b)

f_t_T <- diff(tasa_cero * tpo) / diff(tpo)

# c)

# Precio
P_0_2 <- 175 * exp(-tpo[1] * tasa_cero[1]) + 175 * exp(-tpo[2] * tasa_cero[2]) +
  175 * exp(-tpo[3] * tasa_cero[3]) + 5175 * exp(-tpo[4] * tasa_cero[4])

# Rendimiento

yield <- function(r) {
  yld <- 175 * exp(-0.5 * r) + 175 * exp(-1 * r) +
    175 * exp(-1.5 * r) + 5175 * exp(-2 * r) - 5106.45
  
  return(yld)
}

multiroot(yield, start = 0.5)$root


## Punto 2 ---------------------------------------------------------------------

rho <- 0.2 # correlación cópula
tao <- rexp(1, rate = 1 / 30) # rate = 1 / mu

# Gráfico de la distribución de \tau
pdf('../img/p2_dist_exp.pdf', width = 7, height = 5)
curve(dexp(x, rate = 1 / 30), from = 0, to = 120, ylim = c(0, 0.04),
      #main = expression(paste('Distribución exponencial, ', mu, ' = 30 días')),
      xlab = expression(tau), ylab = expression(f(tau)),
      bty = 'n', yaxt = 'n', xaxt = 'n', cex.lab = 1
      )
box(col = 'grey', lwd = 1.5)
axis(1, col = 'grey', col.ticks = 'grey', lwd = 1.5, cex.axis = 0.8)
axis(2, at = c(0, 0.01, 0.02, 0.03, 0.04), col = 'grey', cex.axis = 0.8,
     labels = c('0 %', '1 %', '2 %', '3 %', '4 %'), lwd = 1.5, las = 1)
dev.off()

Tpo <- 30 # tiempo en días
prob_def <- 0.02 # probabilidad de default en 30 días
Qt <- pexp(Tpo, 1 / 30)
Nt <- qnorm(Qt)
Nx <- qnorm(0.99)
pnorm((Nt + sqrt(rho) * Nx) / sqrt(1 - rho)) * 1000000


## Punto 3 ---------------------------------------------------------------------

# No requiere programación


# Punto 4 ----------------------------------------------------------------------

S0 <- 30000
K <- 10000
tpo <- 4
lambda <- -0.1
sigma <- 0.15
mu <- -0.25

r <- -(lambda * sigma - mu)
precio_esp <- S0 * exp(r * tpo)

d1 <- (log(precio_esp / K) + sigma^2 * tpo / 2)/(sigma * sqrt(tpo))
d2 <- (log(precio_esp / K) - sigma^2 * tpo / 2)/(sigma * sqrt(tpo))

phi1 <- pnorm(d1)
phi2 <- pnorm(d2)

(C_T <- exp(-0.06 * tpo) * (precio_esp * phi1 - K * phi2))

