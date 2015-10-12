## -----------------------------------------------------------------------------
#
# Taller 2: Derivados financieros
# Juan Pablo Calle Quintero
#
## -----------------------------------------------------------------------------

## Librerías requeridas --------------------------------------------------------

library(fGarch) # estimación de modelos GARCH
library(fOptions) # valoración de opciones
library(ggplot2) # gráficos de calidad
library(gridExtra) # herramientas adicionales para graficar, grid.arrange()
library(data.table) # manipulación de datos


## Definición de funciones -----------------------------------------------------

# gŕafico en blanco, a veces útil para usar con la función grid.arrange()
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

sim_rw <- function(n, S0, mu = 0.15, sigma = 0.25, dt = 0.01) {
  # Simulación de caminata aleatoria
  
  St <- vector(mode = 'numeric', length = n)
  St[1] <- S0
  
  for (i in 2:n) {
    St[i] <- St[i - 1] * (1 + mu * dt + sigma * rnorm(1) * sqrt(dt))
  }
  
  return(St)
}


## Lectura de datos ------------------------------------------------------------

datos_p1 <- fread('../datos/datos_p1.csv')
datos_p1[, Date := as.Date(Date, format = '%d/%m/%Y')]
datos_p1

datos_p2 <- fread('../datos/datos_p2.csv')
datos_p2[, Date := as.Date(Date, format = '%d/%m/%Y')]
datos_p2


## Análisis descriptivo --------------------------------------------------------

ggplot(data = datos_p1) + theme_bw(12) +
  geom_line(aes(x = Date, y = sp500)) +
  labs(x = '',
       y = 'S&P500')

p2_base <- ggplot(data = datos_p2, aes(x = Date)) + theme_bw(12)

p2_sp500 <- p2_base + geom_line(aes(y = sp500)) +
  labs(x = '',
       y = 'Futuro S&P500 3 meses')

p2_att <- p2_base + geom_line(aes(y = att)) +
  labs(x = '',
       y = 'AT&T Corp.')

p2_msoft <- p2_base + geom_line(aes(y = msoft)) +
  labs(x = '',
       y = 'Microsoft Corp.')

grid.arrange(p2_sp500, blankPlot, p2_att, p2_msoft, ncol = 2, nrow = 2)


## Punto 1 ---------------------------------------------------------------------

datos_p1[, sp500_r := c(NA, # se pierde el primer dato al diferenciar
                        log(sp500[2:.N]) - log(sp500[1:(.N - 1)]))]
datos_p1

# a)

# set.seed(123) # fijar semilla generador de números aleatorios (reproducir ejm)
K <- 2000
sigma_op <- 0.1
r_op <- 0.05 
S0 <- 1930
trading_days <- 252
year_days <- 365
mu_day <- datos_p1[2:.N, mean(sp500_r)]
sigma_day <- datos_p1[2:.N, sd(sp500_r)]
simul_st <- sim_rw(trading_days, S0 = S0,
                   mu = mu_day, sigma = sigma_day, dt = 1)
plot(simul_st, type = 'l')
nro_replicas <- 100
simul_precios <- replicate(nro_replicas,
                           sim_rw(n = trading_days, S0 = S0,
                                  mu = mu_day, sigma = sigma_day, dt = 1))
ts.plot(simul_precios, lwd = 0.5)
abline(h = K, col = 'red', lty = 2)

val_esp_St_K <- mean(pmax(simul_precios[nro_replicas, ] - K, 0))
(C_St <- exp(-r_op * trading_days) * val_esp_St_K)

simul_st[trading_days]
exp(-r_op * 1)

# b)

GBSOption(TypeFlag = 'c', S = 1930, X = 2000, Time = 1, r = 0.05, b = 0.05,
          sigma = 0.1)

GBSOption(TypeFlag = 'c', S = 1930, X = simul_st[trading_days],
          Time = 1, r = 0.05, b = 0.05, sigma = 0.1)

# c)

ggplot(data = datos_p1, aes(x = Date, y = sp500_r)) + theme_bw(12) +
  geom_line() +
  geom_hline(yintercept = datos_p1[, mean(sp500_r, na.rm = TRUE)],
             colour = 'red', linetype = 2) +
  labs(x = '',
       y = 'log retornos S&P500')

datos_p1[.N]
garch11 <- garchFit(formula = ~garch(1, 1), data = datos_p1[2:.N, sp500_r])
predict(garch11, n.ahead = 3, mse = 'cond')
predict(garch11, n.ahead = 10, mse = 'cond', plot = TRUE)
garch11@fit
garch11@sigma.t
plot(garch11@sigma.t, type = 'l')
omega <- garch11@fit$matcoef[1, 1]
alpha1 <- garch11@fit$matcoef[2, 1]
beta1 <- garch11@fit$matcoef[3, 1]

sigma2_1 <- omega + alpha1 * garch11@residuals[nrow(datos_p1) - 1] ^ 2 +
  beta1 * garch11@h.t[nrow(datos_p1) - 1]
sqrt(sigma2_1) # pronóstico volatilidad diaria estimada un día adelante

sigma2_2 <- omega + (alpha1 + beta1) * sigma2_1
sqrt(sigma2_2) # pronóstico volatilidad diaria estimada dos días adelante

sigma2_3 <- omega + (alpha1 + beta1) * sigma2_2
sqrt(sigma2_3) # pronóstico volatilidad estimada tres días adelante (05/10/2015)


## Punto 2 ---------------------------------------------------------------------

acciones_att <- 2000
acciones_msoft <- 1000
prop_acciones_att <- acciones_att / (acciones_msoft + acciones_att)
prop_acciones_msoft <- 1 - prop_acciones_att

datos_p2[, sp500_r := c(NA, # se pierde el primer dato al diferenciar
                        log(sp500[2:.N]) - log(sp500[1:(.N - 1)]))]
datos_p2[, att_r := c(NA, # se pierde el primer dato al diferenciar
                      log(att[2:.N]) - log(att[1:(.N - 1)]))]
datos_p2[, msoft_r := c(NA, # se pierde el primer dato al diferenciar
                        log(msoft[2:.N]) - log(msoft[1:(.N - 1)]))]
datos_p2[, port_r := c(NA, msoft_r[2:.N] * prop_acciones_msoft +
                         att_r[2:.N] * prop_acciones_att)]
datos_p2

reg <- lm(sp500_r ~ port_r, data = datos_p2)
summary(reg)

ggplot(data = datos_p2, aes(x = port_r, y = sp500_r)) + theme_bw(12) +
  geom_point(size = 1.5) +
  stat_smooth(method = 'lm', na.rm = TRUE, fill = 'gray', alpha = 0.2)
