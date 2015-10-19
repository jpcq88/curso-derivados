## -----------------------------------------------------------------------------
#
# Taller 2: Derivados Financieros
# Juan Pablo Calle Quintero
# jpcalleq@unal.edu.co
# Última modificación: 19 de octubre de 2015
# 
# Puede descragar este código en el siguiente enlace:
# https://github.com/jpcq88/curso-derivados/blob/master/taller2/r/taller2.R
#
## -----------------------------------------------------------------------------

## Librerías requeridas --------------------------------------------------------

library(fGarch) # estimación de modelos GARCH
library(fOptions) # valoración de opciones
library(ggplot2) # gráficos de calidad
library(gridExtra) # herramientas adicionales para graficar, grid.arrange()
library(data.table) # manipulación de datos


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

sim_st <- function(tpo, S0, mu = 0.15, sigma = 0.25, r = mu, dt = 0.01) {
  # Simulación precio St de un activo
  # po: tiempo en años
  # S0: precio inicial del activo
  # mu: retorno esperado del activo anualizado
  # sigma: volatilidad de los retornos del activo anualizado
  # r: tasa libre de riesgo anualizada
  # dt: time step (debe ser menor o igual a tpo)
  
  steps <- tpo / dt
  St <- vector(mode = 'numeric', length = steps)
  St[1] <- S0
  
  for (i in 2:steps) {
    
    # $S_i=S_{i-1} e^{(r-\frac{1}{2}\sigma^2)dt + \sigma\sqrt{dt}\phi_i}$
    St[i] <-
      St[i - 1] * exp((r - 0.5 * sigma ^ 2)*dt + sigma * sqrt(dt) * rnorm(1))
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

# Gráfico índice S&P500
ggplot(data = datos_p1) + theme_bw(12) +
  geom_line(aes(x = Date, y = sp500)) +
  labs(x = '',
       y = 'S&P500')

p2_base <- ggplot(data = datos_p2, aes(x = Date)) + theme_bw(12)

# Gráfico del precio del futuro a 3 meses sobre el índice S&P500
p2_sp500 <- p2_base + geom_line(aes(y = sp500)) +
  labs(x = '',
       y = 'Futuro S&P500 3 meses')

# Gráfico precio acción AT&T
p2_att <- p2_base + geom_line(aes(y = att)) +
  labs(x = '',
       y = 'AT&T Corp.')

# Gráfico precio acción Microsoft
p2_msoft <- p2_base + geom_line(aes(y = msoft)) +
  labs(x = '',
       y = 'Microsoft Corp.')

# Los tres gráfcos del punto 2
grid.arrange(p2_sp500, blankPlot, p2_att, p2_msoft, ncol = 2, nrow = 2)


## Punto 1 ---------------------------------------------------------------------

# Calcular los log-retornos del índice S&P500, quedan almacenados en sp500_r
datos_p1[, sp500_r := c(NA, # se pierde el primer dato al diferenciar
                        log(sp500[2:.N]) - log(sp500[1:(.N - 1)]))]
datos_p1

# a)

# Para hacer el código reproducible fijemos la semilla del generador de
# números aleatorios.
set.seed(123)

# Definamos parámetros del modelo
tpo_al_venc <- 1 # en años
K <- 2000 # precio acordado al vencimiento
sigma_op <- 0.1 # volatilidad de la opción (anualizada)
r_op <- 0.05 # tasa libre de riesgo para valoración de la oción (anualizada)
S0 <- 1930 # precio a vista del subyacente
trading_days <- 252 # número de días de negociación en un año en EEUU
year_days <- 365 # número de días en un año

# Los siguientes parámetros son los calculados con base en los datos históricos
mu_day <- datos_p1[, mean(sp500_r, na.rm = TRUE)] # retorno promedio diario
mu_ann <- exp(mu_day * trading_days) - 1 # retorno anualizado
sigma_day <- datos_p1[, sd(sp500_r, na.rm = TRUE)] # volatilidad diaria promedio
sigma_ann <- sigma_day * sqrt(trading_days) # volatilidad anualizada

# simular una realización del precio de la acción
simul_st <- sim_st(tpo_al_venc, S0 = S0,
                   mu = mu_ann, sigma = sigma_op, # usando sigma de la opción
                   r = r_op, dt = tpo_al_venc / year_days)

# Graficar una realización del precio de la acción
# Guarda el gráfico en la carpeta img con el nombre p1_simul_precio.pdf
pdf(file = '../img/p1_simul_precio.pdf', width = 5, height = 4)

opar <- par()
par(mar = c(2, 4, 1, 1))
plot(simul_st, type = 'l',
     xlab = '',
     ylab = 'Precio de la acción ',
     xlim = c(0, year_days + 40), cex.lab = 0.8, cex.axis = 0.6)
abline(h = K, lty = 2, col = 'red')
abline(h = S0, lty = 2, col = 'red')
text(labels = paste('ST =', round(simul_st[year_days])),
     cex = 0.6, col = 'red', pos = 3,
     x = year_days + 20, y = simul_st[year_days])
text(labels = paste('K =', round(K, 2)),
     cex = 0.6, col = 'red', pos = 3,
     x = year_days + 20, y = K)
text(labels = paste('S0 =', round(S0, 2)),
     cex = 0.6, col = 'red', pos = 3,
     x = year_days + 20, y = S0)
par(opar)

dev.off()

nro_replicas <- 1000 # número de réplicas (muestras) simulación munte carlo

# Simalar 1000 posibles realizaciones del precio de la acción
simul_precios <- replicate(nro_replicas,
                           sim_st(tpo_al_venc, S0 = S0,
                                 mu = mu_ann, sigma = sigma_op,
                                 r = r_op, dt = tpo_al_venc / year_days))

# Graficar 1000 realizaciones del precio de la acción
# Guarda el gráfico en la carpeta img con el nombre p1_simul_precios.pdf
pdf(file = '../img/p1_simul_precios.pdf', width = 5, height = 4)

opar <- par()
par(mar = c(2, 4, 1, 1), cex.lab = 0.8, cex.axis = 0.6)
ts.plot(simul_precios, lwd = 0.2,
        xlab = '', ylab = 'Precio de la acción', xlim = c(0, year_days + 40))
abline(h = K, col = 'red', lty = 2)
abline(h = S0, lty = 2, col = 'red')
text(labels = paste('K =', round(K, 2)),
     cex = 0.6, col = 'red', pos = 3,
     x = year_days + 30, y = K)
text(labels = paste('S0 =', round(S0, 2)),
     cex = 0.6, col = 'red', pos = 1,
     x = year_days + 30, y = S0)
par(opar)

dev.off()

# Valor esperado de $max((S_T - K)^+, 0)$
val_esp_St_K <- mean(pmax(simul_precios[year_days, ] - K, 0))

# Valor de la opción obtenido con la simulación
(C_St <- exp(-r_op * 1) * val_esp_St_K)

# b)

# Valor de la opción usando la fórmula de Black-Scholes
GBSOption(TypeFlag = 'c', S = 1930, X = 2000, Time = 1, r = 0.05, b = 0.05,
          sigma = 0.1)@price

# Valor de la opción con Black-Scholes calculado manualmaente
sigma_op_day <- sigma_op / sqrt(trading_days)
r_op_day <- exp(-r_op * trading_days)
tpo_al_venc_day <- tpo_al_venc * 365
d1 <- ((r_op + 0.5 * (sigma_op ^ 2)) * tpo_al_venc + log(S0 / K)) /
  (sigma_op * sqrt(tpo_al_venc))
d2 <- ((r_op - 0.5 * (sigma_op ^ 2)) * tpo_al_venc + log(S0 / K)) /
  (sigma_op * sqrt(tpo_al_venc))
(C_bs <- pnorm(d1) * S0 - pnorm(d2) * K * exp(-r_op * tpo_al_venc))

# c)

# gráfico log-retornos índice S&P500
ggplot(data = datos_p1, aes(x = Date, y = sp500_r)) + theme_bw(12) +
  geom_line() +
  geom_hline(yintercept = datos_p1[, mean(sp500_r, na.rm = TRUE)],
             colour = 'red', linetype = 2) +
  labs(x = '',
       y = 'log retornos S&P500')

datos_p1[.N] # la última fecha es el 2 de octubre de 2015

# Ajuste modelo garch(1,1) para los log-retornos del índice S&P500
garch11 <- garchFit(formula = ~garch(1, 1), data = datos_p1[2:.N, sp500_r])

garch11@fit # parámetros estimados
garch11@sigma.t # varianzas estimadas

# Gráfico de volatilidad estimada
pdf(file = '../img/p1c_sigma_est.pdf', width = 2.5, height = 1.5)

opar <- par()
par(mar = c(1, 2, 1, 1), cex.lab = 0.4, cex.axis = 0.4)
plot(sqrt(garch11@sigma.t),
     type = 'l', xlab = '', ylab = '', bty = 'n', xaxt = 'n',
     ylim = c(0.06, 0.16), tcl = -0.2, lwd = 0.6)
par(opar)

dev.off()

# Extraer los valores de los parámetros estimados
omega <- garch11@fit$matcoef[1, 1]
alpha1 <- garch11@fit$matcoef[2, 1]
beta1 <- garch11@fit$matcoef[3, 1]

# Estimación $\sigma^2$ para el 3 de octubre (un día adelante)
sigma2_1 <- omega + alpha1 * garch11@residuals[nrow(datos_p1) - 1] ^ 2 +
  beta1 * garch11@h.t[nrow(datos_p1) - 1]
sqrt(sigma2_1) # pronóstico volatilidad diaria estimada un día adelante

# Estimación $\sigma^2$ para el 4 de octubre (dos días adelante)
sigma2_2 <- omega + (alpha1 + beta1) * sigma2_1
sqrt(sigma2_2) # pronóstico volatilidad diaria estimada dos días adelante

# Estimación $\sigma^2$ para el 5 de octubre (tres días adelante)
sigma2_3 <- omega + (alpha1 + beta1) * sigma2_2
sqrt(sigma2_3) # pronóstico volatilidad estimada tres días adelante (05/10/2015)
sqrt(sigma2_3)*sqrt(trading_days) # anualizada


## Punto 2 ---------------------------------------------------------------------

# Definición de parámetros del portafolio
acciones_att <- 2000 # número de acciones de AT&T
acciones_msoft <- 1000 # número de acciones de Microsoft

# Calcular los $\Delta_P$ (port_d) y $\Delta_F$ (sp500_d)
# Diferencias futuro 3 meses índice S&P500
datos_p2[, sp500_d := c(NA, # se pierde el primer dato al diferenciar
                        sp500[2:.N] - sp500[1:(.N - 1)])]
# Diferencias acción AT&T
datos_p2[, att_d := c(NA, # se pierde el primer dato al diferenciar
                      att[2:.N] - att[1:(.N - 1)])]
# Diferencias acción Microsoft
datos_p2[, msoft_d := c(NA, # se pierde el primer dato al diferenciar
                        msoft[2:.N] - msoft[1:(.N - 1)])]
# Valor del portafolio
datos_p2[, port := .(msoft[1:.N] * acciones_msoft +
                       att[1:.N] * acciones_att)]
# Diferencias valor del portafolio
datos_p2[, port_d := c(NA, port[2:.N] - port[1:(.N - 1)])]
datos_p2

# a)

# Ajuste regresión $\Delta S = \beta_0 + \beta_1 \Delta F + \epsilon_t$
reg <- lm(port_d ~ sp500_d, data = datos_p2)
summary(reg)

# Coeficiente de correlación $\rho$
rho <- sqrt(summary(reg)$r.squared)

# Radio óptimo de cobertura $h^*$
vol_port <- datos_p2[, sd(port, na.rm = TRUE)] # volatilidad portafolio
vol_sp500 <- datos_p2[, sd(sp500, na.rm = TRUE)] # volatilidad futuro S&P500
rad_cob <- rho * (vol_port / vol_sp500)

# b)

# Número óptimo de contratos $N^*$
vlr_port <- datos_p2[.N, port]
vlr_fut <- 250 * datos_p2[.N, sp500]
nro_cont <- rad_cob * (vlr_port / vlr_fut)

# Gráfico de regresión
pdf(file = '../img/p2_reg.pdf', width = 5, height = 4)

ggplot(data = datos_p2, aes(x = sp500_d, y = port_d)) + theme_bw(9) +
  geom_point(size = 1.5) +
  stat_smooth(method = 'lm', na.rm = TRUE, fill = 'gray', alpha = 0.2) +
  labs(x = 'Delta S&P500',
       y = 'Delta portafolio AT&T y Microsoft Corp.') +
  annotate('text', label = paste('rho ==', round(rho, 4)),
           x = -100, y = 10000, parse = TRUE, size = 4)

dev.off()

# El mismo gráfico pero más pequeño para poner al margen
pdf(file = '../img/p2_reg_margen.pdf', width = 2.5, height = 1.5)

ggplot(data = datos_p2, aes(x = sp500_d, y = port_d)) + theme_bw(5) +
  geom_point(size = 1) +
  stat_smooth(method = 'lm', na.rm = TRUE, fill = 'gray', alpha = 0.2) +
  labs(x = 'Delta S&P500',
       y = 'Delta portafolio') +
  annotate('text', label = paste('rho ==', round(rho, 4)),
           x = -100, y = 10000, parse = TRUE, size = 1.5) +
  theme(axis.ticks = element_blank())

dev.off()

# c)

# Estimaciones de la media y desviación estándar del portafolio $\Delta P$
mu_port <- datos_p2[, mean(port_d, na.rm = TRUE)]
sigma_port <- datos_p2[, sd(port_d, na.rm = TRUE)]

# Cálculo valor del portafolio $\Delta X = \Delta P - N^* \Delta F$
datos_p2[, x_d := .(port_d - nro_cont * sp500_d)]

# Estimaciones de la media y desviación estándar del portafolio $\Delta X$
mu_xd <- datos_p2[, mean(x_d, na.rm = TRUE)]
sigma_xd <- datos_p2[, sd(x_d, na.rm = TRUE)]

nobs_simul <- 1000 # número de simulaciones

# Simulamos los portafolios y los guardamos
dat_simul <- data.table(serie = rep(c('dP', 'dX'), each = nobs_simul),
                        sim = c(
                          rnorm(nobs_simul, mean = mu_port, sd = sigma_port),
                          rnorm(nobs_simul, mean = mu_xd, sd = sigma_xd))
                        )
dat_simul

# Gráfico de las densidades de los portafolios $\Delta P$ y $\Delta X$
pdf(file = '../img/p2_densidad.pdf', width = 6, height = 4)

ggplot(data = dat_simul, aes(colour = serie)) + theme_bw(10) +
  geom_density(aes(x = sim)) +
  labs(x = 'Valor del portafolio',
       y = 'Densidad') +
  theme(legend.title = element_blank())

dev.off()
