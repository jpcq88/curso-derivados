## -----------------------------------------------------------------------------
#
# Taller 3: Derivados Financieros
# Juan Pablo Calle Quintero
# jpcalleq@unal.edu.co
# Última modificación: 21 de noviembre de 2015
# 
# Puede descragar este código en el siguiente enlace:
# https://github.com/jpcq88/curso-derivados/tree/master/taller3
#
## -----------------------------------------------------------------------------

## Librerías requeridas --------------------------------------------------------

library(fOptions) # valoración de opciones
library(ggplot2) # gráficos de calidad
library(gridExtra) # herramientas adicionales para graficar, grid.arrange()
library(data.table) # manipulación de datos
library(RQuantLib) # funciones varias de finanzas cuantitativas
library(moments) # calcular sesgo y kurtosis


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

ret_crit <- function(x, v.crit) {
  # función para identificar los valores críticos
  tmp <- vector(length = length(x))
  for (i in seq_along(tmp)) {
    if (x[i] < -abs(v.crit)) {
      tmp[i] <- '< val crit'
    } else if (x[i] > abs(v.crit)) {
      tmp[i] <- '> val crit'
    } else {
      tmp[i] <- 'x'
    }
  }
  return(tmp)
}


## Lectura de datos ------------------------------------------------------------

sp500_idx <- fread('../datos/datos_sp500.csv', header = TRUE)
sp500_idx[, fecha := as.Date(fecha, format = '%Y-%m-%d')]
sp500_idx
sp500_opt <- fread('../datos/datos_sp500_opt.csv', header = TRUE)
sp500_opt
strike <- read.csv('../datos/strike.csv', header = TRUE)


## Análisis descriptivo --------------------------------------------------------

# Gráfico opción sobre el índice S&P500
precio_opt <- ggplot(data = sp500_opt) + theme_bw(12) +
  geom_line(aes(x = 1:length(sp500_opt), y = sp500_opt)) +
  labs(x = '',
       y = 'Precio Opción S&P500')

# Gráfico índice S&P500
precio_idx <- ggplot(data = sp500_idx) + theme_bw(12) +
  geom_line(aes(x = fecha, y = sp500)) +
  labs(x = '',
       y = 'Precio Índice S&P500')

# Guardamos gráficos en formato pdf
pdf(file = '../img/graficos_series.pdf', width = 7, height = 5)

grid.arrange(precio_idx, precio_opt, nrow = 1, ncol = 2)

dev.off()


## Punto 1 ---------------------------------------------------------------------

# a)

# del 4 de noviembre al tercer viernes de diciembre (18/12/2015) hay 44 días
# El precio de cierre del índice S&P500 el 4 de niviembre era 2102.31
# El precio de ejercicio es 2110.20

vol_imp <- function(option_value) {
  iv <- EuropeanOptionImpliedVolatility(type = 'call', value = option_value,
                                  underlying = 2102.31, strike = 2110.20,
                                  riskFreeRate = 0.05, dividendYield = 0,
                                  maturity = Tmat, volatility = 0.25)
  return(iv[1])
}

Tmat <- as.numeric(difftime("2015-12-18","2015-11-04")) / 365.24
EuropeanOptionImpliedVolatility(type = 'call', value = 153.65,
                                underlying = 2102.31, strike = 2110.20,
                                riskFreeRate = 0.05, dividendYield = 0,
                                maturity = Tmat, volatility = 0.25)[1]
vol_imp(153.24)
imp_vol <- sapply(sp500_opt[1:43, sp500_opt], vol_imp)
precio_opt <- sp500_opt[1:43, sp500_opt]

dat_tmp <- data.frame(strike$strike, imp_vol)

pdf('../img/p1a_grafico.pdf', width = 5, height = 5)
ggplot(data = dat_tmp) + theme_bw(12) +
  geom_line(aes(x = strike.strike, y = imp_vol)) +
  labs(x = 'Strike', y = 'Volatilidad implícita')
dev.off()

# b)

# Calcular los log-retornos del índice S&P500, quedan almacenados en sp500_r
sp500_idx[, sp500_r := c(NA, # se pierde el primer dato al diferenciar
                        log(sp500[2:.N]) - log(sp500[1:(.N - 1)]))]
sp500_idx

# Cálculo volatilidad diaria de los log-retornos del índice S&P500
(mu_ret_sp500_dia <- sp500_idx[, mean(sp500_r, na.rm = TRUE)])
(sd_ret_sp500_dia <- sp500_idx[, sd(sp500_r, na.rm = TRUE)])
sd_ret_sp500_dia * sqrt(252)
val_critico <- 3 * sd_ret_sp500_dia

ret_2a <- sp500_idx[(.N - 2 * 252):.N, .(fecha, sp500_r)]

ret_2a[, val_crit := ret_crit(sp500_r, val_critico)]

ret_2a[, table(val_crit)] # frecuencia
ret_2a[, prop.table(table(val_crit))] # proporciones

ret_2a[, col_val_crit := 'black'] # definir colores del gráfico
ret_2a[val_crit == '< val crit', col_val_crit := 'red']
ret_2a[val_crit == '> val crit', col_val_crit := 'blue']

# Guardar gráfico en pdf
pdf('../img/p1b_grafico.pdf', width = 5, height = 5)
ggplot(data = ret_2a) + theme_bw(12) +
  geom_line(aes(x = fecha, y = sp500_r)) +
  geom_point(aes(x = fecha, y = sp500_r), colour = ret_2a[, col_val_crit]) +
  geom_hline(yintercept = c(-val_critico, val_critico), linetype = 2) +
  labs(y = 'log-retornos índice S&P500')
dev.off()

# c)

curve(dlnorm(x, 0, 0.25), xlim = c(0, 3), ylim = c(0, 2))
curve(dlnorm(x, 0, 0.5), add = TRUE)


## Punto 2 ---------------------------------------------------------------------

# a)

K1 <- 25 # long call strike
K2 <- 30 # short call strike
S0 <- 32 # precio spot del subyacente
sigma <- 0.3 # volatilidad anual: 30 \%
tasa_lr <- 0.05 # tasa anual libre de riesgos: 5 \%
tiempo_venc <- 0.5 # tiempo al vencimiento: 6 meses

precio_k1 <- GBSOption(TypeFlag = 'c',
                       S = S0,
                       X = K1,
                       Time = tiempo_venc,
                       r = tasa_lr,
                       b = tasa_lr,
                       sigma = sigma)@price

precio_k2 <- GBSOption(TypeFlag = 'c',
                       S = S0,
                       X = K2,
                       Time = tiempo_venc,
                       r = tasa_lr,
                       b = tasa_lr,
                       sigma = sigma)@price

(bull_spred <- (1 / (K2 - K1)) * (precio_k1 - precio_k2))

# b)

delta_k1 <- GBSGreeks(Selection = 'delta',
                      TypeFlag = 'c',
                      S = S0,
                      X = K1,
                      Time = tiempo_venc,
                      r = tasa_lr,
                      b = tasa_lr,
                      sigma = sigma) # $\Delta_{K1}=0.9174$

delta_k2 <- GBSGreeks(Selection = 'delta',
                      TypeFlag = 'c',
                      S = S0,
                      X = K2,
                      Time = tiempo_venc,
                      r = tasa_lr,
                      b = tasa_lr,
                      sigma = sigma) # $\Delta_{K2}=0.7013$

# $\sigma_{\Delta P} = \sigma_S \sqrt{\delta t} \times S_0 \times \delta$
sigma_p <- sigma * S0 * (1 / sqrt(2))  * (delta_k2 + delta_k2) # en 6 meses

nro_opciones <- 1000
cuantil_99 <- qnorm(0.01) # -2.3263

# VaR 99 % del portafolio bull spread para 1000 opciones
VaR_p <- - nro_opciones * cuantil_99 * sigma_p

# c)

gamma_k1 <- GBSGreeks(Selection = 'gamma',
                      TypeFlag = 'c',
                      S = S0,
                      X = K1,
                      Time = tiempo_venc,
                      r = tasa_lr,
                      b = tasa_lr,
                      sigma = sigma) # $\gamma_{K1} = 0.0224$

gamma_k2 <- GBSGreeks(Selection = 'gamma',
                      TypeFlag = 'c',
                      S = S0,
                      X = K2,
                      Time = tiempo_venc,
                      r = tasa_lr,
                      b = tasa_lr,
                      sigma = sigma) # $\gamma_{K2} = 0.0511$

sim_mod_lin <- function(S, delta, mu = 0, sd = 1) {
  # simula una realización del modelo lineal para $\Delta P$:
  # $\Delta P = S \delta \Delta x$
  
  rnd <- rnorm(1, mu, sd)
  delta_p <- S * delta * rnd
  
  return(delta_p)
}

sim_mod_cuad <- function(S, delta, gamma, mu = 0, sd = 1) {
  # simula una realización del modelo cuadrático para $\Delta P$:
  # $\Delta P = S \delta \Delta x + \frac{1}{2} S^2 \gamma (\Delta x)^2$
  
  rnd <- rnorm(1, mu, sd)
  delta_p <- S * delta * rnd + 0.5 * S^2 * gamma * rnd^2
  
  return(delta_p)
}

# sim_mod_cuad2 <- function(n, S, delta, gamma, mu = 0, sd =1, r, K, tpo) {
#   # simulación montecarlo del modelo cuadrático para $\Delta P$:
#   # $\Delta P = S \delta \Delta x + \frac{1}{2} S^2 \gamma (\Delta x)^2$
#   # n: número de réplicas
#   
#   delta_p <- vector(length = n)
#   
#   S_tmp <- S
#   delta_tmp <- delta
#   gamma_tmp <- gamma
#   
#   delta_p[1] <- sim_mod_cuad(S_tmp, sum(delta_tmp), sum(gamma_tmp), mu, sd)
#   
#   for (i in 2:n) {
#     S_tmp <- S_tmp * rnorm(1, mu, sd)
#     
#     delta_tmp <- GBSGreeks(Selection = 'delta',
#                            TypeFlag = 'c',
#                            S = S_tmp,
#                            X = K[1],
#                            Time = tpo,
#                            r = r,
#                            b = r,
#                            sigma = sd)[1] +
#       GBSGreeks(Selection = 'delta',
#                 TypeFlag = 'c',
#                 S = S_tmp,
#                 X = K[2],
#                 Time = tpo,
#                 r = r,
#                 b = r,
#                 sigma = sd)[1]
#     gamma_tmp <- GBSGreeks(Selection = 'gamma',
#                            TypeFlag = 'c',
#                            S = S_tmp,
#                            X = K[1],
#                            Time = tpo,
#                            r = r,
#                            b = r,
#                            sigma = sd)[1] +
#       GBSGreeks(Selection = 'gamma',
#                 TypeFlag = 'c',
#                 S = S_tmp,
#                 X = K[2],
#                 Time = tpo,
#                 r = r,
#                 b = r,
#                 sigma = sd)[1]
#     
#     delta_p[i] <- sim_mod_cuad(S_tmp, delta_tmp, gamma_tmp, mu, sd)
#     
#   }
#   
#   return(delta_p)
# }
# 
# sim_mod_cuad2(10, S0, c(delta_k1, delta_k2), c(gamma_k1, gamma_k2),
#               mu = 0, sd = sigma * sqrt(0.5), tasa_lr, c(K1, K2), tiempo_venc)

# Para hacer el código reproducible fijemos la semilla del generador de
# números aleatorios.
set.seed(123)

# simulación de los dos dos modelos con 10000 réplicas
sim_c <- replicate(10000,
                   sim_mod_cuad(S0, delta_k1 + delta_k2, gamma_k1 + gamma_k2,
                                mu = 0, sd = sigma * sqrt(0.5)))
sim_l <- replicate(10000,
                   sim_mod_lin(S0, delta_k1 + delta_k2,
                               mu = 0, sd = sigma * sqrt(0.5)))

sim <- data.table(sim_c, sim_l)

# media, desviación, kurtosis y sesgo de $\Delta P$
momentos <- sim[, .(med = mean(sim_c),
                    sd = sd(sim_c),
                    kurt = kurtosis(sim_c) - 3,
                    skw = skewness(sim_c))]

k <- momentos[, kurt]
s <- momentos[, skw]

cuantil_cf_99 <- cuantil_99 +
  (cuantil_99^2 - 1)*s/6 -
  (2*cuantil_99^3 - 5*cuantil_99)*(s^2)/36 +
  (cuantil_99^3 - 3*cuantil_99)*k/24 # teórico

cuantil_m_99 <- quantile(sim[, sim_c], 0.01) # muestral

VaR_cf <- - nro_opciones * cuantil_cf_99 * momentos[, sd]

pdf('../img/p3c_densidades.pdf', width = 5, height = 5)
# Comparación de las densidades
ggplot(data = sim) + theme_bw(12) +
  geom_density(aes(x = sim_c), colour = 'blue') +
  geom_density(aes(x = sim_l), colour = 'black') +
  geom_vline(xintercept = -VaR_p/1000, colour = 'black') +
  geom_vline(xintercept = -VaR_cf/1000, colour = 'blue') +
  xlim(sim[, min(sim)] - 20, 80) +
  labs(x = expression(paste(Delta, ' P')), y = 'densidad')
dev.off()

