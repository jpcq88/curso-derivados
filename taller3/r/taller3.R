## -----------------------------------------------------------------------------
#
# Taller 3: Derivados Financieros
# Juan Pablo Calle Quintero
# jpcalleq@unal.edu.co
# Última modificación: 16 de noviembre de 2015
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
library(GUIDE) # otras funciones para valorar opciones


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

(bull_spred <- precio_k1 - precio_k2)
bullspreadcalls()

# Para hacer el código reproducible fijemos la semilla del generador de
# números aleatorios.
set.seed(123)
