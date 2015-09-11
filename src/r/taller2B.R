################################################################################
#
# Taller 2B: Derivados
#
################################################################################

library(fOptions)

# S: Precio actual del activo
# X: Precio de ejercicio (K)
# Time: Tiempo de vencimiento en años
# r: Tasa de interes anual
# b: ?? por ahora poner el mismo valor de r
# sigma: Volatilidad anualizada del activo subyacente
# n: Número de periodos

# Punto 4A
crr_tree <- BinomialTreeOption(TypeFlag = 'ce', S = 1932.39, X = 2000,
                               Time = 1, r = 0.05, b = 0.05, sigma = 0.1387,
                               n = 4)

BinomialTreePlot(crr_tree, dy = 1, cex = 0.8, ylim = c(-5, 5), xlab = '',
                 ylab = 'Valor de la Opción', bty = 'n', xaxt = 'n', yaxt = 'n')
axis(side = 1, at = 1:5,
     labels = c('t = 0', 't = 1', 't = 2', 't = 3', 't = 4'))

tpo <- 1 # año
nper <- 4
sigma <- 0.1387
S_0 <- 1932.39
# u <- 1.0720
# d <- 0.9330
u <- exp(sigma * sqrt(tpo / nper))
d <- 1 / u
p <- (exp(R*tpo/nper) - d) / (u - d)
R <- 0.05
K <- 2000

C_t4 <- c(
  max(S_0 * u^4 - K, 0),
  max(S_0 * d * u^3 - K, 0),
  max(S_0 * d^2 * u^2 - K, 0),
  max(S_0 * d^3 * u - K, 0),
  max(S_0 * d^4 - K, 0)
)
C_t4

C_t3 <- c(
  max((p*C_t4[1] + (1 - p)*C_t4[2])*exp(-R*tpo/nper), 0),
  max((p*C_t4[2] + (1 - p)*C_t4[3])*exp(-R*tpo/nper), 0),
  max((p*C_t4[3] + (1 - p)*C_t4[4])*exp(-R*tpo/nper), 0),
  max((p*C_t4[4] + (1 - p)*C_t4[5])*exp(-R*tpo/nper), 0)
)
C_t3

C_t2 <- c(
  max((p*C_t3[1] + (1 - p)*C_t3[2])*exp(-R*tpo/nper), 0),
  max((p*C_t3[2] + (1 - p)*C_t3[3])*exp(-R*tpo/nper), 0),
  max((p*C_t3[3] + (1 - p)*C_t3[4])*exp(-R*tpo/nper), 0)
)
C_t2

C_t1 <- c(
  max((p*C_t2[1] + (1 - p)*C_t2[2])*exp(-R*tpo/nper), 0),
  max((p*C_t2[2] + (1 - p)*C_t2[3])*exp(-R*tpo/nper), 0)
)
C_t1

C_t0 <- max((p*C_t1[1] + (1 - p)*C_t1[2])*exp(-R*tpo/nper), 0)
C_t0

# Punto 4B
(delta_t3 <- (crr_tree[1, 5] - crr_tree[2, 5]) / (S_0*(u - d)))
(B_t3 <- (u*crr_tree[2, 5] - d*crr_tree[1, 5]) / ((1 + R)*(u - d)))
(B_t3 <- (u*crr_tree[2, 5] - d*crr_tree[1, 5]) / (exp(R)*(u - d)))

(delta_t2 <- (crr_tree[1, 4] - crr_tree[2, 4]) / (S_0*(u - d)))
(B_t2 <- (u*crr_tree[2, 4] - d*crr_tree[1, 4]) / ((1 + R)*(u - d)))
(B_t2 <- (u*crr_tree[2, 4] - d*crr_tree[1, 4]) / (exp(R)*(u - d)))

(delta_t1 <- (crr_tree[1, 3] - crr_tree[2, 3]) / (S_0*(u - d)))
(B_t1 <- (u*crr_tree[2, 3] - d*crr_tree[1, 3]) / ((1 + R)*(u - d)))
(B_t1 <- (u*crr_tree[2, 3] - d*crr_tree[1, 3]) / (exp(R)*(u - d)))

(delta_t0 <- (crr_tree[1, 2] - crr_tree[2, 2]) / (S_0*(u - d)))
(B_t0 <- (u*crr_tree[2, 2] - d*crr_tree[1, 2]) / ((1 + R)*(u - d)))
(B_t0 <- (u*crr_tree[2, 2] - d*crr_tree[1, 2]) / (exp(R)*(u - d)))


# Punto 4C

crr_tree3 <- BinomialTreeOption(TypeFlag = 'ce', S = 2071.16, X = 2000,
                                Time = 1/4, r = 0.05, b = 0.05, sigma = 0.1387,
                                n = 1)

BinomialTreePlot(crr_tree3, dy = 1, dx = 0.005, cex = 1, ylim = c(-2, 2),
                 xlab = '', ylab = 'Valor de la Opción', bty = 'n', xaxt = 'n',
                 yaxt = 'n', xlim = c(0.9, 2.1))
axis(side = 1, at = 1:2,
     labels = c('t = 3', 't = 4'))


S_0m <- 2071.16
C_t4_m <- c(
  max(S_0m * u - K, 0),
  max(S_0m * d - K, 0)
)
C_t4_m

max((p*C_t4_m[1] + (1 - p)*C_t4_m[2])*exp(-R*tpo/nper), 0)
