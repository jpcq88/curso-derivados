################################################################################
#
# Taller 2B: Derivados
#
################################################################################


tpo <- 4 # semestres
nper <- 1 # semestral
sigma <- 0.09 # semestral
S_0 <- 100
R <- 0.04 # tasa 4 % efectiva semestre
u <- exp(sigma * sqrt(tpo / nper))
d <- 1 / u
p <- (1 + R - d) / (u - d)
K <- 98

(S_4u0d <- S_0 * u^4 * d^0)
(S_3u0d <- S_0 * u^3 * d^0)
(S_2u0d <- S_0 * u^2 * d^0)
(S_1u0d <- S_0 * u^1 * d^0)
(S_0u1d <- S_0 * u^0 * d^1)
(S_0u2d <- S_0 * u^0 * d^2)
(S_0u3d <- S_0 * u^0 * d^3)
(S_0u4d <- S_0 * u^0 * d^4)
(S_1u1d <- S_0 * u^1 * d^1)

(S_3u1d <- S_0 * u^3 * d^1)

C_t4 <- c(
  max(S_0 * d^0 * u^4 - K, 0),
  max(S_0 * d^1 * u^3 - K, 0),
  max(S_0 * d^2 * u^2 - K, 0),
  max(S_0 * d^3 * u^1 - K, 0),
  max(S_0 * d^4 * u^0 - K, 0)
)
C_t4

C_t3 <- c(
  max((p*C_t4[1] + (1 - p)*C_t4[2]) / (1 + R), 0),
  max((p*C_t4[2] + (1 - p)*C_t4[3]) / (1 + R), 0),
  max((p*C_t4[3] + (1 - p)*C_t4[4]) / (1 + R), 0),
  max((p*C_t4[4] + (1 - p)*C_t4[5]) / (1 + R), 0)
)
C_t3

C_t2 <- c(
  max((p*C_t3[1] + (1 - p)*C_t3[2]) / (1 + R), 0),
  max((p*C_t3[2] + (1 - p)*C_t3[3]) / (1 + R), 0),
  max((p*C_t3[3] + (1 - p)*C_t3[4]) / (1 + R), 0)
)
C_t2

C_t1 <- c(
  max((p*C_t2[1] + (1 - p)*C_t2[2]) / (1 + R), 0),
  max((p*C_t2[2] + (1 - p)*C_t2[3]) / (1 + R), 0)
)
C_t1

C_t0 <- max((p*C_t1[1] + (1 - p)*C_t1[2]) / (1 + R), 0)
C_t0

crr_tree <- cbind(
  c(C_t0, 0, 0, 0, 0),
  c(C_t1, 0, 0, 0),
  c(C_t2, 0, 0),
  c(C_t3, 0),
  C_t4
)
crr_tree

# Punto 1B
(delta_t3 <- (crr_tree[1, 5] - crr_tree[2, 5]) / (S_0*(u - d)))
(B_t3 <- (u*crr_tree[2, 5] - d*crr_tree[1, 5]) / ((1 + R)*(u - d)))

(delta_t2 <- (crr_tree[1, 4] - crr_tree[2, 4]) / (S_0*(u - d)))
(B_t2 <- (u*crr_tree[2, 4] - d*crr_tree[1, 4]) / ((1 + R)*(u - d)))

(delta_t1 <- (crr_tree[1, 3] - crr_tree[2, 3]) / (S_0*(u - d)))
(B_t1 <- (u*crr_tree[2, 3] - d*crr_tree[1, 3]) / ((1 + R)*(u - d)))

(delta_t0 <- (crr_tree[1, 2] - crr_tree[2, 2]) / (S_0*(u - d)))
(B_t0 <- (u*crr_tree[2, 2] - d*crr_tree[1, 2]) / ((1 + R)*(u - d)))

(p*C_t1[1] + (1-p)*C_t1[2])/((1+R))
(p*C_t2[1] + (1-p)*C_t2[2])/((1+R))
(p*C_t3[1] + (1-p)*C_t3[2])/((1+R))


# Punto 1D
sum(
  ifelse(S_0 * u^(0:4) * d^(4 - 0:4) >= K,
         S_0 * u^(0:4) * d^(4 - 0:4) - K,
         0) * dbinom(0:4, 4, p)
  ) / (1 + R)^4

sum(
  ifelse(S_1u0d * u^(0:3) * d^(3 - 0:3) >= K,
         S_1u0d * u^(0:3) * d^(3 - 0:3) - K,
         0) * dbinom(0:3, 3, p)
  ) / (1 + R)^3

sum(
  ifelse(S_0u1d * u^(0:3) * d^(3 - 0:3) >= K,
         S_0u1d * u^(0:3) * d^(3 - 0:3) - K,
         0) * dbinom(0:3, 3, p)
  ) / (1 + R)^3

sum(
  ifelse(S_2u0d * u^(0:2) * d^(2 - 0:2) >= K,
         S_2u0d * u^(0:2) * d^(2 - 0:2) - K,
         0) * dbinom(0:2, 2, p)
  ) / (1 + R)^2

sum(
  ifelse(S_1u1d * u^(0:2) * d^(2 - 0:2) >= K,
         S_1u1d * u^(0:2) * d^(2 - 0:2) - K,
         0) * dbinom(0:2, 2, p)
  ) / (1 + R)^2

sum(
  ifelse(S_0u2d * u^(0:2) * d^(2 - 0:2) >= K,
         S_0u2d * u^(0:2) * d^(2 - 0:2) - K,
         0) * dbinom(0:2, 2, p)
  ) / (1 + R)^2
