# --- Permutaciones y Combinaciones en R ---

# Funciones ----
Permutaciones_sin <- function(n, r){
  return(factorial(n)/factorial(n-r))
}
Permutaciones_repe <- function(n, r){
  return(n^r)
}

Combinacion <- function(n, r){
  return(factorial(n)/(factorial(n-r)*factorial(r)))
}
Combinacion_repe <- function(n, r){
  return (factorial(n + r - 1)) / (factorial(r) * factorial(n - 1))
}

# EJERCICIO 4A ----
permu_a <- gtools::permutations(11, 3)
print(nrow(permu_a))

Permutaciones_repeticion <- gtools::permutations(11, 3, repeats = TRUE)
print(nrow(Permutaciones_repeticion))

# EJERCICIO 4B ----
Combinacion_letra_sinrepe <- nrow(gtools::combinations(5, 3, letters[1:5], repeats.allowed = FALSE))
print(Combinacion_letra_sinrepe)

Combinacion_letra_repe <- nrow(gtools::combinations(5, 3, letters[1:5], repeats.allowed = TRUE))
print(Combinacion_letra_repe)

# EJERCICIO 4C ----
print(Permutaciones_sin(39, 35))
print(Combinacion(39, 25))

# PROBLEMA VENDEDORA VIAJERA ----
n <- 50
total_rutas <- factorial(n-1)
rutas_optimas <- 1/total_rutas

cat("El total de rutas es: ", format(total_rutas, scientific=TRUE), "\n")
cat("La probabilidad de ruta óptima es: ", format(rutas_optimas, scientific=TRUE), "\n")

# PROBLEMA HIPERGEOMÉTRICA ----
N <- 40    # población
K <- 18    # rechazantes
n <- 2     # muestra
k <- 2     # exactamente 2 rechazantes

comb_K_k <- choose(K, k)           # C(18,2)
comb_rest <- choose(N - K, n - k)  # C(22,0)
comb_total <- choose(N, n)         # C(40,2)

prob_manual <- (comb_K_k * comb_rest) / comb_total

cat("C(18,2) =", comb_K_k, "\n")
cat("C(22,0) =", comb_rest, "\n")
cat("C(40,2) =", comb_total, "\n")
cat("Probabilidad (manual) =", prob_manual, "\n")
