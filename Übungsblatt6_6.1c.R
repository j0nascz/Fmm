library(Matrix) 
library(wordspace) 
# Normalisierung der Spalten 
set.seed(0) 
# Fixiere Seed für Reproduzierbarkeit 
n <- 8 
M <- matrix(rnorm(n^2), nrow = n) 
V <- normalize.cols(M) 
# numerische Gründe, aus theoretischer Sicht nicht nötig 
L <- diag(seq(1, n)) 
A <- V %*% L %*% solve(V) 
# Zur Erzeugung von A ist die Berechnung der Inversen erlaubt