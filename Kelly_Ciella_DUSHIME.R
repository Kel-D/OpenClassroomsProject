#EXERCICE 1


#Données
X = c(1051, 1337, 1389, 1921, 1942, 2322, 3629, 4006, 4012, 4063, 
      4921, 5445, 5620, 5817, 5905, 5956, 6068, 6121, 6473, 7501,
      7886, 8108, 8546, 8666, 8831, 9106, 9711, 9806, 10205,10396,
      10861,11026,11214,11362,11604,11608,11745,11762,11895,12044,
      13520,13670,14110,14496,15395,16179,17092,17568,17568 )

n = 49 #Taille des données

#vecteurs contenant les valeurs possibles de theta
tt = seq(8000, 12000, length = 100)

# Fonction de densité de Weibull
weibull_density <- function(t) {
  2 * X * exp(-((X / t) ^ 2)) / t ^ 2
}


#Tracé de la densité empirique et théorique

plot(density(X), main = 'Densité théorique et empirique', col = 'blue')
lines(X, weibull_density(seq(8000, 12000, length = n)), col = 'red')
legend("topright", legend = c("Densité empirique", "Densité théorique"), col = c("blue", "red"), lty = c(1, 1))


#Fonction de vraisemblance
L = function(t){
  return ((2 / t^2)^n * exp(sum(log(X)) - (sum(X^2) / t^2)))
}

#Tracé du graphique de la fonction de vraisemblance
plot(tt, L(tt), type='l')

#Fonction de log-vraisemblance
lnL = function(t){
  return( n * log(2 / t^2) + sum(log(X)) - sum(X^2) / t^2)
}

#Tracé du graphique de la fonction de log-vraisemblance
plot(tt, lnL(tt), type='l')

#Fonction score
U = function(t){
  return ((-2 * n / t) + ((sum(X^2) * 2) / t^3 ))
}

#Tracé du graphique de la fonction score
plot(tt, U(tt), type='l')
abline(h=0, col='blue')

#On détermine le point exact theta_hat = 9892.176812 en calculant U(t) = 0 à la main


#EXERCICE2

#On a calculé la fonction score U dans l'exercice 1

Uprime = function(t){
  return (2 * n / t^2 - 6 * sum(X^2) / t^4)
}


I = function(t){
  return (n * 4 / t^2)
}

#Mettons les 2 fonctions (U et Uprime) côte à côte pour les comparer
par(mfrow = c(1,2))
plot(tt, I(tt), type='l')
plot(tt, Uprime(tt), type='l')


#Calcul de l'EMV par la méthode de Newton
theta.hat1 = rep(NA, 100)
theta.hat1[1] = mean(X)

for(m in 2:100){
  theta.hat1[m] = theta.hat1[m-1] - U(theta.hat1[m-1]) / Uprime(theta.hat1[m-1])
}


#Calcul de l'EMV par la méthode de Newton-Raphson
theta.hat2 = rep(NA, 100)
theta.hat2[1] = mean(X)

for(m in 2:100){
  theta.hat2[m] = theta.hat2[m-1] + U(theta.hat2[m-1]) / I(theta.hat2[m-1])
}


#On voit que à partir d'un certain moment, la valeur se stabilise. 
#On obtient donc une estimation par maximum de vraisemblance pour le paramètre d'échelle
#La valeur du point de maximum obtenue est 9892.177 qui est égale à la valeur trouvée à l'exercice 1 en faisant le calcul à la main


















