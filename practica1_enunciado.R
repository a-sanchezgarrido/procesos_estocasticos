
# PRÁCTICA 1: ANÁLISIS Y SIMULACIÓN DE CADENAS DE MARKOV

# 1. Creación y representación de objetos tipo cadena de Markov
library("markovchain")

'Para una cadena de Markov hace falta especificar el espacio de estados y la matriz de transición entre los estados'

estados <- c("0", "1", "2", "3", "4")
P <- rbind(c(1, 0, 0, 0, 0),
           c(1/2, 0, 1/2, 0, 0),
           c(0, 1/2, 0, 1/2, 0),
           c(0, 0, 1/2, 0, 1/2),
           c(0, 0, 0, 0, 1))
mc <- new("markovchain", states = estados, transitionMatrix = P, name = "Ruina del jugador")

print(mc) # para ver la cadena de Markov

'Para representar la cadena como un grafo usamos:'
set.seed(1234)
plot(mc)

'Para un análisis general, identificar clases irreducibles o clasificar estados:'
summary(mc)
'Obtenemos que hay 3 clases irreducibles: {0}, {4} y {1,2,3}.  También nos informa de que las clases A y B son cerradas. 
 Una clase es cerrada cuando, una vez que la cadena alcanza algún estado de dicha clase ya no puede salir de ella.
'

# 2.Probabilidades de transición
'Podemos seleccionar las probabilidades desde un estado a los demás:'
conditionalDistribution(mc, "3")

'Si queremos saber lo que pasa en 2 jugadas'
conditionalDistribution(mc*mc, "3")

'Recordemos que el producto matricial en R se obtiene usando el operardor %*%. Por ejemplo, para obtener
la matriz de transición en dos pasos, que sabemos que viene dada por P**2, haremos:'
P %*% P
show(mc^2)  # otra forma

conditionalDistribution(mc^5, "3")
'Vemos que en 5 jugadas esa es la probabilidad de estar en cada estado, ahora la matriz de transición en 5 pasos:'
library(expm)
P %^% 5
show(mc^5)  # otra

conditionalDistribution(mc^20, "3")

# 3. Probabilidad de absorción y tiempo medio de absorción
'Podemos calcular la probabilidad de absorción por estados absorbentes:'
absorptionProbabilities(mc)
'     0    4
 1 0.75 0.25
 2 0.50 0.50  --> Si partimos de 3 euros hay 1/4 de probabilidad de llegar a 0
 3 0.25 0.75'


'Tiempo medio de absorción'
meanAbsorptionTime(mc)
'1 2 3  
 3 4 3    ->  tenemos una riqueza de 2 euros de media tardaremos 4 lanzamientos de moneda hasta que acabe el juego
'

# 4. Simulación
'Se pueden simular trayectorias, aqui empezamos en 2€ de ganancia y simulamos 30 jugadas'

set.seed(111)
X0 <- "2"   # Si no le pasamos t0 = x0, coge un punto aleatorio
trayectoria <- rmarkovchain(n = 30, object = mc, t0 = X0)

plot(0:30, c(X0, trayectoria), type = "l", lty = 1, col="blue", ylim=c(0,4))  # gráfica

'Podemos simular varias trayectorias:'
set.seed(111)
npasos <- 30
trayectoria <- rmarkovchain(n = npasos, object = mc, t0=X0)
colores <- rainbow(5)
plot(0:npasos, c(X0, trayectoria), type = "l", lty = 1, col=colores[1], ylim=c(0, 4))
for(i in 2:5){
  trayectoria <- rmarkovchain(n = npasos, object = mc, t0=X0)
  lines(0:npasos, c(X0, trayectoria), col=colores[i])
}

# 5. Distribuciones estacionarias y límite
estados <- c("soleado", "nublado", "lluvioso")
P <- rbind(c(0.7, 0.2, 0.1),
           c(0.3, 0.4, 0.3),
           c(0.2, 0.45, 0.35))
mc.tiempo <- new("markovchain", states = estados, transitionMatrix = P, name = "Tiempo atmosférico")

set.seed(123)
plot(mc.tiempo)

'Quiero ver con qué frecuencia se da cada tiempo atmosférico,
 vimos que la distribución límite existe si la cadena es irreducible 
 y aperiódica.'

is.irreducible(mc.tiempo)
# [1] TRUE
period(mc.tiempo)
# [1] 1

'La distribución límite es estacionaria, la calculamos:'
steadyStates(mc.tiempo)
'Concluimos que, en el largo plazo, el 46.36% de los días serán soleados, el 31.81% de los 
 días estará nublado, y el 21,82% de los días será lluviosos.'

'La distribución límite coincide con las filas de la matriz 
 de transición en n pasos, cuando n tiende a infinito'
P %^% 3
P %^% 5
P %^% 10
P %^% 30  # a partir de 30 ya son iguales
P %^% 50

'Simulo el tiempo durante 1000 días seguidos:'
set.seed(1111)
trayectoria <- rmarkovchain(n = 1000, object = mc.tiempo)
trayectoria[1:30]

'Hacemos una tabla con las frecuencias absolutas:'
tabla <- table(trayectoria)
tabla <- tabla[estados]
tabla

barplot(tabla, xlab="Numero de dias") # Gráfico de barras

tabla / 1000  # frecuencias relativas

steadyStates(mc.tiempo) # se parecen las frecuencias relativas

'Si aumentamos el número de pasos a 1000, la aproximación es mejor:'
set.seed(123)
trayectoria <- rmarkovchain(n = 10000, object = mc.tiempo)
table(trayectoria)[estados] / 10000

# 6. Ejemplos de aplicación
# 6.1 Cálculo de PageRank
'• A tiene enlaces a C, D, y E.
 • B tiene enlace a E.
 • C tiene enlace a B y D.
 • D itene enalce a B.
 • E tiene B y C.
 De esta manera, tenemos la matriz de transición:
 P =  0 0 1/3 1/3 1/3
      0 0   0   0 1
      0 1/2 0 1/2 0
      0 1   0   0 0
      0 1/2 1/2 0 0 
'
# Definimos los estadosP
estados <- c("A","B", "C", "D", "E")
# Definimos la matriz de transición
P <- rbind(c(0, 0, 1/3, 1/3, 1/3),
           c(0, 0, 0, 0, 1),
           c(0, 1/2, 0, 1/2, 0),
           c(0, 1, 0, 0, 0),
           c(0, 1/2, 1/2, 0, 0))
# Definimos el objeto de tipo cadena de Markov
mc.pageRank <- new("markovchain", states = estados, transitionMatrix = P, name = "PageRank")

set.seed(122)
plot(mc.pageRank)     # veamos su aspecto

summary(mc.pageRank)  # para ver clases irreducibles y clasificar estados
'Tiene dos clases: {A} transitoria y {B,C,D,E} recurrentes'

'Probabilidad de que el estado transitorio alcance cada estado recurrente'
absorptionProbabilities(mc.pageRank)

'y el tiempo medio para que el estado transitorio sea absorbido'
meanAbsorptionTime(mc.pageRank)

'Vemos que la cadena no es irreducible, por lo que la distribución límite podría no existir.'

'Para conseguir una
 cadena irreducible vamos a considerar un factor de amortiguación d=0.85. Por lo que modificamos la cadena
 de Markov con el siguiente código:'
d <- 0.85
n <- 5
M <- d*P + (1-d)*(1/n)*matrix(1, n, n)
# Redefinimos la cadena teniendo en cuenta la nueva matriz de transición
mc.pageRank <- new("markovchain", states = estados, transitionMatrix = M, name = "Pagerank")

is.irreducible(mc.pageRank) # Irreducible y aperiódica
period(mc.pageRank)

steadyStates(mc.pageRank) # Al ser irreducible y aperiódica la distribución límite existe
'Obtenemos los pageRanks:
 πA = 0.03, πB = 0.3446, πC = 0.1793, πD = 0.1147, πE = 0.3313.'

# 6.2 Finanzas: riesgo de crédito
'Los ratings de crédito son evaluaciones que indican solvencia o capacidad de una entidad 
 para cumplir con sus obligaciones financieras. Una de las escalas de clasificación es:
 AAA | Riesgo mínimo, AA | Riesgo muy bajo, ..., CCC | Riesgo alto, D | Quiebra
 '
rc <- c("AAA", "AA", "A", "BBB", "BB", "B", "CCC", "D")
P <- rbind(c(90.81, 8.33, 0.68, 0.06, 0.08, 0.02, 0.01, 0.01),
           c(0.70, 90.65, 7.79, 0.64, 0.06, 0.13, 0.02, 0.01),
           c(0.09, 2.27, 91.05, 5.52, 0.74, 0.26, 0.01, 0.06),
           c(0.02, 0.33, 5.95, 85.93, 5.30, 1.17, 1.12, 0.18),
           c(0.03, 0.14, 0.67, 7.73, 80.53, 8.84, 1.00, 1.06),
           c(0.01, 0.11, 0.24, 0.43, 6.48, 83.46, 4.07, 5.20),
           c(0.21, 0, 0.22, 1.30, 2.38, 11.24, 64.86, 19.79),
           c(0, 0, 0, 0, 0, 0, 0, 100))/100
ratings <- new("markovchain", states = rc, transitionMatrix = P, name = "Rating")

summary(ratings)  # análisis previo 
'Dos clases irreducibles, {D} y {AAA,AA,A,BBB,BB,B,CCC}, absorbente y transitorio respectivamente'

conditionalDistribution(ratings, "BBB") # empezamos desde BBB, prob en un año
conditionalDistribution(ratings^20, "BBB")  # dentro de 20 años
meanAbsorptionTime(ratings) # tiempo medio para quiebra

'La cadena no es irreducible, y a largo plazo la cadena se va al estado absorbente.
 Es decir, la distribución límite es inmediata, todos los estados transitorios 
 tienen prob 0 y el estado absorbente probabilidad 1'
is.irreducible(ratings)
steadyStates(ratings)










































