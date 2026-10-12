
library(markovchain)

# --- Problema 1 ---
estados <- c('1','2','3','4','5','6')
P <- rbind(c(0,1/4,1/4,0,1/4,1/4),
           c(0,1,0,0,0,0),
           c(1/2,0,1/2,0,0,0),
           c(0,1/3,0,1/3,1/3,0),
           c(0,0,0,1,0,0),
           c(0,0,0,0,0,1)) 
mc <- new("markovchain", states = estados, transitionMatrix = P)

# 1.Dibuja el grafo, encuentra clases irreducibles y clasifica sus estados
set.seed(123)
plot(mc)      # grafo
summary(mc)   # C_1 = {2} y C_2 = [6] recurrentes, C_3 = {1,3} y C_4 = {4,5} transitorios

# 2. Calcula prob de absorción
absorptionProbabilities(mc)
# r1,2 = 0.6667 | r3,2 = 0,6667 | r4,2 = 1 | r5,2 = 1
# r1,6 = 0.3333 | r3,6 = 0,3333 | r4,6 = 0 | r5,6 = 0

# 3. Calcula el tiempo medio de absorción por algún estado absorbente
meanAbsorptionTime(mc)
# A = {2,6} T1,A = 3,6667 | T3,A = 5,6667 | T4,A = 4 | T5,A = 5

# 4. Simula y representa cinco trayectorias empezando en un estado al azar. Indicar patrones
set.seed(111)
trayectoria <- rmarkovchain(n=30, object= mc) # Si no pone t0 = x, se coge aleatorio

colores <- rainbow(5)
plot(1:30, trayectoria, col = colores[1], type="l", ylim = c(1, 6), ylab = "Estado", xlab = "Paso")

for(i in 2:5){
  trayectoria <- rmarkovchain(n=30, object = mc)
  lines(1:30, trayectoria, col = colores[i], type="l")
}
'Patrones: da igual por donde empiecen que acaban llegando a 2 o 6, 
 no todas llegan al mismo tiempo a los estados absorbentes, pero llegan'


# --- Problema 2 ---
estados <- c('c','nc')
P <- rbind(c(0.8,0.2),
           c(0.3,0.7))
mc <- new("markovchain", states = estados, transitionMatrix = P)

# 1. Modelar a través de un grafo y determina matriz de transición - Hecho
plot(mc)

# 2. Uno compra el primer mes, prob de que lo vuelva a comprar en un año
conditionalDistribution(mc^12,'c')
# Prob('c') = 0,6000977 y Prob('nc') = 0,3999023

# 3. 1000 individuos, 100 compran el primer mes, cuantos en un mes, y en un año
c0 <- c(100,1000-100)
c1 <- c0*mc
c1    # 350 personas lo comprarán en 1 mes
c12 <- c0*mc^12
c12   # 600 personas lo comprarán en 1 año
'π^n = π^0 · P^n  ->  (Cn, NCn) = (C0, NC0) · P^n'


# --- Problema 3 ---
estados <- c('0','1','2','3','4','5','6')
P <- rbind(c(1, 0, 0, 0, 0, 0, 0),
           c(1/6, 0, 5/6, 0, 0, 0, 0),
           c(0, 2/6, 0, 4/6, 0, 0, 0),
           c(0, 0, 3/6, 0, 3/6, 0, 0),
           c(0, 0, 0, 4/6, 0, 2/6, 0),
           c(0, 0, 0, 0, 5/6, 0, 1/6),
           c(0, 0, 0, 0, 0, 1, 0))  
mc <- new("markovchain", states = estados, transitionMatrix = P)

# 1. Modelar a través de un grafo y determina matriz de transición
plot(mc)

# 2. Duración media del juego
summary(mc)
meanAbsorptionTime(mc)  # Como empieza en 4 rojas y 2 verdes miramos 4: 80,8 pasos de media


# --- Problema 4 ---
# Miro el grafo y empiezo a sacar las probabilidades:
estados <- c('A','B','C','D','E','F','G')
P <- rbind(c(0,   1/2, 0,   0,   0,   0,   1/2), # A -> B, G
           c(1/3, 0,   1/3, 1/3, 0,   0,   0),   # B -> A, C, D
           c(0,   0,   0,   1,   0,   0,   0),   # C -> D
           c(0,   1/2, 0,   0,   1/2, 0,   0),   # D -> B, E
           c(0,   0,   0,   0,   1,   0,   0),   # E -> E (El truco del profesor para el sumidero)
           c(1/3, 0,   0,   0,   1/3, 0,   1/3), # F -> A, E, G
           c(0,   0,   0,   0,   0,   1,   0))   # G -> F
d<-0.85 # factor de amortiguación 
n<-7    # número de estados
M <- d * P + (1 - d) * (1/n) * matrix(1, n, n)
mc <- new("markovchain", states = estados, transitionMatrix = M, name = "Pagerank")

plot(mc)  # Comprobamos que es el grafo que debería salir

summary(mc) # Una única clase de estados recurrentes
is.irreducible(mc)
period(mc)

steadyStates(mc) # Al ser irreducible y aperiódica la distribución límite existe

'Resultados que le han salido al profesor en la clase:
 πA = 0.0706, πB = 0.0879, πC = 0.0463, πD = 0.0857, πE = 0.5478, πF = 0.0858, πG = 0.0758
 Orden: E, B, F, D, G, A, C.
 *Tienes que meter en la fila de E, que NO manda flechas a nadie, como que se manda a sí mismo*'


# --- Problema 5 ---
# parámetros
lambda_1 <- 1/5 # tasa de fallo (horas)
lambda_2 <- 6   # tasa de reparación (10 min = 1/6 horas -> tasa = 6)
T <- 2000       # tiempo a largo plazo para ver la convergencia (horas)

set.seed(123)
tau <- NULL
i <- 1

# simulación de los tiempos de cada ciclo
while (sum(tau) < T) {
  alpha <- rexp(n = 1, rate = lambda_1)
  beta <- rexp(n = 1, rate = lambda_2)
  tau[i] <- alpha + beta
  i <- i + 1
}

# cálculo de la tasa N_t / t
tiempos <- c(0, cumsum(tau))
N <- 0:length(tau)
cociente <- N / tiempos

# representación gráfica
# type = "s" se mantiene constante | type = "S" se anticipa al siguiente punto
plot(tiempos, cociente, type = "s", 
     xlab = "Tiempo (horas)", ylab = "Tasa de renovaciones (N_t / t)", 
     xlim = c(0, T), main = "Convergencia de la tasa de renovaciones")

# línea teórica de convergencia (6/31 = 0.1935)
# por la Ley de los grandes números: N_t/t converge a 1/mu (mu es el tiempo medio que dura un ciclo)
# Exp(1/5) -> media = 1/(1/5) = 5 y tiempo de reparación 1/6 -> tiempo total (mu) = 31/6 -> 1/mu = 6/31
abline(h = 6/31, col = "blue", lwd = 2)
'Converge a 6/31 por la ley de los grandes números (explicado arriba)'


# --- Problema 6 ---
# Cargamos la librería necesaria para simular procesos gaussianos
library(MASS)

# Consideramos t en el intervalo [0,1] (creamos 100 puntos)
t <- seq(0, 1, length.out = 100)

# Calculamos la matriz de distancias h (diferencia entre puntos)
h <- abs(outer(t, t, "-"))

# Definimos el parámetro l y el núcleo de Cauchy
l <- 0.2
K <- 1 / (1 + (h/l)^2)

# Simulamos 10 trayectorias asumiendo media 0
set.seed(123)
# mvrnorm genera las simulaciones basándose en la matriz de covarianza K
trayectorias <- mvrnorm(n = 10, mu = rep(0, length(t)), Sigma = K)

# Dibujamos las 10 trayectorias a la vez con matplot
colores <- rainbow(10)
matplot(t, t(trayectorias), type = "l", lty = 1, col = colores,
        xlab = "t", ylab = "Valor del proceso", 
        main = "10 Trayectorias de un Proceso Gaussiano (Núcleo de Cauchy)")















