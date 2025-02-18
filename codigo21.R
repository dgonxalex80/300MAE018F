# Lanzamiento de un dado
n=200
x=sample(1:6,n, replace = TRUE)
td1=prop.table(table(x))
barplot(td1, las=1, col = c1)

#-------------------------------------------------------------------------------

# Suma de dos dados
n=200
d1=sample(1:6,n, replace = TRUE)
d2=sample(1:6,n, replace = TRUE)
dados=data.frame(d1,d2)
suma=apply(dados, 1, sum)
barplot(table(suma), las=1,cex.axis=0.7, col= c1)
data.frame(prop.table(table(suma)))

#-------------------------------------------------------------------------------

# Extracción de una bola de una urna
# +  Simulación de urna con:  3 bolas Blancas, 5 Rojas y 4 Azules
# + B representa las bolas blancas
# + R representa las bolas rojas
# + A representa las bolas azules

sample(c("B","R","A"),2,rep=T,prob=c(3,5,4))

#-------------------------------------------------------------------------------

# Tablas de contingencia
x=c(20,60,100,30,140,50)
m=matrix(x,ncol=2)
rownames(m)=c("Adminitrativo","Operativo","Vendedor")
colnames(m)=c("Mujer","Hombre")
m

#-------------------------------------------------------------------------------

# se dadicionan frecuencias marginales
addmargins(m)

#-------------------------------------------------------------------------------

# se convierten en probabilidades
prop.table(m)

#-------------------------------------------------------------------------------

# probabilidad condicional por filas
prop.table(m,1)

#-------------------------------------------------------------------------------

# probabilidad condicional por columnas
prop.table(m,2)

#-------------------------------------------------------------------------------
# simulacion de la probabilidad de la suma dos dados igual a 7

simulate_probability <- function(n_lanzamientos) {
  dados_1 <- sample(1:6, n_lanzamientos, replace = TRUE)
  dados_2 <- sample(1:6, n_lanzamientos, replace = TRUE)
  suma_dados <- dados_1 + dados_2
  prob_suma_7 <- sum(suma_dados == 7) / n_lanzamientos
  return(prob_suma_7)
}

# Tamaños de muestra a considerar
tamanos_muestra <- seq(100, 1000000, by = 100)

# Vector para almacenar probabilidades
probabilidades <- numeric(length(tamanos_muestra))

# Simulación y cálculo de probabilidades para diferentes tamaños de muestra
for (i in 1:length(tamanos_muestra)) {
  probabilidades[i] <- simulate_probability(tamanos_muestra[i])
}

# Gráfico de convergencia
plot(tamanos_muestra, probabilidades, type = "l",
     xlab = "Tamaño de Muestra", ylab = "Probabilidad de Suma 7",
     main = "Convergencia de Probabilidad de Suma 7 al Lanzar Dos Dados", las =1)
abline(h=6/36, col="red")

#-------------------------------------------------------------------------------
