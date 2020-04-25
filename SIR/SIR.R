library(deSolve)

datos = read.csv("Yucatan.csv")
Infectados = datos$Ln.Infectados.a.la.fecha.
Burnt = Infectados[10:length(Infectados)]
t = seq(from = 0, to = length(Burnt)-1, by = 1)
data = data.frame(t, Burnt)

modelo = lm(Burnt ~ t)
modelo$coefficients

plot(t, modelo$fitted.values, type = "l")
points(t, Burnt)

parms = c(beta = 0.8, gamma = 0.83)
inits = c(S = 0.99, I = 0.0001, R = 0)
dt = seq(0, 100, 0.1)

SIR = function(t, x, parms) {
  with(as.list(c(parms, x)), {
    dS = -beta*S*I
    dI = +beta*S*I - gamma*I
    dR = gamma*I
    der = c(dS, dI, dR)
    list(der)
  })
}

simulacion = as.data.frame(lsoda(inits, dt, SIR, parms = parms))
plot(simulacion$time, simulacion$I)
