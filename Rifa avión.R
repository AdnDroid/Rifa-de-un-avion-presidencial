#Supongamos que en el país de las maravillas, al gobernador se le acaban las ideas para
#recaudar fondos, así que decide hechar su suerte y apostar una gran cantidad en la lotería
#Dicho concurso otorga 20 premios de 100,000,000 cada uno y para su realización se vendieron
#6,000,000 de boletos a 500 pesos cada uno.

#Si el gobernador destina 500,000,000 para la compra de boletos, calcule el valor esperado
#de dicha "inversión".

#Simule al menos 1000 escenarios posibles

premio=100000000
Simulaciones=1000
GananciasV<-c()
set.seed(1996)

for (j in 1:Simulaciones) 
{
  BoletosInicialesPeje<-1000000
  boletosIniciales=6000000
  premiosIniciales=20
  ganancias=0  
  boletosRestantes=boletosIniciales
  premiosRestantes=premiosIniciales
  boletosRestantesPEJE=BoletosInicialesPeje

  while (premiosRestantes>0 && boletosRestantesPEJE>0) 
  {

    probaDePremio=premiosRestantes/boletosRestantes

    prueba<-rbinom(1,1,probaDePremio)


      if (prueba==1) 
        {
        ganancias=ganancias+premio
        premiosRestantes=premiosRestantes-1
        boletosRestantes=boletosRestantes-1
        boletosRestantesPEJE=boletosRestantesPEJE-1
        }else {boletosRestantes=boletosRestantes-1
        boletosRestantesPEJE=boletosRestantesPEJE-1
        }

  }

GananciasV[j]<-c(ganancias)
}

mean(GananciasV)
GananciasV2<-c()
Acumulado<-0

for (i in 1:length(GananciasV)) 
{
  Acumulado<-Acumulado+GananciasV[i]
  AcumuladoOk<-Acumulado/i
  GananciasV2[i]<-AcumuladoOk 
  
}

options(scipen=5)
x<-1:1000
format(GananciasV,scientific=FALSE)
formatC(GananciasV, digits = 0, format = "f")

plot(x=x,y=GananciasV2,type = "l",xlab = "Simulaciones Acumuladas",ylab = "Valor Esperado", main = "Gráfico de estabilización" ,ylim=c(0,700000000))
abline(h=339800000,col="red")
abline(h=500000000,col="blue")
legend(x = "topright", legend = c("Ganancias Esperadas","Dinero Invertido"), fill = c("red", "blue"), 
       title = "")


hist(GananciasV,main = "Distribución de Ganancias Esperadas",xlab = "Ganancia",ylab = "Frecuencia",breaks =25,include.lowest =T ,equidist=T )
abline(col="red",v =500000000 )
help("hist")
legend(x = "topright", legend = c("Ganancias Simuladas Acumuladas","Dinero Invertido"), fill = c("gray", "red"), 
       title = "")
