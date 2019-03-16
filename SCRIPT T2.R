#Kevin Steven García - 1533173
#Alejandro Vargas - 1525953
#Alejandro Soto - 1532457
#Taller 2 Aplicada III(DBCA Y DISEÑO FACTORIAL)
#----------------------------------------------------#
#Punto 1:
#Ingresamos los datos
Niquel<-c(67.0,67.5,76.0,72.7,73.1,65.8,75.6)
Hierro<-c(71.9,68.8,82.6,78.1,74.2,70.8,84.9)
Cobre<-c(72.2,66.4,74.5,67.3,73.2,68.7,69)
Fuerza<-c(Niquel,Hierro,Cobre)
Agente<-as.factor(c(rep("1Niquel",7),rep("2Hierro",7),rep("3Cobre",7)))
Lingote<-as.factor(rep(seq(1,7),3))
datos<-data.frame(Lingote=Lingote,Agente=Agente, Fuerza=Fuerza)
attach(datos) 
head(datos)

#Descriptivos
library(ggplot2)
p<-ggplot(datos, aes(Agente, Fuerza)) + geom_point()
p + scale_x_discrete(name="Agente soldante",
                     labels=c("Niquel","Hierro","Cobre"))+labs(y="Fuerza(1000lb/plg^2)")

#Incluye la media en el gráfico
x11()
p + scale_x_discrete(name="Agente soldante",
                     labels=c("Niquel","Hierro","Cobre")) + labs(y="Fuerza(1000lb/plg^2)") +
  stat_summary(fun.y=mean, colour="red", geom="point", shape=19, size=2)

#Construimos el modelo y obtenemos la ANOVA:
mod<-aov(Fuerza~Agente+Lingote, data=datos)
summary(mod)
#Si no se cumplen los supuestos, no sirve interpretar los resultados de la ANOVA

#Verificamos los supuestos:
#Se obtienen los residuales:
resid<-residuals(mod)

#Normalidad:
#Pruebas gráficas:
library(car)
x11()
par(mfrow=c(1,2))
hist(resid, freq=FALSE)
curve(dnorm(x,mean(resid), sd(resid)), add=TRUE, col=2)
qqPlot(resid, pch=20,main="QQ-Plot de los residuos")

#Prueba formal
#Shapiro
shapiro.test(resid) 

#Media Cero
t.test(resid, mu = 0, alternative = c("two.sided"))

#Homogeneidad de Varianzas
#Residuos estandarizados por agente soldante:
library(ggplot2)
x11()
p1<-ggplot(datos, aes(Agente, rstandard(mod))) + geom_point()
p1 + scale_x_discrete(name="Agente soldante",
                     labels=c("Niquel","Hierro","Cobre"))+labs(y="Residuos estandarizados")
#Barlett
bartlett.test(resid~Agente, data=datos) 

#Independencia en los errores
#Prueba de rachas: H0:Los residuales se distribuyen de manera aleatoria
library("tseries")
residualesfactor<-c()
for (i in 1:length(residuals(mod))) {
  if (residuals(mod)[i]>0){
    residualesfactor[i]=1
  }
  if (residuals(mod)[i]<0){
    residualesfactor[i]=-1
  }
}
runs.test(factor(residualesfactor))


#Pruebas de Comparaciones Múltiple (Pruebas Postanova)
library(multcompView)
library(lsmeans)
leastsquare=lsmeans(mod, "Agente", adjust="fisher")
cld(leastsquare, alpha=.05, Letters=letters)

#----------------------------------------------------#
#Punto 2

