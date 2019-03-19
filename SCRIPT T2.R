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
#Cargar datos
datos2<-read.table("clipboard", header=TRUE, dec=",")
attach(datos2)

datos2$Insecticida<-as.factor(datos2$Insecticida)
datos2$Herbicida=as.factor(datos2$Herbicida)
#ver los datos
str(datos2)
head(datos2)

dvc=c()
for (i in 1:25) {
  if(datos2$Herbicida[i]=="2"){
    dvc=c(dvc,datos2$desarrollo[i])
  }
  
}


#descriptivas
min(dvc) ; max(dvc) ; quantile(dvc,probs = c(0.25,0.5,0.75))
des=sqrt(var(dvc)*((length(dvc)-1)/length(dvc)))
cv=(des/mean(dvc))*100


#grafica
library(ggplot2)
p<-ggplot(datos2, aes(Herbicida, desarrollo)) + geom_point()
p + scale_x_discrete(name="Herbicida",
                     labels=c("0","0.5","1","1.5","2")) + labs(y="Desarrollo")

#incluyendo el punto medio
x11()
p + scale_x_discrete(name="Herbicida",
                     labels=c("0","0.5","1","1.5","2")) + labs(y="Desarrollo") +
  stat_summary(fun.y=mean, colour="red", geom="point", shape=19, size=2)

#MODELO
mod3b<-aov(desarrollo~Herbicida+Insecticida, data=datos2)

summary(mod3b)


#supuestos
resid3b<-residuals(mod3b)

#Normalidad:
#Pruebas gráficas:
library(car)
x11()
par(mfrow=c(1,2))
hist(resid3b, freq=FALSE)
curve(dnorm(x,mean(resid3b), sd(resid3b)), xlim=c(-40,60), add=TRUE, col=2)
qqPlot(resid3b, pch=20,main="QQ-Plot de los residuos")

#Prueba formal
#Shapiro
shapiro.test(resid3b) 

#Media Cero
t.test(resid4, mu = 0, alternative = c("two.sided"))


#Homogeneidad de Varianzas
bartlett.test(resid3b~Herbicida, data=datos2)

#test de levane
leveneTest(resid3b~Herbicida, data=datos2)#tampoco sirve
#Independencia en los errores
#Prueba de rachas: H0:Los residuales se distribuyen de manera aleatoria
library("tseries")
residualesfactor<-c()
for (i in 1:length(residuals(mod3b))) {
  if (residuals(mod3b)[i]>0){
    residualesfactor[i]=1
  }
  if (residuals(mod3b)[i]<0){
    residualesfactor[i]=-1
  }
}
runs.test(factor(residualesfactor))

#----------------------------------------------------#
#Punto 3
datos3<-read.table("clipboard", header=TRUE, dec=",") # código para la subir la base de datos al R
attach(datos3)
str(datos3)

## código para hacer las descriptivas del modelo
Efectos <- data.frame(Operario, Maquina, Ruptura)
plot.design(Efectos, fun="mean", main=" Gráfica de efectos principales", ylab= "Altura", xlab="Factor")

interaction.plot(Operario, Maquina, Ruptura) # Muestra la grafica de interaccion entre los niveles de los operarios y las maquinas

Oper1<-c(109,110,110,115,108,109,110,108)
summary(Oper1) # obtiene un resumen de las descriptivas para el operario 1
sd(Oper1) # desviaicon estandar para el operario 1

Oper2<-c(110,112,110,111,111,109,114,112)
summary(Oper2) # obtiene un resumen de las descriptivas para el operario 2
sd(Oper2) # desviaicon estandar para el operario 2

Oper3<-c(116,114,112,115,114,119,120,117)
summary(Oper3) # obtiene un resumen de las descriptivas para el operario 3
sd(Oper3) # desviaicon estandar para el operario 3

Maq1<-c(109,110,110,112,116,114)
summary(Maq1) # obtiene un resumen de las descriptivas para la maquina 1
sd(Maq1)# desviacion estandar para los datos de la maquina 1
Maq2<-c(110,115,110,111,112,115)
summary(Maq2) # obtiene un resumen de las descriptivas para la maquina 2
sd(Maq2)# desviacion estandar para los datos de la maquina 2
Maq3<-c(108,109,111,109,114,119)
summary(Maq3) # obtiene un resumen de las descriptivas para la maquina 3
sd(Maq3)# desviacion estandar para los datos de la maquina 3
Maq4<-c(110,108,114,112,120,117)
summary(Maq4) # obtiene un resumen de las descriptivas para la maquina 4
sd(Maq4)# desviacion estandar para los datos de la maquina 4


library(car) # Libreria para obtener el modelos y poder obtener la anova

mod3<-lm(Ruptura~Operario+Maquina+Operario:Maquina, data=datos3)
anova(mod3) # Anova del modelo generado

resid3<-resid(mod3)# REsiduales para el modelo

# Este codigo crea un histograma de los residuales y le superpone una curva de una normal
hist(resid3, freq=FALSE)
curve(dnorm(x,mean(resid3), sd(resid3)), xlim=c(-10,10), add=TRUE, col=2)

# hace un grafico QQ-plot para los residuales del modelo 
qqPlot(resid3, pch=20)


shapiro.test(resid3)# Hace una prueba estadistica parra probar la normalidad

t.test(resid3, mu=0, alternative = c("two.sided")) # Prueba para la media 0

# Código para prbar la homogeneidad de los residuales
datos3$Int <- interaction(Operario, Maquina)
datos3$Int

bartlett.test(resid3~datos3$Int)

#Codigo para probar la independencia de los residuales
library("tseries")
residualesfactor1<-c()
for (i in 1:length(residuals(mod3))) {
  if (residuals(mod3)[i]>0){
    residualesfactor1[i]=1
  }
  if (residuals(mod3)[i]<0){
    residualesfactor1[i]=-1
  }
}
runs.test(factor(residualesfactor1))

# Pruebas multiples para los factores individuales
library(multcompView)
library(lsmeans)

leastsquare1 = lsmeans(mod3, ~Operario,  adjust="tukey") # Prueba multiple para los operarios
cld(leastsquare1, alpha=.05, Letters=letters)

leastsquare2 = lsmeans(mod3, ~Maquina,  adjust="tukey") # Prueba multiple para las maquinas
cld(leastsquare2, alpha=.05, Letters=letters)


#----------------------------------------------------#
#Punto 4
datos4<-read.table("clipboard", header=TRUE, dec=",")
attach(datos4)
datos4$Concentracion=as.factor(datos4$Concentracion)

#ver los datos
str(datos4)
head(datos4)

#Descriptivas
#consumo
dvc=c(datos4[9:16,3],datos4[25:32,3],datos4[41:48,3])
min(dvc) ; max(dvc) ; quantile(dvc,probs = c(0.25,0.5,0.75))
des=sqrt(var(dvc)*((length(dvc)-1)/length(dvc)))
cv=(des/mean(dvc))*100

#boxplot
X11()
boxplot(Consumo~Concentracion*Tipo, ylab="Porcentaje de O2")
#grafico de efectos principales
x11()
par(mfrow=c(1,2))
Efectos <- data.frame(Concentracion, Tipo,Consumo)
plot.design(Efectos, fun="mean", main=" GrÃ¡fica de efectos principales", ylab= "Consumo", xlab="Factor",ylim =c(7,20))

#Grafico de interacciones
x11()
interaction.plot(Concentracion, Tipo,Consumo)
#en otro orden
x11()
interaction.plot( Concentracion, Tipo,Consumo)


#MODELO FACTORIAL
mod4<-lm(Consumo~Concentracion+Tipo+Concentracion:Tipo, data=datos4)
anova(mod4)

#prueba de los supuestos
resid4=residuals(mod4)
#Normalidad:
#Pruebas grÃ¡ficas:
library(car)
x11()
par(mfrow=c(1,2))
hist(resid4, freq=FALSE)
curve(dnorm(x,mean(resid4), sd(resid4)), xlim=c(-20,30), add=TRUE, col=2)
qqPlot(resid4, pch=20,main="QQ-Plot de los residuos")

#Prueba formal
#Shapiro
shapiro.test(resid4) 

#Media Cero
t.test(resid4, mu = 0, alternative = c("two.sided"))

#Homogeneidad de Varianzas
datos4$Int <- interaction(Concentracion,Tipo)
datos4$Int

#Barlett
bartlett.test(resid4~datos4$Int) #no funciona solamente tengo 1 replica



#test de levane
leveneTest(resid4~Concentracion*Tipo, data=datos4)#tampoco sirve

#Independencia en los errores
#Prueba de rachas: H0:Los residuales se distribuyen de manera aleatoria
library("tseries")
residualesfactor<-c()
for (i in 1:length(residuals(mod4))) {
  if (residuals(mod4)[i]>0){
    residualesfactor[i]=1
  }
  if (residuals(mod4)[i]<0){
    residualesfactor[i]=-1
  }
}
runs.test(factor(residualesfactor))

#Como la interaccion es no significativa:
library(multcompView)
library(lsmeans)
#comparacion para Concentracion
leastsquare1 = lsmeans(mod4, ~Concentracion,  adjust="tukey")
cld(leastsquare1, alpha=.05, Letters=letters)

datos4$Concentracion
#comparacion para Tipo
leastsquare2 = lsmeans(mod4, ~Tipo,  adjust="tukey")
cld(leastsquare2, alpha=.05, Letters=letters)