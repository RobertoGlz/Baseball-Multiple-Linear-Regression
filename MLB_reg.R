
# Autor: Roberto González Téllez
# No hice un código en Markdown orque por algún motivo no lo pude instalar bien entonces corrí mi script normal y
# compilé el documento en Overleaf.

library(dplyr)
library(caret)
library(car)
library(stargazer)
library(corrplot)
library(foreign)
library(readxl)
library(tidyverse)
library(tseries)
library(pastecs)
library(fmsb)
library(forecast)
library(lmtest)
library(stats)

############################################################################################

# W = beta_0 + beta_1*OBP + beta_2*SLG + beta_3*HR + beta_4*ERA + beta_5*WHIP + beta_6*D_American + u

#Load Datasets MLB_YYYY_B for offense and MLB_YYYY_P for pitching

MLB_w_dummy <- as.data.frame(read_excel("Raw/MLB_w_dummy.xlsx"))
names(MLB_w_dummy)[names(MLB_w_dummy) == "tm"] <- "Tm"

MLB_2015_B <- subset.data.frame(read_excel("Raw/2015_MLB_B.xlsx"), select = c(Tm,OBP,SLG,HR))
for (stat in c("OBP","SLG","HR")){
  names(MLB_2015_B)[names(MLB_2015_B) == stat] <- paste(stat,"_2015",sep="")
}

MLB_2015_P <- subset.data.frame(read_excel("Raw/2015_MLB_P.xlsx"), select = c(Tm,ERA,WHIP))
for (stat in c("ERA","WHIP")){
  names(MLB_2015_P)[names(MLB_2015_P) == stat] <- paste(stat,"_2015",sep="")
}

MLB_2016_B <- subset.data.frame(read_excel("Raw/2016_MLB_B.xlsx"), select = c(Tm,OBP,SLG,HR))
for (stat in c("OBP","SLG","HR")){
  names(MLB_2016_B)[names(MLB_2016_B) == stat] <- paste(stat,"_2016",sep="")
}

MLB_2016_P <- subset.data.frame(read_excel("Raw/2016_MLB_P.xlsx"), select = c(Tm,ERA,WHIP))
for (stat in c("ERA","WHIP")){
  names(MLB_2016_P)[names(MLB_2016_P) == stat] <- paste(stat,"_2016",sep="")
}

MLB_2017_B <- subset.data.frame(read_excel("Raw/2017_MLB_B.xlsx"), select = c(Tm,OBP,SLG,HR))
for (stat in c("OBP","SLG","HR")){
  names(MLB_2017_B)[names(MLB_2017_B) == stat] <- paste(stat,"_2017",sep="")
}

MLB_2017_P <- subset.data.frame(read_excel("Raw/2017_MLB_P.xlsx"), select = c(Tm,ERA,WHIP))
for (stat in c("ERA","WHIP")){
  names(MLB_2017_P)[names(MLB_2017_P) == stat] <- paste(stat,"_2017",sep="")
}

MLB_2018_B <- subset.data.frame(read_excel("Raw/2018_MLB_B.xlsx"), select = c(Tm,OBP,SLG,HR))
for (stat in c("OBP","SLG","HR")){
  names(MLB_2018_B)[names(MLB_2018_B) == stat] <- paste(stat,"_2018",sep="")
}

MLB_2018_P <- subset.data.frame(read_excel("Raw/2018_MLB_P.xlsx"), select = c(Tm,ERA,WHIP))
for (stat in c("ERA","WHIP")){
  names(MLB_2018_P)[names(MLB_2018_P) == stat] <- paste(stat,"_2018",sep="")
}

MLB_2019_B <- subset.data.frame(read_excel("Raw/2019_MLB_B.xlsx"), select = c(Tm,OBP,SLG,HR))
for (stat in c("OBP","SLG","HR")){
  names(MLB_2019_B)[names(MLB_2019_B) == stat] <- paste(stat,"_2019",sep="")
}

MLB_2019_P <- subset.data.frame(read_excel("Raw/2019_MLB_P.xlsx"), select = c(Tm,ERA,WHIP))
for (stat in c("ERA","WHIP")){
  names(MLB_2019_P)[names(MLB_2019_P) == stat] <- paste(stat,"_2019",sep="")
}

# Merge datasets to build "full" dataset
merge_1 <- left_join(MLB_w_dummy, MLB_2015_B, by = "Tm")
merge_2 <- left_join(merge_1, MLB_2015_P, by = "Tm")
merge_3 <- left_join(merge_2, MLB_2016_B, by = "Tm")
merge_4 <- left_join(merge_3, MLB_2016_P, by = "Tm")
merge_5 <- left_join(merge_4, MLB_2017_B, by = "Tm")
merge_6 <- left_join(merge_5, MLB_2017_P, by = "Tm")
merge_7 <- left_join(merge_6, MLB_2018_B, by = "Tm")
merge_8 <- left_join(merge_7, MLB_2018_P, by = "Tm")
merge_9 <- left_join(merge_8, MLB_2019_B, by = "Tm")
full_merge <- left_join(merge_9, MLB_2019_P, by = "Tm")

#Construct stat averages per team
full_merge$avg_wins <- rep(0,nrow(full_merge))
full_merge$avg_OBP <- rep(0,nrow(full_merge))
full_merge$avg_SLG <- rep(0,nrow(full_merge))
full_merge$avg_HR <- rep(0,nrow(full_merge))
full_merge$avg_ERA <- rep(0,nrow(full_merge))
full_merge$avg_WHIP <- rep(0,nrow(full_merge))

#Average Wins
for (k in 1:30){
  full_merge[k,33] <- round((full_merge[k,3]+full_merge[k,4]+full_merge[k,5]+full_merge[k,6]+
                               full_merge[k,7])/5,0)
}

#Average OBP
for (k in 1:30){
  full_merge[k,34] <- round((full_merge[k,8]+full_merge[k,13]+full_merge[k,18]+full_merge[k,23]+
                                full_merge[k,28])/5,3)
}
     
#Average SLG
for (k in 1:30){
  full_merge[k,35] <- round((full_merge[k,9]+full_merge[k,14]+full_merge[k,19]+full_merge[k,24]+
                               full_merge[k,29])/5,3)
}

#Average HR
for (k in 1:30){
  full_merge[k,36] <- round((full_merge[k,10]+full_merge[k,15]+full_merge[k,20]+full_merge[k,25]+
                               full_merge[k,30])/5,0)
}

#Average ERA
for (k in 1:30){
  full_merge[k,37] <- round((full_merge[k,11]+full_merge[k,16]+full_merge[k,21]+full_merge[k,26]+
                               full_merge[k,31])/5,2)
}

#Average WHIP
for (k in 1:30){
  full_merge[k,38] <- round((full_merge[k,12]+full_merge[k,17]+full_merge[k,22]+full_merge[k,27]+
                               full_merge[k,32])/5,2)
}

#Keep only avg variables
working_dataset <- subset.data.frame(full_merge, select = c(Tm,d_american_league,avg_wins,avg_OBP,
                                                            avg_SLG,avg_HR,avg_ERA,avg_WHIP))
attach(working_dataset)
rm(k)
#########################################################################################

# Creamos data frame de variables continuas y graficamos sus Diagramas de Caja y Brazos
cont_variables <- data.frame(avg_wins, avg_ERA, avg_HR, avg_OBP, avg_SLG, avg_WHIP)
summary(cont_variables)
boxplot(cont_variables[c(1,3)], col="green", main="Wins & HR")
boxplot(cont_variables[c(2,6)], col="turquoise", main="ERA & WHIP")
boxplot(cont_variables[c(4,5)], col="yellow", main="OBP & SLG")

# Gráficos de dispersión para ver si relación lineal es lo mejor
plot(avg_ERA,avg_wins, type="p", col="red", main="ERA v Wins")
abline(lm(avg_wins~avg_ERA, data=working_dataset), col="blue")

plot(avg_HR,avg_wins, type="p", col="red", main="HR v Wins")
abline(lm(avg_wins~avg_HR, data=working_dataset), col="blue")

plot(avg_OBP,avg_wins, type="p", col="red", main="OBP v Wins")
abline(lm(avg_wins~avg_OBP, data=working_dataset), col="blue")

plot(avg_SLG,avg_wins, type="p", col="red", main="SLG v Wins")
abline(lm(avg_wins~avg_SLG, data=working_dataset), col="blue")

plot(avg_WHIP,avg_wins, type="p", col="red", main="WHIP v Wins")
abline(lm(avg_wins~avg_WHIP, data=working_dataset), col="blue")

#Notar que en todas las variables parece ser que una relación lineal es la ideal para modelar

# Análisis incremental para seleccionar el modelo que mejor ajusta hasta ahora
dep_vars <- data.frame(avg_wins,avg_ERA, avg_HR, avg_OBP, avg_SLG, avg_WHIP, d_american_league)
M <- round(cor(dep_vars),4)
corrplot(cor(dep_vars), method="number",type="upper")

# Estimación por MMCO de modelos con análisis incremental
mod_1 <- lm(avg_wins~avg_ERA,data=working_dataset)
mod_2 <- lm(avg_wins~avg_ERA+avg_OBP,data=working_dataset)
mod_3 <- lm(avg_wins~avg_ERA+avg_OBP+avg_SLG,data=working_dataset)
mod_4 <- lm(avg_wins~avg_ERA+avg_OBP+avg_SLG+avg_WHIP,data=working_dataset)
mod_5 <- lm(avg_wins~avg_ERA+avg_OBP+avg_SLG+avg_WHIP+avg_HR,data=working_dataset)
mod_6 <- lm(avg_wins~avg_ERA+avg_OBP+avg_SLG+avg_WHIP+avg_HR+d_american_league,
            data=working_dataset)
mod_7 <- lm(avg_wins~avg_ERA+avg_OBP+avg_SLG+avg_WHIP+avg_HR+d_american_league+
              I(d_american_league*avg_SLG),
            data=working_dataset)
stargazer(mod_1,mod_2,mod_3,mod_4,type="text")
stargazer(mod_5,mod_6,mod_7,type="text")

#El mejor modelo por significancia es el modelo 3
# Probemos Criterio de Akaike para confirmar
akaike_1 <- AIC(mod_1)
akaike_2 <- AIC(mod_2)
akaike_3 <- AIC(mod_3)
akaike_4 <- AIC(mod_4)
akaike_5 <- AIC(mod_5)
akaike_6 <- AIC(mod_6)
akaike_7 <- AIC(mod_7)
min(c(akaike_1,akaike_2,akaike_3,akaike_4,akaike_5,akaike_6,akaike_7))
# Acá probamos con el criterio Bayesiano (o de Scwartz)
bic_1 <- BIC(mod_1)
bic_2 <- BIC(mod_2)
bic_3 <- BIC(mod_3)
bic_4 <- BIC(mod_4)
bic_5 <- BIC(mod_5)
bic_6 <- BIC(mod_6)
bic_7 <- BIC(mod_7)
min(c(bic_1,bic_2,bic_3,bic_4,bic_5,bic_6,bic_7))
# Comprobamos que el modelo con mejores Criterios de Información es el del modelo 3

# Sin embargo, la teoría nos dice que pertenecer a la Liga Americana debería aumentar 
# significativamente el SLG así que haremos una prueba más de significancia 
# de modelo restringido vs no restringido
mod_3_interaction_d_SLG <- lm(avg_wins~avg_ERA+avg_OBP+avg_SLG+
                            I(d_american_league*avg_SLG),data=working_dataset)
stargazer(mod_3,mod_3_interaction_d_SLG,type="text")
n <- length(avg_wins)
k_opt <- 5
F_restr <- ((.950-.949)/2)/((.950/(n-k_opt-1)))
F_restr > qf(.975,2,n-k_opt-1) 
# F < crítico de F con 2,24 gl, por lo tanto no rechazamos la nula de que los 
# coeficientes del no restringido no sean necesarios (i.e. iguales a 0) y usaremos el modelo 3
best_mod <- mod_3
# Intervalo de confianza para nuestros estimadores al 95% de significancia
confint(best_mod)

# Podemos observar que la bondad de ajuste del modelo es:
r2 <- round(summary(best_mod)$r.squared,3)
paste("R^2 =", r2)
adj_r2 <- round(summary(best_mod)$adj.r.squared,3)
paste("Adj. R^2 =", adj_r2)

# Bondad de Ajuste Gráfica
y <- avg_wins
y_gorro <- best_mod$fitted.values
u_gorro <- best_mod$residuals
k_best_mod <- 3
SR <- u_gorro**2
varianza_gorro_residuales <- sum(SR)/(n-k_best_mod-1)

obs <- 1:n
plot(obs,y,type="l", main="Bondad de Ajuste", col="red")
lines(obs,y_gorro,type="l",col="blue")

# Análisis de la Varianza (ANOVA)
anova_mod <- aov(best_mod)
summary(anova_mod)

# Probemos que se cumplen los supuestos y si no cumple, realizamos las correcciones necesarias

# Supuesto de normalidad del término estocástico
# Histograma
hist(u_gorro,breaks=10,col="red",freq=F, main="Histograma con curva normal")
dz <- seq(min(u_gorro), max(u_gorro),0.001)
lines(dnorm(dz,0,sqrt(varianza_gorro_residuales))~dz,type="l",col="blue", lwd=3)
# Al tener tamaño de muestra de 30 usamos la prueba de Jarque-Bera
jarque.bera.test(u_gorro)
# Como p-value > a 0.05 entonces no rechazamos la hipótesis de que nuestros residuales
# se distribuyan normales

# Supuesto de no multicolinealidad
# En la matriz de correlaciones vimos que existe alta correlación positiva entre avg_OBP
# y avg_SLG así que haremos pruebas con Regla de Klein para verificar si existe multicol.
aux_OBP <- lm(avg_OBP~avg_ERA+avg_SLG, data=working_dataset)
aux_ERA <- lm(avg_ERA~avg_OBP+avg_SLG, data=working_dataset)
aux_SLG <- lm(avg_SLG~avg_ERA+avg_OBP, data=working_dataset)
stargazer(aux_ERA,aux_OBP,aux_SLG,type="text")
# Notar que ningún R^2 de las regresiones auxiliares > R^2 del modelo original, por lo tanto
# determinamos que no existe multicolinealidad.
vif(best_mod)
# Notar también que los Factores Infladores de Varianza son < 10 por lo que podemos continuar
# con el análisis seguros de que no existe multicolinealidad

# Supuesto de homoscedasticidad
# Análisis gráfico
plot(obs,u_gorro,type="l",col="red")
abline(h=0,col="blue")
plot(y_gorro,SR,type="p",col="red")
abline(h=0,col="blue")
# Los métodos gráficos sugieren que existe heteroscedasticidad
# Prueba de Breusch, Pagan y Godfrey
bptest(best_mod) # Como no se rechaza la nula por p-value => homoscedasticidad
# Prueba de White
aux_White <- lm(SR~avg_ERA+avg_OBP+avg_SLG+I(avg_ERA**2)+I(avg_OBP**2)+I(avg_SLG**2)+
                  I(avg_ERA*avg_OBP)+I(avg_ERA*avg_SLG)+I(avg_OBP*avg_SLG),data=working_dataset)
r2_White <- summary(aux_White)$r.squared
LM_White <- n*r2_White
p_value_White <- pchisq(q=LM_White, df=9,lower.tail=FALSE)
# Como el p-value de la Prueba de White > 0.05 entonces concluimos que el modelo presenta
# homoscedasticidad

# Supuesto de no autocorrelación del término estocástico
# Método gráfico
plot(obs,u_gorro,type="l",col="red")
abline(h=0,col="blue")
u_gorro_1 <- vector()
for (i in 2:n){
  u_gorro_1[i]<-u_gorro[i-1]
}
plot(u_gorro_1,u_gorro,type="p",col="red")
abline(h=0,col="blue")
abline(v=0,col="blue")
# En general parece ser que no existe autocorrelación en el modelo estimado
# Verifiquemos con Durbin-Watson y Breusch-Godfrey
dwtest(best_mod)
bgtest(best_mod)
# Al ser los p-value de ambas pruebas >0.05 entonces no rechazamos la nula de que no existe
# autocorrelación en el modelo

# Supuesto de no endogeneidad
# Método Gráfico
plot(obs,u_gorro,type="l",col="red")
abline(h=0,col="blue")
# Prueba RESET
resettest(best_mod,power=2,type="fitted")
resettest(best_mod,power=3,type="fitted") 
resettest(best_mod,power=2:3,type="fitted")
# Concluimos que el modelo está bien especificado porque p-value > 0.05
  








