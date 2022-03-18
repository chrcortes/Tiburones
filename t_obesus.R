setwd("C:/Users/corte/Dropbox/maestría/RESULTADOS/modelos/")

datos <- read.csv("datos.csv")
datos$date <- as.Date(as.character(datos$date), format = "%d/%m/%Y")
datos$Mes <- factor(datos$Mes, levels = levels(datos$Mes)[ c(2,3,4,6,1,7,5)])


library(mgcv)
#library(gam)
datos.2 <- data.frame(tob=c(datos$ar.CP.tob, datos$ar.EB.tob, datos$ar.EC.tob, datos$ar.LC.tob, datos$ar.PT.tob, datos$ar.RP.tob), 
                     slew=c(datos$ar.CP.slew, datos$ar.EB.slew, datos$ar.EC.slew, datos$ar.LC.slew, datos$ar.PT.slew, datos$ar.RP.slew), 
                     cga=c(datos$ar.CP.cga, datos$ar.EB.cga, datos$ar.EC.cga, datos$ar.LC.cga, datos$ar.PT.cga, datos$ar.RP.cga), 
                     cal=c(datos$ar.CP.cal, datos$ar.EB.cal, datos$ar.EC.cal, datos$ar.LC.cal, datos$ar.PT.cal, datos$ar.RP.cal), 
                     cfa=c(datos$ar.CP.cfa, datos$ar.EB.cfa, datos$ar.EC.cfa, datos$ar.LC.cfa, datos$ar.PT.cfa, datos$ar.RP.cfa), 
                     sst=c(datos$sst.CP, datos$sst.EB, datos$sst.EC, datos$sst.LC, datos$sst.PT, datos$sst.RP), 
                     sstA=c(datos$sstA.CP, datos$sstA.EB, datos$sstA.EC, datos$sstA.LC, datos$sstA.PT, datos$sstA.RP),
                     chla = c(datos$chla.CP, datos$chla.EB, datos$chla.EC, datos$chla.LC, datos$chla.PT, datos$chla.RP),
                     st=c(datos$st.CP, datos$st.EB, datos$st.EC, datos$st.LC, datos$st.PT, datos$st.RP),
                     co=c(datos$c.CP, datos$c.EB, datos$c.EC, datos$c.LC, datos$c.PT, datos$c.RP),
                     isla = c(rep("So", 13), rep("SB", 13), rep("SB", 13), rep("SB", 13), rep("So", 13), rep("RP", 13)), 
                     sitio = c(rep("CP", 13), rep("EB", 13), rep("EC", 13), rep("LC", 13), rep("PT", 13), rep("RP", 13)), 
                     temporada = datos$Temporada,
                     mes = datos$Mes,
                     Estacion = datos$Estacion)
datos.2 <- na.omit(datos.2)

#### Correlacion de Pearson ####
cor.test.p <- function(x){
  FUN <- function(x, y) cor.test(x, y)[["p.value"]]
  z <- outer(
    colnames(x), 
    colnames(x), 
    Vectorize(function(i,j) FUN(x[,i], x[,j]))
  )
  dimnames(z) <- list(colnames(x), colnames(x))
  z
}

#### Correlacion (R-values)
cor.R <- round(cor(datos.2[,-c(11:15)]), digits = 4)
#write.csv(as.data.frame(cor.R), "cor_R.csv")

cor.P <- round(cor.test.p(datos.2[,-c(11:15)]), digits = 4)
#write.csv(as.data.frame(cor.P), "cor_P.csv")

plot((datos.2[,-c(11:15)]))

plot(sst ~ isla, data = datos.2)
plot(sstA ~ isla, data = datos.2)
plot(st ~ isla, data = datos.2)
plot(chla ~ isla, data = datos.2)

# diferencias significativaas entre ISLAS
kruskal.test(sst ~ isla, data = datos.2) # no hay dif significativas en sst
kruskal.test(chla ~ isla, data = datos.2) # SI hay dif significativas en chla
dunn.test(datos.2$chla, datos.2$isla)

# diferencias significativaas entre SITIOS
kruskal.test(sst ~ sitio, data = datos.2) # no hay dif significativas en sst
kruskal.test(chla ~ sitio, data = datos.2) # SI hay dif significativas en chla
dunn.test(datos.2$chla, datos.2$sitio)


plot(sst ~ sitio, data = datos.2)
plot(chla ~ sitio, data = datos.2)
ggplot(datos.2, aes(isla, sst, fill = isla)) + geom_boxplot()
ggplot(datos.2, aes(isla, chla, fill = isla)) + geom_boxplot()
ggplot(datos.2, aes(sitio, sst, fill = isla)) + geom_boxplot()
ggplot(datos.2, aes(sitio, chla, fill = isla)) + geom_boxplot()

slew.cga <- lm(cga ~ slew, data = datos.2)
abline(slew.cga, col = "red")
summary(slew.cga)
coef(slew.cga)


plot(cga ~ slew, data = datos.2)
slew.cga <- lm(cga ~ slew, data = datos.2)
abline(slew.cga, col = "red")
summary(slew.cga)
coef(slew.cga)

plot(sst ~ st, data = datos.2)
st.sst <- lm(sst ~ st, data = datos.2)
abline(st.sst, col = "red")
summary(st.sst)
coef(st.sst)

plot(sstA ~ st, data = datos.2)
st.sstA <- lm(sstA ~ st, data = datos.2)
abline(st.sstA, col = "red")
summary(st.sstA)
coef(st.sstA)
RP.tob 
head(datos.2)

#### loop ####
gam.tob <- gam(tob ~ s(st, k = 4), data = datos.2, family = poisson)
gam.slew <- gam(slew ~ s(st, k = 4), data = datos.2, family = poisson)
gam.cga <- gam(cga ~ s(st, k = 4), data = datos.2, family = poisson)
gam.cal <- gam(cal ~ s(st, k = 4), data = datos.2, family = poisson)
gam.cfa <- gam(cfa ~ s(st, k = 4), data = datos.2, family = poisson)

tabla.spp <- data.frame(Model = factor(c("tob", "slew", "cga", "cal", "cfa"), levels = c("tob", "slew", "cga", "cal", "cfa")), R2 = NA, adj.R2 = NA, RSS = NA, AIC = NA)


#### T. obe ####
names(datos.2)
mod.tob.0 <- gam(tob ~ 1, data = datos.2, family = quasipoisson)
mod.tob.1 <- gam(tob ~ s(slew, k = 4), data = datos.2, family = quasipoisson)
mod.tob.2 <- gam(tob ~ s(cga, k = 4), data = datos.2, family = quasipoisson)
mod.tob.3 <- gam(tob ~ s(cal, k = 4), data = datos.2, family = quasipoisson)
mod.tob.4 <- gam(tob ~ s(cfa, k = 4), data = datos.2, family = quasipoisson)
mod.tob.5 <- gam(tob ~ s(sst, k = 4), data = datos.2, family = quasipoisson)
mod.tob.6 <- gam(tob ~ s(sstA, k = 4), data = datos.2, family = quasipoisson)
mod.tob.7 <- gam(tob ~ s(chla, k = 4), data = datos.2, family = quasipoisson)
mod.tob.8 <- gam(tob ~ s(st, k = 4), data = datos.2, family = quasipoisson)
mod.tob.9 <- gam(tob ~ s(co, k = 4), data = datos.2, family = quasipoisson)
mod.tob.10 <- gam(tob ~ isla, data = datos.2, family = quasipoisson)
mod.tob.11 <- gam(tob ~ sitio, data = datos.2, family = quasipoisson)
mod.tob.12 <- gam(tob ~ temporada, data = datos.2, family = quasipoisson)
mod.tob.13 <- gam(tob ~ mes, data = datos.2, family = quasipoisson)
mod.tob.14 <- gam(tob ~ Estacion, data = datos.2, family = quasipoisson)
summary(mod.tob.1)

#mod.tob.1$null.deviance
#deviance(mod.tob.1) # Devianza residual
#logLik.gam(mod.tob.1)
#gam.check(mod.tob.1)


# Seleccion del mejor modelo
summary(mod.tob.0)
mod.tob.a <- gam(tob ~ sitio, data = datos.2, family = quasipoisson)
summary(mod.tob.a)
anova(mod.tob.0, mod.tob.a, test = "Chisq")

mod.tob.b <- gam(tob ~ sitio + s(chla, k = 4), data = datos.2, family = quasipoisson)
summary(mod.tob.b)
anova(mod.tob.a, mod.tob.b, test = "Chisq")

mod.tob.c <- gam(tob ~ sitio + s(chla, k = 4) + s(slew, k = 4), data = datos.2, family = quasipoisson)
summary(mod.tob.c)
anova(mod.tob.b, mod.tob.c, test = "Chisq")

mod.tob.d <- gam(tob ~ sitio + s(chla, k = 4) + s(slew, k = 4) + s(co, k = 4), data = datos.2, family = quasipoisson)
summary(mod.tob.d)
anova(mod.tob.c, mod.tob.d, test = "Chisq")

mod.tob.e <- gam(tob ~ sitio + s(chla, k = 4) + s(slew, k = 4) + s(co, k = 4) + s(sstA, k = 4), data = datos.2, family = quasipoisson)
summary(mod.tob.e)
anova(mod.tob.d, mod.tob.e, test = "Chisq")

plot.gam(mod.tob.e, pages = 1, scale=0, shade=T, all.terms = T)
plot(mod.tob.e, select = 5, shade=T, all.terms = T)

#### figuras de efecto ###
par(mfrow = c(2, 3))
plot(mod.tob.e, select = 1, shade=T, all.terms = T, xlab = "Clorofila-a")
plot(mod.tob.e, select = 2, shade=T, all.terms = T, xlab = "S. lewini")
plot(mod.tob.e, select = 3, shade=T, all.terms = T, xlab = "Corriente")
plot(mod.tob.e, select = 4, shade=T, all.terms = T, xlab = "Anomalías de TSM")
plot(mod.tob.e, select = 5, shade=T, all.terms = T, xlab = "Sitio")
plot.gam(mod.tob.e, select = 5,  xlab = "Sitio")

#x11(); par(mfrow=c(2,3))
#for(i in 1:5){
  plot(mod.tob.e, select=i, scale=0, shade=T, all.terms = T) 
}
#?plot.gam

#x11()
vis.gam(mod.tob.e, view = c("chla", "sstA"), type = "response", plot.type = "persp", theta = 50)


#?vis.gam
#vis.gam(mod.tob.d, ticktype="detailed",color="heat",theta=-35)




#### S. lew ####
mod.slew.0 <- gam(slew ~ 1, data = datos.2, family = quasipoisson)
mod.slew.1 <- gam(slew ~ s(tob, k = 4), data = datos.2, family = quasipoisson)
mod.slew.2 <- gam(slew ~ s(cga, k = 4), data = datos.2, family = quasipoisson)
mod.slew.3 <- gam(slew ~ s(cal, k = 4), data = datos.2, family = quasipoisson)
mod.slew.4 <- gam(slew ~ s(cfa, k = 4), data = datos.2, family = quasipoisson)
mod.slew.5 <- gam(slew ~ s(sst, k = 4), data = datos.2, family = quasipoisson)
mod.slew.6 <- gam(slew ~ s(sstA, k = 4), data = datos.2, family = quasipoisson)
mod.slew.7 <- gam(slew ~ s(chla, k = 4), data = datos.2, family = quasipoisson)
mod.slew.8 <- gam(slew ~ s(st, k = 4), data = datos.2, family = quasipoisson)
mod.slew.9 <- gam(slew ~ s(co, k = 4), data = datos.2, family = quasipoisson)
mod.slew.10 <- gam(slew ~ isla, data = datos.2, family = quasipoisson)
mod.slew.11 <- gam(slew ~ sitio, data = datos.2, family = quasipoisson)
mod.slew.12 <- gam(slew ~ temporada, data = datos.2, family = quasipoisson)
mod.slew.13 <- gam(slew ~ mes, data = datos.2, family = quasipoisson)
mod.slew.14 <- gam(slew ~ Estacion, data = datos.2, family = quasipoisson)
summary(mod.slew.13)
plot.gam(mod.slew.11, pages = 1, scale = 0, all.terms = T)


# Seleccion del mejor modelo
summary(mod.slew.0)
mod.slew.a <- gam(slew ~ isla, data = datos.2, family = quasipoisson)
summary(mod.slew.a)
anova(mod.slew.0, mod.slew.a, test = "Chisq")

mod.slew.b <- gam(slew ~ isla + s(tob, k = 4), data = datos.2, family = quasipoisson)
summary(mod.slew.b)
anova(mod.slew.a, mod.slew.b, test = "Chisq")

mod.slew.c <- gam(slew ~ isla + s(tob, k = 4) + s(cga, k = 4), data = datos.2, family = quasipoisson)
summary(mod.slew.c)
anova(mod.slew.b, mod.slew.c, test = "Chisq")

mod.slew.d <- gam(slew ~ isla + s(tob, k = 4) + s(cga, k = 4) + s(st, k = 4), data = datos.2, family = quasipoisson)
summary(mod.slew.d)
anova(mod.slew.c, mod.slew.d, test = "Chisq")


#### figuras de efecto ###
plot.gam(mod.slew.d, pages = 1, scale=0, shade=T, all.terms = T)
plot.gam(mod.slew.d, scale=0, shade=T, select = 3)

par(mfrow = c(2, 2))
plot(mod.slew.d, select = 1, shade=T, all.terms = T, xlab = "T. obesus")
plot(mod.slew.d, select = 2, shade=T, all.terms = T, xlab = "C. galapagensis")
plot(mod.slew.d, select = 3, shade=T, all.terms = T, xlab = "TM in situ")
plot(mod.slew.d, select = 4, shade=T, all.terms = T, xlab = "Isla")


#### C. gal ####
mod.cga.0 <- gam(cga ~ 1, data = datos.2, family = quasipoisson)
mod.cga.1 <- gam(cga ~ s(tob, k = 4), data = datos.2, family = quasipoisson)
mod.cga.2 <- gam(cga ~ s(slew, k = 4), data = datos.2, family = quasipoisson)
mod.cga.3 <- gam(cga ~ s(cal, k = 4), data = datos.2, family = quasipoisson)
mod.cga.4 <- gam(cga ~ s(cfa, k = 4), data = datos.2, family = quasipoisson)
mod.cga.5 <- gam(cga ~ s(sst, k = 4), data = datos.2, family = quasipoisson)
mod.cga.6 <- gam(cga ~ s(sstA, k = 4), data = datos.2, family = quasipoisson)
mod.cga.7 <- gam(cga ~ s(chla, k = 4), data = datos.2, family = quasipoisson)
mod.cga.8 <- gam(cga ~ s(st, k = 4), data = datos.2, family = quasipoisson)
mod.cga.9 <- gam(cga ~ s(co, k = 4), data = datos.2, family = quasipoisson)
mod.cga.10 <- gam(cga ~ isla, data = datos.2, family = quasipoisson)
mod.cga.11 <- gam(cga ~ sitio, data = datos.2, family = quasipoisson)
mod.cga.12 <- gam(cga ~ temporada, data = datos.2, family = quasipoisson)
mod.cga.13 <- gam(cga ~ mes, data = datos.2, family = quasipoisson)
mod.cga.14 <- gam(cga ~ Estacion, data = datos.2, family = quasipoisson)
summary(mod.cga.10)
plot.gam(mod.cga.11, pages = 1, scale = 0, all.terms = T)


# Seleccion del mejor modelo
summary(mod.cga.0)
mod.cga.a <- gam(cga ~ s(slew, k = 4), data = datos.2, family = quasipoisson)
summary(mod.cga.a)
anova(mod.cga.0, mod.cga.a, test = "Chisq")

mod.cga.b <- gam(cga ~ s(slew, k = 4) + s(tob, k = 4), data = datos.2, family = quasipoisson)
summary(mod.cga.b)
anova(mod.cga.a, mod.cga.b, test = "Chisq")

mod.cga.c <- gam(cga ~ s(slew, k = 4) + s(tob, k = 4) + s(st, k = 4), data = datos.2, family = quasipoisson)
summary(mod.cga.c)
anova(mod.cga.b, mod.cga.c, test = "Chisq")

summary(mod.cga.c)$r.sq
summary(mod.cga.c)$dev.exp*100

#### figuras de efecto ###
plot.gam(mod.cga.c, pages = 1, scale = 0, shade = T, all.terms = T)
plot.gam(mod.cga.c, scale = 0, shade = T, select = 3)

par(mfrow = c(2, 2))
plot(mod.cga.c, select = 1, shade=T, all.terms = T, xlab = "S. lewini")
plot(mod.cga.c, select = 2, shade=T, all.terms = T, xlab = "T. obesus")
plot(mod.cga.c, select = 3, shade=T, all.terms = T, xlab = "TM in situ")


#### C. alb ####
mod.cal.0 <- gam(cal ~ 1, data = datos.2, family = quasipoisson)
mod.cal.1 <- gam(cal ~ s(tob, k = 4), data = datos.2, family = quasipoisson)
mod.cal.2 <- gam(cal ~ s(slew, k = 4), data = datos.2, family = quasipoisson)
mod.cal.3 <- gam(cal ~ s(cga, k = 4), data = datos.2, family = quasipoisson)
mod.cal.4 <- gam(cal ~ s(cfa, k = 4), data = datos.2, family = quasipoisson)
mod.cal.5 <- gam(cal ~ s(sst, k = 4), data = datos.2, family = quasipoisson)
mod.cal.6 <- gam(cal ~ s(sstA, k = 4), data = datos.2, family = quasipoisson)
mod.cal.7 <- gam(cal ~ s(chla, k = 4), data = datos.2, family = quasipoisson)
mod.cal.8 <- gam(cal ~ s(st, k = 4), data = datos.2, family = quasipoisson)
mod.cal.9 <- gam(cal ~ s(co, k = 4), data = datos.2, family = quasipoisson)
mod.cal.10 <- gam(cal ~ isla, data = datos.2, family = quasipoisson)
mod.cal.11 <- gam(cal ~ sitio, data = datos.2, family = quasipoisson)
plot.gam(mod.cal.3, pages = 1, scale = 0, all.terms = T)
mod.cal.12 <- gam(cal ~ temporada, data = datos.2, family = quasipoisson)
mod.cal.13 <- gam(cal ~ mes, data = datos.2, family = quasipoisson)
mod.cal.14 <- gam(cal ~ Estacion, data = datos.2, family = quasipoisson)
summary(mod.cal.4)
plot(datos.2$cfa, datos.2$cal)

# Seleccion del mejor modelo
summary(mod.cal.0)
mod.cal.a <- gam(cal ~ sitio, data = datos.2, family = quasipoisson)
summary(mod.cal.a)$dev.exp*100
anova(mod.cal.0, mod.cal.a, test = "Chisq")

mod.cal.b <- gam(cal ~ sitio + s(sstA, k = 4), data = datos.2, family = quasipoisson)
summary(mod.cal.b)
anova(mod.cal.a, mod.cal.b, test = "Chisq")

mod.cal.c <- gam(cal ~ sitio + s(sstA, k = 4) + s(co, k = 4), data = datos.2, family = quasipoisson)
summary(mod.cal.c)
anova(mod.cal.b, mod.cal.c, test = "Chisq")

mod.cal.d <- gam(cal ~ sitio + s(sstA, k = 4) + s(co, k = 4) + s(cga, k = 4), data = datos.2, family = quasipoisson)
summary(mod.cal.d)
anova(mod.cal.c, mod.cal.d, test = "Chisq")

plot.gam(mod.cal.d, pages = 1, scale=0, shade=T, all.terms = T)

par(mfrow = c(2, 2))
plot(mod.cal.d, select = 1, shade=T, all.terms = T, xlab = "Anomalías de TSM")
plot(mod.cal.d, select = 2, shade=T, all.terms = T, xlab = "Corriente")
plot(mod.cal.d, select = 3, shade=T, all.terms = T, xlab = "C. galapagensis")
plot(mod.cal.d, select = 4, shade=T, all.terms = T, xlab = "Sitio")



#### C. fal ####
mod.cfa.0 <- gam(cfa ~ 1, data = datos.2, family = quasipoisson)
mod.cfa.1 <- gam(cfa ~ s(tob, k = 4), data = datos.2, family = quasipoisson)
mod.cfa.2 <- gam(cfa ~ s(slew, k = 4), data = datos.2, family = quasipoisson)
mod.cfa.3 <- gam(cfa ~ s(cga, k = 4), data = datos.2, family = quasipoisson)
mod.cfa.4 <- gam(cfa ~ s(cal, k = 4), data = datos.2, family = quasipoisson)
mod.cfa.5 <- gam(cfa ~ s(sst, k = 4), data = datos.2, family = quasipoisson)
mod.cfa.6 <- gam(cfa ~ s(sstA, k = 4), data = datos.2, family = quasipoisson)
mod.cfa.7 <- gam(cfa ~ s(chla, k = 4), data = datos.2, family = quasipoisson)
mod.cfa.8 <- gam(cfa ~ s(st, k = 4), data = datos.2, family = quasipoisson)
mod.cfa.9 <- gam(cfa ~ s(co, k = 4), data = datos.2, family = quasipoisson)
mod.cfa.10 <- gam(cfa ~ isla, data = datos.2, family = quasipoisson)
mod.cfa.11 <- gam(cfa ~ sitio, data = datos.2, family = quasipoisson)
mod.cfa.12 <- gam(cfa ~ temporada, data = datos.2, family = quasipoisson)
mod.cfa.13 <- gam(cfa ~ mes, data = datos.2, family = quasipoisson)
mod.cfa.14 <- gam(cfa ~ Estacion, data = datos.2, family = quasipoisson)
summary(mod.cfa.14)
plot.gam(mod.cfa.13, pages = 1, scale = 0, all.terms = T)


# Seleccion del mejor modelo
summary(mod.cfa.0)
mod.cfa.a <- gam(cfa ~ s(cal, k = 4), data = datos.2, family = quasipoisson)
summary(mod.cfa.a)
anova(mod.cfa.0, mod.cfa.a, test = "Chisq")

mod.cfa.b <- gam(cfa ~ s(cal, k = 4) + s(slew, k = 4), data = datos.2, family = quasipoisson)
summary(mod.cfa.b)
anova(mod.cfa.a, mod.cfa.b, test = "Chisq")

mod.cfa.c <- gam(cfa ~ s(cal, k = 4) + s(slew, k = 4) + mes, data = datos.2, family = quasipoisson)
summary(mod.cfa.c)
anova(mod.cfa.b, mod.cfa.c, test = "Chisq")

# figuras de efectos parciales
plot.gam(mod.cfa.c, pages = 1, scale=0, shade=T, all.terms = T)
plot.gam(mod.cfa.c, scale = 0, shade = T, select = 3) 

par(mfrow = c(2, 2))
plot(mod.cfa.c, select = 1, shade=T, all.terms = T, xlab = "C. albimarginatus")
plot(mod.cfa.c, select = 2, shade=T, all.terms = T, xlab = "S. lewini")
plot(mod.cfa.c, select = 3, shade=T, all.terms = T, xlab = "Mes")








#### Step.gam TOB
#mod.tob <- gam(tob ~ s(slew, k = 4) + s(cga, k = 4) + s(cal, k = 4) + s(cfa, k = 4) + s(sst, k = 4) + s(sstA, k = 4) + s(chla, k = 4) + s(st, k = 4) + s(co, k = 4) + isla + sitio + temporada + mes + Estacion, data = datos.2, family = quasipoisson)
#mod.tob <- gam(tob ~ s(slew,  4) + s(cga,  4) + s(cal,  4) + s(cfa,  4) + s(sst,  4) + s(sstA,  4) + s(chla,  4) + s(st,  4) + s(co,  4) + isla + sitio + temporada + mes + Estacion, data = datos.2, family = quasipoisson)
#summary(mod.tob)

#step.tob <- step.Gam(mod.tob, scope = list("slew" = ~1 + slew + s(slew, 4), "cga" = ~1 + cga + s(cga, 4), "cal" = ~1 + cal + s(cal, 4), "cfa" = ~1 + cfa + s(cfa, 4), "sst" = ~1 + sst + s(sst, 4), "sstA" = ~1 + sstA + s(sstA, 4), "chla" = ~1 + chla + s(chla, 4), "st" = ~1 + st + s(st, 4), "co" = ~1 + co + s(co, 4), "isla" = ~1 + isla, "sitio" = ~1 + sitio, "temporada" = ~1 + temporada, "mes" = ~1 + mes, "Estacion" = ~1 + Estacion))

#??step.Gam


#### RP ####
datos.RP <- data.frame(tob = datos$ar.RP.tob, 
                      slew=datos$ar.RP.slew, 
                      cga=datos$ar.RP.cga, 
                      cal=datos$ar.RP.cal, 
                      cfa=datos$ar.RP.cfa, 
                      sst=datos$sst.RP, 
                      sstA=datos$sstA.RP,
                      chla =datos$chla.RP,
                      st=datos$st.RP,
                      co= datos$c.RP,
                      temporada = datos$Temporada,
                      mes = datos$Mes,
                      estacion = datos$Estacion)
datos.RP <- na.omit(datos.RP)

#### RP - T. obe ####
names(datos.2)
?glm
lm
glm.tob.9 <- gam(tob ~ co, data = datos.2, family = quasipoisson)
summary(glm.tob.9)
plot.gam(glm.tob.9)
plot.gam(glm.tob.9, pages = 1, scale = 0, all.terms = T)
plot(datos.2$sst, datos.2$tob)

points(glm.tob.9$fitted.values, col = 1)


mod.tob.0 <- gam(tob ~ 1, data = datos.RP, family = quasipoisson)
mod.tob.1 <- gam(tob ~ s(slew, k = 4), data = datos.RP, family = quasipoisson)
mod.tob.2 <- gam(tob ~ s(cga, k = 4), data = datos.RP, family = quasipoisson)
mod.tob.3 <- gam(tob ~ s(cal, k = 4), data = datos.RP, family = quasipoisson)
mod.tob.4 <- gam(tob ~ s(cfa, k = 4), data = datos.RP, family = quasipoisson)
mod.tob.5 <- gam(tob ~ s(sst, k = 4), data = datos.RP, family = quasipoisson)
mod.tob.6 <- gam(tob ~ s(sstA, k = 4), data = datos.RP, family = quasipoisson)
mod.tob.7 <- gam(tob ~ s(chla, k = 4), data = datos.RP, family = quasipoisson)
mod.tob.8 <- gam(tob ~ s(st, k = 4), data = datos.RP, family = quasipoisson)
mod.tob.9 <- gam(tob ~ s(co, k = 4), data = datos.RP, family = quasipoisson)
mod.tob.10 <- gam(tob ~ temporada, data = datos.RP, family = quasipoisson)
mod.tob.11 <- gam(tob ~ mes, data = datos.RP, family = quasipoisson)
mod.tob.12 <- gam(tob ~ estacion, data = datos.RP, family = quasipoisson)

summary(mod.tob.1)
(1-(mod.tob.1$deviance/mod.tob.1$null.deviance))*100
datos.RP
?summary
mod.tob.1$r.squared
mod.tob.1$
summary(get(mod.tob.1))$null.deviance
summary(mod.tob.1)$deviance

## tabla 
tabla.RP <- data.frame(Model = factor(c("slew", "cga", "cal", "cfa", "sst", "sstA", "chla", "st", "co", "temporada", "mes", "estacion"), levels = c("slew", "cga", "cal", "cfa", "sst", "sstA", "chla", "st", "co", "temporada", "mes", "estacion")), adj.R2 = NA, RSS = NA)

lista.mod <- paste0("mod.tob.", 1:12)
mod.tob.1$R
for(i in 1:12){
  tabla.RP$R2[i] <- summary(get(lista.mod[i]))$r.squared
  tabla.RP$adj.R2[i] <- summary(get(lista.mod[i]))$adj.r.squared
  tabla.RP$RSS[i] <- deviance(get(lista.mod[i]))
  tabla.RP$AIC[i] <- AIC(get(lista.mod[i]))
}
tabla.RP
summary(get(lista.mod[1]))$R-sq.(adj)



#### step.gam ####
names(mtcars)
mod.full <- gam(mpg ~ s(wt) + cyl + s(hp) + s(disp), data = mtcars)
mod.full
summary(mod.full)
step.ob <- step.Gam(mod.full, scope = list("wt" = ~1 + wt + s(wt, 4), "cyl" = ~1 + cyl, "hp" = ~1 + hp + s(hp, 4), "disp" = ~1 + disp + s(disp, 4)))
step.ob
summary(step.ob)




#### chafa ####
mod.tob <- gam(tob ~ s(sst, k = 4) + s(chla, k = 4) + sitio + mes, data = datos.2, family = quasipoisson)
mod.tob.a <- gam(tob ~ s(chla, k = 4) + sitio + mes, data = datos.2, family = quasipoisson)
mod.tob.b <- gam(tob ~ s(chla, k = 4) + sitio, data = datos.2, family = quasipoisson)
mod.tob.c <- gam(tob ~ s(chla, k = 4), data = datos.2, family = quasipoisson)

summary(mod.tob)
plot(mod.tob, pages = 0, all.terms = T)

anova(mod.tob, mod.tob.a, mod.tob.b, test = "Chisq")
anova(mod.tob, mod.tob.a, mod.tob.b, mod.tob.c, test = "Chisq")
anova(mod.tob, mod.tob.b, test = "Chisq")
anova(mod.tob.b, mod.tob, test = "Chisq")

mod.tob.sst <- gam(tob ~ s(sst, k = 4), data = datos.2, family = quasipoisson)
summary(mod.tob.sst)
plot(mod.tob.sst, pages = 1, all.terms = T)

plot(datos.2[, c("sst","tob")])
|