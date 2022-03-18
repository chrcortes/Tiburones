setwd("C:/Users/corte/Dropbox/maestría/RESULTADOS/datos/")
datos.gam <- read.csv("datos.gam.raw.csv")
datos.gam$mes <- factor(datos.gam$mes, levels = levels(datos.gam$mes)[ c(2,3,4,6,1,7,5)])
names(datos.gam)

# estadisticos de prueba ####
library(ggplot2)
theme_set(theme_bw())
library(dunn.test)
datos.kw <- stack(datos.gam[, 1:5])
names(datos.kw) <- c("abu", "sp")
datos.kw$isla <- datos.gam$isla
datos.kw$sitio <- datos.gam$sitio
datos.kw$mes <- datos.gam$mes
datos.kw$temporada <- factor(datos.gam$temporada, levels = c(1, 2), labels = c("Dic15-Jun16", "Dic16-Jun17"))
datos.kw$spp <- datos.kw$sp
head(datos.kw)

#levels(datos.kw$isla) <- c("RP", "SB", "SO")
levels(datos.kw$mes) <- c("12", "1", "2", "3", "4", "5", "6")
levels(datos.kw$spp) <- c("T. obesus", "S. lewini", "C. galapagensis", "C. albimarginatus", "C. falciformis")


# normalidad y homocedasticidad
shapiro.test(datos.kw$abu) # W = 0.46021, p-value < 2.2e-16
bartlett.test(abu ~ isla, data = datos.kw) # Bartlett's K-squared = 397.86, df = 2, p-value < 2.2e-16

# kruskal.test
#### 1.a. General Islas ####
kruskal.test(abu ~ isla, data = datos.kw)
dunn.test(datos.kw$abu, datos.kw$isla)

boxplot(abu ~ isla, data = datos.kw, pch = 20)
aggregate(abu ~ isla, FUN = median, data = datos.kw)

ggplot(datos.kw, aes(isla, abu)) + geom_boxplot() # QUITAR CUADRICULA (HORIZONTAS-VERTICAL CON: + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

palette.islas <- c("#f46038", "#2f294f", "#1b998b")
ggplot(datos.kw, aes(isla, abu, fill = isla)) + geom_boxplot() + xlab("Isla") + ylab("Abundancia relativa\n(ind./buceo)") +
  scale_fill_manual(values = palette.islas, labels=c("Roca Partida", "San Benedicto", "Socorro")) + 
  labs(fill = "Isla") + 
  theme(axis.text.x = element_blank()) + 
  scale_y_continuous(expand = c(0, 1))
#ggsave("C:/Users/corte/Dropbox/maestría/RESULTADOS/figuras/1a_general_islas.png", width = 7, height = 6)

#### 1.b. spp Islas ####
kruskal.test(abu ~ sp, data = subset(datos.kw, isla == "RP"))
kruskal.test(abu ~ sp, data = subset(datos.kw, isla == "SB"))
kruskal.test(abu ~ sp, data = subset(datos.kw, isla == "So"))
# aggregate(abu ~  sp + isla, FUN = median, data = datos.kw)
round(tapply(datos.kw$abu, list(datos.kw$sp, datos.kw$isla), median), 1)
round(tapply(datos.kw$abu, list(datos.kw$sp, datos.kw$isla), max), 1)

data.RP <- subset(datos.kw, isla == "RP")
data.SB <- subset(datos.kw, isla == "SB")
data.So <- subset(datos.kw, isla == "So")
dunn.test(data.RP$abu, data.RP$sp)
dunn.test(data.SB$abu, data.SB$sp)
dunn.test(data.So$abu, data.So$sp)

palette.spp <- c("#f46038", "#2f294f", "#1b998b", "#e71d35", "#eb952f")
ggplot(datos.kw, aes(sp, abu, fill = spp)) + geom_boxplot() + facet_wrap(~isla) + xlab("Especie") + ylab("Abundancia relativa\n(ind./buceo)") + 
  scale_fill_manual(values = palette.spp) + 
  theme(legend.text = element_text(face="italic")) + 
  labs(fill = "Especie") + 
  theme(axis.text.x = element_blank()) + 
  scale_y_continuous(expand = c(0, 1))
#ggsave("C:/Users/corte/Dropbox/maestría/RESULTADOS/figuras/1b_spp_islas.png", width = 7, height = 5, dpi = 600)

# Ahora sin RP (hacer zoom a SB y So)
datos.kw.SB <- subset(datos.kw, isla == "SB")
datos.kw.So <- subset(datos.kw, isla == "So")
datos.kw2 <- rbind(datos.kw.SB,datos.kw.So)

ggplot(datos.kw2, aes(sp, abu, fill = spp)) + geom_boxplot() + facet_wrap(~isla) + xlab("Especie") + ylab("Abundancia relativa\n(ind./buceo)") + 
  scale_fill_manual(values = palette.spp) + 
  theme(legend.text = element_text(face="italic")) + 
  labs(fill = "Especie") + 
  theme(axis.text.x = element_blank()) + 
  scale_y_continuous(expand = c(0, 1))
#ggsave("C:/Users/corte/Dropbox/maestría/RESULTADOS/figuras/1b_spp_islas2.png", width = 5, height = 5, dpi = 600)


#### 2.a. General Temporadas ####
wilcox.test(datos.kw$abu ~ datos.kw$temporada)  # p = 0.1807
boxplot(abu ~ temporada, data = datos.kw, pch = 20)

#### 2.b. spp Islas Temporadas ####
wilcox.test(abu ~ temporada, data = subset(datos.kw, sp == "tob" & isla == "RP"))
wilcox.test(abu ~ temporada, data = subset(datos.kw, sp == "slew" & isla == "RP"))
wilcox.test(abu ~ temporada, data = subset(datos.kw, sp == "cga" & isla == "RP"))
wilcox.test(abu ~ temporada, data = subset(datos.kw, sp == "cal" & isla == "RP"))
wilcox.test(abu ~ temporada, data = subset(datos.kw, sp == "cfa" & isla == "RP"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
wilcox.test(abu ~ temporada, data = subset(datos.kw, sp == "tob" & isla == "SB"))
wilcox.test(abu ~ temporada, data = subset(datos.kw, sp == "slew" & isla == "SB"))
wilcox.test(abu ~ temporada, data = subset(datos.kw, sp == "cga" & isla == "SB"))
wilcox.test(abu ~ temporada, data = subset(datos.kw, sp == "cal" & isla == "SB"))
wilcox.test(abu ~ temporada, data = subset(datos.kw, sp == "cfa" & isla == "SB"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
wilcox.test(abu ~ temporada, data = subset(datos.kw, sp == "tob" & isla == "So"))
wilcox.test(abu ~ temporada, data = subset(datos.kw, sp == "slew" & isla == "So"))
wilcox.test(abu ~ temporada, data = subset(datos.kw, sp == "cga" & isla == "So"))
wilcox.test(abu ~ temporada, data = subset(datos.kw, sp == "cal" & isla == "So"))
wilcox.test(abu ~ temporada, data = subset(datos.kw, sp == "cfa" & isla == "So"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# aggregate(abu ~  sp + isla, FUN = median, data = datos.kw)
round(tapply(datos.kw$abu, list(datos.kw$sp, datos.kw$temporada, datos.kw$isla), median), 1)

ggplot(datos.kw, aes(temporada, abu, fill = spp)) + geom_boxplot() + facet_wrap(~isla) + xlab("Temporada") + ylab("Abundancia relativa\n(ind./buceo)") + 
  labs(fill = "Especie") + 
  scale_fill_manual(values = palette.spp) + 
  theme(legend.text=element_text(face="italic")) + 
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1)) + 
  scale_y_continuous(expand = c(0, 1))
#ggsave("C:/Users/corte/Dropbox/maestría/RESULTADOS/figuras/2b_spp_islas_temp.png", width = 7, height = 5, dpi = 600)

#### 2.c. General Meses ####
kruskal.test(abu ~ mes, data = datos.kw)
boxplot(abu ~ mes, data = datos.kw, pch = 20)

#### 2.c-d. General Meses Islas ####
kruskal.test(abu ~ mes, data = subset(datos.kw, isla == "RP"))
kruskal.test(abu ~ mes, data = subset(datos.kw, isla == "SB"))
kruskal.test(abu ~ mes, data = subset(datos.kw, isla == "So"))

#### 2.d. spp islas Meses ####
kruskal.test(abu ~ mes, data = subset(datos.kw, sp == "tob" & isla == "RP"))
kruskal.test(abu ~ mes, data = subset(datos.kw, sp == "slew" & isla == "RP"))
kruskal.test(abu ~ mes, data = subset(datos.kw, sp == "cga" & isla == "RP"))
kruskal.test(abu ~ mes, data = subset(datos.kw, sp == "cal" & isla == "RP"))
kruskal.test(abu ~ mes, data = subset(datos.kw, sp == "cfa" & isla == "RP"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
kruskal.test(abu ~ mes, data = subset(datos.kw, sp == "tob" & isla == "SB"))
kruskal.test(abu ~ mes, data = subset(datos.kw, sp == "slew" & isla == "SB"))
kruskal.test(abu ~ mes, data = subset(datos.kw, sp == "cga" & isla == "SB"))
kruskal.test(abu ~ mes, data = subset(datos.kw, sp == "cal" & isla == "SB"))
kruskal.test(abu ~ mes, data = subset(datos.kw, sp == "cfa" & isla == "SB"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
kruskal.test(abu ~ mes, data = subset(datos.kw, sp == "tob" & isla == "So"))
kruskal.test(abu ~ mes, data = subset(datos.kw, sp == "slew" & isla == "So"))
kruskal.test(abu ~ mes, data = subset(datos.kw, sp == "cga" & isla == "So"))
kruskal.test(abu ~ mes, data = subset(datos.kw, sp == "cal" & isla == "So"))
kruskal.test(abu ~ mes, data = subset(datos.kw, sp == "cfa" & isla == "So"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ggplot(datos.kw, aes(mes, abu, fill = spp)) + geom_boxplot() + facet_wrap( ~ isla, nrow = 2) + xlab("Mes") + ylab("Abundancia relativa\n(ind./buceo)") + labs(fill = "Especie")  + theme(legend.text = element_text(face = "italic")) +
  scale_fill_manual(values = palette.spp) + 
  scale_y_continuous(expand = c(0, 1)) +
  theme(legend.position=c(0.9, 0.1), legend.justification=c(1,0))

# Sin RP
ggplot(datos.kw, aes(mes, abu, fill = spp)) + geom_boxplot() + facet_wrap( ~ isla, nrow = 1) + xlab("Mes") + ylab("Abundancia relativa\n(ind./buceo)") + labs(fill = "Especie")  + theme(legend.position="none") +
  scale_fill_manual(values = palette.spp) + 
  scale_y_continuous(expand = c(0, 1)) + ylim(0,23)

#ggsave("C:/Users/corte/Dropbox/maestría/RESULTADOS/figuras/2d_spp_islas_mes2.png", width = 7, height = 4, dpi = 600)

# EJEMPLO METODOS
datos.kw.RP <- subset(datos.kw, isla == "RP")
ggplot(datos.kw.RP, aes(isla, abu, fill = spp)) + geom_boxplot() + ylab("Abundancia relativa\n(ind./buceo)") + labs(fill = "Especie")  + theme(legend.text = element_text(face = "italic")) +
  scale_fill_manual(values = palette.spp)
#ggsave("C:/Users/corte/Dropbox/maestría/RESULTADOS/figuras/3ejemplo_metodo.png", width = 5, height = 5, dpi = 300)



kruskal.test(abu ~ temporada, data = subset(datos.kw, isla == "RP"))
kruskal.test(abu ~ temporada, data = subset(datos.kw, sp == "tob" & isla == "RP"))

aggregate(abu ~ temporada + mes + sp, FUN = sum, data = datos.kw)

aggregate(abu ~ sp + isla, FUN = median, data = datos.kw)


#### graficas ####
ggplot(datos.kw, aes(mes, abu, fill = spp)) + geom_boxplot() + facet_wrap(~isla) + xlab("Mes") + ylab("Abundancia relativa\n(ind./buceo)") + labs(fill = "Especies")  + theme(legend.text=element_text(face="italic")) 

# la misma pero apiladas
ggplot(datos.kw, aes(mes, abu, fill = spp)) + geom_boxplot() + facet_wrap(~isla) + xlab("Mes") + ylab("Abundancia relativa\n(ind./buceo)") + labs(fill = "Especies")  + theme(legend.text=element_text(face="italic")) + facet_grid(isla ~ .)

palette.spp <- c("#f46038", "#2f294f", "#1b998b", "#e71d35", "#eb952f")
ggplot(datos.kw, aes(mes, abu, fill = spp)) + geom_boxplot() + facet_wrap(~isla) + xlab("Mes") + ylab("Abundancia relativa\n(ind./buceo)") + labs(fill = "Especies")  + theme(legend.text=element_text(face="italic")) + scale_fill_manual(values = palette.spp)

ggplot(datos.kw, aes(mes, abu, fill = spp)) + geom_boxplot() + facet_wrap(~isla) + xlab("Mes") + ylab("Abundancia relativa\n(ind./buceo)") + labs(fill = "Especies")  + theme(legend.text=element_text(face="italic")) + facet_grid(isla ~ .) + scale_fill_manual(values = palette.spp)


ggplot(datos.kw, aes(mes, abu, fill = sp)) + geom_boxplot() + xlab("Mes") + ylab("Abundancia relativa")


ggplot(datos.kw, aes(sp, abu, fill = spp)) + geom_boxplot() + facet_wrap(isla~.) + xlab("Mes") + ylab("Abundancia relativa\n(ind./buceo)") + labs(fill = "Especies")  + theme(legend.text=element_text(face="italic")) 



datos.gam$pos <- datos.gam$sstA >= 0
ggplot(datos.gam, aes(mes, sstA, fill = pos)) + geom_bar(stat="identity", position="identity") + facet_wrap(sitio~temporada, ncol=6) + scale_fill_manual(values=c("#1b998b", "#e71d35"))


datos.gam




ggplot(datos.kw, aes(temporada, abu, fill = sp)) + geom_boxplot() + facet_wrap(~isla) + xlab("Mes") + ylab("Abundancia relativa")

ggplot(datos.kw, aes(mes, abu, shape = sp)) + stat_summary(fun.data = "mean_cl_normal") + facet_wrap(~isla) + xlab("Mes") + ylab("Abundancia relativa")

ggplot(datos.kw, aes(mes, abu, shape = sp)) + stat_summary(fun.data = "mean_se") + facet_wrap(~isla) + xlab("Mes") + ylab("Abundancia relativa")

ggplot(datos.kw, aes(mes, abu, shape = sp)) + stat_summary(fun.data = "median_hilow") + facet_wrap(~isla) + xlab("Mes") + ylab("Abundancia relativa")












#### PCA ####
library(factoextra)
library(FactoMineR)
datos.pca <- datos.gam
datos.pca <- na.omit(datos.pca)

labels <- datos.pca[ , 11]

datos.pca <- datos.pca[ , c(1:10, 16:19)]

pca.gam <- PCA(datos.pca, scale.unit = T, graph = F)
fviz_pca_ind(pca.gam, geom = "point", habillage = labels)
fviz_pca_var(pca.gam) 
fviz_pca_biplot(pca.gam, geom = "point", habillage = labels) 

fviz_contrib(pca.gam, choice = "ind") 


#### LM T. obe ####
# lm(y ~ x)
lm.tob.co <- lm(tob ~ co , data = datos.gam)
summary(lm.tob.co)
plot(tob ~ co , data = datos.gam)
abline(lm.tob.co)

lm.tob.1 <- lm(tob ~ slew, data = datos.spp.380)
plot(datos.pca.380$tob, datos.pca.380$slew)
abline(lm.tob.1)
cor.test(datos.pca.380$tob, datos.pca.380$slew)

# RP
head(datos.gam)
datos.gam.RP <- datos.gam[datos.gam$isla =="RP", ]
head(datos.gam.RP)

lm.RP.tob.1 <- lm(tob ~ slew, data = datos.gam.RP)
plot(datos.gam.RP$slew, datos.gam.RP$tob)
abline(lm.RP.tob.1)
cor.test(datos.gam.RP$tob, datos.gam.RP$slew)

# GAM.RP
library(mgcv)
gam.RP.1 <- gam(tob ~ s(slew), data = datos.gam.RP, family = poisson)
summary(gam.RP.1)

par(mfrow = c(1,2))
plot(datos.gam.RP$slew, datos.gam.RP$tob)
abline(lm.RP.tob.1)
plot(gam.RP.1)

# 396
datos.396 <- read.csv("datos_396.csv")
datos.396.2 <- na.omit(datos.396)
names(datos.396.2)
labels.396 <- datos.396.2[ , 2]
datos.pca.396 <- datos.396.2[ , c(7:17)]

pca.396 <- PCA(datos.pca.396, scale.unit = T, graph = F)
fviz_pca_ind(pca.396, geom = "point", habillage = labels.396)
fviz_pca_var(pca.396) 
fviz_pca_biplot(pca.396, geom = "point", habillage = labels.396) 

# 396 RP
datos.396.RP <- datos.396.2[datos.396.2$isla =="Roca Partida", ]
labels.396.RP <- datos.396.RP[ , 2]
datos.pca.396.RP <- datos.396.RP[ , c(7:17)]

pca.396.RP <- PCA(datos.pca.396.RP, scale.unit = T, graph = F)
fviz_pca_ind(pca.396.RP, geom = "point", habillage = labels.396.RP)
fviz_pca_var(pca.396.RP) 
fviz_pca_biplot(pca.396.RP, geom = "point", habillage = labels.396.RP)

# lm(y ~ x)
lm.396.RP.tob.1 <- lm(tob ~ st, data = datos.396.RP)
plot(datos.396.RP$st, datos.396.RP$tob)
abline(lm.396.RP.tob.1)
cor.test(datos.396.RP$tob, datos.396.RP$st)

lm.396.RP.tob.2 <- lm(tob ~ co, data = datos.396.RP)
plot(datos.396.RP$co, datos.396.RP$tob)
abline(lm.396.RP.tob.2)
cor.test(datos.396.RP$tob, datos.396.RP$co)




#### T. obe ####
names(datos.spp.380)
mod.tob.0 <- gam(tob ~ 1, data = datos.2, family = poisson)
mod.tob.1 <- gam(tob ~ Isla, data = datos.spp.380, family = quasipoisson)
mod.tob.2 <- gam(tob ~ Sitio, data = datos.spp.380, family = poisson)
mod.tob.3 <- gam(tob ~ Temporada, data = datos.spp.380, family = poisson)
mod.tob.4 <- gam(tob ~ Mes, data = datos.spp.380, family = poisson)
mod.tob.5 <- gam(tob ~ s(cal), data = datos.spp.380, family = poisson)
mod.tob.6 <- gam(tob ~ s(cfa), data = datos.spp.380, family = poisson)
mod.tob.7 <- gam(tob ~ s(cga), data = datos.spp.380, family = poisson)
mod.tob.8 <- gam(tob ~ s(cli, k = 3), data = datos.spp.380, family = poisson)
mod.tob.9 <- gam(tob ~ s(cob, k = 3), data = datos.spp.380, family = poisson)
mod.tob.10 <- gam(tob ~ s(slew), data = datos.spp.380, family = poisson)

dev.exp <- function(x){
  nul <- x$null.deviance
  res <- x$deviance
  (1-(res/nul))*100
}
dev.exp(mod.tob.1)

mod.tob.full <- gam(tob ~ Isla + Sitio + Temporada + Mes + s(cal) + s(cfa) + s(cga) + s(cli, k = 3) + s(cob, k = 3) + s(slew) , data = datos.spp.380, family = poisson)
dev.exp(mod.tob.full)
?step.wise
library(gam)
?step.Gam
mod.tob.step <- step.Gam(mod.tob.full)
summary(mod.tob.step)

#mod.tob.1$null.deviance
#deviance(mod.tob.1) # Devianza residual
#logLik.gam(mod.tob.1)
#gam.check(mod.tob.1)


# Seleccion del mejor modelo
summary(mod.tob.0)
mod.tob.a <- gam(tob ~ isla, data = datos.2, family = quasipoisson)
summary(mod.tob.a)
anova(mod.tob.0, mod.tob.a, test = "Chisq")

mod.tob.b <- gam(tob ~ isla + s(chla, k = 4), data = datos.2, family = quasipoisson)
summary(mod.tob.b)
anova(mod.tob.a, mod.tob.b, test = "Chisq")
