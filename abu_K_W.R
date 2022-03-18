#setwd("C:/Users/corte/Dropbox/maestría/RESULTADOS")
#datos.spp <- read.csv("datos/abu_spp.csv")
#head(datos.spp)
#datos.spp$Mes <- factor(datos.spp$Mes, levels = levels(datos.spp$Mes)[ c(2,3,5,1,6,4)])

# abundancia relativa ... 9:43
# man por temporadas
# 2015-2016..... 2016-2017 no hay un patron estacional marcado
# 13:46
# kruskal para ver si hay diferencias o no en los meses, cada spp es diferente, cada tempo es diferente. una general, junto todas mis abundancias y en la tesis presenta diferencias...
# 
datos.x <- read.csv("modelos/datos.csv")
datos.x$date <- as.Date(as.character(datos.x$date), format = "%d/%m/%Y")
datos.x$Temporada <- as.factor(datos.x$Temporada)
names(datos.x)

### NORMALIDAD ---------------------------------------------------------------
# shapiro.test(datos.x$ar.RP.tob)  # p = 0.81      *****
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shapiro.test(c(datos.x$ar.RP.tob, datos.x$ar.RP.slew, datos.x$ar.RP.cga, datos.x$ar.RP.cal, datos.x$ar.RP.cfa))  # p = 0.0000000005615
shapiro.test(c(datos.x$ar.SB.tob, datos.x$ar.SB.slew, datos.x$ar.SB.cga, datos.x$ar.SB.cal, datos.x$ar.SB.cfa))  # p = 0.000000001428
shapiro.test(c(datos.x$ar.So.tob, datos.x$ar.So.slew, datos.x$ar.So.cga, datos.x$ar.So.cal, datos.x$ar.So.cfa))  # p = 0.00000000005583
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#shapiro.test(mtcars$mpg)

### HOMOCEDASTICIDAD - - - - - - - - - - - - - - - - - - - - - - - - - - 
bartlett.test(datos.x$ar.RP.tob ~ datos.x$Temporada) # p = 0.069     *****
fligner.test(datos.x$ar.So.tob ~ datos.x$Temporada)  # p = 0.15      *****

### t de student - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# t.test(datos.x$ar.RP.tob ~ datos.x$Temporada) # p = 0.158     
# t.test(datos.x$ar.SB.cal ~ datos.x$Temporada) # p = 0.016     *****
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# plot(datos.x$ar.RP.tob ~ datos.x$Temporada)
# plot(datos.x$ar.SB.cal ~ datos.x$Temporada)

### Temporadas ----------------------------------------------------------
# Wilcoxon / Mann-Whitney - - - - - - - - - - - - - - - - - - - - - - - -
?wilcox.test
wilcox.test(datos.x$ar.RP.tob ~ datos.x$Temporada)  # p = 0.1807
wilcox.test(datos.x$ar.RP.slew ~ datos.x$Temporada) # p = 0.7748
wilcox.test(datos.x$ar.RP.cga ~ datos.x$Temporada)  # p = 1
wilcox.test(datos.x$ar.RP.cal ~ datos.x$Temporada)  # p = 0.1526
wilcox.test(datos.x$ar.RP.cfa ~ datos.x$Temporada)  # p = 0.3166
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
wilcox.test(datos.x$ar.SB.tob ~ datos.x$Temporada)  # p = 0.4452
wilcox.test(datos.x$ar.SB.slew ~ datos.x$Temporada) # p = 0.1526
wilcox.test(datos.x$ar.SB.cga ~ datos.x$Temporada)  # p = 0.3505
wilcox.test(datos.x$ar.SB.cal ~ datos.x$Temporada)  # p = 0.0123 *****
boxplot(datos.x$ar.SB.cal ~ datos.x$Temporada, xlab = "Temporada", ylab = "Abundancia relativa (ind./buceo)", names = F) 
axis(1, 1:2, labels = c("Dic15-Jun16", "Dic16-Jun17"))
aggregate(ar.SB.cal ~ Temporada, data = datos.x, FUN = mean)
aggregate(ar.SB.cal ~ Temporada, data = datos.x, FUN = sd)
wilcox.test(datos.x$ar.SB.cfa ~ datos.x$Temporada)  # p = 0.6121
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
wilcox.test(datos.x$ar.So.tob ~ datos.x$Temporada)  # p = 0.8357
wilcox.test(datos.x$ar.So.slew ~ datos.x$Temporada) # p = 0.4190
wilcox.test(datos.x$ar.So.cga ~ datos.x$Temporada)  # p = 0.9428
wilcox.test(datos.x$ar.So.cal ~ datos.x$Temporada)  # p = 0.5861
wilcox.test(datos.x$ar.So.cfa ~ datos.x$Temporada)  # p = 0.4618
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cal.mean <- aggregate(ar.SB.cal ~ Temporada, data = datos.x, FUN = mean)
cal.sd <-aggregate(ar.SB.cal ~ Temporada, data = datos.x, FUN = sd)
names(cal.sd) <- c("Temporada", "SD")
cal.plot <- merge(cal.mean, cal.sd)
plot(cal.plot$Temporada, cal.plot$ar.SB.cal, ylab = "Abundancia relativa", xlab = "Temporada")


aggregate(Buceo ~ Isla + Mes + Temporada, data = datos.spp.380, FUN = length)
### Especies ------------------------------------------------------------
# datos - - - - - - - - - - - - - - - - - - - - - - - -
dat.RP <- data.frame(AR = c(datos.x$ar.RP.tob, datos.x$ar.RP.slew, datos.x$ar.RP.cga, datos.x$ar.RP.cal, datos.x$ar.RP.cfa), sp = c(rep("tob", 13), rep("slew", 13), rep("cga", 13), rep("cal", 13), rep("cfa", 13)), mes = datos.x$Mes) 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
dat.SB <- data.frame(AR = c(datos.x$ar.SB.tob, datos.x$ar.SB.slew, datos.x$ar.SB.cga, datos.x$ar.SB.cal, datos.x$ar.SB.cfa), sp = c(rep("tob", 13), rep("slew", 13), rep("cga", 13), rep("cal", 13), rep("cfa", 13)), mes = datos.x$Mes)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
dat.So <- data.frame(AR = c(datos.x$ar.So.tob, datos.x$ar.So.slew, datos.x$ar.So.cga, datos.x$ar.So.cal, datos.x$ar.So.cfa), sp = c(rep("tob", 13), rep("slew", 13), rep("cga", 13), rep("cal", 13), rep("cfa", 13)), mes = datos.x$Mes)
 
# Kruskall  - - - - - - - - - - - - - - - - - - - - - - - - - - - -
library(dunn.test)
# Kruskall por meses para cada isla - - - - - - - - - - - - - - - -
kruskal.test(dat.RP$AR ~ dat.RP$mes) # p = 0.28297
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
kruskal.test(dat.SB$AR ~ dat.SB$mes) # p = 0.8728   
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
kruskal.test(dat.So$AR ~ dat.So$mes) # p = 0.7956

# Kruskall por meses para cada sp - - - - - - - - - - - - - - - - -
kruskal.test(AR ~ mes, data = subset(dat.RP, sp == "tob"))
kruskal.test(AR ~ mes, data = subset(dat.RP, sp == "slew"))
kruskal.test(AR ~ mes, data = subset(dat.RP, sp == "cga"))
kruskal.test(AR ~ mes, data = subset(dat.RP, sp == "cal"))
kruskal.test(AR ~ mes, data = subset(dat.RP, sp == "cfa"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
kruskal.test(AR ~ mes, data = subset(dat.SB, sp == "tob"))
kruskal.test(AR ~ mes, data = subset(dat.SB, sp == "slew"))
kruskal.test(AR ~ mes, data = subset(dat.SB, sp == "cga"))
kruskal.test(AR ~ mes, data = subset(dat.SB, sp == "cal"))
kruskal.test(AR ~ mes, data = subset(dat.SB, sp == "cfa"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
kruskal.test(AR ~ mes, data = subset(dat.So, sp == "tob"))
kruskal.test(AR ~ mes, data = subset(dat.So, sp == "slew"))
kruskal.test(AR ~ mes, data = subset(dat.So, sp == "cga"))
kruskal.test(AR ~ mes, data = subset(dat.So, sp == "cal"))
kruskal.test(AR ~ mes, data = subset(dat.So, sp == "cfa"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Kruskall por especies para cada isla - - - - - - - - - - - - - - - - -
kruskal.test(dat.RP$AR ~ dat.RP$sp) # p = 5.566e-07 ***** 
dunn.test(dat.RP$AR, dat.RP$sp)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
kruskal.test(dat.SB$AR ~ dat.SB$sp) # p = 4.631e-05 *****
dunn.test(dat.SB$AR, dat.SB$sp)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
kruskal.test(dat.So$AR ~ dat.So$sp) # p = 2.456e-05 *****
dunn.test(dat.So$AR, dat.So$sp)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -


x11(); par(mfrow=c(2,2))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
boxplot(dat.RP$AR ~ dat.RP$sp, names = F, ylab = "Abundancia relativa (ind./buceo)")
axis(1, at=1:5, labels = c("C. alb", "C. fal", "C. gal", "S. lew", "T. obe"), font = 3)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
boxplot(dat.SB$AR ~ dat.SB$sp, names = F, ylab = "Abundancia relativa (ind./buceo)")
axis(1, at=1:5, labels = c("C. alb", "C. fal", "C. gal", "S. lew", "T. obe"), font = 3)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
boxplot(dat.So$AR ~ dat.So$sp, names = F, ylab = "Abundancia relativa (ind./buceo)")
axis(1, at=1:5, labels = c("C. alb", "C. fal", "C. gal", "S. lew", "T. obe"), font = 3)
?plot
?axis


class(datos.x$Temporada)
?integer
kw.isla <- kruskal.test(Abundancia ~ Isla, data = datos.2)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


shapiro.test(datos$mpg)

bartlett.test(mpg ~ cyl, data = datos)
fligner.test(mpg ~ cyl, data = datos)

bartlett.test(mpg ~ am, data = datos)
fligner.test(mpg ~ am, data = datos)

anova.1a <- aov(mpg ~ cyl, data = datos)
anova.1b <- aov(mpg ~ am, data = datos)

anova.cyl <- aov(mpg ~ cyl, data = datos) #el nombre depende de mi, igual a linea 27
anova.am <- aov(mpg ~ am, data = datos)

summary(anova.1a)
summary(anova.1b)

anova.2 <- aov(mpg ~ cyl + am, data = datos)
summary(anova.2)

anova.f <- aov(mpg ~ cyl * am, data = datos) #factorial
summary(anova.f)

TukeyHSD(anova.1a)
TukeyHSD(anova.1b)
TukeyHSD(anova.2)
TukeyHSD(anova.f)

capture.output(summary(anova.f), file="analisis de varianza.doc")
capture.output(TukeyHSD(anova.f), file = "analisis de Tukey.doc")

#### graficas ####
boxplot(mpg ~ cyl + am, data = datos)
boxplot(mpg ~ cyl * am, data = datos)









### parametros graficos ---------------------------------------------------
sitios.abr <- scale_x_discrete(breaks = c("Cabo Pearce", "Cuevitas", "El Boiler", "El Cañón", "Punta SO", "Punta Tosca", "Roca Oneal", "Roca Partida", "The Old Man of the Rock"), labels = c("CP", "LC", "EB", "EC", "PSO", "PT", "RO", "RP", "TO")) # ETIQUETAS DE SITIOS ABREVIADAS
sitios90 <- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


# plots -----------------------------------------------------------------

ppi <- 600
png("C:/Users/corte/Dropbox/maestría/RESULTADOS/figuras/rel_mes_sp.png", width = 9*ppi, height = 6*ppi, res = ppi) # W=ancho, H=alto
ggplot(a.n.mes.sp, aes(x = factor(Mes2), y = rel, colour = Codigo, group = Codigo)) + geom_line() + geom_point(size = 2) + labs(x = "Mes", y = "Abundancia relativa", colour = "Especie")  # pagina 230
dev.off()
