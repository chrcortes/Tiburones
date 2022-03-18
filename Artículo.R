setwd("C:/Users/corte/Dropbox/maestría/RESULTADOS/R Markdown/")
#datos <- read.csv("Base_final_15-17.csv")
#datos$Fecha <- as.Date(as.character(datos$Fecha), format = "%d/%m/%Y")
#datos$Mes <- factor(datos$Mes, levels = levels(datos$Mes)[ c(2,3,4,6,1,7,5)])
#names(datos)

datos <- read.csv("Base_final_15-17-fechas.csv")
datos$fecha <- as.Date(as.character(datos$fecha), format = "%d/%m/%Y")
datos$mes <- factor(datos$mes, levels = levels(datos$mes)[ c(2,3,4,6,1,7,5)])
names(datos)

library(ggplot2)
theme_set(theme_bw())


#### ABUNDANCIA ####
datos.abu <- stack(datos[, 6:10])
names(datos.abu) <- c("abu", "sp")
datos.abu$isla <- datos$isla
datos.abu$sitio <- datos$sitio
datos.abu$fecha <- datos$fecha
datos.abu$mes <- datos$mes
datos.abu$temporada <- factor(datos$temporada, levels = c(1, 2), labels = c("Dic15-Jun16", "Dic16-Jun17"))
datos.abu$spp <- datos.abu$sp
#levels(datos.abu$isla) <- c("RP", "SB", "SO")
levels(datos.abu$mes) <- c("12", "1", "2", "3", "4", "5", "6")
levels(datos.abu$spp) <- c("T. obesus", "S. lewini", "C. galapagensis", "C. albimarginatus", "C. falciformis")
head(datos.abu)

#### exploratorio ####
# nuevos datos
datos.explo <- stack(datos[, 6:14])
names(datos.explo) <- c("abu", "sp")
datos.explo$isla <- datos$isla
datos.explo$sitio <- datos$sitio
datos.explo$fecha <- datos$fecha
levels(datos.explo$sp) <- c("T. obesus", "S. lewini", "C. galapagensis", "C. albimarginatus", "C. falciformis", "G. cuvier", "R. typus", "C. limbatus", "C. obscurus")
head(datos.explo)

# % ocurrencias
datos.explo$ocu <- ifelse(datos.explo$abu > 0, 1, 0)
head(datos.explo)
abu.ocu <- aggregate(ocu ~ sp, data = datos.explo, FUN = sum)
abu.ocu$ocux100 <- abu.ocu$ocu / (length(datos.explo$ocu)/9) * 100

# % abundancia relativa
abu <- aggregate(abu ~ sp, data = datos.explo, FUN = sum)
abu.ocu <- merge(abu.ocu, abu)
abu.ocu$arx100 <- abu.ocu$abu / sum(datos.explo$abu) * 100
abu.ocu

ggplot(abu.ocu, aes(ocux100, arx100, shape=sp)) +
  geom_point(size = 2.5) + scale_shape_manual(values=c(8, 15:18, 0:2, 5)) +
  geom_hline(yintercept=4, linetype="dashed") + 
  geom_vline(xintercept=39,linetype="dashed") # 
  annotate("text", x=(abu.ocu$ocux100+15), y=(abu.ocu$arx100+1), family="serif",
           fontface="italic", label=abu.ocu$sp) + ylim(0,100)

quantile(abu.ocu$arx100, 0.25)
quantile(abu.ocu$ocux100, 0.25)
quantile(abu.ocu$arx100, 0.5)
quantile(abu.ocu$ocux100, 0.5)



# normalidad y homocedasticidad
shapiro.test(datos.abu$abu) # W = 0.2588, p-value < 2.2e-16
bartlett.test(abu ~ isla, data = datos.abu) # Bartlett's K-squared = 4659.9, df = 2, p-value < 2.2e-16

library(dunn.test)

# kruskal.test
#### 1.a. General Islas ####
kruskal.test(abu ~ isla, data = datos.abu)
dunn.test(datos.abu$abu, datos.abu$isla)

ggplot(datos.abu, aes(isla, abu)) + geom_boxplot() # QUITAR CUADRICULA (HORIZONTAS-VERTICAL CON: + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))
aggregate(abu ~ isla, FUN = median, data = datos.abu)

palette.islas <- c("#f46038", "#2f294f", "#1b998b")
ggplot(datos.abu, aes(isla, abu, fill = isla)) + geom_boxplot() + xlab("Isla") + ylab("Abundancia relativa\n(ind./buceo)") +
  scale_fill_manual(values = palette.islas, labels=c("Roca Partida", "San Benedicto", "Socorro")) + 
  labs(fill = "Isla") + 
  theme(axis.text.x = element_blank()) + 
  scale_y_continuous(expand = c(0, 1))
#ggsave("C:/Users/corte/Dropbox/maestría/RESULTADOS/figuras/1a_general_islas.png", width = 7, height = 6)

#### 1.b. spp Islas ####
kruskal.test(abu ~ sp, data = subset(datos.abu, isla == "Roca Partida"))
kruskal.test(abu ~ sp, data = subset(datos.abu, isla == "San Benedicto"))
kruskal.test(abu ~ sp, data = subset(datos.abu, isla == "Socorro"))
# aggregate(abu ~  sp + isla, FUN = median, data = datos.abu)
round(tapply(datos.abu$abu, list(datos.abu$sp, datos.abu$isla), median), 1)
round(tapply(datos.abu$abu, list(datos.abu$sp, datos.abu$isla), max), 1)

data.RP <- subset(datos.abu, isla == "Roca Partida")
data.SB <- subset(datos.abu, isla == "San Benedicto")
data.So <- subset(datos.abu, isla == "Socorro")
dunn.test(data.RP$abu, data.RP$sp)
dunn.test(data.SB$abu, data.SB$sp)
dunn.test(data.So$abu, data.So$sp)

palette.spp <- c("#f46038", "#2f294f", "#1b998b", "#e71d35", "#eb952f")
ggplot(datos.abu, aes(sp, abu, fill = spp)) + geom_boxplot() + facet_wrap(~isla) + xlab("Especie") + ylab("Abundancia relativa\n(ind./buceo)") + 
  scale_fill_manual(values = palette.spp) + 
  theme(legend.text = element_text(face="italic")) + 
  labs(fill = "Especie") + 
  theme(axis.text.x = element_blank()) + 
  scale_y_continuous(expand = c(0, 1))
#ggsave("C:/Users/corte/Dropbox/maestría/RESULTADOS/figuras/1b_spp_islas.png", width = 7, height = 5, dpi = 600)

# Ahora sin RP (hacer zoom a SB y So)
datos.abu.SB <- subset(datos.abu, isla == "San Benedicto")
datos.abu.So <- subset(datos.abu, isla == "Socorro")
datos.abu2 <- rbind(datos.abu.SB,datos.abu.So)

ggplot(datos.abu2, aes(sp, abu, fill = spp)) + geom_boxplot() + facet_wrap(~isla) + xlab("Especie") + ylab("Abundancia relativa\n(ind./buceo)") + 
  scale_fill_manual(values = palette.spp) + 
  theme(legend.text = element_text(face="italic")) + 
  labs(fill = "Especie") + 
  theme(axis.text.x = element_blank()) + 
  scale_y_continuous(expand = c(0, 1))
#ggsave("C:/Users/corte/Dropbox/maestría/RESULTADOS/figuras/1b_spp_islas2.png", width = 5, height = 5, dpi = 600)


#### 2.a. General Temporadas ####
wilcox.test(datos.abu$abu ~ datos.abu$temporada)  # p = 0.1807
boxplot(abu ~ temporada, data = datos.abu, pch = 20)

#### 2.b. spp Islas Temporadas ####
wilcox.test(abu ~ temporada, data = subset(datos.abu, sp == "tob" & isla == "Roca Partida")) #p<0.05 OBESUS
wilcox.test(abu ~ temporada, data = subset(datos.abu, sp == "slew" & isla == "Roca Partida")) 
wilcox.test(abu ~ temporada, data = subset(datos.abu, sp == "cga" & isla == "Roca Partida")) #p<0.05 GALAPAGOS
wilcox.test(abu ~ temporada, data = subset(datos.abu, sp == "cal" & isla == "Roca Partida")) #p<0.05 ALBIMARGINATUS
wilcox.test(abu ~ temporada, data = subset(datos.abu, sp == "cfa" & isla == "Roca Partida"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
wilcox.test(abu ~ temporada, data = subset(datos.abu, sp == "tob" & isla == "San Benedicto"))
wilcox.test(abu ~ temporada, data = subset(datos.abu, sp == "slew" & isla == "San Benedicto"))
wilcox.test(abu ~ temporada, data = subset(datos.abu, sp == "cga" & isla == "San Benedicto"))
wilcox.test(abu ~ temporada, data = subset(datos.abu, sp == "cal" & isla == "San Benedicto"))
wilcox.test(abu ~ temporada, data = subset(datos.abu, sp == "cfa" & isla == "San Benedicto"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
wilcox.test(abu ~ temporada, data = subset(datos.abu, sp == "tob" & isla == "Socorro"))
wilcox.test(abu ~ temporada, data = subset(datos.abu, sp == "slew" & isla == "Socorro"))
wilcox.test(abu ~ temporada, data = subset(datos.abu, sp == "cga" & isla == "Socorro"))
wilcox.test(abu ~ temporada, data = subset(datos.abu, sp == "cal" & isla == "Socorro"))
wilcox.test(abu ~ temporada, data = subset(datos.abu, sp == "cfa" & isla == "Socorro"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# aggregate(abu ~  sp + isla, FUN = median, data = datos.abu)
round(tapply(datos.abu$abu, list(datos.abu$sp, datos.abu$temporada, datos.abu$isla), median), 1)

ggplot(datos.abu, aes(temporada, abu, fill = spp)) + geom_boxplot() + facet_wrap(~isla) + xlab("Temporada") + ylab("Abundancia relativa\n(ind./buceo)") + 
  labs(fill = "Especie") + 
  scale_fill_manual(values = palette.spp) + 
  theme(legend.text=element_text(face="italic")) + 
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1)) + 
  scale_y_continuous(expand = c(0, 1))
#ggsave("C:/Users/corte/Dropbox/maestría/RESULTADOS/figuras/2b_spp_islas_temp.png", width = 7, height = 5, dpi = 600)

#### 2.c. General Meses ####
kruskal.test(abu ~ mes, data = datos.abu)
boxplot(abu ~ mes, data = datos.abu, pch = 20)

#### 2.c-d. General Meses Islas ####
kruskal.test(abu ~ mes, data = subset(datos.abu, isla == "Roca Partida")) #p<0.05
kruskal.test(abu ~ mes, data = subset(datos.abu, isla == "San Benedicto"))
kruskal.test(abu ~ mes, data = subset(datos.abu, isla == "Socorro"))
boxplot(abu ~ mes, data = subset(datos.abu, isla == "Roca Partida"), pch = 20)
dunn.test(data.RP$abu, data.RP$mes)

#### 2.d. spp islas Meses ####
kruskal.test(abu ~ mes, data = subset(datos.abu, sp == "tob" & isla == "Roca Partida"))
kruskal.test(abu ~ mes, data = subset(datos.abu, sp == "slew" & isla == "Roca Partida"))
kruskal.test(abu ~ mes, data = subset(datos.abu, sp == "cga" & isla == "Roca Partida")) #p<0.05
boxplot(abu ~ mes, data = subset(datos.abu, sp == "cga" & isla == "Roca Partida"), pch = 20)
kruskal.test(abu ~ mes, data = subset(datos.abu, sp == "cal" & isla == "Roca Partida"))
kruskal.test(abu ~ mes, data = subset(datos.abu, sp == "cfa" & isla == "Roca Partida"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
kruskal.test(abu ~ mes, data = subset(datos.abu, sp == "tob" & isla == "San Benedicto")) #p<0.05
kruskal.test(abu ~ mes, data = subset(datos.abu, sp == "slew" & isla == "San Benedicto"))
kruskal.test(abu ~ mes, data = subset(datos.abu, sp == "cga" & isla == "San Benedicto"))
kruskal.test(abu ~ mes, data = subset(datos.abu, sp == "cal" & isla == "San Benedicto"))
kruskal.test(abu ~ mes, data = subset(datos.abu, sp == "cfa" & isla == "San Benedicto"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
kruskal.test(abu ~ mes, data = subset(datos.abu, sp == "tob" & isla == "Socorro"))
kruskal.test(abu ~ mes, data = subset(datos.abu, sp == "slew" & isla == "Socorro"))
kruskal.test(abu ~ mes, data = subset(datos.abu, sp == "cga" & isla == "Socorro"))
kruskal.test(abu ~ mes, data = subset(datos.abu, sp == "cal" & isla == "Socorro"))
kruskal.test(abu ~ mes, data = subset(datos.abu, sp == "cfa" & isla == "Socorro"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ggplot(datos.abu, aes(mes, abu, fill = spp)) + geom_boxplot() + facet_wrap( ~ isla, nrow = 2) + xlab("Mes") + ylab("Abundancia relativa\n(ind./buceo)") + labs(fill = "Especie")  + theme(legend.text = element_text(face = "italic")) +
  scale_fill_manual(values = palette.spp) + 
  scale_y_continuous(expand = c(0, 1)) + ylim(0,100) +
  theme(legend.position=c(0.9, 0.1), legend.justification=c(1,0))

# Sin RP
ggplot(datos.abu, aes(mes, abu, fill = spp)) + geom_boxplot() + facet_wrap( ~ isla, nrow = 1) + xlab("Mes") + ylab("Abundancia relativa\n(ind./buceo)") + labs(fill = "Especie")  + theme(legend.position="none") +
  scale_fill_manual(values = palette.spp) + 
  scale_y_continuous(expand = c(0, 1)) + ylim(0,55)

#ggsave("C:/Users/corte/Dropbox/maestría/RESULTADOS/figuras/2d_spp_islas_mes2.png", width = 7, height = 4, dpi = 600)


#### correlacion #####
library(corrplot)
datos.cor <- datos[, 6:14]
names(datos.cor) 
mcor <- cor(datos.cor)
# Print mcor and round to 2 digits
round(mcor, digits=2)
corrplot(mcor)

ggplot(datos.abu, aes(fecha, sp, fill=abu)) + geom_bar()

head(datos.abu)

ggplot(datos, aes(sitio, tob)) + geom_boxplot() +ylim(0,35)
ggplot(datos, aes(sitio, slew)) + geom_boxplot()

#### gráficos de líneas promedios ####
abu.mean <- aggregate(abu ~ sitio + mes + sp, data = datos.abu, FUN = mean)
head(abu.mean)
ggplot(abu.mean, aes(x=mes, y=abu, colour=sp, group=sp)) + geom_line() + facet_wrap(~sitio)

abu.median <- aggregate(abu ~ sitio + mes + sp, data = datos.abu, FUN = median)
head(abu.median)
ggplot(abu.median, aes(x=mes, y=abu, colour=sp, group=sp)) + geom_line() + facet_wrap(~sitio)


#### graficas ####
ggplot(datos.abu, aes(mes, abu, fill = spp)) + geom_boxplot() + facet_wrap(~isla) + xlab("Mes") + ylab("Abundancia relativa\n(ind./buceo)") + labs(fill = "Especies")  + theme(legend.text=element_text(face="italic")) 

# la misma pero apiladas
ggplot(datos.abu, aes(mes, abu, fill = spp)) + geom_boxplot() + facet_wrap(~isla) + xlab("Mes") + ylab("Abundancia relativa\n(ind./buceo)") + labs(fill = "Especies")  + theme(legend.text=element_text(face="italic")) + facet_grid(isla ~ .)

palette.spp <- c("#f46038", "#2f294f", "#1b998b", "#e71d35", "#eb952f")
ggplot(datos.abu, aes(mes, abu, fill = spp)) + geom_boxplot() + facet_wrap(~isla) + xlab("Mes") + ylab("Abundancia relativa\n(ind./buceo)") + labs(fill = "Especies")  + theme(legend.text=element_text(face="italic")) + scale_fill_manual(values = palette.spp)

ggplot(datos.abu, aes(mes, abu, fill = spp)) + geom_boxplot() + facet_wrap(~isla) + xlab("Mes") + ylab("Abundancia relativa\n(ind./buceo)") + labs(fill = "Especies")  + theme(legend.text=element_text(face="italic")) + facet_grid(isla ~ .) + scale_fill_manual(values = palette.spp)


ggplot(datos.abu, aes(mes, abu, fill = sp)) + geom_boxplot() + xlab("Mes") + ylab("Abundancia relativa")


ggplot(datos.abu, aes(sp, abu, fill = spp)) + geom_boxplot() + facet_wrap(isla~.) + xlab("Mes") + ylab("Abundancia relativa\n(ind./buceo)") + labs(fill = "Especies")  + theme(legend.text=element_text(face="italic")) 



datos.gam$pos <- datos.gam$sstA >= 0
ggplot(datos.gam, aes(mes, sstA, fill = pos)) + geom_bar(stat="identity", position="identity") + facet_wrap(sitio~temporada, ncol=6) + scale_fill_manual(values=c("#1b998b", "#e71d35"))


datos.gam




ggplot(datos.abu, aes(temporada, abu, fill = sp)) + geom_boxplot() + facet_wrap(~isla) + xlab("Mes") + ylab("Abundancia relativa")

ggplot(datos.abu, aes(mes, abu, shape = sp)) + stat_summary(fun.data = "mean_cl_normal") + facet_wrap(~isla) + xlab("Mes") + ylab("Abundancia relativa")

ggplot(datos.abu, aes(mes, abu, shape = sp)) + stat_summary(fun.data = "mean_se") + facet_wrap(~isla) + xlab("Mes") + ylab("Abundancia relativa")

ggplot(datos.abu, aes(mes, abu, shape = sp)) + stat_summary(fun.data = "median_hilow") + facet_wrap(~isla) + xlab("Mes") + ylab("Abundancia relativa")







#abu  sp isla sitio mes   temporada spp
tob <- aggregate(tob ~ Isla + Sitio + Mes, data = datos, FUN = mean)

ggplot(datos, aes(Mes, tob)) + geom_boxplot() + facet_wrap( ~ Isla)
ggplot(datos, aes(Mes, slew)) + geom_boxplot() + facet_wrap( ~ Isla)
ggplot(datos, aes(Mes, cga)) + geom_boxplot() + facet_wrap( ~ Isla)
ggplot(datos, aes(Mes, cal)) + geom_boxplot() + facet_wrap( ~ Isla)
ggplot(datos, aes(Mes, cfa)) + geom_boxplot() + facet_wrap( ~ Isla)

head(abu)

abu$Mes <- as.numeric(substr(abu$Fecha, 4, 5))

head(abu)

tob <- as.factor(data = subset(abu, Especie == "Triaenodon obesus"))

palette.spp <- c("#f46038", "#2f294f", "#1b998b", "#e71d35", "#eb952f")

ggplot(abu, aes(Mes, Abundancia, fill = Especie)) + geom_boxplot() + facet_wrap( Especie~ Isla, nrow = 2) + xlab("Mes") + ylab("Abundancia relativa\n(ind./buceo)") + labs(fill = "Especie")  + theme(legend.text = element_text(face = "italic")) +
  scale_fill_manual(values = palette.spp) + 
  scale_y_continuous(expand = c(0, 1)) +
  theme(legend.position=c(0.9, 0.1), legend.justification=c(1,0))




