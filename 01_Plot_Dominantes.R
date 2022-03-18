#setwd("C:/Users/Admin/Dropbox/maestría/RESULTADOS/")
#library(dunn.test)
library(reshape) # cast()
library(plyr)    # revalue()
library(ggplot2)
library(dunn.test)
#install.packages('ggplot2', dependencies = TRUE)
datos <- read.csv("datos/Revilla_pelagicos/Revilla_14_21.csv")

#________________________________________________________________________________________
#### 0.- Explorador ####
names(datos)
sum(datos$Abundance)    # 671,914 tiburones
length(datos$Abundance) #   7,302 observaciones
(sum(datos$Abundance))-(sum(colSums(datos[, 12:46], na.rm = T))) # verificador (debe dar 0)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#### Fecha ###
datos$Date
datos$Date <- as.Date(datos$Date, format = "%d/%m/%Y")
#sort(table(datos$Date))
#unique(datos$Date)
levels(datos$Month)
datos$Month <- factor(datos$Month, levels = levels(datos$Month)[ c(8,2,4,3,6,1,7,5)])

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#table(datos$Temperature)
#table(datos$Current)
#table(datos$Direction)
#levels(datos$Site)
#table(datos$Month)
#table(datos$Year)
#table(datos$Base)
#length(levels(datos$Species)) # 96
#r_spp <- aggregate(Abundance ~ Code + Species, data = datos, FUN = sum)
#write.csv(r_spp, "datos/r_spp.csv")

#________________________________________________________________________________________
# 1.- DATOS ####
#________________________________________________________________________________________
# 0.- Subset ###
head(datos,3)
levels(datos$Island)
datos <- subset(datos, datos$Island != "Clarión")
datos$Island <- droplevels(datos$Island)

datos$Site <- droplevels(datos$Site)
datos <- subset(datos, datos$Site != "Roca O'neal")
datos <- subset(datos, datos$Site != "El Fondeadero")
datos <- subset(datos, datos$Site != "The Old Man of the Rock")
datos <- subset(datos, datos$Site != "Zoológico")
datos <- subset(datos, datos$Site != "Punta SO")
datos$Site <- droplevels(datos$Site)
levels(datos$Site)

datos$Site <- factor(datos$Site, levels = levels(datos$Site)[ c(6,2,3,4,1,5)])
# orden sitios: "Roca Partida", "El Boiler", "El Cañón", "Las Cuevitas", "Cabo Pearce", "Punta Tosca"

#datos$Month <- droplevels(datos$Month)
#levels(datos$Month)

#________________________________________________________________________________________
# 1.- id ###
# Para crear un ID único para cada Transect
datos$id <- paste(datos$Date, datos$Site, datos$Transect, sep = "/")
length(unique(datos$id)) # 812 Transects

revilla <- aggregate(Abundance ~ id + Date + Island + Site + Month + Year, data = datos, FUN = sum)
head(revilla, 4)
revilla$Tr <- 1 #sum(revilla$Tr)

#dives <- cast(revilla, Island + Site ~ Month, value = "Tr", sum)
#dives2 <- cast(revilla, Island + Site ~ Year + Month, value = "Tr", sum)
#write.csv(dives2, "datos/r_dives.csv")

#________________________________________________________________________________________
# 2.- peces/tiburones ###
names(datos)
id.spp <- cast(datos, id ~ Code, value = "Abundance", sum) # genera el df de id x peces
head(id.spp, 3)
sum(datos$Abundance) - (sum(colSums(id.spp[, -1], na.rm = T))) # verificador (debe dar 0)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#id.sharks <- id.spp[,c(1,100,92,21,14,20,45,86,22,25)] 
id.sharks <- data.frame(id = id.spp$id, tob = id.spp$tob, slew = id.spp$slew, cga = id.spp$cga, cal = id.spp$cal, cfa = id.spp$cfa, gcu = id.spp$gcu, rty = id.spp$rty, cli = id.spp$cli, cob = id.spp$cob)
sum(colSums(id.sharks[,-1])) # 37,473 tiburones
names(id.sharks)

sharks <- merge(revilla ,id.sharks)
head(sharks, 4)

#________________________________________________________________________________________
#### Plot Dominantes ####
head(sharks, 3)
abu <- as.data.frame(colSums(sharks[, c(9:17)]))
head(abu)
ocu <- ifelse(sharks[, c(9:17)]>0,1,0)
ocu <- as.data.frame(colSums(ocu))
head(ocu)

#### Data frame final de dominantes: 
df.dom <- data.frame(spp = row.names(abu), abundancia = abu[,1], ocurrencia = ocu[,1])
head(df.dom)
sum(df.dom$abundancia) # 37,473 tiburones
sum(df.dom$ocurrencia) # 812 censos

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Calculo de la Abundancia relativa y % de ocurrencia
df.dom$ar <- (df.dom$abundancia/sum(df.dom$abundancia))*100
df.dom$po <- (df.dom$ocurrencia/nrow(sharks))*100

median(df.dom$ar)
median(df.dom$po)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Las especies dominantes son aquellas cuya "ar" es > la AR_promedio y el "po" es > el PO_promedio
df.dom$dom <- ifelse(df.dom$ar>median(df.dom$ar) & df.dom$po>median(df.dom$po), df.dom$spp, NA)
#### Correccion nombres de especies
df.dom$spp <- revalue(df.dom$spp, c("tob" = "T. obesus",
                                    "slew" = "S. lewini",
                                    "cga" = "C. galapagensis",
                                    "cal" = "C. albimarginatus",
                                    "cfa" = "C. falciformis",
                                    "gcu" = "G. cuvier",
                                    "rty" = "R. typus",
                                    "cli" = "C. limbatus",
                                    "cob" = "C. obscurus"))
#write.csv(df.dom, "plots_paper/df.dom.csv")
names.spp <- df.dom[c(1:5),] # na.omit(df.dom)
nrow(names.spp)

#________________________________________________________________________________________
#### Plot dominantes ###
theme_set(             theme(panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(),
                             panel.background = element_rect(colour = "gray15"),
                             axis.title.x = element_text(size=13, color = "gray15"),
                             axis.title.y = element_text(size=13, color = "gray15", 
                                                         angle = 90, vjust = 2),
                             axis.text.x = element_text(size=12, color = "gray15"),
                             axis.text.y = element_text(size=12, color = "gray15"),
                             legend.title = element_text(size=20),
                             legend.text = element_text(size=15)))

# Plot # pag 79 del cookbook
png("plots_paper/dominantes.png", width = 7, height = 5, units = "in", res = 300)
ggplot(df.dom, aes(x=po, y=ar)) + geom_point(cex = 1) +
  annotate("text", x=names.spp$po, y=names.spp$ar+.1, label=names.spp$spp, 
           size = 4, hjust = -.05, fontface = "italic", color = "gray15") +
  geom_vline(xintercept = median(df.dom$po), linetype="dashed", color = "gray15") + 
  geom_hline(yintercept = median(df.dom$ar), linetype="dashed", color = "gray15") + 
  labs(x = "Ocurrence (%)", y = "Relative abundance (%)") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 80)) + 
  theme(plot.margin = unit(c(.3,.5,.2,.2), "cm"))
dev.off()


#________________________________________________________________________________________
#### (sharks.2) ####
head(sharks, 3)
nrow(sharks) # 812
names(sharks)
sharks.2 <- subset(sharks[,-c(7,8,14:17)]) # mata Abundance y Tr + gcu, rty, cli, cob
sharks.2 <- melt(sharks.2, id.vars = c("id", "Date", "Island", "Site", "Month", "Year"))
head(sharks.2, 3)

colnames(sharks.2)[7] <- "Species"
colnames(sharks.2)[8] <- "Abundance"
head(sharks.2, 3)
nrow(sharks.2) # 4,060 = 812 * 5 
# sharks.2 = ESPECIES DOMINANTES (5) APILADAS EN UNA SOLA COLUMNA llamada "Species"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#### (sharks.2) ABUNDANCIA RELATIVA POR ESPECIES ####
sharks.ar.5 <- aggregate(Abundance ~ Island + Site + Month + Year + Species, data = sharks.2, FUN = mean)
head(sharks.ar.5, 3)
nrow(sharks.ar.5) # 845


#________________________________________________________________________________________
#### 2.- KW ####
#### Revisar script: estadísticos_de_prueba.R
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#### Normalidad y Homocedasticidad
shapiro.test(sharks.ar.5$Abundance) # W = 0.36602, p-value < 2.2e-16
bartlett.test(Abundance ~ Site, data = sharks.ar.5) # K-sq = 1195.1, df = 5, p-value < 2.2e-16

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#### 2.1 spp - Sitios ####
head(sharks.ar.5, 3)
kruskal.test(Abundance ~ Species, data = subset(sharks.ar.5, Site == "Roca Partida")) #
kruskal.test(Abundance ~ Species, data = subset(sharks.ar.5, Site == "Cabo Pearce")) #
kruskal.test(Abundance ~ Species, data = subset(sharks.ar.5, Site == "El Boiler")) #
kruskal.test(Abundance ~ Species, data = subset(sharks.ar.5, Site == "El Cañón")) #
kruskal.test(Abundance ~ Species, data = subset(sharks.ar.5, Site == "Punta Tosca")) #
kruskal.test(Abundance ~ Species, data = subset(sharks.ar.5, Site == "Las Cuevitas")) #

data.RP <- subset(sharks.ar.5, Site == "Roca Partida")
data.CP <- subset(sharks.ar.5, Site == "Cabo Pearce")
data.EB <- subset(sharks.ar.5, Site == "El Boiler")
data.EC <- subset(sharks.ar.5, Site == "El Cañón")
data.PT <- subset(sharks.ar.5, Site == "Punta Tosca")
data.LC <- subset(sharks.ar.5, Site == "Las Cuevitas")

dunn.test(data.RP$Abundance, data.RP$Species)
dunn.test(data.CP$Abundance, data.CP$Species)
dunn.test(data.EB$Abundance, data.EB$Species)
dunn.test(data.EC$Abundance, data.EC$Species)
dunn.test(data.PT$Abundance, data.PT$Species)
dunn.test(data.LC$Abundance, data.LC$Species)

# aggregate(Abundance ~  Species + Site, FUN = median, data = sharks.ar.5)
round(tapply(sharks.ar.5$Abundance, list(sharks.ar.5$Species, sharks.ar.5$Site), mean), 2)
#round(tapply(sharks.ar.5$Abundance, list(sharks.ar.5$Species, sharks.ar.5$Site), max), 1)


palette.spp <- c("#f46038", "#2f294f", "#1b998b", "#e71d35", "#eb952f")
ggplot(sharks.ar.5, aes(Species, Abundance, fill = Species)) + geom_boxplot() + facet_wrap(~Site, scale = "free_y", ncol=3) + xlab("Especie") + ylab("Abundancia relativa\n(ind./buceo)") + 
  scale_fill_manual(values = palette.spp) + 
  theme(legend.text = element_text(face="italic")) + 
  labs(fill = "Especie") + 
  theme(axis.text.x = element_blank()) + 
  scale_y_continuous(expand = c(0, 1))

#### 2.2 spp - Mes ####
kruskal.test(Abundance ~ Month, data = subset(data.RP, Species == "tob")) 
kruskal.test(Abundance ~ Month, data = subset(data.CP, Species == "tob"))
kruskal.test(Abundance ~ Month, data = subset(data.EB, Species == "tob"))
kruskal.test(Abundance ~ Month, data = subset(data.EC, Species == "tob")) 
kruskal.test(Abundance ~ Month, data = subset(data.PT, Species == "tob")) 
kruskal.test(Abundance ~ Month, data = subset(data.LC, Species == "tob"))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

head(sharks.ar.5, 3)
levels(sharks.ar.5$Species)
ggplot(sharks.ar.5, aes(Month, Abundance, fill = Month)) + geom_boxplot() + facet_wrap(Site ~ Species, ncol = 5, scale = "free_y") + xlab("Mes") + ylab("Abundancia relativa\n(ind./buceo)") + labs(fill = "Especie")  + theme(legend.text = element_text(face = "italic")) +
  scale_y_continuous(expand = c(0, 1)) 
+ scale_fill_manual(values = palette.spp) 

ggplot(sharks.ar.5, aes(Month, Abundance, fill = Species)) + geom_boxplot() + facet_wrap( ~ Site, ncol = 3, scale = "free_y") + xlab("Mes") + ylab("Abundancia relativa\n(ind./buceo)") + labs(fill = "Especie")  + theme(legend.text = element_text(face = "italic")) +
  scale_fill_manual(values = palette.spp) + 
  scale_y_continuous(expand = c(0, 1))



tob <-lm(tob ~ Year, data = aaa)
plot(tob ~ Year, data = aaa)
abline(tob)




class(datos$Date)
boxplot(Abundance ~ Date, data = subset(datos, Code == "tob" & Site == "Roca Partida"))
boxplot(Abundance ~ Date, data = subset(datos, Code == "slew" & Site == "Roca Partida"))

rp.tob <- subset(datos, Code == "tob" & Site == "Roca Partida")

head(sharks.ar.5, 3)

ggplot(sharks.ar.5, aes(as.factor(Date), Abundance, fill = Month)) + geom_boxplot() + facet_wrap( ~ Species, ncol = 1, scale = "free_y") + xlab("Mes") + ylab("Abundancia relativa\n(ind./buceo)") + labs(fill = "Especie")  + theme(legend.text = element_text(face = "italic")) +
  scale_fill_manual(values = palette.spp) + 
  scale_y_continuous(expand = c(0, 1))

