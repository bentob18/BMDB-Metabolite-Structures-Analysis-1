install.packages('tidyverse')
install.packages('readr')
install.packages('cowplot')
install.packages('dplyr')
library(dplyr)
library(tidyverse)
library(readr)
library(cowplot)

table <- read.csv2("BMDB_items.csv")

molWheight <- as.numeric(na.omit(table$Mol_wheight))
TPSA <- as.numeric(na.omit(table$Total_PSA))
Rings_num <- as.numeric(na.omit(table$Rings_Num))
RC <- as.numeric(na.omit(table$Rotatables_Count))
AN <- as.numeric(na.omit(table$Acceptors_Num))
DN <- as.numeric(na.omit(table$Donors_Num))
CHLOG <- as.numeric(na.omit(table$Chem_LogP))

#Aplicando funções estatísticas
media_peso <- mean(molWheight)
mediana_peso <- median(molWheight)
quantil_peso <- quantile(molWheight)
quantil_peso
#Plotando gráficos em função da coluna Mol wheight
plot(molWheight,TPSA,type="p",pch = 8,col = "blue")
plot(molWheight,Rings_num,type="p",pch = 8,col = "blue")
plot(molWheight,RC,type="p",pch = 8,col = "blue")
plot(molWheight,AN,type="p",pch = 8,col = "blue")
plot(molWheight,DN,type="p",pch = 8,col = "blue")
plot(CHLOG,molWheight,type="p",pch = 8,col = "blue")

#Ordenação de dados da coluna Mol_wheight
rank_peso <- rank(table$Mol_wheight)
sort_peso <- sort(table$Mol_wheight)
ord_peso <- order(table$Mol_wheight)

#Tabela de frequencias
breaks <- seq(from = min(molWheight), to=max(molWheight), length=10)
Wheight_freq <- cut(molWheight, breaks = breaks, right = TRUE, include.lowest = TRUE)
table(Wheight_freq)
hist(molWheight, breaks = breaks)
lines(density(molWheight), lwd=3, col='blue')

#Boxplot
boxplot(molWheight ,ylab = "Mol_wheight", range = 9, horizontal = TRUE)
boxplot(molWheight, range=9, horizontal=TRUE, varwidth=FALSE, notch=FALSE,
        outline=TRUE, boxwex=1, border=c("blue"), xlab="Mol_wheight")

max(molWheight)
i_max <- which.max(molWheight)
min(molWheight)
i_min <- which.max(molWheight)


class(table$Mol_wheight)
desvio_padraoMW <- sd(molWheight)
class(molWheight)
options(max.print=1000)
molWheight[order(molWheight)]


sum(molWheight >0 & molWheight <5000, na.rm = TRUE)
less_molWheight <- which(molWheight < 1200 & CHLOG >= 0)
lMolWheight <- molWheight[less_molWheight]
lCHLOG <- CHLOG[less_molWheight]

plot(lCHLOG,lMolWheight,type="p",pch = 8,col = "blue")
plot(CHLOG,molWheight,type="p",pch = 8,col = "blue")

#Encontrando valores únicos em algums colunas
unique(table$Mol_wheight)
length(unique(table$Mol_wheight))
unique(table$Formula)
length(unique(table$Formula))



ggplot(data = table) + geom_point(mapping = aes( x = Chem_LogP , y = Mol_wheight, color = "class"))





