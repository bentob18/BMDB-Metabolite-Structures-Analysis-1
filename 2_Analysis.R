install.packages('readr')
install.packages('cowplot')
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


plot(AN, DN, type="p",pch = 8,col = "blue")
plot(RC, AN, pch = 8, col = "red")
plot(RC, DN, pch = 8, col = "purple")
plot(Rings_num, RC, pch = 8, col = "orange")
plot(Rings_num, AN, pch = 8, col = "green")
 
sd(TPSA)
options(max.print=1000)t8
TPSA[order(TPSA, decreasing = TRUE)]
boxplot(TPSA ~ Rings_num, range =100, horizontal = FALSE)



