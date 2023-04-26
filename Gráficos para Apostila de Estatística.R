##%######################################################%##
#                                                          #
####              Apostila de Estatística               ####
#                                                          #
##%######################################################%##


#Cores HEX
#Laranja 1 
#f7b89d
#Laranja 2
#f2895c
#Laranja 3
#e75212
#Roxo 1
#d26b98
#Roxo 2
#912c58
#Roxo 3
#6c2042
#Verde 1
#afd9d0
#Verde 2
#7bc0b1
#Verde 3
#4c9f8d
#Rosa 1
#e5acb7
#Rosa 2
#d57688
#Rosa 3
#be3a53

##%######################################################%##
#                                                          #
####   Estatística Descritiva Univariada Qualitativa    ####
#                                                          #
##%######################################################%##

###_______________ Dados _______________###

ABO <- data.frame( Tipo = c("A+", "A-", "B+", "B-",
                            "AB+", "AB-", "O+", "O-"),
                   Quantidade = c(15, 2, 6, 1,
                                  1, 1, 32, 2))
ABO <- data.frame(Tipo = c("A+", "A+", "A+", "A+", "A+",
                                "A+", "A+", "A+", "A+", "A+",
                                "A+", "A+", "A+", "A+", "A+",
                                "A-", "A-",
                                "B+", "B+", "B+", "B+", "B+",
                                "B+", "B-","AB+", "AB-",
                                "O+", "O+", "O+", "O+", "O+",
                                "O+", "O+", "O+", "O+", "O+",
                                "O+", "O+", "O+", "O+", "O+",
                                "O+", "O+", "O+", "O+", "O+",
                                "O+", "O+", "O+", "O+", "O+",
                                "O+", "O+", "O+", "O+", "O+",
                                "O+", "O+", "O-", "O-"))
ABO_pie <- c(1.67, 1.67, 1.67, 3.33,
             3.33, 10, 25, 53.33)

#Explorando rapidamente os dados
str(ABO)

#Corrigindo como o R lê a variável Tipo
ABO$Tipo <- as.factor(ABO$Tipo)
class(ABO$Tipo)

###_______________ Tabela de distribuição de frequências _______________###

#Carregando o pacote Summarytools
library(summarytools)
freq(ABO$Tipo)

###_______________ Gráficos _______________###

#Ordenando os dados
library(tidyverse)
ABO = ABO %>% arrange(Tipo, Quantidade)

ABO$Tipo <- factor(ABO$Tipo, levels = c("AB-", "AB+", "B-", "A-",
                                        "O-", "B+", "A+", "O+"))

table(ABO$Tipo)

#Gráfico de Barras Verticais

library(ggplot2)

#Gráfico com uma cor só 
ggplot(ABO) +
  aes(x = Tipo) +
  geom_bar(fill = "#7bc0b1") +
  theme_classic()

#Gráfico com cada barra de uma cor
ggplot(ABO) +
  aes(x = Tipo, fill = Tipo) +
  geom_bar() +
  scale_fill_manual(values = c(`AB-` = "#f2895c", `AB+` = "#e75212", `B-` = "#d57688", `A-` = "#be3a53",
                               `O-` = "#912c58", `B+` = "#6c2042", `A+` = "#7bc0b1", `O+` = "#4c9f8d"
    
  )) +
  theme_classic()


#Gráfico de Barras Horizontais

#Gráfico com uma cor só 
ggplot(ABO) +
  aes(x = Tipo) +
  geom_bar(fill = "#7bc0b1") +
  theme_classic() +
  coord_flip()

#Gráfico com cada barra de uma cor
ggplot(ABO) +
  aes(x = Tipo, fill = Tipo) +
  geom_bar() +
  scale_fill_manual(values = c(`AB-` = "#f2895c", `AB+` = "#e75212", `B-` = "#d57688", `A-` = "#be3a53",
                               `O-` = "#912c58", `B+` = "#6c2042", `A+` = "#7bc0b1", `O+` = "#4c9f8d"
                               
  )) +
  theme_classic() +
  coord_flip()


#Gráficos de setores

#Gráfico com uma cor só
pie(ABO_pie, labels = c("AB-", "AB+", "B-", "A-",
                        "O-", "B+", "A+", "O+"), 
    border = "white", col = "#7bc0b1")  



#Gráfico com cores diferentes para cada setor


pie(ABO_pie, labels = c("AB-", "AB+", "B-", "A-",
                        "O-", "B+", "A+", "O+"), 
    border = "white", col = c( "#f2895c", "#e75212", "#d57688","#be3a53",
                               "#912c58", "#6c2042", "#7bc0b1", "#4c9f8d"))                                  





##%######################################################%##
#                                                          #
####  Estatística Descritiva Univariada Quantitativa    ####
#                                                          #
##%######################################################%##

###_______________ Variáveis Discretas _______________###

###_______________ Dados _______________###
nfilhos <- data.frame(`N de filhos` = c(0, 0, 0, 0, 0, 
                                        1, 1, 1, 1, 1, 1, 1, 
                                        2, 2, 2, 2, 2, 2,
                                        3, 3))

nfilhos_pie <- c(10, 25, 30, 35)
###_______________ Tabela de distribuição de frequências _______________###

#Carregando o pacote Summarytools
library(summarytools)
#Tabela de frequência
freq(nfilhos$N.de.filhos)


###_______________ Gráficos _______________###


#Gráfico de Barras Verticais

library(ggplot2)

#Gráfico com uma cor só 
ggplot(nfilhos) +
  aes(x = N.de.filhos) +
  geom_bar(fill = "#7bc0b1") +
  theme_classic()


table(nfilhos$N.de.filhos)

#Gráficos de setores

#Gráfico com uma cor só
pie(nfilhos_pie, labels = c("3", "0", "2", "1"), 
    border = "white", col = "#7bc0b1")  



###_______________ Dados _______________###
library(palmerpenguins)
dados_penguins <- penguins
#Explorando rapidamente os dados
str(dados_penguins)
library(visdat)
vis_dat(dados_penguins)
#Excluindo as linhas com NA
library(tidyr)
penguins_data <- dados_penguins %>% drop_na()
vis_dat(penguins_data)

#Grafico Histograma
library(ggplot2)
ggplot(penguins_data) +
  aes(x = body_mass_g) +
  geom_histogram(bins = 30L, fill = "#7bc0b1", color="#4c9f8d") +
  labs(x="Body mass in g") +
  theme_classic()

#Grafico de Densidade
penguins_data$bill_length_mm
ggplot(penguins_data) +
  aes(x = body_mass_g) +
  geom_density(adjust = 1L, fill = "#7bc0b1", color="#4c9f8d") +
  theme_classic()+
  labs(x = "Body mass in g") 

#Grafico de linhas

# create data
xValue <- 1:10
yValue <- cumsum(rnorm(10))
data <- data.frame(xValue,yValue)

# Plot
ggplot(data, aes(x=xValue, y=yValue)) +
  geom_line( color="#7bc0b1", size=2, alpha=0.9, linetype=2) +
  theme_classic() +
  ggtitle("Evolution of something")


#Gráfico Boxplot

ggplot(penguins_data) +
 aes(x = species, y = body_mass_g) +
 geom_boxplot(fill = "#7bc0b1") +
 theme_classic()+
  labs(y = "Body mass in g", x = "Species")


ggsave("boxplot1.tiff", width = 10, height = 10, units = c("cm"), dpi = 200)























































































