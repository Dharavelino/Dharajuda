###Script Base###

#0. Ver o diretório
getwd()

#0.1. Mudar o diretório
setwd("C:/Users/dhara/Documents")

#1.Pacotes necessários
library(readxl)
library(DescTools)
library(summarytools)
library(dplyr)
library(ggplot2)
library(readr)
library(RVAideMemoire)
library(FSA)
library(userfriendlyscience)
library(RcmdrMisc)
library(stringr)
library(export)
library(classInt)
library(FactoMineR)
library(dendextend)
library(corrplot)
library(visdat)
library(factoextra)


#2.Para ler um documento em xlsx

dados <- as.data.frame(read_excel("C:/Users/dhara/pasta.onde.esta.o.arquivo/nome.do.arquivo.xlsx" ))

#3.Para ler um documento em csv

read.csv("C:/Users/dhara/Downloads/nome.do.arquivo")

#4. Dimensão de dados
dim(dados)

#5. Estrutura de dados
str(dados)

#6. Usando visdat para estrutura do dados e dados ausentes
vis_dat(dados)
vis_miss(dados)

#7. Resumo dos dados
summary(dados)

#8. Verificando NA
table(is.na(dados))
summary(dados)
sum(is.na(dados$variavel))
sum(is.na(dados))

#9.Explorando os dados
#Média aritmética 
mean(dados$comprimento)

#Mediana 
median(dados$comprimento)

#Moda 
Mode(dados$comprimento)

#Primeiro quartil 
summary(dados$comprimento) [2]

#Terceiro quartil 
summary(dados$comprimento) [5]

#IQR 
IQR(dados$comprimento)

#Amplitude 
max(dados$comprimento)-min(dados$comprimento)

#Variância
var(dados$comprimento)

#Desvio-padrão 
sd(dados$comprimento)

#Coeficiente de variação 
sd(dados$comprimento)/mean(dados$comprimento)

#Frequencia
freq(dados)

#10. Gráficos
#Usar Addins-GGPLOT

#11. Mudar a forma que o R está lendo uma variável
dados$Escolaridade <- as.factor(dados$Escolaridade)
dados$Idade <- as.numeric(dados$Idade)
dados$variavel <- as.character(dados$variavel)

#12. Mudar o nome da variável
colnames(dados)[1:6] <-
  c("Variavel1",
    "Variavel2",
    "Variavel3",
    "Variavel4",
    "Variavel5",
    "Variavel6")

#13. Uniformizando as categorias de um banco
levels(dados$Escolaridade) <- c(
  "Ensino fundamental",
  "Ensino fundamental",
  "Ensino médio",
  "Ensino médio",
  "Ensino superior",
  "Ensino superior",
  "Pós-graduação"
)
#dar freq para ver se realmente mudou
freq(dados$Escolaridade)

#14. Avaliando a normalidade dos dados através do teste Shapiro-Wilk
#normalidade em relação à uma variável 
byf.shapiro(Acertos ~ `Área de Formação` , data = dados3)
#normalidade geral
shapiro.test(shapiro.test(dadosBRA1[dadosBRA1$Espécime=="1A", "ComprimentoSub"]))

#15. Avaliando a homocedasticidade da variância através do teste de Levene
LeveneTest(ComprimentoSub~Espécime, data=dadosBRA1, center="median")

#16. ANOVA para dados normais e homogêneos (comparação entre 3 ou + grupos)
aov.nome.que.eu.quiser <- aov(comprimento ~ especime, data = dados)
#summary pra ver o resultado da anova
summary(aov.nome.que.eu.quiser)

#17. Post hoc Tukey
TukeyHSD(aov.nome.que.eu.quiser)

#18. ANOVA com Welch para dados normais mas sem homcedasticidade, post hoc games-howell (comparação entre 3 ou + grupos)
one.way <- oneway(dados$ComprimentoSub, dados$Localidade, posthoc = 'games-howell')
one.way

#19.Kruskal-Wallis para dados que não seguem a normalidade  (comparação entre 3 ou + grupos)
kruskal.test(LarguraSubt ~ Localidade, data = dados)

#20.Post Hoc Kruskal-Wallis: Teste Dunn
dunnTest(LarguraSubt ~ Localidade, data = dados, method = "bh") 

#21.Teste T para dados normais e homogêneos (comparação entre 2 grupos)
t.test(dados2A$COMPRIMENTO~dados2A$ESPECIME, var.equal=T)

#22. Teste T para dados normais sem homocedasticidade, CORREÇÃO DE Welch (comparação entre 2 grupos)
t.test(COMPRIMENTO ~ ESPECIME, data = dados2A)
t.test(dados2A$COMPRIMENTO~dados2A$ESPECIME, var.equal=F)

#22.Test de Mann-Whitney para dados que fogem da normalidade (comparação entre 2 grupos)
wilcox.test(LarguraSubt ~ Pais, data = dados)

#23.Avaliando a correlacao entre as variaveis
corrplot::corrplot(cor(dados_cluster), method = "pie")

#24.PCA
res.pca <- prcomp(dados_cluster, scale = T)
fviz_eig(res.pca)

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca(res.pca, repel = TRUE,
         col.var = "#2E9FDF", # Variables color
         col.ind = "#696969"  # Individuals color
)

P2 <- fviz_pca(res.pca, repel = TRUE,
               col.var = "#2E9FDF", # Variables color
               col.ind = "#696969"  # Individuals color
)

#Para exportar o gráfico 
export::graph2ppt(P2)

#25.Cluster Hierarquico com PCA
# Compute PCA with ncp = 3
res.pca <- PCA(dados_cluster, ncp = 3, graph = FALSE)

# Compute hierarchical clustering on principal components
res.hcpc <- HCPC(res.pca, graph = FALSE)
leafcolor <- c("black", "black", "black")
fviz_dend(res.hcpc, show_labels = T, horiz = T, palette = leafcolor)


fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)

fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)

plot(res.hcpc, choice = "3D.map")


#26 selecionando apenas o que tem "xxx" nas colunas
data1<- data [data$CATEGORIA == "Ectosomal Subtylostyle", ]

#27 Mudando a ordem
dados$ID <- factor(dados$ID, levels = c("A1", "A2", "A6", "A4", "A7",
                                        "A9", "A5", "A8", "A3", "A10"))

#28 ADD linhas com NA
library ("berryFunctions")
data <- insertRows(data, 73:76, new = NA)

#29 exportar pro excel
library("writexl")
write_xlsx(dados_graph2,"C:\\Users\\dhara\\OneDrive\\Documentos\\sponge.xlsx")
























