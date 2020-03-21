## Script para análise de dados parciais.Projeto de Heterogeneidade do ambiente luminoso sob a copa de espécies arbóreas nativas da Mata Atlântica
## parte do curso de Análise exploratório de dados e boa prátias em R
## dados coletados em campo e pertencentas a monografia de Fernanda Cunha
##-------------------------------------------------------------------------------------#

###Vamos começar reconhecendo a tabela utilizada para as análises. Ela está dentro da pasta data###
library("tidyr")
files.path <- list.files(path = "data",
                         pattern = ".csv",
                         full.names = TRUE)
files.path

dados <- read.csv(file = "data/variaveis_luz.csv")
dados
names(dados)

attach(dados) ###chama os fatores dentro das sua tabela
primeirofator <- factor(dados$especies) ###a coluna com a abundancia das espécies
segundofator <- factor(dados$luz)####a coluna com a abundancia da variável de luz
is.factor(primeirofator)
is.factor(segundofator)

install.packages("car")
library("car")###pacote para analises

leveneTest(valores~primeirofator, center = median) #homogeinidade
leveneTest(valores~segundofator, center = median)
shapiro.test(valores)#normalidade

logvalores <- log(valores) ###normalizando e homogeinizando os dados

leveneTest(logvalores~primeirofator, center = median) #homogeinidade
leveneTest(logvalores~segundofator, center = median)
shapiro.test(logvalores)#normalidade

resultadoaov <- aov(logvalores~primeirofator+segundofator+primeirofator:segundofator)
resultadoaovfim <- summary(resultadoaov) #anova bifatorial
resultadoaovfim

tukey <- TukeyHSD(x = resultadoaov, conf.level = 0.95)##teste Tukey
tukey



##Pacotes de cores WesAnderson para gráficos ##
install.packages("wesanderson")
library(wesanderson)

# copia os dados das tabelas de FATOR DE SÍTIO DIRETO para o R
setwd("~/Disciplina_R/Ambiente_Luminoso/data")
means <- as.matrix(read.table("media_especies_luz_FSD.txt", header = T, row.names = 1))####médias
se <- as.matrix(read.table("erro_padrao_especies_luz_FSD.txt", header = T, row.names = 1))####erro padrão
n <- as.matrix(read.table("total_especies_luz_FSD.txt", header = T, row.names = 1))#### n total


# calcula a amplitude da variação do erro
se.sup <- means + se ##limite superior do erro
se.inf <- means - se ##limite inferior do erro

# desenha o gráfico de FSD

bp <- barplot(means,beside = T, ylim = c(0,max(se.sup*1.15)),
              xlab = "Época",
              ylab = "Fator de sítio direto (MJ.m-2.y-1)",legend.text = c("G. guidonia","I. edulis","N. membranacea","P. gonoacantha"),
              col = wes_palette("Darjeeling2", 4, type = c("discrete", "continuous")), ## adiciona cores ao gráfico
              args.legend = list(xjust = 0.8 , yjust = 0.1))
points(bp,means,pch = 19)
arrows(bp,se.sup,bp,se.inf, code = 3,angle = 90,length = 0.05)


teste <- barplot(mean)


# copia os dados das tabelas de ÍNDICE DE ÁREA FOLIAR para o R
setwd("~/Disciplina_R/Ambiente_Luminoso/data")
means <- as.matrix(read.table("media_especies_luz_IAF.txt", header = T, row.names = 1))####médias
se <- as.matrix(read.table("erro_padrao_especies_luz_IAF.txt", header = T, row.names = 1))####erro padrão
n <- as.matrix(read.table("total_especies_luz_IAF.txt", header = T, row.names = 1))#### n total

# calcula a amplitude da variação do erro
se.sup <- means + se ##limite superior do erro
se.inf <- means - se ##limite inferior do erro

# desenha o gráfico de IAF

bp <- barplot(means,beside = T, ylim = c(0,max(se.sup*1.15)),
              xlab = "Época",
              ylab = "Índice de área foliar(m2/m2)",legend.text = c("G. guidonia","I. edulis","N. membranacea","P. gonoacantha"),
              col = wes_palette("Darjeeling2", 4, type = c("discrete", "continuous")), ## adiciona cores ao gráfico
              args.legend = list(xjust = 0.8, yjust = 0.1))
points(bp,means,pch = 19)
arrows(bp,se.sup,bp,se.inf, code = 3,angle = 90,length = 0.05)


teste <- barplot(mean)
