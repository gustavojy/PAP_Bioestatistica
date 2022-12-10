## ----setup, include=FALSE---------------------------
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE)


## ---------------------------------------------------
#1. Importação de dados
library(readxl)
(dados_alunos <- read_excel("dados_alunos.xlsx"))

# 2. Gráfico de dispersão + regressão
library(ggplot2)
ggplot(data = dados_alunos,
       aes(x = Peso, y = Altura))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)


#3. Calcular regressão na unha
##Calcular média de x=Peso e y=Altura
(media_x <- mean(dados_alunos$Peso))
(media_y <- mean(dados_alunos$Altura))

## Calcular média de Peso ao quadrado
mean(dados_alunos$Peso)^2

## Calcular média de Altura ao quadrado
mean(dados_alunos$Altura)^2

# Banco de dados com x, y e x*y
(dados <- data.frame(
  x = dados_alunos$Peso,
  y = dados_alunos$Altura,
  x.y = dados_alunos$Peso*dados_alunos$Altura,
  x_quadrado = dados_alunos$Peso^2,
  y_quadrado = dados_alunos$Altura^2
))


# Feito pelo Cristian após a aula
apply(dados, 2 , sum)

# Soma de x*y
(soma_xy <- sum(dados$x.y))

# Soma x_quadrado
(soma_xquad <- sum(dados$x_quadrado))

# Calculo b
(b <- (soma_xy-nrow(dados_alunos)*media_x*media_y)/(soma_xquad-nrow(dados_alunos)*media_x^2))


# Calculo a
(a <- media_y - b*media_x)


# Equação: y = a+b*x
(peso80 <- a + b*80)


(peso100 <- a + b*100)


# Função  Altura = a+b*Peso
lm(dados_alunos$Altura~dados_alunos$Peso)


# Calculando correlação

(soma_yquad <- sum(dados$y_quadrado))

(syy <- soma_yquad-nrow(dados_alunos)*media_y^2)
(sxx <- soma_xquad-nrow(dados_alunos)*media_x^2)
(sxy <- soma_xy-nrow(dados_alunos)*media_x*media_y)


## ---------------------------------------------------
(correlacao <- sxy/sqrt(sxx*syy))

cor(dados_alunos$Altura, dados_alunos$Peso)


