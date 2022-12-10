# Importação de dados
## install.packages("readxl")
library(readxl)

## Clicar em "Import Dataset" -> "From Excel"
dados <- read_excel("dados/dados.xlsx")
str(dados)
  ### chr = charactere = Variável qualitativa
  ### num = numeric = Variável quantitativa
    ### Qualitativa: nominal / ordinal?
    ### Quantitativa: discreta / contínua?

# Tabelas de frequência e gráficos associados
## QUALITATIVA NOMINAL
### Tabelas de frequência
# install.packages("summarytools")
library(summarytools)

freq(dados$Genero)
  ### Interpretação: dos 30 entrevistados, 50% são do sexo masculino e 50%, do feminino

# Gráficos
#### de colunas (ou barras): Podemos utilizar tanto a freq. Absoluta, como a relativa
#### gráfico de pizza (de setores): utiliza-se somente a relativa, pois a pizza perfaz 100%



## QUALITATIVA ORDINAL
freq(dados$Grau_de_Instruçao)
  ### Interpretação: dos 30 entrevistados, 20% fizeram até o ensino fundamental, 33,33%, o ensino médio e 46.67%, o ensino superior

# Gráficos: colunas e pizza / gráfico de escada para a freq acumulada


## QUANTITATIVAS DISCRETAS
freq(dados$N_Filhos)

### Gráfico de barras / pizza


## QUANTITATIVAS CONTÍNUAS
freq(dados$Salario) # precisa criar intervalos de números

### Regra de Sturges
#1. Calcular a amplitude (Max - Min)
(amplitude <- diff(range(dados$Salario)))

#2. Determinar o número de classes usando a regra de Sturges:
## k = 1 + 3.222 log(n) em que n é o tamanho da amostra.
(k <- 1 + 3.222*log10(nrow(dados)))
nclass.Sturges(dados$Salario)

#3. Amplitude / k: O resultado da divisão é o intervalo de classe. É sempre melhor arredondar esse número para um valor mais alto
(div <- amplitude/k)
round(div, 0)

#4. Portanto, criaremos nossas classes indo de 1 em 1 salário mínimo
## Desenhar tabela com intervalos
table(cut(dados$Salario, seq(1, 6, l = 6)))
round(prop.table(table(cut(dados$Salario, seq(1, 6, l = 6)))), 2)

#5. Histograma
hist(dados$Salario, breaks=c(seq(1,6,1)),
     ylab="Frequencias absolutas",
     main="",
     col="gray")







# Medidas de tendência central
## Média aritmética: media = (x1 + x2 + ... + xn) / n
mean(dados$Altura)

weighted.mean()


## Mediana
median(dados$Altura)

  ## Ímpar: valor central com valores ordenados
  ## Par: média aritmética dos dois valores centrais com valores ordenados
sort(dados$N_Filhos)
freq(dados$N_Filhos)

median(dados$N_Filhos)


## Moda: nada mais é do que uma tabela de frequência absoluta/relativa
table(dados$N_Filhos)




# Medidas de dispersão
## Amplitude = MAX - MIN: medida que é muito afetada por valores atípicos/não leva em consideração os demais valores
diff(range(dados$Idade))

## Variância (s²)
### A variância, assim como o desvio padrão, é uma das medidas de dispersão. Através da variância, podemos verificar o quão próximo estão os valores de um valor central, que neste caso, é a média desses valores.

#1. soma do quadrado das observações
sum(dados$Idade^2)

#2. média ao quadrado vezes o número de observações
mean(dados$Idade)^2 * nrow(dados)

# 1. - 2.
sum(dados$Idade^2) - mean(dados$Idade)^2 * nrow(dados)

# Dividir o resultado por (n-1)
s_quad <- (sum(dados$Idade^2) - mean(dados$Idade)^2 * nrow(dados))/(nrow(dados)-1)

# Fórmula no R
var(dados$Idade) # A variância da idade dos entrevistados é de 42.62 anos² em relação à média


## Desvio padrão (s) / s = raiz de s²
###O desvio padrão é uma medida que expressa o grau de dispersão de um conjunto de dados. Ou seja, o desvio padrão indica o quanto um conjunto de dados é uniforme. Quanto mais próximo de 0 for o desvio padrão, mais homogêneo são os dados.

sqrt(s_quad)
sd(dados$Idade) # O desvio padrão da idade dos entrevistados é de 6,5 anos em relação à média

## Comparando com outra variável: a idade dos entrevistados tem uma distribuição mais (menos) homogênea que a variável "xxx", pois possui menor desvio padrão



## Coeficiente de variação (CV) CV(%) = s / média *100
(cv_filhos <- sd(dados$N_Filhos)/mean(dados$N_Filhos) * 100)
(cv_idade <- sd(dados$Idade)/mean(dados$Idade) * 100)
# Quanto maior o CV, mais dispersos são os dados
# Os entrevistados são  mais dispersos quanto ao número de filho do que quanto à idade





# Percentil, quartil e boxlot
## Os percentis dividem os dados em 100 conjuntos iguais (P1, ...P99)
## Os quartis dividem os dados em 4 conjuntos iguais (Q1, Q2 e Q3)

### Q2 = P50 = Mediana
quantile(dados$Altura, 0.50)


quantile(dados$Altura, 0.66)
# Interpretação: 66% das alturas dos entrevistados são menores que 170,42 cm

### Cálculo manual
# 1. (n × p = i + f), em que "i" representa a parte inteira e "f" a parte decimal do produto n × p, 0 < p < 1.
  # n = número de observações
  # p = percentil a ser calculado

## Percentil 66 (P66)
nrow(dados)*0.66 # i = 19 e f = 0.8 (f > 0)

### Pj -> P66 = x[i+1] = x[19+1] = x[20]
### O valor que ocupa a posição x[20] corresponde ao P66 (com os dados ordenados)
### Interpretação: 66% das alturas são menores que 170 cm ou 34% das alturas são maiores que 170 cm

quantile(dados$Altura, 0.66, type = 3)



## Boxplot
#1. Ordenar os dados crescente
sort(dados$Altura)

#2. Determinar Q1(P25), Q2(P50) e Q3(P75)
summary(dados$Altura)

## Q1(P25)
nrow(dados)*0.25 # f>0 ; x[7+1] = x[8] = 160 cm

## Q2(P50)
nrow(dados)*0.5 # f=0 ; (x[15]+x[15+1] / 2) = 166.5 cm

##Q3(P75)
nrow(dados)*0.75 # f>0 ; x[22+1] = 173 cm

#3. Calcular limites
  ## IQR: IQR = Q3 − Q1
(IQR <- 173-160)

  ## Limite inferior:  Q1 − 1.5IQR
160-1.5*13 # 140,5

  ## Limite superior: Q3 + 1.5IQR
173+1.5*13

#4. Determinar valores MAX e MIN dentro dos limites

#5. Verificar se há valores atípicos

boxplot(dados$Altura, horizontal = T)
hist(dados$Altura)

hist(dados$Salario)




# Simetria
## Distribuição simétrica: Media = Mediana = Mo.

## Distribuição assimétrica à direita: Média < Mediana e < Mo

## Distribuição assimétrica à esquerda: Média > Mediana > Mo







# Regressão e correlação

## Regressão linear simples: y = a + bx
### a = intercepto (onde a reta toca no eixo y)
### b = coeficiente angular (a cada 1 unidade adicionada em x, cresce b no eixo y)

## y vs x = altura vs peso


#1. Calcular

### b = Sxy / Sxx
### Sxy = [Soma(x*y) - nrow() * media_x * Media_y]

### Sxx = Soma(x²) - nrow() * (media_x)²

### a = media_y - b*media_x




## Correlação
# r = Sxy / raiz(Sxx * Syy)

## Sxy = Soma(x*y) - nrow() * media_x * media_y
## Sxx = Soma(x²) - nrow() * (media_x)²
## Syy = Soma(y²) - nrow() * (media_y)²


#1. Calcular

## Interpretação: Correlação positiva ou negativa?
## Está mais próximo de 0 ou de 1 (-1)?


     