install.packages("tidyverse") # rodar 1 vez
library(tidyverse) # a cada vez que reiniciar

dados_biologia <- c(1,3,7,9)
dados_biologia

especies <- c("Cachorro", "Gato", "Coelho", "Rato")
especies




install.packages("deSolve")

#carregar o pacote
library(deSolve)

x_values <- seq(from = 0, to = 5, by = 0.1)
x_values

out <- ode(y = 2, 
           times = x_values,
           func = funcao2, 
           parms = 1)
plot(out)

# Função: nome()




# sample(): amostragem aleatória

dados_biologia <- c(1,3,7,9)

sample(x = dados_biologia,
       size = 10,
       replace = TRUE,
       prob = c(0.3, 0.1, 0.5, 0.1))


cbind() # Colunm bind = Juntar colunas

dados_biologia <- c(1,3,7,9)
especies <- c("Cachorro", "Gato", "Coelho", "Rato")

dados_juntos <- cbind(dados_biologia, especies)
dados_juntos


dados_data_frame <- data.frame(dados_biologia, especies)

install.packages("writexl")
library(writexl)

write_xlsx(dados_data_frame, "dados.xlsx")

# sum() - função de somar
dados_biologia <- c(1,3,7,9)
sum(dados_biologia)

sum(c(1,2,3,4))


# points
gompertz <- function(inicial, r, t, K) {
  (K)*exp(log(inicial/K)*exp(-r*t))
}
curve(gompertz(inicial=10^9, r=0.006, t=x, K=10^(13)), 
      0, 
      1000, 
      lwd=2, 
      col="red",
      xlab="Tempo (t)", 
      ylab="Tamanho da população (N)",
      main="Curva de crescimento Gompertz")

-log((log(1)-1)/(log(10^(-4))))/0.006

points(370.0545, (10^13)/exp(1))




