##### Instalando Bibliotecas #####
install.packages("MASS") # Para datasets
install.packages("ggplot2") # Para gráficos
install.packages("dplyr") # Para manipulação de dados
install.packages("ggResidpanel") # Para gráficos de resíduos
install.packages("ISLR") # Para datasets
install.packages("ggcorrplot") # Para gráficos de correlação

##### Carregando Bibliotecas #####
library(MASS)
library(ggplot2)
library(dplyr)
library(ggResidpanel)
library(ISLR)
library(ggcorrplot)

##### Carregando Dados #####
data("College")
help(College)
View(College)

dim(College)
str(College)
summary(College)

# Assumindo a variável Grad.Rate como variável resposta

##### Análise Exploratória #####

# Histograma
ggplot(College, aes(x = Grad.Rate)) + geom_histogram(alpha=0.8, bins = 30, colour= "black", fill='grey') + theme_bw() +
    labs(x = "Grad.Rate", y = "Frequência") + ggtitle("Histograma da variável Grad.Rate", subtitle = "Dataset College")
    
# Gráfico de dispersão
help(geom_smooth)
ggplot(College, aes(x = Room.Board, y = Grad.Rate)) + geom_point() + geom_smooth(method = "lm", se = TRUE) + theme_bw() +
    labs(x = "Grad.Rate", y = "Room.Board") + ggtitle("Gráfico de dispersão da variável Grad.Rate", subtitle = "Dataset College")

ggplot(College, aes(x = Room.Board, y = Grad.Rate)) + geom_point() + geom_smooth(method = "loess", se = TRUE) + theme_bw() +
    labs(x = "Grad.Rate", y = "Room.Board") + ggtitle("Gráfico de dispersão da variável Grad.Rate", subtitle = "Dataset College")

help("ggcorrplot")
ggcorrplot::ggcorrplot(cor(College[, 2:18]), hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, method = "square",
                       colors = c("red", "blue", "springgreen3"), title = "Correlação entre as variáveis do dataset College")


##### Regressão Linear Simples #####
simple_linear_regression <- lm(Grad.Rate ~ Room.Board, data = College)
summary(simple_linear_regression)
resid_panel(simple_linear_regression)

ggplot(College, aes(x = log(Room.Board), y = Grad.Rate)) + 
    geom_point() + geom_smooth(method = "lm", se = TRUE) + theme_bw() +
    labs(x = "Grad.Rate", y = "Room.Board") + 
    ggtitle("Gráfico de dispersão da variável de log Grad.Rate", subtitle = "Dataset College")

simple_linear_regression_log <- lm(Grad.Rate ~ log(Room.Board), data = College)    
summary(simple_linear_regression_log)
resid_panel(simple_linear_regression_log)

confint(simple_linear_regression_log, level = 0.95) # Intervalos de confiança para os coeficientes
##### Regressão Linear Múltipla #####
multiple_linear_regression <- lm(Grad.Rate ~ ., data = College)
summary(multiple_linear_regression)
resid_panel(multiple_linear_regression)
resid_xpanel(multiple_linear_regression) # Gráfico de resíduos por variável independente

confint(multiple_linear_regression, level = 0.95) # Intervalos de confiança para os coeficientes
