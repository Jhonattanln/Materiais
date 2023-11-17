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


##### Regressão Linear Múltipla com Seleção de Variáveis #####
# Seleção de Variáveis por AIC
# Backward
help(step)
ajuste_back_AIC <- step(multiple_linear_regression, direction = "backward", k=2) # Seleção de variáveis por AIC
summary(ajuste_back_AIC)
resid_panel(ajuste_back_AIC)
resid_xpanel(ajuste_back_AIC)

# Forward
ajuste_null <- lm(Grad.Rate ~ 1, data = College)
ajuste_forw_AIC <- step(ajuste_null, direction = "forward", k=2, scope = formula(multiple_linear_regression)) # Seleção de variáveis por AIC
summary(ajuste_forw_AIC)
resid_panel(ajuste_forw_AIC)
resid_xpanel(ajuste_forw_AIC)

# Both
ajuste_both_AIC <- step(ajuste_null, direction = "both", k=2) # Seleção de variáveis por AIC
summary(ajuste_both_AIC)
resid_panel(ajuste_both_AIC)
resid_xpanel(ajuste_both_AIC)
                        
                        
# Selecionando variáveis por BIC
# Backward
k_BIC <- log(nrow(College))
ajuste_back_BIC <- step(multiple_linear_regression, direction = "backward", k=k_BIC) # Seleção de variáveis por BIC
summary(ajuste_back_BIC)
resid_panel(ajuste_back_BIC)
resid_xpanel(ajuste_back_BIC)

# Forward
ajuste_fowa_BIC <- step(ajuste_null, direction = "forward", k=k_BIC, scope = formula(multiple_linear_regression)) # Seleção de variáveis por BIC
summary(ajuste_fowa_BIC)
resid_panel(ajuste_fowa_BIC)
resid_xpanel(ajuste_fowa_BIC)

