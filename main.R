#----------------------------------------------------------------------#
#           Trabalho Final - Probabilidade e Estat�stica               #
#----------------------------------------------------------------------#

#Limpando o ambiente 
rm(list=ls(all=TRUE))

#Remover nota��o cient�fica 
options(scipen=999)

#Pacotes 

library(readxl)
library(corrplot)
library(MASS)

#Leitura dos dados 
dados <- read_excel("C:/Users/.../data.xlsx")
#View(dados)

dados <- data.frame(dados)
class(dados)
head(dados) #03/06/2019
tail(dados) #01/06/2020
dim(dados) #261 observa��es 
dados <- na.omit(dados)
dim(dados) #261 observa��es (sem NA)

#An�lise Explorat�ria 

summary(dados$Latam)
summary(dados$Petra)
summary(dados$B2W)
summary(dados$Selic)
summary(dados$IPCA)
summary(dados$Brent)
summary(dados$Dolar)
summary(dados$Ibov)
summary(dados$Juros.Futuro)
summary(dados$VIX)
summary(dados$Risco.Pais)
summary(dados$Nasdaq)
summary(dados$SP.500)
summary(dados$Dow.Jones)
summary(dados$EWZ)
summary(dados$EWZS)
summary(dados$Ouro)
summary(dados$Bitcoin)
summary(dados$Milho)

#Gr�ficos 

plot(ts(dados$Latam), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Pre�o - Latam")

plot(ts(dados$Petra), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Pre�o - Petra")

plot(ts(dados$B2W), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Pre�o - B2W")

plot(ts(dados$Selic), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Selic")

plot(ts(dados$IPCA), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="IPCA")

plot(ts(dados$Brent), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Pre�o - Brent")

plot(ts(dados$Dolar), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="D�lar")

plot(ts(dados$Ibov), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="�ndice Bovespa")

plot(ts(dados$Covid), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Covid - Dummy")

plot(ts(dados$Juros.Futuro), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Jurus Futuros-DI1F22")

plot(ts(dados$VIX), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="VIX")

plot(ts(dados$Risco.Pais), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Risco Pa�s")

plot(ts(dados$Nasdaq), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="�ndice Nasdaq")

plot(ts(dados$SP.500), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="�ndice Sp&500")

plot(ts(dados$Dow.Jones), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="�ndice Dow Jones")

plot(ts(dados$EWZ), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="�ndice EWZ")

plot(ts(dados$EWZS), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="�ndice EWZS")

plot(ts(dados$Ouro), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Ouro")

plot(ts(dados$Bitcoin), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Bitcoin")

plot(ts(dados$Milho), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Milho")

#Retornos 

retornos <- data.frame(dados$Data[-1],
                       diff(log(dados$Latam)),
                       diff(log(dados$Petra)),
                       diff(log(dados$B2W)),
                       diff(dados$Selic),
                       diff(dados$IPCA),
                       diff(log(dados$Brent)),
                       diff(log(dados$Dolar)),
                       dados$Covid[-1],
                       diff(dados$Juros.Futuro),
                       diff(log(dados$VIX)),
                       diff(dados$Risco.Pais),
                       diff(log(dados$Ibov)),
                       diff(log(dados$Nasdaq)),
                       diff(log(dados$SP.500)),
                       diff(log(dados$Dow.Jones)),
                       diff(log(dados$EWZ)),
                       diff(log(dados$EWZS)),
                       diff(log(dados$Ouro)),
                       diff(log(dados$Bitcoin)),
                       diff(log(dados$Milho)))
head(retornos)

colnames(retornos) <- c("Data","Latam","Petra","B2W",
                        "Selic","IPCA","Brent","Dolar",
                        "Covid","Juros","VIX", "Risco",
                        "Ibov","Nasdaq", "SP",
                        "Dow","EWZ","EWZS","Ouro","Bitcoin","Milho")

head(retornos) #04/06/2019
tail(retornos) #01/06/2020
dim(retornos) #260 observa��es 

#An�lise Explorat�ria 

summary(retornos$Latam)
summary(retornos$Petra)
summary(retornos$B2W)
summary(retornos$Selic)
summary(retornos$IPCA)
summary(retornos$Brent)
summary(retornos$Dolar)
summary(retornos$Ibov)
summary(retornos$Juros)
summary(retornos$VIX)
summary(retornos$Risco)
summary(retornos$Nasdaq)
summary(retornos$SP)
summary(retornos$DOW)
summary(retornos$EWZ)
summary(retornos$EWZS)
summary(retornos$Ouro)
summary(retornos$Bitcoin)
summary(retornos$Milho)

#Gr�ficos 

plot(ts(retornos$Latam), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Retornos - Latam")

plot(ts(retornos$Petra), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Retornos - Petra")

plot(ts(retornos$B2W), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Retornos - B2W")

plot(ts(retornos$Selic), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Selic")

plot(ts(retornos$IPCA), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="IPCA")

plot(ts(retornos$Brent), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Retornos - Brent")

plot(ts(retornos$Dolar), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Retornos - D�lar")

plot(ts(retornos$Ibov), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Retornos - �ndice Bovespa")

plot(ts(retornos$Covid), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Covid - Dummy")

plot(ts(retornos$Juros), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Jurus Futuros-DI1F22")

plot(ts(retornos$VIX), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="VIX")

plot(ts(retornos$Risco), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Risco Pa�s")

plot(ts(retornos$Nasdaq), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="�ndice Nasdaq")

plot(ts(retornos$SP), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="�ndice Sp&500")

plot(ts(retornos$Dow), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="�ndice Dow Jones")

plot(ts(retornos$EWZ), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="�ndice EWZ")

plot(ts(retornos$EWZS), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="�ndice EWZs")

plot(ts(retornos$Ouro), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Ouro")

plot(ts(retornos$Bitcoin), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Bitcoin")

plot(ts(retornos$Milho), lwd=2, col="darkblue", xlab="Tempo (em dias)", 
     ylab="Milho")

#Matriz de Correla��es 

#Todas as vari�veis 
matrizcor <- retornos[ ,-1]
head(matrizcor)
matrizcor <- cor(matrizcor)
matrizcor

corrplot(matrizcor, method = "color")

#Vari�veis Explicativas 
matrizcor2 <- retornos[ , -c(1,2,3,4)]
head(matrizcor2)
matrizcor2 <- cor(matrizcor2)
matrizcor2

corrplot(matrizcor2, method = "color")

#An�lise autom�tica
colnames(retornos)
retornos1 <- retornos[ , -c(1,2,3)]
colnames(retornos1)
ajuste=lm(B2W ~., data = retornos1)
step(ajuste,direction = "both")

#Fazer os testes com o modelo
fit = lm( Ibov ~ VIX + Risco+ Bitcoin+Dow, data = retornos)
summary(fit)
ajustados = as.vector(fit$fitted.values)
residuos = as.vector(fit$residuals)
df = data.frame(ajustados,residuos)

#An�lise de colinearidade e multicolinearidade

#Colinearidade
X = data.frame(Ibov=retornos$Ibov,VIX=retornos$VIX, Risco =retornos$Risco,
               Bitcoin=retornos$Bitcoin,Dow=retornos$Dow)
cor(X)

#Multicolinearidade
library(car)
fit = lm(Ibov ~ Risco + SP + EWZ + EWZS + Ouro + Bitcoin, data = retornos)
vif(fit)
#Como  existem vari�veis com VIF maior que 10, podemos dizer que h� multicolinearidade.

#Diagn�stico de normalidade
#Quantile-quantile plot (QQplot)
library(ggplot2)
ggplot(df, aes(sample = residuos)) +
  stat_qq() +
  stat_qq_line()
#Como os pontos seguem o comportamento da reta (n�o est�o distantes dela), com exce��o 
#dos quantis mais altos, temos ind�cios de que os erros s�o normalmente distribu�dos.

#Teste de Shapiro-Wilk < alfa
shapiro.test(residuos)
#os dados podem ser considerados normalmente distribu�dos (Menor que alfa)

#Teste de Kolmogorov-Smirnov > alfa
ks.test(residuos, "pnorm", mean = 0, sd = sd(residuos))
#p-valor maior que qualquer alfa. Os dados seguem uma distribui��o Normal

#Testes de homocedasticidade
#Gr�fico dos res�duos versus valores ajustados
ggplot(df, aes(x = ajustados, y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red")

#Teste de Breusch-Pagan > alfa
library(lmtest)
bptest(fit)
#Como p-valor menor que qualquer alfa usual, rejeitamos a hip�tese de que as 
#vari�ncias dos erros s�o iguais.

#Teste de Goldfeld-Quandt > alfa
library(lmtest)
gqtest(fit)
#J� no teste de Goldfeld-Quandt rejeita-se a hip�tese de homocedasticidade.No entanto, 
#entre as limita��es deste teste est� a exig�ncia de que a amostra seja relativamente grande

#Diagn�stico de independ�ncia

#Gr�fico dos res�duos versus a ordem de coleta
ggplot(df, aes(x=1:length(residuos), y = residuos)) +
  geom_line()

#Teste de Durbin-Watson > alfa
dwtest(fit)
#rejeitamos a hip�tese de que os res�duos s�o independentes.

#Valores Extremos
library(car)
outlierTest(fit)

#Teste da falta de ajuste (lack of fit)
library(alr3)
pureErrorAnova(fit)

