##======================================================================
## Diferentes formas de contrastes para modelos lineares
##======================================================================

##----------------------------------------------------------------------
## Exemplo do catalisador

dados <- read.csv("../dados/temp_cat.csv")
str(dados)
dados$Temperatura <- as.factor(dados$Temperatura)

## Separa em um fator
da <- aggregate(Rendimento ~ Temperatura + Catalisador,
                data = dados, FUN = mean)
da1 <- da[, -1]
plot(Rendimento ~ as.numeric(Catalisador), data = da1)

## Contraste treatment = zerar primeiro nivel
m1 <- lm(Rendimento ~ Catalisador, data = da1,
         contrasts = list(Catalisador = contr.treatment))
summary(m1)
tapply(da1$Rendimento, da1$Catalisador, mean)
diff(tapply(da1$Rendimento, da1$Catalisador, mean))
model.matrix(m1)

## Contraste soma zero
m3 <- lm(Rendimento ~ Catalisador, data = da1,
         contrasts = list(Catalisador = contr.sum))
summary(m3)
tapply(da1$Rendimento, da1$Catalisador, mean)
mean(da1$Rendimento) # media geral = intercepto
mean(da1$Rendimento) + coef(m3)[2] # media do catalisador A
mean(da1$Rendimento) - coef(m3)[2] # media do catalisador B
model.matrix(m3)

# Na mao
y <- da1$Rendimento
X <- rbind(c(1, 1),
           c(1, 1),
           c(1, -1),
           c(1, -1))
(Xt <- t(X))
Xt %*% X
solve(Xt %*% X)
Xt %*% y
solve(Xt %*% X) %*% Xt %*% y

sum(da1$Rendimento)/4 - mean(da1$Rendimento[da1$Catalisador == "B"])
mean(da1$Rendimento[da1$Catalisador == "A"]) - sum(da1$Rendimento)/4

##----------------------------------------------------------------------
## Adicionando mais um nivel para o catalisador
da2 <- data.frame(Catalisador = factor(rep(c("A", "B", "C"), each = 2)),
                  Rendimento = c(da1$Rendimento, 55, 63))
str(da2)
plot(Rendimento ~ as.numeric(Catalisador), data = da2)
## Contraste treatment
m1 <- lm(Rendimento ~ Catalisador, data = da2,
         contrasts = list(Catalisador = contr.treatment))
summary(m1)
tapply(da2$Rendimento, da2$Catalisador, mean)
model.matrix(m1)

## Contraste soma zero
m3 <- lm(Rendimento ~ Catalisador, data = da2,
         contrasts = list(Catalisador = contr.sum))
summary(m3)
tapply(da2$Rendimento, da2$Catalisador, mean)
mean(da2$Rendimento) # media geral = intercepto
mean(da2$Rendimento) + coef(m3)[2] # media do catalisador A
mean(da2$Rendimento) + coef(m3)[3] # media do catalisador B
-coef(m3)[2] - coef(m3)[3] # tau_3
mean(da2$Rendimento) - coef(m3)[2] - coef(m3)[3] # media do catalisador C
model.matrix(m3)

# Na mao
y <- da2$Rendimento
X <- rbind(c(1, 1, 0),
           c(1, 1, 0),
           c(1, 0, 1),
           c(1, 0, 1),
           c(1, -1, -1),
           c(1, -1, -1))
(Xt <- t(X))
Xt %*% X
solve(Xt %*% X)
MASS::fractions(solve(Xt %*% X))
Xt %*% y
solve(Xt %*% X) %*% Xt %*% y

sum(da2$Rendimento[da2$Catalisador == "A"])/3 -
    sum(da2$Rendimento[da2$Catalisador == "B"])/6 -
    sum(da2$Rendimento[da2$Catalisador == "C"])/6

##======================================================================
## Modelo com 2 fatores
##======================================================================

## Agrega dados brutos
da <- aggregate(Rendimento ~ Temperatura + Catalisador,
                data = dados, FUN = mean)
# Qual a matriz X?
str(da)
with(dados,
     interaction.plot(Temperatura, Catalisador, Rendimento, mean))

da$Temperatura <- as.factor(da$Temperatura)

## Contraste treatment
m1 <- lm(Rendimento ~ Temperatura * Catalisador, data = da,
         contrasts = list(Temperatura = contr.treatment,
                          Catalisador = contr.treatment))
summary(m1)
da
model.matrix(m1)
## Contraste treatment com dadps completos
m1 <- lm(Rendimento ~ Temperatura * Catalisador, data = dados,
         contrasts = list(Temperatura = contr.treatment,
                          Catalisador = contr.treatment))
summary(m1)
da
with(dados, tapply(Rendimento, list(Temperatura, Catalisador), mean))


## Contraste soma zero
m3 <- lm(Rendimento ~ Catalisador * Temperatura, data = da,
         contrasts = list(Temperatura = contr.sum,
                          Catalisador = contr.sum))
summary(m3)
m3aov <- aov(Rendimento ~ Catalisador * Temperatura, data = da)
summary(m3aov)
dae::yates.effects(m3aov, data = da)
dae::yates.effects(m3aov, data = da)/2

## Media geral
mean(da$Rendimento)
model.tables(m3aov)
model.matrix(m3)

## Catalisador
(mean(da$Rendimento[da$Catalisador == "A"]) -
 mean(da$Rendimento[da$Catalisador == "B"]))/2

mean(da$Rendimento) + coef(m3)[2] # media do catalisador A
mean(da$Rendimento[da$Catalisador == "A"])
mean(da$Rendimento) - coef(m3)[2] # media do catalisador B
mean(da$Rendimento[da$Catalisador == "B"])

## Temperatura
(mean(da$Rendimento[da$Temperatura == "40"]) -
 mean(da$Rendimento[da$Temperatura == "60"]))/2

mean(da$Rendimento) + coef(m3)[3] # media da temperatura BAIXA
mean(da$Rendimento[da$Temperatura == "40"])
mean(da$Rendimento) - coef(m3)[3] # media do temperatura ALTA
mean(da$Rendimento[da$Temperatura == "60"])

## Catalisador:Temperatura
(mean(c(da$Rendimento[da$Catalisador == "A" & da$Temperatura == "40"],
       da$Rendimento[da$Catalisador == "B" & da$Temperatura == "60"])) -
    mean(c(da$Rendimento[da$Catalisador == "A" & da$Temperatura == "60"],
           da$Rendimento[da$Catalisador == "B" & da$Temperatura == "40"])))/2

mean(da$Rendimento) + coef(m3)[4] # media de A,40:B,60
mean(c(da$Rendimento[da$Catalisador == "A" & da$Temperatura == "40"],
       da$Rendimento[da$Catalisador == "B" & da$Temperatura == "60"]))
mean(da$Rendimento) - coef(m3)[4] # media de A,60:B,40
mean(c(da$Rendimento[da$Catalisador == "A" & da$Temperatura == "60"],
       da$Rendimento[da$Catalisador == "B" & da$Temperatura == "40"]))

##----------------------------------------------------------------------
# Na mao
y <- da$Rendimento
X <- rbind(c(1, 1, 1, 1),
           c(1, 1, -1, -1),
           c(1, -1, 1, -1),
           c(1, -1, -1, 1))
(Xt <- t(X))
Xt %*% X
solve(Xt %*% X)
MASS::fractions(solve(Xt %*% X))
Xt %*% y
## Soma total
sum(da$Rendimento)
## Soma catalisador
sum(da$Rendimento[da$Catalisador == "A"]) -
    sum(da$Rendimento[da$Catalisador == "B"])
## Soma temperatura
sum(da$Rendimento[da$Temperatura == "40"]) -
    sum(da$Rendimento[da$Temperatura == "60"])
## Soma interacao
sum(da$Rendimento[da$Catalisador == "A" & da$Temperatura == "40"],
    da$Rendimento[da$Catalisador == "B" & da$Temperatura == "60"]) -
    sum(da$Rendimento[da$Catalisador == "A" & da$Temperatura == "60"],
        da$Rendimento[da$Catalisador == "B" & da$Temperatura == "40"])
solve(Xt %*% X) %*% Xt %*% y

## Media geral
mean(da$Rendimento)
## Media catalisador
(mean(da$Rendimento[da$Catalisador == "A"]) -
    mean(da$Rendimento[da$Catalisador == "B"]))/2
## Media temperatura
(mean(da$Rendimento[da$Temperatura == "40"]) -
    mean(da$Rendimento[da$Temperatura == "60"]))/2
## Media interacao
((mean(c(da$Rendimento[da$Temperatura == "40" & da$Catalisador == "A"],
         da$Rendimento[da$Temperatura == "60" & da$Catalisador == "B"]))) -
    (mean(c(da$Rendimento[da$Temperatura == "60" & da$Catalisador == "A"],
            da$Rendimento[da$Temperatura == "40" & da$Catalisador == "B"]))))/2

dae::yates.effects(m3aov, data = da)
dae::yates.effects(m3aov, data = da)/2
