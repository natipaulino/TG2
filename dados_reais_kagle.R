library(caTools)
library("randomForest")
library(caret)
library(xgboost) #for fitting the xgboost model
library(ROCR)
library(truncnorm)


set.seed(108)

library(readxl)
dados <- read_excel("C:/Users/natal/OneDrive/Área de Trabalho/Semestre 2022.2/TG2/kaggle.xlsx")
View(dados)
summary(dados)
dim(dados)

dados <- dados[complete.cases(dados),]
summary(dados)
dim(dados)

dados = data.frame(dados)
head(dados)


colnames(dados) <- c("y", "uso_credito", "idade", "dividas_30_59", "credito_tomado",
                     "salario", "num_emprestimos", "dividas_90mais", "num_emp_imob",
                     "dividas_60_89", "dependentes")
#dados$historico = as.factor(dados$historico)

dados$y = as.factor(dados$y)


######## Descritiva
library(ggplot2)

##### Idade

boxplot(idade~y,dados)

idade<-ggplot(dados, aes(x=idade, y=y)) +
  geom_boxplot(fill=c("#0072B2", "#D63B2F"))+
  labs(title="Boxplot da idade por Inadimplência/Adimplência",x="Idade",
       y = "Inadimplência = 1  e Adimplência = 0")
idade

idade2 <- idade + scale_fill_manual(breaks = c("0", "1"), 
                      values=c("#0072B2", "#D63B2F"))
idade2

##### Número Empréstimos Imobiliários

num_emp_imob <-ggplot(dados, aes(x=num_emp_imob, y=y)) +
  geom_boxplot(fill=c("#0072B2", "#D63B2F"))+
  labs(title="Boxplot Número Empréstimos Imobiliários por Inadimplência/Adimplência",x="Número Empréstimos Imobiliários",
       y = " Inadimplência = 1  e Adimplência = 0")

num_emp_imob

num_emp_imob2 <- num_emp_imob + scale_fill_manual(breaks = c("0", "1"), 
                          values=c("#0072B2", "#D63B2F"))


dados_inadimp = subset(dados, y==1)
summary(dados_inadimp)
dados_inadimp2 = subset(dados, y==0)
summary(dados_inadimp2)

##### uso_credito

uso_credito <- ggplot(dados, aes(x=uso_credito,y=y)) +
  geom_boxplot(fill=c("#0072B2", "#D63B2F"))+
  xlim(0, 5)+
  labs(title="Uso de Credito por Inadimplência/Adimplência",x="Uso de Crédito",
       y =  "Inadimplência = 1  e Adimplência = 0")

uso_credito

uso_credito2<- uso_credito + scale_fill_manual(breaks = c("0", "1"), 
                                                              values=c("#0072B2", "#D63B2F"))

uso_credito2

info_perdeu1 <- (182/120269)*100
info_perdeu1


##### dividas_30_59

dividas_30_59 <-ggplot(dados, aes(x=dividas_30_59,y=y)) +
  geom_boxplot(fill=c("#0072B2", "#D63B2F"))+
  xlim(0, 8)+
  labs(title="N° de atraso 30 a 59 dias por Inadimplência/Adimplência",
       x="N° de atraso 30 a 59 dias", y = "Inadimplência = 1  e Adimplência = 0")

dividas_30_59

dividas_30_592<- dividas_30_59 + scale_fill_manual(breaks = c("0", "1"), 
                                               values=c("#0072B2", "#D63B2F"))

dividas_30_592


info_perdeu2 <- (165/120269)*100
info_perdeu2

##### dividas_60_89

dividas_60_89 <-ggplot(dados, aes(x=dados$dividas_60_89,y=y)) +
  geom_boxplot(fill=c("#0072B2", "#D63B2F"))+
  xlim(0, 8)+
  labs(title="N° de atraso  60 a 89 dias por Inadimplência/Adimplência",
       x="N° de atraso 60 a 89 dias", y = "Inadimplência = 1  e Adimplência = 0")

dividas_60_89

dividas_60_892<- dividas_60_89 + scale_fill_manual(breaks = c("0", "1"), 
                                                 values=c("#0072B2", "#D63B2F"))


dividas_60_892

info_perdeu3 <- (150/120269)*100
info_perdeu3

##### dividas_90mais

dividas_90mais <-ggplot(dados, aes(x=dividas_90mais, y=y)) +
  geom_boxplot(fill=c("#0072B2", "#D63B2F"))+
  xlim(0, 10)+
  labs(title="N° de atraso 90 dias de atraso por Inadimplência/Adimplência",
       x="N° de atraso 90 dias", y = "Inadimplência = 1  e Adimplência = 0")

dividas_90mais

dividas_90mais2<- dividas_90mais + scale_fill_manual(breaks = c("0", "1"), 
                                                 values=c("#0072B2", "#D63B2F"))


dividas_90mais2

info_perdeu3 <- (163/120269)*100
info_perdeu3


##### salario

salario <-ggplot(dados, aes(x=salario, y=y,fill=y)) +
  geom_boxplot(fill=c("#0072B2", "#D63B2F"))+
  xlim(0, 60000)+
  labs(title="Renda Mensal por Inadimplência/Adimplência",x="Renda Mensal",
       y = "Inadimplência = 1  e Adimplência = 0")

salario

salario2<- salario + scale_fill_manual(breaks = c("0", "1"), 
                                                 values=c("#0072B2", "#D63B2F"))



salario2

info_perdeu4 <- (221/120269)*100
info_perdeu4



##### Numero de emprestimo

num_emprestimos <-ggplot(dados, aes(x=num_emprestimos, y=y)) +
  geom_boxplot(fill=c("#0072B2", "#D63B2F"))+
  xlim(0, 40)+
  labs(title="N° Empréstimo por Inadimplência/Adimplência",x="N° Empréstimo", 
       y = "Inadimplência = 1  e Adimplência = 0")

num_emprestimos

num_emprestimos2<- num_emprestimos + scale_fill_manual(breaks = c("0", "1"), 
                                           values=c("#0072B2", "#D63B2F"))


num_emprestimos2

info_perdeu5 <- (57/120269)*100
info_perdeu5

##### Numero de credito_tomado

num_emp_imob <-ggplot(dados, aes(x=credito_tomado, y=y)) +
  geom_boxplot(fill=c("#0072B2", "#D63B2F"))+
  xlim(0, 5)+
  labs(title="N° Credito Tomado por Inadimplência/Adimplência",
       x="N°Credito Tomado", y = "Inadimplência = 1  e Adimplência = 0")

num_emp_imob

num_emp_imob2<- num_emp_imob + scale_fill_manual(breaks = c("0", "1"), 
                                                   values=c("#0072B2", "#D63B2F"))


num_emp_imob2

info_perdeu6 <- (2352/120269)*100
info_perdeu6
##### Dependentes

dependentes <-ggplot(dados, aes(x=dependentes, y=y,fill=y)) +
  geom_boxplot(fill=c("#0072B2", "#D63B2F"))+
  xlim(0,10)+
  labs(title="N° de dependentes por Inadimplência/Adimplência",
       x=" N° de dependentes", y = "Inadimplência = 1  e Adimplência = 0")

dependentes

dependentes2<-dependentes +  scale_fill_manual(breaks = c("0", "1"), 
                                                values=c("#0072B2", "#D63B2F"))

dependentes2

info_perdeu7 <- (2/120269)*100
info_perdeu7

#### Divisão treinamento e teste

divisao <- createDataPartition(dados$y, p = .7, list = F)
treinamento <- dados[divisao,]
teste <- dados[-divisao,]  

# Regressão Logistica

regressao <- glm(y~., family="binomial", data = treinamento)
summary(regressao)


### Calculando odds-ratio. 
### A odds-ratio mostra a raz?o de chances
### do sucesso ocorrer, quando comparamos diferentes grupos,
### com as outras covari?veis mantidas fixas.

#install.packages("epiDisplay")
#library(epiDisplay)

Odds_Ratio = exp(cbind(coef(regressao), confint(regressao, level = 0.95)))
colnames(Odds_Ratio) = c("Razão de Chances","I.C. 2.5%", "I.C. 97.5%")
Odds_Ratio

# Floresta Aleatória

floresta <- randomForest(y~., data=treinamento)

# XGBoost

#define variáveis preditor e resposta no conjunto de treinamento

treinamento_x = data.matrix(treinamento[, -1])
treinamento_y = as.numeric(treinamento[,1])-1

#define variáveis preditor e resposta no conjunto de testees

teste_x = data.matrix(teste[, -1])
teste_y = as.numeric(teste[, 1])-1

#define conjuntos finais de treinamento e testees

xgb_treinamento = xgb.DMatrix(data = treinamento_x, label = treinamento_y)
xgb_teste = xgb.DMatrix(data = teste_x, label = teste_y)


#modelo final
XGboost = xgboost(data = xgb_treinamento, nrounds = 30, verbose = 0,
                       objective = "binary:logistic")

# Validação no teste

pred_regressao = predict(regressao,teste,type = "response")
pred_floresta = predict(floresta,teste,type="prob")[,2]
pred_xgboost = predict(XGboost,teste_x)

# criar curva roc
roc_regressao <- prediction(pred_regressao, teste$y)
roc_floresta <- prediction(pred_floresta, teste$y)
roc_xgboost <- prediction(pred_xgboost, teste_y)

# calcule a area sob a curva
auc_regressao <-  as.numeric(slot(performance(roc_regressao, "auc"), "y.values"))
auc_floresta <-   as.numeric(slot(performance( roc_floresta, "auc"),"y.values"))
auc_xgboost <-  as.numeric(slot(performance( roc_xgboost, "auc"),"y.values"))

# ks

ks.test_regressao <- performance(roc_regressao, "tpr", "fpr")
test.ks_regressao <- max(attr(ks.test_regressao, "y.values")[[1]] - (attr(ks.test_regressao, "x.values")[[1]])) 
ks.test_floresta <- performance(roc_floresta, "tpr", "fpr")
test.ks_floresta <- max(attr(ks.test_floresta, "y.values")[[1]] - (attr(ks.test_floresta, "x.values")[[1]])) 
ks.test_xgboost <- performance(roc_xgboost, "tpr", "fpr")
test.ks_xgboost <- max(attr(ks.test_xgboost, "y.values")[[1]] - (attr(ks.test_xgboost, "x.values")[[1]])) 

# Acuracia

predicao_logistica = ifelse(pred_regressao>0.5,1,0)
acuracia_logistica = sum(predicao_logistica==teste$y)/
  length(teste$y)

predicao_floresta = ifelse(pred_floresta>0.5,1,0)
acuracia_floresta = sum(predicao_floresta==teste$y)/
  length(teste$y)

predicao_xgboost = ifelse(pred_xgboost>0.5,1,0)
acuracia_xgboost = sum(predicao_xgboost==teste_y)/
  length(teste_y)


# Medidas resumidas AUC

auc_regressao
auc_floresta
auc_xgboost


# Medidas resumidas KS

as.numeric(test.ks_regressao)
as.numeric(test.ks_floresta)
as.numeric(test.ks_xgboost)


# Medidas resumidas Acuracia

as.numeric(acuracia_logistica)
as.numeric(acuracia_floresta)
as.numeric(acuracia_xgboost)


# Plotando a distribuição da probabilidade para os grupos de adimplentes e inadimplentes
# na base teste

predicoes <- data.frame(cbind(pred_regressao, pred_floresta, 
                              pred_xgboost, teste_y))
colnames(predicoes) = c("regressao", "floresta", "xgboost", "y")
predicoes

pred_regressao_plot = ggplot(predicoes, aes(x=regressao, colour=as.factor(y))) + geom_density()+
  scale_color_manual(name="y",
                     labels=c("0", "1"),
                     values=c("#0072B2", "#D63B2F"))+
  labs(title="Predição da Regressão Logística",x="Probabilidade da Regressão Logística", y = "Densidade")


pred_floresta_plot = ggplot(predicoes, aes(x=floresta, colour=as.factor(y))) + geom_density()+
  scale_color_manual(name="y",
                     labels=c("0", "1"),
                     values=c("#0072B2", "#D63B2F"))+
  labs(title="Predição da Floresta Aleatória",x="Probabilidade da Floresta Aleatória", y = "Densidade")

pred_xgboost_plot = ggplot(predicoes, aes(x=xgboost, colour=as.factor(y))) + geom_density()+
  scale_color_manual(name="y",
                     labels=c("0", "1"),
                     values=c("#0072B2", "#D63B2F"))+
  labs(title="Predição do XGBoost",x="Probabilidade do XGBoost", y = "Densidade")

graf3 = gridExtra::grid.arrange(pred_regressao_plot,pred_floresta_plot,pred_xgboost_plot,
                                nrow = 3)


# Plotando as curvas ROC

library(pROC)
roc_score_regressao = roc(teste$y, pred_regressao)
plot(roc_score_regressao)

roc_score_floresta = roc(teste$y, pred_floresta)
plot(roc_score_floresta)

roc_score_xgboost = roc(teste$y, pred_xgboost)
plot(roc_score_xgboost)

plot(roc_score_regressao, main = "Curva Roc", xlab = "(1-Especificidade)",
     ylab = "Sensibilidade", col ="#8DA0CB" )
legend(x =0.6 , y = 0.3,legend =c("Regressão", "Floresta Aleatória","XGBoost"), 
       col = c("#8DA0CB" , "#66C2A5" ,"#FC8D62"), lty = c(1,1,1), bty="n")
lines(roc_score_floresta,col= "#66C2A5")
lines(roc_score_xgboost,col="#FC8D62")

