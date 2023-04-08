library(caTools)
library("randomForest")
library(caret)
library(xgboost) #for fitting the xgboost model
library(ROCR)

set.seed(108)


dados <- list()
divisao <- list()
treinamento <- list()
teste <- list()
regressao <- list()
floresta <- list()
treinamento_x <- list()
treinamento_y <- list()
teste_x <- list()
teste_y <- list()
xgb_treinamento <- list()
xgb_teste <- list()
XGboost <- list()
pred_regressao <- list()
pred_floresta <- list()
pred_xgboost <- list()
roc_regressao <- list()
roc_floresta <- list()
roc_xgboost <- list()
auc_regressao <- list()
auc_floresta <- list()
auc_xgboost <- list()
ks.test_regressao <- list()
test.ks_regressao <- list()
ks.test_floresta <- list()
test.ks_floresta <- list() 
ks.test_xgboost <- list()
test.ks_xgboost <- list()
predicao_logistica <- list()
acuracia_logistica <- list()
predicao_floresta <- list()
acuracia_floresta <- list()
predicao_xgboost <- list()
acuracia_xgboost <- list()
prob <- NA

valor = 500

for (p in 1:valor) {
  
  dados[[p]] <- data.frame(
    sexo=sample(c(0,1), size = 1000, replace = TRUE),
    idade=round(runif(1000, 18, 80)),
    renda_sm=round(runif(1000,500,30000))/1212
  )
  
  cenario=(-4+3*dados[[p]]$sexo+
             0.05*(dados[[p]]$idade-50)^2-
             1.5*dados[[p]]$renda_sm
            +rnorm(1000,0,1))
  
#  cenario=(-8+10*dados[[p]]$sexo+
#             0.1*dados[[p]]$idade^2-
#             4*dados[[p]]$renda_sm
#           +rnorm(1000,0,1))
  
  mu=(1/(1 + exp(-cenario))) #usando link logit
  #mu=exp(-exp(cenario)) #usando link log-log

  phi = 1
  
  shape1 = mu*phi
  shape2 = (1-mu)*phi
  
  for (i in 1:1000)
  {
    prob[i]=rbeta(1, shape1[i], shape2[i])
  }

  ## gerando y a patir das probs de alguma distribuição 
  #dados[[p]]$y=factor(ifelse(prob>0.5,1,0))
  dados[[p]]$y=factor(ifelse(prob>=quantile(prob,0.4),1,0))

  # Divisão treinamento e teste
  
  divisao[[p]] <- createDataPartition(dados[[p]]$y, p = .7, list = F)
  treinamento[[p]] <- dados[[p]][divisao[[p]],]
  teste[[p]] <- dados[[p]][-divisao[[p]],]  
  
  # Regressão Logistica
  
  regressao[[p]] <- glm(y~sexo+idade+renda_sm, family="binomial", data = treinamento[[p]])

  # Floresta Aleatória
  
  floresta[[p]] <- randomForest(y~sexo+idade+renda_sm, data=treinamento[[p]])

  # XGBoost
  
  #define variáveis preditor e resposta no conjunto de treinamento
  
  treinamento_x[[p]] = data.matrix(treinamento[[p]][, -4])
  treinamento_y[[p]] = as.numeric(treinamento[[p]][,4])-1
  
  #define variáveis preditor e resposta no conjunto de testees
  
  teste_x[[p]] = data.matrix(teste[[p]][, -4])
  teste_y[[p]] = as.numeric(teste[[p]][, 4])-1
  
  #define conjuntos finais de treinamento e testees
  
  xgb_treinamento[[p]] = xgb.DMatrix(data = treinamento_x[[p]], label = treinamento_y[[p]])
  xgb_teste[[p]] = xgb.DMatrix(data = teste_x[[p]], label = teste_y[[p]])
  

  #modelo final
  XGboost[[p]] = xgboost(data = xgb_treinamento[[p]], nrounds = 30, verbose = 0,
                         objective = "binary:logistic")
  
  # Validação no teste
  
  pred_regressao[[p]] = predict(regressao[[p]],teste[[p]],type = "response")
  pred_floresta[[p]] = predict(floresta[[p]],teste[[p]],type="prob")[,2]
  pred_xgboost[[p]] = predict(XGboost[[p]],teste_x[[p]])
  
  # criar curva roc
  roc_regressao[[p]] <- prediction(pred_regressao[[p]], teste[[p]]$y)
  roc_floresta[[p]] <- prediction(pred_floresta[[p]], teste[[p]]$y)
  roc_xgboost[[p]] <- prediction(pred_xgboost[[p]], teste_y[[p]])
  
  # calcule a area sob a curva
   auc_regressao[[p]] <-  as.numeric(slot(performance(roc_regressao[[p]], "auc"), "y.values"))
   auc_floresta[[p]] <-   as.numeric(slot(performance( roc_floresta[[p]], "auc"),"y.values"))
   auc_xgboost[[p]] <-  as.numeric(slot(performance( roc_xgboost[[p]], "auc"),"y.values"))
  
   # ks
  
   ks.test_regressao[[p]] <- performance(roc_regressao[[p]], "tpr", "fpr")
   test.ks_regressao[[p]] <- max(attr(ks.test_regressao[[p]], "y.values")[[1]] - (attr(ks.test_regressao[[p]], "x.values")[[1]])) 
   ks.test_floresta[[p]] <- performance(roc_floresta[[p]], "tpr", "fpr")
   test.ks_floresta[[p]] <- max(attr(ks.test_floresta[[p]], "y.values")[[1]] - (attr(ks.test_floresta[[p]], "x.values")[[1]])) 
   ks.test_xgboost[[p]] <- performance(roc_xgboost[[p]], "tpr", "fpr")
   test.ks_xgboost[[p]] <- max(attr(ks.test_xgboost[[p]], "y.values")[[1]] - (attr(ks.test_xgboost[[p]], "x.values")[[1]])) 

   # Acuracia
   
   predicao_logistica[[p]] = ifelse(pred_regressao[[p]]>0.5,1,0)
   acuracia_logistica[[p]] = sum(predicao_logistica[[p]]==teste[[p]]$y)/
     length(teste[[p]]$y)
   
   predicao_floresta[[p]] = ifelse(pred_floresta[[p]]>0.5,1,0)
   acuracia_floresta[[p]] = sum(predicao_floresta[[p]]==teste[[p]]$y)/
     length(teste[[p]]$y)
   
   predicao_xgboost[[p]] = ifelse(pred_xgboost[[p]]>0.5,1,0)
   acuracia_xgboost[[p]] = sum(predicao_xgboost[[p]]==teste_y[[p]])/
     length(teste_y[[p]])
   
}

# Medidas resumidas AUC

summary(as.numeric(auc_regressao))
summary(as.numeric(auc_floresta))
summary(as.numeric(auc_xgboost))

sd(as.numeric(auc_regressao))
sd(as.numeric(auc_floresta))
sd(as.numeric(auc_xgboost))


# Medidas resumidas KS

summary(as.numeric(test.ks_regressao))
summary(as.numeric(test.ks_floresta))
summary(as.numeric(test.ks_xgboost))

sd(as.numeric(test.ks_regressao))
sd(as.numeric(test.ks_floresta))
sd(as.numeric(test.ks_xgboost))

# Medidas resumidas Acuracia

summary(as.numeric(acuracia_logistica))
summary(as.numeric(acuracia_floresta))
summary(as.numeric(acuracia_xgboost))

sd(as.numeric(acuracia_logistica))
sd(as.numeric(acuracia_floresta))
sd(as.numeric(acuracia_xgboost))



## Boxplot AUC, KS e Acuracia

par(mfrow=c(1,3))

boxplot_auc_regressao <- boxplot(as.numeric(auc_regressao),
        main = "Cenário 2: AUC Regressão para n = 1000",
        xlab = "AUC Regressão Logística", ylim = c(0.4,1),
        col = "lightblue",pch=19,
        horizontal = T,notch=F)


boxplot_auc_floresta <- boxplot(as.numeric(auc_floresta),
                                 main = "Cenário 2: AUC Floresta para n = 1000",
                                 xlab = "AUC Floresta Aleatória",
                                 ylim = c(0.4,1),
                                 col = "lightpink",pch=19,
                                 horizontal = T,notch=F)


boxplot_auc_xgboost <- boxplot(as.numeric(auc_xgboost),
                                main = "Cenário 2: AUC XGBoost para n = 1000",
                                xlab = "AUC XGBoost",
                                ylim = c(0.4,1),
                                col = "lightgreen",pch=19,
                                horizontal = T,notch=F)

par(mfrow=c(1,3))

boxplot_ks_regressao <- boxplot(as.numeric(test.ks_regressao),
                                main = "Cenário 2: KS Regressão para n = 1000",
                                xlab = "KS Regressão Logística",
                                ylim = c(0.2,1),
                                col = "lightblue",pch=19,
                                horizontal = T,notch=F)


boxplot_ks_floresta <- boxplot(as.numeric(test.ks_floresta),
                               main = "Cenário 2: KS Floresta para n = 1000",
                               xlab = "KS Floresta Aleatória",
                               ylim = c(0.2,1),
                               col = "lightpink",pch=19,
                               horizontal = T,notch=F)
boxplot_ks_xgboost <- boxplot(as.numeric(test.ks_xgboost),
                              main = "Cenário 2: KS XGBoost para n = 1000",
                              xlab = "KS XGBoost",
                              ylim = c(0.2,1),
                              col = "lightgreen",pch=19,
                              horizontal = T,notch=F)

par(mfrow=c(1,3))

boxplot_ac_regressao <- boxplot(as.numeric(acuracia_logistica),
                                main = "Cenário 2: Acurácia Regressão para n = 1000",
                                xlab = "Acurácia Regressão Logística",
                                ylim = c(0.35,1),
                                col = "lightblue",pch=19,
                                horizontal = T,notch=F)


boxplot_ac_floresta <- boxplot(as.numeric(acuracia_floresta),
                               main = "Cenário 2: Acurácia Floresta para n = 1000",
                               xlab = "Acurácia Floresta Aleatória",
                               ylim = c(0.35,1),
                               col = "lightpink",pch=19,
                               horizontal = T,notch=F)
boxplot_ac_xgboost <- boxplot(as.numeric(acuracia_xgboost),
                              main = "Cenário 2: Acurácia XGBoost para n = 1000",
                              xlab = "Acurácia XGBoost",
                              ylim = c(0.35,1),
                              col = "lightgreen",pch=19,
                              horizontal = T,notch=F)
dev.off()


plot(prob~dados[[p]]$renda_sm)
plot(prob~dados[[p]]$sexo)
plot(prob~dados[[p]]$idade)

