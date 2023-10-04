#Naive Bayes 4D por categoria

########## Event1D = 1 ##########
#Carregando a base de treinamento
base_train <- read.csv2("base_incidentes_train1.csv",sep = ",",header = TRUE)
base_train <- base_train %>% select(Final.Narrative_clean,Event4D)
base_train$Event4D <- as.factor(base_train$Event4D)

#Transformando a base de treinamento em corpus
corpus = VCorpus(VectorSource(base_train$Final.Narrative_clean)) 

#Document Term Matrix
dtm = DocumentTermMatrix(corpus) 
dtm = removeSparseTerms(dtm, 0.999) 

convert <- function(x) 
{
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}  

datanaive = apply(dtm, 2, convert)
dataset_train = as.data.frame(as.matrix(datanaive))    
dataset_train$Class = factor(base_train$Event4D)

#####Preparando a base de teste
#Carregando a base de teste
base_test <- read.csv2("base_incidentes_test1.csv",sep = ",",header = TRUE)
base_test <- base_test %>% select(Final.Narrative_clean,Event4D)
base_test$Event4D <- as.factor(base_test$Event4D)

#Transformando a base de teste em corpus
corpus = VCorpus(VectorSource(base_test$Final.Narrative_clean)) 

#Document Term Matrix
dtm_test = DocumentTermMatrix(corpus) 
dtm_test = removeSparseTerms(dtm_test, 0.999) 

convert <- function(x) 
{
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}  

datanaive_test = apply(dtm_test, 2, convert)
dataset_test = as.data.frame(as.matrix(datanaive_test))    
dataset_test$Class = as.factor(base_test$Event4D)

###### CLASSIFICADOR
#nivelando
complete_factor_levels <- c(dataset_train$Class %>% factor %>% levels, dataset_test$Class %>% factor %>% levels) %>% unique
dataset_train <- dataset_train %>%
  mutate(Class = factor(Class, levels = complete_factor_levels))
dataset_test <- dataset_test %>%
  mutate(Class = factor(Class, levels = complete_factor_levels))

#Classificador
classifier_nb <- naiveBayes(dataset_train, dataset_train$Class)
nb_pred = predict(classifier_nb, type = 'class', newdata =  dataset_test)
confusionMatrix(nb_pred,dataset_test$Class)

#Ajustando a matriz confusao para plotagem
confusion_graf <- data.frame(confusionMatrix(nb_pred,dataset_test$Class)$table)
confusion_soma_col <- confusion_graf %>% 
  group_by(Reference) %>% 
  summarise(soma_col = sum(Freq))
confusion_graf <- confusion_graf %>%
  left_join(confusion_soma_col,join_by(Reference == Reference))
confusion_graf$perc <- confusion_graf$Freq/confusion_graf$soma_col

#Plotando a Matriz confusao
confusion_graf %>%
  ggplot(aes(Prediction,Reference, fill = perc))+
  geom_tile()+
  scale_fill_gradient2(low="lightblue",high = "darkblue",label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Categoria original",
       y = "Categoria prevista",
       fill = "% de ocorrencias")

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(nb_pred,dataset_test$Class)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#Construindo a tabela predito x referencia
base_predNB2 <- as.data.frame(nb_pred)
base_predNB2$Event4D <- base_test$Event4D


########## Event1D = 2 ##########
#Carregando a base de treinamento
base_train <- read.csv2("base_incidentes_train2.csv",sep = ",",header = TRUE)
base_train <- base_train %>% select(Final.Narrative_clean,Event4D)
base_train$Event4D <- as.factor(base_train$Event4D)

#Transformando a base de treinamento em corpus
corpus = VCorpus(VectorSource(base_train$Final.Narrative_clean)) 

#Document Term Matrix
dtm = DocumentTermMatrix(corpus) 
dtm = removeSparseTerms(dtm, 0.999) 

convert <- function(x) 
{
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}  

datanaive = apply(dtm, 2, convert)
dataset_train = as.data.frame(as.matrix(datanaive))    
dataset_train$Class = factor(base_train$Event4D)

#####Preparando a base de teste
#Carregando a base de teste
base_test <- read.csv2("base_incidentes_test2.csv",sep = ",",header = TRUE)
base_test <- base_test %>% select(Final.Narrative_clean,Event4D)
base_test$Event4D <- as.factor(base_test$Event4D)

#Transformando a base de teste em corpus
corpus = VCorpus(VectorSource(base_test$Final.Narrative_clean)) 

#Document Term Matrix
dtm_test = DocumentTermMatrix(corpus) 
dtm_test = removeSparseTerms(dtm_test, 0.999) 

convert <- function(x) 
{
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}  

datanaive_test = apply(dtm_test, 2, convert)
dataset_test = as.data.frame(as.matrix(datanaive_test))    
dataset_test$Class = as.factor(base_test$Event4D)

###### CLASSIFICADOR
#nivelando
complete_factor_levels <- c(dataset_train$Class %>% factor %>% levels, dataset_test$Class %>% factor %>% levels) %>% unique
dataset_train <- dataset_train %>%
  mutate(Class = factor(Class, levels = complete_factor_levels))
dataset_test <- dataset_test %>%
  mutate(Class = factor(Class, levels = complete_factor_levels))

#Classificador
classifier_nb <- naiveBayes(dataset_train, dataset_train$Class)
nb_pred = predict(classifier_nb, type = 'class', newdata =  dataset_test)
confusionMatrix(nb_pred,dataset_test$Class)

#Ajustando a matriz confusao para plotagem
confusion_graf <- data.frame(confusionMatrix(nb_pred,dataset_test$Class)$table)
confusion_soma_col <- confusion_graf %>% 
  group_by(Reference) %>% 
  summarise(soma_col = sum(Freq))
confusion_graf <- confusion_graf %>%
  left_join(confusion_soma_col,join_by(Reference == Reference))
confusion_graf$perc <- confusion_graf$Freq/confusion_graf$soma_col

#Plotando a Matriz confusao
confusion_graf %>%
  ggplot(aes(Prediction,Reference, fill = perc))+
  geom_tile()+
  scale_fill_gradient2(low="lightblue",high = "darkblue",label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Categoria original",
       y = "Categoria prevista",
       fill = "% de ocorrencias")

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(nb_pred,dataset_test$Class)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#Construindo a tabela predito x referencia
base_predaux <- as.data.frame(nb_pred)
base_predaux$Event4D <- base_test$Event4D
base_predNB2 <- rbind(base_predNB2,base_predaux)

########## Event1D = 3 ##########
#Carregando a base de treinamento
base_train <- read.csv2("base_incidentes_train3.csv",sep = ",",header = TRUE)
base_train <- base_train %>% select(Final.Narrative_clean,Event4D)
base_train$Event4D <- as.factor(base_train$Event4D)

#Transformando a base de treinamento em corpus
corpus = VCorpus(VectorSource(base_train$Final.Narrative_clean)) 

#Document Term Matrix
dtm = DocumentTermMatrix(corpus) 
dtm = removeSparseTerms(dtm, 0.999) 

convert <- function(x) 
{
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}  

datanaive = apply(dtm, 2, convert)
dataset_train = as.data.frame(as.matrix(datanaive))    
dataset_train$Class = factor(base_train$Event4D)

#####Preparando a base de teste
#Carregando a base de teste
base_test <- read.csv2("base_incidentes_test3.csv",sep = ",",header = TRUE)
base_test <- base_test %>% select(Final.Narrative_clean,Event4D)
base_test$Event4D <- as.factor(base_test$Event4D)

#Transformando a base de teste em corpus
corpus = VCorpus(VectorSource(base_test$Final.Narrative_clean)) 

#Document Term Matrix
dtm_test = DocumentTermMatrix(corpus) 
dtm_test = removeSparseTerms(dtm_test, 0.999) 

convert <- function(x) 
{
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}  

datanaive_test = apply(dtm_test, 2, convert)
dataset_test = as.data.frame(as.matrix(datanaive_test))    
dataset_test$Class = as.factor(base_test$Event4D)

###### CLASSIFICADOR
#nivelando
complete_factor_levels <- c(dataset_train$Class %>% factor %>% levels, dataset_test$Class %>% factor %>% levels) %>% unique
dataset_train <- dataset_train %>%
  mutate(Class = factor(Class, levels = complete_factor_levels))
dataset_test <- dataset_test %>%
  mutate(Class = factor(Class, levels = complete_factor_levels))

#Classificador
classifier_nb <- naiveBayes(dataset_train, dataset_train$Class)
nb_pred = predict(classifier_nb, type = 'class', newdata =  dataset_test)
confusionMatrix(nb_pred,dataset_test$Class)

#Ajustando a matriz confusao para plotagem
confusion_graf <- data.frame(confusionMatrix(nb_pred,dataset_test$Class)$table)
confusion_soma_col <- confusion_graf %>% 
  group_by(Reference) %>% 
  summarise(soma_col = sum(Freq))
confusion_graf <- confusion_graf %>%
  left_join(confusion_soma_col,join_by(Reference == Reference))
confusion_graf$perc <- confusion_graf$Freq/confusion_graf$soma_col

#Plotando a Matriz confusao
confusion_graf %>%
  ggplot(aes(Prediction,Reference, fill = perc))+
  geom_tile()+
  scale_fill_gradient2(low="lightblue",high = "darkblue",label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Categoria original",
       y = "Categoria prevista",
       fill = "% de ocorrencias")

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(nb_pred,dataset_test$Class)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#Construindo a tabela predito x referencia
base_predaux <- as.data.frame(nb_pred)
base_predaux$Event4D <- base_test$Event4D
base_predNB2 <- rbind(base_predNB2,base_predaux)

########## Event1D = 4 ##########
#Carregando a base de treinamento
base_train <- read.csv2("base_incidentes_train4.csv",sep = ",",header = TRUE)
base_train <- base_train %>% select(Final.Narrative_clean,Event4D)
base_train$Event4D <- as.factor(base_train$Event4D)

#Transformando a base de treinamento em corpus
corpus = VCorpus(VectorSource(base_train$Final.Narrative_clean)) 

#Document Term Matrix
dtm = DocumentTermMatrix(corpus) 
dtm = removeSparseTerms(dtm, 0.999) 

convert <- function(x) 
{
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}  

datanaive = apply(dtm, 2, convert)
dataset_train = as.data.frame(as.matrix(datanaive))    
dataset_train$Class = factor(base_train$Event4D)

#####Preparando a base de teste
#Carregando a base de teste
base_test <- read.csv2("base_incidentes_test4.csv",sep = ",",header = TRUE)
base_test <- base_test %>% select(Final.Narrative_clean,Event4D)
base_test$Event4D <- as.factor(base_test$Event4D)

#Transformando a base de teste em corpus
corpus = VCorpus(VectorSource(base_test$Final.Narrative_clean)) 

#Document Term Matrix
dtm_test = DocumentTermMatrix(corpus) 
dtm_test = removeSparseTerms(dtm_test, 0.999) 

convert <- function(x) 
{
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}  

datanaive_test = apply(dtm_test, 2, convert)
dataset_test = as.data.frame(as.matrix(datanaive_test))    
dataset_test$Class = as.factor(base_test$Event4D)

###### CLASSIFICADOR
#nivelando
complete_factor_levels <- c(dataset_train$Class %>% factor %>% levels, dataset_test$Class %>% factor %>% levels) %>% unique
dataset_train <- dataset_train %>%
  mutate(Class = factor(Class, levels = complete_factor_levels))
dataset_test <- dataset_test %>%
  mutate(Class = factor(Class, levels = complete_factor_levels))

#Classificador
classifier_nb <- naiveBayes(dataset_train, dataset_train$Class)
nb_pred = predict(classifier_nb, type = 'class', newdata =  dataset_test)
confusionMatrix(nb_pred,dataset_test$Class)

#Ajustando a matriz confusao para plotagem
confusion_graf <- data.frame(confusionMatrix(nb_pred,dataset_test$Class)$table)
confusion_soma_col <- confusion_graf %>% 
  group_by(Reference) %>% 
  summarise(soma_col = sum(Freq))
confusion_graf <- confusion_graf %>%
  left_join(confusion_soma_col,join_by(Reference == Reference))
confusion_graf$perc <- confusion_graf$Freq/confusion_graf$soma_col

#Plotando a Matriz confusao
confusion_graf %>%
  ggplot(aes(Prediction,Reference, fill = perc))+
  geom_tile()+
  scale_fill_gradient2(low="lightblue",high = "darkblue",label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Categoria original",
       y = "Categoria prevista",
       fill = "% de ocorrencias")

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(nb_pred,dataset_test$Class)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#Construindo a tabela predito x referencia
base_predaux <- as.data.frame(nb_pred)
base_predaux$Event4D <- base_test$Event4D
base_predNB2 <- rbind(base_predNB2,base_predaux)

########## Event1D = 5 ##########
#Carregando a base de treinamento
base_train <- read.csv2("base_incidentes_train5.csv",sep = ",",header = TRUE)
base_train <- base_train %>% select(Final.Narrative_clean,Event4D)
base_train$Event4D <- as.factor(base_train$Event4D)

#Transformando a base de treinamento em corpus
corpus = VCorpus(VectorSource(base_train$Final.Narrative_clean)) 

#Document Term Matrix
dtm = DocumentTermMatrix(corpus) 
dtm = removeSparseTerms(dtm, 0.999) 

convert <- function(x) 
{
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}  

datanaive = apply(dtm, 2, convert)
dataset_train = as.data.frame(as.matrix(datanaive))    
dataset_train$Class = factor(base_train$Event4D)

#####Preparando a base de teste
#Carregando a base de teste
base_test <- read.csv2("base_incidentes_test5.csv",sep = ",",header = TRUE)
base_test <- base_test %>% select(Final.Narrative_clean,Event4D)
base_test$Event4D <- as.factor(base_test$Event4D)

#Transformando a base de teste em corpus
corpus = VCorpus(VectorSource(base_test$Final.Narrative_clean)) 

#Document Term Matrix
dtm_test = DocumentTermMatrix(corpus) 
dtm_test = removeSparseTerms(dtm_test, 0.999) 

convert <- function(x) 
{
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}  

datanaive_test = apply(dtm_test, 2, convert)
dataset_test = as.data.frame(as.matrix(datanaive_test))    
dataset_test$Class = as.factor(base_test$Event4D)

###### CLASSIFICADOR
#nivelando
complete_factor_levels <- c(dataset_train$Class %>% factor %>% levels, dataset_test$Class %>% factor %>% levels) %>% unique
dataset_train <- dataset_train %>%
  mutate(Class = factor(Class, levels = complete_factor_levels))
dataset_test <- dataset_test %>%
  mutate(Class = factor(Class, levels = complete_factor_levels))

#Classificador
classifier_nb <- naiveBayes(dataset_train, dataset_train$Class)
nb_pred = predict(classifier_nb, type = 'class', newdata =  dataset_test)
confusionMatrix(nb_pred,dataset_test$Class)

#Ajustando a matriz confusao para plotagem
confusion_graf <- data.frame(confusionMatrix(nb_pred,dataset_test$Class)$table)
confusion_soma_col <- confusion_graf %>% 
  group_by(Reference) %>% 
  summarise(soma_col = sum(Freq))
confusion_graf <- confusion_graf %>%
  left_join(confusion_soma_col,join_by(Reference == Reference))
confusion_graf$perc <- confusion_graf$Freq/confusion_graf$soma_col

#Plotando a Matriz confusao
confusion_graf %>%
  ggplot(aes(Prediction,Reference, fill = perc))+
  geom_tile()+
  scale_fill_gradient2(low="lightblue",high = "darkblue",label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Categoria original",
       y = "Categoria prevista",
       fill = "% de ocorrencias")

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(nb_pred,dataset_test$Class)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#Construindo a tabela predito x referencia
base_predaux <- as.data.frame(nb_pred)
base_predaux$Event4D <- base_test$Event4D
base_predNB2 <- rbind(base_predNB2,base_predaux)

########## Event1D = 6 ##########
#Carregando a base de treinamento
base_train <- read.csv2("base_incidentes_train6.csv",sep = ",",header = TRUE)
base_train <- base_train %>% select(Final.Narrative_clean,Event4D)
base_train$Event4D <- as.factor(base_train$Event4D)

#Transformando a base de treinamento em corpus
corpus = VCorpus(VectorSource(base_train$Final.Narrative_clean)) 

#Document Term Matrix
dtm = DocumentTermMatrix(corpus) 
dtm = removeSparseTerms(dtm, 0.999) 

convert <- function(x) 
{
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}  

datanaive = apply(dtm, 2, convert)
dataset_train = as.data.frame(as.matrix(datanaive))    
dataset_train$Class = factor(base_train$Event4D)

#####Preparando a base de teste
#Carregando a base de teste
base_test <- read.csv2("base_incidentes_test6.csv",sep = ",",header = TRUE)
base_test <- base_test %>% select(Final.Narrative_clean,Event4D)
base_test$Event4D <- as.factor(base_test$Event4D)

#Transformando a base de teste em corpus
corpus = VCorpus(VectorSource(base_test$Final.Narrative_clean)) 

#Document Term Matrix
dtm_test = DocumentTermMatrix(corpus) 
dtm_test = removeSparseTerms(dtm_test, 0.999) 

convert <- function(x) 
{
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}  

datanaive_test = apply(dtm_test, 2, convert)
dataset_test = as.data.frame(as.matrix(datanaive_test))    
dataset_test$Class = as.factor(base_test$Event4D)

###### CLASSIFICADOR
#nivelando
complete_factor_levels <- c(dataset_train$Class %>% factor %>% levels, dataset_test$Class %>% factor %>% levels) %>% unique
dataset_train <- dataset_train %>%
  mutate(Class = factor(Class, levels = complete_factor_levels))
dataset_test <- dataset_test %>%
  mutate(Class = factor(Class, levels = complete_factor_levels))

#Classificador
classifier_nb <- naiveBayes(dataset_train, dataset_train$Class)
nb_pred = predict(classifier_nb, type = 'class', newdata =  dataset_test)
confusionMatrix(nb_pred,dataset_test$Class)

#Ajustando a matriz confusao para plotagem
confusion_graf <- data.frame(confusionMatrix(nb_pred,dataset_test$Class)$table)
confusion_soma_col <- confusion_graf %>% 
  group_by(Reference) %>% 
  summarise(soma_col = sum(Freq))
confusion_graf <- confusion_graf %>%
  left_join(confusion_soma_col,join_by(Reference == Reference))
confusion_graf$perc <- confusion_graf$Freq/confusion_graf$soma_col

#Plotando a Matriz confusao
confusion_graf %>%
  ggplot(aes(Prediction,Reference, fill = perc))+
  geom_tile()+
  scale_fill_gradient2(low="lightblue",high = "darkblue",label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Categoria original",
       y = "Categoria prevista",
       fill = "% de ocorrencias")

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(nb_pred,dataset_test$Class)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#Construindo a tabela predito x referencia
base_predaux <- as.data.frame(nb_pred)
base_predaux$Event4D <- base_test$Event4D
base_predNB2 <- rbind(base_predNB2,base_predaux)

########## Event1D = 7 ##########
#Carregando a base de treinamento
base_train <- read.csv2("base_incidentes_train7.csv",sep = ",",header = TRUE)
base_train <- base_train %>% select(Final.Narrative_clean,Event4D)
base_train$Event4D <- as.factor(base_train$Event4D)

#Transformando a base de treinamento em corpus
corpus = VCorpus(VectorSource(base_train$Final.Narrative_clean)) 

#Document Term Matrix
dtm = DocumentTermMatrix(corpus) 
dtm = removeSparseTerms(dtm, 0.999) 

convert <- function(x) 
{
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}  

datanaive = apply(dtm, 2, convert)
dataset_train = as.data.frame(as.matrix(datanaive))    
dataset_train$Class = factor(base_train$Event4D)

#####Preparando a base de teste
#Carregando a base de teste
base_test <- read.csv2("base_incidentes_test7.csv",sep = ",",header = TRUE)
base_test <- base_test %>% select(Final.Narrative_clean,Event4D)
base_test$Event4D <- as.factor(base_test$Event4D)

#Transformando a base de teste em corpus
corpus = VCorpus(VectorSource(base_test$Final.Narrative_clean)) 

#Document Term Matrix
dtm_test = DocumentTermMatrix(corpus) 
dtm_test = removeSparseTerms(dtm_test, 0.999) 

convert <- function(x) 
{
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}  

datanaive_test = apply(dtm_test, 2, convert)
dataset_test = as.data.frame(as.matrix(datanaive_test))    
dataset_test$Class = as.factor(base_test$Event4D)

###### CLASSIFICADOR
#nivelando
complete_factor_levels <- c(dataset_train$Class %>% factor %>% levels, dataset_test$Class %>% factor %>% levels) %>% unique
dataset_train <- dataset_train %>%
  mutate(Class = factor(Class, levels = complete_factor_levels))
dataset_test <- dataset_test %>%
  mutate(Class = factor(Class, levels = complete_factor_levels))

#Classificador
classifier_nb <- naiveBayes(dataset_train, dataset_train$Class)
nb_pred = predict(classifier_nb, type = 'class', newdata =  dataset_test)
confusionMatrix(nb_pred,dataset_test$Class)

#Ajustando a matriz confusao para plotagem
confusion_graf <- data.frame(confusionMatrix(nb_pred,dataset_test$Class)$table)
confusion_soma_col <- confusion_graf %>% 
  group_by(Reference) %>% 
  summarise(soma_col = sum(Freq))
confusion_graf <- confusion_graf %>%
  left_join(confusion_soma_col,join_by(Reference == Reference))
confusion_graf$perc <- confusion_graf$Freq/confusion_graf$soma_col

#Plotando a Matriz confusao
confusion_graf %>%
  ggplot(aes(Prediction,Reference, fill = perc))+
  geom_tile()+
  scale_fill_gradient2(low="lightblue",high = "darkblue",label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Categoria original",
       y = "Categoria prevista",
       fill = "% de ocorrencias")

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(nb_pred,dataset_test$Class)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#Construindo a tabela predito x referencia
base_predaux <- as.data.frame(nb_pred)
base_predaux$Event4D <- base_test$Event4D
base_predNBN4 <- rbind(base_predNB2,base_predaux)
write.csv(base_predNBN4, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_predNBN4.csv", row.names=TRUE)

##### Event1D = 9 #####
base_test <- read.csv2("base_incidentes_test9.csv",sep = ",",header = TRUE)
base_test <- base_test %>% select(Final.Narrative_clean,Event4D)
base_test$nb_pred <- 9999
base_test <- base_test %>% select(nb_pred, Event4D)
base_predNBN4 <- rbind(base_predNBN4,base_test)
write.csv(base_predNBN4, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_predNBN4.csv", row.names=TRUE)

##### FINAL #####

#Matriz Confusão
base_predNBN4 <- read.csv2("base_predNBN4.csv",sep = ",",header = TRUE)
base_predNBN4$nb_pred <- as.factor(base_predNBN4$nb_pred)
base_predNBN4$Event4D <- as.factor(base_predNBN4$Event4D)
complete_factor_levels <- c(base_predNBN4$nb_pred %>% factor %>% levels, base_predNBN4$Event4D %>% factor %>% levels) %>% unique
base_predNBN4 <- base_predNBN4 %>% mutate(nb_pred = factor(nb_pred, levels = complete_factor_levels))
base_predNBN4 <- base_predNBN4 %>% mutate(Event4D = factor(Event4D, levels = complete_factor_levels))
confusionMatrix(base_predNBN4$nb_pred,base_predNBN4$Event4D)

#Ajustando a matriz confusao para plotagem
confusion_graf <- data.frame(confusionMatrix(base_predNBN4$nb_pred,base_predNBN4$Event4D)$table)
confusion_soma_col <- confusion_graf %>% 
  group_by(Reference) %>% 
  summarise(soma_col = sum(Freq))
confusion_graf <- confusion_graf %>%
  left_join(confusion_soma_col,join_by(Reference == Reference))
confusion_graf$perc <- confusion_graf$Freq/confusion_graf$soma_col
confusion_graf <- confusion_graf[order(confusion_graf$Reference),]

#Plotando a Matriz confusao
confusion_graf %>%
  ggplot(aes(Prediction,Reference, fill = perc))+
  geom_tile()+
  scale_fill_gradient2(low="lightblue",high = "darkblue",label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Categoria original",
       y = "Categoria prevista",
       fill = "% de ocorrencias")

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(base_predNBN4$nb_pred,base_predNBN4$Event4D)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

########## Acurácia por categoria N1 ref
base_predNBN4$Event1D <- substr(base_predNBN4$Event4D,1,1)

# Event1D = 1
#Matriz Confusão
base_predNBN4_aux <- filter(base_predNBN4,Event1D == 1)
base_predNBN4_aux$nb_pred <- as.factor(base_predNBN4_aux$nb_pred)
base_predNBN4_aux$Event4D <- as.factor(base_predNBN4_aux$Event4D)
complete_factor_levels <- c(base_predNBN4_aux$nb_pred %>% factor %>% levels, base_predNBN4_aux$Event4D %>% factor %>% levels) %>% unique
base_predNBN4_aux <- base_predNBN4_aux %>% mutate(nb_pred = factor(nb_pred, levels = complete_factor_levels))
base_predNBN4_aux <- base_predNBN4_aux %>% mutate(Event4D = factor(Event4D, levels = complete_factor_levels))
confusionMatrix(base_predNBN4_aux$nb_pred,base_predNBN4_aux$Event4D)

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(base_predNBN4_aux$nb_pred,base_predNBN4_aux$Event4D)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

# Event1D = 2
#Matriz Confusão
base_predNBN4_aux <- filter(base_predNBN4,Event1D == 2)
base_predNBN4_aux$nb_pred <- as.factor(base_predNBN4_aux$nb_pred)
base_predNBN4_aux$Event4D <- as.factor(base_predNBN4_aux$Event4D)
complete_factor_levels <- c(base_predNBN4_aux$nb_pred %>% factor %>% levels, base_predNBN4_aux$Event4D %>% factor %>% levels) %>% unique
base_predNBN4_aux <- base_predNBN4_aux %>% mutate(nb_pred = factor(nb_pred, levels = complete_factor_levels))
base_predNBN4_aux <- base_predNBN4_aux %>% mutate(Event4D = factor(Event4D, levels = complete_factor_levels))
confusionMatrix(base_predNBN4_aux$nb_pred,base_predNBN4_aux$Event4D)

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(base_predNBN4_aux$nb_pred,base_predNBN4_aux$Event4D)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

# Event1D = 3
#Matriz Confusão
base_predNBN4_aux <- filter(base_predNBN4,Event1D == 3)
base_predNBN4_aux$nb_pred <- as.factor(base_predNBN4_aux$nb_pred)
base_predNBN4_aux$Event4D <- as.factor(base_predNBN4_aux$Event4D)
complete_factor_levels <- c(base_predNBN4_aux$nb_pred %>% factor %>% levels, base_predNBN4_aux$Event4D %>% factor %>% levels) %>% unique
base_predNBN4_aux <- base_predNBN4_aux %>% mutate(nb_pred = factor(nb_pred, levels = complete_factor_levels))
base_predNBN4_aux <- base_predNBN4_aux %>% mutate(Event4D = factor(Event4D, levels = complete_factor_levels))
confusionMatrix(base_predNBN4_aux$nb_pred,base_predNBN4_aux$Event4D)

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(base_predNBN4_aux$nb_pred,base_predNBN4_aux$Event4D)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

# Event1D = 4
#Matriz Confusão
base_predNBN4_aux <- filter(base_predNBN4,Event1D == 4)
base_predNBN4_aux$nb_pred <- as.factor(base_predNBN4_aux$nb_pred)
base_predNBN4_aux$Event4D <- as.factor(base_predNBN4_aux$Event4D)
complete_factor_levels <- c(base_predNBN4_aux$nb_pred %>% factor %>% levels, base_predNBN4_aux$Event4D %>% factor %>% levels) %>% unique
base_predNBN4_aux <- base_predNBN4_aux %>% mutate(nb_pred = factor(nb_pred, levels = complete_factor_levels))
base_predNBN4_aux <- base_predNBN4_aux %>% mutate(Event4D = factor(Event4D, levels = complete_factor_levels))
confusionMatrix(base_predNBN4_aux$nb_pred,base_predNBN4_aux$Event4D)

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(base_predNBN4_aux$nb_pred,base_predNBN4_aux$Event4D)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

# Event1D = 5
#Matriz Confusão
base_predNBN4_aux <- filter(base_predNBN4,Event1D == 5)
base_predNBN4_aux$nb_pred <- as.factor(base_predNBN4_aux$nb_pred)
base_predNBN4_aux$Event4D <- as.factor(base_predNBN4_aux$Event4D)
complete_factor_levels <- c(base_predNBN4_aux$nb_pred %>% factor %>% levels, base_predNBN4_aux$Event4D %>% factor %>% levels) %>% unique
base_predNBN4_aux <- base_predNBN4_aux %>% mutate(nb_pred = factor(nb_pred, levels = complete_factor_levels))
base_predNBN4_aux <- base_predNBN4_aux %>% mutate(Event4D = factor(Event4D, levels = complete_factor_levels))
confusionMatrix(base_predNBN4_aux$nb_pred,base_predNBN4_aux$Event4D)

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(base_predNBN4_aux$nb_pred,base_predNBN4_aux$Event4D)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

# Event1D = 6
#Matriz Confusão
base_predNBN4_aux <- filter(base_predNBN4,Event1D == 6)
base_predNBN4_aux$nb_pred <- as.factor(base_predNBN4_aux$nb_pred)
base_predNBN4_aux$Event4D <- as.factor(base_predNBN4_aux$Event4D)
complete_factor_levels <- c(base_predNBN4_aux$nb_pred %>% factor %>% levels, base_predNBN4_aux$Event4D %>% factor %>% levels) %>% unique
base_predNBN4_aux <- base_predNBN4_aux %>% mutate(nb_pred = factor(nb_pred, levels = complete_factor_levels))
base_predNBN4_aux <- base_predNBN4_aux %>% mutate(Event4D = factor(Event4D, levels = complete_factor_levels))
confusionMatrix(base_predNBN4_aux$nb_pred,base_predNBN4_aux$Event4D)

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(base_predNBN4_aux$nb_pred,base_predNBN4_aux$Event4D)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

# Event1D = 7
#Matriz Confusão
base_predNBN4_aux <- filter(base_predNBN4,Event1D == 7)
base_predNBN4_aux$nb_pred <- as.factor(base_predNBN4_aux$nb_pred)
base_predNBN4_aux$Event4D <- as.factor(base_predNBN4_aux$Event4D)
complete_factor_levels <- c(base_predNBN4_aux$nb_pred %>% factor %>% levels, base_predNBN4_aux$Event4D %>% factor %>% levels) %>% unique
base_predNBN4_aux <- base_predNBN4_aux %>% mutate(nb_pred = factor(nb_pred, levels = complete_factor_levels))
base_predNBN4_aux <- base_predNBN4_aux %>% mutate(Event4D = factor(Event4D, levels = complete_factor_levels))
confusionMatrix(base_predNBN4_aux$nb_pred,base_predNBN4_aux$Event4D)

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(base_predNBN4_aux$nb_pred,base_predNBN4_aux$Event4D)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

# Event1D = 9
#Matriz Confusão
base_predNBN4_aux <- filter(base_predNBN4,Event1D == 9)
base_predNBN4_aux$nb_pred <- as.factor(base_predNBN4_aux$nb_pred)
base_predNBN4_aux$Event4D <- as.factor(base_predNBN4_aux$Event4D)
complete_factor_levels <- c(base_predNBN4_aux$nb_pred %>% factor %>% levels, base_predNBN4_aux$Event4D %>% factor %>% levels) %>% unique
base_predNBN4_aux <- base_predNBN4_aux %>% mutate(nb_pred = factor(nb_pred, levels = complete_factor_levels))
base_predNBN4_aux <- base_predNBN4_aux %>% mutate(Event4D = factor(Event4D, levels = complete_factor_levels))
confusionMatrix(base_predNBN4_aux$nb_pred,base_predNBN4_aux$Event4D)

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(base_predNBN4_aux$nb_pred,base_predNBN4_aux$Event4D)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)