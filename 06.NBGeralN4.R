#Naive Bayes 4D - Geral

#####Preparando a base de treinamento geral

#Carregando a base de treinamento
base_train <- read.csv2("base_incidentes_train.csv",sep = ",",header = TRUE)
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
write.csv(dataset_train, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\dataset_train4D.csv", row.names=TRUE)

rm(list = ls())
gc()

#####Preparando a base de teste
#Carregando a base de teste
base_test <- read.csv2("base_incidentes_test.csv",sep = ",",header = TRUE)
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
write.csv(dataset_test, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\dataset_test4D.csv", row.names=TRUE)

rm(list = ls())
gc()

###### CLASSIFICADOR
#Importando as bases
dataset_test <- read.csv2("dataset_test4D.csv",sep = ",",header = TRUE)
dataset_test$Class = as.factor(dataset_test$Class)
dataset_train <- read.csv2("dataset_train4D.csv",sep = ",",header = TRUE)
dataset_train$Class = as.factor(dataset_train$Class)

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

#construido a matrix pred x ref
base_predNBgeralN4 <- as.data.frame(nb_pred)
base_test <- read.csv2("base_incidentes_test.csv",sep = ",",header = TRUE)
base_test <- base_test %>% select(Final.Narrative_clean,Event4D)
base_predNBgeralN4$Event4D <- base_test$Event4D
base_predNBgeralN4$nb_pred <- as.factor(base_predNBgeralN4$nb_pred)
base_predNBgeralN4$Event4D <- as.factor(base_predNBgeralN4$Event4D)
complete_factor_levels <- c(base_predNBgeralN4$nb_pred %>% factor %>% levels, base_predNBgeralN4$Event4D %>% factor %>% levels) %>% unique
base_predNBgeralN4 <- base_predNBgeralN4 %>% mutate(nb_pred = factor(nb_pred, levels = complete_factor_levels))
base_predNBgeralN4 <- base_predNBgeralN4 %>% mutate(Event4D = factor(Event4D, levels = complete_factor_levels))

########## Acurácia por categoria N1 ref
base_predNBgeralN4$Event1D <- substr(base_predNBgeralN4$Event4D,1,1)

# Event1D = 1
#Matriz Confusão
base_predNBgeralN4_aux <- filter(base_predNBgeralN4,Event1D == 1)
base_predNBgeralN4_aux$nb_pred <- as.factor(base_predNBgeralN4_aux$nb_pred)
base_predNBgeralN4_aux$Event4D <- as.factor(base_predNBgeralN4_aux$Event4D)
complete_factor_levels <- c(base_predNBgeralN4_aux$nb_pred %>% factor %>% levels, base_predNBgeralN4_aux$Event4D %>% factor %>% levels) %>% unique
base_predNBgeralN4_aux <- base_predNBgeralN4_aux %>% mutate(nb_pred = factor(nb_pred, levels = complete_factor_levels))
base_predNBgeralN4_aux <- base_predNBgeralN4_aux %>% mutate(Event4D = factor(Event4D, levels = complete_factor_levels))
confusionMatrix(base_predNBgeralN4_aux$nb_pred,base_predNBgeralN4_aux$Event4D)

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(base_predNBgeralN4_aux$nb_pred,base_predNBgeralN4_aux$Event4D)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

# Event1D = 2
#Matriz Confusão
base_predNBgeralN4_aux <- filter(base_predNBgeralN4,Event1D == 2)
base_predNBgeralN4_aux$nb_pred <- as.factor(base_predNBgeralN4_aux$nb_pred)
base_predNBgeralN4_aux$Event4D <- as.factor(base_predNBgeralN4_aux$Event4D)
complete_factor_levels <- c(base_predNBgeralN4_aux$nb_pred %>% factor %>% levels, base_predNBgeralN4_aux$Event4D %>% factor %>% levels) %>% unique
base_predNBgeralN4_aux <- base_predNBgeralN4_aux %>% mutate(nb_pred = factor(nb_pred, levels = complete_factor_levels))
base_predNBgeralN4_aux <- base_predNBgeralN4_aux %>% mutate(Event4D = factor(Event4D, levels = complete_factor_levels))
confusionMatrix(base_predNBgeralN4_aux$nb_pred,base_predNBgeralN4_aux$Event4D)

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(base_predNBgeralN4_aux$nb_pred,base_predNBgeralN4_aux$Event4D)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

# Event1D = 3
#Matriz Confusão
base_predNBgeralN4_aux <- filter(base_predNBgeralN4,Event1D == 3)
base_predNBgeralN4_aux$nb_pred <- as.factor(base_predNBgeralN4_aux$nb_pred)
base_predNBgeralN4_aux$Event4D <- as.factor(base_predNBgeralN4_aux$Event4D)
complete_factor_levels <- c(base_predNBgeralN4_aux$nb_pred %>% factor %>% levels, base_predNBgeralN4_aux$Event4D %>% factor %>% levels) %>% unique
base_predNBgeralN4_aux <- base_predNBgeralN4_aux %>% mutate(nb_pred = factor(nb_pred, levels = complete_factor_levels))
base_predNBgeralN4_aux <- base_predNBgeralN4_aux %>% mutate(Event4D = factor(Event4D, levels = complete_factor_levels))
confusionMatrix(base_predNBgeralN4_aux$nb_pred,base_predNBgeralN4_aux$Event4D)

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(base_predNBgeralN4_aux$nb_pred,base_predNBgeralN4_aux$Event4D)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

# Event1D = 4
#Matriz Confusão
base_predNBgeralN4_aux <- filter(base_predNBgeralN4,Event1D == 4)
base_predNBgeralN4_aux$nb_pred <- as.factor(base_predNBgeralN4_aux$nb_pred)
base_predNBgeralN4_aux$Event4D <- as.factor(base_predNBgeralN4_aux$Event4D)
complete_factor_levels <- c(base_predNBgeralN4_aux$nb_pred %>% factor %>% levels, base_predNBgeralN4_aux$Event4D %>% factor %>% levels) %>% unique
base_predNBgeralN4_aux <- base_predNBgeralN4_aux %>% mutate(nb_pred = factor(nb_pred, levels = complete_factor_levels))
base_predNBgeralN4_aux <- base_predNBgeralN4_aux %>% mutate(Event4D = factor(Event4D, levels = complete_factor_levels))
confusionMatrix(base_predNBgeralN4_aux$nb_pred,base_predNBgeralN4_aux$Event4D)

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(base_predNBgeralN4_aux$nb_pred,base_predNBgeralN4_aux$Event4D)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

# Event1D = 5
#Matriz Confusão
base_predNBgeralN4_aux <- filter(base_predNBgeralN4,Event1D == 5)
base_predNBgeralN4_aux$nb_pred <- as.factor(base_predNBgeralN4_aux$nb_pred)
base_predNBgeralN4_aux$Event4D <- as.factor(base_predNBgeralN4_aux$Event4D)
complete_factor_levels <- c(base_predNBgeralN4_aux$nb_pred %>% factor %>% levels, base_predNBgeralN4_aux$Event4D %>% factor %>% levels) %>% unique
base_predNBgeralN4_aux <- base_predNBgeralN4_aux %>% mutate(nb_pred = factor(nb_pred, levels = complete_factor_levels))
base_predNBgeralN4_aux <- base_predNBgeralN4_aux %>% mutate(Event4D = factor(Event4D, levels = complete_factor_levels))
confusionMatrix(base_predNBgeralN4_aux$nb_pred,base_predNBgeralN4_aux$Event4D)

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(base_predNBgeralN4_aux$nb_pred,base_predNBgeralN4_aux$Event4D)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

# Event1D = 6
#Matriz Confusão
base_predNBgeralN4_aux <- filter(base_predNBgeralN4,Event1D == 6)
base_predNBgeralN4_aux$nb_pred <- as.factor(base_predNBgeralN4_aux$nb_pred)
base_predNBgeralN4_aux$Event4D <- as.factor(base_predNBgeralN4_aux$Event4D)
complete_factor_levels <- c(base_predNBgeralN4_aux$nb_pred %>% factor %>% levels, base_predNBgeralN4_aux$Event4D %>% factor %>% levels) %>% unique
base_predNBgeralN4_aux <- base_predNBgeralN4_aux %>% mutate(nb_pred = factor(nb_pred, levels = complete_factor_levels))
base_predNBgeralN4_aux <- base_predNBgeralN4_aux %>% mutate(Event4D = factor(Event4D, levels = complete_factor_levels))
confusionMatrix(base_predNBgeralN4_aux$nb_pred,base_predNBgeralN4_aux$Event4D)

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(base_predNBgeralN4_aux$nb_pred,base_predNBgeralN4_aux$Event4D)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

# Event1D = 7
#Matriz Confusão
base_predNBgeralN4_aux <- filter(base_predNBgeralN4,Event1D == 7)
base_predNBgeralN4_aux$nb_pred <- as.factor(base_predNBgeralN4_aux$nb_pred)
base_predNBgeralN4_aux$Event4D <- as.factor(base_predNBgeralN4_aux$Event4D)
complete_factor_levels <- c(base_predNBgeralN4_aux$nb_pred %>% factor %>% levels, base_predNBgeralN4_aux$Event4D %>% factor %>% levels) %>% unique
base_predNBgeralN4_aux <- base_predNBgeralN4_aux %>% mutate(nb_pred = factor(nb_pred, levels = complete_factor_levels))
base_predNBgeralN4_aux <- base_predNBgeralN4_aux %>% mutate(Event4D = factor(Event4D, levels = complete_factor_levels))
confusionMatrix(base_predNBgeralN4_aux$nb_pred,base_predNBgeralN4_aux$Event4D)

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(base_predNBgeralN4_aux$nb_pred,base_predNBgeralN4_aux$Event4D)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

# Event1D = 9
#Matriz Confusão
base_predNBgeralN4_aux <- filter(base_predNBgeralN4,Event1D == 9)
base_predNBgeralN4_aux$nb_pred <- as.factor(base_predNBgeralN4_aux$nb_pred)
base_predNBgeralN4_aux$Event4D <- as.factor(base_predNBgeralN4_aux$Event4D)
complete_factor_levels <- c(base_predNBgeralN4_aux$nb_pred %>% factor %>% levels, base_predNBgeralN4_aux$Event4D %>% factor %>% levels) %>% unique
base_predNBgeralN4_aux <- base_predNBgeralN4_aux %>% mutate(nb_pred = factor(nb_pred, levels = complete_factor_levels))
base_predNBgeralN4_aux <- base_predNBgeralN4_aux %>% mutate(Event4D = factor(Event4D, levels = complete_factor_levels))
confusionMatrix(base_predNBgeralN4_aux$nb_pred,base_predNBgeralN4_aux$Event4D)

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(base_predNBgeralN4_aux$nb_pred,base_predNBgeralN4_aux$Event4D)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#Acurácia geral N1
base_predNBgeralN4$nb_pred1D <- substr(base_predNBgeralN4$nb_pred,1,1)
complete_factor_levels <- c(base_predNBgeralN4$nb_pred1D %>% factor %>% levels, base_predNBgeralN4$Event1D %>% factor %>% levels) %>% unique
base_predNBgeralN4 <- base_predNBgeralN4 %>% mutate(nb_pred1D = factor(nb_pred1D, levels = complete_factor_levels))
base_predNBgeralN4 <- base_predNBgeralN4 %>% mutate(Event1D = factor(Event1D, levels = complete_factor_levels))
confusionMatrix(base_predNBgeralN4$nb_pred1D,base_predNBgeralN4$Event1D)

#Apresentando as estatisticas por categoria
confusion_classes <- data.frame(confusionMatrix(base_predNBgeralN4$nb_pred1D,base_predNBgeralN4$Event1D)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)
