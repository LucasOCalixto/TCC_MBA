#Naive Bayes N1

#####Preparando a base de treinamento

#Carregando a base de treinamento
base_train <- read.csv2("base_incidentes_train.csv",sep = ",",header = TRUE)
base_train <- base_train %>% select(Final.Narrative_clean,Event1D)
base_train$Event1D <- as.factor(base_train$Event1D)
base_train <- mutate_if(base_train,is.character, utf8::utf8_encode)

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
dataset_train$Class = factor(base_train$Event1D)
write.csv(dataset_train, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\dataset_train.csv", row.names=TRUE)

#####Preparando a base de teste
#Carregando a base de teste
base_test <- read.csv2("base_incidentes_test.csv",sep = ",",header = TRUE)
base_test <- base_test %>% select(Final.Narrative_clean,Event1D)
base_test$Event1D <- as.factor(base_test$Event1D)
base_test <- mutate_if(base_test,is.character, utf8::utf8_encode)

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
dataset_test$Class = as.factor(base_test$Event1D)
write.csv(dataset_test, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\dataset_test.csv", row.names=TRUE)

rm(list = ls())
gc()

###### CLASSIFICADOR
#Importando as bases
dataset_test <- read.csv2("dataset_test.csv",sep = ",",header = TRUE)
dataset_test$Class = as.factor(dataset_test$Class)
dataset_test <- mutate_if(dataset_test,is.character, utf8::utf8_encode)
dataset_train <- read.csv2("dataset_train.csv",sep = ",",header = TRUE)
dataset_train$Class = as.factor(dataset_train$Class)
dataset_train <- mutate_if(dataset_train,is.character, utf8::utf8_encode)

#nivelando
complete_factor_levels <- c(dataset_train$Class %>% factor %>% levels, dataset_test$Class %>% factor %>% levels) %>% unique

dataset_train <- dataset_train %>%
  mutate(Class = factor(Class, levels = complete_factor_levels))

dataset_test <- dataset_test %>%
  mutate(Class = factor(Class, levels = complete_factor_levels))

#Classificador
classifier_nb <- naiveBayes(dataset_train, dataset_train$Class)
nb_predN1 = predict(classifier_nb, type = 'class', newdata =  dataset_test)
confusionMatrix(nb_predN1,dataset_test$Class)

#Ajustando a matriz confusao para plotagem
confusion_graf <- data.frame(confusionMatrix(nb_predN1,dataset_test$Class)$table)
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
confusion_classes <- data.frame(confusionMatrix(nb_predN1,dataset_test$Class)$byClass)

confusion_classes %>%
  arrange(Sensitivity) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#construindo a base com predito x event2D x final narrative
base_predNBN1 <- as.data.frame(nb_predN1)
base_test <- read.csv2("base_incidentes_test.csv",sep = ",",header = TRUE)
base_test <- mutate_if(base_test,is.character, utf8::utf8_encode)
base_predNBN1$Event4D <- base_test$Event4D
base_predNBN1$Event3D <- base_test$Event3D
base_predNBN1$Event2D <- base_test$Event2D
base_predNBN1$Event1D <- base_test$Event1D
base_predNBN1$Final.Narrative_clean <- base_test$Final.Narrative_clean
write.csv(base_predNBN1, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_predNB1.csv", row.names=TRUE)
#base_predNBN1 <- read.csv2("base_predNBN1.csv",sep = ",",header = TRUE)

#separando as bases de teste por 1D
base_test1 = filter(base_predNBN1,nb_predN1==1)
base_test2 = filter(base_predNBN1,nb_predN1==2)
base_test3 = filter(base_predNBN1,nb_predN1==3)
base_test4 = filter(base_predNBN1,nb_predN1==4)
base_test5 = filter(base_predNBN1,nb_predN1==5)
base_test6 = filter(base_predNBN1,nb_predN1==6)
base_test7 = filter(base_predNBN1,nb_predN1==7)
base_test9 = filter(base_predNBN1,nb_predN1==9)

#salvando as bases de teste
write.csv(base_test1, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_incidentes_test1.csv", row.names=TRUE)
write.csv(base_test2, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_incidentes_test2.csv", row.names=TRUE)
write.csv(base_test3, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_incidentes_test3.csv", row.names=TRUE)
write.csv(base_test4, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_incidentes_test4.csv", row.names=TRUE)
write.csv(base_test5, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_incidentes_test5.csv", row.names=TRUE)
write.csv(base_test6, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_incidentes_test6.csv", row.names=TRUE)
write.csv(base_test7, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_incidentes_test7.csv", row.names=TRUE)
write.csv(base_test9, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_incidentes_test9.csv", row.names=TRUE)
