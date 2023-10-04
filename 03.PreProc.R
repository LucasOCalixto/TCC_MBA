#Pre processamento#

#Carregar a base de dados
base_incidentes <- read.csv2("base_incidentes.csv",sep = ",",header = TRUE)
base_incidentes <- base_incidentes %>% select(Final.Narrative, Event4D, Event3D, Event2D, Event1D)
base_incidentes$Event1D <- as.factor(base_incidentes$Event1D)
base_incidentes$Event2D <- as.factor(base_incidentes$Event2D)
base_incidentes$Event3D <- as.factor(base_incidentes$Event3D)
base_incidentes$Event4D <- as.factor(base_incidentes$Event4D)
base_incidentes <- mutate_if(base_incidentes,is.character, utf8::utf8_encode)

#Limpando os dados
corpus = VCorpus(VectorSource(base_incidentes$Final.Narrative)) 
corpus = tm_map(corpus, content_transformer(tolower)) 
corpus = tm_map(corpus, removeNumbers) 
corpus = tm_map(corpus, removePunctuation) 
corpus = tm_map(corpus, removeWords, stopwords("english")) 
corpus = tm_map(corpus, stemDocument) 
corpus = tm_map(corpus, stripWhitespace)
df_corpus <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)
base_incidentes$Final.Narrative_clean <- df_corpus$text

#Tokenizando base limpa
tidy_incidentes_clean <- unnest_tokens(base_incidentes,tokens,Final.Narrative_clean,"words","text",to_lower=TRUE,drop=TRUE)
tidy_incidentes_clean <- tidy_incidentes_clean %>% select(Event4D, Event3D, Event2D, Event1D, tokens)

#dividido em base e treinamento
split = sample(2,nrow(base_incidentes),prob = c(0.5,0.5),replace = TRUE)
train_set = base_incidentes[split == 1,]
test_set = base_incidentes[split == 2,]


#criando as bases de treinamento por cada Event1D
train_set1 = train_set[train_set$Event1D == 1, ]
train_set2 = train_set[train_set$Event1D == 2, ]
train_set3 = train_set[train_set$Event1D == 3, ]
train_set4 = train_set[train_set$Event1D == 4, ]
train_set5 = train_set[train_set$Event1D == 5, ]
train_set6 = train_set[train_set$Event1D == 6, ]
train_set7 = train_set[train_set$Event1D == 7, ]

#Salvando a base stemmizada
write.csv(tidy_incidentes_clean, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_stem.csv", row.names=TRUE)
write.csv(base_incidentes, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_incidentes_clean.csv", row.names=TRUE)
write.csv(train_set, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_incidentes_train.csv", row.names=TRUE)
write.csv(test_set, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_incidentes_test.csv", row.names=TRUE)
write.csv(train_set1, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_incidentes_train1.csv", row.names=TRUE)
write.csv(train_set2, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_incidentes_train2.csv", row.names=TRUE)
write.csv(train_set3, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_incidentes_train3.csv", row.names=TRUE)
write.csv(train_set4, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_incidentes_train4.csv", row.names=TRUE)
write.csv(train_set5, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_incidentes_train5.csv", row.names=TRUE)
write.csv(train_set6, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_incidentes_train6.csv", row.names=TRUE)
write.csv(train_set7, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_incidentes_train7.csv", row.names=TRUE)
