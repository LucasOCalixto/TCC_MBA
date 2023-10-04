# Instalador de pacotes

#dplyr - filter, select, mutate, arrange, desc, renamen, left_join, sample_n, count, slice_max, bind_rows, group_by, ungroup, summarise, join_by
#knitr - kable
#kableExtra - kables_tyling
#caret - confusionMatrix
#e1071 - naiveBayes
#ggplot2 - ggplot, aes, geom_col, labs, facet_wrap, geom_tile, scale_fill_gradient2, theme_minimal, theme, element_text, element_blank
#qcc - pareto.chart
#scales - percent_format
#stringr - str_sub
#tidytext - unnest_tokens, bind_tf_idf
#tm - Vcorpus, VectorSource, tm_map, content_transformer, stopwords, DocumentTermMatrix, removeSparseTerms, inspect
#wordcloud2 - wordcloud2


#Instalar pacotes

pacotes <- c("tidytext","ggplot2","dplyr",
             "stringr","tm","e1071","caret","wordcloud2",
             "qcc","knitr","kableExtra","scales","scutr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
