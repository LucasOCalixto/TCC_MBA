# Preparacao dos dados #

#Carregando a base de dados como data frame
base_incidentes <- read.csv2("January2015toFebruary2023.csv",sep = ",",header = TRUE)

#Selecionando as colunas pertinentes
base_incidentes <- base_incidentes %>% select(Final.Narrative,Event)
#Criando as colunas de Event
base_incidentes <- rename(base_incidentes,Event4D = Event)
base_incidentes$Event3D <- substr(base_incidentes$Event4D,1,3)
base_incidentes$Event2D <- substr(base_incidentes$Event4D,1,2)
base_incidentes$Event1D <- substr(base_incidentes$Event4D,1,1)

#Salvar novas bases
base_incidentes <- mutate_if(base_incidentes,is.character, utf8::utf8_encode)
write.csv(base_incidentes, "C:\\Users\\lucas\\Desktop\\MBA\\TCC\\10.R_tent10\\base_incidentes.csv", row.names=TRUE)

#Gráficos para análise descritiva
base_incidentes <- read.csv2("base_incidentes.csv",sep = ",",header = TRUE)

#Pareto por categoria 1D
#Agrupando pelas categorias de evento
base_incidentes_agrup <- base_incidentes %>%
  select(Event1D) %>%
  aggregate(by=list(base_incidentes$Event1D),FUN=length) %>%
  mutate(Group.1=reorder(Group.1,Event1D)) %>%
  arrange(desc(Event1D)) %>%
  rename(Ocorrencias=Event1D,Event1D=Group.1)

#Pareto das categorias de Event
Event1D <- base_incidentes_agrup$Ocorrencias
names(Event1D) <- base_incidentes_agrup$Event1D
pareto_cat <- pareto.chart(Event1D,cumperc = seq(0, 100, by = 20))

#Pareto por categoria 2D
#Agrupando pelas categorias de evento
base_incidentes_agrup <- base_incidentes %>%
  select(Event2D) %>%
  aggregate(by=list(base_incidentes$Event2D),FUN=length) %>%
  mutate(Group.1=reorder(Group.1,Event2D)) %>%
  arrange(desc(Event2D)) %>%
  rename(Ocorrencias=Event2D,Event2D=Group.1)

#Pareto das categorias de Event
Event2D <- base_incidentes_agrup$Ocorrencias
names(Event2D) <- base_incidentes_agrup$Event2D
pareto_cat <- pareto.chart(Event2D,cumperc = seq(0, 100, by = 20))

#Pareto por categoria 4D
#Agrupando pelas categorias de evento
base_incidentes_agrup <- base_incidentes %>%
  select(Event4D) %>%
  aggregate(by=list(base_incidentes$Event4D),FUN=length) %>%
  mutate(Group.1=reorder(Group.1,Event4D)) %>%
  arrange(desc(Event4D)) %>%
  rename(Ocorrencias=Event4D,Event4D=Group.1)

#Pareto das categorias de Event
Event4D <- base_incidentes_agrup$Ocorrencias
names(Event4D) <- base_incidentes_agrup$Event4D
pareto_cat <- pareto.chart(Event4D,cumperc = seq(0, 100, by = 20))

### Contagem de número de subcategorias N2 para cada N1
base_incidentes_agrup$Event1D <- substr(base_incidentes_agrup$Event2D,1,1)
#Agrupando pelas categorias de evento1D
base_incidentes_agrup2 <- base_incidentes_agrup %>%
  select(Event1D) %>%
  aggregate(by=list(base_incidentes_agrup$Event1D),FUN=length) %>%
  mutate(Group.1=reorder(Group.1,Event1D)) %>%
  arrange(desc(Event1D)) %>%
  rename(Event2D=Event1D,Event1D=Group.1)
