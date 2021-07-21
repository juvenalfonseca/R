### remove variáveis
rm(list=ls())

### instala e carrega biblioteca
install.packages("this.path")
library(this.path, quietly=TRUE)

### define path a partir da pasta raiz do projeto
THIS_DIR <- gsub("/rotinas", '', this.dir())

### cria função para retornar path do source
pathSource = function(script='') {
  files <- list.files(THIS_DIR, full.names=TRUE, recursive=TRUE, all.files=TRUE,  pattern="(*.R$|.env$|.csv)")
  fs    <- data.frame(dirname = dirname(files), basename = basename(files), path = files)
  
  if (script != '') {
    fs    <- fs[fs$basename == script,]  
  }
  return(as.character(fs$path))
}

### carrega configurações necessárias
source(pathSource('config.R'))

### carrega datasets
df.periodo <- read.csv2(file = pathSource(enumDataSets$`campeonato-brasileiro-pontos-corridos-2003-2020-periodo.csv`), sep = ";", header = TRUE, stringsAsFactors = FALSE)
df.jogos   <- read.csv2(file = pathSource(enumDataSets$`campeonato-brasileiro-pontos-corridos-2003-2020-jogos.csv`  ), sep = ";", header = TRUE, stringsAsFactors = FALSE)

### padroniza caixa dos nomes das variáveis
names.periodo <- colnames(df.periodo)
names.jogos   <- colnames(df.jogos)

df.periodo <- df.periodo %>% rename_at(all_of(vars(names.periodo)), tolower)
df.jogos   <- df.jogos   %>% rename_at(all_of(vars(names.jogos  )), tolower)

### captalizar strings
df.jogos <-
  df.jogos %>%
  mutate(dia       = capwords(dia      , TRUE),
         mandante  = capwords(mandante , TRUE),
         visitante = capwords(visitante, TRUE),     
         vencedor  = capwords(vencedor , TRUE),              
         arena     = capwords(arena    , TRUE))

### altera campos de datas de character para date
df.periodo <- 
  df.periodo %>% 
  mutate(inicio = as.Date(inicio, format = "%d/%m/%Y"),
         fim    = as.Date(fim   , format = "%d/%m/%Y"))

df.jogos <- 
  df.jogos %>% 
  mutate(data = as.Date(data, format = "%d/%m/%Y") )
  
### junta os datasets e retorna apenas os registros corretos criados na junção (full join)
df <- merge(df.periodo, df.jogos) %>% filter(data >= inicio, data <= fim)

###### ANÁLISES ######
### GOLS POR EDIÇÃO
gols.edicao <-
  df %>%
  group_by(torneio) %>%
  summarise(gols_mandante   = sum(mandante.placar , na.rm = TRUE),
            gols_visitantes = sum(visitante.placar, na.rm = TRUE),
            gols_total      = gols_mandante + gols_visitantes  ) %>%
  ungroup() %>%
  mutate(gols_mandante_perc   = (gols_mandante/gols_total  )*100,
         gols_visitantes_perc = (gols_visitantes/gols_total)*100)
  
### Melhores ataques por edição
gols.clubes.mandantes <-
  df %>%
  rename(clube=mandante) %>%
  group_by(torneio, clube) %>%
  summarise(gols_mandante = sum(mandante.placar , na.rm = TRUE), .groups="keep")

gols.clubes.visitantes <-
  df %>%
  rename(clube=visitante) %>%
  group_by(torneio, clube) %>%
  summarise(gols_visitante = sum(visitante.placar , na.rm = TRUE), .groups="keep")  

gols.clubes <-
  inner_join(gols.clubes.mandantes, gols.clubes.visitantes, by=c("torneio","clube")) %>%
  mutate(gols_total = gols_mandante + gols_visitante )

gols.torneio.ataque <-
  gols.clubes %>%
  group_by(torneio) %>%
  summarise(ataque_pior   = min(gols_total),
            ataque_melhor = max(gols_total)) %>%
  ungroup()

gols.torneio.ataque.pior <-
  inner_join(gols.torneio.ataque, gols.clubes, by="torneio") %>%
  filter(ataque_pior == gols_total) %>%
  select(torneio, clube, ataque_pior)
  
gols.torneio.ataque.melhor <-
  inner_join(gols.torneio.ataque, gols.clubes, by="torneio") %>%
  filter(ataque_melhor == gols_total) %>%
  select(torneio, clube, ataque_melhor)

gols.ataques <- inner_join(gols.torneio.ataque.pior, gols.torneio.ataque.melhor, by="torneio", suffix=c("_pior","_melhor"))
  

#### RESULTADOS
# Gols por edição
gols.edicao

# Melhores e piores ataques
gols.ataques

# Maior pontuador de todas as edições
pontos.participantes <- 
  df %>% 
  select(torneio, mandante) %>%
  group_by(torneio) %>%
  summarise( times = n_distinct(mandante)) %>%
  ungroup() %>%
  mutate(pontos_max = (times - 1)*2*3)

pontos <-
  df %>%
  select(torneio, mandante, visitante, mandante.placar, visitante.placar) %>%
  mutate(pontos_mandante  = ifelse(mandante.placar  > visitante.placar, 3, ifelse(mandante.placar < visitante.placar, 0, 1)), 
         pontos_visitante = ifelse(visitante.placar > mandante.placar , 3, ifelse(visitante.placar < mandante.placar, 0, 1)))
  
pontos.mandantes <-
  pontos %>%
  group_by(torneio, mandante) %>%
  summarise(pontos_mandante = sum(pontos_mandante, na.rm=TRUE), .groups="keep") %>%
  rename(clube=mandante)
  
pontos.visitante <-
  pontos %>%
  group_by(torneio, visitante) %>%
  summarise(pontos_visitante = sum(pontos_visitante, na.rm=TRUE), .groups="keep" ) %>%
  rename(clube=visitante)

pontos.total <- 
  inner_join(pontos.mandantes, pontos.visitante, by=c("torneio","clube")) %>%
  mutate(pontos_total = pontos_mandante + pontos_visitante)

### maior pontuador de todas as edições
maior.pontuador <- pontos.total %>% filter(pontos_total == max(pontos.total$pontos_total))
### menor pontuador de todas as edições
menor.pontuador <- pontos.total %>% filter(pontos_total == min(pontos.total$pontos_total))

### 
campeoes <- 
  pontos.total %>%
  group_by(torneio) %>%
  summarise(maior_ponto = max(pontos_total)) %>%
  ungroup() %>%
  inner_join(pontos.total, by=c("torneio")) %>%
  filter( maior_ponto == pontos_total ) %>%
  select(-maior_ponto) %>%
  inner_join(pontos.participantes, by="torneio") %>%
  mutate(aproveitamento = (pontos_total/pontos_max)*100 ) %>%
  mutate(ano = gsub('[a-z|A-Z]','',torneio))

### Campeões ano a ano
campeoes %>% select(torneio, clube, pontos_mandante, pontos_visitante, pontos_total, aproveitamento)

### campeões com a maior e menor pontuação
campeoes.pontuacao <-
  campeoes %>%
  mutate(maior_ponto = max(pontos_total),
         menor_ponto = min(pontos_total)) %>%
  filter(pontos_total == maior_ponto | pontos_total == menor_ponto) %>%
  mutate( observacao = ifelse(pontos_total == maior_ponto, 'Campeão com a maior pontuação', 'Campeão com a menor pontuação')) %>%
  select(torneio, clube, pontos_total, observacao)

### campeões com o maior e menor aproveitamento
campeoes.aproveitamento <-
  campeoes %>%
  mutate(maior_aproveitamento = max(aproveitamento),
         menor_aproveitamento = min(aproveitamento)) %>%
  filter(aproveitamento == maior_aproveitamento | aproveitamento == menor_aproveitamento) %>%
  mutate( observacao = ifelse(aproveitamento == maior_aproveitamento, 'Campeão com o melhor aproveitamento', 'Campeão com o menor aproveitamento')) %>%
  select( torneio, clube, aproveitamento, observacao ) 

### gráfico de aproveitamento dos campeões
ggplot(campeoes, aes(x=ano, y=aproveitamento, fill=clube)) +
  geom_bar(position='dodge', stat='identity') +
  geom_text(aes(label=as.integer(aproveitamento)), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(title='Aproveitamento dos campeões do brasileirão de pontos corridos', x='Ano', y='Aproveitamento') +
  guides(colour = "none") +
  theme_light() +
  ggeasy::easy_center_title()



