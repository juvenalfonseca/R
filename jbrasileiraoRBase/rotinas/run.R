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

names(df.periodo) <- tolower(names.periodo)
names(df.jogos)   <- tolower(names.jogos)

### altera campos de datas de character para date
df.periodo['inicio'] <- as.Date(df.periodo$inicio, format = "%d/%m/%Y")
df.periodo['fim'   ] <- as.Date(df.periodo$fim   , format = "%d/%m/%Y")

df.jogos['data']   <- as.Date(df.jogos$data  , format = "%d/%m/%Y")

### captalizar strings
df.jogos['dia'      ] <- capwords(df.jogos$dia      , TRUE)
df.jogos['mandante' ] <- capwords(df.jogos$mandante , TRUE)
df.jogos['visitante'] <- capwords(df.jogos$visitante, TRUE)
df.jogos['vencedor' ] <- capwords(df.jogos$vencedor , TRUE)
df.jogos['arena'    ] <- capwords(df.jogos$arena    , TRUE)
 
### junta os datasets e retorna apenas os registros corretos criados na junção (full join)
df <- merge(df.periodo, df.jogos)
df <- df[df$data >= df$inicio & df$data <= df$fim, ]


###### ANÁLISES ######
### GOLS POR EDIÇÃO
gols_mandante  <- aggregate(x=df$mandante.placar , by=list(df$torneio), FUN=sum)
gols_visitante <- aggregate(x=df$visitante.placar, by=list(df$torneio), FUN=sum)

names(gols_mandante)  <- c('torneio', 'gols_mandante')
names(gols_visitante) <- c('torneio', 'gols_visitante')

gols.edicao <- merge(gols_mandante, gols_visitante, by="torneio")

gols.edicao['gols_total'          ] <- gols.edicao$gols_mandante + gols.edicao$gols_visitante
gols.edicao['gols_mandante_perc'  ] <- (gols.edicao$gols_mandante/gols.edicao$gols_total)*100
gols.edicao['gols_visitantes_perc'] <- (gols.edicao$gols_visitante/gols.edicao$gols_total)*100

gols.edicao


### MELHORES E PIORES ATAQUES POR EDIÇÃO
df['clube']                  <- df$mandante
gols.clubes.mandantes        <- aggregate(x=df$mandante.placar , by=list(df$torneio, df$clube), FUN=sum)
names(gols.clubes.mandantes) <- c('torneio', 'clube', 'gols_mandante')

df['clube']                   <- df$visitante
gols.clubes.visitantes        <- aggregate(x=df$visitante.placar , by=list(df$torneio, df$clube), FUN=sum)
names(gols.clubes.visitantes) <- c('torneio', 'clube', 'gols_visitante')

gols.clubes               <- merge(gols.clubes.mandantes, gols.clubes.visitantes, by=c("torneio","clube"))
gols.clubes['gols_total'] <- gols.clubes$gols_mandante + gols.clubes$gols_visitante

ataque_pior   <- aggregate(x=gols.clubes$gols_total, by=list(gols.clubes$torneio), FUN=min)
ataque_melhor <- aggregate(x=gols.clubes$gols_total, by=list(gols.clubes$torneio), FUN=max)

names(ataque_pior)   <- c('torneio','ataque_pior')
names(ataque_melhor) <- c('torneio','ataque_melhor')

gols.torneio.ataque.pior <- merge(gols.clubes, ataque_pior, by.x=c("torneio",'gols_total'), by.y=c("torneio","ataque_pior"))[c('torneio','clube','gols_total')]
names(gols.torneio.ataque.pior)[names(gols.torneio.ataque.pior) == 'gols_total'] <- 'ataque_pior'

gols.torneio.ataque.melhor <- merge(ataque_melhor, gols.clubes, by.x=c("torneio","ataque_melhor"), by.y=c("torneio",'gols_total'))[c('torneio','clube','ataque_melhor')]

gols.ataques <- merge(gols.torneio.ataque.melhor, gols.torneio.ataque.pior, by="torneio", suffix=c("_melhor","_pior"))

gols.ataques


# MAIOR E MENOR PONTUADOR DE TODAS AS EDIÇÕES
pontos.participantes               <- aggregate(x=df[, c('mandante')], by=list(df[, c('torneio')]), FUN=n_distinct)
names(pontos.participantes)        <- c('torneio','times')
pontos.participantes['pontos_max'] <- (pontos.participantes$times - 1)*2*3

pontos <- df[, c('torneio', 'mandante', 'visitante', 'mandante.placar', 'visitante.placar')]
pontos['pontos_mandante' ] <- ifelse(pontos$mandante.placar  > pontos$visitante.placar, 3, ifelse(pontos$mandante.placar  < pontos$visitante.placar, 0, 1))
pontos['pontos_visitante'] <- ifelse(pontos$visitante.placar > pontos$mandante.placar , 3, ifelse(pontos$visitante.placar < pontos$mandante.placar, 0, 1))

pontos.mandantes        <- aggregate(x=pontos$pontos_mandante, by=list(pontos$torneio, pontos$mandante), FUN=sum)
names(pontos.mandantes) <- c('torneio','clube','pontos_mandante')

pontos.visitante        <- aggregate(x=pontos$pontos_visitante, by=list(pontos$torneio, pontos$visitante), FUN=sum)
names(pontos.visitante) <- c('torneio','clube','pontos_visitante')

pontos.total                 <- merge(pontos.mandantes, pontos.visitante, by=c("torneio","clube")) 
pontos.total['pontos_total'] <- pontos.total$pontos_mandante + pontos.total$pontos_visitante  
  
### maior pontuador de todas as edições
maior.pontuador <- pontos.total[pontos.total$pontos_total == max(pontos.total$pontos_total), ]
maior.pontuador

### menor pontuador de todas as edições
menor.pontuador <- pontos.total[pontos.total$pontos_total == min(pontos.total$pontos_total), ]
menor.pontuador



### CAMPEÕES
campeoes        <- aggregate(x=pontos.total$pontos_total, by=list(pontos.total$torneio), FUN=max)
names(campeoes) <- c('torneio','maior_ponto')

campeoes <- merge(campeoes, pontos.total, by=c("torneio"))
campeoes <- campeoes[campeoes$maior_ponto == campeoes$pontos_total, ]
campeoes <- merge(campeoes, pontos.participantes, by=c("torneio"))
campeoes['aproveitamento'] <- (campeoes$pontos_total/campeoes$pontos_max)*100
campeoes['ano'           ] <- gsub('[a-z|A-Z]','', campeoes$torneio)

### Campeões ano a ano
campeoes[, c('torneio', 'clube', 'pontos_mandante', 'pontos_visitante', 'pontos_total', 'aproveitamento')]


### campeões com a maior e menor pontuação
campeoes.pontuacao                <- campeoes[, c('torneio', 'clube', 'pontos_total', 'aproveitamento')]
campeoes.pontuacao['maior_ponto'] <- max(campeoes.pontuacao$pontos_total)
campeoes.pontuacao['menor_ponto'] <- min(campeoes.pontuacao$pontos_total)
campeoes.pontuacao                <- campeoes.pontuacao[campeoes.pontuacao$pontos_total == campeoes.pontuacao$maior_ponto | campeoes.pontuacao$pontos_total == campeoes.pontuacao$menor_ponto, ]  
campeoes.pontuacao['observacao']  <- ifelse(campeoes.pontuacao$pontos_total == campeoes.pontuacao$maior_ponto, 'Campeão com a maior pontuação', 'Campeão com a menor pontuação')
campeoes.pontuacao <- campeoes.pontuacao[, c('torneio', 'clube', 'pontos_total', 'observacao')]
campeoes.pontuacao


### campeões com o maior e menor aproveitamento
campeoes.aproveitamento <- campeoes[, c('torneio', 'clube', 'pontos_total', 'aproveitamento')]
campeoes.aproveitamento['maior_aproveitamento'] <- max(campeoes.aproveitamento$aproveitamento)
campeoes.aproveitamento['menor_aproveitamento'] <- min(campeoes.aproveitamento$aproveitamento)
campeoes.aproveitamento                         <- campeoes.aproveitamento[campeoes.aproveitamento$aproveitamento == campeoes.aproveitamento$maior_aproveitamento | campeoes.aproveitamento$aproveitamento == campeoes.aproveitamento$menor_aproveitamento, ]  
campeoes.aproveitamento['observacao']           <- ifelse(campeoes.aproveitamento$aproveitamento == campeoes.aproveitamento$maior_aproveitamento, 'Campeão com o melhor aproveitamento', 'Campeão com o menor aproveitamento')
campeoes.aproveitamento <- campeoes.aproveitamento[, c('torneio', 'clube', 'aproveitamento', 'observacao')]
campeoes.aproveitamento

### gráfico de aproveitamento dos campeões
ggplot(campeoes, aes(x=ano, y=aproveitamento, fill=clube)) +
  geom_bar(position='dodge', stat='identity') +
  geom_text(aes(label=as.integer(aproveitamento)), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(title='Aproveitamento dos campeões do brasileirão de pontos corridos', x='Ano', y='Aproveitamento') +
  guides(colour = "none") +
  theme_light() +
  ggeasy::easy_center_title()

