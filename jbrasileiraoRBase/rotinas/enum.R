enum <- function(...) {
  nms <- eval(substitute(alist(...)))
  x <- as.list(setNames(as.character(nms), nms))
}

enumDataSets <- 
  enum("campeonato-brasileiro-pontos-corridos-2003-2020-jogos.csv",
       "campeonato-brasileiro-pontos-corridos-2003-2020-periodo.csv")
