library(jsonlite)
library(yaml)
library(purrr)
library(tidyverse)
library(ggplot2)

# Parte 0:

relatorio1 <- read_json("~/Atividade2_ME918/dados/relatorio1.json")
relatorio2 <- read_json("~/Atividade2_ME918/dados/relatorio2.json")
relatorio3 <- read_json("~/Atividade2_ME918/dados/relatorio3.json")
relatorio4 <- read_json("~/Atividade2_ME918/dados/relatorio4.json")

str(relatorio1)
str(relatorio2)
str(relatorio3)
str(relatorio4)

# Parte 1:
#configuracoes.yaml

# Parte 2:

configuracoes <- read_yaml("~/Atividade2_ME918/configuracoes.yaml")

funcao_leitura <- function(x){
  paste0("~/Atividade2_ME918/dados/",x)
}


lista_arquivos <- funcao_leitura(configuracoes$arquivos)

lista_relatorios <- map(lista_arquivos, read_json)

# Parte 3:

lista_df1a <- map(lista_relatorios[[1]], data.frame)
lista_df1b <- list_rbind(lista_df1a)

lista_df2a <- map(lista_relatorios[[2]], data.frame)
lista_df2b <- list_rbind(lista_df2a)

lista_df3a <- map(lista_relatorios[[3]], data.frame)
lista_df3b <- list_rbind(lista_df3a)

lista_df4a <- map(lista_relatorios[[4]], data.frame)
lista_df4b <- list_rbind(lista_df4a)

list_df <- rbind(lista_df1b, lista_df2b, lista_df3b, lista_df4b)

list_df1 <- list_df %>% 
  mutate (across(evento, as.factor))

list_df1$horario <- ymd_hms(list_df1$horario)


# Parte 4:

list_df1 %>% arrange(horario) %>% 
  mutate(recalibragem = evento == "recalibragem") %>% 
  mutate(contagem = cumsum(recalibragem)) %>% 
  ggplot(aes(x = horario, y = contagem))+
  geom_line()+
  labs(y = "Total de Recalibragens")


