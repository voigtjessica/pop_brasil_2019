#População do Brasil

# O que eu faço aqui:

# Raspo a população do Brasil por cidades em 2019 usando o Rvest (e depois as capitais)
# Salvo em um objeto R

library(data.table)
library(rvest)
library(tidyverse)
library(janitor)

url_base <- "https://pt.wikipedia.org/wiki/Lista_de_munic%C3%ADpios_do_Brasil_por_popula%C3%A7%C3%A3o"

pagina <- read_html(url_base)

lista_tabelas <- html_table(pagina, header = TRUE)
print(lista_tabelas)

cidades <- as.data.frame(lista_tabelas)

t <- cidades %>%
  clean_names() %>%
  filter(municipio %in% capitais) 

#Baixando lista das capitais
url2 <-"https://pt.wikipedia.org/wiki/Lista_de_capitais_do_Brasil_por_popula%C3%A7%C3%A3o"
pagina <- read_html(url2)

lista_tabelas <- html_table(pagina, header = TRUE)
capital <- as.data.frame(lista_tabelas[[1]])

c <- capital %>%
  clean_names() %>%
  select(capitais, unidade_federativa) %>%
  mutate(is_capital = TRUE) %>%
  rename(municipio = capitais)

pop_brasil_2019 <- cidades %>%
  clean_names() %>%
  left_join(c) %>%
  mutate(is_capital = ifelse(is.na(is_capital), FALSE, TRUE),
         posicao = gsub("º", "", posicao),
         posicao = as.numeric(posicao))
  
setwd("C:/Users/coliv/Documents/R-Projects/pop_brasil_2019/pop_brasil_2019")
save(pop_brasil_2019, file="pop_brasil_2019.Rdata")
fwrite(pop_brasil_2019, file="pop_brasil_2019.csv")

#Alguns cálculos:

cidades %>%
  clean_names() %>%
  left_join(c) %>%
  mutate(is_capital = ifelse(is.na(is_capital), FALSE, TRUE),
         serve = ifelse(populacao > 200000 | municipio == "Diadema" | is_capital == TRUE,
                        TRUE, FALSE)) %>%
  summarise(pop = sum(populacao[which(serve == TRUE)]),
            pop_total = sum(populacao),
            perc = pop/pop_total)

cidades %>%
  clean_names() %>%
  mutate(serve = ifelse(populacao > 200000 | municipio == "Diadema", TRUE, FALSE)) %>%
  summarise(pop = sum(populacao[which(serve == TRUE)]),
            pop_total = sum(populacao),
            perc = pop/pop_total)