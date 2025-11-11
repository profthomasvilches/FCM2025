



library(tidyverse)


dengue_sp <- read.csv2("Dados/dengue_sp.csv")
View(dengue_sp)

dengue_sp <- dengue_sp %>% 
  rename(municipio = 1)

dengue_sp <- dengue_sp %>% 
  filter(municipio != "Total",
         municipio != "&",
         !grepl("IGNORADO", municipio) # ! significa não
         )

tail(dengue_sp)

View(dengue_sp)


## dados do IBGE

dados_ibge <- read.csv2("Dados/tabela4709.csv", skip = 3)

View(dados_ibge)

dados_ibge <- dados_ibge %>% 
  rename(Cod = 1)

### Manipulando dados

# separar o codigo do municipio

dengue_sp <- dengue_sp %>% 
  mutate(
    codigo = str_extract(municipio, "\\d{6}")
  )


dados_ibge <- dados_ibge %>% 
  mutate(
    Cod2 = str_extract(Cod, "^\\d{6}")
  )

# esse serve também
dados_ibge %>% 
  mutate(
    Cod2 = str_remove(Cod, "\\d$")
  )



## manipulando dengue

glimpse(dengue_sp)

dengue_sp <- dengue_sp %>% 
  mutate(across(starts_with("X"), as.integer))%>% # transforma em inteiro
  replace(is.na(.), 0) # coloca zero no lugar

dengue_sp <- dengue_sp %>% 
  select(-Total) %>% 
  pivot_longer(2:12, names_to = "Ano", values_to = "Casos")


dengue_sp <- dengue_sp %>% 
  mutate(Ano = str_remove_all(Ano, "\\D")) %>% 
  mutate(Ano = as.integer(Ano))

df_final <- dados_ibge %>% 
  select(Cod = Cod2, Inhab = X2022) %>% 
  right_join(dengue_sp, by = c("Cod" = "codigo"))

df_final %>% filter(is.na(Inhab))

df_final <- df_final %>% 
  mutate(
    incidencia = Casos/Inhab*100000
  )

df_final %>% 
  filter(Casos > 600000)


# graficos de caixa
ggplot(df_final)+
  geom_boxplot(aes(x = factor(Ano), y = Casos))



ggplot(df_final)+
  geom_boxplot(aes(x = factor(Ano), y = incidencia))


#### Lendo dados DBC


library(tidyverse)


#read.dbc::read.dbc()


dados <- read.dbc::read.dbc("Dados/DENGBR24.dbc")

nrow(dados)

names(dados)

glimpse(dados)

dados_sp <- dados %>% 
  filter(grepl("^35", ID_MN_RESI))

nrow(dados_sp)

rm(dados)

glimpse(dados_sp)

write.csv(dados_sp, "dados_dengue_sp.csv")

names(dados_sp)

dados_sp %>% 
  select(CLASSI_FIN) %>% unique



############# trabalhar com conjunto de dados muito grandes

library(tidyverse)
library("duckdb")

con <- dbConnect(duckdb::duckdb(), dbdir = "Dados/meu_banco.duckdb")
# com os dados em um CSV
dbExecute(con, "CREATE TABLE dados AS SELECT * FROM read_csv_auto('dados_dengue_sp.csv', encoding='latin-1', sep = ';')")

# com os dados no R
dbWriteTable(con, "dados", dados_sp, overwrite = TRUE)

# Consultas SQL diretas (sem carregar na RAM!)
dbGetQuery(con, "DESCRIBE dados")

sem_pri <- dbGetQuery(con, 'SELECT SEM_PRI FROM dados WHERE CLASSI_FIN IN (10, 11, 12)')


df_sem_prim <- data.frame(sem_pri)

df_sem_prim %>% 
  group_by(SEM_PRI) %>% 
  summarise(
    n = n()
  ) %>% 
  ungroup() %>% 
  ggplot()+
  geom_col(aes(x = SEM_PRI, y = n))+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.5)
  )



################## Dados de mortalidade



library(tidyverse)


glimpse(dados_simdo)


roda_arquivo <- function(arquivo){
  
  dados_simdo <- read.dbc::read.dbc(arquivo)
  
  dados <- dados_simdo %>% 
    select(DTOBITO, CAUSABAS_O) %>% 
    mutate(
      dataobito = as.Date(DTOBITO, "%d%m%Y")
    )
  # 
  # as.Date("2015-10-12")-as.Date("2015-10-10")
  # 
  # x <- c(as.Date("2015-10-12"), as.Date("2015-07-12"), as.Date("2015-05-12"))
  # 
  # floor(as.integer(x-as.Date("2015-01-01"))/7+1)
  
  
  dados <- dados %>% 
    mutate(
      semana = floor(as.integer(dataobito-as.Date("2015-01-01"))/7+1),
      diasemana = as.Date("2015-01-01")+7*(semana-1),
      estado = str_remove(arquivo, ".*DO"),
      estado = str_remove(estado, "\\d{4}\\.dbc")
    )
  return(dados)
  
}

arquivo = "Dados/DATASUS/DOAC2015.dbc"

roda_arquivo("Dados/DATASUS/DOAC2015.dbc")

arquivos <- list.files("Dados/DATASUS/", full.names = TRUE)

resultados <- map(arquivos, roda_arquivo)

dados <- Reduce(rbind, resultados)


nrow(dados)
head(dados)

dados %>% 
  group_by(diasemana, estado) %>% 
  summarise(
    n = n()
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = diasemana, y = n, color = estado))+
  geom_line()+
  theme_bw()

# 02/03/2015 -> %d/%m/%Y
# lubridate

dmy("02032015")
dmy("02/03/2015")

# rastreando neoplasias CID

dados_cancer <- dados_simdo %>% 
  filter(
    grepl("^C|^D[1234]", CAUSABAS)
  )

nrow(dados_cancer)





