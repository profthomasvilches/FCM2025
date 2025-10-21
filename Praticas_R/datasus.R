

#read.dbc::read.dbc()


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


