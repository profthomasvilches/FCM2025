x = c(1, 2, 3)
y = c(2, 4, 6)
plot(x, y)

getwd()

dados <- read.csv("Dados/Pokemon_full.csv")

dados <- read.csv("~/Downloads/Pokemon_full.csv")

head(dados) # olha as primeiras linhas
tail(dados, 12) # olha as ultimas linhas

View(dados)

library(tidyverse)

names(dados)

# Seleciona colunas
select(dados, name, hp, speed, attack)

# filtra colunas
filter(dados, attack < 50)

# operacoes

mutate(dados, x = attack+speed) # cria nova variável
mutate(dados, attack = attack/2) # modifica variável
mutate(dados, IMC = weight/(height*height)) # modifica variável

dados <- mutate(dados, IMC = weight/(height*height))

# exemplo operador

df <- select(dados, name, hp, attack, speed)
df <- filter(df, attack < 50)
df <- mutate(df, x = attack+speed)
df

# O operador pipe pega o que está à esquerda dele e coloca como primeiro argumento da próxima função
df <- dados %>% 
  select(name, hp, attack, speed) %>% 
  filter(attack < 50) %>% 
  mutate(x = attack+speed)

# gsub modifica strings
x <- c("Thomas", "Fernando", "Thais")
# Se colocar ponto, o pipe ele rastreia e substitui. Se não colocar, ele põe como primeiro argumento.
x %>% 
gsub("Th", "th", .)
# |> pipe nativo do R


dados %>%
  filter(height > 10) %>%
  select(name, height, weight)%>%
  mutate(imc = weight/(height*height)) %>% 
  ggplot()+
  geom_density(aes( x = imc))


head(dados)
dados %>% head

# comando interessante
glimpse(dados)
summary(dados)
str(dados)

dados %>% pull(IMC) # retorna vetor
dados %>% select(IMC) # retorna uma coluna

mean(c(1, 2, 3, 4))

dados %>% 
  mutate(media = mean(IMC)) # cria e preenche uma coluna com o mesmo valor

dados %>% 
  summarise(media = mean(IMC), desvio = sd(IMC)) # resume os dados, retornando uma coluna para cada variável

dados %>%
  group_by(type) %>% 
  summarise(media = mean(IMC), desvio = sd(IMC)) # resume os dados, retornando uma coluna para cada variável


dados %>% 
  group_by(type) %>% 
  mutate(media = mean(IMC)) %>% View # cria e preenche uma coluna com o mesmo valor


dados %>% 
  group_by(type) %>% 
  mutate(media = mean(IMC)) %>% 
  filter(IMC > media) %>% View

df <- dados %>% 
  group_by(type) %>% 
  mutate(media = mean(IMC))

df %>% 
  ungroup() %>% 
  mutate(media2 = mean(IMC)) %>% View

# busca padrões
# aceita Regular Expression (ReGex)
grep("saur|fly", dados$name)
grepl("saur|fly", dados$name)


grep("[Ss]aur", dados$name)

x
grep("Th[oa]", x)

n <- c("097.765.986-90", "123.765.98-37")
grepl("\\d{3}\\d{3}\\.\\.\\d{3}-\\d{2}", n)


x <- c(
  "Amonia",
  "Ferro",
  "Dióxido de enxofre",
  "Dioxido de Enxofre",
  "Manganês",
  "Dióxido  de  Enxofre",
  "dioxido de  Enxofre",
  "dioxidode  Enxofre"
)

# + um ou mais
# * zero ou mais
grepl("[Dd]i[óo]xido *de\\s+[eE]nxofre", x)

grepl(".", c("a", "b", "c", "0", " "))

dados %>% 
  filter(attack > 50)

dados$attack > 50

dados %>% 
  filter(grepl("saur|fly", name), attack > 50, type != "grass")

"saur" == "ivysaur"
grepl("saur", "ivysaur")

#### trabalhando juntando dados

# bind row

df1 <- dados %>% 
    filter(attack > 70)

df2 <- dados %>% 
    filter(attack <= 70)


rbind(df1, df2) # juntar linhas

# com colunas diferentes

df1 <- dados %>% 
  select(attack, speed, weight) %>% 
  filter(attack > 70)

df2 <- dados %>% 
  select(attack, weight, height, hp) %>% 
  filter(attack <= 70)

rbind(df1, df2) # juntar linhas - nao aceita dimensoes e nomes diferentes

bind_rows(df1, df2) # juntar linhas - completa se não bater


# juntar colunas

df1 <- dados %>% head(100)
df2 <- dados %>% tail(100)

cbind(df1, df2) %>% names

bind_cols(df1, df2, .name_repair = "check_unique")

###############

df_resumo <- dados %>%
  group_by(type) %>% 
  summarise(media = mean(IMC), desvio = sd(IMC)) # resume os dados, retornando uma coluna para cada variável

  

# Fazendo join
# left, right, full, inner

left_join(dados, df_resumo, by = c("type")) %>% View

df_resumo_mis <- df_resumo %>%  filter(type != "grass")

left_join(dados, df_resumo_mis, by = c("type")) %>% View
right_join(dados, df_resumo_mis, by = c("type")) %>% View

df_resumo_mis$type[5] = "thomas" 
right_join(dados, df_resumo_mis, by = c("type")) %>% View
left_join(dados, df_resumo_mis, by = c("type")) %>% View

full_join(dados, df_resumo_mis, by = c("type")) %>% View
inner_join(dados, df_resumo_mis, by = c("type")) %>% View



