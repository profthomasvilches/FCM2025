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

# exemplo operador

dados %>% 
  select(name, hp, attack, speed) %>% 
  filter(attack < 50) %>% 
  mutate(x = attack+speed)

x = c("Thomas", "Fernando", "Thais")

x %>% 
gsub("Th", "th", .)



