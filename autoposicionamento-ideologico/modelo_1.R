#install.packages("broom")
library(haven)
library(dplyr)
library(broom)
#MODELOS ESSENCIAIS
#modelo 1
#modelo 2
#modelo 5

#modelo 1: Estudo 1 da taxa de resposta
#Regressão Logística
#Y: respondeu ou não sobre sua autoidentificação ideoógica
# X principal: Tratamento Bolsonaro-Direita
# outros x: cidade + infos sociodemograficas
#objetivo: causalidade das respostas

#regressão multivariada -> diversos x -> controles sociodemográficos

#EXPERIMENTO 1
cidades_paulistas <- read_sav("2.BD_CausalidadeReversa_PesquisasCidadesPaulistas.sav")
#limpar dados
cidades_paulistas <- cidades_paulistas %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%   
  mutate(across(where(is.character), ~ na_if(., " "))) %>%   
  mutate(across(where(is.character), ~ na_if(., "  ")))

experimento_1 <- cidades_paulistas %>%
  select(
    #autoposicionamento ideológico
    right, 
    left,
    l1_rec,
    l1_missing,
    l1_nomi,
    #avaliacao do presidente
    m1_cat,
    m1_recode,
    #tratamento
    treatment,
    #informações sociodemográficas
    female,
    age,
    educ_cat,
    income_cat,
    #cidade
    city,
    #neighborhood
  ) %>%
  rename(
    answered = l1_missing,
    l1_cat = l1_nomi,
    l1_desc = l1_rec
  ) %>%
  mutate(
    id=row_number(),
    treatment=factor(treatment),
    age=as.integer(age)
  )
#tirar Santos da amostra
unique(experimento_1$city)
xp_1_fltr <- subset(experimento_1, city != "Santos")
xp_1_fltr
nrow(xp_1_fltr)
#Criando pessoa padrão de referência (Jureminha)
#OBS: pq não tem nenhuma referência pra income_cat
jureminha <- data.frame(
  female ="Mulher",
  age=40,
  city='Guarujá',
  educ_cat="Superior incompleto ou mais",
  m1_cat=3,
  treatment = c('Control', 'Treatment')
)

#Referencia
experimento_1 <- experimento_1 %>%
  mutate(
    treatment=relevel(treatment, ref="Control")
  )

#Regressão logística multivariada
modelo_1 <- glm(answered ~ treatment + city + income_cat + educ_cat + age + female,
                     data=xp_1_fltr,
                     family=binomial(link= "logit"))
summary(modelo_1)
resultados_tidy <- tidy(modelo_1, conf.int = TRUE)
resultados_tidy

#Já no segundo apontamos o código de área (DDD) 12, Esses valores foram escolhidos por serem os valores médios ou mais frequentes no experimento 1. 



# regressão logística
#regressão logística é usada principalmente pra variáveis binárias

#experimento 2

