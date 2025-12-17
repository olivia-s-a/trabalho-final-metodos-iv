#install.packages("broom")
library(haven)
library(dplyr)
library(broom)

#dados
lapop_br<- read_sav("1.LAPOP_BR_2020_ECON.sav")

cidades_paulistas <- read_sav("2.BD_CausalidadeReversa_PesquisasCidadesPaulistas.sav")

telefone <- read_sav("3.BD_CausalidadeReversa_Telefone.sav")        

#limpar dados
cidades_paulistas <- cidades_paulistas %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%   
  mutate(across(where(is.character), ~ na_if(., " "))) %>%   
  mutate(across(where(is.character), ~ na_if(., "  ")))

experimento_1 <- cidades_paulistas %>%
  select(right, left, treatment) %>%
  mutate(
    id=row_number(),
    answered = case_when(is.na(right)~0,
                         TRUE ~ 1),
    treatment=factor(treatment)
    )

# regressão logística
#regressão logística é usada principalmente pra variáveis binárias
#experimento 1
xp1_control_ref <- experimento_1 %>%
  mutate(
    treatment=relevel(treatment, ref="Control")
  )

reg_logistica <- glm(answered ~ treatment,
                     data=xp1_control_ref,
                     family=binomial(link= "logit"))

summary(reg_logistica)

  

#experimento 2

#reg_logistica <- glm(votaria_governo ~ ideologia, data=br_latbar_23,family=binomial(link= "logit"))