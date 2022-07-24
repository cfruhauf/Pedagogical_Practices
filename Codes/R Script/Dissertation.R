################################################### Packages ###################################################### 
#Before starting the analysis I loaded the packages that would be used:

library(tidyverse)
library(dummies)
library(scales)
library(patchwork)

#I also set directory that R should use, the one where I had saved the microdata on my computer:

setwd("~/Dissertação/Dados/2017/DADOS")

#Next step is created a data frame for each school level I would analized and for teachers, principals and schools data.

#######################################    9th year of Elementary School   ##########################################
ALUNO9EF <- read.csv("TS_ALUNO_9EF.csv", header=T,sep=",") #Loading the file

ALUNO9EF <- subset(ALUNO9EF, IN_PREENCHIMENTO_QUESTIONARIO == 1 & IN_PREENCHIMENTO_PROVA == 1) #Deleting observations with tests or/and blank questionnaires

ALUNO9EF = ALUNO9EF %>% rename(REGIAO = ID_REGIAO, ESCOLA = ID_ESCOLA, DEPENDENCIA_ADM = ID_DEPENDENCIA_ADM,
                               LOCALIZACAO = ID_LOCALIZACAO, TURMA = ID_TURMA, ALUNO = ID_ALUNO,
                               LP = PROFICIENCIA_LP_SAEB, MT = PROFICIENCIA_MT_SAEB, GENERO = TX_RESP_Q001,
                               RACA = TX_RESP_Q002, TV = TX_RESP_Q005, GELADEIRA = TX_RESP_Q008,
                               FREEZER = TX_RESP_Q010, MAQ = TX_RESP_Q011, CARRO = TX_RESP_Q012,
                               COMP = TX_RESP_Q013, BANHO = TX_RESP_Q014, EMPR = TX_RESP_Q017,
                               ESC_MAE = TX_RESP_Q019, ESC_PAI = TX_RESP_Q023, REPROV = TX_RESP_Q048,
                               GOSTA_LP = TX_RESP_Q050, GOSTA_MT = TX_RESP_Q053) #Columns renaming

ALUNO9EF <- ALUNO9EF %>% filter(!GENERO == "" & !RACA == "" & !TV  == "" & !GELADEIRA == "" &
                                !FREEZER == "" & !MAQ == "" & !CARRO == "" & !COMP == "" &
                                !BANHO == "" & !EMPR == "" & !ESC_MAE == "" & !ESC_PAI == "" &
                                !REPROV == "" & !GOSTA_LP == "" & !GOSTA_MT == "") #Deleting other blanks observations

#Standardizing answers to model
ALUNO9EF <- ALUNO9EF %>%
  mutate(DEPENDENCIA_ADM = case_when(DEPENDENCIA_ADM == "1" ~ "0",  #Federal
                                     DEPENDENCIA_ADM == "2" ~ "0",  #Estadual
                                     DEPENDENCIA_ADM == "3" ~ "0",  #Municipal
                                     DEPENDENCIA_ADM == "4" ~ "1")) #Privado

ALUNO9EF <- ALUNO9EF %>%
  mutate(LOCALIZACAO = case_when(LOCALIZACAO == "1" ~ "1",  #Urbano
                                LOCALIZACAO == "2" ~ "0")) #Rural

# Questão 1 - Qual é o seu sexo?
ALUNO9EF <- ALUNO9EF %>% 
  mutate(GENERO = case_when(GENERO == "A" ~ "0",  #Masculino
                            GENERO == "B" ~ "1")) #Feminino

# Questão 2 - Qual é a sua cor ou raça?
ALUNO9EF <- ALUNO9EF %>% filter(!RACA == "F") %>%
  mutate(RACA = case_when(RACA == "A" ~ "1",  #Branca
                          RACA == "B" ~ "0",  #Preta
                          RACA == "C" ~ "0",  #Parda
                          RACA == "D" ~ "1",  #Amarelo
                          RACA == "E" ~ "0")) #Indígena

# Questão 5 - Na sua casa tem televisão em cores?
ALUNO9EF <- ALUNO9EF %>%
  mutate(TV = case_when(TV == "A" ~ "1",  #Não tem
                        TV == "B" ~ "2",  #Sim, uma
                        TV == "C" ~ "2",  #Sim, duas
                        TV == "D" ~ "2",  #Sim, três
                        TV == "E" ~ "2")) #Sim, quatro ou mais

# Questão 8 - Na sua casa tem geladeira?
ALUNO9EF <- ALUNO9EF %>%
  mutate(GELADEIRA = case_when(GELADEIRA == "A" ~ "1",  #Não tem
                               GELADEIRA == "B" ~ "2",  #Sim, uma
                               GELADEIRA == "C" ~ "2",  #Sim, duas
                               GELADEIRA == "D" ~ "2",  #Sim, três
                               GELADEIRA == "E" ~ "2")) #Sim, quatro ou mais

# Questão 9 - Na sua casa tem freezer (parte da geladeira duplex)?
ALUNO9EF <- ALUNO9EF %>%
  mutate(FREEZER = case_when(FREEZER == "A" ~ "1",  #Não tem
                             FREEZER == "B" ~ "2",  #Sim, um
                             FREEZER == "C" ~ "2",  #Sim, dois
                             FREEZER == "D" ~ "2",  #Sim, três
                             FREEZER == "E" ~ "2")) #Sim, quatro ou mais

# Questão 11 - Na sua casa tem máquina de lavar roupa (O tanquinho NÃO deve ser considerado)?
ALUNO9EF <- ALUNO9EF %>%
  mutate(MAQ = case_when(MAQ == "A" ~ "1",  #Não tem
                         MAQ == "B" ~ "2",  #Sim, uma
                         MAQ == "C" ~ "2",  #Sim, duas
                         MAQ == "D" ~ "2",  #Sim, três
                         MAQ == "E" ~ "2")) #Sim, quatro ou mais

# Questão 12 - Na sua casa tem carro?
ALUNO9EF <- ALUNO9EF %>%
  mutate(CARRO = case_when(CARRO == "A" ~ "1",  #Não tem
                           CARRO == "B" ~ "2",  #Sim, um
                           CARRO == "C" ~ "2",  #Sim, dois
                           CARRO == "D" ~ "2",  #Sim, três
                           CARRO == "E" ~ "2")) #Sim, quatro ou mais

# Questão 13 - Na sua casa tem computador?
ALUNO9EF <- ALUNO9EF %>%
  mutate(COMP = case_when(COMP == "A" ~ "1",  #Não tem
                          COMP == "B" ~ "2",  #Sim, um
                          COMP == "C" ~ "2",  #Sim, dois
                          COMP == "D" ~ "2",  #Sim, três
                          COMP == "E" ~ "2")) #Sim, quatro ou mais

# Questão 14 - Na sua casa tem banheiro?
ALUNO9EF <- ALUNO9EF %>%
  mutate(BANHO = case_when(BANHO == "A" ~ "1",  #Não tem
                           BANHO == "B" ~ "2",  #Sim, um
                           BANHO == "C" ~ "2",  #Sim, dois
                           BANHO == "D" ~ "2",  #Sim, três
                           BANHO == "E" ~ "2")) #Sim, quatro ou mais

# Questão 17 - Em sua casa trabalha empregado(a) doméstico(a) pelo menos cinco dias por semana?
ALUNO9EF <- ALUNO9EF %>%
  mutate(EMPR = case_when(EMPR == "A" ~ "1",  #Não
                          EMPR == "B" ~ "2",  #Sim, um(a) empregado(a)
                          EMPR == "C" ~ "2",  #Sim, dois(duas) empregados(as)
                          EMPR == "D" ~ "2",  #Sim, três empregados(as)
                          EMPR == "E" ~ "2")) #Sim, quatro ou mais empregados(as)

# Questão 19 - Até que série sua mãe, ou a mulher responsável por você, estudou?
ALUNO9EF <- ALUNO9EF %>% filter(!ESC_MAE == "G") %>%
  mutate(ESC_MAE = case_when(ESC_MAE == "A" ~ "1",  #Nunca estudou
                             ESC_MAE == "B" ~ "1",  #Não completou a 4.ª série/5.º ano
                             ESC_MAE == "C" ~ "2",  #Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano
                             ESC_MAE == "D" ~ "3",  #Completou a 8.ª série/9.º ano, mas não completou o Ensino Médio
                             ESC_MAE == "E" ~ "4",  #Completou o Ensino Médio, mas não completou a Faculdade
                             ESC_MAE == "F" ~ "5")) #Completou a Faculdade

# Questão 23 - Até que série seu pai, ou o homem responsável por você, estudou?
ALUNO9EF <- ALUNO9EF %>% filter(!ESC_PAI == "G") %>%
  mutate(ESC_PAI = case_when(ESC_PAI == "A" ~ "1",  #Nunca estudou
                             ESC_PAI == "B" ~ "1",  #Não completou a 4.ª série/5.º ano
                             ESC_PAI == "C" ~ "2",  #Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano
                             ESC_PAI == "D" ~ "3",  #Completou a 8.ª série/9.º ano, mas não completou o Ensino Médio
                             ESC_PAI == "E" ~ "4",  #Completou o Ensino Médio, mas não completou a Faculdade
                             ESC_PAI == "F" ~ "5")) #Completou a Faculdade

# Questão 48 - Você já foi reprovado?
ALUNO9EF <- ALUNO9EF %>%
  mutate(REPROV = case_when(REPROV == "A" ~ "0",  #Não
                            REPROV == "B" ~ "1",  #Sim, uma vez
                            REPROV == "C" ~ "1")) #Sim, duas vezes ou mais

# Questão 50 - Você gosta de estudar Língua Portuguesa?
ALUNO9EF <- ALUNO9EF %>%
  mutate(GOSTA_LP = case_when(GOSTA_LP == "A" ~ "1",  #Sim
                              GOSTA_LP == "B" ~ "0")) #Não

# Questão 53 - Você gosta de estudar Matemática?
ALUNO9EF <- ALUNO9EF %>%
  mutate(GOSTA_MT = case_when(GOSTA_MT == "A" ~ "1",  #Sim
                              GOSTA_MT == "B" ~ "0")) #Não

#Average grade per class
ALUNO9EF_TURMA = ALUNO9EF %>% group_by(TURMA) %>% summarise(MT = mean(MT), LP = mean(LP))
ALUNO9EF = merge(ALUNO9EF, ALUNO9EF_TURMA, by.x=c("TURMA"),  by.y=c("TURMA"), all=TRUE)
ALUNO9EF = ALUNO9EF %>% rename(LP = LP.x, LP_TURMA = LP.y, MT = MT.x, MT_TURMA = MT.y) #Renaming columns

#Creating dummies
ALUNO9EF <- cbind(ALUNO9EF, dummy(ALUNO9EF$ESC_MAE, sep = " "))
ALUNO9EF = ALUNO9EF %>%
  rename(ESC_MENOS_4_5_MAE=`ALUNO9EF 1`,ESC_MENOS_8_9_MAE=`ALUNO9EF 2`,ESC_MENOS_EM_MAE=`ALUNO9EF 3`,
         ESC_MENOS_ES_MAE=`ALUNO9EF 4`,ESC_ES_MAE=`ALUNO9EF 5`)

ALUNO9EF <- cbind(ALUNO9EF, dummy(ALUNO9EF$ESC_PAI, sep = " "))
ALUNO9EF = ALUNO9EF %>%
  rename(ESC_MENOS_4_5_PAI=`ALUNO9EF 1`,ESC_MENOS_8_9_PAI=`ALUNO9EF 2`,ESC_MENOS_EM_PAI=`ALUNO9EF 3`,
         ESC_MENOS_ES_PAI=`ALUNO9EF 4`,ESC_ES_PAI=`ALUNO9EF 5`)

ALUNO9EF <- cbind(ALUNO9EF, dummy(ALUNO9EF$REGIAO, sep = " "))
ALUNO9EF = ALUNO9EF %>%
  rename(NORTE=`ALUNO9EF 1`,NORDESTE=`ALUNO9EF 2`,SUDESTE=`ALUNO9EF 3`,
         SUL=`ALUNO9EF 4`,CENTRO_OESTE=`ALUNO9EF 5`)

ALUNO9EF <- transform(ALUNO9EF, TURMA = as.factor(TURMA), ESCOLA = as.factor(ESCOLA),
                      ALUNO = as.factor(ALUNO), REGIAO = as.factor(REGIAO), DEPENDENCIA_ADM = as.factor(DEPENDENCIA_ADM),
                      LOCALIZACAO = as.factor(LOCALIZACAO), GENERO = as.factor(GENERO), RACA=as.factor(RACA),
                      TV = as.factor(TV), GELADEIRA = as.factor(GELADEIRA), FREEZER = as.factor(FREEZER),
                      MAQ = as.factor(MAQ), CARRO = as.factor(CARRO), COMP = as.factor(COMP),
                      BANHO = as.factor(BANHO), EMPR = as.factor(EMPR), REPROV = as.factor(REPROV),
                      GOSTA_LP = as.factor(GOSTA_LP), GOSTA_MT = as.factor(GOSTA_MT),
                      ESC_MENOS_4_5_MAE = as.factor(ESC_MENOS_4_5_MAE), ESC_MENOS_8_9_MAE = as.factor(ESC_MENOS_8_9_MAE),
                      ESC_MENOS_EM_MAE = as.factor(ESC_MENOS_EM_MAE), ESC_MENOS_ES_MAE = as.factor(ESC_MENOS_ES_MAE),
                      ESC_ES_MAE = as.factor(ESC_ES_MAE), ESC_MENOS_4_5_PAI = as.factor(ESC_MENOS_4_5_PAI),
                      ESC_MENOS_8_9_PAI = as.factor(ESC_MENOS_8_9_PAI), ESC_MENOS_EM_PAI = as.factor(ESC_MENOS_EM_PAI),
                      ESC_MENOS_ES_PAI = as.factor(ESC_MENOS_ES_PAI), ESC_ES_PAI = as.factor(ESC_ES_PAI),
                      NORTE = as.factor(NORTE), NORDESTE = as.factor(NORDESTE), SUDESTE = as.factor(SUDESTE),
                      SUL = as.factor(SUL), CENTRO_OESTE = as.factor(CENTRO_OESTE))#Transforming variables into Factors

#Subset to Portuguese Language
ALUNO9EFLP = ALUNO9EF [,c(7,1,12,30,95,37,38,41,44,46:50,53,96:105,84,86,106:110,8,9)]
ALUNO9EFLP <- ALUNO9EFLP %>% filter(!is.na(LP))

#Subset to Mathematics
ALUNO9EFMT = ALUNO9EF [,c(7,1,12,34,94,37,38,41,44,46:50,53,96:105,84,89,106:110,8,9)]
ALUNO9EFMT <- ALUNO9EFMT %>% filter(!is.na(MT))

##########################################    3rd year of High School   #############################################
ALUNO3EM <- read.csv("TS_ALUNO_3EM_ESC.csv", header=T,sep=",") #Loading the file

ALUNO3EM <- subset(ALUNO3EM, IN_PREENCHIMENTO_QUESTIONARIO == 1 & IN_PREENCHIMENTO_PROVA == 1) #Deleting observations with tests or/and blank questionnaires

ALUNO3EM = ALUNO3EM %>% rename(REGIAO = ID_REGIAO, ESCOLA = ID_ESCOLA, DEPENDENCIA_ADM = ID_DEPENDENCIA_ADM,
                               LOCALIZACAO = ID_LOCALIZACAO, TURMA = ID_TURMA, ALUNO = ID_ALUNO,
                               LP = PROFICIENCIA_LP_SAEB, MT = PROFICIENCIA_MT_SAEB, GENERO = TX_RESP_Q001,
                               RACA = TX_RESP_Q002, TV = TX_RESP_Q005, GELADEIRA = TX_RESP_Q008,
                               FREEZER = TX_RESP_Q010, MAQ = TX_RESP_Q011, CARRO = TX_RESP_Q012,
                               COMP = TX_RESP_Q013, BANHO = TX_RESP_Q014, EMPR = TX_RESP_Q017,
                               ESC_MAE = TX_RESP_Q019, ESC_PAI = TX_RESP_Q023, REPROV = TX_RESP_Q041,
                               GOSTA_LP = TX_RESP_Q044, GOSTA_MT = TX_RESP_Q052) #Columns renaming

ALUNO3EM <- ALUNO3EM %>% filter(!GENERO == "" & !RACA == "" & !TV  == "" & !GELADEIRA == "" &
                                !FREEZER == "" & !MAQ == "" & !CARRO == "" & !COMP == "" &
                                !BANHO == "" & !EMPR == "" & !ESC_MAE == "" & !ESC_PAI == "" &
                                !REPROV == "" & !GOSTA_LP == "" & !GOSTA_MT == "") #Deleting other blanks observations

#Standardizing answers to model
ALUNO3EM <- ALUNO3EM %>%
  mutate(DEPENDENCIA_ADM = case_when(DEPENDENCIA_ADM == "1" ~ "0",  #Federal
                                     DEPENDENCIA_ADM == "2" ~ "0",  #Estadual
                                     DEPENDENCIA_ADM == "3" ~ "0",  #Municipal
                                     DEPENDENCIA_ADM == "4" ~ "1")) #Privado

ALUNO3EM <- ALUNO3EM %>%
  mutate(LOCALIZACAO = case_when(LOCALIZACAO == "1" ~ "1",  #Urbano
                                 LOCALIZACAO == "2" ~ "0")) #Rural

# Questão 1 - Qual é o seu sexo?
ALUNO3EM <- ALUNO3EM %>%
  mutate(GENERO = case_when(GENERO == "A" ~ "0",  #Masculino
                            GENERO == "B" ~ "1")) #Feminino

# Questão 2 - Qual é a sua cor ou raça?
ALUNO3EM <- ALUNO3EM %>% filter(!RACA == "F") %>%
  mutate(RACA = case_when(RACA == "A" ~ "1",  #Branca
                          RACA == "B" ~ "0",  #Preta
                          RACA == "C" ~ "0",  #Parda
                          RACA == "D" ~ "1",  #Amarelo
                          RACA == "E" ~ "0")) #Indígena

# Questão 5 - Na sua casa tem televisão em cores?
ALUNO3EM <- ALUNO3EM %>%
  mutate(TV = case_when(TV == "A" ~ "1",  #Não tem
                        TV == "B" ~ "2",  #Sim, uma
                        TV == "C" ~ "2",  #Sim, duas
                        TV == "D" ~ "2",  #Sim, três
                        TV == "E" ~ "2")) #Sim, quatro ou mais

# Questão 8 - Na sua casa tem geladeira?
ALUNO3EM <- ALUNO3EM %>% 
  mutate(GELADEIRA = case_when(GELADEIRA == "A" ~ "1",  #Não tem
                               GELADEIRA == "B" ~ "2",  #Sim, uma
                               GELADEIRA == "C" ~ "2",  #Sim, duas
                               GELADEIRA == "D" ~ "2",  #Sim, três
                               GELADEIRA == "E" ~ "2")) #Sim, quatro ou mais

# Questão 9 - Na sua casa tem freezer (parte da geladeira duplex)?
ALUNO3EM <- ALUNO3EM %>%
  mutate(FREEZER = case_when(FREEZER == "A" ~ "1",  #Não tem
                             FREEZER == "B" ~ "2",  #Sim, um
                             FREEZER == "C" ~ "2",  #Sim, dois
                             FREEZER == "D" ~ "2",  #Sim, três
                             FREEZER == "E" ~ "2")) #Sim, quatro ou mais

# Questão 11 - Na sua casa tem máquina de lavar roupa (O tanquinho NÃO deve ser considerado)?
ALUNO3EM <- ALUNO3EM %>%
  mutate(MAQ = case_when(MAQ == "A" ~ "1",  #Não tem
                         MAQ == "B" ~ "2",  #Sim, uma
                         MAQ == "C" ~ "2",  #Sim, duas
                         MAQ == "D" ~ "2",  #Sim, três
                         MAQ == "E" ~ "2")) #Sim, quatro ou mais

# Questão 12 - Na sua casa tem carro?
ALUNO3EM <- ALUNO3EM %>% 
  mutate(CARRO = case_when(CARRO == "A" ~ "1",  #Não tem
                           CARRO == "B" ~ "2",  #Sim, um
                           CARRO == "C" ~ "2",  #Sim, dois
                           CARRO == "D" ~ "2",  #Sim, três
                           CARRO == "E" ~ "2")) #Sim, quatro ou mais

# Questão 13 - Na sua casa tem computador?
ALUNO3EM <- ALUNO3EM %>% 
  mutate(COMP = case_when(COMP == "A" ~ "1",  #Não tem
                          COMP == "B" ~ "2",  #Sim, um
                          COMP == "C" ~ "2",  #Sim, dois
                          COMP == "D" ~ "2",  #Sim, três
                          COMP == "E" ~ "2")) #Sim, quatro ou mais

# Questão 14 - Na sua casa tem banheiro?
ALUNO3EM <- ALUNO3EM %>% 
  mutate(BANHO = case_when(BANHO == "A" ~ "1",  #Não tem
                           BANHO == "B" ~ "2",  #Sim, um
                           BANHO == "C" ~ "2",  #Sim, dois
                           BANHO == "D" ~ "2",  #Sim, três
                           BANHO == "E" ~ "2")) #Sim, quatro ou mais

# Questão 17 - Em sua casa trabalha empregado(a) doméstico(a) pelo menos cinco dias por semana?
ALUNO3EM <- ALUNO3EM %>% 
  mutate(EMPR = case_when(EMPR == "A" ~ "1",  #Não
                          EMPR == "B" ~ "2",  #Sim, um(a) empregado(a)
                          EMPR == "C" ~ "2",  #Sim, dois(duas) empregados(as)
                          EMPR == "D" ~ "2",  #Sim, três empregados(as)
                          EMPR == "E" ~ "2")) #Sim, quatro ou mais empregados(as)

# Questão 19 - Até que série sua mãe, ou a mulher responsável por você, estudou?
ALUNO3EM <- ALUNO3EM %>% filter(!ESC_MAE == "G") %>% 
  mutate(ESC_MAE = case_when(ESC_MAE == "A" ~ "1",  #Nunca estudou
                             ESC_MAE == "B" ~ "1",  #Não completou a 4.ª série/5.º ano
                             ESC_MAE == "C" ~ "2",  #Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano
                             ESC_MAE == "D" ~ "3",  #Completou a 8.ª série/9.º ano, mas não completou o Ensino Médio
                             ESC_MAE == "E" ~ "4",  #Completou o Ensino Médio, mas não completou a Faculdade
                             ESC_MAE == "F" ~ "5")) #Completou a Faculdade

# Questão 23 - Até que série seu pai, ou o homem responsável por você, estudou?
ALUNO3EM <- ALUNO3EM %>% filter(!ESC_PAI == "G") %>%
  mutate(ESC_PAI = case_when(ESC_PAI == "A" ~ "1",  #Nunca estudou
                             ESC_PAI == "B" ~ "1",  #Não completou a 4.ª série/5.º ano
                             ESC_PAI == "C" ~ "2",  #Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano
                             ESC_PAI == "D" ~ "3",  #Completou a 8.ª série/9.º ano, mas não completou o Ensino Médio
                             ESC_PAI == "E" ~ "4",  #Completou o Ensino Médio, mas não completou a Faculdade
                             ESC_PAI == "F" ~ "5")) #Completou a Faculdade

# Questão 48 - Você já foi reprovado?
ALUNO3EM <- ALUNO3EM %>%
  mutate(REPROV = case_when(REPROV == "A" ~ "0",  #Não
                            REPROV == "B" ~ "1",  #Sim, uma vez
                            REPROV == "C" ~ "1")) #Sim, duas vezes ou mais

# Questão 50 - Você gosta de estudar Língua Portuguesa?
ALUNO3EM <- ALUNO3EM %>%
  mutate(GOSTA_LP = case_when(GOSTA_LP == "A" ~ "1",  #Sim
                              GOSTA_LP == "B" ~ "0")) #Não

# Questão 53 - Você gosta de estudar Matemática?
ALUNO3EM <- ALUNO3EM %>%
  mutate(GOSTA_MT = case_when(GOSTA_MT == "A" ~ "1",  #Sim
                              GOSTA_MT == "B" ~ "0")) #Não

#Average grade per class
ALUNO3EM_TURMA = ALUNO3EM %>% group_by(TURMA) %>% summarise(MT = mean(MT), LP = mean(LP))
ALUNO3EM = merge(ALUNO3EM, ALUNO3EM_TURMA, by.x=c("TURMA"),  by.y=c("TURMA"), all=TRUE)
ALUNO3EM = ALUNO3EM %>% rename(LP = LP.x, LP_TURMA = LP.y, MT = MT.x, MT_TURMA = MT.y) #Renaming columns

#Creating dummies
ALUNO3EM <- cbind(ALUNO3EM, dummy(ALUNO3EM$ESC_MAE, sep = " "))
ALUNO3EM = ALUNO3EM %>%
  rename(ESC_MENOS_4_5_MAE=`ALUNO3EM 1`,ESC_MENOS_8_9_MAE=`ALUNO3EM 2`,ESC_MENOS_EM_MAE=`ALUNO3EM 3`,
         ESC_MENOS_ES_MAE=`ALUNO3EM 4`,ESC_ES_MAE=`ALUNO3EM 5`)

ALUNO3EM <- cbind(ALUNO3EM, dummy(ALUNO3EM$ESC_PAI, sep = " "))
ALUNO3EM = ALUNO3EM %>%
  rename(ESC_MENOS_4_5_PAI=`ALUNO3EM 1`,ESC_MENOS_8_9_PAI=`ALUNO3EM 2`,ESC_MENOS_EM_PAI=`ALUNO3EM 3`,
         ESC_MENOS_ES_PAI=`ALUNO3EM 4`,ESC_ES_PAI=`ALUNO3EM 5`)

ALUNO3EM <- cbind(ALUNO3EM, dummy(ALUNO3EM$REGIAO, sep = " "))
ALUNO3EM = ALUNO3EM %>%
  rename(NORTE=`ALUNO3EM 1`,NORDESTE=`ALUNO3EM 2`,SUDESTE=`ALUNO3EM 3`,
         SUL=`ALUNO3EM 4`,CENTRO_OESTE=`ALUNO3EM 5`)

ALUNO3EM <- transform(ALUNO3EM, TURMA = as.factor(TURMA), ESCOLA = as.factor(ESCOLA),
                      ALUNO = as.factor(ALUNO), REGIAO = as.factor(REGIAO), DEPENDENCIA_ADM = as.factor(DEPENDENCIA_ADM),
                      LOCALIZACAO = as.factor(LOCALIZACAO), GENERO = as.factor(GENERO), RACA=as.factor(RACA),
                      TV = as.factor(TV), GELADEIRA = as.factor(GELADEIRA), FREEZER = as.factor(FREEZER),
                      MAQ = as.factor(MAQ), CARRO = as.factor(CARRO), COMP = as.factor(COMP),
                      BANHO = as.factor(BANHO), EMPR = as.factor(EMPR), REPROV = as.factor(REPROV),
                      GOSTA_LP = as.factor(GOSTA_LP), GOSTA_MT = as.factor(GOSTA_MT),
                      ESC_MENOS_4_5_MAE = as.factor(ESC_MENOS_4_5_MAE), ESC_MENOS_8_9_MAE = as.factor(ESC_MENOS_8_9_MAE),
                      ESC_MENOS_EM_MAE = as.factor(ESC_MENOS_EM_MAE), ESC_MENOS_ES_MAE = as.factor(ESC_MENOS_ES_MAE),
                      ESC_ES_MAE = as.factor(ESC_ES_MAE), ESC_MENOS_4_5_PAI = as.factor(ESC_MENOS_4_5_PAI),
                      ESC_MENOS_8_9_PAI = as.factor(ESC_MENOS_8_9_PAI), ESC_MENOS_EM_PAI = as.factor(ESC_MENOS_EM_PAI),
                      ESC_MENOS_ES_PAI = as.factor(ESC_MENOS_ES_PAI), ESC_ES_PAI = as.factor(ESC_ES_PAI),
                      NORTE = as.factor(NORTE), NORDESTE = as.factor(NORDESTE), SUDESTE = as.factor(SUDESTE),
                      SUL = as.factor(SUL), CENTRO_OESTE = as.factor(CENTRO_OESTE))#Transforming variables into Factors

#Subset to Portuguese Language
ALUNO3EMLP = ALUNO3EM [,c(7,1,12,30,98,37,38,41,44,46:50,53,99:108,77,80,109:113,8,9)]
ALUNO3EMLP <- ALUNO3EMLP %>% filter(!is.na(LP))

#Subset to Mathematics
ALUNO3EMMT = ALUNO3EM [,c(7,1,12,34,97,37,38,41,44,46:50,53,99:108,77,88,109:113,8,9)]
ALUNO3EMMT <- ALUNO3EMMT %>% filter(!is.na(MT))

#################################################    Teachers   #####################################################
PROF <- read.csv("TS_PROFESSOR.csv", header=T, sep=",") #Loading the file

PROF <- subset(PROF, IN_PREENCHIMENTO_QUESTIONARIO == 1) #Deleting observations with blank questionnaires

PROF = PROF %>% rename(TURMA = ID_TURMA, PROFESSOR = CO_PROFESSOR, SERIE = ID_SERIE,
                       PG_PROF = TX_RESP_Q008, RENDA_PROF = TX_RESP_Q010, EXP_ESCOLA_M5 = TX_RESP_Q014,
                       DISCIPLINA = TX_RESP_Q105, IPP_G_1 = TX_RESP_Q107, IPP_G_2 = TX_RESP_Q108,
                       IPP_G_3 = TX_RESP_Q109, IPP_G_4 = TX_RESP_Q110, IPP_G_5 = TX_RESP_Q111, IPP_G_6 = TX_RESP_Q112, 
                       IPP_G_7 = TX_RESP_Q113, IPP_LP_1 = TX_RESP_Q114, IPP_LP_2 = TX_RESP_Q115, IPP_LP_3 = TX_RESP_Q116,
                       IPP_LP_4 = TX_RESP_Q117, IPP_LP_5 = TX_RESP_Q118, IPP_LP_6 = TX_RESP_Q119, IPP_MT_1 = TX_RESP_Q120, 
                       IPP_MT_2 = TX_RESP_Q121, IPP_MT_3 = TX_RESP_Q122,IPP_MT_4 = TX_RESP_Q123, IPP_MT_5 = TX_RESP_Q124, 
                       IPP_MT_6 = TX_RESP_Q125) #Columns renaming

PROF$IDENTIFICACAO <- paste(PROF$ID_ESCOLA,PROF$TURMA,PROF$PROFESSOR)
PROF$DUPLICADOS <- duplicated(PROF$IDENTIFICACAO)

# Deleting duplicate observations from teachers who answered the questionnaire more than once
DUPLICADOS <- PROF[,c(136,137)]
DUPLICADOS <- DUPLICADOS %>% filter(DUPLICADOS == TRUE)
PROF = merge(PROF, DUPLICADOS, by.x=c("IDENTIFICACAO"),  by.y=c("IDENTIFICACAO"), all=TRUE)
PROF <- PROF %>% filter(is.na(DUPLICADOS.y))
PROF <- PROF[,c(2:136)]

PROF <- PROF %>% filter(!PG_PROF == "" & !RENDA_PROF  == "" & !EXP_ESCOLA_M5 == "" &
                        !IPP_G_1 == "" & !IPP_G_2 == "" & !IPP_G_3 == "" & !IPP_G_4 == "" &
                        !IPP_G_5 == "" & !IPP_G_6 == "" & !IPP_G_7 == "") #Deleting other blanks observations

#Standardizing answers to model
# Questão 8 - Indique o curso de pós-graduação de mais alta titulação que você possui.
PROF <- PROF %>%
  mutate(PG_PROF = case_when(PG_PROF == "A" ~ "1",  #Não fiz ou não completei curso de pós-graduação
                             PG_PROF == "B" ~ "2",  #Atualização ou Aperfeiçoamento (mínimo de 180 horas)
                             PG_PROF == "C" ~ "2",  #Especialização (mínimo de 360 horas)
                             PG_PROF == "D" ~ "3",  #Mestrado
                             PG_PROF == "E" ~ "3")) #Doutorado

# Questão 10 - Como professor, qual é, aproximadamente, o seu salário bruto? (com adicionais, se houver).
PROF <- PROF %>%
  mutate(RENDA_PROF = case_when(RENDA_PROF == "A" ~ "1",  #Até R$ 937,00
                                RENDA_PROF == "B" ~ "2",  #Entre R$ 937,01 e R$ 1.405,50
                                RENDA_PROF == "C" ~ "2",  #Entre R$ 1.405,51 e R$ 1.874,00
                                RENDA_PROF == "D" ~ "2",  #Entre R$ 1.874,01 e R$ 2.342,50
                                RENDA_PROF == "E" ~ "2",  #Entre R$ 2.342,51 e R$ 2.811,00
                                RENDA_PROF == "F" ~ "3",  #Entre R$ 2.811,01 e R$ 3.279,50
                                RENDA_PROF == "G" ~ "3",  #Entre R$ 3.279,51 e R$ 3.748,00
                                RENDA_PROF == "H" ~ "4",  #Entre R$ 3.748,01 e R$ 4.685,00
                                RENDA_PROF == "I" ~ "4",  #Entre R$ 4.685,01 e R$ 6.559,00
                                RENDA_PROF == "J" ~ "5",  #Entre R$ 6.559,01 e R$ 9.370,00
                                RENDA_PROF == "K" ~ "5")) #R$ 9.370,01 ou mais

# Questão 14 - Há quantos anos você trabalha como professor nesta escola?
PROF <- PROF %>%
  mutate(EXP_ESCOLA_M5 = case_when(EXP_ESCOLA_M5 == "A" ~ "0",  #Menos de um ano
                                   EXP_ESCOLA_M5 == "B" ~ "0",  #1-2 anos
                                   EXP_ESCOLA_M5 == "C" ~ "0",  #3-5 anos
                                   EXP_ESCOLA_M5 == "D" ~ "1",  #6-10 anos
                                   EXP_ESCOLA_M5 == "E" ~ "1",  #11-15 anos
                                   EXP_ESCOLA_M5 == "F" ~ "1",  #16-20 anos
                                   EXP_ESCOLA_M5 == "G" ~ "1")) #Mais de 20 anos

# Questão 107 - Indique a frequência com que você desenvolve as seguintes práticas pedagógicas nesta turma: Propor dever de casa.
PROF$IPPG1 <- PROF$IPP_G_1
PROF <- PROF %>%
  mutate(IPP_G_1 = case_when(IPP_G_1 == "A" ~ "1",  #Nunca
                             IPP_G_1 == "B" ~ "1",  #Uma vez por ano
                             IPP_G_1 == "C" ~ "1",  #De 3 a 4 vezes ao ano
                             IPP_G_1 == "D" ~ "1",  #Mensalmente
                             IPP_G_1 == "E" ~ "2",  #Semanalmente
                             IPP_G_1 == "F" ~ "2")) #Diariamente

# Questão 108 - Indique a frequência com que você desenvolve as seguintes práticas pedagógicas nesta turma: Corrigir com os alunos o dever de casa.
PROF$IPPG2 <- PROF$IPP_G_2
PROF <- PROF %>%
  mutate(IPP_G_2 = case_when(IPP_G_2 == "A" ~ "1",  #Nunca
                             IPP_G_2 == "B" ~ "1",  #Uma vez por ano
                             IPP_G_2 == "C" ~ "1",  #De 3 a 4 vezes ao ano
                             IPP_G_2 == "D" ~ "1",  #Mensalmente
                             IPP_G_2 == "E" ~ "2",  #Semanalmente
                             IPP_G_2 == "F" ~ "2")) #Diariamente

# Questão 109 - Indique a frequência com que você desenvolve as seguintes práticas pedagógicas nesta turma: Desenvolver atividades em grupo, em sala de aula, para que os alunos busquem soluções de problemas.
PROF$IPPG3 <- PROF$IPP_G_3
PROF <- PROF %>%
  mutate(IPP_G_3 = case_when(IPP_G_3 == "A" ~ "1",  #Nunca
                             IPP_G_3 == "B" ~ "1",  #Uma vez por ano
                             IPP_G_3 == "C" ~ "1",  #De 3 a 4 vezes ao ano
                             IPP_G_3 == "D" ~ "1",  #Mensalmente
                             IPP_G_3 == "E" ~ "2",  #Semanalmente
                             IPP_G_3 == "F" ~ "2")) #Diariamente

# Questão 110 - Indique a frequência com que você desenvolve as seguintes práticas pedagógicas nesta turma: Desenvolver projetos temáticos com o objetivo de aprimorar as habilidades de trabalho em equipe.
PROF$IPPG4 <- PROF$IPP_G_4
PROF <- PROF %>% 
  mutate(IPP_G_4 = case_when(IPP_G_4 == "A" ~ "1",  #Nunca
                             IPP_G_4 == "B" ~ "1",  #Uma vez por ano
                             IPP_G_4 == "C" ~ "1",  #De 3 a 4 vezes ao ano
                             IPP_G_4 == "D" ~ "1",  #Mensalmente
                             IPP_G_4 == "E" ~ "2",  #Semanalmente
                             IPP_G_4 == "F" ~ "2")) #Diariamente

# Questão 111 - Indique a frequência com que você desenvolve as seguintes práticas pedagógicas nesta turma: Solicitar que os alunos copiem textos e atividades do livro didático ou do quadro negro (lousa).
PROF$IPPG5 <- PROF$IPP_G_5
PROF <- PROF %>% 
  mutate(IPP_G_5 = case_when(IPP_G_5 == "A" ~ "1",  #Nunca
                             IPP_G_5 == "B" ~ "1",  #Uma vez por ano
                             IPP_G_5 == "C" ~ "1",  #De 3 a 4 vezes ao ano
                             IPP_G_5 == "D" ~ "1",  #Mensalmente
                             IPP_G_5 == "E" ~ "2",  #Semanalmente
                             IPP_G_5 == "F" ~ "2")) #Diariamente

# Questão 112 - Indique a frequência com que você desenvolve as seguintes práticas pedagógicas nesta turma: Estimular os alunos a expressarem suas opiniões e a desenvolverem argumentos a partir de temas diversos.
PROF$IPPG6 <- PROF$IPP_G_6
PROF <- PROF %>% 
  mutate(IPP_G_6 = case_when(IPP_G_6 == "A" ~ "1",  #Nunca
                             IPP_G_6 == "B" ~ "1",  #Uma vez por ano
                             IPP_G_6 == "C" ~ "1",  #De 3 a 4 vezes ao ano
                             IPP_G_6 == "D" ~ "1",  #Mensalmente
                             IPP_G_6 == "E" ~ "2",  #Semanalmente
                             IPP_G_6 == "F" ~ "2")) #Diariamente

# Questão 113 - Indique a frequência com que você desenvolve as seguintes práticas pedagógicas nesta turma: Propor situações de aprendizagem que sejam familiares ou de interesse dos alunos.
PROF$IPPG7 <- PROF$IPP_G_7
PROF <- PROF %>% 
  mutate(IPP_G_7 = case_when(IPP_G_7 == "A" ~ "1",  #Nunca
                             IPP_G_7 == "B" ~ "1",  #Uma vez por ano
                             IPP_G_7 == "C" ~ "1",  #De 3 a 4 vezes ao ano
                             IPP_G_7 == "D" ~ "1",  #Mensalmente
                             IPP_G_7 == "E" ~ "2",  #Semanalmente
                             IPP_G_7 == "F" ~ "2")) #Diariamente

# Questão 114 - Indique a frequência com que você desenvolve as seguintes práticas pedagógicas nesta turma: Promover discussões a partir de textos de jornais ou revistas.
PROF$IPPLP1 <- PROF$IPP_LP_1
PROF <- PROF %>% mutate(IPP_LP_1 = case_when(IPP_LP_1 == "A" ~ "1",  #Nunca
                                                       IPP_LP_1 == "B" ~ "1",  #Uma vez por ano
                                                       IPP_LP_1 == "C" ~ "1",  #De 3 a 4 vezes ao ano
                                                       IPP_LP_1 == "D" ~ "1",  #Mensalmente
                                                       IPP_LP_1 == "E" ~ "2",  #Semanalmente
                                                       IPP_LP_1 == "F" ~ "2")) #Diariamente

# Questão 115 - Indique a frequência com que você desenvolve as seguintes práticas pedagógicas nesta turma: Propor atividades gramaticais relacionadas aos textos de jornais ou revistas.
PROF$IPPLP2 <- PROF$IPP_LP_2
PROF <- PROF %>% mutate(IPP_LP_2 = case_when(IPP_LP_2 == "A" ~ "1",  #Nunca
                                                       IPP_LP_2 == "B" ~ "1",  #Uma vez por ano
                                                       IPP_LP_2 == "C" ~ "1",  #De 3 a 4 vezes ao ano
                                                       IPP_LP_2 == "D" ~ "1",  #Mensalmente
                                                       IPP_LP_2 == "E" ~ "2",  #Semanalmente
                                                       IPP_LP_2 == "F" ~ "2")) #Diariamente

# Questão 116 - Indique a frequência com que você desenvolve as seguintes práticas pedagógicas nesta turma: Promover a leitura e discussão de contos, crônicas, poesias ou romances.
PROF$IPPLP3 <- PROF$IPP_LP_3
PROF <- PROF %>% mutate(IPP_LP_3 = case_when(IPP_LP_3 == "A" ~ "1",  #Nunca
                                                       IPP_LP_3 == "B" ~ "1",  #Uma vez por ano
                                                       IPP_LP_3 == "C" ~ "1",  #De 3 a 4 vezes ao ano
                                                       IPP_LP_3 == "D" ~ "1",  #Mensalmente
                                                       IPP_LP_3 == "E" ~ "2",  #Semanalmente
                                                       IPP_LP_3 == "F" ~ "2")) #Diariamente

# Questão 117 - Indique a frequência com que você desenvolve as seguintes práticas pedagógicas nesta turma: Utilizar contos, crônicas, poesias ou romances para exercitar aspectos da gramática.
PROF$IPPLP4 <- PROF$IPP_LP_4
PROF <- PROF %>% mutate(IPP_LP_4 = case_when(IPP_LP_4 == "A" ~ "1",  #Nunca
                                                       IPP_LP_4 == "B" ~ "1",  #Uma vez por ano
                                                       IPP_LP_4 == "C" ~ "1",  #De 3 a 4 vezes ao ano
                                                       IPP_LP_4 == "D" ~ "1",  #Mensalmente
                                                       IPP_LP_4 == "E" ~ "2",  #Semanalmente
                                                       IPP_LP_4 == "F" ~ "2")) #Diariamente

# Questão 118 - Indique a frequência com que você desenvolve as seguintes práticas pedagógicas nesta turma: Utilizar revistas em quadrinhos como instrumento de aprendizado.
PROF$IPPLP5 <- PROF$IPP_LP_5
PROF <- PROF %>% mutate(IPP_LP_5 = case_when(IPP_LP_5 == "A" ~ "1",  #Nunca
                                                       IPP_LP_5 == "B" ~ "1",  #Uma vez por ano
                                                       IPP_LP_5 == "C" ~ "1",  #De 3 a 4 vezes ao ano
                                                       IPP_LP_5 == "D" ~ "1",  #Mensalmente
                                                       IPP_LP_5 == "E" ~ "2",  #Semanalmente
                                                       IPP_LP_5 == "F" ~ "2")) #Diariamente

# Questão 119 - Indique a frequência com que você desenvolve as seguintes práticas pedagógicas nesta turma: Fixar os nomes de conceitos gramaticais e linguísticos.
PROF$IPPLP6 <- PROF$IPP_LP_6
PROF <- PROF %>% mutate(IPP_LP_6 = case_when(IPP_LP_6 == "A" ~ "1",  #Nunca
                                                       IPP_LP_6 == "B" ~ "1",  #Uma vez por ano
                                                       IPP_LP_6 == "C" ~ "1",  #De 3 a 4 vezes ao ano
                                                       IPP_LP_6 == "D" ~ "1",  #Mensalmente
                                                       IPP_LP_6 == "E" ~ "2",  #Semanalmente
                                                       IPP_LP_6 == "F" ~ "2")) #Diariamente

# Questão 120 - Indique a frequência com a que você desenvolve as seguintes práticas pedagógicas nesta turma: Fazer exercícios para fixar procedimentos e regras.
PROF$IPPMT1 <- PROF$IPP_MT_1
PROF <- PROF %>% 
  mutate(IPP_MT_1 = case_when(IPP_MT_1 == "A" ~ "1",  #Nunca
                              IPP_MT_1 == "B" ~ "1",  #Uma vez por ano
                              IPP_MT_1 == "C" ~ "1",  #De 3 a 4 vezes ao ano
                              IPP_MT_1 == "D" ~ "1",  #Mensalmente
                              IPP_MT_1 == "E" ~ "2",  #Semanalmente
                              IPP_MT_1 == "F" ~ "2")) #Diariamente

# Questão 121 - Indique a frequência com a que você desenvolve as seguintes práticas pedagógicas nesta turma: Discutir se os resultados numéricos obtidos na solução de um problema são adequados à situação apresentada.
PROF$IPPMT2 <- PROF$IPP_MT_2
PROF <- PROF %>% 
  mutate(IPP_MT_2 = case_when(IPP_MT_2 == "A" ~ "1",  #Nunca
                              IPP_MT_2 == "B" ~ "1",  #Uma vez por ano
                              IPP_MT_2 == "C" ~ "1",  #De 3 a 4 vezes ao ano
                              IPP_MT_2 == "D" ~ "1",  #Mensalmente
                              IPP_MT_2 == "E" ~ "2",  #Semanalmente
                              IPP_MT_2 == "F" ~ "2")) #Diariamente

# Questão 122 - Indique a frequência com a que você desenvolve as seguintes práticas pedagógicas nesta turma: Discutir diferentes modos para resolver problemas e cálculos.
PROF$IPPMT3 <- PROF$IPP_MT_3
PROF <- PROF %>% 
  mutate(IPP_MT_3 = case_when(IPP_MT_3 == "A" ~ "1",  #Nunca
                              IPP_MT_3 == "B" ~ "1",  #Uma vez por ano
                              IPP_MT_3 == "C" ~ "1",  #De 3 a 4 vezes ao ano
                              IPP_MT_3 == "D" ~ "1",  #Mensalmente
                              IPP_MT_3 == "E" ~ "2",  #Semanalmente
                              IPP_MT_3 == "F" ~ "2")) #Diariamente

# Questão 123 - Indique a frequência com a que você desenvolve as seguintes práticas pedagógicas nesta turma: Lidar com temas que aparecem em jornais e/ou revistas, discutindo a relação dos temas com a matemática.
PROF$IPPMT4 <- PROF$IPP_MT_4
PROF <- PROF %>%
  mutate(IPP_MT_4 = case_when(IPP_MT_4 == "A" ~ "1",  #Nunca
                              IPP_MT_4 == "B" ~ "1",  #Uma vez por ano
                              IPP_MT_4 == "C" ~ "1",  #De 3 a 4 vezes ao ano
                              IPP_MT_4 == "D" ~ "1",  #Mensalmente
                              IPP_MT_4 == "E" ~ "2",  #Semanalmente
                              IPP_MT_4 == "F" ~ "2")) #Diariamente

# Questão 124 - Indique a frequência com a que você desenvolve as seguintes práticas pedagógicas nesta turma: Fornecer esquemas/regras que permitem obter as respostas certas dos cálculos e problemas.
PROF$IPPMT5 <- PROF$IPP_MT_5
PROF <- PROF %>% 
  mutate(IPP_MT_5 = case_when(IPP_MT_5 == "A" ~ "1",  #Nunca
                              IPP_MT_5 == "B" ~ "1",  #Uma vez por ano
                              IPP_MT_5 == "C" ~ "1",  #De 3 a 4 vezes ao ano
                              IPP_MT_5 == "D" ~ "1",  #Mensalmente
                              IPP_MT_5 == "E" ~ "2",  #Semanalmente
                              IPP_MT_5 == "F" ~ "2")) #Diariamente

# Questão 125 - Indique a frequência com a que você desenvolve as seguintes práticas pedagógicas nesta turma: Experimentar diferentes ações (coletar informações, recortar, explorar, manipular etc.) para resolver problemas.
PROF$IPPMT6 <- PROF$IPP_MT_6
PROF <- PROF %>% 
  mutate(IPP_MT_6 = case_when(IPP_MT_6 == "A" ~ "1",  #Nunca
                              IPP_MT_6 == "B" ~ "1",  #Uma vez por ano
                              IPP_MT_6 == "C" ~ "1",  #De 3 a 4 vezes ao ano
                              IPP_MT_6 == "D" ~ "1",  #Mensalmente
                              IPP_MT_6 == "E" ~ "2",  #Semanalmente
                              IPP_MT_6 == "F" ~ "2")) #Diariamente

#Creating dummies
PROF <- cbind(PROF, dummy(PROF$PG_PROF, sep = " "))
PROF = PROF %>%
  rename(SEM_PG =`PROF 1`, LATO_PG =`PROF 2`, STRICTO_PG=`PROF 3`)


PROF <- cbind(PROF, dummy(PROF$RENDA_PROF, sep = " "))
PROF = PROF %>%
  rename(RENDA_ATE1_PROF=`PROF 1`,RENDA_1E3_PROF=`PROF 2`,RENDA_3E4_PROF=`PROF 3`,RENDA_4E7_PROF=
         `PROF 4`,RENDA_7_PROF=`PROF 5`)

PROF <- transform(PROF,ID_ESCOLA = as.factor(ID_ESCOLA), TURMA = as.factor(TURMA), SERIE = as.factor(SERIE),
                  PROFESSOR = as.factor(PROFESSOR), SEM_PG = as.factor(SEM_PG), LATO_PG = as.factor(LATO_PG),
                  STRICTO_PG = as.factor(STRICTO_PG), EXP_ESCOLA_M5 = as.factor(EXP_ESCOLA_M5), 
                  RENDA_ATE1_PROF = as.factor(RENDA_ATE1_PROF), RENDA_1E3_PROF = as.factor(RENDA_1E3_PROF),
                  RENDA_3E4_PROF = as.factor(RENDA_3E4_PROF), RENDA_4E7_PROF = as.factor(RENDA_4E7_PROF),
                  RENDA_7_PROF = as.factor(RENDA_7_PROF), IPP_G_1 = as.factor(IPP_G_1), IPP_G_2 = as.factor(IPP_G_2),
                  IPP_G_3 = as.factor(IPP_G_3), IPP_G_4 = as.factor(IPP_G_4), IPP_G_5 = as.factor(IPP_G_5), 
                  IPP_G_6 = as.factor(IPP_G_6), IPP_G_7 = as.factor(IPP_G_7), IPP_MT_1 = as.factor(IPP_MT_1),
                  IPP_MT_2 = as.factor(IPP_MT_2), IPP_MT_3 = as.factor(IPP_MT_3), IPP_MT_4 = as.factor(IPP_MT_4), 
                  IPP_MT_5 = as.factor(IPP_MT_5), IPP_MT_6 = as.factor(IPP_MT_6), IPP_LP_1 = as.factor(IPP_LP_1),
                  IPP_LP_2 = as.factor(IPP_LP_2), IPP_LP_3 = as.factor(IPP_LP_3), IPP_LP_4 = as.factor(IPP_LP_4), 
                  IPP_LP_5 = as.factor(IPP_LP_5), IPP_LP_6 = as.factor(IPP_LP_6)) #Transforming variables into Factors

################################### 9th year of Elementary School Mathematics Teachers #####################################
PROF9EFMT <- PROF %>% filter(SERIE == "9" & DISCIPLINA!="A" & !IPP_MT_1 == "" & 
                             !IPP_MT_2 == "" & !IPP_MT_3 == "" & !IPP_MT_4 == "" &
                               !IPP_MT_5 == "" & !IPP_MT_6 == "") #Deleting other blanks observations

PROF9EFMT1 <- PROF9EFMT

PROF9EFMT = PROF9EFMT [,c(7,8,11,155:157,24,158:162,117:123,130:135)] #Reordering variables

############################### 9th year of Elementary School Portuguese Language Teachers #################################
PROF9EFLP <- PROF %>% filter(SERIE == "9" & DISCIPLINA!="B" & !IPP_LP_1 == "" & 
                               !IPP_LP_2 == "" & !IPP_LP_3 == "" & !IPP_LP_4 == "" &
                               !IPP_LP_5 == "" & !IPP_LP_6 == "") #Deleting other blanks observations

PROF9EFLP1 <- PROF9EFLP


PROF9EFLP = PROF9EFLP [,c(7,8,11,155:157,24,158:162,117:129)] #Reordering variables

###################################### 3rd year of High School Mathematics Teachers ########################################
PROF3EMMT <- PROF %>% filter(!SERIE == "5" & !SERIE == "9" & DISCIPLINA!="A" & !IPP_MT_1 == "" & 
                               !IPP_MT_2 == "" & !IPP_MT_3 == "" & !IPP_MT_4 == "" &
                               !IPP_MT_5 == "" & !IPP_MT_6 == "") #Deleting other blanks observations

PROF3EMMT1 <- PROF3EMMT

PROF3EMMT = PROF3EMMT [,c(7,8,11,155:157,24,158:162,117:123,130:135)] #Reordering variables

################################## 3rd year of High School Portuguese Language Teachers ####################################
PROF3EMLP <- PROF %>% filter(!SERIE == "5" & !SERIE == "9" & DISCIPLINA!="B" & !IPP_LP_1 == "" & 
                               !IPP_LP_2 == "" & !IPP_LP_3 == "" & !IPP_LP_4 == "" &
                               !IPP_LP_5 == "" & !IPP_LP_6 == "") #Deleting other blanks observations

PROF3EMLP1 <- PROF3EMLP

PROF3EMLP = PROF3EMLP [,c(7,8,11,155:157,24,158:162,117:129)] #Reordering variables

###########################################      School Principal     ###############################################
DIR <- read.csv("TS_DIRETOR.csv", header=T, sep=",") #Loading the file

DIR <- subset(DIR, IN_PREENCHIMENTO_QUESTIONARIO == 1) #Deleting observations with blank questionnaires

DIR = DIR %>% rename(ESCOLA = ID_ESCOLA, EXP_DIR = TX_RESP_Q016) #Columns renaming
                                       
#Standardizing answers to model
# Questão 16 - Há quantos anos você exerce funções de direção?
DIR$EXP_DIR_M5 <- DIR$EXP_DIR
DIR <- DIR %>% filter(!EXP_DIR == "") %>% 
  mutate(EXP_DIR = case_when(EXP_DIR == "A" ~ "1",  #Menos de um ano
                             EXP_DIR == "B" ~ "2",  #1-2 anos
                             EXP_DIR == "C" ~ "3",  #3-5 anos
                             EXP_DIR == "D" ~ "4",  #6-10 anos
                             EXP_DIR == "E" ~ "5",  #11-15 anos
                             EXP_DIR == "F" ~ "5",  #16-20 anos
                             EXP_DIR == "G" ~ "5")) #Mais de 20 anos

DIR <- DIR %>% filter(!DIR$EXP_DIR_M5 == "") %>%
  mutate(EXP_DIR_M5 = case_when(EXP_DIR_M5 == "A" ~ "0",  #Menos de um ano
                                    EXP_DIR_M5 == "B" ~ "0",  #1-2 anos
                                    EXP_DIR_M5 == "C" ~ "0",  #3-5 anos
                                    EXP_DIR_M5 == "D" ~ "1",  #6-10 anos
                                    EXP_DIR_M5 == "E" ~ "1",  #11-15 anos
                                    EXP_DIR_M5 == "F" ~ "1",  #16-20 anos
                                    EXP_DIR_M5 == "G" ~ "1")) #Mais de 20 anos
#Creating dummies
DIR <- cbind(DIR, dummy(DIR$EXP_DIR, sep = " "))
DIR = DIR %>%
  rename(EXP_DIR_MENOS1 = `DIR 1`, EXP_DIR_1A2 = `DIR 2`, EXP_DIR_3A5 = `DIR 3`,
         EXP_DIR_6A10 = `DIR 4`, EXP_DIR_MAIS10 = `DIR 5`)

DIR <- transform(DIR, ESCOLA = as.factor(ESCOLA), EXP_DIR = as.factor(EXP_DIR),
                 EXP_DIR_MENOS1 = as.factor(EXP_DIR_MENOS1), EXP_DIR_1A2 = as.factor(EXP_DIR_1A2),
                 EXP_DIR_3A5 = as.factor(EXP_DIR_3A5), EXP_DIR_6A10 = as.factor(EXP_DIR_6A10),
                 EXP_DIR_MAIS10 = as.factor(EXP_DIR_MAIS10)) #Transforming variables into Factors

DIR = DIR [,c(4,119:124)] #Reordering variables

################################################      School     ####################################################
ESCOLA <- read.csv("TS_ESCOLA.csv", header=T, sep=",") #Loading the file

ESCOLA = ESCOLA %>% rename(ESCOLA = ID_ESCOLA, NSE_ESCOLA = NIVEL_SOCIO_ECONOMICO) #Columns renaming

#Standardizing answers to model
ESCOLA <- ESCOLA %>% 
  mutate(NSE_ESCOLA = case_when(NSE_ESCOLA == "Grupo 1" ~ "1",  # Muito Baixo 
                                NSE_ESCOLA == "Grupo 2" ~ "2",  #Baixo 
                                NSE_ESCOLA == "Grupo 3" ~ "3",  #Médio Baixo
                                NSE_ESCOLA == "Grupo 4" ~ "4",  #Médio 
                                NSE_ESCOLA == "Grupo 5" ~ "5",  #Médio Alto 
                                NSE_ESCOLA == "Grupo 6" ~ "6")) #Alto

ESCOLA <- ESCOLA %>% filter(!is.na(NSE_ESCOLA))

ESCOLA <- transform(ESCOLA, ESCOLA = as.factor(ESCOLA),
                    NSE_ESCOLA = as.factor(NSE_ESCOLA)) #Transforming variables into Factors

ESCOLA = ESCOLA [,c(4,10)] #Reordering variables

#################################################     GRAPHICS    #####################################################
#Joining the graphics created in the other scripts

######################################################  GRAPHIC 1  ########################################################
g1 + g2 + g3 + g4

######################################################  GRAPHIC 2  ########################################################
#IPP_G
IPP_G_9EFLP + IPP_G_9EFMT + IPP_G_3EMLP + IPP_G_3EMMT

######################################################  GRAPHIC 3  ########################################################
#IPP_DISCIPLINAS
IPP_LP_9EFLP  + IPP_MT_9EFMT + IPP_LP_3EMLP + IPP_MT_3EMMT

######################################################  GRAPHIC 4  ########################################################
interesse1 + interesse2 + interesse3 + interesse4

