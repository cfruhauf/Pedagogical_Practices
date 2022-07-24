# PEDAGOGICAL PRACTICES AND SCHOOL PERFOMANCE IN BRAZIL

## Research objectives
- Verify if the pedagogical practices used by teachers in classrooms are determinants of the quality of Brazilian education, denoted by the performance of students in the 9th grade of Elementary School and 3rd grade of High School in Portuguese Language and Mathematics in the 2017 SAEB test.
- Compare the impacts of pedagogical practices used by teachers in the classroom on student performance among students in the 9th grade of Elementary School and the 3rd grade of High School, as well as between the subjects of Portuguese Language and Mathematics.

## Data source
As a database, microdata from the National Assessment of Basic Education of 2017 obtained from the brazilian Basic Education Assessment System were used. The exam is carried out biannually in public and private schools across the country, with two assessment instruments: parametric tests applied to students and socioeconomic questionnaires answered by students, teachers and directors of the evaluated schools. The data refer to students in the 9th year of Elementary School and 3rd year of High School, their respective teachers, principals and schools.
**Download page**: [Inep: SAEB microdata](https://dados.gov.br/dataset/inep-microdados-do-saeb)

## Data organization and data cleaning (on RStudio)

Before starting the analysis I loaded the packages that would be used:

```
library(tidyverse)
library(dummies)
library(scales)
library(patchwork)
```
I also set directory that R should use, the one where I had saved the microdata on my computer:
```
setwd("~/Dissertation/Data/2017/DATABASE")
```
So, I created a data frame for each school level I would analized and for teachers, principals and schools data.
Starting with Students of 9th year of Elementary School:

- [x] 9th ES Students
- [ ] 3rd HS Students
- [ ] Teachers
- [ ] Principles
- [ ] Schools

```
ALUNO9EF <- read.csv("TS_ALUNO_9EF.csv", header=T,sep=",") # Uploading the file
```
Finally, here begins the cleaning and processing of data.

```
ALUNO9EF <- subset(ALUNO9EF, IN_PREENCHIMENTO_QUESTIONARIO == 1 & IN_PREENCHIMENTO_PROVA == 1) #Excluindo quem provas ou/e questionÃ¡rios em branco

ALUNO9EF = ALUNO9EF %>% rename(REGIAO = ID_REGIAO, ESCOLA = ID_ESCOLA, DEPENDENCIA_ADM = ID_DEPENDENCIA_ADM,
                               LOCALIZACAO = ID_LOCALIZACAO, TURMA = ID_TURMA, ALUNO = ID_ALUNO,
                               LP = PROFICIENCIA_LP_SAEB, MT = PROFICIENCIA_MT_SAEB, GENERO = TX_RESP_Q001,
                               RACA = TX_RESP_Q002, TV = TX_RESP_Q005, GELADEIRA = TX_RESP_Q008,
                               FREEZER = TX_RESP_Q010, MAQ = TX_RESP_Q011, CARRO = TX_RESP_Q012,
                               COMP = TX_RESP_Q013, BANHO = TX_RESP_Q014, EMPR = TX_RESP_Q017,
                               ESC_MAE = TX_RESP_Q019, ESC_PAI = TX_RESP_Q023, REPROV = TX_RESP_Q048,
                               GOSTA_LP = TX_RESP_Q050, GOSTA_MT = TX_RESP_Q053) #Renomeando colunas

ALUNO9EF <- ALUNO9EF %>% filter(!GENERO == "" & !RACA == "" & !TV  == "" & !GELADEIRA == "" &
                                !FREEZER == "" & !MAQ == "" & !CARRO == "" & !COMP == "" &
                                !BANHO == "" & !EMPR == "" & !ESC_MAE == "" & !ESC_PAI == "" &
                                !REPROV == "" & !GOSTA_LP == "" & !GOSTA_MT == "") #Retirando observaÃ§Ãµes em branco

# Padronizar respostas para modelo
ALUNO9EF <- ALUNO9EF %>%
  mutate(DEPENDENCIA_ADM = case_when(DEPENDENCIA_ADM == "1" ~ "0",  #Federal
                                     DEPENDENCIA_ADM == "2" ~ "0",  #Estadual
                                     DEPENDENCIA_ADM == "3" ~ "0",  #Municipal
                                     DEPENDENCIA_ADM == "4" ~ "1")) #Privado

ALUNO9EF <- ALUNO9EF %>%
  mutate(LOCALIZACAO = case_when(LOCALIZACAO == "1" ~ "1",  #Urbano
                                LOCALIZACAO == "2" ~ "0")) #Rural

# QuestÃ£o 1 - Qual Ã© o seu sexo?
ALUNO9EF <- ALUNO9EF %>% 
  mutate(GENERO = case_when(GENERO == "A" ~ "0",  #Masculino
                            GENERO == "B" ~ "1")) #Feminino

# QuestÃ£o 2 - Qual Ã© a sua cor ou raÃ§a?
ALUNO9EF <- ALUNO9EF %>% filter(!RACA == "F") %>%
  mutate(RACA = case_when(RACA == "A" ~ "1",  #Branca
                          RACA == "B" ~ "0",  #Preta
                          RACA == "C" ~ "0",  #Parda
                          RACA == "D" ~ "1",  #Amarelo
                          RACA == "E" ~ "0")) #IndÃ­gena

# QuestÃ£o 5 - Na sua casa tem televisÃ£o em cores?
ALUNO9EF <- ALUNO9EF %>%
  mutate(TV = case_when(TV == "A" ~ "1",  #NÃ£o tem
                        TV == "B" ~ "2",  #Sim, uma
                        TV == "C" ~ "2",  #Sim, duas
                        TV == "D" ~ "2",  #Sim, trÃªs
                        TV == "E" ~ "2")) #Sim, quatro ou mais

# QuestÃ£o 8 - Na sua casa tem geladeira?
ALUNO9EF <- ALUNO9EF %>%
  mutate(GELADEIRA = case_when(GELADEIRA == "A" ~ "1",  #NÃ£o tem
                               GELADEIRA == "B" ~ "2",  #Sim, uma
                               GELADEIRA == "C" ~ "2",  #Sim, duas
                               GELADEIRA == "D" ~ "2",  #Sim, trÃªs
                               GELADEIRA == "E" ~ "2")) #Sim, quatro ou mais

# QuestÃ£o 9 - Na sua casa tem freezer (parte da geladeira duplex)?
ALUNO9EF <- ALUNO9EF %>%
  mutate(FREEZER = case_when(FREEZER == "A" ~ "1",  #NÃ£o tem
                             FREEZER == "B" ~ "2",  #Sim, um
                             FREEZER == "C" ~ "2",  #Sim, dois
                             FREEZER == "D" ~ "2",  #Sim, trÃªs
                             FREEZER == "E" ~ "2")) #Sim, quatro ou mais

# QuestÃ£o 11 - Na sua casa tem mÃ¡quina de lavar roupa (O tanquinho NÃO deve ser considerado)?
ALUNO9EF <- ALUNO9EF %>%
  mutate(MAQ = case_when(MAQ == "A" ~ "1",  #NÃ£o tem
                         MAQ == "B" ~ "2",  #Sim, uma
                         MAQ == "C" ~ "2",  #Sim, duas
                         MAQ == "D" ~ "2",  #Sim, trÃªs
                         MAQ == "E" ~ "2")) #Sim, quatro ou mais

# QuestÃ£o 12 - Na sua casa tem carro?
ALUNO9EF <- ALUNO9EF %>%
  mutate(CARRO = case_when(CARRO == "A" ~ "1",  #NÃ£o tem
                           CARRO == "B" ~ "2",  #Sim, um
                           CARRO == "C" ~ "2",  #Sim, dois
                           CARRO == "D" ~ "2",  #Sim, trÃªs
                           CARRO == "E" ~ "2")) #Sim, quatro ou mais

# QuestÃ£o 13 - Na sua casa tem computador?
ALUNO9EF <- ALUNO9EF %>%
  mutate(COMP = case_when(COMP == "A" ~ "1",  #NÃ£o tem
                          COMP == "B" ~ "2",  #Sim, um
                          COMP == "C" ~ "2",  #Sim, dois
                          COMP == "D" ~ "2",  #Sim, trÃªs
                          COMP == "E" ~ "2")) #Sim, quatro ou mais

# QuestÃ£o 14 - Na sua casa tem banheiro?
ALUNO9EF <- ALUNO9EF %>%
  mutate(BANHO = case_when(BANHO == "A" ~ "1",  #NÃ£o tem
                           BANHO == "B" ~ "2",  #Sim, um
                           BANHO == "C" ~ "2",  #Sim, dois
                           BANHO == "D" ~ "2",  #Sim, trÃªs
                           BANHO == "E" ~ "2")) #Sim, quatro ou mais

# QuestÃ£o 17 - Em sua casa trabalha empregado(a) domÃ©stico(a) pelo menos cinco dias por semana?
ALUNO9EF <- ALUNO9EF %>%
  mutate(EMPR = case_when(EMPR == "A" ~ "1",  #NÃ£o
                          EMPR == "B" ~ "2",  #Sim, um(a) empregado(a)
                          EMPR == "C" ~ "2",  #Sim, dois(duas) empregados(as)
                          EMPR == "D" ~ "2",  #Sim, trÃªs empregados(as)
                          EMPR == "E" ~ "2")) #Sim, quatro ou mais empregados(as)

# QuestÃ£o 19 - AtÃ© que sÃ©rie sua mÃ£e, ou a mulher responsÃ¡vel por vocÃª, estudou?
ALUNO9EF <- ALUNO9EF %>% filter(!ESC_MAE == "G") %>%
  mutate(ESC_MAE = case_when(ESC_MAE == "A" ~ "1",  #Nunca estudou
                             ESC_MAE == "B" ~ "1",  #NÃ£o completou a 4.Âª sÃ©rie/5.Âº ano
                             ESC_MAE == "C" ~ "2",  #Completou a 4.Âª sÃ©rie/5.Âº ano, mas nÃ£o completou a 8.Âª sÃ©rie/9.Âº ano
                             ESC_MAE == "D" ~ "3",  #Completou a 8.Âª sÃ©rie/9.Âº ano, mas nÃ£o completou o Ensino MÃ©dio
                             ESC_MAE == "E" ~ "4",  #Completou o Ensino MÃ©dio, mas nÃ£o completou a Faculdade
                             ESC_MAE == "F" ~ "5")) #Completou a Faculdade

# QuestÃ£o 23 - AtÃ© que sÃ©rie seu pai, ou o homem responsÃ¡vel por vocÃª, estudou?
ALUNO9EF <- ALUNO9EF %>% filter(!ESC_PAI == "G") %>%
  mutate(ESC_PAI = case_when(ESC_PAI == "A" ~ "1",  #Nunca estudou
                             ESC_PAI == "B" ~ "1",  #NÃ£o completou a 4.Âª sÃ©rie/5.Âº ano
                             ESC_PAI == "C" ~ "2",  #Completou a 4.Âª sÃ©rie/5.Âº ano, mas nÃ£o completou a 8.Âª sÃ©rie/9.Âº ano
                             ESC_PAI == "D" ~ "3",  #Completou a 8.Âª sÃ©rie/9.Âº ano, mas nÃ£o completou o Ensino MÃ©dio
                             ESC_PAI == "E" ~ "4",  #Completou o Ensino MÃ©dio, mas nÃ£o completou a Faculdade
                             ESC_PAI == "F" ~ "5")) #Completou a Faculdade

# QuestÃ£o 48 - VocÃª jÃ¡ foi reprovado?
ALUNO9EF <- ALUNO9EF %>%
  mutate(REPROV = case_when(REPROV == "A" ~ "0",  #NÃ£o
                            REPROV == "B" ~ "1",  #Sim, uma vez
                            REPROV == "C" ~ "1")) #Sim, duas vezes ou mais

# QuestÃ£o 50 - VocÃª gosta de estudar LÃ­ngua Portuguesa?
ALUNO9EF <- ALUNO9EF %>%
  mutate(GOSTA_LP = case_when(GOSTA_LP == "A" ~ "1",  #Sim
                              GOSTA_LP == "B" ~ "0")) #NÃ£o

# QuestÃ£o 53 - VocÃª gosta de estudar MatemÃ¡tica?
ALUNO9EF <- ALUNO9EF %>%
  mutate(GOSTA_MT = case_when(GOSTA_MT == "A" ~ "1",  #Sim
                              GOSTA_MT == "B" ~ "0")) #NÃ£o

# Nota mÃ©dia por turma
ALUNO9EF_TURMA = ALUNO9EF %>% group_by(TURMA) %>% summarise(MT = mean(MT), LP = mean(LP))
ALUNO9EF = merge(ALUNO9EF, ALUNO9EF_TURMA, by.x=c("TURMA"),  by.y=c("TURM
```
