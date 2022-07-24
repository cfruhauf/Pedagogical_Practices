###########################################################################################################################
###################################################      Agrupar     ######################################################
###########################################################################################################################
###################################### Aluno 3 EM + Professor Matemática + Diretor ########################################
df3EMMT = merge(ALUNO3EMMT, PROF3EMMT, by.x=c("TURMA"),  by.y=c("TURMA"), all=FALSE)
df3EMMT = merge(df3EMMT, DIR, by.x=c("ESCOLA"),  by.y=c("ESCOLA"), all=FALSE)
df3EMMT = merge(df3EMMT, ESCOLA, by.x=c("ESCOLA"),  by.y=c("ESCOLA"), all=FALSE)

########################################### Variáveis abertas para estatísticas ###########################################
df3EMMT_ESTAT = merge(ALUNO3EMMT, PROF3EMMT1, by.x=c("TURMA"),  by.y=c("TURMA"), all=FALSE)
df3EMMT_ESTAT = merge(df3EMMT_ESTAT, DIR, by.x=c("ESCOLA"),  by.y=c("ESCOLA"), all=FALSE)
df3EMMT_ESTAT = merge(df3EMMT_ESTAT, ESCOLA, by.x=c("ESCOLA"),  by.y=c("ESCOLA"), all=FALSE)

###########################################################################################################################
################################################      Salvar dataset     ##################################################
###########################################################################################################################
write.csv(df3EMMT,file="df43EM_MT_final2.csv",sep=";",row.names=FALSE,col.names=TRUE)

###########################################################################################################################
#####################################     Carregar arquivos com Índices    ################################################
###########################################################################################################################
df3EMMT2 <- read.csv("df3EM_MT_final_completo.csv", header=T,sep=",")

df3EMMT2 %>% filter(SEM_PG == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_MT_I = mean(IPP_MT_I) )

df3EMMT2 %>% filter(LATO_PG == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_MT_I = mean(IPP_MT_I) )

df3EMMT2 %>% filter(STRICTO_PG == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_MT_I = mean(IPP_MT_I) )

df3EMMT2 %>% filter(EXP_ESCOLA_M5 == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_MT_I = mean(IPP_MT_I) )

df3EMMT2 %>% filter(EXP_ESCOLA_M5 == 0) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_MT_I = mean(IPP_MT_I) )

df3EMMT2 %>% filter(RENDA_ATE1_PROF == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_MT_I = mean(IPP_MT_I) )

df3EMMT2 %>% filter(RENDA_1E3_PROF == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_MT_I = mean(IPP_MT_I) )

df3EMMT2 %>% filter(RENDA_3E4_PROF == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_MT_I = mean(IPP_MT_I) )

df3EMMT2 %>% filter(RENDA_4E7_PROF == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_MT_I = mean(IPP_MT_I) )

df3EMMT2 %>% filter(RENDA_7_PROF == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_MT_I = mean(IPP_MT_I) )

df3EMMT2 %>% filter(EXP_DIR_M5 == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_MT_I = mean(IPP_MT_I) )

df3EMMT2 %>% filter(EXP_DIR_M5 == 0) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_MT_I = mean(IPP_MT_I) )

df3EMMT2 %>% filter(LOCALIZACAO == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_MT_I = mean(IPP_MT_I) )

df3EMMT2 %>% filter(LOCALIZACAO == 0) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_MT_I = mean(IPP_MT_I) )

df3EMMT2 %>% filter(DEPENDENCIA_ADM == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_MT_I = mean(IPP_MT_I) )

df3EMMT2 %>% filter(DEPENDENCIA_ADM == 0) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_MT_I = mean(IPP_MT_I) )

###########################################################################################################################
#################################################     Estatísticas    #####################################################
###########################################################################################################################

######################################################  GRÁFICO 1  ########################################################
colnames(df3EMMT2)[4] = 'Proficiencia'
df3EMMT_ESTAT1 <- df3EMMT2[,c(4,70,72)]

df3EMMT_ESTAT1 <- df3EMMT_ESTAT1 %>%
  pivot_longer(
    cols = c(IPP_G_I, IPP_MT_I),
    names_to = "indice",
    values_to = "valor"
  )

df3EMMT_ESTAT1$Ano_Disciplina <- "3º ano Ensino Médio - Matemática"
df3EMMT_ESTAT1$Ano <- "3º ano Ensino Médio"
df3EMMT_ESTAT1$Disciplina <- "Matemática"

g4 <- df3EMMT_ESTAT1 %>%
  ggplot(aes(x = valor, fill= indice)) +
  geom_histogram(aes(y = stat(count) / sum(count)), position = "dodge2", bins = 11)+
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0, 0.25)) + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "gray94"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold", size = 15),
        axis.title = element_text (margin (t = 0, r = 0, b = 1, l = 0, unit = "pt"), face = "bold", size = 15),
        axis.text = element_text(size = 15),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 15),
        legend.position="bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.title = element_text(size = 15),
        plot.title = element_text(size = 15, hjust = 0.5) ) +
  scale_x_continuous(name ="Escala",breaks=c(0:10)) +
  scale_fill_manual(name = "Índices", values = c("lightcoral", "lightgreen","lightblue")) +
  labs(y = "Percentual (%)", title = "3º ano Ensino Médio - Matemática")

######################################################  GRÁFICO 2  ########################################################
df3EMMT_ESTAT2 <- df3EMMT_ESTAT[,c(169:175)]

df3EMMT_ESTAT2 <- df3EMMT_ESTAT2 %>%
  pivot_longer(
    cols = c(IPPG1,IPPG2,IPPG3,IPPG4,IPPG5,IPPG6,IPPG7),
    names_to = "pratica",
    values_to = "frequencia"
  )

IPP_G_3EMMT <- df3EMMT_ESTAT2 %>%
  ggplot(aes(x = fct_rev(fct_infreq(pratica)), fill = frequencia)) +
  geom_bar((aes(y = (..count..)/sum(..count..))),position = "fill")+
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  scale_fill_manual(name = "Frequência", values = c("red2", "orangered",
                                                    "darkorange","goldenrod1", 
                                                    "yellow", "yellowgreen"),
                    breaks = c("A","B","C","D","E","F"),
                    labels = c("Nunca","Uma vez por ano","De 3 a 4 vezes ao ano","Mensalmente","Semanalmente","Diariamente"))+
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.position="bottom",
        plot.title = element_text(size = 15, hjust = 0.5)) +
  labs(title = "3º ano Ensino Médio - Matemática")

######################################################  GRÁFICO 3  ########################################################
df3EMMT_ESTAT3 <- df3EMMT_ESTAT[,c(182:187)]

df3EMMT_ESTAT3 <- df3EMMT_ESTAT3 %>%
  pivot_longer(
    cols = c(IPPMT1,IPPMT2,IPPMT3,IPPMT4,IPPMT5,IPPMT6),
    names_to = "pratica",
    values_to = "frequencia"
  )

IPP_MT_3EMMT <- df3EMMT_ESTAT3 %>%
  ggplot(aes(x = fct_rev(fct_infreq(pratica)), fill = frequencia)) +
  geom_bar((aes(y = (..count..)/sum(..count..))),position = "fill")+
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  scale_fill_manual(name = "Frequência", values = c("red2", "orangered",
                                                    "darkorange","goldenrod1", 
                                                    "yellow", "yellowgreen"),
                    breaks = c("A","B","C","D","E","F"),
                    labels = c("Nunca","Uma vez por ano","De 3 a 4 vezes ao ano","Mensalmente","Semanalmente","Diariamente"))+
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.position="bottom",
        plot.title = element_text(size = 15, hjust = 0.5)) +
  labs(title = "3º ano Ensino Médio - Matemática")

######################################################  GRÁFICO 4  ########################################################
df3EMMT_ESTAT4 <- df3EMMT_ESTAT[,c(6,27)]

df3EMMT_ESTAT4 <- df3EMMT_ESTAT4 %>% 
  mutate(GENERO = case_when(GENERO == "0" ~ "Masculino",  #Masculino
                            GENERO == "1" ~ "Feminino")) #Feminino

df3EMMT_ESTAT4$GENERO <- as.factor(df3EMMT_ESTAT4$GENERO)

df3EMMT_ESTAT4 <- df3EMMT_ESTAT4 %>%
  mutate(GOSTA_MT = case_when(GOSTA_MT == "1" ~ "Sim",  #Sim
                              GOSTA_MT == "0" ~ "Não")) #Não

interesse4 <- df3EMMT_ESTAT4 %>% ggplot(aes(fill = GOSTA_MT, y = (..count..)/sum(..count..), x = GENERO)) + 
  geom_bar(position="dodge") +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0, 0.50))+
  scale_fill_manual(name = "Gosta de estudar a disciplina ?", values = c("coral1", "aquamarine"))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "gray94"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.position="bottom",
        plot.title = element_text(size = 15, hjust = 0.5))+
  labs(x = "Gênero",y = "Percentual (%)", title = "3º ano Ensino Médio - Matemática")
