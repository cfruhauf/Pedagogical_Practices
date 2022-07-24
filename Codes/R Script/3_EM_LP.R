###########################################################################################################################
###################################################      Agrupar     ######################################################
###########################################################################################################################
################################### Aluno 3 EM + Professor Língua Portuguesa + Diretor ####################################
df3EMLP = merge(ALUNO3EMLP, PROF3EMLP, by.x=c("TURMA"),  by.y=c("TURMA"), all=FALSE)
df3EMLP = merge(df3EMLP, DIR, by.x=c("ESCOLA"),  by.y=c("ESCOLA"), all=FALSE)
df3EMLP = merge(df3EMLP, ESCOLA, by.x=c("ESCOLA"),  by.y=c("ESCOLA"), all=FALSE)

########################################### Variáveis abertas para estatísticas ###########################################
df3EMLP_ESTAT = merge(ALUNO3EMLP, PROF3EMLP1, by.x=c("TURMA"),  by.y=c("TURMA"), all=FALSE)
df3EMLP_ESTAT = merge(df3EMLP_ESTAT, DIR, by.x=c("ESCOLA"),  by.y=c("ESCOLA"), all=FALSE)
df3EMLP_ESTAT = merge(df3EMLP_ESTAT, ESCOLA, by.x=c("ESCOLA"),  by.y=c("ESCOLA"), all=FALSE)

###########################################################################################################################
################################################      Salvar dataset     ##################################################
###########################################################################################################################
write.csv(df3EMLP,file="df43EM_LP_final2.csv",sep=";",row.names=FALSE,col.names=TRUE)

###########################################################################################################################
#####################################     Carregar arquivos com Índices    ################################################
###########################################################################################################################
df3EMLP2 <- read.csv("df3EM_LP_final_completo.csv", header=T,sep=",")

df3EMLP2 %>% filter(SEM_PG == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df3EMLP2 %>% filter(LATO_PG == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df3EMLP2 %>% filter(STRICTO_PG == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df3EMLP2 %>% filter(EXP_ESCOLA_M5 == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df3EMLP2 %>% filter(EXP_ESCOLA_M5 == 0) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df3EMLP2 %>% filter(RENDA_ATE1_PROF == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df3EMLP2 %>% filter(RENDA_1E3_PROF == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df3EMLP2 %>% filter(RENDA_3E4_PROF == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df3EMLP2 %>% filter(RENDA_4E7_PROF == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df3EMLP2 %>% filter(RENDA_7_PROF == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df3EMLP2 %>% filter(EXP_DIR_M5 == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df3EMLP2 %>% filter(EXP_DIR_M5 == 0) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df3EMLP2 %>% filter(LOCALIZACAO == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df3EMLP2 %>% filter(LOCALIZACAO == 0) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df3EMLP2 %>% filter(DEPENDENCIA_ADM == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df3EMLP2 %>% filter(DEPENDENCIA_ADM == 0) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

###########################################################################################################################
#################################################     Estatísticas    #####################################################
###########################################################################################################################

######################################################  GRÁFICO 1  ########################################################
colnames(df3EMLP2)[4] = 'Proficiencia'
df3EMLP_ESTAT1 <- df3EMLP2[,c(4,70,72)]

df3EMLP_ESTAT1 <- df3EMLP_ESTAT1 %>%
  pivot_longer(
    cols = c(IPP_G_I, IPP_LP_I),
    names_to = "indice",
    values_to = "valor"
  )

df3EMLP_ESTAT1$Ano_Disciplina <- "3º ano Ensino Médio - Língua Portuguesa"
df3EMLP_ESTAT1$Ano <- "3º ano Ensino Médio"
df3EMLP_ESTAT1$Disciplina <- "Língua Portuguesa"

g3 <- df3EMLP_ESTAT1 %>%
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
        legend.text = element_text(size = 15),
        legend.position="bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.title = element_text(size = 15),
        plot.title = element_text(size = 15, hjust = 0.5) ) +
  scale_x_continuous(name ="Escala",breaks=c(0:10)) +
  scale_fill_manual(name = "Índices", values = c("lightcoral", "lightblue")) +
  labs(y = "Percentual (%)", title = "3º ano Ensino Médio - Língua Portuguesa")

######################################################  GRÁFICO 2  ########################################################
df3EMLP_ESTAT2 <- df3EMLP_ESTAT[,c(169:175)]

df3EMLP_ESTAT2 <- df3EMLP_ESTAT2 %>%
  pivot_longer(
    cols = c(IPPG1,IPPG2,IPPG3,IPPG4,IPPG5,IPPG6,IPPG7),
    names_to = "pratica",
    values_to = "frequencia"
  )

IPP_G_3EMLP <- df3EMLP_ESTAT2 %>%
  ggplot(aes(x = fct_rev(fct_infreq(pratica)), fill = frequencia)) +
  geom_bar((aes(y = (..count..)/sum(..count..))),position = "fill",show.legend = FALSE)+
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
        plot.title = element_text(size = 15, hjust = 0.5)) +
  labs(title = "3º ano Ensino Médio - Língua Portuguesa")

######################################################  GRÁFICO 3  ########################################################
df3EMLP_ESTAT3 <- df3EMLP_ESTAT[,c(176:181)]

df3EMLP_ESTAT3 <- df3EMLP_ESTAT3 %>%
  pivot_longer(
    cols = c(IPPLP1,IPPLP2,IPPLP3,IPPLP4,IPPLP5,IPPLP6),
    names_to = "pratica",
    values_to = "frequencia"
  )

IPP_LP_3EMLP <- df3EMLP_ESTAT3 %>%
  ggplot(aes(x = fct_rev(fct_infreq(pratica)), fill = frequencia)) +
  geom_bar((aes(y = (..count..)/sum(..count..))),position = "fill",show.legend = FALSE)+
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
        plot.title = element_text(size = 15, hjust = 0.5)) +
  labs(title = "3º ano Ensino Médio - Língua Portuguesa")


######################################################  GRÁFICO 4  ########################################################
df3EMLP_ESTAT4 <- df3EMLP_ESTAT[,c(6,27)]

df3EMLP_ESTAT4 <- df3EMLP_ESTAT4 %>% 
  mutate(GENERO = case_when(GENERO == "0" ~ "Masculino",  #Masculino
                            GENERO == "1" ~ "Feminino")) #Feminino

df3EMLP_ESTAT4$GENERO <- as.factor(df3EMLP_ESTAT4$GENERO)

df3EMLP_ESTAT4 <- df3EMLP_ESTAT4 %>%
  mutate(GOSTA_LP = case_when(GOSTA_LP == "1" ~ "Sim",  #Sim
                              GOSTA_LP == "0" ~ "Não")) #Não

interesse3 <- df3EMLP_ESTAT4 %>% ggplot(aes(fill = GOSTA_LP, y = (..count..)/sum(..count..), x = GENERO)) + 
  geom_bar(position="dodge", show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent)+
  coord_cartesian(ylim = c(0, 0.50))+
  scale_fill_manual(values = c("coral1", "aquamarine"))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "gray94"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(size = 15, hjust = 0.5))+
  labs(x = "Gênero",y = "Percentual (%)", title = "3º ano Ensino Médio - Língua Portuguesa")

