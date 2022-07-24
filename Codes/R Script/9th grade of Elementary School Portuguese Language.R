###########################################################################################################################
####################################################      Merge     #######################################################
###########################################################################################################################
########################################### Variables to estimate the model ###############################################
df9EFLP = merge(ALUNO9EFLP, PROF9EFLP, by.x=c("TURMA"),  by.y=c("TURMA"), all=FALSE)
df9EFLP = merge(df9EFLP, DIR, by.x=c("ESCOLA"),  by.y=c("ESCOLA"), all=FALSE)
df9EFLP = merge(df9EFLP, ESCOLA, by.x=c("ESCOLA"),  by.y=c("ESCOLA"), all=FALSE)

############################################ Variables to calculate statistics ############################################
df9EFLP_ESTAT = merge(ALUNO9EFLP, PROF9EFLP1, by.x=c("TURMA"),  by.y=c("TURMA"), all=FALSE)
df9EFLP_ESTAT = merge(df9EFLP_ESTAT, DIR, by.x=c("ESCOLA"),  by.y=c("ESCOLA"), all=FALSE)
df9EFLP_ESTAT = merge(df9EFLP_ESTAT, ESCOLA, by.x=c("ESCOLA"),  by.y=c("ESCOLA"), all=FALSE)

###########################################################################################################################
################################################      Saving dataset     ##################################################
###########################################################################################################################
write.csv(df9EFLP,file="df49EF_LP_final2.csv",sep=";",row.names=FALSE,col.names=TRUE)

###########################################################################################################################
#####################################     Loading files with the Indexes    ###############################################
###########################################################################################################################
df9EFLP2 <- read.csv("df9EF_LP_final_completo.csv", header=T,sep=",")

###########################################################################################################################
##################################################     Statistics    ######################################################
###########################################################################################################################

df9EFLP2 %>% filter(SEM_PG == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df9EFLP2 %>% filter(LATO_PG == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df9EFLP2 %>% filter(STRICTO_PG == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df9EFLP2 %>% filter(EXP_ESCOLA_M5 == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df9EFLP2 %>% filter(EXP_ESCOLA_M5 == 0) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df9EFLP2 %>% filter(RENDA_ATE1_PROF == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df9EFLP2 %>% filter(RENDA_1E3_PROF == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df9EFLP2 %>% filter(RENDA_3E4_PROF == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df9EFLP2 %>% filter(RENDA_4E7_PROF == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df9EFLP2 %>% filter(RENDA_7_PROF == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df9EFLP2 %>% filter(EXP_DIR_M5 == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df9EFLP2 %>% filter(EXP_DIR_M5 == 0) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df9EFLP2 %>% filter(LOCALIZACAO == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df9EFLP2 %>% filter(LOCALIZACAO == 0) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df9EFLP2 %>% filter(DEPENDENCIA_ADM == 1) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )

df9EFLP2 %>% filter(DEPENDENCIA_ADM == 0) %>% summarise(IPP_G_I = mean(IPP_G_I),IPP_LP_I = mean(IPP_LP_I) )



######################################################  GRAPHIC 1  ########################################################
colnames(df9EFLP2)[4] = 'Proficiencia'
df9EFLP_ESTAT1 <- df9EFLP2[,c(4,70,72)]

df9EFLP_ESTAT1 <- df9EFLP_ESTAT1 %>%
  pivot_longer(
    cols = c(IPP_G_I, IPP_LP_I),
    names_to = "indice",
    values_to = "valor"
  )

df9EFLP_ESTAT1$Ano_Disciplina <- "9º ano Ensino Fundamental - Língua Portuguesa"
df9EFLP_ESTAT1$Ano<- "9º ano Ensino Fundamental"
df9EFLP_ESTAT1$Disciplina <- "Língua Portuguesa"

g1 <- df9EFLP_ESTAT1 %>%
  ggplot(aes(x = valor, fill= indice)) +
  geom_histogram(aes(y = stat(count) / sum(count)), position = "dodge2", bins = 11, show.legend = FALSE)+
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0, 0.25)) + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "gray94"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold", size = 15),
        axis.title = element_text (margin (t = 0, r = 0, b = 1, l = 0, unit = "pt"), face = "bold", size = 15),
        axis.text = element_text(size = 15),
        axis.title.x = element_blank(),
        legend.position="bottom",
        legend.margin = margin(-20, 0, 0, 0),
        plot.title = element_text(size = 15, hjust = 0.5) ) +
  scale_x_continuous(name ="Escala",breaks=c(0:10)) +
  scale_fill_manual(name = "Índices", values = c("lightcoral","lightblue")) +
  labs(y = "Percentual (%)", title = "9º ano Ensino Fundamental - Língua Portuguesa")

######################################################  GRAPHIC 2  ########################################################
df9EFLP_ESTAT2 <- df9EFLP_ESTAT[,c(169:175)]

df9EFLP_ESTAT2 <- df9EFLP_ESTAT2 %>%
  pivot_longer(
    cols = c(IPPG1,IPPG2,IPPG3,IPPG4,IPPG5,IPPG6,IPPG7),
    names_to = "pratica",
    values_to = "frequencia"
  )

IPP_G_9EFLP <- df9EFLP_ESTAT2 %>%
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
  labs(title = "9º ano Ensino Fundamental - Língua Portuguesa")

######################################################  GRAPHIC 3  ########################################################
df9EFLP_ESTAT3 <- df9EFLP_ESTAT[,c(176:181)]

df9EFLP_ESTAT3 <- df9EFLP_ESTAT3 %>%
  pivot_longer(
    cols = c(IPPLP1,IPPLP2,IPPLP3,IPPLP4,IPPLP5,IPPLP6),
    names_to = "pratica",
    values_to = "frequencia"
  )

IPP_LP_9EFLP <- df9EFLP_ESTAT3 %>%
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
  labs(title = "9º ano Ensino Fundamental - Língua Portuguesa")

######################################################  GRAPHIC 4  ########################################################
df9EFLP_ESTAT4 <- df9EFLP_ESTAT[,c(6,27)]

df9EFLP_ESTAT4 <- df9EFLP_ESTAT4 %>% 
  mutate(GENERO = case_when(GENERO == "0" ~ "Masculino",  #Masculino
                            GENERO == "1" ~ "Feminino")) #Feminino

df9EFLP_ESTAT4$GENERO <- as.factor(df9EFLP_ESTAT4$GENERO)

df9EFLP_ESTAT4 <- df9EFLP_ESTAT4 %>%
  mutate(GOSTA_LP = case_when(GOSTA_LP == "1" ~ "Sim",  #Sim
                              GOSTA_LP == "0" ~ "Não")) #Não

interesse1 <- df9EFLP_ESTAT4 %>% ggplot(aes(fill = GOSTA_LP, y = (..count..)/sum(..count..), x = GENERO)) + 
  geom_bar(position="dodge",show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0, 0.50))+
  scale_fill_manual(values = c("coral1", "aquamarine"))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "gray94"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(size = 15, hjust = 0.5))+
  labs(y = "Percentual (%)", title = "9º ano Ensino Fundamental - Língua Portuguesa")
  
