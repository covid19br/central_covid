library(readr)
library(dplyr)
library(ggplot2)
library(zoo)

testes_sp<-read.csv("./Área de Trabalho/central_covid/scripts_R_genericos/20201116_testes-RTPCR.csv", sep = ";")

postivo<-testes_sp %>% 
  filter(Resultado == "POSITIVO", 
         Município == "SAO PAULO") %>% 
  mutate(Date_Coleta = as.Date(Dt.Coleta, "%d/%m/%Y")) %>% 
  # filter(Date_Coleta >= "01/10/2020") %>% 
  group_by(Date_Coleta) %>% 
  summarise(N = n()) %>% 
  as.data.frame()
negativo<-testes_sp %>% 
  filter(Resultado == "NEGATIVO", 
         Município == "SAO PAULO") %>% 
  mutate(Date_Coleta = as.Date(Dt.Coleta, "%d/%m/%Y")) %>% 
  # filter(Date_Coleta >= "01/10/2020") %>% 
  group_by(Date_Coleta) %>% 
  summarise(N = n()) %>% 
  as.data.frame()
prop_pos<-left_join(postivo, negativo, by = "Date_Coleta")

prop_pos<-testes_sp %>% 
  filter(Município == "SAO PAULO",
         Resultado != "INCONCLUSIVO") %>%
  mutate(Date_Coleta = as.Date(Dt.Coleta, "%d/%m/%Y")) %>% 
  group_by(Date_Coleta) %>% 
  summarise(N = n(), 
            Positivo = sum(Resultado == "POSITIVO"),
            Negativo = sum(Resultado == "NEGATIVO")) %>% 
  mutate(Positivity = Positivo/(Positivo + Negativo)) %>%
  as.data.frame()
prop_pos_zoo<-zoo(prop_pos$Positivity, as.Date(prop_pos$Date_Coleta))
prop_pos_mean7<-rollmean(prop_pos_zoo, 7)

postivo <- postivo %>% 
  filter(Date_Coleta >=  "2020-10-01") %>% 
  as.data.frame()
negativo<-negativo %>% 
  filter(Date_Coleta >=  "2020-10-01") %>% 
  as.data.frame()

p.plot<-
  ggplot(data = postivo, aes(x = Date_Coleta, y= N, col = "Positivo"))+
  geom_line()+
  geom_line(data = negativo, aes(x = Date_Coleta, y = N, col = "Negativo"))+
  theme_bw()+
  labs(x = "Data Coleta",
       y = "",
       title = "Positividade Município de São Paulo - RT-PCR")
p.plot
               
p.plot.postivo<-
  ggplot(data = prop_pos, aes(x = Date_Coleta, y = Positivity))+
  geom_line()+
  geom_line(prop_pos_mean7, aes(y=V1))+
  theme_bw()+
  labs(x = "Data Coleta",
       y = "",
       title = "Positividade Município de São Paulo - RT-PCR")
p.plot.postivo

plot.zoo(prop_pos_mean7, xlab = "Data Coleta", ylab = "Positivity", main = "Positividade Município de São Paulo - RT-PCR Mean7")
lines(prop_pos_mean7)
