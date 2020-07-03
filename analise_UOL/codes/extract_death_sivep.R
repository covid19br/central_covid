
sivep_30<-read_sivep_generica(file.name = './dados/SIVEP-Gripe/SRAGHospitalizado_2020_06_30.zip')
sivep_23<-read_sivep_generica(file.name = './dados/SIVEP-Gripe/SRAGHospitalizado_2020_06_23.zip')
sivep_17<-read_sivep_generica(file.name = './dados/SIVEP-Gripe/SRAGHospitalizado_2020_06_17.zip')
sivep_09<-read_sivep_generica(file.name = './dados/SIVEP-Gripe/SRAGHospitalizado_2020_06_09.zip')

## compilando e salvando em csv ##
sivep_30_obitos<-sivep_30 %>%
  filter(pcr_sars2 == 1 | classi_fin == 5)%>%
  as.data.frame()
sivep_30_obitos<-sivep_30_obitos%>%
  filter(evolucao == 2)%>%
  as.data.frame()
sivep_30_obitos_sum<-sivep_30_obitos%>%
  group_by(dt_evoluca)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()

write.csv(sivep_30_obitos_sum,"~/Área de Trabalho/central_covid/analise_UOL/dados/spreasheet_e_CSV/extract_30_junho.csv")

sivep_23_obitos<-sivep_23 %>%
  filter(pcr_sars2 == 1 | classi_fin == 5)%>%
  as.data.frame()
sivep_23_obitos<-sivep_23_obitos%>%
  filter(evolucao == 2)%>%
  as.data.frame()
sivep_23_obitos_sum<-sivep_23_obitos%>%
  group_by(dt_evoluca)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()

write.csv(sivep_23_obitos_sum,"~/Área de Trabalho/central_covid/analise_UOL/dados/spreasheet_e_CSV/extract_23_junho.csv")

sivep_17_obitos<-sivep_17 %>%
  filter(pcr_sars2 == 1 | classi_fin == 5)%>%
  as.data.frame()
sivep_17_obitos<-sivep_17_obitos%>%
  filter(evolucao == 2)%>%
  as.data.frame()
sivep_17_obitos_sum<-sivep_17_obitos%>%
  group_by(dt_evoluca)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()

write.csv(sivep_17_obitos_sum,"~/Área de Trabalho/central_covid/analise_UOL/dados/spreasheet_e_CSV/extract_17_junho.csv")

sivep_09_obitos<-sivep_09 %>%
  filter(pcr_sars2 == 1 | classi_fin == 5)%>%
  as.data.frame()
sivep_09_obitos<-sivep_09_obitos%>%
  filter(evolucao == 2)%>%
  as.data.frame()
sivep_09_obitos_sum<-sivep_09_obitos%>%
  group_by(dt_evoluca)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()

write.csv(sivep_09_obitos_sum,"~/Área de Trabalho/central_covid/analise_UOL/dados/spreasheet_e_CSV/extract_09_junho.csv")
