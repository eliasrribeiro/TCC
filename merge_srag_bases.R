library(dplyr)
library(stringr)
library(crunch)

#SG_UF, UTI,, EVOLUCAO,

# 2016

SRAG_2016 <- read.csv2("Monografia_1/INFLUD16.csv") %>% 
  select(
    DT_NOTIFIC,
    DT_INTERNA,
    DT_SIN_PRI,
    CS_SEXO,
    NU_IDADE_N,
    CLASSI_FIN,
    CS_GESTANT,
    PUERPERA,
    CS_RACA,
    CS_ESCOL_N,
    VACINA,
    FEBRE,
    TOSSE,
    GARGANTA,
    DISPNEIA,
    DESC_RESP,
    SATURACAO,
    DIARREIA,
    CARDIOPATI,
    PNEUMOPATI,
    RENAL,
    OBESIDADE,
    UTI,
    SUPORT_VEN,
    EVOLUCAO
  )

# 2017

SRAG_2017 <- read.csv2("Monografia_1/INFLUD17.csv") %>% 
  select(
    DT_NOTIFIC,
    DT_INTERNA,
    DT_SIN_PRI,
    CS_SEXO,
    NU_IDADE_N,
    CLASSI_FIN,
    CS_GESTANT,
    PUERPERA,
    CS_RACA,
    CS_ESCOL_N,
    VACINA,
    FEBRE,
    TOSSE,
    GARGANTA,
    DISPNEIA,
    DESC_RESP,
    SATURACAO,
    DIARREIA,
    CARDIOPATI,
    PNEUMOPATI,
    RENAL,
    OBESIDADE,
    UTI,
    SUPORT_VEN,
    EVOLUCAO
  )

# 2018

SRAG_2018 <- read.csv2("Monografia_1/INFLUD18.csv") %>% 
  select(
    DT_NOTIFIC,
    DT_INTERNA,
    DT_SIN_PRI,
    CS_SEXO,
    NU_IDADE_N,
    CLASSI_FIN,
    CS_GESTANT,
    PUERPERA,
    CS_RACA,
    CS_ESCOL_N,
    VACINA,
    FEBRE,
    TOSSE,
    GARGANTA,
    DISPNEIA,
    DESC_RESP,
    SATURACAO,
    DIARREIA,
    CARDIOPATI,
    PNEUMOPATI,
    RENAL,
    OBESIDADE,
    UTI,
    SUPORT_VEN,
    EVOLUCAO
  )


# 2019

SRAG_2019 <- read.csv2("Monografia_1/INFLUD19.csv") %>% 
  select(
    DT_NOTIFIC,
    DT_INTERNA,
    DT_SIN_PRI,
    CS_SEXO,
    NU_IDADE_N,
    CLASSI_FIN,
    CS_GESTANT,
    PUERPERA,
    CS_RACA,
    CS_ESCOL_N,
    VACINA,
    FEBRE,
    TOSSE,
    GARGANTA,
    DISPNEIA,
    DESC_RESP,
    SATURACAO,
    DIARREIA,
    CARDIOPATI,
    PNEUMOPATI,
    RENAL,
    OBESIDADE,
    UTI,
    SUPORT_VEN,
    EVOLUCAO
  )



# 2020

SRAG_2020 <- read.csv2("Monografia_1/INFLUD20-08-08-2022.csv") %>% 
  select(
    DT_NOTIFIC,
    DT_INTERNA,
    SG_UF,
    DT_SIN_PRI,
    CS_SEXO,
    NU_IDADE_N,
    CLASSI_FIN,
    CS_GESTANT,
    PUERPERA,
    CS_RACA,
    CS_ESCOL_N,
    VACINA,
    FEBRE,
    TOSSE,
    GARGANTA,
    DISPNEIA,
    DESC_RESP,
    SATURACAO,
    DIARREIA,
    DOR_ABD,
    FADIGA,
    PERD_OLFT,
    PERD_PALA, 
    DIABETES,
    CARDIOPATI,
    PNEUMOPATI,
    RENAL,
    OBESIDADE,
    UTI,
    SUPORT_VEN,
    EVOLUCAO
  )

# 2021

SRAG_2021 <- read.csv2("Monografia_1/INFLUD21-08-08-2022.csv") %>%
  select(
    DT_NOTIFIC,
    DT_INTERNA,
    SG_UF,
    DT_SIN_PRI,
    CS_SEXO,
    NU_IDADE_N,
    CLASSI_FIN,
    CS_GESTANT,
    PUERPERA,
    CS_RACA,
    CS_ESCOL_N,
    VACINA,
    FEBRE,
    TOSSE,
    GARGANTA,
    DISPNEIA,
    DESC_RESP,
    SATURACAO,
    DIARREIA,
    DOR_ABD,
    FADIGA,
    PERD_OLFT,
    PERD_PALA, 
    DIABETES,
    CARDIOPATI,
    PNEUMOPATI,
    RENAL,
    OBESIDADE,
    UTI,
    SUPORT_VEN,
    EVOLUCAO
  )

# 2022

SRAG_2022 <- read.csv2("Monografia_1/INFLUD22-08-08-2022.csv") %>%
  select(
    DT_NOTIFIC,
    DT_INTERNA,
    SG_UF,
    DT_SIN_PRI,
    CS_SEXO,
    NU_IDADE_N,
    CLASSI_FIN,
    CS_GESTANT,
    PUERPERA,
    CS_RACA,
    CS_ESCOL_N,
    VACINA,
    FEBRE,
    TOSSE,
    GARGANTA,
    DISPNEIA,
    DESC_RESP,
    SATURACAO,
    DIARREIA,
    DOR_ABD,
    FADIGA,
    PERD_OLFT,
    PERD_PALA, 
    DIABETES,
    CARDIOPATI,
    PNEUMOPATI,
    RENAL,
    OBESIDADE,
    UTI,
    SUPORT_VEN,
    EVOLUCAO
  )

# merge 2016 and 2017

base_1617 <- full_join(SRAG_2016, SRAG_2017)

# merge 2016, 2017 and 2018

base_161718 <- full_join(base_1617, SRAG_2018)

# tidying age (nu_idade_n)

base_161718 <- base_161718 %>%
  mutate(NU_IDADE_N = ifelse(str_sub(NU_IDADE_N, end = 1) == "1", (as.numeric(str_sub(NU_IDADE_N, start = 2)) / 8760),
                             ifelse(str_sub(NU_IDADE_N, end = 1) == "2", (as.numeric(str_sub(NU_IDADE_N, start = 2)) / 365),
                                    ifelse(str_sub(NU_IDADE_N, end = 1) == "3", (as.numeric(str_sub(NU_IDADE_N, start = 2)) / 12), as.numeric(str_sub(NU_IDADE_N, start = 2))))))

# tidying instruction (cs_escol_n)

base_161718 <- base_161718 %>% 
  mutate(CS_ESCOL_N = ifelse(CS_ESCOL_N == 0, 0,
                             ifelse(CS_ESCOL_N == 1, 1,
                                    ifelse(CS_ESCOL_N == 2, 3,
                                           ifelse(CS_ESCOL_N == 3, 4, 
                                                  ifelse(CS_ESCOL_N == 10, 5, CS_ESCOL_N))))))

# merge 2016, 2017, 2018 and 2019

base_16171819 <- full_join(base_161718, SRAG_2019)

# merge 2016, 2017, 2018, 2019 and 2020

base_1617181920 <- full_join(base_16171819, SRAG_2020)

# merge 2016, 2017, 2018, 2019, 2020 and 2021

base_161718192021 <- full_join(base_1617181920, SRAG_2021)

# merge all

unique_base <- full_join(base_161718192021, SRAG_2022)

# save gz file

write.csv.gz(unique_base, "srag_16-22.gz")

# save rds file

saveRDS(unique_base, "SRAG_16-22.rds")

# sim

# sim_data <- readRDS("sim_96-20.rds") %>% 
#   mutate(
#     pais = "BRASIL",
#     regiao = ifelse(uf == "AC" | uf == "AM" | uf == "AP" | uf == "PA" | uf == "RO" | uf == "RR" | uf == "TO", "NORTE", 
#                     ifelse(uf == "AL" | uf == "BA" | uf == "CE" | uf == "MA" | uf == "PE" | uf == "PI" | uf == "PB" | uf == "RN" | uf == "SE", "NORDESTE",
#                            ifelse(uf == "DF" | uf == "GO" | uf == "MS" | uf == "MT", "CENTRO-OESTE",
#                                   ifelse(uf == "ES" | uf == "MG" | uf == "RJ" | uf == "SP", "SUDESTE", "SUL")))),
#     municipio_uf = paste(municipio, "-", uf)
#   )
# 
# # order cities and states
# 
# order_city <- unique(sim_data$municipio_uf)[order(unique(sim_data$municipio_uf))]
# 
# order_state <- unique(sim_data$uf)[order(unique(sim_data$uf))]
