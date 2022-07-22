# Libraries -----------------

library(tidymodels)
library(janitor)
library(stringr)
library(vip)
library(probably)
library(gt)

#### APLICANDO AO CONJUNTO TESTE 2021/10 ATE 2022

data_nespecificado <- readr::read_csv("srag_16-22_novo.gz")


data_nespecificado <- data_nespecificado  %>% 
  rename(sexo = CS_SEXO, idade = NU_IDADE_N) %>% 
  filter(as.Date(DT_SIN_PRI,format="%d/%m/%Y") < as.Date("01-12-2021",format="%d-%m-%Y")) %>% 
  filter(sexo == "F") %>% # 1.342.879 points
  filter(idade > 9 & idade < 56) %>% # 502.903 points
  mutate(ano = str_sub(DT_SIN_PRI, start = 7)) %>% 
  mutate(classi_gesta_puerp = as.factor(case_when(
    CS_GESTANT == 1 ~ "1tri",
    CS_GESTANT == 2 ~ "2tri",
    CS_GESTANT == 3 ~ "3tri",
    CS_GESTANT == 4 ~ "IG_ig",
    CS_GESTANT == 5 & PUERPERA == 1 ~ "puerp",
    CS_GESTANT == 9 & PUERPERA == 1 ~ "puerp",
    TRUE ~ "não"))
  ) %>% 
  filter(classi_gesta_puerp != "não") %>% # 37.847 points
  mutate(classi_fin = as.factor(case_when(
    CLASSI_FIN == 1 ~ "não-covid",
    CLASSI_FIN == 2 ~ "não-covid",
    CLASSI_FIN == 3 ~ "não-covid",
    CLASSI_FIN == 5 ~ "covid-19",
    CLASSI_FIN == 4 ~ "não especificado",
    CLASSI_FIN == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(raca = as.factor(case_when( # race
    CS_RACA == 1 ~ "branca",
    CS_RACA == 2 | CS_RACA == 3 | CS_RACA == 4 |
      CS_RACA == 5 ~ "não branca",
    CS_RACA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%   
  mutate(escolaridade = as.factor(case_when( # instruction
    # ~ "sem escolaridade",
    CS_ESCOL_N == 0 | CS_ESCOL_N == 1 | CS_ESCOL_N == 2 ~ "até fundamental",
    CS_ESCOL_N == 3 ~ "médio",
    CS_ESCOL_N == 4 ~ "superior",
    CS_ESCOL_N == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(vacina = as.factor(case_when( #vacine
    VACINA == 1  ~ "sim",
    VACINA == 2 ~ "não",
    VACINA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(febre = as.factor(case_when( # fever
    FEBRE == 1 ~ "sim",
    FEBRE == 2 ~ "não",
    FEBRE == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(tosse = as.factor(case_when( # cough
    TOSSE == 1 ~ "sim",
    TOSSE == 2 ~ "não",
    TOSSE == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(garganta = as.factor(case_when( # throat
    GARGANTA == 1 ~ "sim",
    GARGANTA  == 2 ~ "não",
    GARGANTA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(dispneia = as.factor(case_when( # dyspnea
    DISPNEIA == 1 ~ "sim",
    DISPNEIA == 2 ~ "não",
    DISPNEIA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(desc_resp = as.factor(case_when( # respiratory distress
    DESC_RESP == 1 ~ "sim",
    DESC_RESP == 2 ~ "não",
    DESC_RESP == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(saturacao = as.factor(case_when( # saturation
    SATURACAO == 1 ~ "sim",
    SATURACAO == 2 ~ "não",
    SATURACAO == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(diarreia = as.factor(case_when( # diarrhea
    DIARREIA == 1 ~ "sim",
    DIARREIA == 2 ~ "não",
    DIARREIA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(cardiopatia = as.factor(case_when( # heart disease
    CARDIOPATI == 1 ~ "sim",
    CARDIOPATI == 2 ~ "não",
    CARDIOPATI == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(pneumopatia = as.factor(case_when( # lung disease
    PNEUMOPATI == 1 ~ "sim",
    PNEUMOPATI == 2 ~ "não",
    PNEUMOPATI == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(renal = as.factor(case_when( # kidney disease
    RENAL == 1 ~ "sim",
    RENAL == 2 ~ "não",
    RENAL == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(obesidade = as.factor(case_when( # obesity
    OBESIDADE == 1 ~ "sim",
    OBESIDADE == 2 ~ "não",
    OBESIDADE == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) #%>% 


questionr::freq(
  data_nespecificado$classi_fin,
  cum = FALSE,
  total = TRUE,
  na.last = FALSE,
  valid = FALSE
)

# n     %
# covid-19         18996  48.7
# em branco         1926   4.9
# ignorado            19   0.0
# não especificado 15815  40.5
# não-covid         2257   5.8
# Total            39013 100.0


data_teste <- data_nespecificado %>% #17.760
  filter(classi_fin == "não especificado" | classi_fin == "ignorado" | classi_fin == "em branco" )

data_teste <- data_teste %>% # 17.760 points
  dplyr::select(!where(is.numeric), idade, -c(ano, DT_SIN_PRI, sexo, classi_fin,SG_UF)) %>% 
  #mutate_if(is.numeric, scale) %>% 
  drop_na() %>% 
  as_tibble()  

saveRDS(data_teste,"dados_nespecificado.rds")

#data_teste <- readRDS("data/dados_nespecificado.rds")

# Train/test data -----------

srag_train <- readRDS("dados_treino.rds")

# Selecting data ------------

xgb_rec <- recipe(classi_fin ~ ., data = srag_train) %>% 
  step_normalize(idade) %>%
  step_dummy(all_nominal(), -classi_fin) %>% 
  themis::step_smote(classi_fin,seed = 69) #%>% 
# prep()

# sort(table(bake(xgb_rec,new_data=NULL)$classi_fin,useNA = "always"))

# Model specification -------
# mtry min_n tree_depth learn_rate loss_reduction sample_size .config              
# <int> <int>      <int>      <dbl>          <dbl>       <dbl> <chr>                
#   6    11          2     0.0582         0.0882       0.476 Preprocessor1_Model12

xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = 2, min_n = 11, 
  loss_reduction = 0.0882,                     ## first three: model complexity
  sample_size = 0.476 , mtry = 6,         ## randomness
  learn_rate = 0.0582,                         ## step size
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

# Workflow ----------------

set.seed(69)

xgb_wf <- workflow() %>%
  add_recipe(xgb_rec) %>%
  add_model(xgb_spec) %>% 
  fit(srag_train)


saveRDS(xgb_wf,"xgb_wf.rds")

preds <- predict(xgb_wf,data_teste)

pred_prob <- predict(xgb_wf,data_teste,type="prob")

best_thresh <- 0.6625 #melhor ponto de corte no modelo

preds_new <- pred_prob %>% 
  mutate(.new_pred_class = factor(ifelse(`.pred_covid-19` >= best_thresh, "covid-19", "não-covid"), 
                                  levels = c("covid-19", "não-covid")))

data_teste$classi_fin_pred <- preds_new$.new_pred_class

table(data_teste$classi_fin_pred)

# covid-19 não-covid 
# 13764      3996 

saveRDS(data_teste,"dados_xgbost_pred.rds")
