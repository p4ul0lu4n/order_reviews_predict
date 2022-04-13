# FEATURE ENGINEERING -----------------------------------------------------

#LIMPANDO O AMBIENTE
rm(list = ls())

#DEFININDO DIRETORIO PADRAO
setwd("~/git/tcc/source")

#EXECUTANDO ARQUIVO DE PREAMBULO (INSTALACAO E CARREGAMENTO DE PACOTES)
source("script/preambulo.R")

#CARREGANDO A BASE
load("data/data.Rdata", verbose = T)

#FUNCAO QUE SUBSTITUI NA POR QUALQUER VALOR
repNA = function(df, valor)tidyr::replace_na(data = df, replace = valor)

#CRIACAO DE VARIAVEIS
data_new_var = data %>%
  dplyr::select(target, seller_idhm, seller_idhm_r, seller_idhm_e, seller_idhm_l, seller_agua_esgoto,
                seller_espvida, seller_e_anosestudo, seller_t_agua, seller_t_banagua, seller_t_luz, customer_idhm, 
                customer_idhm_r, customer_idhm_e, customer_idhm_l, customer_idhm, customer_idhm_r, 
                customer_idhm_e, customer_idhm_l, customer_agua_esgoto, customer_espvida, customer_e_anosestudo, 
                customer_t_agua, customer_t_banagua, customer_t_luz, payment_value, payment_type, payment_installments, 
                order_approved_at, order_purchase_timestamp, order_delivered_carrier_date, order_estimated_delivery_date, 
                product_weight_g, product_length_cm, product_height_cm, product_width_cm, product_photos_qty) %>%
  dplyr::mutate(product_photos_qty = ifelse(test = is.na(product_photos_qty), yes = 0, no = product_photos_qty), 
                temp_diff_purchase_approv = as.numeric(as.Date(order_approved_at) - as.Date(order_purchase_timestamp)), 
                temp_diff_approv_carrier = as.numeric(as.Date(order_delivered_carrier_date) - as.Date(order_approved_at)), 
                temp_carrier_estim = as.numeric(as.Date(order_estimated_delivery_date) - as.Date(order_delivered_carrier_date)), 
                #payment_type = as.factor(payment_type)
                payment_type = dplyr::case_when(payment_type == 'boleto' ~ 1,
                                                payment_type == 'credit_card' ~ 2,
                                                payment_type == 'debit_card' ~ 3,
                                                payment_type == 'voucher' ~ 4,
                                                TRUE ~ 0)) %>%
  dplyr::select(-c(order_approved_at, order_purchase_timestamp, order_delivered_carrier_date, order_estimated_delivery_date)) %>%
  dplyr::mutate(dplyr::across(c(dplyr::starts_with('seller'),
                                dplyr::starts_with('customer')), ~repNA(.x, median(.x, na.rm = T)), .names = "{col}")) %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("temp"), ~repNA(.x, -2), .names = "{col}")) %>%
  dplyr::mutate(dplyr::across(c(dplyr::starts_with("product"),
                                dplyr::starts_with("payment_i"), 
                                dplyr::starts_with("payment_v")), ~repNA(.x, 0), .names = "{col}")) %>%
  dplyr::mutate_if(is.integer, as.numeric)

# #REBALANCEAMENTO DA AMOSTRA
# 
# #NUMERO DE AVALIACOES RUINS
# target_ruim = data_new_var %>%
#   dplyr::filter(target == 1) %>%
#   nrow()
# 
# #NUMERO DE AVALIACOES BOAS
# target_bom = data_new_var %>%
#   dplyr::filter(target == 0) %>%
#   nrow()
# 
# #NUMERO DE REGISTROS A SEREM INCLUIDOS PARA OVERSAMPLING
# new_values = target_bom - target_ruim
# 
# #BASE BOM NOVOS REGISTROS A PARTIR DE REAMOSTRAGEM
# base_new_values = imbalance::oversam(data_new_var, classAttr = "target", numInstances = new_values)
# 
# base_mesclada_final = rbind(data, base_new_values)
# 
# base_final = unique(base_mesclada_final)

# PRE PROCESS -----------------------------------------------------------

#DEFININDO SEMENTE
set.seed(123)

#DIVIDINDO A POPULACAO EM TREINO E TESTE
data_split = rsample::initial_split(data = data_new_var, prop = 0.7, strata = target)

#DATA PREPARATION
data_recipe = rsample::training(x = data_split) %>%
  recipes::recipe(target ~ .) %>%
  #recipes::step_novel(recipes::all_nominal_predictors(), -recipes::all_outcomes()) %>% #TRANSFORMANDO AS VARIAVEIS NOMINAIS EM FATOR
  #recipes::step_dummy(recipes::all_nominal_predictors(), -recipes::all_outcomes()) %>% #CRIANDO VARIAVEIS DUMMY (ONE-HOT ENCODING)
  #recipes::step_corr(recipes::all_nominal_predictors(), threshold = 0.7, method = "spearman") %>% #REMOVENDO VARIAVEIS NOMINAIS ALTAMENTE CORRELACIONADAS
  recipes::step_corr(recipes::all_numeric_predictors(), threshold = 0.7, method = "pearson") %>% #REMOVENDO VARIAVEIS NUMERICAS ALTAMENTE CORRELACIONADAS  
  recipes::step_zv(recipes::all_numeric_predictors(), -recipes::all_outcomes()) #REMOVENDO VARIAVEIS COM VARIABILIDADE PROXIMA DE ZERO

#BASE DE TREINO
preppared_data = data_recipe %>%
  recipes::prep() %>%
  recipes::juice()

#BASE DE TESTE
test = data_recipe %>%
  recipes::prep() %>%
  recipes::bake(rsample::testing(data_split))

#CROSS VALIDATION
cv = rsample::vfold_cv(rsample::training(x = data_split), v = 10, repeats = 3, strata = target)

# RANDOM FOREST -----------------------------------------------------------

#ESPECIFICANDO O MODELO
rf_spec = parsnip::rand_forest() %>% 
  set_args(mtry = tune()) %>%
  parsnip::set_engine("ranger", importance = "impurity") %>% 
  parsnip::set_mode("classification")

#CRIANDO WORKFLOW 
rf_workflow = workflows::workflow() %>% 
  workflows::add_recipe(data_recipe) %>%
  workflows::add_model(rf_spec)

#GRID DE PARAMETROS
rf_grid = expand.grid(mtry = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

tictoc::tic()
#AJUSTE DO MODELO
rf_fit = rf_workflow %>%
  tune::tune_grid(resamples = cv, 
                  grid = rf_grid,
                  metrics = yardstick::metric_set(recall, 
                                                  f_meas, 
                                                  accuracy, 
                                                  kap, 
                                                  roc_auc, 
                                                  sens),
                  control = tune::control_resamples(save_pred = TRUE)
  ) 
tictoc::toc()
#SALVANDO O MODELO EM UM ARQUIVO .RDATA
save(rf_fit, file = "Data/rf3_model.Rdata")

# #CARREGANDO O MODELO
# load(file = "Data/rf2_model.Rdata", verbose = T)

#CONFUSION MATRIX
rf_fit %>% 
  collect_predictions() %>%
  conf_mat(truth = target, estimate = .pred_class, dnn = c("Esperado", "Observado")) %>%
  autoplot(type = "heatmap")

#CURVA ROC
rf_fit %>%
  collect_predictions() %>%
  group_by(id2) %>% # id contains our folds
  roc_curve(target, .pred_1) %>% 
  autoplot()

# LOGISTIC REGRESSION -----------------------------------------------------

#ESPECIFICANDO O MODELO
log_spec = parsnip::logistic_reg() %>% 
  parsnip::set_engine(engine = "glm") %>%
  parsnip::set_mode("classification")

#CRIANDO WORKFLOW 
log_workflow = workflows::workflow() %>% 
  workflows::add_recipe(data_recipe) %>%
  workflows::add_model(log_spec)

#AJUSTE DO MODELO
log_fit = log_workflow %>% 
  tune::fit_resamples(resamples = cv, 
                      metrics = yardstick::metric_set(recall, 
                                                      f_meas, 
                                                      accuracy, 
                                                      kap, 
                                                      roc_auc, 
                                                      sens),
                      control = tune::control_resamples(save_pred = TRUE)
  ) 

#SALVANDO O MODELO EM UM ARQUIVO .RDATA
save(log_fit, file = "Data/log2_model.Rdata")

# #CARREGANDO O MODELO
# load(file = "Data/log1_model.Rdata", verbose = T)

#CONFUSION MATRIX
log_fit %>% 
  collect_predictions() %>%
  conf_mat(truth = target, estimate = .pred_class, dnn = c("Esperado", "Observado")) %>%
  autoplot(type = "heatmap")

#CURVA ROC
log_fit %>%
  collect_predictions() %>%
  group_by(id) %>% # id contains our folds
  roc_curve(target, .pred_1) %>% 
  autoplot()

# XGOOST -----------------------------------------------

#ESPECIFICANDO O MODELO
xgb_spec <- parsnip::boost_tree() %>%
  parsnip::set_engine("xgboost") %>% 
  parsnip::set_mode("classification")

#CRIANDO WORKFLOW 
xgb_workflow = workflows::workflow() %>% 
  workflows::add_recipe(data_recipe) %>%
  workflows::add_model(xgb_spec)

#AJUSTE DO MODELO
xgb_fit = xgb_workflow %>% 
  tune::fit_resamples(resamples = cv, 
                      metrics = yardstick::metric_set(recall, 
                                                      f_meas, 
                                                      accuracy, 
                                                      kap, 
                                                      roc_auc, 
                                                      sens),
                      control = tune::control_resamples(save_pred = TRUE)
  ) 

#SALVANDO O MODELO EM UM ARQUIVO .RDATA
save(xgb_fit, file = "Data/xgb2_model.Rdata")

# #CARREGANDO O MODELO
# load(file = "Data/xgb1_model.Rdata", verbose = T)

#CONFUSION MATRIX
xgb_fit %>% 
  collect_predictions() %>%
  conf_mat(truth = target, estimate = .pred_class, dnn = c("Esperado", "Observado")) %>%
  autoplot(type = "heatmap")

#CURVA ROC
xgb_fit %>%
  collect_predictions() %>%
  group_by(id2) %>% # id contains our folds
  roc_curve(target, .pred_1) %>% 
  autoplot()

# COMPARANDO MODELOS ------------------------------------------------------

#MODEL METRICS
rf_metrics = rf_fit %>% 
  tune::collect_metrics(summarise = TRUE) %>%
  dplyr::mutate(model = "Random Forest")

log_metrics = log_fit %>% 
  tune::collect_metrics(summarise = TRUE) %>%
  dplyr::mutate(model = "Logistic Regression")


xgb_metrics = xgb_fit %>% 
  tune::collect_metrics(summarise = TRUE) %>%
  dplyr::mutate(model = "XGBoost")

model_compare = dplyr::bind_rows(
  log_metrics,
  rf_metrics,
  xgb_metrics,
) 

#CHANGE DATA STRUCTURE
model_comp = model_compare %>% 
  dplyr::select(model, .metric, mean, std_err) %>% 
  tidyr::pivot_wider(names_from = .metric, values_from = c(mean, std_err)) 

#MODEL COMPARE PLOT BY F1 SCORE
model_comp %>% 
  dplyr::arrange(mean_f_meas) %>% 
  dplyr::mutate(model = fct_reorder(model, mean_f_meas)) %>%
  ggplot(aes(model, mean_f_meas, fill = model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  geom_text(
    size = 3,
    aes(label = round(mean_f_meas, 4), y = mean_f_meas + 0.08),
    vjust = 1
  )

#MODEL COMPARE PLOT BY ACCURACY
model_comp %>% 
  dplyr::arrange(mean_accuracy) %>% 
  dplyr::mutate(model = fct_reorder(model, mean_accuracy)) %>%
  ggplot(aes(model, mean_accuracy, fill = model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  geom_text(
    size = 3,
    aes(label = round(mean_accuracy, 4), y = mean_accuracy + 0.08),
    vjust = 1
  )

#MODEL COMPARE PLOT BY RECALL
model_comp %>% 
  dplyr::arrange(mean_recall) %>% 
  dplyr::mutate(model = fct_reorder(model, mean_recall)) %>%
  ggplot(aes(model, mean_recall, fill = model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  geom_text(
    size = 3,
    aes(label = round(mean_recall, 4), y = mean_recall + 0.08),
    vjust = 1
  )

# MODEL EVALUATION --------------------------------------------------------

#RANDOM FOREST 
rf_last_fit = tune::last_fit(rf_workflow, 
                             split = data_split,
                             metrics = yardstick::metric_set(recall, 
                                                             f_meas, 
                                                             accuracy, 
                                                             kap,
                                                             roc_auc)
)

#LOGISTIC REGRESSION
log_last_fit = tune::last_fit(log_workflow, 
                              split = data_split,
                              metrics = yardstick::metric_set(recall, 
                                                              f_meas, 
                                                              accuracy, 
                                                              kap,
                                                              roc_auc)
)

#XGBOOST
xgb_last_fit = tune::last_fit(xgb_workflow, 
                              split = data_split,
                              metrics = yardstick::metric_set(recall, 
                                                              f_meas, 
                                                              accuracy, 
                                                              kap,
                                                              roc_auc)
)

#COLETANDO METRICAS
rf_last_fit %>% tune::collect_metrics(summarize = T)
log_last_fit %>% tune::collect_metrics(summarize = T)
xgb_last_fit %>% tune::collect_metrics(summarize = T)