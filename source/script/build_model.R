# FEATURE ENGINEERING -----------------------------------------------------

#LIMPANDO O AMBIENTE
rm(list = ls())

#DEFININDO DIRETORIO PADRAO
setwd("~/git/tcc/source")

#EXECUTANDO ARQUIVO DE PREAMBULO (INSTALACAO E/OU CARREGAMENTO DE PACOTES)
source("script/preambulo.R")

#CARREGANDO A BASE
load("data/data.Rdata", verbose = T)

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
                temp_diff_carrier_estim = as.numeric(as.Date(order_estimated_delivery_date) - as.Date(order_delivered_carrier_date)), 
                payment_type = as.factor(payment_type)) %>%
  dplyr::select(-c(order_approved_at, order_purchase_timestamp, order_delivered_carrier_date, order_estimated_delivery_date)) %>%
  dplyr::mutate_if(is.integer, as.numeric)

#LIBERANDO ESPAÇO REMOVENDO O OBJETO DATA
remove(data)

# PRE PROCESS -----------------------------------------------------------

#DEFININDO SEMENTE
set.seed(123)

#DIVIDINDO A POPULACAO EM TREINO E TESTE
data_split = rsample::initial_split(data = data_new_var, prop = 0.7, strata = target)

#BASE DE TREINO E TESTE
data_train = rsample::training(x = data_split)
data_test = rsample::testing(data_split)

#LIBERANDO ESPAÇO REMOVENDO O OBJETO DATA
remove(data_new_var)

#DATA PREPOCESSING
data_recipe = data_train %>%
  recipes::recipe(target ~ .) %>%
  recipes::step_relevel(target, ref_level = "1") %>%
  themis::step_downsample(target, under_ratio = 1) %>%
  recipes::step_knnimpute(recipes::all_numeric_predictors(), -recipes::all_outcomes()) %>%
  recipes::step_normalize(recipes::all_numeric_predictors(), -recipes::all_outcomes()) %>%
  recipes::step_dummy(recipes::all_nominal_predictors(), -recipes::all_outcomes()) %>% 
  recipes::step_corr(recipes::all_numeric_predictors(), threshold = 0.7, method = "pearson") %>%   
  recipes::step_nzv(recipes::all_numeric_predictors(), -recipes::all_outcomes()) %>% 
  recipes::step_pca(recipes::all_numeric_predictors(), threshold = .8)
  #themis::step_smote(target, over_ratio = 1)

# #VERIFICACAO DA BASE DE TREINO
# train = data_recipe %>%
#   recipes::prep() %>%
#   recipes::juice()
# 
# 
# #VERIFICACAO DA BASE DE TESTE
# test = data_recipe %>%
#   recipes::prep() %>%
#   recipes::bake(rsample::testing(data_split))

#CROSS VALIDATION
cv = rsample::vfold_cv(data = data_train, v = 3, repeats = 2, strata = target)

# RANDOM FOREST -----------------------------------------------------------

#ESPECIFICANDO O MODELO
rf_spec = parsnip::rand_forest() %>% 
  parsnip::set_engine("ranger", importance = "impurity") %>% 
  parsnip::set_mode("classification")

#CRIANDO WORKFLOW 
rf_workflow = workflows::workflow() %>% 
  workflows::add_recipe(data_recipe) %>%
  workflows::add_model(rf_spec)

#SALVANDO RDATA DO WORKFLOW
save(rf_workflow, file = "Data/rf4_workflow.Rdata")

#GRID DE PARAMETROS
rf_grid = expand.grid(mtry = c(3, 4, 5))

#INICIANDO CRONOMETRO DE EXECUCAO
tictoc::tic()

#AJUSTE DO MODELO
rf_fit = rf_workflow %>%
  tune::tune_grid(resamples = cv, 
                  grid = rf_grid,
                  metrics = yardstick::metric_set(recall, 
                                                  spec,
                                                  f_meas, 
                                                  accuracy, 
                                                  kap, 
                                                  roc_auc),
                  control = tune::control_resamples(save_pred = TRUE)
  ) 

#SALVANDO O MODELO EM UM ARQUIVO .RDATA
save(rf_fit, file = "Data/rf4_model.Rdata")

#FINALIZANDO CRONOMETRO DE EXECUCAO
tictoc::toc()

#CARREGANDO O MODELO
load(file = "Data/rf4_model.Rdata", verbose = T)

#MOSTRANDO O MELHOR MODELO NO TREINO
rf_fit %>% tune::show_best(metric = "recall")

#SELECIONANDO O MODELO COM OS MELHORES PARAMETROS
rf_best_param = rf_fit %>% 
  tune::select_best(metric = "recall")

#CARREGANDO O RDATA DO WORKFLOW
load(file = "Data/rf4_workflow.Rdata", verbose = T)

#FINALIZANDO O workflow COM OS MELHORES PARAMETROS
rf_best_workflow = rf_workflow %>% 
  tune::finalize_workflow(rf_best_param)

#RANDOM FOREST 
rf_last_fit = tune::last_fit(rf_best_workflow, 
                             split = data_split,
                             metrics = yardstick::metric_set(recall, 
                                                             spec,
                                                             f_meas, 
                                                             accuracy, 
                                                             kap,
                                                             roc_auc)
)

#COLETANDO METRICAS
rf_last_fit %>% tune::collect_metrics(summarize = T)

#CONFUSION MATRIX
rf_conf_mat = rf_last_fit %>% 
  tune::collect_predictions() %>%
  yardstick::conf_mat(truth = target, estimate = .pred_class) %>%
  tune::autoplot(type = "heatmap")

#SALVANDO O PLOT
ggsave(filename = "plots/rf4_conf_mat.png", plot = rf_conf_mat, height = 5, width = 5.5)

#CURVA ROC
rf_roc_curve = rf_last_fit %>%
  tune::collect_predictions() %>%
  dplyr::group_by(id) %>%
  yardstick::roc_curve(target, .pred_1) %>% 
  tune::autoplot()

#SALVANDO O PLOT
ggsave(filename = "plots/rf4_roc_curve.png", plot = rf_roc_curve, height = 5, width = 5.5)

# LOGISTIC REGRESSION -----------------------------------------------------

#ESPECIFICANDO O MODELO
log_spec = parsnip::logistic_reg() %>% 
  parsnip::set_engine(engine = "glm") %>%
  parsnip::set_mode("classification")

#CRIANDO WORKFLOW 
log_workflow = workflows::workflow() %>% 
  workflows::add_recipe(data_recipe) %>%
  workflows::add_model(log_spec)

#SALVANDO RDATA DO WORKFLOW
save(log_workflow, file = "Data/log_workflow.Rdata")

#GRID DE PARAMETROS
log_grid = expand.grid(mtry = c(0.1, 1, 5))

#INICIANDO CRONOMETRO DE EXECUCAO
tictoc::tic()

#AJUSTE DO MODELO
log_fit = log_workflow %>% 
  tune::fit_resamples(resamples = cv, 
                      grid = log_grid,
                      metrics = yardstick::metric_set(recall,
                                                      spec,
                                                      f_meas, 
                                                      accuracy, 
                                                      kap, 
                                                      roc_auc),
                      control = tune::control_resamples(save_pred = TRUE)
  ) 

#SALVANDO O MODELO EM UM ARQUIVO .RDATA
save(log_fit, file = "Data/log_model.Rdata")

#FINALIZANDO CRONOMETRO DE EXECUCAO
tictoc::toc()

#CARREGANDO O MODELO
load(file = "Data/log_model.Rdata", verbose = T)

#MOSTRANDO O MELHOR MODELO NO TREINO
log_fit %>% tune::show_best(metric = "recall")

#SELECIONANDO O MODELO COM OS MELHORES PARAMETROS
log_best_param = log_fit %>% 
  tune::select_best(metric = "recall")

#CARREGANDO O RDATA DO WORKFLOW
load(file = "Data/log_workflow.Rdata", verbose = T)

#FINALIZANDO O workflow COM OS MELHORES PARAMETROS
log_best_workflow = log_workflow %>% 
  tune::finalize_workflow(log_best_param)

#RANDOM FOREST 
log_last_fit = tune::last_fit(log_best_workflow, 
                              split = data_split,
                              metrics = yardstick::metric_set(recall,
                                                              spec,
                                                              f_meas, 
                                                              accuracy, 
                                                              kap,
                                                              roc_auc)
)

#COLETANDO METRICAS
log_last_fit %>% tune::collect_metrics(summarize = T)

#CONFUSION MATRIX
log_cont_mat = log_last_fit %>% 
  tune::collect_predictions() %>%
  yardstick::conf_mat(truth = target, estimate = .pred_class) %>%
  tune::autoplot(type = "heatmap")

#SALVANDO O PLOT
ggsave(filename = "plots/log_conf_mat.png", plot = log_cont_mat, height = 5, width = 5.5)

#CURVA ROC
log_roc_curve = log_last_fit %>%
  tune::collect_predictions() %>%
  dplyr::group_by(id) %>%
  yardstick::roc_curve(target, .pred_1) %>% 
  tune::autoplot()

#SALVANDO O PLOT
ggsave(filename = "plots/log_roc_curve.png", plot = log_roc_curve, height = 5, width = 5.5)

#SALVANDO O PLOT

# XGOOST -----------------------------------------------

#ESPECIFICANDO O MODELO
xgb_spec = parsnip::boost_tree() %>%
  parsnip::set_engine("xgboost") %>% 
  parsnip::set_mode("classification")

#CRIANDO WORKFLOW 
xgb_workflow = workflows::workflow() %>% 
  workflows::add_recipe(data_recipe) %>%
  workflows::add_model(xgb_spec)

#SALVANDO RDATA DO WORKFLOW
save(xgb_workflow, file = "Data/xgb_workflow.Rdata")

#GRID DE PARAMETROS
xgb_grid = expand.grid(subsample = 0.5, 
                       colsample_bytree = 0.5,
                       max_depth = 3,
                       min_child = seq(1), 
                       eta = c(0.1)
)

#INICIANDO CRONOMETRO DE EXECUCAO
tictoc::tic()

#AJUSTE DO MODELO
xgb_fit = xgb_workflow %>% 
  tune::tune_grid(resamples = cv, 
                  grid = xgb_grid,
                  metrics = yardstick::metric_set(recall,
                                                  spec,
                                                  f_meas, 
                                                  accuracy, 
                                                  kap, 
                                                  roc_auc),
                  control = tune::control_resamples(save_pred = TRUE)
  ) 

#SALVANDO O MODELO EM UM ARQUIVO .RDATA
save(xgb_fit, file = "Data/xgb_model.Rdata")

#FINALIZANDO CRONOMETRO DE EXECUCAO
tictoc::toc()

#CARREGANDO O MODELO
load(file = "Data/xgb_model.Rdata", verbose = T)

#MOSTRANDO O MELHOR MODELO NO TREINO
xgb_fit %>% tune::show_best(metric = "recall")

#SELECIONANDO O MODELO COM OS MELHORES PARAMETROS
xgb_best_param = xgb_fit %>% 
  tune::select_best(metric = "recall")

#CARREGANDO O RDATA DO WORKFLOW
load(file = "Data/xgb_workflow.Rdata", verbose = T)

#FINALIZANDO O workflow COM OS MELHORES PARAMETROS
xgb_best_workflow = xgb_workflow %>% 
  tune::finalize_workflow(xgb_best_param)

#RANDOM FOREST 
xgb_last_fit = tune::last_fit(xgb_best_workflow, 
                              split = data_split,
                              metrics = yardstick::metric_set(recall,
                                                              spec,
                                                              f_meas, 
                                                              accuracy, 
                                                              kap,
                                                              roc_auc)
)

#COLETANDO METRICAS
xgb_last_fit %>% tune::collect_metrics(summarize = T)

#CONFUSION MATRIX
xgb_conf_mat = xgb_last_fit %>% 
  tune::collect_predictions() %>%
  yardstick::conf_mat(truth = target, estimate = .pred_class) %>%
  tune::autoplot(type = "heatmap")

#SALVANDO O PLOT
ggsave(filename = "plots/xgb_conf_mat.png", plot = xgb_conf_mat, height = 5, width = 5.5)

#CURVA ROC
xgb_roc_curve = xgb_last_fit %>%
  tune::collect_predictions() %>%
  dplyr::group_by(id) %>%
  yardstick::roc_curve(target, .pred_1) %>% 
  tune::autoplot()

#SALVANDO O PLOT
ggsave(filename = "plots/xgb_roc_curve.png", plot = xgb_roc_curve, height = 5, width = 5.5)

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