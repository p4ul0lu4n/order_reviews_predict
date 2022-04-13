# ESPECIFICANDO O MODELO
rf_spec = parsnip::rand_forest() %>% 
  set_args(mtry = tune()) %>%
  parsnip::set_engine("ranger", importance = "impurity") %>% 
  parsnip::set_mode("classification")

# CRIANDO WORKFLOW 
rf_workflow = workflows::workflow() %>% 
  workflows::add_recipe(data_recipe) %>%
  workflows::add_model(rf_spec)

# GRID DE PARAMETROS
rf_grid = expand.grid(mtry = c(1,2,3, 4, 5,6,7,8,9,10))

# AJUSTE DO MODELO
rf_fit_tune = rf_workflow %>% 
  tune::tune_grid(resamples = cv, # CV object
                  grid = rf_grid, # grid of values to try
                  metrics = yardstick::metric_set(recall,f_meas,accuracy,kap,roc_auc,sens), # metrics we care about
                  control = tune::control_resamples(save_pred = TRUE)
  )

# SALVANDO O MODELO EM UM ARQUIVO .RDATA
# save(rf_fit_tune, file = "codigo-fonte/data/rf_fit_tune.Rdata")

# CARREGANDO O MODELO
# load(file = "codigo-fonte/data/rf_model.Rdata", verbose = T)

# METRICAS NO TREINO
rf_fit_tune %>% collect_metrics()

# MOSTRANDO O MELHOR MODELO NO TREINO
rf_fit_tune %>% tune::show_best(metric = "recall")

# FINALIZANDO O FLUXO DE TRABALHO
param_final <- rf_fit_tune %>% tune::select_best(metric = "recall")
rf_workflow_best <- rf_workflow %>% finalize_workflow(param_final)

# ANALISANDO MELHOR MODELO NO TREINO? (Como faz?)
# rf_fit <- rf_workflow_best %>% last_fit(rsample::training(x = data_split))

# AVALIE O MODELO NO CONJUNTO DE TESTE --------------------------------------
# Métricas em cima do teste
# Ajustar no conjunto de treinamento e avaliar no conjunto de teste
rf_fit <- rf_workflow_best %>% last_fit(data_split)

# COLENTANDO METRICAS MELHOR MODELO AVALIADO no TESTE
rf_fit %>% collect_metrics()

# GERANDO PREDICOES DO DATASET DE TREINO
train_predictions <- rf_fit %>% collect_predictions(); train_predictions

# CONFUSION MATRIX
rf_fit %>% 
  collect_predictions() %>%
  conf_mat(truth = target_default, estimate = .pred_class, dnn = c("Esperado", "Observado")) %>%
  autoplot(type = "heatmap")