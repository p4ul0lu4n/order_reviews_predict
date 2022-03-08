# PREAMBULO ---------------------------------------------------------------

#LIMPANDO O AMBIENTE
rm(list = ls())

#INSTALACAO E/OU CARREGAMENTO DOS PACOTES
pacotes = c("dplyr", "lubridate", "stringr", "tictoc", "ggplot2", "tidymodels", "ranger", "doSNOW", "xgboost")
novos.pacotes = pacotes[!(pacotes %in% installed.packages()[, "Package"])]
if(length(novos.pacotes)) install.packages(novos.pacotes, repos = 'https://cran.us.r-project.org')
options(warn = -1)
unlist(lapply(pacotes, require, character.only = TRUE))

#DEFININDO DIRETORIO PADRAO
setwd("~/git/tcc/source")

# LEITURA DOS DATASETS -------------------------------------------------------

# NOME DAS BASES A SEREM LIDAS VIA LOOP
bases = c("customers", "geolocation", "orders", "order_items", 
          "order_payments", "order_reviews", "products", "sellers")

#LOOP DE LEITURA DAS BASES
for(i in 1:length(bases)){
  assign(x = bases[i], 
         value = read.csv(file = paste0("data/olist_", bases[i], "_dataset.csv"), 
                          header = T, 
                          sep = ",", 
                          dec = ".", 
                          encoding = "UTF-8",
                          na = ''
         )
  )
}

# ANALISE DESCRITIVA ------------------------------------------------------------

#BASE INICIAL DE DESENVOLVIMENTO
data = orders %>% 
  dplyr::left_join(y = order_reviews, by = c("order_id" = "order_id")) %>%
  dplyr::filter(!is.na(review_score)) %>%
  dplyr::left_join(y = customers, by = c("customer_id" = "customer_id")) %>%
  dplyr::left_join(y = order_payments, by = c("order_id" = "order_id")) %>%
  dplyr::left_join(y = order_items, by = c("order_id" = "order_id")) %>%
  dplyr::left_join(y = sellers, by = c("seller_id" = "seller_id")) %>%
  dplyr::left_join(y = products, by = c("product_id" = "product_id")) %>% 
  dplyr::mutate(yearmon = format(as.Date(order_purchase_timestamp), '%Y%m'), 
                target = as.factor(dplyr::case_when(review_score <= 3 ~ 1, TRUE ~ 0))) %>%
  dplyr::filter(yearmon >= '201701' & yearmon <= '201807')

#REMOVENDO AS BASES QUE NAO SERAO MAIS UTILIZADAS
remove(order_items, order_payments, order_reviews, products, customers, sellers, orders)

# #VERIFICANDO BALANCEAMENTO DA POPULACAO
# avaliacao = data %>% 
#   dplyr::group_by(target) %>%
#   dplyr::summarise(reviews = dplyr::n(), 
#                    prop = round(reviews/nrow(data) * 100, 2)) %>%
#   dplyr::mutate(value = paste0(reviews, " (", format(prop, decimal.mark = ','), "%)"))
# 
# #GRAFICO PARA APRESENTAR O BALANCO DA POPULACAO
# plot_avaliacao = ggplot(data=avaliacao, aes(x = target, y = reviews)) +
#   geom_bar(stat = "identity", fill = "steelblue")+
#   geom_text(aes(label = value), vjust = -0.3, size = 3.5)+
#   theme_minimal()
#   
# #SALVANDO ARQUIVO DO PLOT
# ggsave(filename = "plots/review_balance.png", plot = plot_avaliacao)
# 
# #QUANTIDADE DE REVIEWS POR MES
# reviews_mes = data %>%
#   dplyr::group_by(yearmon) %>%
#   dplyr::summarise(reviews = dplyr::n(), 
#                    prop = round(reviews/nrow(data) * 100, 2)) %>%
#   dplyr::mutate(value = paste0(reviews, " (", format(prop, decimal.mark = ','), "%)"))
# 
# #GRAFICO DE REVIEWS POR MES
# plot_reviews_mes = ggplot(data=reviews_mes, aes(x = yearmon, y = reviews, group = 1)) +
#   geom_line(fill = "steelblue")+
#   geom_point()+
#   geom_text(aes(label = reviews), vjust = -0.6, size = 3.5)+
#   theme_minimal()
# 
# #SALVANDO ARQUIVO DO PLOT
# ggsave(filename = "plots/review_mes.png", plot = plot_reviews_mes)

# FEATURE ENGINEERING -----------------------------------------------------

data_new_var = data %>%
  dplyr::mutate(aprovado = ifelse(is.na(order_approved_at), yes = 0, no = 1), 
                entregue = ifelse(is.na(order_delivered_customer_date), yes = 0, no = 1), 
                atraso = dplyr::case_when(as.Date(order_delivered_customer_date) > as.Date(order_estimated_delivery_date) | is.na(order_delivered_customer_date) ~ 1, TRUE ~ 0), 
                reg_cli = dplyr::case_when(customer_state %in% c('AC', 'AM', 'AP', 'PA', 'RO', 'RR', 'TO') ~ 1, 
                                           customer_state %in% c('AL', 'BA', 'CE', 'MA', 'PB', 'PE', 'PI', 'SE', 'RN') ~ 2, 
                                           customer_state %in% c('DF', 'GO', 'MS', 'MT') ~ 3,
                                           customer_state %in% c('ES', 'MG', 'RJ', 'SP') ~ 4, 
                                           customer_state %in% c('PR', 'SC', 'RS') ~ 5, 
                                           TRUE ~ 0), 
                reg_vend = dplyr::case_when(seller_state %in% c('AC', 'AM', 'AP', 'PA', 'RO', 'RR', 'TO') ~ 1, 
                                            seller_state %in% c('AL', 'BA', 'CE', 'MA', 'PB', 'PE', 'PI', 'SE', 'RN') ~ 2, 
                                            seller_state %in% c('DF', 'GO', 'MS', 'MT') ~ 3,
                                            seller_state %in% c('ES', 'MG', 'RJ', 'SP') ~ 4, 
                                            seller_state %in% c('PR', 'SC', 'RS') ~ 5, 
                                            TRUE ~ 0),
                reg_vend_cli = ifelse(test = reg_vend == reg_cli, yes = 1, no = 0),
                tipo_pag = dplyr::case_when(payment_type == 'boleto' ~ 1, 
                                            payment_type == 'credit_card' ~ 2, 
                                            payment_type == 'debit_card' ~ 3, 
                                            payment_type == 'voucher' ~ 4, 
                                            TRUE ~ 0), 
                pag1 = ifelse(test = payment_installments == 1, yes = 1, no = 0)
  ) %>%
  # dplyr::group_by(review_id) %>%
  # dplyr::mutate(qtd_itens = max(order_item_id)) %>% 
  # dplyr::ungroup() %>%
  dplyr::select(target, aprovado, entregue, atraso, reg_cli, reg_vend, reg_vend_cli, pag1, tipo_pag) 

# PRE PROCESS -----------------------------------------------------------

#DIVIDINDO A POPULACAO EM TREINO E TESTE
data_split = rsample::initial_split(data = data_new_var, prop = 0.7, strata = target)

#DATA PREPARATION
data_recipe = rsample::training(x = data_split) %>%
  recipes::recipe(target ~ .) %>%
  recipes::step_corr(recipes::all_predictors(), threshold = 0.9) %>%
  recipes::prep()

#BASE DE TREINO
train = data_recipe %>%
  recipes::juice()

#BASE DE TESTE
test = data_recipe %>%
  recipes::bake(testing(data_split))

#DEFININDO SEMENTE
set.seed(123)

#CROSS VALIDATION
cv = rsample::vfold_cv(train, v = 10, repeats = 5, strata = target)

# RANDOM FOREST -----------------------------------------------------------

#ESPECIFICANDO O MODELO
rf_spec = parsnip::rand_forest() %>% 
  parsnip::set_engine("ranger", importance = "impurity") %>% 
  parsnip::set_mode("classification")

#CRIANDO WORKFLOW 
rf_workflow = workflows::workflow() %>% 
  workflows::add_recipe(data_recipe) %>%
  workflows::add_model(rf_spec)

#AJUSTE DO MODELO
rf_fit = rf_workflow %>% 
  tune::fit_resamples(resamples = cv, 
                      metrics = yardstick::metric_set(recall, 
                                                      f_meas, 
                                                      accuracy, 
                                                      kap, 
                                                      roc_auc),
                      control = tune::control_resamples(save_pred = TRUE)
  ) 

#COLETANDO METRICAS
rf_fit %>% collect_metrics(summarize = TRUE)

#SALVANDO O MODELO EM UM ARQUIVO .RDATA
save(rf_fit, file = "Data/rf1_model.Rdata")


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
                                                      roc_auc),
                      control = tune::control_resamples(save_pred = TRUE)
  ) 

#COLETANDO METRICAS
log_fit %>% collect_metrics(summarize = TRUE)

#SALVANDO O MODELO EM UM ARQUIVO .RDATA
save(log_fit, file = "Data/log1_model.Rdata")

# GRADIENT BOOSTING MACHINE -----------------------------------------------

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
                                                      roc_auc),
                      control = tune::control_resamples(save_pred = TRUE)
  ) 

#COLETANDO METRICAS
xgb_fit %>% collect_metrics(summarize = TRUE)

#SALVANDO O MODELO EM UM ARQUIVO .RDATA
save(xgb_fit, file = "Data/xgb1_model.Rdata")
