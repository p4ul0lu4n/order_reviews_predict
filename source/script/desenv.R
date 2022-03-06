# PREAMBULO ---------------------------------------------------------------

#LIMPANDO O AMBIENTE
rm(list = ls())

#INSTALACAO E/OU CARREGAMENTO DOS PACOTES
pacotes = c("dplyr", "lubridate", "stringr", "tictoc", "ggplot2")
novos.pacotes = pacotes[!(pacotes %in% installed.packages()[, "Package"])]
if(length(novos.pacotes)) install.packages(novos.pacotes, repos = 'https://cran.us.r-project.org')
options(warn = -1)
unlist(lapply(pacotes, require, character.only = TRUE))

#DEFININDO DIRETORIO PADRAO
#setwd("source")

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

# #BASE COM OS ID'S
# data_id = data %>%
#   dplyr::select(contains("_id"))
# 
# #BASE COM AS TARGET E DEMAIS ASPIRANTES A VARIAVEIS
# data_exp = data %>%
#   dplyr::select(target, yearmon, !(contains("_id")))

#VERIFICANDO BALANCEAMENTO DA POPULACAO
avaliacao = data %>% 
  dplyr::group_by(target) %>%
  dplyr::summarise(reviews = dplyr::n(), 
                   prop = round(reviews/nrow(data) * 100, 2)) %>%
  dplyr::mutate(value = paste0(reviews, " (", format(prop, decimal.mark = ','), "%)"))

#GRAFICO PARA APRESENTAR O BALANCO DA POPULACAO
plot_avaliacao = ggplot(data=avaliacao, aes(x = target, y = reviews)) +
  geom_bar(stat = "identity", fill = "steelblue")+
  geom_text(aes(label = value), vjust = -0.3, size = 3.5)+
  theme_minimal()
  
#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/review_balance.png", plot = plot_avaliacao)

#QUANTIDADE DE REVIEWS POR MES
reviews_mes = data %>%
  dplyr::group_by(yearmon) %>%
  dplyr::summarise(reviews = dplyr::n(), 
                   prop = round(reviews/nrow(data) * 100, 2)) %>%
  dplyr::mutate(value = paste0(reviews, " (", format(prop, decimal.mark = ','), "%)"))

#GRAFICO DE REVIEWS POR MES
plot_reviews_mes = ggplot(data=reviews_mes, aes(x = yearmon, y = reviews, group = 1)) +
  geom_line(fill = "steelblue")+
  geom_point()+
  geom_text(aes(label = reviews), vjust = -0.6, size = 3.5)+
  theme_minimal()

#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/review_mes.png", plot = plot_reviews_mes)

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
                #qtd_itens = 
                tipo_pag = dplyr::case_when(payment_type == 'boleto' ~ 1, 
                                            payment_type == 'credit_card' ~ 2, 
                                            payment_type == 'debit_card' ~ 3, 
                                            payment_type == 'voucher' ~ 4, 
                                            TRUE ~ 0), 
                dur_pag = ifelse(test = quantile(payment_installments, probs = 0.25), yes = 1, no = 0)
                ) %>%
  dplyr::select(target, aprovado, entregue, atraso, reg_cli, reg_vend, reg_vend_cli, dur_pag, tipo_pag)

# TREINO MODELO -----------------------------------------------------------
