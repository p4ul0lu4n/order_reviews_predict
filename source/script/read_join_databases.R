# LEITURA DOS DATASETS -------------------------------------------------------

#LIMPANDO O AMBIENTE
rm(list = ls())

#DEFININDO DIRETORIO PADRAO
setwd("~/git/tcc/source")

#EXECUTANDO ARQUIVO DE PREAMBULO (INSTALACAO E CARREGAMENTO DE PACOTES)
source("script/preambulo.R")

# NOME DAS BASES A SEREM LIDAS VIA LOOP
bases = c("customers", "geolocation", "orders", "order_items", 
          "order_payments", "order_reviews", "products", "sellers")

#LOOP DE LEITURA DAS BASES DA OLIST
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

#LEITURA DA BASE AUXILIAR DE UF
uf = read.csv(file = "data/ibge_estados.csv", 
              header = T, 
              sep = ";", 
              dec = ".", 
              encoding = "UTF-8",
              na = ''
) %>%
  dplyr::select(-name_state)

#LEITURA DAS BASES DE IDH (CENSO 2010)
idh_atlas = read.csv(file = "data/idh_atlas.csv", 
                     header = T, 
                     sep = ",", 
                     dec = ".", 
                     encoding = "UTF-8",
                     na = ''
) %>% 
  dplyr::filter(ano == 2010) 


# PRE PROCESSAMENTO/TRATAMENTO --------------------------------------------

#IDH COM AS VARIAVEIS QUE FAZEM SENTIDO PARA A ANALISE
idh = idh_atlas %>% 
  dplyr::mutate(municipio = toupper(abjutils::rm_accent(`município`))) %>% #REMOCAO DE ACENTOS E PADRONIZACAO EM CAIXA ALTA
  dplyr::select(uf, municipio, espvida, e_anosestudo, idhm, idhm_e, idhm_l, idhm_r, t_agua, t_banagua, t_luz, agua_esgoto)

#BASE IDH JUNTA
data_idh = idh %>%
  dplyr::inner_join(y = uf, by = c("uf" = "uf"))

#TRATANDO A VARIAVEL DE CIDADE DO VENDEDOR E CLIENTE
sellers_idh = sellers %>% 
  dplyr::mutate(seller_city = toupper(abjutils::rm_accent(seller_city))) %>% #REMOCAO DE ACENTOS E PADRONIZACAO EM CAIXA ALTA
  dplyr::mutate(seller_city = stringr::str_squish(seller_city)) %>% #REMOVENDO QUALQUER ESPACO ALEM DOS NECESSARIOS
  dplyr::inner_join(y = data_idh, by = c("seller_state" = "state", "seller_city" = "municipio")) %>%
  dplyr::rename(seller_uf = uf, seller_espvida = espvida, seller_e_anosestudo = e_anosestudo, seller_idhm = idhm, seller_idhm_r = idhm_r, 
                seller_idhm_l = idhm_l, seller_idhm_e = idhm_e, seller_t_agua = t_agua, seller_t_banagua = t_banagua, seller_t_luz = t_luz, 
                seller_agua_esgoto = agua_esgoto)

customers_idh = customers %>% 
  dplyr::mutate(customer_city = toupper(abjutils::rm_accent(customer_city))) %>% #REMOCAO DE ACENTOS E PADRONIZACAO EM CAIXA ALTA
  dplyr::mutate(customer_city = stringr::str_squish(customer_city)) %>% #REMOVENDO QUALQUER ESPACO ALEM DOS NECESSARIOS
  dplyr::inner_join(y = data_idh, by = c("customer_state" = "state", "customer_city" = "municipio")) %>%
  dplyr::rename(customer_uf = uf, customer_espvida = espvida, customer_e_anosestudo = e_anosestudo, customer_idhm = idhm, customer_idhm_r = idhm_r, 
                customer_idhm_l = idhm_l, customer_idhm_e = idhm_e, customer_t_agua = t_agua, customer_t_banagua = t_banagua, customer_t_luz = t_luz, 
                customer_agua_esgoto = agua_esgoto)

#BASE OLIST JUNTA
data = orders %>% 
  dplyr::left_join(y = order_reviews, by = c("order_id" = "order_id")) %>%
  dplyr::filter(!is.na(review_score)) %>%
  dplyr::left_join(y = customers_idh, by = c("customer_id" = "customer_id")) %>%
  dplyr::left_join(y = order_payments, by = c("order_id" = "order_id")) %>%
  dplyr::left_join(y = order_items, by = c("order_id" = "order_id")) %>%
  dplyr::left_join(y = sellers_idh, by = c("seller_id" = "seller_id")) %>%
  dplyr::left_join(y = products, by = c("product_id" = "product_id")) %>% 
  dplyr::mutate(yearmon = format(as.Date(order_purchase_timestamp), '%Y%m'), 
                target = as.factor(dplyr::case_when(review_score <= 3 ~ 1, TRUE ~ 0))) %>%
  dplyr::filter(yearmon >= '201701' & yearmon <= '201807')

#REMOVENDO AS BASES QUE NAO SERAO MAIS UTILIZADAS
remove(order_items, order_payments, order_reviews, products, customers, customers_idh, sellers, sellers_idh, orders, geolocation, idh_atlas, data_idh, idh, uf)

#SALVANDO O OBJETO EM ARQUIVO .RDATA
save(data, file = "data/data.Rdata")