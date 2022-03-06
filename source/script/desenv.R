# PREAMBULO ---------------------------------------------------------------
rm(list = ls())

#INSTALACAO E/OU CARREGAMENTO DOS PACOTES
pacotes = c("dplyr", "lubridate", "stringr", "tictoc", "ggplot2")
novos.pacotes = pacotes[!(pacotes %in% installed.packages()[, "Package"])]
if(length(novos.pacotes)) install.packages(novos.pacotes, repos = 'https://cran.us.r-project.org')
options(warn = -1)
unlist(lapply(pacotes, require, character.only = TRUE))

#DEFININDO DIRETORIO PADRAO
setwd("source/")

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
                          dec = "."
                          )
         )
}


# ANALISE DESCRITIVA ------------------------------------------------------------

#BASE INICIAL DE DESENVOLVIMENTO
develop = orders %>% 
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

#BASE COM OS ID'S
develop_id = develop %>%
  dplyr::select(contains("_id"))

#BASE COM AS VARIAVEIS EXPLICATIVAS E TARGET
develop_exp = develop %>%
  dplyr::select(target, yearmon, !(contains("_id")))

#VERIFICANDO BALANCEAMENTO DA POPULACAO
avaliacao = develop_exp %>% 
  dplyr::group_by(target) %>%
  dplyr::summarise(reviews = dplyr::n(), 
                   prop = round(reviews/nrow(develop) * 100, 2)) %>%
  dplyr::mutate(value = paste0(reviews, " (", format(prop, decimal.mark = ','), "%)"))

#GRAFICO PARA APRESENTAR O BALANCO DA POPULACAO
plot_avaliacao = ggplot(data=avaliacao, aes(x = target, y = reviews)) +
  geom_bar(stat = "identity", fill = "steelblue")+
  geom_text(aes(label = value), vjust = -0.3, size = 3.5)+
  theme_minimal()
  
#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/review_balance.png", plot = plot_avaliacao)

#QUANTIDADE DE REVIEWS POR MES
reviews_mes = develop_exp %>%
  dplyr::group_by(yearmon) %>%
  dplyr::summarise(reviews = dplyr::n(), 
                   prop = round(reviews/nrow(develop) * 100, 2)) %>%
  dplyr::mutate(value = paste0(reviews, " (", format(prop, decimal.mark = ','), "%)"))

#GRAFICO DE REVIEWS POR MES
plot_reviews_mes = ggplot(data=reviews_mes, aes(x = yearmon, y = reviews, group = 1)) +
  geom_line(fill = "steelblue")+
  geom_point()+
  geom_text(aes(label = reviews), vjust = -0.6, size = 3.5)+
  theme_minimal()

#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/review_mes.png", plot = plot_reviews_mes)
