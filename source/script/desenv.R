# PREAMBULO ---------------------------------------------------------------
rm(list = ls())

#instalacao e/ou carregamento de pacotes necessarios
pacotes = c("dplyr", "lubridate", "stringr", "tictoc", "ggplot2")
novos.pacotes = pacotes[!(pacotes %in% installed.packages()[, "Package"])]
if(length(novos.pacotes)) install.packages(novos.pacotes, repos = 'https://cran.us.r-project.org')
options(warn = -1)
unlist(lapply(pacotes, require, character.only = TRUE))

dir = "D:/luan/Documents/TCC/"

setwd(dir)

# LEITURA DOS DATASETS -------------------------------------------------------

# NOME DAS BASES A SEREM LIDAS VIA LOOP
bases = c("customers", "geolocation", "orders", "order_items", 
          "order_payments", "order_reviews", "products", "sellers")

#LOOP DE LEITURA DAS BASES
for(i in 1:length(bases)){
  assign(x = bases[i], 
         value = read.csv(file = paste0("olist_", bases[i], "_dataset.csv"), 
                          header = T, 
                          sep = ",", 
                          dec = "."
                          )
         )
}


# DESENVOLVIMENTO ------------------------------------------------------------

#BASE INICIAL DE DESENVOLVIMENTO
develop = orders %>% 
  dplyr::left_join(y = order_reviews, by = c("order_id" = "order_id")) %>%
  dplyr::filter(!is.na(review_score)) %>%
  dplyr::left_join(y = customers, by = c("customer_id" = "customer_id")) %>%
  dplyr::left_join(y = order_payments, by = c("order_id" = "order_id")) %>%
  dplyr::left_join(y = order_items, by = c("order_id" = "order_id")) %>%
  dplyr::left_join(y = sellers, by = c("seller_id" = "seller_id")) %>%
  dplyr::left_join(y = products, by = c("product_id" = "product_id")) %>% 
  dplyr::mutate(yearmon = format(as.Date(order_approved_at), '%Y%m'), 
                target = as.factor(dplyr::case_when(review_score <= 3 ~ 1, TRUE ~ 0)))

#VERIFICANDO BALANCEAMENTO DA POPULACAO
avaliacao = develop %>% 
  dplyr::group_by(target) %>%
  dplyr::summarise(n = dplyr::n(), 
                   prop = round(n/nrow(develop) * 100, 2)) %>%
  dplyr::mutate(value = paste0(n, "(", format(prop, decimal.mark = ','), "%)"))

#GRAFICO
ggplot(data=avaliacao, aes(x=target, y=value)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=len), vjust=-0.3, size=3.5)+
  theme_minimal()

