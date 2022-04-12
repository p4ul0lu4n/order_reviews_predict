# PREAMBULO ---------------------------------------------------------------

#LIMPANDO O AMBIENTE
rm(list = ls())

#INSTALACAO E/OU CARREGAMENTO DOS PACOTES
pacotes = c("dplyr", "lubridate", "stringr", "tictoc", "ggplot2", "ggeasy", "ggpubr", "tidymodels", "ranger", "doSNOW", "xgboost", "abjutils", "forcats")
novos.pacotes = pacotes[!(pacotes %in% installed.packages()[, "Package"])]
if(length(novos.pacotes)) install.packages(novos.pacotes, repos = 'https://cran.us.r-project.org')
options(warn = -1)
unlist(lapply(pacotes, require, character.only = TRUE))

#DEFININDO NUMEROS SEM NOTACAO CIENTIFICA
options(scipen = 9999999999999999)

#DEFININDO DIRETORIO PADRAO
setwd("~/git/tcc/source")

# LEITURA DOS DATASETS -------------------------------------------------------

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

#CONTAGEM DE LINHAS COM VALORES NULOS
countNA = function(x) sum(is.na(x)) 

rows_with_na = data %>%
  dplyr::mutate(flag_na = as.factor(dplyr::case_when(apply(data, 1, countNA) == 0 ~ 0, TRUE ~ 1))) %>%
  dplyr::group_by(flag_na) %>%
  dplyr::summarise(na = dplyr::n(), prop = round(na/nrow(data) * 100, 2)) %>%
  dplyr::mutate(value = paste0(format(na, big.mark = '.'), " (", format(prop, decimal.mark = ','), "%)"))

#GRAFICO PARA APRESENTAR A QTDE DE LINHAS COM VALORES NULOS
plot_na = ggplot(data=rows_with_na, aes(x = flag_na, y = na)) +
  geom_bar(stat = "identity", fill = "steelblue")+
  geom_text(aes(label = value), vjust = -0.3, size = 3.5)+
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  xlab("Nulos") +
  ylab("Quantidade") +
  ggtitle(label = "Linhas sem Valores Nulos x Com valores Nulos") +
  ggeasy::easy_center_title()

#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/rows_with_na.png", plot = plot_na, height = 5, width = 5.5)

#QUANTIDADE POR REVIEW SCORE
review_score = data %>%
  dplyr::group_by(review_score) %>%
  dplyr::summarise(score = dplyr::n(),
                   prop = round(score/nrow(data) * 100, 2)) %>%
  dplyr::mutate(value = paste0(format(score, big.mark = '.'), " (", format(prop, decimal.mark = ','), "%)"))

#GRAFICO PARA APRESENTAR A DISTRIBUICAO DE AVALIACOES POR SCORE DA POPULACAO
plot_review_score = ggplot(data=review_score, aes(x = review_score, y = score)) +
  geom_bar(stat = "identity", fill = "steelblue")+
  geom_text(aes(label = value), vjust = -0.3, size = 3.5)+
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  xlab("Nota (Score)") +
  ylab("Quantidade") +
  ggtitle(label = "Reviews por Nota (Score)") +
  ggeasy::easy_center_title()

#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/review_score.png", plot = plot_review_score, height = 5, width = 5.5)

#VERIFICANDO BALANCEAMENTO DA POPULACAO
balance = data %>%
  dplyr::group_by(target) %>%
  dplyr::summarise(reviews = dplyr::n(),
                   prop = round(reviews/nrow(data) * 100, 2)) %>%
  dplyr::mutate(value = paste0(format(reviews, big.mark = '.'), " (", format(prop, decimal.mark = ','), "%)"))

#GRAFICO PARA APRESENTAR O BALANCO DA POPULACAO
plot_balance = ggplot(data=avaliacao, aes(x = target, y = reviews)) +
  geom_bar(stat = "identity", fill = "steelblue")+
  geom_text(aes(label = value), vjust = -0.3, size = 3.5)+
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  xlab("Target") +
  ylab("Quantidade") +
  ggtitle(label = "Reviews (Bons x Ruins)") +
  ggeasy::easy_center_title()

#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/review_balance.png", plot = plot_balance, height = 5, width = 5.5)


# ANALISE EXPLORATORIA ------------------------------------------------------------


############################## ANALISE POR REGIAO #################################

#QUANTIDADE DE REVIEWS POR REGIAO (CLIENTE)
reviews_regiao_cli = data %>%
  dplyr::mutate(reg_cli = dplyr::case_when(customer_state %in% c('AC', 'AM', 'AP', 'PA', 'RO', 'RR', 'TO') ~ 'Norte', 
                                           customer_state %in% c('AL', 'BA', 'CE', 'MA', 'PB', 'PE', 'PI', 'SE', 'RN') ~ 'Nordeste', 
                                           customer_state %in% c('DF', 'GO', 'MS', 'MT') ~ 'Centro-Oeste',
                                           customer_state %in% c('ES', 'MG', 'RJ', 'SP') ~ 'Sudeste', 
                                           customer_state %in% c('PR', 'SC', 'RS') ~ 'Sul', 
                                           TRUE ~ 'Não Consta')) %>%
  dplyr::group_by(reg_cli) %>%
  dplyr::summarise(reviews = dplyr::n(),
                   prop = round(reviews/nrow(data) * 100, 2)) %>%
  dplyr::mutate(value = paste0(format(reviews, big.mark = '.'), " (", format(prop, decimal.mark = ','), "%)")) %>%
  dplyr::filter(reg_cli != "Não Consta")

#GRAFICO PARA APRESENTAR OS REVIEWS POR REGIAO (CLIENTE)
plot_regiao_cli = ggplot(data=reviews_regiao_cli, aes(x = reg_cli, y = reviews)) +
  geom_bar(stat = "identity", position = 'identity', fill = "steelblue")+
  geom_text(aes(label = value), vjust = -0.3, size = 3.5)+
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Reviews por Região do Cliente") +
  xlab("Região") + 
  ylab("Quantidade") +
  ggeasy::easy_center_title()

#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/reviews_regiao_cli.png", plot = plot_regiao_cli, height = 5, width = 5.5)

#QUANTIDADE REVIEWS POR REGIAO E AVALIACAO
reviews_regiao_cli_target = data %>%
  dplyr::mutate(reg_cli = dplyr::case_when(customer_state %in% c('AC', 'AM', 'AP', 'PA', 'RO', 'RR', 'TO') ~ 'Norte', 
                                           customer_state %in% c('AL', 'BA', 'CE', 'MA', 'PB', 'PE', 'PI', 'SE', 'RN') ~ 'Nordeste', 
                                           customer_state %in% c('DF', 'GO', 'MS', 'MT') ~ 'Centro-Oeste',
                                           customer_state %in% c('ES', 'MG', 'RJ', 'SP') ~ 'Sudeste', 
                                           customer_state %in% c('PR', 'SC', 'RS') ~ 'Sul', 
                                           TRUE ~ 'Não Consta')) %>%
  dplyr::group_by(reg_cli, target) %>%
  dplyr::summarise(reviews = dplyr::n()) %>%
  dplyr::mutate(prop = round(reviews/sum(reviews) * 100, 2), 
                value = paste0(format(reviews, big.mark = '.'), " (", format(prop, decimal.mark = ','), "%)")) %>%
  dplyr::filter(reg_cli != "Não Consta")

#GRAFICO PARA APRESENTAR OS REVIEWS POR REGIAO (CLIENTE)
plot_regiao_cli_target = ggplot(data = reviews_regiao_cli_target, aes(x = reg_cli, y = prop, fill = target)) +
  geom_bar(position = "fill", stat = "identity")+
  geom_text(aes(label = value), position = "fill", vjust = -0.3, size = 2.5) +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Reviews por Região do Cliente e suas Avaliações") +
  xlab("Região") + 
  ylab("Proporção") +
  ggeasy::easy_center_title()
  

#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/reviews_regiao_cli_target.png", plot = plot_regiao_cli_target, height = 5, width = 5.5)

#QUANTIDADE DE REVIEWS POR REGIAO (VENDEDOR)
reviews_regiao_vend = data %>%
  dplyr::mutate(reg_vend = dplyr::case_when(seller_state %in% c('AC', 'AM', 'AP', 'PA', 'RO', 'RR', 'TO') ~ 'Norte',
                                            seller_state %in% c('AL', 'BA', 'CE', 'MA', 'PB', 'PE', 'PI', 'SE', 'RN') ~ 'Nordeste',
                                            seller_state %in% c('DF', 'GO', 'MS', 'MT') ~ 'Centro-Oeste',
                                            seller_state %in% c('ES', 'MG', 'RJ', 'SP') ~ 'Sudeste',
                                            seller_state %in% c('PR', 'SC', 'RS') ~ 'Sul',
                                            TRUE ~ 'Não Consta')) %>%
  dplyr::group_by(reg_vend) %>%
  dplyr::summarise(reviews = dplyr::n(),
                   prop = round(reviews/nrow(data) * 100, 2)) %>%
  dplyr::mutate(value = paste0(format(reviews, big.mark = '.'), " (", format(prop, decimal.mark = ','), "%)")) %>%
  dplyr::filter(reg_vend != "Não Consta")

#GRAFICO PARA APRESENTAR OS REVIEWS POR REGIAO (VENDEDOR)
plot_regiao_vend = ggplot(data=reviews_regiao_vend, aes(x = reg_vend, y = reviews)) +
  geom_bar(stat = "identity", fill = "steelblue")+
  geom_text(aes(label = value), vjust = -0.3, size = 3.5)+
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Reviews por Região do Vendedor") +
  xlab("Região") + 
  ylab("Quantidade") +
  ggeasy::easy_center_title()

#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/reviews_regiao_vend.png", plot = plot_regiao_vend, height = 5, width = 5.5)

#QUANTIDADE REVIEWS POR REGIAO E AVALIACAO
reviews_regiao_vend_target = data %>%
  dplyr::mutate(reg_vend = dplyr::case_when(seller_state %in% c('AC', 'AM', 'AP', 'PA', 'RO', 'RR', 'TO') ~ 'Norte',
                                            seller_state %in% c('AL', 'BA', 'CE', 'MA', 'PB', 'PE', 'PI', 'SE', 'RN') ~ 'Nordeste',
                                            seller_state %in% c('DF', 'GO', 'MS', 'MT') ~ 'Centro-Oeste',
                                            seller_state %in% c('ES', 'MG', 'RJ', 'SP') ~ 'Sudeste',
                                            seller_state %in% c('PR', 'SC', 'RS') ~ 'Sul',
                                            TRUE ~ 'Não Consta')) %>%
  dplyr::group_by(reg_vend, target) %>%
  dplyr::summarise(reviews = dplyr::n()) %>%
  dplyr::mutate(prop = round(reviews/sum(reviews) * 100, 2), 
                value = paste0(format(reviews, big.mark = '.'), " (", format(prop, decimal.mark = ','), "%)")) %>%
  dplyr::filter(reg_vend != "Não Consta")

#GRAFICO PARA APRESENTAR OS REVIEWS POR REGIAO (VENEDOR E TARGET)
plot_regiao_vend_target = ggplot(data = reviews_regiao_vend_target, aes(x = reg_vend, y = prop, fill = target)) +
  geom_bar(position = "fill", stat = "identity")+
  geom_text(aes(label = value), position = "fill", vjust = -0.3, size = 2.5) +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Reviews por Região do Vendedor e suas Avaliações") +
  xlab("Região") + 
  ylab("Porcentagem") +
  ggeasy::easy_center_title()

#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/reviews_regiao_vend_target.png", plot = plot_regiao_vend_target, height = 5, width = 5.5)

#QUANTIDADE DE REVIEWS EM QUE A REGIAO DO VENDEDOR E A MESMA DO CLIENTE
reviews_regiao_vend_cli_target = data %>%
  dplyr::mutate(reg_vend = dplyr::case_when(seller_state %in% c('AC', 'AM', 'AP', 'PA', 'RO', 'RR', 'TO') ~ 'Norte',
                                            seller_state %in% c('AL', 'BA', 'CE', 'MA', 'PB', 'PE', 'PI', 'SE', 'RN') ~ 'Nordeste',
                                            seller_state %in% c('DF', 'GO', 'MS', 'MT') ~ 'Centro-Oeste',
                                            seller_state %in% c('ES', 'MG', 'RJ', 'SP') ~ 'Sudeste',
                                            seller_state %in% c('PR', 'SC', 'RS') ~ 'Sul',
                                            TRUE ~ 'Não Consta'), 
                reg_cli = dplyr::case_when(customer_state %in% c('AC', 'AM', 'AP', 'PA', 'RO', 'RR', 'TO') ~ 'Norte', 
                                           customer_state %in% c('AL', 'BA', 'CE', 'MA', 'PB', 'PE', 'PI', 'SE', 'RN') ~ 'Nordeste', 
                                           customer_state %in% c('DF', 'GO', 'MS', 'MT') ~ 'Centro-Oeste',
                                           customer_state %in% c('ES', 'MG', 'RJ', 'SP') ~ 'Sudeste', 
                                           customer_state %in% c('PR', 'SC', 'RS') ~ 'Sul', 
                                           TRUE ~ 'Não Consta'), 
                reg_vend_cli = ifelse(test = reg_vend == reg_cli, yes = 'Mesma Região', no = 'Regiões Diferentes')) %>%
  dplyr::group_by(reg_vend_cli, target) %>%
  dplyr::summarise(reviews = dplyr::n()) %>%
  dplyr::mutate(prop = round(reviews/sum(reviews) * 100, 2), 
                value = paste0(format(reviews, big.mark = '.'), " (", format(prop, decimal.mark = ','), "%)"))

#GRAFICO PARA APRESENTAR OS REVIEWS POR REGIAO IGUAL ENTRE CLIENTES E VENDEDORES
plot_regiao_vend_cli_target = ggplot(data=reviews_regiao_vend_cli, aes(x = reg_vend_cli, y = reviews, fill = target)) +
  geom_bar(stat = "identity", position = "fill")+
  geom_text(aes(label = value), vjust = -0.3, size = 3.5, position = "fill")+
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Reviews por Igualdade de Região (Cliente e Vendedor) \n e suas Avaliações") +
  xlab("Região") +
  ylab("Proporção") +
  ggeasy::easy_center_title()

#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/reviews_regiao_vend_cli_target.png", plot = plot_regiao_vend_cli_target, height = 5, width = 5.5)


############################## ANALISE POR IDH #################################

#IDH CLIENTE POR AVALIACAO
plot_idh_cli_target = ggplot(data = data, aes(x=target, y=customer_idhm)) + 
  geom_boxplot(fill = "steelblue") +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Distribuição de IDH por \n Cliente e Avaliação") +
  xlab("Target") +
  ylab("IDH") +
  ggeasy::easy_center_title()

#IDH VENDEDOR POR AVALIACAO
plot_idh_vend_target = ggplot(data = data, aes(x=target, y=seller_idhm)) + 
  geom_boxplot(fill = "steelblue") +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Distribuição de IDH por \n Vendedor e Avaliação") +
  xlab("Target") +
  ylab("IDH") +
  ggeasy::easy_center_title()

#PLOTANDO OS GRAFICOS JUNTOS
plot_idh_vend_cli_target = ggpubr::ggarrange(plot_idh_cli_target, 
                                             plot_idh_vend_target, 
                                             ncol = 2, 
                                             nrow = 1, 
                                             vjust = 2, 
                                             hjust = 0)

#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/idh_vend_cli_target.png", plot = plot_idh_vend_cli_target, height = 5, width = 5.5)


####################### ANALISE POR ESPEC. PRODUTO #############################

#QUANTIDADE DE AVALIACOES POR PESO DO PRODUTO
peso_tamanho_produto_target = data %>%
  dplyr::select(target, product_weight_g, product_length_cm, product_height_cm, product_width_cm) %>%
  dplyr::mutate(product_volume_m3 = (product_length_cm * product_height_cm * product_width_cm)/ 100, 
                product_weight_kg = product_weight_g/1000) %>%
  dplyr::group_by(target)

#GRAFICO DO PESO DO PRODUTO POR AVALIACAO
plot_peso_produto_target = ggplot(data = peso_tamanho_produto_target, aes(x = product_weight_kg, colour = target)) + 
  stat_ecdf(geom = "step") +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Distribuição Acumulada do Peso dos \n Produtos (kg) por Avaliação") +
  ylab("Acumulada") +
  xlab("Peso (kg)") +
  ggeasy::easy_center_title()

#GRAFICO DO PESO DO PRODUTO POR AVALIACAO
plot_peso_produto_target = ggplot(data = peso_tamanho_produto_target, aes(x = product_weight_kg, colour = target)) + 
  stat_ecdf(geom = "step") +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Distribuição Acumulada do Peso dos \n Produtos (kg) por Avaliação") +
  ylab("Acumulada") +
  xlab("Peso (kg)") +
  ggeasy::easy_center_title()

#GRAFICO DO VOLUME DO PRODUTO POR AVALIACAO
plot_volume_produto_target = ggplot(data = peso_tamanho_produto_target, aes(x = product_volume_m3, colour = target)) + 
  stat_ecdf(geom = "step") +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Distribuição Acumulada do Volume dos \n Produtos (m3) por Avaliação") +
  ylab("Acumulada") +
  xlab("Volume (m3)") +
  ggeasy::easy_center_title()


#PLOTANDO OS GRAFICOS JUNTOS
plot_peso_volume_produto_target= ggpubr::ggarrange(plot_peso_produto_target, 
                                                   plot_volume_produto_target, 
                                                   ncol = 1, 
                                                   nrow = 2, 
                                                   vjust = 2, 
                                                   hjust = 0)

#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/peso_volume_produto_target.png", plot = plot_peso_volume_produto_target, height = 5, width = 5.5)

#FOTOS POR PRODUTO E SUAS AVALIACOES
fotos_produto_target = data %>%
  dplyr::select(target, product_photos_qty) %>%
  dplyr::mutate(product_photos_qty = ifelse(test = is.na(product_photos_qty), yes = 0, no = product_photos_qty)) %>%
  dplyr::filter(product_photos_qty <= 6) %>%
  dplyr::group_by(product_photos_qty, target) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(prop = round(n/sum(n) * 100, 2), 
                value = paste0(format(n, big.mark = '.'), " (", format(prop, decimal.mark = ','), "%)"))

#GRAFICO PARA APRESENTAR OS REVIEWS POR REGIAO (VENDEDOR)
plot_foto_produto_target = ggplot(data=fotos_produto, aes(x = as.factor(product_photos_qty), y = n, fill = target)) +
  geom_bar(stat = "identity", position = "fill")+
  geom_text(aes(label = value), vjust = -0.3, size = 2, position = "fill")+
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Número de Fotos do Produto por avaliação") +
  xlab("Número de Fotos") + 
  ylab("Quantidade") +
  ggeasy::easy_center_title()

#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/foto_produto_target.png", plot = plot_foto_produto_target, height = 5, width = 5.5)


###################### ANALISE POR VARIAVEIS DE TEMPO ##########################

temp_diff_target = data %>%
  dplyr::mutate(temp_carimb_approv = as.numeric(as.Date(order_approved_at) - as.Date(order_purchase_timestamp)), 
                temp_approv_lanc = as.numeric(as.Date(order_delivered_carrier_date) - as.Date(order_approved_at)), 
                temp_lanc_estim_deliv = as.numeric(as.Date(order_estimated_delivery_date) - as.Date(order_delivered_carrier_date))) %>%
  dplyr::select(target, order_purchase_timestamp, order_approved_at, order_delivered_carrier_date, order_estimated_delivery_date, 
                temp_carimb_approv, temp_approv_lanc, temp_lanc_estim_deliv) %>%
  dplyr::filter(temp_carimb_approv >= 0 & temp_approv_lanc >= 0 & temp_lanc_estim_deliv >= 0) %>%
  dplyr::group_by(target)

#GRAFICO DO TEMPO ENTRE O DATA DA REALIZACAO DO PEDIDO E A DATA DE APROVACAO POR AVALIACAO
plot_temp_carimb_approv_target = ggplot(data = temp_diff_target, aes(x = temp_carimb_approv, colour = target)) + 
  stat_ecdf(geom = "step") +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Distribuição Acumulada do Tempo (dias) entre a Data de \n Carimbo do Pedido e a de Aprovação por Avaliação") +
  ylab("Acumulada") +
  xlab("Volume (m3)") +
  ggeasy::easy_center_title()

#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/temp_carimb_approv_target.png", plot = plot_temp_carimb_approv_target, height = 5, width = 5.5)

#GRAFICO DO TEMPO ENTRE O DATA DA REALIZACAO DO PEDIDO E A DATA DE APROVACAO POR AVALIACAO
plot_temp_approv_lanc_target = ggplot(data = temp_diff_target, aes(x = temp_approv_lanc, colour = target)) + 
  stat_ecdf(geom = "step") +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Distribuição Acumulada do Tempo (dias) entre a Data de \n Aprovação do Pedido e a de Lançamento por Avaliação") +
  ylab("Acumulada") +
  xlab("Volume (m3)") +
  ggeasy::easy_center_title()

#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/temp_approv_lanc_target.png", plot = plot_temp_approv_lanc_target, height = 5, width = 5.5)

#GRAFICO DO TEMPO ENTRE O DATA DA REALIZACAO DO PEDIDO E A DATA DE APROVACAO POR AVALIACAO
plot_temp_lanc_estim_deliv_target = ggplot(data = temp_diff_target, aes(x = temp_lanc_estim_deliv, colour = target)) + 
  stat_ecdf(geom = "step") +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Distribuição Acumulada do Tempo (dias) entre a Data de \n Lançamento do Pedido e a de Entrega Estimada por Avaliação") +
  ylab("Acumulada") +
  xlab("Volume (m3)") +
  ggeasy::easy_center_title()

#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/temp_lanc_estim_deliv_target.png", plot = plot_temp_lanc_estim_deliv_target, height = 5, width = 5.5)


##################### ANALISE POR VARIAVEIS FINANCEIRAS ########################

#VALOR DA TRANSACAO POR AVALIACAO
finan_value_target = data %>%
  dplyr::select(target, payment_value) %>%
  dplyr::group_by(target) 

#DIST DO VALOR DA TRANSACAO POR AVALIACAO
plot_finan_value_target = ggplot(data = finan_value_target, aes(x=target, y=payment_value)) + 
  geom_boxplot(fill = "steelblue") +
  scale_y_continuous(limits = c(0, 1000)) +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Distribuição de Valores das Transações por Avaliação") +
  xlab("Target") +
  ylab("Valor da Transação") +
  ggeasy::easy_center_title()

#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/finan_value_target.png", plot = plot_finan_value_target, height = 5, width = 5.5)

#VARIAVEIS DE TIPO DE PAGAMENTO POR AVALIACAO
finan_pay_type_target = data %>%
  dplyr::select(target, payment_type) %>%
  dplyr::group_by(payment_type, target) %>%
  dplyr::summarise(reviews = dplyr::n()) %>%
  dplyr::mutate(prop = round((reviews/sum(reviews)) * 100, 2),
                value = paste0(format(reviews, big.mark = '.'), " (", format(prop, decimal.mark = ','), "%)")) %>%
  dplyr::arrange(payment_type)

#GRAFICO PARA APRESENTAR OS REVIEWS POR TIPO DE PAGAMENTO
plot_finan_pay_type_target = ggplot(data=finan_pay_type_target, aes(x = payment_type, y = reviews, fill = target)) +
  geom_bar(stat = "identity", position = "fill")+
  geom_text(aes(label = value), vjust = -0.3, size = 3.5, position = "fill")+
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Reviews por Tipo de Pagamento e suas Avaliações") +
  xlab("Tipo de Pagamento") +
  ylab("Proporção") +
  ggeasy::easy_center_title()

#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/finan_pay_type_target.png", plot = plot_finan_pay_type_target, height = 5, width = 5.5)

#VARIAVEIS DE PARCELAS POR AVALIACAO
finan_pay_inst_target = data %>%
  dplyr::select(target, payment_installments) %>%
  dplyr::group_by(target)

#GRAFICO PARA APRESENTAR OS REVIEWS POR TIPO DE PAGAMENTO
plot_finan_pay_inst_target = ggplot(data = finan_pay_inst_target, aes(x = payment_installments, colour = target)) +
  #geom_boxplot(fill = "steelblue") +
  stat_ecdf(geom = "step") +
  theme(panel.border = element_rect(color = "steelblue",
                                    fill = NA,
                                    size = 2)) +
  ggtitle(label = "Reviews por Número de Parcelas e suas Avaliações") +
  xlab("Target") +
  ylab("Proporção") +
  ggeasy::easy_center_title()

#SALVANDO ARQUIVO DO PLOT
ggsave(filename = "plots/finan_pay_inst_target.png", plot = plot_finan_pay_inst_target, height = 5, width = 5.5)


# FEATURE ENGINEERING -----------------------------------------------------

#FUNCAO QUE SUBSTITUI NA POR QUALQUER VALOR
repNA = function(x, valor)tidyr::replace_na(data = x, replace = valor)

#CRIACAO DE VARIAVEIS
data_new_var = data %>%
  dplyr::mutate(aprovado = ifelse(is.na(order_approved_at), yes = '0', no = '1'), 
                entregue = ifelse(is.na(order_delivered_customer_date), yes = '0', no = '1'), 
                atraso = dplyr::case_when(as.Date(order_delivered_customer_date) > as.Date(order_estimated_delivery_date) | is.na(order_delivered_customer_date) ~ '1', TRUE ~ '0'), 
                reg_cli = dplyr::case_when(customer_state %in% c('AC', 'AM', 'AP', 'PA', 'RO', 'RR', 'TO') ~ '1', 
                                           customer_state %in% c('AL', 'BA', 'CE', 'MA', 'PB', 'PE', 'PI', 'SE', 'RN') ~ '2', 
                                           customer_state %in% c('DF', 'GO', 'MS', 'MT') ~ '3',
                                           customer_state %in% c('ES', 'MG', 'RJ', 'SP') ~ '4', 
                                           customer_state %in% c('PR', 'SC', 'RS') ~ '5', 
                                           TRUE ~ '0'), 
                reg_vend = dplyr::case_when(seller_state %in% c('AC', 'AM', 'AP', 'PA', 'RO', 'RR', 'TO') ~ '1', 
                                            seller_state %in% c('AL', 'BA', 'CE', 'MA', 'PB', 'PE', 'PI', 'SE', 'RN') ~ '2', 
                                            seller_state %in% c('DF', 'GO', 'MS', 'MT') ~ '3',
                                            seller_state %in% c('ES', 'MG', 'RJ', 'SP') ~ '4', 
                                            seller_state %in% c('PR', 'SC', 'RS') ~ '5', 
                                            TRUE ~ '0'),
                reg_vend_cli = ifelse(test = reg_vend == reg_cli, yes = '1', no = '0'),
                tipo_pag = dplyr::case_when(payment_type == 'boleto' ~ '1', 
                                            payment_type == 'credit_card' ~ '2', 
                                            payment_type == 'debit_card' ~ '3', 
                                            payment_type == 'voucher' ~ "4", 
                                            TRUE ~ '0'), 
                pag1 = ifelse(test = payment_installments == 1, yes = '1', no = '0'), 
                seller_idhm_cat = dplyr::case_when(seller_idhm < 0.5 ~ '1', 
                                                   seller_idhm >= 0.5 & seller_idhm < 0.8 ~ '2', 
                                                   seller_idhm >= 0.8 ~ '3', 
                                                   TRUE ~ '0'), 
                seller_idhm_r_cat = dplyr::case_when(seller_idhm_r < 0.5 ~ '1', 
                                                     seller_idhm_r >= 0.5 & seller_idhm_r < 0.8 ~ '2', 
                                                     seller_idhm_r >= 0.8 ~ '3', 
                                                     TRUE ~ '0'), 
                seller_idhm_l_cat = dplyr::case_when(seller_idhm_l < 0.5 ~ '1', 
                                                     seller_idhm_l >= 0.5 & seller_idhm_l < 0.8 ~ '2', 
                                                     seller_idhm_l >= 0.8 ~ '3', 
                                                     TRUE ~ '0'),
                seller_idhm_e_cat = dplyr::case_when(seller_idhm_e < 0.5 ~ '1', 
                                                     seller_idhm_e >= 0.5 & seller_idhm_e < 0.8 ~ '2', 
                                                     seller_idhm_e >= 0.8 ~ '3', 
                                                     TRUE ~ '0'), 
                customer_idhm_cat = dplyr::case_when(customer_idhm < 0.5 ~ '1', 
                                                     customer_idhm >= 0.5 & customer_idhm < 0.8 ~ '2', 
                                                     customer_idhm >= 0.8 ~ '3', 
                                                     TRUE ~ '0'), 
                customer_idhm_r_cat = dplyr::case_when(customer_idhm_r < 0.5 ~ '1', 
                                                       customer_idhm_r >= 0.5 & customer_idhm_r < 0.8 ~ '2', 
                                                       customer_idhm_r >= 0.8 ~ '3', 
                                                       TRUE ~ '0'), 
                customer_idhm_l_cat = dplyr::case_when(customer_idhm_l < 0.5 ~ '1', 
                                                       customer_idhm_l >= 0.5 & customer_idhm_l < 0.8 ~ '2', 
                                                       customer_idhm_l >= 0.8 ~ '3', 
                                                       TRUE ~ '0'),
                customer_idhm_e_cat = dplyr::case_when(customer_idhm_e < 0.5 ~ '1', 
                                                       customer_idhm_e >= 0.5 & customer_idhm_e < 0.8 ~ '2', 
                                                       customer_idhm_e >= 0.8 ~ '3', 
                                                       TRUE ~ '0')
  ) %>%
  dplyr::select(target, aprovado, entregue, atraso, reg_cli, reg_vend, reg_vend_cli, pag1, tipo_pag, 
                seller_idhm_cat, seller_idhm_r_cat, seller_idhm_e_cat, seller_idhm_l_cat, seller_agua_esgoto,
                seller_espvida, seller_e_anosestudo, seller_t_agua, seller_t_banagua, seller_t_luz,
                customer_idhm_cat, customer_idhm_r_cat, customer_idhm_e_cat, customer_idhm_l_cat, 
                customer_idhm_cat, customer_idhm_r_cat, customer_idhm_e_cat, customer_idhm_l_cat, customer_agua_esgoto,
                customer_espvida, customer_e_anosestudo, customer_t_agua, customer_t_banagua, customer_t_luz) %>%
  dplyr::mutate(dplyr::across(c(dplyr::starts_with('seller_t'), 
                                dplyr::starts_with('seller_e'),
                                dplyr::starts_with('seller_a'), 
                                dplyr::starts_with('customer_t'), 
                                dplyr::starts_with('customer_e'),
                                dplyr::starts_with('customer_a')), ~repNA(.x, mean(.x, na.rm = T)), .name = "{col}"))

# PRE PROCESS -----------------------------------------------------------

#DEFININDO SEMENTE
set.seed(123)

#DIVIDINDO A POPULACAO EM TREINO E TESTE
data_split = rsample::initial_split(data = data_new_var, prop = 0.7, strata = target)

#DATA PREPARATION
data_recipe = rsample::training(x = data_split) %>%
  recipes::recipe(target ~ .) %>%
  recipes::step_novel(recipes::all_nominal_predictors(), -recipes::all_outcomes()) %>% #TRANSFORMANDO AS VARIAVEIS NOMINAIS EM FATOR
  recipes::step_dummy(recipes::all_nominal_predictors(), -recipes::all_outcomes()) %>% #CRIANDO VARIAVEIS DUMMY (ONE-HOT ENCODING)
  recipes::step_corr(recipes::all_nominal_predictors(), threshold = 0.7, method = "spearman") %>% #REMOVENDO VARIAVEIS NOMINAIS ALTAMENTE CORRELACIONADAS
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
                                                      roc_auc, 
                                                      sens),
                      control = tune::control_resamples(save_pred = TRUE)
  ) 

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