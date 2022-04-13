# ANALISE EXPLORATORIA ------------------------------------------------------------

#LIMPANDO O AMBIENTE
rm(list = ls())

#DEFININDO DIRETORIO PADRAO
setwd("~/git/tcc/source")

#EXECUTANDO ARQUIVO DE PREAMBULO (INSTALACAO E CARREGAMENTO DE PACOTES)
source("script/preambulo.R")

#CARREGANDO A BASE
load("data/data.Rdata", verbose = T)

############################## ANALISE DE NULOS ################################

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

######################### ANALISE DE BALANCEAMENTO #############################

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