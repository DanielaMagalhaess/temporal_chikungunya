library(forecast)
library(fpp3)
library(ggplot2)
library(readxl)
library(feasts)

# DADOS

setwd("C:/Users/danie/Documents/PUC/Mestrado/simulacao/dfs")

dados <- read_excel("df_geral_mes_ano.xlsx")

# ANALISE DA SERIE

dados_ts <- dados |>
  mutate(`Mês/ano` = yearmonth(`Mês/ano`)) |>
  as_tsibble(index = `Mês/ano`)


stl_dcmp <- dados_ts |>
  model(
    STL(taxa ~ trend(window = 24) + 
          season(window = "periodic"),
        robust = TRUE)
  ) |>
  components()

stl_dcmp |> autoplot()

dados_ts |> gg_subseries(taxa) + theme_minimal()

dados_ts |> gg_season(taxa, labels = "right") + 
  geom_point() + theme_minimal()



dados_ts %>%
  gg_tsdisplay(
    difference(taxa, lag = 12) |> difference(differences = 1),
    plot_type = "partial",  # <-- isso ativa o PACF no lugar do histograma
    lag_max = 36
  ) +
  labs(title = "Double Differenced", y = "")


taxa_diff <- dados_ts |>
  mutate(taxa_d = difference(taxa, lag = 12) |> difference(differences = 1)) |>
  drop_na()

# ACF
taxa_diff |> ACF(taxa_d, lag_max = 36) |> autoplot() + theme_minimal()

# PACF
taxa_diff |> PACF(taxa_d, lag_max = 36) |> autoplot() + theme_minimal()



# MODELOS - ETS

dados_treino <- dados_ts |>
  filter(year(`Mês/ano`) < 2025)

pneumo_fit_ets <- dados_treino %>%
  model(
    ets_auto = ETS(taxa)
  )

# Passo 2: veja o que ele escolheu
pneumo_fit_ets |> report()

# Previsão para 2025 (12 meses)
pneumo_fc_ets <- forecast(pneumo_fit_ets, h = 12)

# Visualizar previsão vs dados reais de 2025
pneumo_fc_ets |>
  filter(.model == "ets_auto") |>
  autoplot(dados_ts, level = NULL) +
  labs(title = "Taxa de Pneumonia em Idosos - Previsão 2025",
       y = "Taxa")


glance(pneumo_fit_ets) |> arrange(AICc) |> select(.model:BIC)

pneumo_fit_ets |> select(ets_auto) |> gg_tsresiduals(lag = 36)

# residuos não sao ruido branco

augment(pneumo_fit_ets) |>
  filter(.model == "ets_auto") |>
  features(.innov, ljung_box, lag = 24, dof = 3)


# MODELOS - ARIMA

































# codigo antigo que estou olhando







#Implementações:
#1. Começar a testar com o mesmo período que o horizonte (h)

#2. Começar a modelar o Alfa (componente de nível), Beta (componente Tendência)
# gama (componente sazonal), que dão os pesos das últimas observações (qnt mais próximo de 1)

#3. Critérios de informação para seleção de modelos. Ajuste de qtd de parâmetros através 
# do AIC (akaike's Information Criterion), ou AICc (Corrected Akaike - para séries pequenas)
# ou BIC (Bayesian information Criterion). Já aparece no ETS. O ideal é minimizar esse critério de informação, 
# escolher aquele com menor AIC

#4. Para um p-valor (ljung_box) bem menor que alfa, rejeitar hipótese nula (H1). Agora, se
# o p-valor for bem maior que alfa, não-rejeito a hipótese nula (H0) -> dados são descorrelatados.

#5.1. Para um FAC que possui um decaimento exponencial, com um corte brusco da FACP no lag p, temos um AR (p),
# um modelo auto regressivo de ordem p.

#5.2. Para um FAC com corte brusco no lag q, e um decaimento exponencial do FACP, temos um MA (q), 
# um modelo de média móvel de ordem q.

#5.3. FACP analisa o AR (autoregressivo), enquanto o FAC analisa o MA (media movel)




##################### Funções para programa:##############################################


Data = function(dados,pneumo) {
  
  return(na.omit(select(dados, Ano, Mês, pneumo)))
  
}

Infos = function(dados, horizon) {
  
  return( list(ano_i = dados[1,1], 
               mes_i = dados[1,2], 
               ano_f = dados[nrow(dados), 1],
               mes_f = dados[nrow(dados), 2], 
               ano_train_f = dados[nrow(dados) - horizon, 1],
               mes_train_f = dados[nrow(dados) - horizon, 2], 
               ano_test_i = dados[nrow(dados) - horizon + 1, 1],
               mes_test_i = dados[nrow(dados) - horizon + 1, 2]))
  
}



# Horizonte de projeção
h = 24


###################### Dados #########################

info_dados_pneumo = Infos(Data(dados, "qtd_idosos_pneumo"), h)

###################### Analise da série #########################

dados$mes <- as.Date(dados$mes)

dados_ts = dados %>% 
  tsibble(index = mes) %>% 
  mutate(mes = yearmonth(mes))

pneumo_idosos_ts = ts(data = Data(dados, "qtd_idosos_pneumo")$qtd_idosos_pneumo, 
                   start = c(info_dados_pneumo$ano_i, info_dados_pneumo$mes_i),
                   frequency = 12, 
                   end = c(info_dados_pneumo$ano_f, info_dados_pneumo$mes_f))

dados_ts |> gg_subseries(taxa) + theme_minimal()

dados_ts |> gg_season(taxa, labels = "right") + 
  geom_point() + theme_minimal()


########################################### PROGRAMA DE PREVISÃO:####

################# 1. Dados Pneumo:######



######################################### 2. Análise Descritiva #########

# 2.1. Função ggmonthplot do pacote forecast para identificar sazonalidade ####

ggmonthplot(pneumo_idosos_ts) + theme_minimal() 


ggseasonplot(pneumo_idosos_ts, year.labels = TRUE) + 
  geom_point() + theme_minimal()

# 2.2. Decomposição da série STL ####

#Observando o gráfico dos dados, podemos inferir sobre três componentes:

#a) Uma componente linear de crescimento dos dados bem suave
#b) Uma componente sazonal nos dados
#c) Uma componente aleatória nos dados

#Para observar as três componentes, podemos decompor a série usando o seguinte comando:

stl_dcmp = dados_ts |> model(
  STL(log(qtd_idosos_pneumo) ~ trend(window = 24) + 
        season(window = "periodic"), 
      robust = T)) |>
  components() 

stl_dcmp |> autoplot()


# 2.3. Autocorrelação para analisar os ARIMA's ajustáveis:####

# Análise de FAC e FACP com log:
dados_ts %>%
  gg_tsdisplay(difference(log(qtd_idosos_pneumo), lag = 12) |> difference(differences = 1), 
               plot_type="partial", 
               lag_max = 36) + 
  labs(title = "Double Differenced", y = "")


'Possibilidades de AR e MA segundo a PACF e ACF, com (d,D) = 1:
MA q-> 0,2,3*
MA Q-> 0,1

AR p-> 0,2,3
AR P-> 0,1,2*'





######################################### 3. Aplicações dos Modelos #########

  # 3.1. Modelo ARIMA: ####

' a.log) Pela ACF, podemos observar um spike no lag 2, o que nos indica um MA (2) 
na parte não sazonal, e um spike relevante no lag 12, que indica um MA (1) na parte 
sazonal já que não há outro spike relevante. -> ARIMA (0, 1, 2)(0, 1, 1)'

'b.log) Começando a análise pelo PACF, vemos que no lag 2 há um spike, indicando um 
AR(2) para a parte não sazonal, e também há spike no lag 12 da parte sazonal,
indicando um AR(1) para esse ponto. No ACF poderíamos partir de um MA(2) para a parte 
não sazonal e um MA(1) para a parte sazonal. 
-> ARIMA (2, 1, 2)(1, 1, 1).'

' Com esses dois modelos iniciais, vamos compará-los com o modelo de auto arima para
identificar aquele que tem o menos AICc.'
' O modelo automatico utiliza um stepwise e uma approx = F para fazer o R encontrar um 
bom modelo que se encaixe aos dados, apesar de levar um tempo maior para rodar o 
modelo, isso não é um problema dado que estamos modelando apenas uma curva.'


pneumo_fit_arima = dados_ts |>
  model(
    arima012011 = ARIMA(log(qtd_idosos_pneumo) ~ 0 + pdq(0,1,2) + PDQ(0,1,1)),
    arima212111 = ARIMA(log(qtd_idosos_pneumo) ~ 0 + pdq(2,1,2) + PDQ(1,1,1)),
    auto_arima = ARIMA(log(qtd_idosos_pneumo) ~ 0 + pdq(0:3, 1, 0:3) + PDQ(0:2, 1, 0:1),
                       stepwise = F, 
                       approximation = F)
  )


pneumo_fit_arima |> pivot_longer(everything(), names_to = "Model name",
                           values_to = "Orders")


glance(pneumo_fit_arima) |> arrange(AICc) |> select(.model:BIC)

# arima012011 com menor AICc. -> ARIMA (0,1,2)(0,1,1)

pneumo_fit_arima |> select(arima012011) |> gg_tsresiduals(lag = 36)

#Análise dos resíduos do modelo escolhido:

'Os resíduos não apresentam spikes no ACF. 
Faremos o teste de ljung-box para validar o p-valor alto, 
com um grau de liberdade = 3 (p+q+P+Q)'

augment(pneumo_fit_arima) |>
  filter(.model == "arima012011") |>
  features(.innov, ljung_box, lag = 24, dof = 3)

'O p-valor está bem acima de 0.05. Com isso, o p-valor é suficientemente alto para considerarmos os
resíduos um ruído branco'

forecast(pneumo_fit_arima, h = h) |>
  filter(.model == "arima012011") |>
  autoplot(dados_ts, level = NULL) + 
  labs(title = "Quantidade de Idosos com Pneumonia",
       y="Idosos")

  # 3.2. Modelo ETS: ####

pneumo_fit_ets <- dados_ts %>% 
  model( 
    ets1 = ETS(log(qtd_idosos_pneumo) ~ error("M") 
               + trend("Ad", alpha = 0.6, beta = 0.0001, phi = 0.9773) 
               + season("M", gamma = 0.0001)),
    ets_auto = ETS(log(qtd_idosos_pneumo))
  )

pneumo_fit_ets |> pivot_longer(everything(), names_to = "Model name",
                                 values_to = "Orders")


glance(pneumo_fit_ets) |> arrange(AICc) |> select(.model:BIC)

# ets1 com menor AICc. -> ETS (M,Ad,M) -> a = 0.6; b,g = 0.0001; phi = 0.9773

pneumo_fit_ets |> select(ets1) |> gg_tsresiduals(lag = 36)

#Análise dos resíduos do modelo escolhido:

'Os resíduos apresentam spikes no lag 2 e 3 do ACF. 
Faremos o teste de ljung-box para validar o p-valor alto, 
com um grau de liberdade = 3 (phi + beta + gamma)'

augment(pneumo_fit_ets) |>
  filter(.model == "ets1") |>
  features(.innov, ljung_box, lag = 24, dof = 3)

'# Pelo teste de Ljung-box, o modelo do ETS falha
com um p-valor muito menor que 0.05, e não podemos considerar que os
resíduos são um ruído branco. No entanto, tivemos o menor AICc, e seguiremos com
esse modelo, mesmo que não passe em todos os testes.'

forecast(pneumo_fit_ets, h = h) |>
  filter(.model == "ets1") |>
  autoplot(dados_ts, level = NULL) + 
  labs(title = "Quantidade de Idosos com Pneumonia",
       y="Idosos")


  # 3.3. Treinamento dos modelos: ####

treinamento_pneumo_idosos = dados_ts |>
  filter(
    year(mes) >= as.integer(info_dados_pneumo$ano_i) & 
      year(mes) <= as.integer(info_dados_pneumo$ano_train_f) & 
      month(mes) >= as.integer(info_dados_pneumo$mes_i) & 
      month(mes) <= as.integer(info_dados_pneumo$mes_train_f)
  ) 

teste_pneumo_idosos = dados_ts |>
  filter(
    year(mes) >= as.integer(info_dados_pneumo$ano_test_i) & 
      year(mes) <= as.integer(info_dados_pneumo$ano_f) & 
      month(mes) >= as.integer(info_dados_pneumo$mes_test_i) & 
      month(mes) <= as.integer(info_dados_pneumo$mes_f)
  )

  # 3.3.1. Treinamento do modelo ARIMA escolhido:####

arima_fit_train = treinamento_pneumo_idosos |>
  model(ARIMA(log(qtd_idosos_pneumo) ~ 0 + pdq(0, 1, 2) + PDQ(0, 1, 1)))

report(arima_fit_train)

arima_fit_train |> gg_tsresiduals(lag_max = 36)

augment(arima_fit_train) |>
  features(.innov, ljung_box, lag = 24, dof = 3)

  # 3.3.2. Treinamento do modelo ETS escolhido:####

ets_fit_train = treinamento_pneumo_idosos |>
  model(ETS(log(qtd_idosos_pneumo) ~ error("M") 
            + trend("Ad", alpha = 0.6, beta = 0.0001, phi = 0.9773) 
            + season("M", gamma = 0.0001)))

report(ets_fit_train)

ets_fit_train |> gg_tsresiduals(lag_max = 36)

augment(ets_fit_train) |>
  features(.innov, ljung_box, lag = 24, dof = 3)


######################################### 4. Comparação dos modelos treinados #########

bind_rows(
  arima_fit_train |> accuracy(),
  ets_fit_train |> accuracy(),
  arima_fit_train |> forecast(h = h) |> accuracy(dados_ts),
  ets_fit_train |> forecast(h = h) |> accuracy(dados_ts)
) |>
  select(-ME, -MPE, -ACF1)

  # 4.1. Comparação dos modelos de treinamento no gráfico (teste + ARIMA + ETS):####

treinamento_ts <- ts(
  data = treinamento_pneumo_idosos$qtd_idosos_pneumo,
  start = c(year(min(treinamento_pneumo_idosos$mes)), 
            month(min(treinamento_pneumo_idosos$mes))),  
  frequency = 12
)

teste_ts <- ts(
  data = teste_pneumo_idosos$qtd_idosos_pneumo,
  start = c(year(min(teste_pneumo_idosos$mes)), 
            month(min(teste_pneumo_idosos$mes))),  
  frequency = 12
)

previsao_train_arima = forecast(arima_fit_train, h = h)|> as_tsibble(index = mes)

previsao_train_arima_ts <- ts(
  data = previsao_train_arima$.mean,
  start = c(year(min(previsao_train_arima$mes)), 
            month(min(previsao_train_arima$mes))),  
  frequency = 12
)

previsao_train_ets = forecast(ets_fit_train, h = h)|> as_tsibble(index = mes)

previsao_train_ets_ts <- ts(
  data = previsao_train_ets$.mean,
  start = c(year(min(previsao_train_ets$mes)), 
            month(min(previsao_train_ets$mes))),  
  frequency = 12
)


autoplot(treinamento_ts, xlab = "Tempo", ylab = "Qtd de Idosos") + 
  autolayer(previsao_train_arima_ts, series = "Previsão Treinamento ARIMA",
            linetype = "dashed", na.rm = TRUE, size = 0.7) +
  autolayer(teste_ts, series = "Teste Real", na.rm = TRUE, color = "black") +
  autolayer(previsao_train_ets_ts, series = "Previsão Treinamento ETS", 
            linetype = "dashed", na.rm = TRUE, size = 0.7) +
  labs(
    title = "Quantidade de Idosos com Pneumonia",
    y = "Idosos",
    x = "Ano",
    color = "Forecasts"
  ) +
  coord_cartesian(ylim = c(0, NA)) + # Ajuste do eixo Y
  theme_minimal()







