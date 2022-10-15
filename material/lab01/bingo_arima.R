#' Nosso objetivo é descobrir qual a ordem do ARIMA usando análise descritiva

# gerando os dados --------------------------------------------------------

ar <- sample(0:2, 1)
ma <- sample(0:2, 1)
dif <- sample(0:2, 1)

ar_parm <- runif(ar)
ma_parm <- runif(ma)

dados <- data.frame(
  mes = 1:300,
  vendas = arima.sim(list(
    order = c(ar,dif,ma),
    ma = ma_parm,
    ar = ar_parm
  ), n = 300)[-1]
)

# montando os dados -------------------------------------------------------

dados_tsibble <- dados |>
  dplyr::mutate(
    mes = as.Date("1995-05-01") + months(mes),
    mes = tsibble::yearmonth(mes)
  ) |>
  tsibble::as_tsibble(index = mes)

# descritiva --------------------------------------------------------------

dados_tsibble |>
  gg_tsdisplay(vendas, plot_type = "partial")

dados_tsibble |>
  fabletools::features(
    vendas,
    list(
      feasts::unitroot_kpss,
      feasts::unitroot_ndiffs
    )
  )

dados_tsibble |>
  dplyr::mutate(dif_vendas = tsibble::difference(vendas)) |>
  gg_tsdisplay(dif_vendas, plot_type = "partial", lag_max = 30)


# modelagem ---------------------------------------------------------------

fit <- dados_tsibble |>
  fabletools::model(
    arima_manual = fable::ARIMA(vendas ~ 1 + pdq(2,0,2) + PDQ(0,0,0)),
    stepwise = fable::ARIMA(vendas ~ 1 + PDQ(0,0,0)),
    search = fable::ARIMA(vendas ~ 1 + PDQ(0,0,0), stepwise = FALSE)
  )

dplyr::glimpse(fit)

fit |>
  broom::glance() |>
  dplyr::select(.model, AIC) |>
  dplyr::arrange(AIC)
