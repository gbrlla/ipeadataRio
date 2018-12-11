# --------------------------------------------------------- #
# |   INSTITUTO DE PESQUISA ECONOMICA APLICADA - IPEA     | #
# |                    PROJETO IPEADATA                   | #
# --------------------------------------------------------- #
# |   COORDENADOR:  Christian Vonbun                      | #
# --------------------------------------------------------- #
# |   PROGRAMADOR: Jessyka Amorim P Goltara               | #
# --------------------------------------------------------- #
# |             VarPerc                                   | #
# --------------------------------------------------------- #

# --------------------------------------------------------- #
# A funcao arrange ordena o DF em ordem crescente segundo a variavel de escolha
# A funcao mutate cria uma nova variavel
# A funcao select seleciona as variaveis de interesse
# --------------------------------------------------------- #

VarPerc <- function(data, seroutput = NULL) {

  # Permitido somente 1 serie
  if (ncol(data) != 2) {
    stop('Permitido somente uma serie')
  }

  # Calculo:  Y_{t} = ((X_{t} / X_{t-1}) - 1) * 100
  ## Saida: Ordena data > Calcula Y_{t} >
  ##        Seleciona variaveis de interesse >
  ##        Remove valores NA
  output <-
    data %>%
    dplyr::arrange(data$VALDATA) %>%
    dplyr::mutate(VALVALOR = (data[, 2] / dplyr::lag(data[, 2]) - 1) * 100) %>%
    dplyr::select(data$VALDATA, data$VALVALOR) %>%
    dplyr::filter(!is.na(data$VALVALOR))

  # Permitido somente 1 serie
  if (!is.null(seroutput)) {
    names(output)[2] <- seroutput
  }

  # Resultado
  return(output)

}
