# --------------------------------------------------------- #
# |   INSTITUTO DE PESQUISA ECONOMICA APLICADA - IPEA     | #
# |                    PROJETO IPEADATA                   | #
# --------------------------------------------------------- #
# |   COORDENADOR: ERIVELTON P. GUEDES                    | #
# --------------------------------------------------------- #
# |   PROGRAMADOR: LUIZ EDUARDO S. GOMES                  | #
# --------------------------------------------------------- #
# |   CONSTRUINDO PACOTE                                  | #
# --------------------------------------------------------- #

# CARREGANDO PACOTES ----------------------------------------

pacotes<-c("devtools","roxygen2")

for (i in 1:length(pacotes))
{
  if (length(names(installed.packages()[,1])[names(installed.packages()[,1])==pacotes[i]])==0)
  {install.packages(pacotes[i], repos="http://cran.fiocruz.br/")}
  library(pacotes[i],character.only = TRUE)
}
rm(i,pacotes)

# CRIANDO PACOTE ----------------------------------------

#------ 1. Criando pacote direcionando a pasta compartilhada pelo Git
devtools::setup("C:/Users/b207056565/Desktop/Ipea/PKG_ipeadataRio/ipeadataRio")

#------ 2. Documentando
devtools::document()

#------ 2.1 Documentando 2
roxygen2::roxygenise()

#------ 3 Adicionando licenca
devtools::use_mit_license()

#------ 4 Adicionando dados
users <- read.csv2("C:/Users/b207056565/Desktop/Ipea/PKG_ipeadataRio/DB/users.csv")
devtools::use_data(users)
# devtools::use_data(users,internal = TRUE)

codpaisesSECEX12FOB12 <- read.csv2("C:/Users/b207056565/Desktop/Ipea/PKG_ipeadataRio/DB/codpaisesSECEX12FOB12.csv")
devtools::use_data(codpaisesSECEX12FOB12)

codterritCONFAZ12 <- read.csv2("C:/Users/b207056565/Desktop/Ipea/PKG_ipeadataRio/DB/codterritCONFAZ12.csv")
devtools::use_data(codterritCONFAZ12)

#------ 5 Checando pacote
devtools::check()

# Use sempre devtools::check() para checar se seu pacote está 100% bem construído.
# Use devtools::use_package() para usar funções de outros pacotes.
# Use devtools::use_data() para adicionar dados ao seu pacote.
