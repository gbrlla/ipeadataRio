# ipeadataRio

*Este pacote será utilizado unicamente pelo grupo de atualização do IpeaData.*

[Acesse aqui o portal do Ipeadata](http://www.ipeadata.gov.br)

![alt text](https://brasilfatosedados.files.wordpress.com/2014/12/fonte-01b-ipeadata.png?w=265)

## Instalação

O pacote encontra-se disponível somente no **GitLab**. Sua instalação e carga é feita a partir de:

```{r eval=FALSE}
devtools::install_git("https://gitlab.com/ipeadata/ipeadataRio", force = TRUE)
library(ipeadataRio)
````

## Uso

O pacote é composto pelas funções:

* *situavar*: Retorna a situação das séries requeridas do IpeaData.

* *exportar*: Exporta relatórios .csv gerados a partir de *situavar*.

## Requisitos

É necessário estar com o *Open Database Connectivity* (ODBC) configurado para permissão de acesso ao banco de dados IpeaData. 

**Somente para uso interno**
