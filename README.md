# Atlas da Questão Agrária no Norte de Minas
Construção do banco de dados do Atlas da Questão Agrária no Norte de Minas

## Descrição

* __Recorte espacial__: mesorregião do Norte de Minas
* __Menor granularidade__: municípios

## População

* População total, urbana e rural obtidas através da [tabela 1378](https://sidra.ibge.gov.br/Tabela/1378) do SIDRA.

## PIB

* PIB dos municípios de 2017 obtido através da [tabela 5938](https://sidra.ibge.gov.br/Tabela/5938) do SIDRA.

## IDH-M, coeficiente de Gini e índice de Theil

* O IDH-M, coeficiente de Gini e índice de Theil foram obtidos através do [Atlas do Desenvolvimento Humano](http://atlasbrasil.org.br/2013/pt/).

## Estabelecimentos agropecuários

* Total de estabelecimentos ([Tabela 6778](https://sidra.ibge.gov.br/Tabela/6778))
* Estabelecimentos da agricultura familiar ([Tabela 6778](https://sidra.ibge.gov.br/Tabela/6778))
* Estabelecimentos no PRONAF ([Tabela 6778](https://sidra.ibge.gov.br/Tabela/6778))
* Estabelecimentos muito grandes (acima de 500 ha) ([Tabela 6778](https://sidra.ibge.gov.br/Tabela/6778))
* Estabelecimentos muito pequenos (menos de 1 ha) ([Tabela 6778](https://sidra.ibge.gov.br/Tabela/6778))
* Estabelecimentos que obtiveram financiamento ([Tabela 6896](https://sidra.ibge.gov.br/Tabela/6896))

## Pecuária

### Bovinos

* Efetivo por município ([Tabela 6910](https://sidra.ibge.gov.br/Tabela/6910))
* Cabeças vendidas ([Tabela 6910](https://sidra.ibge.gov.br/Tabela/6910))
* Litros de leite vendidos ([Tabela 6912](https://sidra.ibge.gov.br/Tabela/6912))

### Suínos

* Efetivo por município ([Tabela 6926](https://sidra.ibge.gov.br/Tabela/6926))
* Cabeças vendidas ([Tabela 6926](https://sidra.ibge.gov.br/Tabela/6926))

### Aves

* Efetivo por município ([Tabela 6941](https://sidra.ibge.gov.br/Tabela/6941))
* Cabeças vendidas ([Tabela 6941](https://sidra.ibge.gov.br/Tabela/6941))
* Ovos vendidos ([Tabela 6941](https://sidra.ibge.gov.br/Tabela/6941))

### Caprinos

* Efetivo por município ([Tabela 6928](https://sidra.ibge.gov.br/Tabela/6928))
* Cabeças vendidas ([Tabela 6928](https://sidra.ibge.gov.br/Tabela/6928))
* Litros de leite vendidos ([Tabela 6928](https://sidra.ibge.gov.br/Tabela/6928))

### Ovinos

* Efetivo por município ([Tabela 6930](https://sidra.ibge.gov.br/Tabela/6930))
* Cabeças vendidas ([Tabela 6930](https://sidra.ibge.gov.br/Tabela/6930))
* Litros de leite vendidos ([Tabela 6930](https://sidra.ibge.gov.br/Tabela/6930))

### Bubalinos

* Efetivo por município ([Tabela 6918](https://sidra.ibge.gov.br/Tabela/6918))
* Cabeças vendidas ([Tabela 6918](https://sidra.ibge.gov.br/Tabela/6918))
* Litros de leite vendidos ([Tabela 6918](https://sidra.ibge.gov.br/Tabela/6918))

## Agricultura

### Lavoura temporária

* Valor da venda ([Tabela 6957](https://sidra.ibge.gov.br/Tabela/6957))
* Área colhida ([Tabela 6957](https://sidra.ibge.gov.br/Tabela/6957))

### Lavoura permanente

* Valor da venda ([Tabela 6955](https://sidra.ibge.gov.br/Tabela/6955))
* Área colhida ([Tabela 6955](https://sidra.ibge.gov.br/Tabela/6955))
* Valor da venda por estabelecimentos com menos de 50 pés ([Tabela 6955](https://sidra.ibge.gov.br/Tabela/6955))

## Máquinas agrícolas

* Número de máquinas (tratores, semeadeiras etc.) por município

## Poços e cisternas

* ...


## Programa de Aquisição de Alimentos

[PAA Dados Abertos](https://aplicacoes.mds.gov.br/sagi/paa/visi_paa_geral/pg_principal.php?url=abertura)

## Territórios rurais e da cidadania

[PDA](http://dados.gov.br/dataset/composicao-dos-territorios-rurais-e-da-cidadania)

## Dicionário

|VARIAVEL                               |DESCRICAO                                                                                                           |
|:--------------------------------------|:-------------------------------------------------------------------------------------------------------------------|
|COD_MUN                                |Código IBGE do município                                                                                            |
|COD_UF                                 |Código IBGE do estado                                                                                               |
|NOME_UF                                |Nome do estado                                                                                                      |
|COD_RGINT                              |Código da região intermediária                                                                                      |
|NOME_RGINT                             |Nome da região intermediária                                                                                        |
|COD_RGI                                |Código da região imediata                                                                                           |
|NOME_RGI                               |Nome da região imediata                                                                                             |
|COD_MESO                               |Código da mesorregião                                                                                               |
|NOME_MESO                              |Nome da mesorregião                                                                                                 |
|COD_MICRO                              |Código da microrregião                                                                                              |
|NOME_MICRO                             |Nome da microrregião                                                                                                |
|NOME_MUN                               |Nome do município                                                                                                   |
|POPULACAO_RURAL                        |População rural do município em 2010                                                                                |
|POPULACAO_TOTAL                        |População total do município em 2010                                                                                |
|POPULACAO_URBANA                       |População urbana do município em 2010                                                                               |
|PIB                                    |Produto Interno Bruto do município em 2017                                                                          |
|VAB                                    |Valor Adicionado Bruto total do município em 2017                                                                   |
|VAGRO                                  |Valor Adicionado Bruto da agropecuária do município em 2017                                                         |
|VINDU                                  |Valor Adicionado Bruto da indústria do município em 2017                                                            |
|VSERV                                  |Valor Adicionado Bruto do setor de serviços do município em 2017                                                    |
|IDHM                                   |Índice de Desenvolvimento Humano do município                                                                       |
|IDHM_E                                 |Índice de Desenvolvimento Humano - Educação do município                                                            |
|IDHM_L                                 |Índice de Desenvolvimento Humano - Longevidade do município                                                         |
|IDHM_R                                 |Índice de Desenvolvimento Humano - Renda do município                                                               |
|GINI                                   |Coeficiente de Gini da renda                                                                                        |
|THEIL                                  |Índice de Theil                                                                                                     |
|ESTABELECIMENTOS                       |Estabelecimentos agropecuários no município em 2017                                                                 |
|FAMILIAR                               |Estabelecimentos agropecuários classificados como da agricultura familiar em 2017                                   |
|PRONAF_B                               |Estabelecimentos agropecuários classificados como da agricultura familiar (PRONAF B) em 2017                        |
|PRONAF_V                               |Estabelecimentos agropecuários classificados como da agricultura familiar (PRONAF V) em 2017                        |
|ESTAB_PEQUENOS                         |Estabelecimentos agropecuários com menos de 1 hectare em 2017                                                       |
|ESTAB_GRANDES                          |Estabelecimentos agropecuários com mais de 500 hectares em 2017                                                     |
|ESTAB_TOTAL_FINANCIAMENTO              |Quantidade de estabelecimentos que obtiveram algum financiamento em 2017                                            |
|ESTAB_FAMILIARL_FINANCIAMENTO          |Quantidade de estabelecimentos classificados como da agricultura familiar que obtiveram algum financiamento em 2017 |
|N_BOVINOS                              |Quantidade de cabeças de bovinos no município em 2017                                                               |
|VENDA_BOVINOS                          |Quantidade de bovinos vendidos no município em 2017                                                                 |
|VENDA_LEITE_BOVINOS                    |Quantidade de leite de vaca vendida no município em 2017 (litros)                                                   |
|N_SUINOS                               |Quantidade de cabeças de suínos no município em 2017                                                                |
|VENDA_SUINOS                           |Quantidade de suínos vendidos no município em 2017                                                                  |
|N_AVES                                 |Quantidade de cabeças de aves no município em 2017                                                                  |
|VENDA_AVES                             |Quantidade de aves vendidas no município em 2017                                                                    |
|VENDA_OVOS                             |Quantidade de ovos de galinha vendidos no município em 2017                                                         |
|N_CAPRINOS                             |Quantidade de cabeças de caprinos no município em 2017                                                              |
|VENDA_CAPRINOS                         |Quantidade de caprinos vendidos no município em 2017                                                                |
|VENDA_LEITE_CAPRINOS                   |Quantidade de leite de cabra vendida no município em 2017                                                           |
|N_OVINOS                               |Quantidade de cabeças de ovinos no município em 2017                                                                |
|VENDA_OVINOS                           |Quantidade de ovinos vendidos no município em 2017                                                                  |
|VENDA_LEITE_OVINOS                     |Quantidade de leite de ovelha vendida no município em 2017                                                          |
|VENDA_LA_OVINOS                        |Quantidade de lã vendida no município em 2017                                                                       |
|N_BUBALINOS                            |Quantidade de cabeças de bubalinos no município em 2017                                                             |
|VENDA_BUBALINOS                        |Quantidade de búfalos vendidos no município em 2017                                                                 |
|VENDA_LEITE_BUBALINOS                  |Quantidade de leite vendida no município em 2017                                                                    |
|VALOR_VENDA_TEMPORARIA                 |Valor total da venda da lavoura temporária no município em 2017                                                     |
|AREA_COLHIDA_TEMPORARIA                |Área total colhida da lavoura temporária no município em 2017                                                       |
|VALOR_VENDA_PERMANENTE                 |Valor total da venda da lavoura permanente no município em 2017                                                     |
|AREA_COLHIDA_PERMANENTE                |Área total colhida da lavoura permanente no município em 2017                                                       |
|VALOR_VENDA_PERMANENTE_MENOS_DE_50_PES |Valor total da venda da lavoura permanente por estabelecimentos com menos de 50 pés no município em 2017            |
|N_MÁQUINAS                             |Quantidade de tratores, implementos e máquinas agrícolas em estabelecimentos agropecuários no município em 2017     |
|TERRITORIOS_CIDADANIA                  |Territórios da cidadania                                                                                            |
