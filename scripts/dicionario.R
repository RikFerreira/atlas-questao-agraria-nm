library(tidyverse)
library(sf)
library(knitr)

attr <- st_read("./output/atlas.gpkg") %>%
  st_drop_geometry() %>%
  as_tibble()

attr %>%
  names() %>%
  as_tibble_col(column_name = "VARIAVEL") %>%
  mutate(
    DESCRICAO = c(
      "Código IBGE do município",
      "Código IBGE do estado",
      "Nome do estado",
      "Código da região intermediária",
      "Nome da região intermediária",
      "Código da região imediata",
      "Nome da região imediata",
      "Código da mesorregião",
      "Nome da mesorregião",
      "Código da microrregião",
      "Nome da microrregião",
      "Nome do município",
      "População rural do município em 2010",
      "População total do município em 2010",
      "População urbana do município em 2010",
      "Produto Interno Bruto do município em 2017",
      "Valor Adicionado Bruto total do município em 2017",
      "Valor Adicionado Bruto da agropecuária do município em 2017",
      "Valor Adicionado Bruto da indústria do município em 2017",
      "Valor Adicionado Bruto do setor de serviços do município em 2017",
      "Índice de Desenvolvimento Humano do município",
      "Índice de Desenvolvimento Humano - Educação do município",
      "Índice de Desenvolvimento Humano - Longevidade do município",
      "Índice de Desenvolvimento Humano - Renda do município",
      "Coeficiente de Gini da renda",
      "Índice de Theil",
      "Estabelecimentos agropecuários no município em 2017",
      "Estabelecimentos agropecuários classificados como da agricultura familiar em 2017",
      "Estabelecimentos agropecuários classificados como da agricultura familiar (PRONAF B) em 2017",
      "Estabelecimentos agropecuários classificados como da agricultura familiar (PRONAF V) em 2017",
      "Estabelecimentos agropecuários com menos de 1 hectare em 2017",
      "Estabelecimentos agropecuários com mais de 500 hectares em 2017",
      "Quantidade de estabelecimentos que obtiveram algum financiamento em 2017",
      "Quantidade de estabelecimentos classificados como da agricultura familiar que obtiveram algum financiamento em 2017",
      "Quantidade de cabeças de bovinos no município em 2017",
      "Quantidade de bovinos vendidos no município em 2017",
      "Quantidade de leite de vaca vendida no município em 2017 (litros)",
      "Quantidade de cabeças de suínos no município em 2017",
      "Quantidade de suínos vendidos no município em 2017",
      "Quantidade de cabeças de aves no município em 2017",
      "Quantidade de aves vendidas no município em 2017",
      "Quantidade de ovos de galinha vendidos no município em 2017",
      "Quantidade de cabeças de caprinos no município em 2017",
      "Quantidade de caprinos vendidos no município em 2017",
      "Quantidade de leite de cabra vendida no município em 2017",
      "Quantidade de cabeças de ovinos no município em 2017",
      "Quantidade de ovinos vendidos no município em 2017",
      "Quantidade de leite de ovelha vendida no município em 2017",
      "Quantidade de lã vendida no município em 2017",
      "Quantidade de cabeças de bubalinos no município em 2017",
      "Quantidade de búfalos vendidos no município em 2017",
      "Quantidade de leite vendida no município em 2017",
      "Valor total da venda da lavoura temporária no município em 2017",
      "Área total colhida da lavoura temporária no município em 2017",
      "Valor total da venda da lavoura permanente no município em 2017",
      "Área total colhida da lavoura permanente no município em 2017",
      "Valor total da venda da lavoura permanente por estabelecimentos com menos de 50 pés no município em 2017",
      "Quantidade de tratores, implementos e máquinas agrícolas em estabelecimentos agropecuários no município em 2017",
      "Territórios da cidadania"
    )
  ) %>%
  kable() %>%
  writeLines("./docs/dicionario.md")
