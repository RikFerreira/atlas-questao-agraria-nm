library(tidyverse)
library(sf)
library(sidrar)
library(geobr)
library(readxl)

# Lista de municípios do Norte de Minas
nm_municipios <- read_csv("./data/mun_nm.csv")

nm_vetores <- read_municipality(year = 2019) %>%
  inner_join(
    nm_municipios,
    by = c("code_muni" = "COD_MUN")
  )

# Tabela da população
info_sidra(1378)

populacao <- get_sidra(
  1378,
  variable = 93,
  geo = "City",
  geo.filter = list("MesoRegion" = "3102"),
  classific = list("c1")
) %>%
  select(
    COD_MUN = `Município (Código)`,
    `Situação do domicílio`, Valor
  ) %>%
  spread(`Situação do domicílio`, Valor) %>%
  rename_at(2:4, ~{paste0("POPULACAO_", str_to_upper(.x))}) %>%
  as_tibble()

# Tabela do PIB
info_sidra(5938)

pib <- get_sidra(
  5938,
  variable = c(37, 498, 513, 517, 6575),
  geo = "City",
  geo.filter = list("MesoRegion" = "3102")
) %>%
  spread(`Variável (Código)`, Valor) %>%
  select(
    COD_MUN = `Município (Código)`,
    PIB = `37`,
    VAB = `498`,
    VAGRO = `513`,
    VINDU = `517`,
    VSERV = `6575`
  ) %>%
  group_by(COD_MUN) %>%
  summarise_all(~{sum(.x, na.rm = TRUE)}) %>%
  as_tibble()

# Atlas do Desenvolvimento Humano no Brasil
atlas <- nm_municipios %>%
  left_join(
    read_xlsx("./data/atlas2013_dadosbrutos_pt.xlsx", sheet = 2) %>%
      filter(ANO == 2010),
    by = c("COD_MUN" = "Codmun7")
  ) %>%
  select(
    COD_MUN,
    IDHM, IDHM_E, IDHM_L, IDHM_R,
    GINI, THEIL
  ) %>%
  mutate(
    COD_MUN = as.character(COD_MUN)
  ) %>%
  as_tibble()

# Tabela com número de estabelecimentos
info_sidra(6778)

# Total de estabelecimentos e estabelecimentos da agricultura familiar
estabelecimentos <- get_sidra(
  6778,
  geo = "City",
  geo.filter = list("MesoRegion" = 3102),
  classific = list("c829")
) %>%
  spread(Tipologia, Valor) %>%
  select(
    COD_MUN = `Município (Código)`,
    ESTABELECIMENTOS = Total,
    FAMILIAR = `Agricultura familiar - sim`,
    PRONAF_B = `Agricultura familiar - Pronaf B`,
    PRONAF_V = `Agricultura familiar - Pronaf V`
  ) %>%
  group_by(COD_MUN) %>%
  summarise_all(~{sum(.x, na.rm = TRUE)}) %>%
  as_tibble()

# Estabelecimentos por tamanho
estab_area <- get_sidra(
  6778,
  geo = "City",
  geo.filter = list("MesoRegion" = 3102),
  classific = list("c220")
) %>%
  spread(`Grupos de área total (Código)`, Valor) %>%
  select(
    COD_MUN = `Município (Código)`,
    "111543", "111544", "111545", "111546",
    "111557", "111558", "41139", "40645"
  ) %>%
  group_by(COD_MUN) %>%
  summarise(
    ESTAB_PEQUENOS = sum(`111543`, `111544`, `111545`, `111546`, na.rm = TRUE),
    ESTAB_GRANDES = sum(`111557`, `111558`, `41139`, `40645`, na.rm = TRUE)
  ) %>%
  as_tibble()

# Estabelecimentos com financiamento
info_sidra(6896)

estab_financiamento <- get_sidra(
  6896,
  variable = 1990,
  geo = "City",
  geo.filter = list("MesoRegion" = 3102),
  classific = list("c829")
) %>%
  spread(`Tipologia (Código)`, Valor) %>%
  select(
    COD_MUN = `Município (Código)`,
    ESTAB_TOTAL_FINANCIAMENTO = `46303`,
    ESTAB_FAMILIARL_FINANCIAMENTO = `46304`
  ) %>%
  group_by(COD_MUN) %>%
  summarise_all(~{sum(.x, na.rm = TRUE)}) %>%
  as_tibble()

# Bovinos
info_sidra(6910)

bovinos <- get_sidra(
  6910,
  variable = c(2057, 9523),
  geo = "City",
  geo.filter = list("MesoRegion" = 3102),
  classific = list("c829")
) %>%
  filter(`Tipologia (Código)` == 46302) %>%
  spread(`Variável (Código)`, Valor) %>%
  select(
    COD_MUN = `Município (Código)`,
    N_BOVINOS = `2057`,
    VENDA_BOVINOS = `9523`
  ) %>%
  group_by(COD_MUN) %>%
  summarise_all(~{sum(.x, na.rm = TRUE)}) %>%
  as_tibble()

# Leite de vaca
info_sidra(6912)

leite_vaca <- get_sidra(
  6912,
  variable = 2080,
  geo = "City",
  geo.filter = list("MesoRegion" = 3102),
  classific = list("c829")
) %>%
  filter(`Tipologia (Código)` == 46302) %>%
  spread(`Variável (Código)`, Valor) %>%
  select(
    COD_MUN = `Município (Código)`,
    VENDA_LEITE_BOVINOS = `2080`
  ) %>%
  as_tibble()

# Suínos
info_sidra(6926)

suinos <- get_sidra(
  6926,
  variable = c(2102, 9532),
  geo = "City",
  geo.filter = list("MesoRegion" = 3102),
  classific = list("c829")
) %>%
  filter(`Tipologia (Código)` == 46302) %>%
  spread(`Variável (Código)`, Valor) %>%
  select(
    COD_MUN = `Município (Código)`,
    N_SUINOS = `2102`,
    VENDA_SUINOS = `9532`
  ) %>%
  group_by(COD_MUN) %>%
  summarise_all(~{sum(.x, na.rm = TRUE)}) %>%
  as_tibble()

# Aves
info_sidra(6941)

aves <- get_sidra(
  6941,
  variable = c(2589, 9509, 2286),
  geo = "City",
  geo.filter = list("MesoRegion" = 3102),
  classific = list("c829")
) %>%
  filter(`Tipologia (Código)` == 46302) %>%
  spread(`Variável (Código)`, Valor) %>%
  select(
    COD_MUN = `Município (Código)`,
    N_AVES = `2589`,
    VENDA_AVES = `9509`,
    VENDA_OVOS = `2286`
  ) %>%
  group_by(COD_MUN) %>%
  summarise_all(~{sum(.x, na.rm = TRUE)}) %>%
  as_tibble()

# Caprinos
info_sidra(6928)

caprinos <- get_sidra(
  6928,
  variable = c(2210, 9533, 2219),
  geo = "City",
  geo.filter = list("MesoRegion" = 3102),
  classific = list("c829")
) %>%
  filter(`Tipologia (Código)` == 46302) %>%
  spread(`Variável (Código)`, Valor) %>%
  select(
    COD_MUN = `Município (Código)`,
    N_CAPRINOS = `2210`,
    VENDA_CAPRINOS = `9533`,
    VENDA_LEITE_CAPRINOS = `2219`
  ) %>%
  group_by(COD_MUN) %>%
  summarise_all(~{sum(.x, na.rm = TRUE)}) %>%
  as_tibble()

# Ovinos
info_sidra(6930)

ovinos <- get_sidra(
  6930,
  variable = c(2230, 9535, 9764, 2236),
  geo = "City",
  geo.filter = list("MesoRegion" = 3102),
  classific = list("c829")
) %>%
  filter(`Tipologia (Código)` == 46302) %>%
  spread(`Variável (Código)`, Valor) %>%
  select(
    COD_MUN = `Município (Código)`,
    N_OVINOS = `2230`,
    VENDA_OVINOS = `9535`,
    VENDA_LEITE_OVINOS = `9764`,
    VENDA_LA_OVINOS = `2236`
  ) %>%
  group_by(COD_MUN) %>%
  summarise_all(~{sum(.x, na.rm = TRUE)}) %>%
  as_tibble()

# Bubalinos
info_sidra(6918)

bubalinos <- get_sidra(
  6918,
  variable = c(2199, 10028, 2208),
  geo = "City",
  geo.filter = list("MesoRegion" = 3102),
  classific = list("c829")
) %>%
  filter(`Tipologia (Código)` == 46302) %>%
  spread(`Variável (Código)`, Valor) %>%
  select(
    COD_MUN = `Município (Código)`,
    N_BUBALINOS = `2199`,
    VENDA_BUBALINOS = `10028`,
    VENDA_LEITE_BUBALINOS = `2208`
  ) %>%
  group_by(COD_MUN) %>%
  summarise_all(~{sum(.x, na.rm = TRUE)}) %>%
  as_tibble()

# Lavoura temporária
info_sidra(6957)

lav_temporaria <- get_sidra(
  6957,
  variable = c(10088, 10089),
  geo = "City",
  geo.filter = list("MesoRegion" = 3102),
  classific = list("c829")
) %>%
  filter(`Tipologia (Código)` == 46302) %>%
  spread(`Variável (Código)`, Valor) %>%
  select(
    COD_MUN = `Município (Código)`,
    VALOR_VENDA_TEMPORARIA = `10088`,
    AREA_COLHIDA_TEMPORARIA = `10089`
  ) %>%
  group_by(COD_MUN) %>%
  summarise_all(~{sum(.x, na.rm = TRUE)}) %>%
  as_tibble()

# Lavoura permanente
info_sidra(6955)

lav_permanente <- get_sidra(
  6955,
  variable = c(10077, 10078, 10083),
  geo = "City",
  geo.filter = list("MesoRegion" = 3102),
  classific = list("c829")
) %>%
  filter(`Tipologia (Código)` == 46302) %>%
  spread(`Variável (Código)`, Valor) %>%
  select(
    COD_MUN = `Município (Código)`,
    VALOR_VENDA_PERMANENTE = `10077`,
    AREA_COLHIDA_PERMANENTE = `10078`,
    VALOR_VENDA_PERMANENTE_MENOS_DE_50_PES = `10083`
  ) %>%
  group_by(COD_MUN) %>%
  summarise_all(~{sum(.x, na.rm = TRUE)}) %>%
  as_tibble()

# PAA
paa_estados <- left_join(
  nm_municipios %>%
    mutate(COD_MUN_6 = COD_MUN %/% 10),
  read_tsv("./data/paa_agri_adesao_est_10-08-2020.xls", skip = 1) %>%
    mutate(COD_MUN = `Codigo IBGE`),
  by = c("COD_MUN_6" = "COD_MUN")
)

paa_municipios <- left_join(
  nm_municipios %>%
    mutate(COD_MUN_6 = COD_MUN %/% 10),
  read_tsv("./data/paa_agri_adesao_mun_10-08-2020.xls", skip = 1) %>%
    mutate(COD_MUN = `Codigo IBGE`),
  by = c("COD_MUN_6" = "COD_MUN")
)

paa_conab_mda <- left_join(
  nm_municipios %>%
    mutate(COD_MUN_6 = COD_MUN %/% 10),
  read_tsv("./data/paa_agri_conab_mda_10-08-2020.xls", skip = 1) %>%
    mutate(COD_MUN = `Codigo IBGE do Agricultor`),
  by = c("COD_MUN_6" = "COD_MUN")
)

paa_conab_mds <- left_join(
  nm_municipios %>%
    mutate(COD_MUN_6 = COD_MUN %/% 10),
  read_tsv("./data/paa_agri_conab_mds_10-08-2020.xls", skip = 1) %>%
    mutate(COD_MUN = `Codigo IBGE do Agricultor`),
  by = c("COD_MUN_6" = "COD_MUN")
)

paa_leite <- left_join(
  nm_municipios %>%
    mutate(COD_MUN_6 = COD_MUN %/% 10),
  read_tsv("./data/paa_agri_individual_leite_10-08-2020.xls", skip = 1) %>%
    rename(COD_MUN = 2),
  by = c("COD_MUN_6" = "COD_MUN")
)

# Territórios da cidadania
territorios_cidadania <- left_join(
  nm_municipios,
  read_csv2("./data/territoriosruraisecidadania.csv", locale = locale(encoding = "ISO-8859-1")) %>%
    rename(COD_MUN = ibge) %>%
    group_by(COD_MUN) %>%
    summarise(TERRITORIOS_CIDADANIA = n()),
  by = "COD_MUN"
) %>%
  mutate(
    COD_MUN = as.character(COD_MUN),
    TERRITORIOS_CIDADANIA = replace_na(TERRITORIOS_CIDADANIA, 0)
  )

# Construção da base de dados
atlas_final <- nm_vetores %>%
  select(COD_MUN = code_muni) %>% mutate(COD_MUN = as.character(COD_MUN)) %>%
  left_join(populacao, "COD_MUN") %>%
  left_join(pib, "COD_MUN") %>%
  left_join(atlas, "COD_MUN") %>%
  left_join(estabelecimentos, "COD_MUN") %>%
  left_join(estab_area, "COD_MUN") %>%
  left_join(estab_financiamento, "COD_MUN") %>%
  left_join(bovinos, "COD_MUN") %>%
  left_join(leite_vaca, "COD_MUN") %>%
  left_join(suinos, "COD_MUN") %>%
  left_join(aves, "COD_MUN") %>%
  left_join(caprinos, "COD_MUN") %>%
  left_join(ovinos, "COD_MUN") %>%
  left_join(bubalinos, "COD_MUN") %>%
  left_join(lav_temporaria, "COD_MUN") %>%
  left_join(lav_permanente, "COD_MUN") %>%
  left_join(territorios_cidadania, "COD_MUN")

atlas_final %>%
  st_write("./output/atlas.gpkg")

atlas_final %>%
  st_drop_geometry() %>%
  write_csv("./output/atlas.csv")
