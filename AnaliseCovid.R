# ============================================================
# Projeto: Análise de COVID-19 no Brasil (versão em R)
# Observação: ajuste os caminhos de arquivos/shapefiles conforme o seu repositório.
# ============================================================

# -------------------------------
# 0) Pacotes
# -------------------------------
# Instale (se necessário):
#install.packages(c(
#"tidyverse","readr","DBI","RSQLite","lubridate","sf","ggplot2","scales","zoo"
#))

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(DBI)
  library(RSQLite)
  library(lubridate)
  library(sf)
  library(ggplot2)
  library(scales)
  library(zoo)
  library(stargazer)
})

# -------------------------------
# 1) Leitura e integração de dados
# -------------------------------
# Dados (ajuste os caminhos se necessário)

#setwd("C:/Users/Samsung/Desktop/Análise Covid")
#getwd()

casos <- read_delim(
  "DadosCOVID/casos.csv",
  delim = ";",
  locale = locale(decimal_mark = ","),
  show_col_types = FALSE
)

# Base SQLite com Região e Estado
con <- dbConnect(SQLite(), "DadosCOVID/base.db")
Regiao <- dbReadTable(con, "Regiao")
Estado <- dbReadTable(con, "Estado")
dbDisconnect(con)

# Unindo Região + Estado e padronizando coluna UF -> uf
base <- Regiao %>%
  inner_join(Estado, by = "CodigoRegiao") %>%
  rename(uf = UF)

# Juntando casos com base (por uf)
casosbrasil <- casos %>%
  left_join(base, by = "uf")

# -------------------------------
# 2) Tratamento de dados faltantes
# -------------------------------
# Preenchendo nomelocalidade (para linhas tipo == "state") com a sigla do estado (uf)
casosbrasil <- casosbrasil %>%
  mutate(
    nomelocalidade = if_else(is.na(nomelocalidade), uf, nomelocalidade)
  )

# Diferença de óbitos entre Estado e Cidade
mortes_por_tipo_estado <- casosbrasil %>% filter(tipo == "state") %>% summarise(x = sum(mortesnovas, na.rm = TRUE)) %>% pull(x)
mortes_por_tipo_cidade <- casosbrasil %>% filter(tipo == "city")  %>% summarise(x = sum(mortesnovas, na.rm = TRUE)) %>% pull(x)
mortes_diff <- mortes_por_tipo_estado - mortes_por_tipo_cidade
message("Diferença de Óbitos entre Estado e Cidade: ", format(mortes_diff, big.mark = ".", decimal.mark = ","))

# -------------------------------
# 3) Função principal de construção do dataset analítico
# -------------------------------
casoss <- function(casosbrasil) {

  # Casos/mortes por cidade e data
  brasilcovid <- casosbrasil %>%
    filter(tipo == "city") %>%
    group_by(NomeRegiao, uf, nomelocalidade, data, populacao) %>%
    summarise(
      casos_cidade  = sum(casosnovos,  na.rm = TRUE),
      mortes_cidade = sum(mortesnovas, na.rm = TRUE),
      .groups = "drop"
    )

  # Casos acumulados por estado (somatório de casosnovos do tipo state)
  casos_estado <- casosbrasil %>%
    filter(tipo == "state") %>%
    group_by(uf) %>%
    summarise(casos_estado = sum(casosnovos, na.rm = TRUE), .groups = "drop")

  # Casos por dia por estado
  casos_dia_estados <- casosbrasil %>%
    filter(tipo == "state") %>%
    group_by(data, uf) %>%
    summarise(casos_dia_estados = sum(casosnovos, na.rm = TRUE), .groups = "drop")

  # Óbitos por dia por estado
  morte_dia_estados <- casosbrasil %>%
    filter(tipo == "state") %>%
    group_by(data, uf) %>%
    summarise(morte_dia_estados = sum(mortesnovas, na.rm = TRUE), .groups = "drop")

  # Enriquecendo brasilcovid com agregações estaduais (por uf e/ou data)
  brasilcovid <- brasilcovid %>%
    left_join(casos_estado,      by = "uf") %>%
    left_join(casos_dia_estados, by = c("data", "uf")) %>%
    left_join(morte_dia_estados, by = c("data", "uf"))

  # Casos por região (somando casos_estado únicos por UF dentro da região)
  casos_regiao_tbl <- brasilcovid %>%
    distinct(NomeRegiao, uf, casos_estado) %>%
    group_by(NomeRegiao) %>%
    summarise(casos_regiao = sum(casos_estado, na.rm = TRUE), .groups = "drop")

  brasilcovid <- brasilcovid %>%
    left_join(casos_regiao_tbl, by = "NomeRegiao")

  # Óbitos acumulados por estado e por região (somando mortes_cidade em todos os dias)
  mortes_estado_tbl <- brasilcovid %>%
    group_by(uf) %>%
    summarise(mortes_estado = sum(mortes_cidade, na.rm = TRUE), .groups = "drop")

  mortes_regiao_tbl <- brasilcovid %>%
    group_by(NomeRegiao) %>%
    summarise(mortes_regiao = sum(mortes_cidade, na.rm = TRUE), .groups = "drop")

  brasilcovid <- brasilcovid %>%
    left_join(mortes_estado_tbl, by = "uf") %>%
    left_join(mortes_regiao_tbl, by = "NomeRegiao")

  # População Brasil (soma das populações únicas no nível state)
  populacaoBrasil <- casosbrasil %>%
    filter(tipo == "state") %>%
    distinct(populacao) %>%
    summarise(pop = sum(populacao, na.rm = TRUE)) %>%
    pull(pop)

  # Métricas de letalidade e mortalidade
  brasilcovid <- brasilcovid %>%
    mutate(
      populacaoBrasil = populacaoBrasil,

      letalidade_cidade = mortes_cidade / na_if(casos_cidade, 0),
      letalidade_estado = mortes_estado / na_if(casos_estado, 0),
      letalidade_regiao = mortes_regiao / na_if(casos_regiao, 0),

      mortalidade_cidade = mortes_cidade / na_if(populacao, 0),
      mortalidade_estado = mortes_estado / na_if(populacao, 0),
      mortalidade_regiao = mortes_regiao / na_if(populacao, 0)
    )

  # Porcentagens (replicando a lógica do notebook original: soma de valores únicos)
  sum_unique_city_cases   <- sum(unique(brasilcovid$casos_cidade),  na.rm = TRUE)
  sum_unique_state_cases  <- sum(unique(brasilcovid$casos_estado),  na.rm = TRUE)
  sum_unique_region_cases <- sum(unique(brasilcovid$casos_regiao),  na.rm = TRUE)

  brasilcovid <- brasilcovid %>%
    mutate(
      porcentagem_cas_cidades = (casos_cidade  / sum_unique_city_cases)   * 100,
      porcentagem_cas_estados = (casos_estado  / sum_unique_state_cases)  * 100,
      porcentagem_cas_regiao  = (casos_regiao  / sum_unique_region_cases) * 100
    )

  # Reordenando colunas para ficar alinhado com o notebook original
  brasilcovid <- brasilcovid %>%
    select(
      NomeRegiao, uf, nomelocalidade, data, populacao,
      casos_cidade, porcentagem_cas_cidades, mortes_cidade, letalidade_cidade, mortalidade_cidade,
      casos_estado, casos_dia_estados, porcentagem_cas_estados, mortes_estado, morte_dia_estados, letalidade_estado, mortalidade_estado,
      casos_regiao, porcentagem_cas_regiao, mortes_regiao, letalidade_regiao, mortalidade_regiao,
      populacaoBrasil
    )

  brasilcovid
}

brasilcovid <- casoss(casosbrasil)

# Checagem rápida (equivalente ao cell 3)
x <- brasilcovid %>% group_by(uf) %>% summarise(mortes_cidade = sum(mortes_cidade, na.rm = TRUE), .groups = "drop")
print(x)

# -------------------------------
# 4) Mapas (sf + ggplot2)
# -------------------------------
mapa_casos_brasil <- function(
  brasilcovid,
  shapefile_path = "Shapefile/BR_UF_2022.shp",
  titulo_mapa = "Total de Casos por Estado no Brasil"
) {
  brasilcovid_unique <- brasilcovid %>% distinct(uf, .keep_all = TRUE)

  brasil <- st_read(shapefile_path, quiet = TRUE) %>%
    rename(uf = SIGLA_UF) %>%
    left_join(brasilcovid_unique, by = "uf")

  # Ponto interno para rótulos
  pts <- st_point_on_surface(brasil$geometry)
  coords <- st_coordinates(pts)
  brasil$cx <- coords[, 1]
  brasil$cy <- coords[, 2]

  ggplot(brasil) +
    geom_sf(aes(fill = casos_estado), color = "black", linewidth = 0.2) +
    geom_text(
      aes(x = cx, y = cy, label = uf),
      size = 2.2
    ) +
    scale_fill_distiller(
      palette = "YlOrRd",
      direction = 1,
      labels = label_number(big.mark=".", decimal.mark=",")
      # trans = "log10"   # se quiser manter log, descomente
    ) +
    labs(title = titulo_mapa, fill = "Casos") +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}

mapa_covid_estado <- function(
  brasilcovid,
  uf_alvo = "SP",
  shapefile_path = "ShapefileSP/SP_Municipios_2022.shp",
  titulo_mapa = "Total de Casos por Município em São Paulo"
) {
  casos_SP <- brasilcovid %>%
    filter(uf == uf_alvo) %>%
    group_by(nomelocalidade) %>%
    summarise(casos_cidade = max(casos_cidade, na.rm = TRUE), .groups = "drop")

  municipios <- st_read(shapefile_path, quiet = TRUE) %>%
    rename(nomelocalidade = NM_MUN) %>%
    left_join(casos_SP, by = "nomelocalidade")

  pts <- st_point_on_surface(municipios$geometry)
  coords <- st_coordinates(pts)
  municipios$cx <- coords[, 1]
  municipios$cy <- coords[, 2]

  ggplot(municipios) +
    geom_sf(aes(fill = casos_cidade), color = "black", linewidth = 0.1) +
    geom_text(
      aes(x = cx, y = cy, label = label_number(big.mark=".", decimal.mark=",")(casos_cidade)),
      size = 1.0
    ) +
    scale_fill_distiller(
      palette = "YlOrRd",                 
      direction = 1,
      labels = label_number(big.mark=".", decimal.mark=",") #,
      #trans = "log10"                   # se você quiser manter log, descomente
    ) +
    labs(title = titulo_mapa, fill = "Casos") +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
  
}

# Exemplos de chamada (dependem de você ter os shapefiles no repositório)
print(mapa_casos_brasil(brasilcovid))
print(mapa_covid_estado(brasilcovid))

# -------------------------------
# 5) Gráficos de setores
# -------------------------------
grafico_setores <- function(brasilcovid) {
  
  # Casos acumulados por região (percentual)
  reg <- brasilcovid %>%
    distinct(NomeRegiao, porcentagem_cas_regiao)
  
  p1 <- ggplot(reg, aes(x = "", y = porcentagem_cas_regiao, fill = NomeRegiao)) +
    geom_col(width = 1) +
    coord_polar(theta = "y") +
    geom_text(
      aes(label = paste0(round(porcentagem_cas_regiao, 1), "%")),
      position = position_stack(vjust = 0.5),
      size = 3
    ) +
    labs(title = "Casos Acumulados por Região") +
    theme_void()
  
  print(p1)
  
  # Região Sudeste: casos acumulados por estado (percentual)
  sudeste <- brasilcovid %>%
    filter(NomeRegiao == "Sudeste") %>%
    group_by(uf) %>%
    summarise(porcentagem_cas_estados = max(porcentagem_cas_estados, na.rm = TRUE), .groups = "drop") %>%
    mutate(porcentagem_cas_estados = 100 * porcentagem_cas_estados / sum(porcentagem_cas_estados, na.rm = TRUE))
  
  p2 <- ggplot(sudeste, aes(x = "", y = porcentagem_cas_estados, fill = uf)) +
    geom_col(width = 1) +
    coord_polar(theta = "y") +
    geom_text(
      aes(label = paste0(round(porcentagem_cas_estados, 1), "%")),
      position = position_stack(vjust = 0.5),
      size = 3
    ) +
    labs(title = "Casos Acumulados por Estado na Região Sudeste") +
    theme_void()
  
  print(p2)
}

grafico_setores(brasilcovid)


# -------------------------------
# 6) Top 10 cidades (taxa de casos)
# -------------------------------
dez_cidades_taxa <- function(brasilcovid, order = "taxa") {
  brasilcovid %>%
    select(uf, nomelocalidade, casos_cidade, populacao, letalidade_cidade, mortalidade_cidade) %>%
    mutate(taxa = casos_cidade / na_if(populacao, 0)) %>%
    arrange(desc(taxa)) %>%
    slice_head(n = 10) %>%
    arrange(.data[[order]])
}

dez_cidades_taxa_SP <- function(brasilcovid, order = "taxa") {
  brasilcovid %>%
    filter(uf == "SP") %>%
    select(uf, nomelocalidade, casos_cidade, populacao, letalidade_cidade, mortalidade_cidade) %>%
    mutate(taxa = casos_cidade / na_if(populacao, 0)) %>%
    arrange(desc(taxa)) %>%
    slice_head(n = 10) %>%
    arrange(.data[[order]])
}

print(dez_cidades_taxa(brasilcovid))
print(dez_cidades_taxa_SP(brasilcovid))

# -------------------------------
# 7) Casos por estado (tabela)
# -------------------------------
casos_por_estado <- function(brasilcovid) {
  brasilcovid %>%
    group_by(uf) %>%
    summarise(
      casos_estado = first(casos_estado),
      mortes_estado = first(mortes_estado),
      letalidade_estado = first(letalidade_estado),
      mortalidade_estado = first(mortalidade_estado),
      .groups = "drop"
    )
}

casos_estado <- casos_por_estado(brasilcovid)
print(casos_estado)

# -------------------------------
# 8) Preparação para gráficos temporais (dispersão/linha)
# -------------------------------
preparar_dados_para_grafico <- function(brasilcovid) {

  dispersao <- brasilcovid %>%
    distinct(data, uf, casos_dia_estados, morte_dia_estados) %>%
    mutate(data = dmy(data)) %>%
    arrange(data) %>%
    mutate(contagem_datas = dense_rank(data)) %>%
    group_by(data, uf, contagem_datas) %>%
    summarise(
      casos_dia_estados = sum(casos_dia_estados, na.rm = TRUE),
      morte_dia_estados = sum(morte_dia_estados, na.rm = TRUE),
      .groups = "drop"
    )

  dispersao_sp <- dispersao %>% filter(uf == "SP")

  # Total Brasil por dia (soma dos estados)
  dispersao_brasil <- dispersao %>%
    group_by(data, contagem_datas) %>%
    summarise(
      casos_dia_estados = sum(casos_dia_estados, na.rm = TRUE),
      morte_dia_estados = sum(morte_dia_estados, na.rm = TRUE),
      .groups = "drop"
    )

  list(dispersao = dispersao_brasil, dispersao_sp = dispersao_sp)
}

tmp <- preparar_dados_para_grafico(brasilcovid)
dispersao <- tmp$dispersao
dispersao_sp <- tmp$dispersao_sp

# -------------------------------
# 9) Gráficos de dispersão conectada
# -------------------------------
plotar_grafico_dispersao_conectado <- function(dispersao, dispersao_sp) {
  
  dispersao$serie    <- "Brasil"
  dispersao_sp$serie <- "SP"
  
  dados <- dplyr::bind_rows(dispersao, dispersao_sp)
  
  ggplot(dados, aes(x = contagem_datas, y = casos_dia_estados, color = serie)) +
    geom_point(alpha = 0.7) +
    geom_line() +
    scale_color_manual(values = c("Brasil" = "steelblue", "SP" = "orange")) +
    labs(
      x = "contagem_datas",
      y = "Casos por dia",
      title = "Casos por dia: Brasil vs SP",
      color = ""
    ) +
    theme_minimal()
}

print(plotar_grafico_dispersao_conectado(dispersao, dispersao_sp))

# -------------------------------
# 10) Casos e óbitos cumulativos em SP
# -------------------------------

dispersao_casos_obitos_SP <- function(dispersao_sp) {
  df <- dispersao_sp %>%
    arrange(contagem_datas) %>%
    mutate(
      casos_dia_sp_cumulativo = cumsum(casos_dia_estados),
      morte_dia_sp_cumulativo = cumsum(morte_dia_estados)
    )
  
  ggplot(df, aes(x = contagem_datas)) +
    geom_point(aes(y = casos_dia_sp_cumulativo, color = "Casos"), alpha = 0.7) +
    geom_point(aes(y = morte_dia_sp_cumulativo, color = "Óbitos"), alpha = 0.7) +
    geom_line(aes(y = casos_dia_sp_cumulativo, color = "Casos")) +
    geom_line(aes(y = morte_dia_sp_cumulativo, color = "Óbitos")) +
    scale_color_manual(values = c("Casos" = "steelblue", "Óbitos" = "orange")) +
    labs(
      x = "contagem_datas",
      y = "Casos e Mortes por dia acumulativos em SP",
      title = "Casos e Óbitos cumulativos no Estado de São Paulo",
      color = ""
    ) +
    theme_minimal()
}

print(dispersao_casos_obitos_SP(dispersao_sp))


# -------------------------------
# 11) Média móvel de óbitos e análise de tendência
# -------------------------------
plotar_dispersao_media_movel_e_mortes <- function(dispersao, dispersao_sp, janela = 6) {
  
  df_brasil <- dispersao %>%
    arrange(contagem_datas) %>%
    mutate(media_movel_brasil = zoo::rollmean(morte_dia_estados, k = janela, align = "right", fill = NA))
  
  df_sp <- dispersao_sp %>%
    arrange(contagem_datas) %>%
    mutate(media_movel_sp = zoo::rollmean(morte_dia_estados, k = janela, align = "right", fill = NA))
  
  # Variação percentual: último vs 15º a contar do final
  n_b <- nrow(df_brasil); n_sp <- nrow(df_sp)
  idx_prev_b <- n_b - 14; idx_prev_sp <- n_sp - 14
  
  variacao_brasil <- ((df_brasil$media_movel_brasil[n_b] - df_brasil$media_movel_brasil[idx_prev_b]) /
                        df_brasil$media_movel_brasil[idx_prev_b]) * 100
  variacao_sp <- ((df_sp$media_movel_sp[n_sp] - df_sp$media_movel_sp[idx_prev_sp]) /
                    df_sp$media_movel_sp[idx_prev_sp]) * 100
  
  status_brasil <- ifelse(abs(variacao_brasil) <= 15, "Estável",
                          ifelse(variacao_brasil > 15, "Em crescimento", "Em queda"))
  status_sp <- ifelse(abs(variacao_sp) <= 15, "Estável",
                      ifelse(variacao_sp > 15, "Em crescimento", "Em queda"))
  
  message(sprintf("Análise de Tendência Brasil: %s (%.2f%%)", status_brasil, variacao_brasil))
  message(sprintf("Análise de Tendência SP: %s (%.2f%%)", status_sp, variacao_sp))

  p_long <- dplyr::bind_rows(
    df_brasil %>% dplyr::transmute(contagem_datas, valor = media_movel_brasil, serie = "Média Móvel Brasil"),
    df_sp     %>% dplyr::transmute(contagem_datas, valor = media_movel_sp,     serie = "Média Móvel SP"),
    df_brasil %>% dplyr::transmute(contagem_datas, valor = morte_dia_estados,  serie = "Mortes Brasil"),
    df_sp     %>% dplyr::transmute(contagem_datas, valor = morte_dia_estados,  serie = "Mortes SP")
  )
  
  ggplot2::ggplot() +
    # Linhas (mortes)
    ggplot2::geom_line(
      data = dplyr::filter(p_long, grepl("^Mortes", serie)),
      ggplot2::aes(x = contagem_datas, y = valor, color = serie),
      linewidth = 0.6, alpha = 0.6
    ) +
    # Pontos (média móvel)
    ggplot2::geom_point(
      data = dplyr::filter(p_long, grepl("^Média", serie)),
      ggplot2::aes(x = contagem_datas, y = valor, color = serie),
      size = 2, alpha = 0.9
    ) +
    ggplot2::scale_color_manual(
      breaks = c("Média Móvel Brasil", "Média Móvel SP", "Mortes Brasil", "Mortes SP"),
      values = c(
        "Média Móvel Brasil" = "blue",
        "Média Móvel SP"     = "red",
        "Mortes Brasil"      = "lightblue",
        "Mortes SP"          = "orange"
      )
    ) +
    ggplot2::labs(
      x = "Contador de Dias",
      y = "Óbitos",
      title = "Média Móvel de óbitos (Brasil e SP)",
      color = NULL
    ) +
    ggplot2::theme_bw()
}

print(plotar_dispersao_media_movel_e_mortes(dispersao, dispersao_sp))

# -------------------------------
# 12) Gráfico de setores: casos e mortes por estado
# -------------------------------

plot_pie_casos_mortes_estado <- function(casos_estado, min_label = 0.02) {
  
  df <- casos_estado %>%
    mutate(uf = as.character(uf)) %>%
    mutate(
      p_casos  = casos_estado  / sum(casos_estado,  na.rm = TRUE),
      p_mortes = mortes_estado / sum(mortes_estado, na.rm = TRUE)
    )
  
  # -------- TABELA --------
  tab <- df %>%
    transmute(
      UF = uf,
      Casos = casos_estado,
      `Casos (%)` = 100 * p_casos,
      Mortes = mortes_estado,
      `Mortes (%)` = 100 * p_mortes
    ) %>%
    arrange(desc(`Casos (%)`))
  
  tab_print <- tab
  tab_print$Casos        <- format(tab_print$Casos,  big.mark = ".", scientific = FALSE, trim = TRUE)
  tab_print$Mortes       <- format(tab_print$Mortes, big.mark = ".", scientific = FALSE, trim = TRUE)
  tab_print$`Casos (%)`  <- sprintf("%.2f%%", tab_print$`Casos (%)`)
  tab_print$`Mortes (%)` <- sprintf("%.2f%%", tab_print$`Mortes (%)`)
  
  print(tab_print, row.names = FALSE)
  
  # -------- CORES --------
  ufs <- sort(unique(df$uf))
  cores <- setNames(grDevices::hcl.colors(length(ufs), palette = "Dark 3"), ufs)
  
  # -------- PIZZA CASOS  --------
  df_casos <- df %>%
    arrange(desc(p_casos)) %>%
    mutate(lbl = ifelse(p_casos >= min_label, sprintf("%.1f%%", 100 * p_casos), ""))
  
  p_casos <- ggplot(df_casos, aes(x = "", y = p_casos, fill = uf)) +
    geom_col(width = 1, color = "white", linewidth = 0.2) +
    coord_polar(theta = "y") +
    geom_text(aes(label = lbl), position = position_stack(vjust = 0.5), size = 2.2) +
    scale_fill_manual(values = cores) +
    labs(title = "Casos por Estado", fill = "UF") +
    theme_void()
  
  # -------- PIZZA MORTES --------
  df_mortes <- df %>%
    arrange(desc(p_mortes)) %>%
    mutate(lbl = ifelse(p_mortes >= min_label, sprintf("%.1f%%", 100 * p_mortes), ""))
  
  p_mortes <- ggplot(df_mortes, aes(x = "", y = p_mortes, fill = uf)) +
    geom_col(width = 1, color = "white", linewidth = 0.2) +
    coord_polar(theta = "y") +
    geom_text(aes(label = lbl), position = position_stack(vjust = 0.5), size = 2.2) +
    scale_fill_manual(values = cores) +
    labs(title = "Mortes por Estado", fill = "UF") +
    theme_void()
  
  print(p_casos)
  print(p_mortes)
}

plot_pie_casos_mortes_estado(casos_estado)
