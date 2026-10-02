library(arrow) # to save results as parquet file if desired
library(data.table)
library(dtplyr)
library(tidyverse)

years <- 1995:2024 # input years of interest

# get list of countries
countries <- read_csv("data/BACI_HS92_V202601/country_codes_V202601.csv")

countries_i <- countries %>%
  transmute(i = country_code, i_name = country_name, i_iso3 = country_iso3)
countries_j <- countries %>%
  transmute(j = country_code, j_name = country_name, j_iso3 = country_iso3)

# define function to fill observed trade flows into a matrix
trade_matrix <- function(rows, cols, row_ids, col_ids, values) {
  out <- matrix(
    0, length(rows), length(cols),
    dimnames = list(as.character(rows), as.character(cols))
  )
  out[cbind(match(row_ids, rows), match(col_ids, cols))] <- values
  out
}

# settings for saving the index in loop below
results <- vector("list", length(years))

for (y in years) {

  # import data
  hs4_data <-
    fread(paste0(
      "data/BACI_HS92_V202601/BACI_HS92_Y",
      y,
      "_V202601.csv"
    )) %>%
    lazy_dt() %>%
    # get HS4 from HS6 (k)
    mutate(hs4 = as.integer(k) %/% 100) %>%
    group_by(i, j, hs4) %>%
    # note: unit of value is 1000s USD
    summarise(x_ijk = sum(v), .groups = "drop") %>%
    as_tibble() # conclude dtplyr

  # get lists of countries and commodities
  exporters <- sort(unique(hs4_data$i))
  importers <- sort(unique(hs4_data$j))
  all_countries <- sort(unique(c(hs4_data$i, hs4_data$j)))
  hs4_codes <- sort(unique(hs4_data$hs4))

  # trade aggregates

  # bilateral trade flows
  bilateral <- hs4_data %>%
    group_by(i, j) %>%
    summarise(x_ij = sum(x_ijk), .groups = "drop")

  x_ij_mat <- trade_matrix(
    exporters, importers, bilateral$i, bilateral$j, bilateral$x_ij
  )

  # exports of k by i to all destinations
  exports_ik <- hs4_data %>%
    group_by(i, hs4) %>%
    summarise(x_ik = sum(x_ijk), .groups = "drop")

  # imports of k by all countries
  imports_ck <- hs4_data %>%
    group_by(j, hs4) %>%
    summarise(m_ck = sum(x_ijk), .groups = "drop") %>%
    rename(country = j)

  # export matrix: exporter i x commodity k
  x_mat <- trade_matrix(
    exporters, hs4_codes, exports_ik$i, exports_ik$hs4, exports_ik$x_ik
  )

  # import matrix: all countries x commodity k
  m_all_mat <- trade_matrix(
    all_countries, hs4_codes, imports_ck$country, imports_ck$hs4, imports_ck$m_ck
  )

  # m_ik
  m_exporter_mat <- m_all_mat[as.character(exporters), , drop = FALSE]

  # m_jk
  m_importer_mat <- m_all_mat[as.character(importers), , drop = FALSE]

  # country and world totals
  x_i <- rowSums(x_mat)
  m_i <- rowSums(m_exporter_mat)
  m_j <- rowSums(m_importer_mat)
  m_wk <- colSums(m_all_mat)
  m_w <- sum(m_wk)

  # export shares: x_ik / x_i
  export_share <- sweep(x_mat, 1, x_i, "/")

  # import shares: m_jk / m_j
  import_share <- sweep(m_importer_mat, 1, m_j, "/")

  # world adjustment: (m_w - m_i) / (m_wk - m_ik)
  denominator <- matrix(
    m_wk,
    nrow = nrow(m_exporter_mat),
    ncol = ncol(m_exporter_mat),
    byrow = TRUE,
    dimnames = dimnames(m_exporter_mat)
  ) - m_exporter_mat

  world_adjustment <- sweep(1 / denominator, 1, m_w - m_i, "*")

  exporter_term <- export_share * world_adjustment

  # where denominator = 0, export_share must also equal 0,
  # so the commodity contributes zero to complementarity
  exporter_term[!is.finite(exporter_term)] <- 0

  c_matrix <- exporter_term %*% t(import_share)

  # I_ij = (x_ij / x_i) / (m_j / (m_w - m_i))
  i_matrix <- sweep(x_ij_mat, 1, (m_w - m_i) / x_i, "*")

  i_matrix <- sweep(i_matrix, 2, m_j, "/")

  # B_ij = C_ij / I_ij
  b_matrix <- i_matrix / c_matrix

  b_matrix[is.na(c_matrix) | c_matrix <= 0] <- NA_real_

  result_y <- as.data.frame(as.table(c_matrix), responseName = "c_ij") %>%
    transmute(
      year = y,
      i = as.numeric(as.character(Var1)),
      j = as.numeric(as.character(Var2)),
      c_ij = as.numeric(c_ij),
      b_ij = as.numeric(b_matrix),
      i_ij = as.numeric(i_matrix)
    ) %>%
    left_join(countries_i, by = "i") %>%
    left_join(countries_j, by = "j") %>%
    select(
      year,
      i, i_name, i_iso3,
      j, j_name, j_iso3,
      c_ij, b_ij, i_ij
    )

  results[[which(years == y)]] <- result_y

}

results <- bind_rows(results)

# write_csv(results, file = "output/intensity.csv") # write indexes to csv
write_parquet(results, "output/intensity.parquet") # write indexes to parquet