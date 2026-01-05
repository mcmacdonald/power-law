


# call pipe to workspace
`%>%` <- magrittr::`%>%`

# rank order the market cap by sector
equities <- equities %>%
  dplyr::group_by(Sector) %>%
  dplyr::mutate(Rank = dplyr::dense_rank(dplyr::desc(Market_Value_Billions))) %>% # rank order
  dplyr::mutate(Case = dplyr::row_number()) %>% # case number for sorting
  dplyr::ungroup() %>%
  dplyr::arrange(Sector, Case, Rank) # sort

# calculate quantiles for the cumulative distribution function
sp1500 <- equities %>%
  dplyr::mutate(
    q50 = stats::quantile(Market_Value_Billions, probs = 0.50), # calculate size of 50% of firms 
    q80 = stats::quantile(Market_Value_Billions, probs = 0.80) # calculate size of 80% of firms
    )

# the cumulative distribution of firm size for S&P 1,500 composite equity index
ggplot2::ggplot(sp1500, ggplot2::aes(Market_Value_Billions)) +
  ggplot2::stat_ecdf(geom = "step", linewidth = 1, col = "black", alpha = 1.00) +
  ggplot2::theme_bw() + 
  ggplot2::scale_x_log10() +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::xlab("(Logged) Market Capitalization S&P 1,500 Composite, Billions (USD)") +
  ggplot2::ylab("Empiricial Cumulative Distribution Function (ECDF)") +
  # mark 50% of firms
  ggplot2::geom_vline(data = sp1500, ggplot2::aes(xintercept = q80), linewidth = 1, linetype = "dashed", color = "red") +
  # mark 80% of firms
  ggplot2::geom_vline(data = sp1500, ggplot2::aes(xintercept = q50), linewidth = 1, linetype = "dashed", color = "red") +
  # label the lines
  ggplot2::geom_label(data = sp1500, mapping = ggplot2::aes(x = q50, y = 0.25, label = paste("\u2265 50% of firms:\n", round(q50, digits = 2), "B")), size = 8/.pt) + 
  ggplot2::geom_label(data = sp1500, mapping = ggplot2::aes(x = q80, y = 0.50, label = paste("\u2265 80% of firms:\n", round(q80, digits = 2), "B")), size = 8/.pt) 





# calculate quantiles for the cumulative distribution function
sp1500 <- equities %>%
  dplyr::group_by(Sector) %>%
  dplyr::mutate(
    q50 = stats::quantile(Market_Value_Billions, probs = 0.50), # calculate size of 50% of firms 
    q80 = stats::quantile(Market_Value_Billions, probs = 0.80) # calculate size of 80% of firms
    ) %>%
  dplyr::ungroup()

# construct the plot
ggplot2::ggplot(sp1500, ggplot2::aes(Market_Value_Billions)) +
  ggplot2::stat_ecdf(geom = "step", linewidth = 1, col = "black", alpha = 1.00) + # geom = "area"
  ggplot2::facet_wrap(~ Sector, nrow = 3, ncol = 4) +
  ggplot2::theme_bw() +
  ggplot2::theme(text = ggplot2::element_text(size = 12)) +
  ggplot2::scale_x_log10() +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::xlab("(Logged) Market Capitalization S&P 1,500 Composite, Billions (USD)") +
  ggplot2::ylab("EmpiricialCumulative Distribution Function (ECDF)") +
  # mark 50% of firms
  ggplot2::geom_vline(data = sp1500, ggplot2::aes(xintercept = q80), linewidth = 1, linetype = "dashed", color = "red") +
  # mark 80% of firms
  ggplot2::geom_vline(data = sp1500, ggplot2::aes(xintercept = q50), linewidth = 1, linetype = "dashed", color = "red") +
  # label the lines
  ggplot2::geom_label(data = sp1500, mapping = ggplot2::aes(x = q50, y = 0.25, label = paste("\u2265 50% of firms:\n ", round(q50, digits = 2), "B")), size = 8/.pt) + 
  ggplot2::geom_label(data = sp1500, mapping = ggplot2::aes(x = q80, y = 0.50, label = paste("\u2265 80% of firms:\n ", round(q80, digits = 2), "B")), size = 8/.pt) 





# the cumulative distribution of firm size for S&P 500 large-cap equities
sp500 <- equities %>%
  dplyr::filter(Index == "S&P 500") %>%
  dplyr::group_by(Sector) %>%
  dplyr::mutate(
    q50 = stats::quantile(Market_Value_Billions, probs = 0.50), # calculate size of 50% of firms 
    q80 = stats::quantile(Market_Value_Billions, probs = 0.80) # calculate size of 80% of firms
  ) %>%
  dplyr::ungroup()

# construct the plot
ggplot2::ggplot(sp500, ggplot2::aes(Market_Value_Billions)) +
  ggplot2::stat_ecdf(geom = "step", linewidth = 1, col = "black", alpha = 1.00) + # geom = "area"
  ggplot2::facet_wrap(~ Sector, nrow = 3, ncol = 4) +
  ggplot2::theme_bw() +
  ggplot2::theme(text = ggplot2::element_text(size = 20)) +
  ggplot2::scale_x_log10()+
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::xlab("(Logged) Market Capitalization S&P 500, Billions (USD)") +
  ggplot2::ylab("EmpiricialCumulative Distribution Function (ECDF)") +
  # mark 50% of firms
  ggplot2::geom_vline(data = sp500, ggplot2::aes(xintercept = q80), linewidth = 1, linetype = "dashed", color = "red") +
  # mark 80% of firms
  ggplot2::geom_vline(data = sp500, ggplot2::aes(xintercept = q50), linewidth = 1, linetype = "dashed", color = "red") +
  # label the lines
  ggplot2::geom_label(data = sp500, mapping = ggplot2::aes(x = q50, y = 0.25, label = paste("\u2265 50% of firms:\n", round(q50, digits = 2), "B")), size = 8/.pt) + 
  ggplot2::geom_label(data = sp500, mapping = ggplot2::aes(x = q80, y = 0.50, label = paste("\u2265 80% of firms:\n", round(q80, digits = 2), "B")), size = 8/.pt) 





# the cumulative distribution of firm size for S&P 400 mid-cap equities
mid_cap <- equities %>%
  dplyr::filter(Index == "S&P 400") %>%
  dplyr::group_by(Sector) %>%
  dplyr::mutate(
    q50 = stats::quantile(Market_Value_Billions, probs = 0.50), # calculate size of 50% of firms 
    q80 = stats::quantile(Market_Value_Billions, probs = 0.80) # calculate size of 80% of firms
  ) %>%
  dplyr::ungroup()

# construct the plot
ggplot2::ggplot(mid_cap, ggplot2::aes(Market_Value_Billions)) +
  ggplot2::stat_ecdf(geom = "step", linewidth = 1, col = "black", alpha = 1.00) + # geom = "area"
  ggplot2::facet_wrap(~ Sector, nrow = 3, ncol = 4) +
  ggplot2::theme_bw() +
  ggplot2::theme(text = ggplot2::element_text(size = 20)) +
  ggplot2::scale_x_log10()+
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::xlab("(Logged) Market Capitalization S&P 400, Billions (USD)") +
  ggplot2::ylab("EmpiricialCumulative Distribution Function (ECDF)") +
  # mark 50% of firms
  ggplot2::geom_vline(data = mid_cap, ggplot2::aes(xintercept = q80), linewidth = 1, linetype = "dashed", color = "red") +
  # mark 80% of firms
  ggplot2::geom_vline(data = mid_cap, ggplot2::aes(xintercept = q50), linewidth = 1, linetype = "dashed", color = "red") +
  # label the lines
  ggplot2::geom_label(data = mid_cap, mapping = ggplot2::aes(x = q50, y = 0.25, label = paste("\u2265 50% of firms:\n", round(q50, digits = 2), "B")), size = 8/.pt) + 
  ggplot2::geom_label(data = mid_cap, mapping = ggplot2::aes(x = q80, y = 0.50, label = paste("\u2265 80% of firms:\n", round(q80, digits = 2), "B")), size = 8/.pt) 





# the cumulative distribution of firm size for S&P 600 small-cap equities
small_cap <- equities %>%
  dplyr::filter(Index == "S&P 600") %>%
  dplyr::group_by(Sector) %>%
  dplyr::mutate(
    q50 = stats::quantile(Market_Value_Billions, probs = 0.50), # calculate size of 50% of firms 
    q80 = stats::quantile(Market_Value_Billions, probs = 0.80) # calculate size of 80% of firms
    ) %>%
  dplyr::ungroup()

# construct the plot
ggplot2::ggplot(small_cap, ggplot2::aes(Market_Value_Billions)) +
  ggplot2::stat_ecdf(geom = "step", linewidth = 1, col = "black", alpha = 1.00) + # geom = "area"
  ggplot2::facet_wrap(~ Sector, nrow = 3, ncol = 4) +
  ggplot2::theme_bw() +
  ggplot2::theme(text = ggplot2::element_text(size = 20)) +
  ggplot2::scale_x_log10()+
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::xlab("(Logged) Market Capitalization S&P 600, Billions (USD)") +
  ggplot2::ylab("EmpiricialCumulative Distribution Function (ECDF)") +
  # mark 50% of firms
  ggplot2::geom_vline(data = small_cap, ggplot2::aes(xintercept = q80), linewidth = 1, linetype = "dashed", color = "red") +
  # mark 80% of frims
  ggplot2::geom_vline(data = small_cap, ggplot2::aes(xintercept = q50), linewidth = 1, linetype = "dashed", color = "red") +
  # label the lines
  ggplot2::geom_label(data = small_cap, mapping = ggplot2::aes(x = q50, y = 0.25, label = paste("\u2265 50% of firms:\n", round(q50, digits = 2), "B")), size = 8/.pt) + 
  ggplot2::geom_label(data = small_cap, mapping = ggplot2::aes(x = q80, y = 0.50, label = paste("\u2265 80% of firms:\n", round(q80, digits = 2), "B")), size = 8/.pt) 





# close .r file


