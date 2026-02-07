


# path to folder 
path <- "fig/ccdf/"

# function to output high resolution images
output <- function(filename, figure, path = path, width = 10, height = 5){
  ggplot2::ggsave(
    filename,
    figure,
    path = path, 
    width = width, 
    height = height, 
    device = 'png', 
    dpi = 250 # larger DPI increases the size of the plot aesthetics 
    )
}



# call pipe to workspace
`%>%` <- magrittr::`%>%`



# rank order the holdings by sector
equities <- equities %>%
  dplyr::group_by(Sector) %>%
  dplyr::mutate(Rank = dplyr::dense_rank(dplyr::desc(holdings))) %>% # rank order
  dplyr::mutate(Case = dplyr::row_number()) %>% # case number for sorting
  dplyr::ungroup() %>%
  dplyr::arrange(Sector, Case, Rank) # sort

# calculate quantiles for the cumulative distribution function for S&P 1,500 composite equity index
sp1500 <- equities %>%
  dplyr::mutate(
    q50 = stats::quantile(holdings, probs = 0.50), # 50% of holdings
    q80 = stats::quantile(holdings, probs = 0.80) # 80% of holdings
    )

# the cumulative distribution of holdings for S&P 1,500 composite equity index
fig01 <- ggplot2::ggplot(sp1500, ggplot2::aes(holdings)) +
  ggplot2::stat_ecdf(geom = "step", linewidth = 1, col = "black", alpha = 1.00) +
  ggplot2::theme_bw() +
  ggplot2::theme(text = ggplot2::element_text(size = 12)) +
  ggplot2::scale_x_log10() +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::xlab("(Logged) Holdings in S&P 1,500 Composite, Billions (USD)") +
  ggplot2::ylab("Empiricial Cumulative Distribution Function (ECDF)") +
  # mark 50% of holdings
  ggplot2::geom_vline(data = sp1500, ggplot2::aes(xintercept = q80), linewidth = 1, linetype = "dashed", color = "red") +
  # mark 80% of holdings
  ggplot2::geom_vline(data = sp1500, ggplot2::aes(xintercept = q50), linewidth = 1, linetype = "dashed", color = "red") +
  # label the lines
  ggplot2::geom_label(data = sp1500, mapping = ggplot2::aes(x = q50, y = 0.25, label = paste("\u2264 50% of holdings:\n", round(q50, digits = 2), "B")), size = 6/ggplot2::.pt) + 
  ggplot2::geom_label(data = sp1500, mapping = ggplot2::aes(x = q80, y = 0.50, label = paste("\u2264 80% of holdings:\n", round(q80, digits = 2), "B")), size = 6/ggplot2::.pt) 

# output figure
output(
  filename = "fig01.png",
  figure = fig01,
  path = path,
  width = 10,
  height = 5
  )



# calculate quantiles for the cumulative distribution function for S&P 1,500 composite equity index
sp1500 <- equities %>%
  dplyr::group_by(Sector) %>%
  dplyr::mutate(
    q50 = stats::quantile(holdings, probs = 0.50), # 50% of holdings
    q80 = stats::quantile(holdings, probs = 0.80) # 80% of holdings
    ) %>%
  dplyr::ungroup()

# construct the plot
fig02 <- ggplot2::ggplot(sp1500, ggplot2::aes(holdings)) +
  ggplot2::stat_ecdf(geom = "step", linewidth = 1, col = "black", alpha = 1.00) + # geom = "area"
  ggplot2::facet_wrap(~ Sector, nrow = 3, ncol = 4, scales = "fixed") +
  ggplot2::theme_bw() +
  ggplot2::theme(text = ggplot2::element_text(size = 12)) +
  ggplot2::scale_x_log10() +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::xlab("(Logged) Holdings in S&P 1,500 Composite, Billions (USD)") +
  ggplot2::ylab("Empiricial Cumulative Distribution Function (ECDF)") +
  # mark 50% of holdings
  ggplot2::geom_vline(data = sp1500, ggplot2::aes(xintercept = q80), linewidth = 1, linetype = "dashed", color = "red") +
  # mark 80% of holdings
  ggplot2::geom_vline(data = sp1500, ggplot2::aes(xintercept = q50), linewidth = 1, linetype = "dashed", color = "red") +
  # label the lines
  ggplot2::geom_label(data = sp1500, mapping = ggplot2::aes(x = q50, y = 0.25, label = paste("\u2264 50% of holdings:\n ", round(q50, digits = 2), "B")), size = 6/ggplot2::.pt) + 
  ggplot2::geom_label(data = sp1500, mapping = ggplot2::aes(x = q80, y = 0.50, label = paste("\u2264 80% of holdings:\n ", round(q80, digits = 2), "B")), size = 6/ggplot2::.pt) 

# output figure
output(
  filename = "fig02.png",
  figure = fig02,
  path = path,
  width = 10,
  height = 5
  )



# calculate quantiles for the cumulative distribution for S&P 500 large-cap equities
sp500 <- equities %>%
  dplyr::filter(Index == "S&P 500") %>%
  dplyr::group_by(Sector) %>%
  dplyr::mutate(
    q50 = stats::quantile(holdings, probs = 0.50), # 50% of holdings
    q80 = stats::quantile(holdings, probs = 0.80) # 80% of holdings
  ) %>%
  dplyr::ungroup()

# construct the plot
fig03 <- ggplot2::ggplot(sp500, ggplot2::aes(aum)) +
  ggplot2::stat_ecdf(geom = "step", linewidth = 1, col = "black", alpha = 1.00) + # geom = "area"
  ggplot2::facet_wrap(~ Sector, nrow = 3, ncol = 4, scales = "fixed") +
  ggplot2::theme_bw() +
  ggplot2::theme(text = ggplot2::element_text(size = 12)) +
  ggplot2::scale_x_log10()+
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::xlab("(Logged) Holdings in S&P 500, Billions (USD)") +
  ggplot2::ylab("Empiricial Cumulative Distribution Function (ECDF)") +
  # mark 50% of holdings
  ggplot2::geom_vline(data = sp500, ggplot2::aes(xintercept = q80), linewidth = 1, linetype = "dashed", color = "red") +
  # mark 80% of holdings
  ggplot2::geom_vline(data = sp500, ggplot2::aes(xintercept = q50), linewidth = 1, linetype = "dashed", color = "red") +
  # label the lines
  ggplot2::geom_label(data = sp500, mapping = ggplot2::aes(x = q50, y = 0.25, label = paste("\u2264 50% of holdings:\n", round(q50, digits = 2), "B")), size = 6/ggplot2::.pt) + 
  ggplot2::geom_label(data = sp500, mapping = ggplot2::aes(x = q80, y = 0.50, label = paste("\u2264 80% of holdings:\n", round(q80, digits = 2), "B")), size = 6/ggplot2::.pt) 

# output figure
output(
  filename = "fig03.png",
  figure = fig03,
  path = path,
  width = 10,
  height = 5
  )



# calculate the quantiles for the cumulative distribution of holdings for S&P 400 mid-cap equities
mid_cap <- equities %>%
  dplyr::filter(Index == "S&P 400") %>%
  dplyr::group_by(Sector) %>%
  dplyr::mutate(
    q50 = stats::quantile(aum, probs = 0.50), # 50% of holdings
    q80 = stats::quantile(aum, probs = 0.80) # 80% of holdings
  ) %>%
  dplyr::ungroup()

# construct the plot
fig04 <- ggplot2::ggplot(mid_cap, ggplot2::aes(holdings)) +
  ggplot2::stat_ecdf(geom = "step", linewidth = 1, col = "black", alpha = 1.00) + # geom = "area"
  ggplot2::facet_wrap(~ Sector, nrow = 3, ncol = 4, scales = "fixed") +
  ggplot2::theme_bw() +
  ggplot2::theme(text = ggplot2::element_text(size = 12)) +
  ggplot2::scale_x_log10()+
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::xlab("(Logged) Holdings in S&P 400, Billions (USD)") +
  ggplot2::ylab("Empiricial Cumulative Distribution Function (ECDF)") +
  # mark 50% of holdings
  ggplot2::geom_vline(data = mid_cap, ggplot2::aes(xintercept = q80), linewidth = 1, linetype = "dashed", color = "red") +
  # mark 80% of holdings
  ggplot2::geom_vline(data = mid_cap, ggplot2::aes(xintercept = q50), linewidth = 1, linetype = "dashed", color = "red") +
  # label the lines
  ggplot2::geom_label(data = mid_cap, mapping = ggplot2::aes(x = q50, y = 0.25, label = paste("\u2264 50% of holdings:\n", round(q50, digits = 2), "B")), size = 6/ggplot2::.pt) + 
  ggplot2::geom_label(data = mid_cap, mapping = ggplot2::aes(x = q80, y = 0.50, label = paste("\u2264 80% of holdings:\n", round(q80, digits = 2), "B")), size = 6/ggplot2::.pt) 

# output figure
output(
  filename = "fig04.png",
  figure = fig04,
  path = path,
  width = 10,
  height = 5
  )



# calculate quantiles for the the cumulative distribution of holdings for S&P 600 small-cap equities
small_cap <- equities %>%
  dplyr::filter(Index == "S&P 600") %>%
  dplyr::group_by(Sector) %>%
  dplyr::mutate(
    q50 = stats::quantile(holdings, probs = 0.50), # 50% of holdings 
    q80 = stats::quantile(holdings, probs = 0.80) # 80% of holdings
    ) %>%
  dplyr::ungroup()

# construct the plot
fig05 <- ggplot2::ggplot(small_cap, ggplot2::aes(holdings)) +
  ggplot2::stat_ecdf(geom = "step", linewidth = 1, col = "black", alpha = 1.00) + # geom = "area"
  ggplot2::facet_wrap(~ Sector, nrow = 3, ncol = 4, scales = "fixed") +
  ggplot2::theme_bw() +
  ggplot2::theme(text = ggplot2::element_text(size = 12)) +
  ggplot2::scale_x_log10()+
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::xlab("(Logged) Holdings in S&P 600, Billions (USD)") +
  ggplot2::ylab("Empiricial Cumulative Distribution Function (ECDF)") +
  # mark 50% of holdings
  ggplot2::geom_vline(data = small_cap, ggplot2::aes(xintercept = q80), linewidth = 1, linetype = "dashed", color = "red") +
  # mark 80% of holdings
  ggplot2::geom_vline(data = small_cap, ggplot2::aes(xintercept = q50), linewidth = 1, linetype = "dashed", color = "red") +
  # label the lines
  ggplot2::geom_label(data = small_cap, mapping = ggplot2::aes(x = q50, y = 0.25, label = paste("\u2264 50% of holdings:\n", round(q50, digits = 2), "B")), size = 6/ggplot2::.pt) + 
  ggplot2::geom_label(data = small_cap, mapping = ggplot2::aes(x = q80, y = 0.50, label = paste("\u2264 80% of holdings:\n", round(q80, digits = 2), "B")), size = 6/ggplot2::.pt) 

# output figure
output(
  filename = "fig05.png",
  figure = fig05,
  path = path,
  width = 10,
  height = 5
  )





# close .r file


