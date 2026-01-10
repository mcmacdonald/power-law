


# don't run
# install package to
# install.packages("quantmod")

# tickers for stocks
tickers <- equities$Ticker

# fetch data from Yahoo Finance
stocks <- new.env() # store data in separate environment

for(ticker in tickers) {
  cat("Fetching", ticker, "...\n")
  tryCatch({
    quantmod::getSymbols(
      ticker,
      src = "yahoo",
      from = "2025-01-01",
      to = Sys.Date(),
      env = stocks,
      auto.assign = TRUE
     )
  }, error = function(e) {
      cat("Failed to fetch", ticker, "\n")
    }
   ) 
  }

# adjusted closing prices
prices <- do.call(merge, eapply(stocks, quantmod::Ad))
colnames(prices) <- gsub("\\.Adjusted", "", colnames(prices))

# don't run
# save locally
# saveRDS(prices, file = "~/Desktop/yahoo_prices.Rds")



# calculate returns, which is the percentage change in the daily stock price
# returns are comparable, prices aren't i.e., which stock moved more?
# can't answer that question from prices alone, but returns measure performance, independent of absolute price level
returns <- diff(log(prices))

# drop first trading day
returns <- returns[-1, ]

# drop first trading day of new year
returns <- returns[-nrow(returns), ]

# dates
dates <- zoo::index(returns)

# quarters
quarters <- list(
  Q1 = 1:59,
  Q2 = 60:121,
  Q3 = 122:185,
  Q4 = 186:249
  )

# function to correlate the returns i.e., do stocks move up and down together?
pearson <- function(returns){
  r <- stats::cor(
    returns, 
    method = "pearson", 
    use = "pairwise.complete.obs"
    )
}

# calculate correlations for each quarter
r_quarters <- list()
# for all in list 
for(q in names(quarters)) {
  idx <- quarters[[q]]
  returns_q <- returns[idx, ]
  r_quarters[[q]] <- pearson(returns_q)
  # date range
  cat(q, ": ", 
      as.character(dates[min(idx)]), "to", as.character(dates[max(idx)]), 
      "\n")
}
Q1 <- r_quarters[["Q1"]]
Q2 <- r_quarters[["Q2"]]
Q3 <- r_quarters[["Q3"]]
Q4 <- r_quarters[["Q4"]]

# don't run
# set abritary threshold for edges
# threshold <- 0.5 # i.e., e.g., r > 0.5

# correlations of the upper triangle
ref <- stats::cor(returns, method = "pearson", use = "pairwise.complete.obs")
ref <- ref[upper.tri(ref, diag = FALSE)]

# quantiles of the correlation coefficents
stats::quantile(ref, 0.10)
stats::quantile(ref, 0.25)
stats::quantile(ref, 0.50)
stats::quantile(ref, 0.75)
stats::quantile(ref, 0.90)
stats::quantile(ref, 0.95)
stats::quantile(ref, 0.99)

# set edge threshold at 95th percentile i.e., top 5% of all pairwise correlations
threshold <- stats::quantile(ref, 0.90)

# structural zeros to any stocks that do not meet this threshold
# returns[abs(returns) < threshold] <- 0
for(q in names(r_quarters)) {
  r_quarters[[q]][abs(r_quarters[[q]]) < threshold] <- 0
  diag(r_quarters[[q]]) <- 0 # drop self-correlations
  }

# quarterly weighted adjacency graphs 
Q1 <- r_quarters[["Q1"]]
Q2 <- r_quarters[["Q2"]]
Q3 <- r_quarters[["Q3"]]
Q4 <- r_quarters[["Q4"]]




# Keep all correlations, just zero the diagonal
for(q in names(r_quarters)) {
  diag(r_quarters[[q]]) <- 0  # Only remove self-loops
}

# Build weighted networks
networks <- list()

for(q in names(r_quarters)) {
  networks[[q]] <- igraph::graph_from_adjacency_matrix(
    r_quarters[[q]],
    mode = "undirected",
    weighted = TRUE,  # Edge weights = correlations
    diag = FALSE
    )
}

library(ergm); library(ergm.count)
test <- intergraph::asNetwork(networks[[1]])

model <- ergm(test ~ sum, 
              reference = ~Poisson,
              response = "weight",
              control = ergm::control.ergm(MCMC.prop.pkg = "ergm.count"))

# If 'test' is your network, check if the weights are stored as an edge attribute
# Example: setting 'weight' as the active response attribute
model <- ergm(
  test ~ sum, 
  reference = ~Poisson, 
  response = "weight"
  )


weighted_degree <- igraph::strength(
  networks[["Q1"]], 
  vids = igraph::V(networks[["Q1"]]),
  weights = igraph::E(networks[["Q1"]])$weight
  )


# construct from adjacency graph
graph <- function(renetturns){
  returns <- igraph::graph_from_adjacency_matrix(
    returns,
    mode = "undirected",
    weighted = TRUE,
    diag = FALSE
    )
  return(returns)
}
igraph::edge_density(graph(Q1))
igraph::edge_density(graph(Q2))
igraph::edge_density(graph(Q3))
igraph::edge_density(graph(Q4))



# plot
plot(returns,
     vertex.size = 20,
     vertex.label.cex = 0.8,
     edge.width = igraph::E(returns)$weight * 5,
     main = "Stock Return Correlation Network"
     )

# visualization with layout
layout <- igraph::layout_with_fr(returns)
plot(returns,
     layout = layout,
     vertex.color = "lightblue",
     vertex.size = 20,
     vertex.label.color = "black",
     edge.width = abs(igraph::E(returns)$weight) * 3,
     main = "Tech Stock Correlation Network"
     )

















