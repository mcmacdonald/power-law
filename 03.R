


# path to folder 
path <- "fig/pl_fit/"

# don't run
# install packages used to conduct analysis
# install.packages(c("poweRlaw", "ggplot2", "scales"))

# maximum likelihood estimation of the distribution of holdings --------------------------

  # replication
  set.seed(15092022) # Huddy's birthday

  # distribution of holdings
  size <- equities$holdings

  # maximum likelihood estimation of the power-law distribution
  # see https://www.rdocumentation.org/packages/poweRlaw/versions/0.70.6
  # see also Clauset, A., Shalizi, C. R., & Newman, M. E. 2009. Power-law distributions in empirical data. SIAM review, 51(4), 661-703.
  mle <- poweRlaw::conpl(size)
  
  # estimate the structural cut-off point for the power law distribution
  k <- poweRlaw::estimate_xmin(mle)$xmin
  mle$setXmin(k) # set the structural cut-off
  
  # estimate the scaling parameter
  a <- poweRlaw::estimate_pars(mle)$pars
  mle$setPars(a) # set the scaling parameter
  
  
  

  # bootstrapped sampling distribution of the scaling parameter
  mle_confint <- function(mle, nsim){
    
    # required packages
    require("poweRlaw")
    
    # structural cut-off values for bootstrapping
    xmin <- round(mle$xmin, digits = 4) # round the structural cut-off
    
    # maximum value of the statistical distribution
    xmax <- max(mle$dat)
    xmax <- round(xmax, digits = 4)
    
    # bootstrapped estimates 
    sims <- poweRlaw::bootstrap(
      m = mle, 
      no_of_sims = nsim, 
      # don't run
      # this code estimates the decay parameter throughout the distribution
      # xmins = seq(xmin, xmax, 0.10), # estimates xmins in the distribution
      xmins = xmin,
      # xmax = xmax,
      threads = 10, # more threads speed up the procedure
      distance = "reweight", # the distance statistic is used to calculate p-values ... reweight because distribution is likely not i.i.d.
      seed = 20110210 # Halle's birthday
      )
    
     # retain the scaling parameters
     sims <- sims$bootstraps[, 3]
  
     # compute 95% confidence intervals for the estimate of the scaling parameter
     lo <- stats::quantile(sims, 0.025, na.rm = TRUE)
     hi <- stats::quantile(sims, 0.975, na.rm = TRUE)
     
     # return
     sims <- cbind(lo, hi)
     return(sims)
  }
  cis <- mle_confint(mle = mle, nsim = 1000)
  
  
  
  # compute p-value for the hypothesis test
  # i.e., does the statistical distribution of equity holdings resemble the power-law distribution?
  mle_p <- function(mle, nsim){
    
    # required packages
    require("poweRlaw")
    
    # structural cut-off values for bootstrapping
    xmin <- round(mle$xmin, digits = 4) # round the structural cut-off
    
    # maximum value of the statistical distribution
    xmax <- max(mle$dat)
    xmax <- round(xmax, digits = 4)
    
    # calculate statistical significance
    p <- poweRlaw::bootstrap_p(
      m = mle, 
      no_of_sims = nsim,
      # don't run
      # this code computes error in decay parameter for any plausible structural cut-off k throughout the statistical distribution
      # xmins = seq(xmin, xmax, 1), # estimates xmins in the distribution
      xmins = xmin,
      # xmax = kn,
      threads = 10, # more threads speed up the procedure
      distance = "reweight",
      seed = 20110210 # Halle's birthday
      )
    
    # return
    p <- p$p # p-value
    # hypothesis test
    cat("interpretation: \n\n")
    cat("null hypothesis: statistical distribution resembles the power law distribution.\n\n")
    cat("alternate hypothesis: statistical distribution does not resemble the power law distribution.\n\n")
    cat("statistical significance (p < 0.05) provides evidence to reject the null hypothesis.\n\n")
    cat("p = "); cat(p)
    return(p)
  }
  p <- mle_p(mle = mle, nsim = 1000)
  
  
  
  
# plot the statistical distribution ---------------------------------------------------------------------
plot_fun <- function(){
    
  # plot dimensions
  par(mfrow = c(1, 1))
  
  # full empirical distribution for plotting
  size <- sort(size)
  n <- length(size)
  ccdf_empirical <- (n:1) / n
  
  # tail of the empirical distribution for plotting
  tail <- size[size >= k]
  n_tail <- length(tail)
  
  # structural cut-off
  xmin <- round(k, digits = 4)  # round the structural cut-off
  
  # maximum value of the statistical distribution
  xmax <- max(size)
  xmax <- round(xmax, digits = 4)
  
  # define confidence intervals
  length.out <- length(size[size > xmin])
  x_seq <- seq(xmin, xmax, length.out = length.out)
  
  # compute CCDF values for power-law
  scale_factor <- sum(size >= k) / length(size)
  ccdf <- function(y){ # formula: P(X > x) = (x/xmin)^(1-alpha)
    p_x <- scale_factor * (x_seq/xmin)^(1 - y)
  }
  ccdf_mu <- ccdf(a)
  ccdf_hi <- ccdf(cis[2])
  ccdf_lo <- ccdf(cis[1])
  
  # define the plot dimensions
  x_min <- min(size[size > 0])
  x_max <- max(size)
  y_min <- min(ccdf_empirical[ccdf_empirical > 0])
  y_max <- 1
  
  # log-log plot
  plot(size, ccdf_empirical, 
       log = "xy",
       pch = 21, bg = "white", col = "black", lwd = 2,
       panel.first = grid(),
       xlim = c(x_min * 0.8, x_max * 1.2),
       ylim = c(y_min * 0.8, 1),
       xlab = "Holdings (USD)", 
       ylab = "Complementary Cumulative Distribution Function (CCDF)",
       main = "Holdings in equities listed on the S&P 1,500 Composite Index",
       axes = FALSE, frame = TRUE
       )
  
  # custom x-axis with readable labels
  x_ticks <- c( # define tick locations (in billions)
    # millions
    0.00001,
    0.0001,
    0.001,
    0.01, 
    0.02, 
    0.05,
    0.1, 
    0.2, 
    0.5,
    # billions
    1, 
    2, 
    5, 
    10,
    20, 
    50,
    100
    )
  
  # ticks within data range
  x_ticks <- x_ticks[x_ticks >= x_min * 0.8 & x_ticks <= x_max * 1.2]
  
  # labels for the x-axis
  x_labels <- sapply(x_ticks, function(x) {
    if (x < 1) {
      paste0("$", x * 1000, "M")  # Millions
    } else if (x < 1000) {
      paste0("$", x, "B")  # Billions
    } else {
      paste0("$", x / 1000, "T")  # Trillions
    }
   }
  )
  
  # x-axis
  # axis(1, at = x_ticks, labels = x_labels, las = 2)  # las=2 rotates labels
  axis(1, at = x_ticks, labels = FALSE, tcl = -0.5)
  
  # y-axis
  y_log_range <- floor(log10(y_min)):0
  axis(2, at = 10^y_log_range, 
       # don't run
       # run this line instead for scientific notation
       # labels = sapply(y_log_range, function(i) as.expression(bquote(10^.(i)))))
       labels = format(10^y_log_range, scientific = FALSE, drop0trailing = TRUE), las = 1)

  # calculate position for labels on the y-axis (in log space)
  y_label_pos <- 10^(log10(y_min * 0.8) - 0.08 * (log10(y_max) - log10(y_min * 0.8)))
  
  # angle text labels on the x-axis
  for (i in seq_along(x_ticks)) {
    text(x_ticks[i], 
         y_label_pos,
         labels = x_labels[i],
         srt = 45,
         adj = c(1, 0.5),
         xpd = TRUE,
         cex = 0.85
        )
  }
  
  # x-axis title i.e., adjust line to make room
  mtext("Holdings (USD)", side = 1, line = 5, cex = 1)
  
  # shaded confidence band
  graphics::polygon(c(x_seq, rev(x_seq)), 
          c(ccdf_lo, rev(ccdf_hi)), 
          col = rgb(1, 0, 0, 0.2), 
          border = NA
          )
  
  # fitted statistical distribution
  lines(x_seq, ccdf_mu, lwd = 3, col = 2)
  
  # vertical line at xmin
  abline(v = xmin, col = "blue", lty = 2, lwd = 2)
  
  # add legend for the structural cut-off
  xmin_label <- if (xmin < 1) {
    paste0("Equities with at least $", round(xmin * 1000, 0), "M in holdings")
  } else {
    paste0("$", round(xmin, 2), "B")
  }
  
  # position label for the structural cut-off on the broken line
  text(xmin, y_max * 0.9,  # position near the top of the y-axis
       # don't run
       # run this line to include label for structural cut-off
       # labels = bquote(italic(k)[x] == .(xmin_label)),
       labels = bquote(.(xmin_label)),
       pos = 4,  # `pos = 4` means "to the right"
       col = "blue", 
       cex = 1,
       offset = 0.5
      )
  
  # construction legend
  legend(c(0.10, 0.10), 
         legend = c(
           # bquote(alpha == .(round(-a, 3)) ~ "[" * .(round(-cis[1], 3)) * "," ~ .(round(-cis[2], 3)) * "]"),
           bquote(alpha == .(round(-a, 3))),
           bquote("["* ~ .(round(-cis[1], 3)) * "," ~ .(round(-cis[2], 3)) *"]"),
           bquote(italic(p) == .(round(p, 4)))
         ),
         bty = "n", cex = 1
         )
}

# don't run
# screenshot of the plot
# fig6 <- grDevices::recordPlot()
  
# function to output high resolution images
output <- function(filename, plot_fun, path = path, width = 10, height = 5){
  grDevices::png(
    file.path(
      path, 
      filename
      ), 
      width = width, 
      height = height, 
      units = "in", 
      res = 250
    )
  plot_fun() # call the function
  grDevices::dev.off()
}

# output figure
output(
  filename = "fig06.png",
  plot_fun = plot_fun,
  path = path,
  width = 10,
  height = 5
  )





# close .r file


