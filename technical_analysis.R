# BDA400 - Assignment 2: Technical Analysis using R
# Preliminary Stage
# Run from the Assignment2 directory or set the working directory to this file's folder.

required_packages <- c("quantmod", "TTR", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(quantmod)
library(TTR)
library(ggplot2)

# Read one stock symbol per line from portfolio.txt.
read_portfolio <- function(file = "portfolio.txt") {
  if (!file.exists(file)) stop("Portfolio file not found: ", file)
  symbols <- trimws(readLines(file, warn = FALSE))
  symbols <- symbols[nzchar(symbols) & !grepl("^\\s*#", symbols)]
  symbols <- unique(toupper(symbols))
  if (!length(symbols)) stop("portfolio.txt contains no stock symbols.")
  symbols
}

# Import daily OHLCV data from Yahoo Finance for each symbol.
# Returns a named list of xts objects; failed symbols are reported and skipped.
load_stock_data <- function(file = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date(),
                            auto.assign = FALSE) {
  symbols <- read_portfolio(file)
  result <- list()
  failures <- character()

  for (symbol in symbols) {
    tryCatch({
      x <- quantmod::getSymbols(
        Symbols = symbol, src = "yahoo",
        from = as.Date(from), to = as.Date(to),
        auto.assign = FALSE, warnings = FALSE
      )
      if (NROW(x) == 0) stop("No rows returned")
      result[[symbol]] <- x
    }, error = function(e) {
      failures <<- c(failures, paste0(symbol, ": ", conditionMessage(e)))
    })
  }

  if (length(failures)) {
    warning("Some symbols could not be loaded:\n", paste(failures, collapse = "\n"))
  }
  if (!length(result)) stop("No stock data could be loaded.")
  result
}

# Statistical mode: returns every value tied for highest frequency.
statistical_mode <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  counts <- table(x)
  as.numeric(names(counts)[counts == max(counts)])
}

# Calculate requested statistics using the adjusted closing-price series.
# Moving average is a 20-trading-day simple moving average (latest value).
calculate_statistics <- function(stock_data, ma_n = 20) {
  if (NCOL(stock_data) < 1 || NROW(stock_data) < 1)
    stop("Stock data must contain at least one row and one column.")

  adjusted <- quantmod::Ad(stock_data)
  prices <- as.numeric(adjusted)
  prices <- prices[is.finite(prices)]
  if (!length(prices)) stop("No finite adjusted prices available.")

  ma <- TTR::SMA(adjusted, n = ma_n)
  latest_ma <- as.numeric(tail(ma[is.finite(ma)], 1))
  if (!length(latest_ma)) latest_ma <- NA_real_

  data.frame(
    Mean = mean(prices),
    Mode = paste(signif(statistical_mode(prices), 8), collapse = ", "),
    Median = median(prices),
    Standard_Deviation = stats::sd(prices),
    Moving_Average_20D = latest_ma,
    Observations = length(prices),
    Latest_Adjusted_Close = tail(prices, 1),
    row.names = NULL,
    check.names = FALSE
  )
}

# Compute one statistics row per stock.
calculate_portfolio_statistics <- function(stock_list, ma_n = 20) {
  if (!length(stock_list)) stop("stock_list is empty.")
  rows <- lapply(stock_list, calculate_statistics, ma_n = ma_n)
  out <- do.call(rbind, rows)
  out$Symbol <- names(stock_list)
  out <- out[, c("Symbol", setdiff(names(out), "Symbol"))]
  rownames(out) <- NULL
  out
}

# Display a concise data preview and statistics for every loaded symbol.
display_stock_data <- function(stock_list, n = 6) {
  for (symbol in names(stock_list)) {
    cat("\n================", symbol, "================\n")
    print(utils::head(as.data.frame(stock_list[[symbol]]), n))
    cat("\nMost recent rows:\n")
    print(utils::tail(as.data.frame(stock_list[[symbol]]), n))
  }
}

# Plot adjusted closing prices and the 20-day simple moving average.
plot_stock_data <- function(stock_list, ma_n = 20) {
  for (symbol in names(stock_list)) {
    x <- stock_list[[symbol]]
    adjusted <- quantmod::Ad(x)
    ma <- TTR::SMA(adjusted, n = ma_n)
    plot_df <- data.frame(
      Date = as.Date(zoo::index(adjusted)),
      Adjusted_Close = as.numeric(adjusted),
      Moving_Average = as.numeric(ma)
    )
    p <- ggplot(plot_df, aes(x = Date)) +
      geom_line(aes(y = Adjusted_Close, colour = "Adjusted close"), na.rm = TRUE) +
      geom_line(aes(y = Moving_Average, colour = paste0(ma_n, "-day SMA")), na.rm = TRUE) +
      labs(title = paste(symbol, "Adjusted Closing Price"),
           x = "Date", y = "Price", colour = "Series") +
      theme_minimal()
    print(p)
  }
}

# ---- Run the preliminary analysis ----
# Ensure portfolio.txt is in the current working directory.
stocks <- load_stock_data()
display_stock_data(stocks)
statistics <- calculate_portfolio_statistics(stocks)
print(statistics)
plot_stock_data(stocks)

# Optional: save results for evidence/reporting.
write.csv(statistics, "stock_statistics.csv", row.names = FALSE)
