


# don't run
# install packages
# install.pacakge(c("readr", "dplyr", "magrittr", "stringr", "state))



# import BlackRock's iShares data

  # url to S&P 500 i.e., large cap equities
  url_l <- "https://www.ishares.com/us/products/239726/ishares-core-sp-500-etf/1467271812596.ajax?fileType=csv&fileName=IVV_holdings&dataType=fund"
  
  # url to S&P 400 i.e., mid-cap equities
  url_m <- "https://www.ishares.com/us/products/239763/ishares-core-sp-midcap-etf/1467271812596.ajax?fileType=csv&fileName=IJH_holdings&dataType=fund"
  
  # url to S&P 600 i.e., small-cap equities
  url_s <- "https://www.ishares.com/us/products/239774/ishares-core-sp-smallcap-etf/1467271812596.ajax?fileType=csv&fileName=IJR_holdings&dataType=fund"
  


# function to import data from the wild
import <- function(url, skip.rows){
  
  # required packages
  require("readr"); require("dplyr"); require("magrittr")
  
  # call pipe
  `%>%` <- magrittr::`%>%`
  
  # read file
  file <- readr::read_csv( # note: the data files share consistent formatting, so this function is re-useable for this task
    url, # file path to the data
    skip = skip.rows, # drop the first 9 rows of the file that contain 'read me' information
    col_names = TRUE, # use the ninth row as column names
    show_col_types = FALSE # set to false
    )
  
  # clean data file ------------------------------------------------------------
  
  # replace spaces in column names
  file <- file %>% dplyr::rename_with(~stringr::str_replace(., " ", "_")) # replace spaces in column names

  # auto-detect columns of type numeric that were mistakeningly read as character
  file <- file %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(~is.character(.) && 
                all(stringr::str_detect(., "^[0-9,.\\-]+$|^-$|^$"), na.rm = TRUE)),
        ~as.numeric(stringr::str_remove_all(., "[^0-9.-]"))
      )
    )
  
  # all strings and text uppercase
  file <- file %>% dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper))
  
  # return 
  return(file)
}
l_cap <- import(url = url_l, skip.rows = 9)
m_cap <- import(url = url_m, skip.rows = 9)
s_cap <- import(url = url_s, skip.rows = 9)



# clean the data to prepare them for analysis -----------------------------------------------

# call pipe
`%>%` <- magrittr::`%>%`

# retain all mid-cap equities
m_cap <- m_cap %>% dplyr::filter(Type == "EQUITY") %>% dplyr::select(-Type)

# retain all small-cap equities
s_cap <- s_cap %>% dplyr::filter(Type == "EQUITY") %>% dplyr::select(-Type)



# identify the inconsistency across the files in the names of columns for the portfolio weight
# ... together, the weighted sums and the correlation coefficents indicate the measures to be redundant

# total weighted portfolio weights should sum to one hundred, with some error
test <- 100 - 2 # use as reference point for tests i,.e., portfolio weights sum up to ninety eight percent (98%)

# large-cap equities
l_cap %>% dplyr::summarise(w_sum = sum(`Weight_(%)`, na.rm = TRUE)) >= test

# mid-cap equities 
m_cap %>%
  dplyr::summarise(
    w_sum1 = round(sum(Market_Weight, na.rm = TRUE), digits = 2) >= test,
    w_sum2 = round(sum(Notional_Weight, na.rm = TRUE), digits = 2) >= test,
    r = stats::cor(Market_Weight, Notional_Weight, use = "complete.obs")
    )

# small-cap equities
s_cap %>%
  dplyr::summarise(
    w_sum1 = round(sum(Market_Weight, na.rm = TRUE), digits = 2) >= test,
    w_sum2 = round(sum(Notional_Weight, na.rm = TRUE), digits = 2) >= test,
    r = stats::cor(Market_Weight, Notional_Weight, use = "complete.obs")
    )



# reformulate so that each dataset has consistent column names

# large-cap equities
l_cap <- l_cap %>%
  dplyr::rename(Portfolio_Weight = `Weight_(%)`) %>%
  dplyr::select(Ticker, Name, Sector, Portfolio_Weight, Market_Value, dplyr::everything())

# mid-cap equities
m_cap <- m_cap %>%
  dplyr::rename(Portfolio_Weight = Market_Weight) %>%
  dplyr::select(Ticker, Name, Sector, Portfolio_Weight, Market_Value, dplyr::everything(), 
         -Notional_Weight)  # drop redundant column

# small-cap equities
s_cap <- s_cap %>%
  dplyr::rename(Portfolio_Weight = Market_Weight) %>%
  dplyr::select(Ticker, Name, Sector, Portfolio_Weight, Market_Value, dplyr::everything(), 
         -Notional_Weight)  # drop redundant column



# append together and add which index to the firm is listed on ...
# for reference:
# this list is the S&P Composite 1500, which combines all 1500 US equity index constituents that represent:
# 1) ~90% of the US market cap; and
# 2) all profitable, liquid, and mature US firms that meet S&P's criteria
equities <- dplyr::bind_rows(
  l_cap %>% dplyr::mutate(Index = "S&P 500"),
  m_cap %>% dplyr::mutate(Index = "S&P 400"),
  s_cap %>% dplyr::mutate(Index = "S&P 600")
  )

# drop missing obeservation
equities <- equities %>% dplyr::filter(Name != "")

# descriptive breakdowns by sector
table(equities$Sector)

# don't run
# observations for cash and/or derivatives 
# equities %>% dplyr::filter(Sector == "CASH AND/OR DERIVATIVES")

# drop
equities <- equities %>% dplyr::filter(Sector != "CASH AND/OR DERIVATIVES")



# rescale the assests under management (AUM) i.e., the sample space for the structural cut-off (xmin) of the power-law distribution is truncated at 1e+05
equities <- equities %>% 
  dplyr::mutate(aum = Market_Value / 1e12) %>%
  dplyr::filter(!is.na(aum) & aum > 0)





# close .r script

