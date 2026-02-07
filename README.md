This repository contains .R files that:

1) download BlackRock's iShares data for the S&P 500, S&P 400 (mid-cap ETFs), and S&P 600 (small-cap ETFs)
2) join them together to get their ETF holdings in the S&P Composite 1,500 equity index
3) analyze their ETF holdings in the S&P Composite 1,500
4) analyze whether their ETF holdings in S&P Composite 1,500 resembles a power-law distribution
5) analyze whether their ETF holdings in different sectors resemble power-law distributions

The rationale for analyzing BlackRock's equity holdings across the entire S&P composite 1,500 equity index is the arbitrary classifications of large-cap equities, mid-cap equities, and small-cap equities. Market cap is not the only thing that determines who is part of what index. For instance, some S&P 500 components have market caps less than the current market cap eligibility criterion for new membership into the S&P 500; however, once in, firms do not to have to continually meet this criterion for continued membership. A number of [other criteria](https://www.fool.com/investing/2019/02/09/how-are-sp-500-stocks-chosen.aspx) also determine S&P 500 index listings.

To import and analyze BlackRock's ETF holdings, first run file 01.R, then 02.R, then 03.R, etc.
