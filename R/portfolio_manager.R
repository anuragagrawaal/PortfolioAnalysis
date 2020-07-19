#' @title PortfolioManager
#' @description Gives the detailed  Risk-Reward Metrics as Computed by MorningStar
#' @param ticker Enter the Fund TICKER
#' @param view Choose a View of the Portfolio, Default: c("overview", "detailed")
#' @return The output is a tibble that gives the overview of the risk and reward metrics of different holdings in a portfoluio.
#' @details This function can be used to examine and assess your holdings in different funds
#' @import dplyr
#' @importFrom purrr map_dbl possibly map_df
#' @import rvest
#' @import dplyr
#' @import stringr
#' @import stringi
#' @import rMorningStar
#' @importFrom lubridate years
#' @import xml2
#' @import tidyr
#' @importFrom readr parse_number
#' @importFrom xts to.monthly
#' @importFrom utils tail
#' @rdname PortfolioManager
#' @export
PortfolioManager = function(ticker, view = c('overview', 'detailed')){
  performance_df = map_df(ticker,possibly(function(ticker){
    performance =  paste0('https://in.finance.yahoo.com/quote/', ticker,'/performance?p=',  ticker)
    performance = tolower( read_html(performance) %>% html_node('body') %>% xml_find_all("//div[contains(@class, 'Bdbw(1px) Bdbc($seperatorColor) Bdbs(s) H(25px) Pt(10px)')]") %>% html_text())


    return_rating = stri_escape_unicode(str_remove_all(str_subset(performance, 'morningstar return rating'), 'morningstar return rating'))
    return_rating = c(nchar(return_rating)/6)

    ytd_cat_ret = str_remove_all(str_subset(performance, 'ytd'), 'ytd')[1]
    DF = data.frame(ytd_cat_ret)
    ytd_cat_ret = separate(DF, ytd_cat_ret, into = c('pre', 'post'), sep = '%') %>% pull('post')
    ytd_cat_ret = parse_number(ytd_cat_ret)
    ytd_cat_ret = c(ytd_cat_ret/100)

    one_mon_cat = str_remove_all(str_subset(performance, '1-month'), '1-month')[1]
    DF = data.frame(one_mon_cat)
    one_mon_cat = separate(DF, one_mon_cat, into = c('pre', 'post'), sep = '%') %>% pull('post')
    one_mon_cat = parse_number(one_mon_cat)
    one_mon_cat = c(one_mon_cat/100)

    three_mon_cat = str_remove_all(str_subset(performance, '3-month'), '3-month')[1]
    DF = data.frame(three_mon_cat)
    three_mon_cat = separate(DF, three_mon_cat, into = c('pre', 'post'), sep = '%') %>% pull('post')
    three_mon_cat = parse_number(three_mon_cat)
    three_mon_cat = c(three_mon_cat/100)

    one_year_cat = str_remove_all(str_subset(performance, '1-year'), '1-year')[1]
    DF = data.frame(one_year_cat)
    one_year_cat = separate(DF, one_year_cat, into = c('pre', 'post'), sep = '%') %>% pull('post')
    one_year_cat = parse_number(one_year_cat)
    one_year_cat = c(one_year_cat/100)

    three_year_cat = str_remove_all(str_subset(performance, '3-year'), '3-year')[1]
    DF = data.frame(three_year_cat)
    three_year_cat = separate(DF, three_year_cat, into = c('pre', 'post'), sep = '%') %>% pull('post')
    three_year_cat = parse_number(three_year_cat)
    three_year_cat = c(three_year_cat/100)

    five_year_cat = str_remove_all(str_subset(performance, '5-year'), '5-year')[2]
    DF = data.frame(five_year_cat)
    five_year_cat = separate(DF, five_year_cat, into = c('pre', 'post'), sep = '%') %>% pull('post')
    five_year_cat = parse_number(five_year_cat)
    five_year_cat = c(five_year_cat/100)

    ten_year_cat = str_remove_all(str_subset(performance, '10-year'), '10-year')[1]
    DF = data.frame(ten_year_cat)
    ten_year_cat = separate(DF, ten_year_cat, into = c('pre', 'post'), sep = '%') %>% pull('post')
    ten_year_cat = parse_number(ten_year_cat)
    ten_year_cat = c(ten_year_cat/100)

    ytd_cat_rank = parse_number(str_remove_all(str_subset(performance, 'ytd'), 'ytd'))[[2]]
    one_month_rank = parse_number(str_remove_all(str_subset(performance, '1-month'), '1-month'))[[2]]
    three_month_rank = parse_number(str_remove_all(str_subset(performance, '3-month'), '3-month'))[[2]]
    one_year_rank = parse_number(str_remove_all(str_subset(performance, '1-year'), '1-year'))[[2]]
    three_year_rank = parse_number(str_remove_all(str_subset(performance, '3-year'), '3-year'))[[2]]
    five_year_rank = parse_number(str_remove_all(str_subset(performance, '5-year'), '5-year'))[[3]]

    ytd_return = ms.ytdreturn(ticker)


    year = paste0(Sys.Date() - 1115,'/')
    dat = getSymbols(ticker, auto.assign = FALSE)
    current_price = as.numeric(tail(dat,n = 1)[,6])
    fund = to.monthly(dat)
    fund = fund[year,]
    ytd_return = as.numeric(tail(periodReturn(fund[,6], period = 'yearly'),n = 1)[,1])

    one_m_ret = parse_number(str_remove_all(str_subset(performance, '1-month'), '1-month'))[[1]]/100
    three_m_ret = parse_number(str_remove_all(str_subset(performance, '3-month'), '3-month'))[[1]]/100
    one_y_ret = parse_number(str_remove_all(str_subset(performance, '1-year'), '1-year'))[[1]]/100
    three_y_ret = parse_number(str_remove_all(str_subset(performance, '3-year'), '3-year'))[[1]]/100
    five_y_ret = parse_number(str_remove_all(str_subset(performance, '5-year'), '5-year'))[[1]]/100
    ten_y_ret = parse_number(str_remove_all(str_subset(performance, '10-year'), '10-year'))[[1]]/100

    ytd_excess_cat_return = abs(ytd_return) - abs(ytd_cat_ret)
    one_m_excess_cat_ret = abs(one_m_ret) - abs(one_mon_cat)
    three_m_excess_cat_ret = abs(three_m_ret) - abs(three_mon_cat)
    one_y_excess_cat_ret = abs(one_y_ret) - abs(one_year_cat)
    three_y_excess_cat_ret = abs(three_y_ret) - abs(three_year_cat)
    five_y_excess_cat_ret = abs(five_y_ret) - abs(three_year_cat)
    ten_y_excess_cat_ret = abs(ten_y_ret) - abs(ten_year_cat)


    risk =  paste0('https://in.finance.yahoo.com/quote/', ticker,'/risk?p=',  ticker)
    risk = tolower( read_html(risk) %>% html_node('body') %>% xml_find_all("//div[contains(@class, 'Bdbw(1px) Bdbc($seperatorColor) Bdbs(s) H(25px) Pt(10px)')]") %>% html_text())
    risk_rating= nchar( stri_escape_unicode(str_remove_all(str_subset(risk, 'morningstar risk rating'), 'morningstar risk rating')))/6
    alpha = parse_number(str_remove_all(str_subset(risk, 'alpha'), 'alpha'))[[1]]
    beta = parse_number(str_remove_all(str_subset(risk, 'beta'), 'beta'))[[1]]
    stdev = parse_number(str_remove_all(str_subset(risk, 'standard deviation'), 'standard deviation'))[[1]]/100
    sharpe_ratio = parse_number(str_remove_all(str_subset(risk, 'sharpe ratio'), 'sharpe ratio'))[[1]]
    treynor_ratio = parse_number(str_remove_all(str_subset(risk, 'treynor ratio'), 'treynor'))[[1]]

    profile =  paste0('https://in.finance.yahoo.com/quote/', ticker,'/profile?p=',  ticker)
    profile = tolower( read_html(profile) %>% html_node('body') %>% xml_find_all("//div[contains(@class, 'Bdbw(1px) Bdbc($seperatorColor) Bdbs(s) H(25px) Pt(10px)')]") %>% html_text())
    fund_category = str_to_title( str_remove_all(str_subset(profile, 'category'), 'category')[[1]])
    fund_family = str_to_title( str_remove_all(str_subset(profile, 'fund family'), 'fund family'))
    net_asset = parse_number(str_remove_all(str_subset(profile, 'net assets'), 'net assets'))
    rating = as.numeric(nchar( stri_escape_unicode( str_remove_all(str_subset(profile, 'morningstar rating'), 'morningstar rating')))/6)
    inception_date = str_remove_all(str_subset(profile, 'inception date'), 'inception date')
    last_dividend = as.numeric( str_remove_all(str_subset(profile, 'last dividend'), 'last dividend'))
    annual_net_expense_ratio = as.numeric(parse_number( str_remove_all(str_subset(profile, 'annual report expense ratio'), 'annual report expense ratio')))/100
    yield = as.numeric(parse_number( str_remove_all(str_subset(profile, 'yield'), 'yield')))/100
    category = fund_category
    master_cat =   case_when(
      category == 'Latin America Stock' ~ 'Equity',
      category == 'China Region' ~ 'Equity',
      category == 'Indian Equity' ~ 'Equity',
      category == 'Japan Stock' ~ 'Equity',
      category == 'Pacific/Asia ex. Japan Stock' ~ 'Equity',
      category == 'Diversified Emerging Mkts' ~ 'Equity',
      category == 'Diversified Pacific/Asia' ~ 'Equity',
      category == 'Equity Energy' ~ 'Equity',
      category == 'Europe Stock' ~ 'Equity',
      category == 'Foreign Large Blend' ~ 'Equity',
      category == 'Foreign Large Growth' ~ 'Equity',
      category == 'Foreign Large Value' ~ 'Equity',
      category == 'Foreign Small/Mid Blend' ~ 'Equity',
      category == 'Foreign Small/Mid Growth' ~ 'Equity',
      category == 'Foreign Small/Mid Value' ~ 'Equity',
      category == 'Health' ~ 'Equity',
      category == 'Communications' ~ 'Equity',
      category == 'Consumer Cyclical' ~ 'Equity',
      category == 'Consumer Defensive' ~ 'Equity',
      category == 'Equity Precious Metals' ~ 'Equity',
      category == 'Financial' ~ 'Equity',
      category == 'Global Real Estate' ~ 'Equity',
      category == 'Industrials' ~ 'Equity',
      category == 'Natural Resources' ~ 'Equity',
      category == 'Technology' ~ 'Equity',
      category == 'Utilities' ~ 'Equity',
      category == 'Miscellaneous Sector' ~ 'Equity',
      category == 'Miscellaneous Sector' ~ 'Equity',
      category == 'Large Growth' ~ 'Equity',
      category == 'Large Blend' ~ 'Equity',
      category == 'Large Value' ~ 'Equity',
      category == 'Mid-Cap Blend' ~ 'Equity',
      category == 'Mid-Cap Growth' ~ 'Equity',
      category == 'Mid-Cap Value' ~ 'Equity',
      category == 'Small Blend' ~ 'Equity',
      category == 'Small Growth' ~ 'Equity',
      category == 'Small Value' ~ 'Equity',
      category == 'World Large Stock' ~ 'Equity',
      category == 'World Large Stock' ~ 'Equity',
      category == 'Corporate Bond' ~ 'Fixed Income',
      category == 'High Yield Bond' ~ 'Fixed Income',
      category == 'Inflation-Protected Bond' ~ 'Fixed Income',
      category == 'Intermediate Core Bond' ~ 'Fixed Income',
      category == 'Intermediate Core Bond' ~ 'Fixed Income',
      category == 'Intermediate Core-Plus Bond' ~ 'Fixed Income',
      category == 'Long Government' ~ 'Fixed Income',
      category == 'Long-Term Bond' ~ 'Fixed Income',
      category == 'Multisector Bond' ~ 'Fixed Income',
      category == 'Nontraditional Bond' ~ 'Fixed Income',
      category == 'Short Government' ~ 'Fixed Income',
      category == 'Short-Term Bond' ~ 'Fixed Income',
      category == 'World Bond' ~ 'Fixed Income',
      category == 'Emerging Markets Bond' ~ 'Fixed Income',
      category == 'World Bond-USD Hedged' ~ 'Fixed Income',
      category == 'Ultrashort Bond' ~ 'Fixed Income',
      category == 'Preferred Stock' ~ 'Fixed Income',
      category == 'Emerging-Markets Local-Currency Bond' ~ 'Fixed Income',
      category == 'Bank Loan' ~ 'Fixed Income',
      category == 'Target Maturity' ~ 'Fixed Income',
      category == 'Target-Date 2000-2010' ~ 'Hybrid',
      category == 'Target-Date 2015' ~ 'Hybrid',
      category == 'Target-Date 2020' ~ 'Hybrid',
      category == 'Target-Date 2025' ~ 'Hybrid',
      category == 'Target-Date 2030' ~ 'Hybrid',
      category == 'Target-Date 2035' ~ 'Hybrid',
      category == 'Target-Date 2040' ~ 'Hybrid',
      category == 'Target-Date 2045' ~ 'Hybrid',
      category == 'Target-Date 2050' ~ 'Hybrid',
      category == 'Target-Date 2055' ~ 'Hybrid',
      category == 'Target-Date 2060+' ~ 'Hybrid',
      category == 'Allocation--30% to 50% Equity' ~ 'Hybrid',
      category == 'Allocation--15% to 30% Equity' ~ 'Hybrid',
      category == 'Allocation--50% to 70% Equity' ~ 'Hybrid',
      category == 'Allocation--70% to 85% Equity' ~ 'Hybrid',
      category == 'Allocation--85%+ Equity' ~ 'Hybrid',
      category == 'World Allocation' ~ 'Hybrid',
      category == 'Target-Date Retirement' ~ 'Hybrid',
      category == 'Prime Money Market' ~ 'Money Market',
      category == 'Money Market-Taxable' ~ 'Money Market',
      category == 'Muni National Interm' ~ 'Municipal Bond Funds',
      category == 'Real Estate' ~ 'Real Estate',
      TRUE ~ 'Other')

    top10_holding = ms.Top10HoldingTotal(ticker)
    percent_change = ms.PercentChange(ticker)
    fund_name = ms.FundName(ticker)

    data.frame(return_rating, ytd_cat_ret , one_mon_cat, three_mon_cat, one_year_cat, three_year_cat,
               five_year_cat, ten_year_cat, ytd_cat_rank, one_month_rank, three_month_rank, one_year_rank,
               three_year_rank, five_year_rank, ytd_return, one_m_ret, three_m_ret, one_y_ret, three_y_ret,
               five_y_ret, ten_y_ret, ytd_excess_cat_return, one_m_excess_cat_ret, three_m_excess_cat_ret,
               one_y_excess_cat_ret, three_y_excess_cat_ret, five_y_excess_cat_ret, ten_y_excess_cat_ret, current_price,
               top10_holding, percent_change, fund_name,
               master_cat ,fund_category, fund_family, net_asset, rating, inception_date, last_dividend, annual_net_expense_ratio, yield,
               risk_rating, alpha, beta, stdev, sharpe_ratio, treynor_ratio)
  }, otherwise = NA_real_)
)

master = data.frame('Fund.Name' = performance_df$fund_name, 'Ticker' = ticker,
                      'Current.Price' = performance_df$current_price, 'Percentage.Change' = performance_df$percent_change, 'Master Category' = performance_df$master_cat,
                      'Fund.Category' = performance_df$fund_category,
                      'Fund Family' = performance_df$fund_family, 'Inception Date' = performance_df$inception_date,
                      'Fund Size Total Assets' = performance_df$net_asset, 'Morningstar Return Rating' = performance_df$return_rating,
                      'Morningstar Risk Rating' = performance_df$risk_rating, 'Morningstar Rating' = performance_df$rating,
                      'YTD Return' = performance_df$ytd_return, 'YTD Return Rank Category' = performance_df$ytd_cat_rank,
                      'YTD Return Category' = performance_df$ytd_cat_ret, 'YTD Return Excess Category' = performance_df$ten_y_ret,
                      'One Month Return' = performance_df$one_m_ret, 'One Month Rank Category' = performance_df$one_month_rank,
                      'One Month Return Category' = performance_df$one_mon_cat, "One Month Excess Category" = performance_df$one_m_excess_cat_ret,
                      'Three Month Return' = performance_df$three_m_ret, 'Three Month Rank Category' = performance_df$three_month_rank,
                      'Three Month Category' = performance_df$one_year_cat, 'Three Month Excess Category' = performance_df$three_m_excess_cat_ret ,
                      'One Year Return' = performance_df$one_y_ret,'One Year Rank Category' = performance_df$one_year_rank,
                      'One Year Return Category' = performance_df$one_year_cat, 'One Year Excess Category' = performance_df$one_y_excess_cat_ret,
                      'Three Year Return' = performance_df$three_y_ret, 'Three Year Rank Category' = performance_df$three_year_rank,
                      'Three Year Return Category' = performance_df$three_year_cat,'Three Year Excess Category' = performance_df$three_y_excess_cat_ret,
                      'Five Year Return' = performance_df$five_y_ret, 'Five Year Rank Category' = performance_df$five_year_rank,
                      'Five Year Return Category' = performance_df$ten_year_cat,
                      'Five Year Excess Category' = performance_df$five_y_excess_cat_ret,  'Ten Year Return' = performance_df$ten_y_ret,
                      'Ten Year Return Category' = performance_df$ten_year_cat, 'Ten Year Excess Category' = performance_df$ten_y_excess_cat_ret,
                      'Expense ratio' = performance_df$annual_net_expense_ratio, 'Beta' = performance_df$beta, 'Alpha' = performance_df$alpha, 'Std Dev' = performance_df$stdev,
                      'Sharpe Ratio' = performance_df$sharpe_ratio , 'Tryenor Ratio' = performance_df$treynor_ratio,
                      'Top Ten Holdings' = performance_df$top10_holding, 'Last Dividend' = performance_df$last_dividend, 'Yield' = performance_df$yield
  )

overview = data.frame('Fund.Name' = performance_df$fund_name, 'Ticker' = ticker,
                        'Current.Price' = performance_df$current_price, 'Percentage.Change' = performance_df$percent_change, 'Master Category' = performance_df$master_cat,
                        'Fund.Category' = performance_df$fund_category,
                        'Fund Family' = performance_df$fund_family,
                        'Fund Size Total Assets' = performance_df$net_asset, 'Morningstar Rating' = performance_df$rating,
                        'YTD Return' = performance_df$ytd_return, 'YTD Return Rank Category' = performance_df$ytd_cat_rank,
                        'YTD Return Category' = performance_df$ytd_cat_ret, 'YTD Return Excess Category' = performance_df$ten_y_ret,
                        'Three Year Return' = performance_df$three_y_ret, 'Three Year Rank Category' = performance_df$three_year_rank,
                        'Three Year Return Category' = performance_df$three_year_cat,'Three Year Excess Category' = performance_df$three_y_excess_cat_ret,
                        'Five Year Return' = performance_df$five_y_ret, 'Five Year Rank Category' = performance_df$five_year_rank,
                        'Five Year Return Category' = performance_df$ten_year_cat,
                        'Five Year Excess Category' = performance_df$five_y_excess_cat_ret,  'Ten Year Return' = performance_df$ten_y_ret,
                        'Ten Year Return Category' = performance_df$ten_year_cat, 'Ten Year Excess Category' = performance_df$ten_y_excess_cat_ret,
                        'Expense ratio' = performance_df$annual_net_expense_ratio, 'Beta' = performance_df$beta, 'Alpha' = performance_df$alpha, 'Std Dev' = performance_df$stdev,
                        'Top Ten Holdings' = performance_df$top10_holding
  )


  detailed = as_tibble(master)
  overview = as_tibble(master)

  if(view == 'detailed'){
    detailed
  }else if(view == 'overview'){
    overview
  }

}
