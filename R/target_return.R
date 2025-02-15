#' @title Optimize portfolio weights given target returns
#' @description Optimizes portfolio weights by minimizing the variance for a given target return
#' @param asset.names Vector of ticker of securities
#' @param increment Number of portfolio to be generated, Default: 100
#' @param rf Risk-free rate of return, Default: 0
#' @param tgt.ret Target return for given funds
#' @param period Period for which the returns are calculated, Default: c("months", "weeks", "quarters", "years")
#' @return Returns a dataframe of the portfolios with different portfolio weights and different target returns and target standard deviation. Using this an investor can choose between range of portfolio to allocate funds.
#' @details Minimizes the variance using the method of lagrange multiplier to calculate the portfolio weights with minimized variance for given target return.
#' @examples
#' optim.TargetReturn(c('FXAIX', 'AAPL'), period = 'weeks', tgt.ret = 0.003)
#' @rdname optim.TargetReturn
#' @import quantmod
#' @importFrom purrr map
#' @import PerformanceAnalytics
#' @importFrom stats cov
#' @import quadprog
#' @importFrom xts to.period merge.xts
#' @importFrom lubridate years
#' @export
optim.TargetReturn = function(asset.names, increment = 100, rf = 0, tgt.ret, period = c('months', 'weeks', 'quarters', 'years')){
  dat = map(.x = asset.names, .f = function(x){
    raw = getSymbols(x, auto.assign = FALSE)
    raw = to.period(raw, period = period)
  })
  dat_clean = map(.x = 1:length(asset.names), function(x){
    trim = paste0(format(Sys.Date() - years(5), '%Y'), '/')
    dat = dat[[x]][trim,6]
    dat = CalculateReturns(dat)
  })
  Ret.monthly = do.call(merge.xts, dat_clean)
  Ret.monthly = Ret.monthly[-1,]
  colnames(Ret.monthly) = asset.names

  mat.ret = matrix(Ret.monthly, nrow = nrow(Ret.monthly))
  colnames(mat.ret) = c(asset.names)
  VCOV = cov(mat.ret)
  avg.ret = matrix(apply(mat.ret, 2, mean), nrow = ncol(Ret.monthly), ncol = 1)
  rownames(avg.ret) = c(asset.names)
  colnames(avg.ret) = c('avg.ret')
  tgt.sd = rep(0, length(tgt.ret))
  wgt = matrix(0, nrow = 100, ncol = length(avg.ret))

  Dmat = 2*VCOV
  dvec = rep.int(0, nrow(avg.ret))
  Amat = cbind(1, avg.ret, diag(nrow(avg.ret)))
  for(i in 1:100){
    bvec = c(1, tgt.ret, rep(0, nrow(avg.ret)))
    soln = solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 2)
    wgt[i,] = soln$solution
    wgt = ifelse(wgt<0.00001, 0, wgt)
    tgt.sd[i] = sqrt(wgt[i,] %*% VCOV %*% wgt[i,])
  }
  colnames(wgt) = c(paste0(asset.names, rep('.wgt', nrow(avg.ret))))
  sharpe_ratio = (tgt.ret - rf)/tgt.sd
  port_summary = cbind(tgt.ret,tgt.sd, wgt, sharpe_ratio)
  port_summary = as.data.frame(port_summary)
  port_summary[1,]

}
