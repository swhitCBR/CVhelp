#' @title NA substitution using moving average
#' @description Substitute variable specific moving averages for daily values
#'
#' @param DF_w_in dataframe with columns to be replaced by moving averages
#' @param col_conv column names for conversion
#' @param MA_window moving average window size (days)
#' @param fillNAs logical, indicating whether NAs at start and end are replaced with estimates from 3-day MA. Defaults to TRUE
#'
#' @returns wide-format dataframe that was input, but with some values replaced with various moving averages
#' @export
#'
sub_MA_data <- function(DF_w_in,
                        col_conv=c("CLC","VNS","SWP","CVP","MSD"),
                        MA_window=c(3,5,5,5,7),
                        fillNAs=TRUE){
  
  tmpDF <- DF_w_in[,-which(names(DF_w_in) %in% col_conv)]
  DF_w_in <- DF_w_in[,col_conv]
  whch_MA_chk= match(col_conv,names(DF_w_in))
  if(length(whch_MA_chk)!=length(MA_window | length(whch_MA_chk)==0)){return(NULL)}
  imp_cols <- col_conv
  imp_mat <- matrix(NA,nrow=nrow(DF_w_in),ncol=length(imp_cols))
  colnames(imp_mat) <- imp_cols
  for(ii in 1:ncol(imp_mat)){
    imp_mat[,ii]=stats::filter(DF_w_in[,imp_cols[ii]],
                               filter = rep(1/MA_window[ii], MA_window[ii]), sides = 2)
    if(fillNAs){
      imp_mat[,ii]=imputeTS::na_ma(imp_mat[,ii],weighting = "simple",k=1)
    }
  }

  data.frame(tmpDF,imp_mat)
  
}
