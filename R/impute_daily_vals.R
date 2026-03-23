#' Title
#'
#' @param DF_w_in daily env data 
#' @param col_inspect columns to check for NAs
#'
#' @returns imputed wide-format dataframe that was input, but with NA valeus replaced with 3-day, 2-sided moving average
#' @export
#'
impute_daily_vals <- function(DF_w_in,col_inspect=c("ORB","MID","OMT","MSD","CLC")){
  tmpDF <- DF_w_in[,-which(names(DF_w_in) %in% col_inspect)]
  DF_w_in <- DF_w_in[,col_inspect]
  whch_na_chk= which(is.na(DF_w_in),arr.ind = T)
  if(nrow(whch_na_chk)<1){return(NULL)}
  # return(which(is.na(DF_w_in),arr.ind = T))
  imp_cols <- names(DF_w_in)[unique(which(is.na(DF_w_in),arr.ind = T)[,2])]
  # return(imp_cols)
  imp_mat <- matrix(NA,nrow=nrow(DF_w_in),ncol=length(imp_cols))
  colnames(imp_mat) <- imp_cols
  for(ii in 1:ncol(imp_mat)){
    imp_mat[,ii]=imputeTS::na_ma(DF_w_in[,imp_cols[ii]],weighting = "simple",k=1)
  }
  
  # using linear imputation ('simputation' package)

  data.frame(tmpDF,imp_mat)
}
