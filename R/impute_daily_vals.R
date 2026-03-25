#' Title
#'
#' @param DF_w_in daily env data 
#' @param col_inspect columns to check for NAs
#'
#' @returns imputed wide-format dataframe that was input, but with NA valeus replaced with 3-day, 2-sided moving average
#' @export
#'
impute_daily_vals <- function(DF_w_in,col_inspect=c("ORB","MID","OMT","MSD","CLC")){
  nm_ord <- names(DF_w_in)
  # print(head(DF_w_in))
  tmpDF <- DF_w_in[,-which(names(DF_w_in) %in% col_inspect)]
  # print(head(tmpDF))
  DF_w_in <- DF_w_in[,col_inspect]
  # print(head(DF_w_in))
  whch_na_chk= which(is.na(DF_w_in),arr.ind = T)
  if(nrow(whch_na_chk)<1){return(NULL)}
  # return(which(is.na(DF_w_in),arr.ind = T))
  imp_cols <- names(DF_w_in)[unique(which(is.na(DF_w_in),arr.ind = T)[,2])]
  message("imputed values for: ",paste(imp_cols,collapse=", "))
  imp_mat <- matrix(NA,nrow=nrow(DF_w_in),ncol=length(imp_cols))
  colnames(imp_mat) <- imp_cols
  for(ii in 1:ncol(imp_mat)){
    imp_mat[,ii]=imputeTS::na_ma(DF_w_in[,imp_cols[ii]],weighting = "simple",k=1)
  }
  
  if(any(!col_inspect %in% imp_cols)){
    non_impDF <- DF_w_in %>% dplyr::select(col_inspect[!col_inspect %in% imp_cols])
  } else{  non_impDF <- NULL}
  
  # non_impDF <- NULL
  # print(head(non_impDF))
  # using linear imputation ('simputation' package)
  DF_w_out <- dplyr::bind_cols(tmpDF,non_impDF,imp_mat)
  DF_w_out <- DF_w_out[,match(nm_ord,names(DF_w_out))]
  
  return(DF_w_out)
}
