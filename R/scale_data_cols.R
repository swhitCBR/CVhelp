# scales_select variables and stores `center`,`scale`,and `columns as attributes`
#' Title
#'
#' @description
#' Scale and center a set of variables (i.e., (x-mean(x))/sd(x))
#' 
#'
#' @param scale_cols names of columns to scale and center
#' @param repl_tab_in dataframe with colums "pattrn" and "rplace" for dealing with variables with special characters: ":", "()" etcs
#' @param x.df_SUB_in full data set
#'
#' @returns dataframe with rescaled columns
#' @export
#'
scale_data_cols <- function(
    scale_cols=c("flength","Tmsd.hor.7dadm","log.VNS.hor.5","SWP.hor.5" ,"CVP.hor.5","Qomt.hor.1net","Tclc.hor.3"),
    repl_tab_in,
    x.df_SUB_in
)
{
  
  # scale_cols_ind <- match(colnames(x.df_SUB_in),scale_cols)
  # XX_in_wattr <- scale(x.df_SUB_in[,scale_cols_ind])
  
  # scale_cols
  XX_in_wattr <- scale(x.df_SUB_in[,scale_cols])
  var_center_v <- attributes(XX_in_wattr)$`scaled:center`
  var_scale_v <- attributes(XX_in_wattr)$`scaled:scale`

  # return(XX_in_wattr)
  # print(str(XX_in_wattr))
  # print(which(!(colnames(XX_in_wattr) %in% scale_cols)))
  # print(head(x.df_SUB_in[,which(!(coll(XX_in_wattr)[2] %in% scale_cols))]))
  # print(head(x.df_SUB_in[,1:5]))
  # return(XX_in_wattr)
  df_tmp0 <- data.frame(x.df_SUB_in[,which(!(names(x.df_SUB_in) %in% scale_cols))],data.frame(XX_in_wattr))
  # print(head(df_tmp0))
  stopifnot(all(repl_tab_in$repl_tab %in% names(x.df_SUB_in)))
  
  names(df_tmp0)[match(repl_tab_in$pattrn,names(df_tmp0))] <- repl_tab_in$rplace
  
  df_tmp1 <- df_tmp0[,match(names(x.df_SUB_in),names(df_tmp0))]
  
  var_scl_info <-list(
    "scaled_vars"=scale_cols,
    "center"=var_center_v,
    "scale"=var_scale_v)
  
  attributes(df_tmp1) <- c(attributes(df_tmp1),var_scl_info)
  
  return(df_tmp1) 
}
