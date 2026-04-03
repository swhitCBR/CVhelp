#' Title
#'
#' @param unscaled Logical, use unscaled data 
#'
#' @returns named list with vital 
#' 
#' @export
#'
HOR_CHP_comp <- function(unscaled=FALSE){
  load("data/HOR_CHP_mod_dat_ls.RData")
  
  x.df_rem1 <- subset(x.df,#tag!="1232531" &
                      !is.na(det.pred.qomt.rms))
  
  # for matching up inputs
  tmpDF <- x.df_rem1 %>% 
    select(tag,year,rel.grp,HOR.det, ,flength,barrier.fac) %>% 
    mutate(rel_grp=paste(year,rel.grp),
           model="survival",
           foc_area="HOR-CHP")
  
  WYT_ref_tab <- table(paste(x.df_rem1$year,x.df_rem1$rel.grp),x.df_rem1$water.yr)
  
  BASE_COLS <- c("form","rel.grp","year","water.yr","route.fac","barrier")
  HOR_CHP_VARS  <- c(
    "flength",         # Fork length
    "log.QOUT",        # OUTFLOW
    "Tmsd.hor.7dadm",  # MSD temp
    "log.VNS.hor.5",   # Vernalis flow
    "SWP.hor.5",       # Exports
    "CVP.hor.5",       # Exports
    "Qomt.hor.1net",   # Flow
    "Tclc.hor.3",
    "VNS.rel",
    "VNS.hor"
  )
  
  Xc.3v8 <- cbind(Xc.3v8,"year.fac2011"=(apply(Xc.3v8[,c("year.fac2012","year.fac2013","year.fac2014","year.fac2015","year.fac2016")],1,sum)==0)+0)
  x.df_SUB <- cbind(Xc.3v8[,c("(Intercept)","year.fac2011",
                              "year.fac2012","year.fac2013","year.fac2014","year.fac2015","year.fac2016",
                              "route.facB","barrier.facTRUE","route.facB:barrier.facTRUE"#,
                              # "log(VNS.hor.5)"
  )],
  x.df_rem1[,c(BASE_COLS,HOR_CHP_VARS)])
  # excluded row
  rw_ind <- which(x.df$tag!="1232531")
  XX_in <- x.df_SUB
  
  
  repl_tab <- data.frame(
    pattrn=c("X.Intercept.","route.facB.barrier.facTRUE"),
    rplace=c("(Intercept)","route.facB:barrier.facTRUE"))
  
  
  
  XX_in_alt <- CVhelp::scale_data_cols(
    repl_tab_in=repl_tab,
    x.df_SUB_in=x.df_SUB)
  
  XX_in <- XX_in_alt
  # XX_in <- x.df_SUB # uncomment and run here to get unscaled estimates
  
  if(unscaled){
    XX_in <- XX_in_alt
  }
    
  # vector of  4 linear combinations used in only for loop in CPP template
  P_lc_n <- length(c("P1_f1","Lam_f1",
                     "P11_f2f3","P12_f2f3"))
  # row_inds for different likelihood forms
  f1 <- which(XX_in[,"form"]==1)
  f2 <- which(XX_in[,"form"]==2)
  f3 <- which(XX_in[,"form"]==3)
  
  BASIC_IND_COLS <- c(1:9)
  MAIN_EFFECT_IDS <- match(c("flength","Tmsd.hor.7dadm","log.VNS.hor.5","SWP.hor.5","CVP.hor.5","Qomt.hor.1net"),names(XX_in))
  
  # only in the intercept
  # INT_ONLY_IDS <- match(c("Tclc.hor.3"),names(XX_in))
  
  S_col_add_ids <- c(BASIC_IND_COLS,MAIN_EFFECT_IDS)
  Bar_col_id <- 9 # column with BAR
  Rte_col_id <- 8 # column with ROUTE
  
  # additive design matrix
  XX_in_add <- XX_in[,S_col_add_ids]
  
  # adding one-way interactions design matrix
  XX_in_w_int <- cbind(XX_in_add,
                       data.frame(
                         # Barrier interactions
                         B_x_Temp=XX_in_add[,Bar_col_id]*XX_in_add[,c("Tmsd.hor.7dadm")],
                         B_x_VNS=XX_in_add[,Bar_col_id]*XX_in_add[,c("log.VNS.hor.5")],
                         B_x_SWP=XX_in_add[,Bar_col_id]*XX_in_add[,c("SWP.hor.5")],
                         B_x_CVP=XX_in_add[,Bar_col_id]*XX_in_add[,c("CVP.hor.5")],
                         B_x_OMT=XX_in_add[,Bar_col_id]*XX_in_add[,c("Qomt.hor.1net")],
                         # Route interactions
                         R_x_Temp=XX_in[,Rte_col_id]*XX_in[,c("Tclc.hor.3")], # does not take column from main effect
                         R_x_VNS=XX_in_add[,Rte_col_id]*XX_in_add[,c("log.VNS.hor.5")],
                         R_x_SWP=XX_in_add[,Rte_col_id]*XX_in_add[,c("SWP.hor.5")],
                         R_x_CVP=XX_in_add[,Rte_col_id]*XX_in_add[,c("CVP.hor.5")],
                         R_x_OMT=XX_in_add[,Rte_col_id]*XX_in_add[,c("Qomt.hor.1net")]
                       ))
  
  # columns used to model survival (intercept and outflow)
  P_col_ids <- c(1,20)
  
  rel_grps_df_tmp <- data.frame(
    rel_grps=unique(paste(XX_in$year,XX_in$rel.grp)),
    t(sapply(unique(paste(XX_in$year,XX_in$rel.grp)),function(x) strsplit(x,split = " ")[[1]])))
  rel_grps_df_tmp[,2]=as.numeric(rel_grps_df_tmp[,2])
  names(rel_grps_df_tmp)[c(2,3)]=c("year","rel_nm")
  
  rel_grps_df_tmp <- rel_grps_df_tmp[order(rel_grps_df_tmp$year,rel_grps_df_tmp$rel_nm),]
  rel_grps_df_tmp$relgrpF <- factor(rel_grps_df_tmp$rel_grps,levels = rel_grps_df_tmp$rel_grps)
  
  tmp_tab <- which(WYT_ref_tab!=0,arr.ind = T)
  rel_grps_df <- data.frame(rel_grps_df_tmp,
                            WYT=c("wet","dry","drought")[tmp_tab[tmp_tab[,"row"],"col"]],
                            R_ind=as.numeric(rel_grps_df_tmp$relgrpF),
                            TMB_ind=as.numeric(rel_grps_df_tmp$relgrpF)-1)
  
  relgrps_ind_IN <- rel_grps_df$TMB_ind[match(paste(XX_in$year,XX_in$rel.grp),rel_grps_df$rel_grps)]
  
  WYT_wet=(relgrps_ind_IN %in% rel_grps_df$TMB_ind[rel_grps_df$WYT=="wet"])+0
  WYT_drought=(relgrps_ind_IN %in% rel_grps_df$TMB_ind[rel_grps_df$WYT=="drought"])+0
  
  XX_in_w_int_noYRS <- XX_in_w_int[-c(2:7)]
  XX_in_w_int_WYT <- cbind(XX_in_w_int_noYRS[,"(Intercept)"],
                           WYT_wet,WYT_drought,
                           XX_in_w_int_noYRS[,which(names(XX_in_w_int_noYRS)!="(Intercept)")])
  names(XX_in_w_int_WYT)[1] <- "(Intercept)"
  
  
  # XX_in
  
  
  TMB_data_baseline <- list(
    "n_relgrps"=19,
    "relgrps_ind"=relgrps_ind_IN,
    "XX"=as.matrix(XX_in[,P_col_ids]),
    "XX_s"=as.matrix(XX_in_w_int_WYT),
    "Dat"=sdat.det.common2,
    "col_Dat1"=match(paste(c("CHP.BBR"),c("11","10","01","00"),sep="."),colnames(sdat.det)) - 1,
    "col_Dat2"=match(paste(c("CHP.2x"),c("11","10","01","00"),sep="."),colnames(sdat.det)) - 1,
    "col_Dat3"=match(paste(c("CHP.3x"),c("111","110","101","100","011","010","001","000"),sep="."),colnames(sdat.det))-1,
    "ind_all"=1:nrow(sdat.det.common2)-1,
    "f1_ind"=f1-1,
    "f2_ind"=f2-1,
    "f3_ind"=f3-1,
    "lvec1"=lvec1.common2,
    "lvec2"=lvec2.common2,
    "col_S"=(1:ncol(XX_in_w_int_WYT)),
    "col_P11"=(1:2)-1,
    "col_P12"=(1:2)-1,
    "y1to7"=(1:7)-1,
    "y1to4"=(1:4)-1,
    "y1256"=c(1,2,5,6)-1,
    "y1357"=c(1,3,5,7)-1,
    "y5to7"=(5:7)-1,
    "y347"=c(3,4,7)-1,
    "y246"=c(2,4,6)-1,
    "y123"=(1:3)-1,
    "y12"=(1:2)-1,
    "y13"=c(1,3)-1,
    "P_lc_n"=P_lc_n)
  
  # initial values of detection probability
  P_mat_base <- matrix(0,nrow=length(TMB_data_baseline$col_P11),
                       ncol=TMB_data_baseline$P_lc_n)
  
  S_pars_input <- rep(0,ncol(TMB_data_baseline$XX_s))
  
  # specifying combinations of parameters to add or remove
  allint_DMs <- apply(cbind(rep(c("11111111111"),1024),
                            expand.grid(
                              c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1)))
                      ,1,paste0,collapse="")
  
  # extracting column ids based on vectors that represent design matrices
  get_DM_col_inds(allint_DMs[1])
  get_DM_col_inds(allint_DMs[1024])
  
  out_ls=list("allint_DMs"=allint_DMs,
              "TMB_data_baseline"=TMB_data_baseline,
              "XX_in"=XX_in_alt,
              "XX_in_w_int_WYT"=XX_in_w_int_WYT)
  
  return(out_ls)
}

