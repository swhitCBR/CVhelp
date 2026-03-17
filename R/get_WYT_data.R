#' get_WYT_data 
#'
#' @param url cdec web address to look-up. Based on 'Official Year Classifications based on 
#' May 1 Runoff Forecasts' section from https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST
#' 
#' @param basin basin name, either "SAC" for Sacramento of "SJ" for San Joaquin
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
get_WYT_data <- function(
    url="https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST",
    basin="SJ",
    dt_rng=c("2011-01-01","2016-12-31")
){
  
  yr_rng <- sapply(dt_rng,function(x) as.numeric(strsplit(x,split = "-")[[1]][1]))
  stopifnot(yr_rng[2]>yr_rng[1])
  # require(rvest)
  
  html <- rvest::read_html(url)
  html
  
  characters <- html |> rvest::html_elements("pre") |> rvest::html_text2()#|> html_test()#|> html_attr("text")
  tmp1 <- strsplit(characters,split = "\n")[[1]]
  # return(tmp1)
  
  # Reconstructed Water Year Type data (based specifically on JSON output at the time)
  indfrst <- which(tmp1=="1905                                            1.82    3.36    5.30   3.24    AN  \r")
  cand_end <- which(sapply(tmp1,substr,start=1,stop=4)=="2024")
  true_end <- min(cand_end[cand_end>indfrst])
  recon_tmp2 <- tmp1[
    seq(from=indfrst+1,
        to=true_end,
        by=1)]
  
  # recon_tmp2
  # return(recon_tmp2)
  
  tmp2 <- tmp1[
    seq(from=which(tmp1=="Official Year Classifications based on May 1 Runoff Forecasts\r"),
        to=which(tmp1=="Abbreviations:\r")-2,
        by=1)]
  
  gsub(x=tmp2[-c(1:3)],pattern = " ",replacement = "")
  tmp3 <- strsplit(x=tmp2[-c(1:3)],split=" ")
  tmp4 <- unlist(tmp3)[unlist(tmp3)!=""]
  tmp5 <- gsub(x=tmp4,pattern="\r",replacement = "")
  df1 <- data.frame(matrix(tmp5,ncol=5,byrow=T,
                           dimnames = list(
                             NULL,
                             c("WY","SAC_ind","SAC_wyt","SJ_ind","SJ_wyt"))))
  
  
  WYT_nm_v <- c(
    "Critical" = "C",
    "Dry" = "D",
    "Below Normal" = "BN",
    "Above Normal" = "AN",
    "Wet" = "W")
  
  df_sj <- df1[,c("WY","SJ_ind","SJ_wyt")]
  
  df_sac <- df1[,c("WY","SAC_ind","SAC_wyt")]
  # 
  df_sj$WYT <- names(WYT_nm_v)[match(df_sj$SJ_wyt,WYT_nm_v)]
  df_sac$WYT <- names(WYT_nm_v)[match(df_sac$SAC_wyt,WYT_nm_v)]
  # 
  if(basin=="SJ"){ 
    names(df_sj)[c(2,3)] <- c("ind", "wyt")
    df_final <- df_sj}
  
  if(basin=="SAC"){ 
    names(df_sac)[c(2,3)] <- c("ind", "wyt")
    df_final <- df_sac}
  
  if(basin=="both"){
    names(df_sac)[c(2,3)] <- c("ind", "wyt")
    names(df_sj)[c(2,3)] <- c("ind", "wyt")
    df_final <- rbind(data.frame(basin="SAC",df_sac),
                      data.frame(basin="SJ",df_sj))}
  
  names(df_final)[1] <- "Year"
  
  df_final <- subset(df_final,Year %in% (yr_rng[1]:yr_rng[2]))
  df_final$Year <- as.numeric(df_final$Year)
  
  return(df_final)
}