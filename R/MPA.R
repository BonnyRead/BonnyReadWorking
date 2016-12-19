#' @title Monthly Performance Analysis
#' @description This function allows you to report some specific indicators of
#' Bonny&Read corp. performance, like revenue,retention rate,etc.
#'
#' @param Dataroute this should be the datapath of the folder which conprise
#' all older format sales files.
#' @param Year Assign the year you want to check for MPA, the format are supposed
#' to be YYYY format
#' @param Month Assign the month you want to check for MPA, the format are supposed
#' to be a a range from 1 to 12.
#'
#' @import data.table
#' @importFrom  plyr rbind.fill
#' @importFrom  magrittr "%>%"
#' @importFrom  lubridate ymd floor_date ceiling_date
#' @importFrom  scales percent
#' @importFrom utils write.csv
#' @import rga
#' @export MPA

 MPA <- function(Dataroute,Year,Month){

  # Checking for the presence of arguments
  if (missing (Dataroute) | missing (Year) | missing (Month))
    stop("You are supposed to fill with all arguments")

  # Read data inside
  dataroute <- list.files(Dataroute, pattern = "*csv", full.names = TRUE)
  datafile <- lapply(dataroute, fread) %>% rbind.fill %>% setDT

  # Define selected date range
  Date <- ymd(paste(Year,formatC(Month,width = 2,format = "d",flag = 0),12))
  Date <- c(Date[1] - 31,Date)
  fr <- floor_date(Date,"month") ; cl <- ceiling_date(Date,"month") - 1
  datafile[,訂單日期 := as.Date(訂單日期)]

  # Aquiring data from GoogleAnalytics
  rga.open()
  gafile <- ga$getData(91337451,start.date = ymd(20140901),end.date = "today",
                       metrics = "ga:users,ga:bounceRate",dimensions = "ga:year,ga:month") %>%
    setDT
  gafile[,month := formatC(month,width = 2,flag = 0,format = "d")]
  gafile[,`:=`(TimePeriod = paste(year,month,sep = "-"),year = NULL,
               month = NULL,bounceRate = (bounceRate / 100) %>% percent)]

  # Counting Performance Analysis data
  tmpfile <- lapply(seq_along(Date),function(k) datafile[訂單日期 >= fr[k] &
                                                               訂單日期 <= cl[k]])
  PAdata <- data.table()
  PAdata <- rbind(PAdata,data.table(TimePeriod = substr(Date,1,7),
                                    ValuePerCustomer = sapply(tmpfile,function(k)
                                    { k[ !訂單狀態 == "已取消",sum(小計) / .N] } ),
                                    NoReasonReturnRate = sapply(tmpfile,function(k)
                                    { nrow(k[送貨狀態 == "已退貨"]) / k[,.N]} %>% percent),
                                    PurchaseTimes = sapply(tmpfile,function(k)
                                    { k[,.N]}),
                                    TotalRevenue = sapply(tmpfile,function(k)
                                    { k[ !送貨狀態 == "已退貨",sum(合計 - 運費)] } )
  )
  )

  # Second mutation on PAdata
  PAdata <- merge(PAdata,gafile,by = "TimePeriod")
  PAdata[,RetentionRate := ( sapply(seq_along(tmpfile),function(k)
  { ( tmpfile[[k]]$電郵 %>% unique ) %in% ( datafile[訂單日期 < fr[k] &
                                                         !訂單狀態 == "已取消",電郵] %>% unique) %>% which %>% length }) /
    PurchaseTimes) %>% percent]
  PAdata[-1,`:=`(TransactionRate = (PurchaseTimes / users) %>% percent ,
                 IncrasingRate = ( PAdata[2 : nrow(PAdata),TotalRevenue] /
                                     PAdata[1 : (nrow(PAdata) - 1),TotalRevenue ] ) %>% percent,
                 UserBoostingRate = ( PAdata[2 : nrow(PAdata),users] /
                                        PAdata[1 : (nrow(PAdata) - 1),users] ) %>% percent )]
  PAdata <- PAdata[-1]
  assign("MPAfile",PAdata,envir = globalenv())
}
