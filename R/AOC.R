#' @title Assessing Order Completion
#'
#' @description This function aims to address the issue that stocking departmnet
#' do not perceive of whether the order complete or not. This can output the
#' orderingid which has been completely ready for sent out.
#'
#' @param Salesfile the datapath of the new sales file of Bonny&Read
#' @param AssessingFile the datapath of the coworked replenishment schedule
#' @param Output Output the result to desktop as csv file
#'
#' @import data.table
#' @importFrom magrittr "%>%"
#' @importFrom  dplyr first
#' @export AOC
  AOC <- function(Salesfile,AssessingFile,Output = FALSE) {

    # Readfile in
    salesdata <- suppressWarnings(fread(Salesfile)) %>% setDT %>% copy
    arrival <- fread(AssessingFile) %>% setDT %>% copy
    arrival[,arrival := as.Date(arrival)]

    # Clean data which enclude processed transactions
    salesdata[,`:=`(訂單日期 = first(訂單日期),
                    訂單狀態 = first(訂單狀態),
                    送貨狀態 = first(送貨狀態)) , by = 訂單號碼]
    salesdata <- salesdata[!(訂單狀態 == "已取消"|訂單狀態 == "已完成")]

    # extract the transactions with preorder item
    tmpfile <- salesdata[,(grepl("預購",商品名稱)|grepl("預購",選項)) %>%
                           any , by = 訂單號碼]
    salesdata <- salesdata[訂單號碼 %in% tmpfile[V1 == TRUE,訂單號碼]]

    # Starting to judge if the preorder item have arrived
    salesdata[選項 == "",選項 := NA]
    salesdata[!(grepl("預購",商品名稱)|grepl("預購",選項)),detection := TRUE]
    arrival[,itemname := gsub("?","",itemname,fixed = TRUE)]
    arrival <- arrival[arrival < Sys.Date()]
    detectlist <- lapply(1 : nrow(arrival),function(k) {
      pewdiepie <- grepl(arrival$itemname[k],salesdata$商品名稱) &
        grepl(arrival$spec[k],salesdata$選項,ignore.case = TRUE)
    })
    for (i in 1 : length(detectlist)) {
       salesdata[detectlist[[i]],detection := TRUE]
      }
    salesdata[is.na(detection),detection := FALSE]
    tmpfile <- salesdata[,all(detection),訂單號碼]
    bookingID <- tmpfile[V1 == TRUE,訂單號碼]
    print(bookingID)
    if (Output == TRUE) { paste(file.path(Sys.getenv("USERPROFILE"),"Desktop"),
      "完成訂單號碼.csv",sep = "/") %>% write.csv(bookingID,.,row.names = FALSE)
    }
  }
