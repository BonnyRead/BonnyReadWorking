#' @title Replenishment Recommendation System
#'
#' @description This application is used to analyze sale force of each items
#' and establish recommendation level which base on own algorithm
#'
#' @param AssignedPath The folder path which stores salesfiles
#' @param Stockingdata The path of the latest storefiles
#' @param pagelimit you should assign the total page number of the product pages
#' of the offcial website.
#'
#' @import data.table
#' @importFrom plyr rbind.fill
#' @importFrom tidyr spread
#' @import mlr
#' @import xml2
#' @import rvest
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_replace_all
#' @importFrom stats quantile
#' @export RRS
#' @author Avalon Lin
#'

RRS <- function(AssignedPath,StockingData,pagelimit) {
  # Checking the presence of two important input file
  if ( missing (AssignedPath) | missing (StockingData))
  { stop ("You are supposed to input both AssignedPath and StockingData") }

  # Initially, you need to read in your data file
  datafile <- list.files(AssignedPath, pattern = "*csv", full.names = TRUE)
  datafile <- lapply(datafile, fread)
  datafile <- rbind.fill(datafile)
  setDT(datafile)
  storefile <- fread(StockingData)

  # We need to handle the missing value in transaction date
  datafile[, 訂單日期 := max(訂單日期), by = 訂單號碼]
  tmpmin <- min(datafile$訂單日期)
  tmpmax <- max(datafile$訂單日期)

  # Spread the data.table to accquire day by bay sales and remove useless
  # label in itemname
  datafile <- datafile[, 訂單日期 := as.Date(訂單日期)] %>%
    .[, .(sales = sum(數量)), keyby = .(商品名稱, 選項, 訂單日期)] %>%
    spread(key = "訂單日期", value = "sales", fill = "0")
  datafile %>% setnames(1 : 2, c("itemname","spec"))
  datafile[, itemname := str_replace_all
           (itemname, "(\\[.*?\\])|\\【.*?\\】", "") %>% trimws ]

  # Accquring all the update date of the items which show off in our website
  klist <- paste0("http://www.bonnyread.com.tw/products?page=", 1 : pagelimit)
  tmpdata <- lapply(klist, read_html)
  ItemURL <- lapply(tmpdata, function(k)
  {k %>% xml_nodes(xpath = "//li[@class = 'boxify-item product-item']/a") %>%
        html_attr("href") %>%
      paste0("http://www.bonnyread.com.tw/", .)}) %>% unlist
  itemname <- lapply(tmpdata, function(k)
  {k %>% html_nodes(xpath = "//div[@class='title text-primary-color']") %>%
      html_text(trim = TRUE)}) %>% unlist
  tmp2 <- lapply(ItemURL, function(k) {k %>% read_html %>%
      html_nodes(xpath = "//script[@type='text/javascript']") %>%
      html_text %>% unlist %>% trimws %>% .[substr(., 12, 18) == "product"] %>%
          strsplit(., ",", fixed = TRUE) %>% unlist %>% .[3]  })

  # Remove the URL if the Error 404 occurs
  removable <- sapply(tmp2, function(k) { is.null(k) } ) %>% which
  if ( length (removable) != 0 ) {
    itemname <- itemname[-removable]
    tmp2 <- tmp2[-removable]
  }
  CreateDate <- lapply(tmp2, function(k)
  { k %>% paste0("{", ., "}") %>% fromJSON  %>% substr(., 1, 10) } ) %>%
    unlist %>% as.Date

  # Build up the datatable and remove useless label in itemname
  Datedata <- data.table(itemname = itemname, CreateDate = CreateDate)
  Datedata[,itemname := str_replace_all(itemname,
                                      "(\\[.*?\\])|\\【.*?\\】", "") %>% trimws]

  # remove useless label in itemname of storefile and switch colnames
  storefile %>% setnames(c("商品名稱", "選項", "商店貨號", "庫存／數量",
                           "售出數量（已確認）"),
                         c("itemname", "spec", "itemid", "remain",
                           "ConfirmedSales"))
  storefile[,itemname := str_replace_all(itemname,
                                      "(\\[.*?\\])|\\【.*?\\】", "") %>% trimws]

  # merge Datedata and salesdata,and merge prior datafile with storefile
  completefile <- merge(Datedata, datafile, by = "itemname") %>%
    merge(storefile, ., by = c("itemname", "spec"))
  mainfile <- completefile[, c(1, 2, 3, 5, 6, 10), with = FALSE]

  # Retrieve the daliy sales matrix and attain the feature from it
  confirmvector <- completefile$CreateDate < tmpmin
  countmatrix <- completefile[, c(-1 : -10), with = FALSE] %>% as.matrix %>%
    apply(., 2, as.numeric)

  # Making classification of colors and categories
  mainfile[grepl(c("戒指", "對戒", "戒組", "尾戒", "關節戒", "連指戒",
                   "情侶戒", "三件戒", "開口戒") %>% paste(collapse = "|"),
                 mainfile$itemname), category := "戒指"]
  mainfile[grepl(c("耳環", "耳針", "耳扣", "耳夾", "耳骨環") %>%
                   paste(collapse = "|"), mainfile$itemname),
           category := "耳環"]
  mainfile[grepl(c("項鍊", "鎖骨鍊", "頸鍊", "頸圈") %>%
                   paste(collapse = "|"), mainfile$itemname),
           category := "項鍊"]
  mainfile[grepl(c("手鍊", "手環", "手鐲") %>% paste(collapse = "|"),
                 mainfile$itemname), category := "手鍊"]
  mainfile[grepl(c("髮飾", "髮帶", "髮圈", "髮夾", "髮箍") %>%
                   paste(collapse = "|"), mainfile$itemname),
           category := "髮飾"]
  mainfile[grepl("手錶", mainfile$itemname), category := "手錶"]
  mainfile[grepl("刺青貼紙", mainfile$itemname),
           category := "刺青貼紙"]
  mainfile[grepl("墨鏡", mainfile$itemname), category := "墨鏡"]
  mainfile[grepl("腳鍊", mainfile$itemname), category := "腳鍊"]
  mainfile[grepl("眼鏡", mainfile$itemname), category := "眼鏡"]
  mainfile[is.na(mainfile$category), category := "其它"]

  mainfile[grepl("Gold", mainfile$spec), color := "Gold"]
  mainfile[grepl("Black", mainfile$spec), color := "Black"]
  mainfile[grepl("Pink", mainfile$spec), color := "Pink"]
  mainfile[grepl("Yellow", mainfile$spec), color := "Yellow"]
  mainfile[grepl("Blue", mainfile$spec), color := "Blue"]
  mainfile[grepl("Red", mainfile$spec), color := "Red"]
  mainfile[grepl("White", mainfile$spec), color := "White"]
  mainfile[grepl("Brown", mainfile$spec), color := "Brown"]
  mainfile[grepl("Purple", mainfile$spec), color := "Purple"]
  mainfile[grepl("Orange", mainfile$spec), color := "Orange"]
  mainfile[grepl("Rose Gold", mainfile$spec), color := "Rose Gold"]
  mainfile[grepl("Grey", mainfile$spec), color := "Grey"]
  mainfile[grepl("Green", mainfile$spec), color := "Green"]
  mainfile[grepl("Silver", mainfile$spec), color := "Silver"]
  mainfile[is.na(mainfile$color), color := "No Show or rare color"]
  mainfile$category <- as.factor(mainfile$category)
  mainfile$color <- as.factor(mainfile$color)

  # Create test file
  predata <- data.table(
    recent3 = apply(countmatrix, 1, function(k)
    {k[(NCOL(countmatrix) - 4) : (NCOL(countmatrix) - 1) ] %>% sum }),
    recent7 = apply(countmatrix, 1, function(k)
    {k[(NCOL(countmatrix) - 8) : (NCOL(countmatrix) - 1) ] %>% sum}),
    color = mainfile$color,
    category = mainfile$category
  )

  # Making loop to generate datatable for machine learning
  # This one is for createdate of products which is earlier than the min date

  # Due to the fact that the last day was incomplete so that I decided to
  # delete it from the test data.
  MLdata <- lapply( 1 : (NCOL(countmatrix) - 14 ), function(k) {
    data.table(
      color = mainfile$color[confirmvector],
      category = mainfile$category[confirmvector],
      recent3 = apply(countmatrix[confirmvector, (k + 4) : (k + 6)], 1, sum ),
      recent7 = apply(countmatrix[confirmvector, k : (k + 6)], 1, sum ),
      target = apply(countmatrix[confirmvector, (k + 7) : (k + 13)], 1, sum )
    )
  }) %>% rbindlist

  # This one for createdate of products which locate between min and max date
  exclude <- completefile[CreateDate < ( as.Date(tmpmax) - 14)]
  colorselector <- mainfile[CreateDate < ( as.Date(tmpmax) - 14)]
  confirmvector <- exclude$CreateDate < tmpmin
  countmatrix <- exclude[, c(-1 : -10), with = FALSE] %>% as.matrix %>%
    apply(., 2, as.numeric)
  countvector <- lapply(exclude$CreateDate, function(k)
    grep(k, colnames(completefile)) - 10 ) %>% unlist
  TFvector <- lapply(exclude$CreateDate, function(k)
    grepl(k, colnames(completefile)) %>% any ) %>% unlist
  countmatrix <- countmatrix[TFvector, ]

  # First round loop
  MLdata <- lapply(seq_along(countvector), function(k) {
    lapply(countvector[k] : (NCOL(countmatrix) - 14), function(m) {
      data.table(color = colorselector[TFvector]$color[k],
                 category = colorselector[TFvector]$category[k],
                 recent3 = countmatrix[k, (m + 4) : (m + 6)] %>% sum,
                 recent7 = countmatrix[k, m : (m + 6)] %>% sum,
                 target = countmatrix[k, (m + 7) : (m + 13)] %>% sum)
    }) %>% rbindlist
  }) %>% rbindlist %>% rbind(MLdata, .)

  # Making machine learning preparation
  tsk <- makeRegrTask("predictsales", data = MLdata, target = "target")
  mod <- train("regr.brnn", tsk)
  mainfile$PredictSales <- predictLearner(makeLearner("regr.brnn"),
                                          mod, predata)

  # analyze replenish or not
  mainfile[, replenish := ifelse((PredictSales * 2) > remain,
                                 "RunOut", "Stayed")]
  mainfile[, avgsales := ConfirmedSales / as.numeric(Sys.Date() - CreateDate)]
  foursector <- quantile(mainfile$avgsales,c(0.2, 0.4, 0.6, 0.8))
  mainfile[, `:=`(recommendation = findInterval(avgsales, foursector),
                  avgsales = NULL)]
  mainfile[, recommendation := factor(recommendation, c(0, 1, 2, 3, 4),
    c("Highly not recommend", "Not recommend", "Fair", "Recommend",
      "Highly recommend"))]
  assign("ReplenishAnalysis", mainfile, envir = globalenv())
}
