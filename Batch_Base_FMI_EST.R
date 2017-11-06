# set parameters to extend java's memory usage
# to keep xlsx scripts from crashing
options(java.parameters = "-Xmx8000m")

# load packages
require(xlsx)
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)
library(caret)
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

# set the working directory
# setwd("C:/Users/502689880/Desktop/FMI_Estimates")
setwd("C:/Users/502689880/Desktop/FMI_Estimates")

# create function for duration analysis
# variable name = "mli"
duration.analysis <- function(mli,...){
  
  # initiate try/catch
  # keeps program running after an "insufficient data" error
  try({
    
    # create durations data frame
    requirements <- read.csv("requirements.csv", header = TRUE, 
                             stringsAsFactors = FALSE)
    durations <- read.csv("durations.csv", header = TRUE, 
                          stringsAsFactors = FALSE)
    durations <- merge(requirements, durations, by="id_project")
    
    # format date columns
    durations$date_actStart <- as.Date(durations$date_actStart, "%m/%d/%Y")
    durations$date_actFinish <- as.Date(durations$date_actFinish, 
                                        "%m/%d/%Y")
    
    # remove duplicate records
    durations <- unique(durations[,1:35])
    
    # create task data frame
    req <- read.csv("req.csv", header = TRUE, stringsAsFactors = FALSE)
    frame <- read.csv("frame.csv", header = TRUE, stringsAsFactors = FALSE)
    task <- read.csv("task.csv", header = TRUE, stringsAsFactors = FALSE)
    
    # create filter by "mli" variable
    task <- task[which(task$id_task == mli),]

    #create spec data and remove duplicate records
    spec <- merge(req, frame, by="num")
    spec <- unique(spec[,1:5])
    spec <- merge(spec, task, by="num")
    
    # Convert character variables to factors
    durations$id_frame <- as.factor(durations$id_frame)
    durations$id_req <- as.factor(durations$id_req)
    durations$id_task <- as.factor(durations$id_task)
    durations$desc_frameFam <- as.factor(durations$desc_frameFam)
    
    # Create new columns for numeric representation
    durations["num_idFrame"] <- NA
    durations["num_idReq"] <- NA
    durations["num_idTask"] <- NA
    durations["num_descFrameFam"] <- NA
    
    # Convert factor variables to numeric
    durations$num_idFrame <- as.numeric(durations$id_frame)
    durations$num_idReq <- as.numeric(durations$id_req)
    durations$num_idTask <- as.numeric(durations$id_task)
    durations$num_descFrameFam <- as.numeric(durations$desc_frameFam)
    
    durations <- durations[which(durations$id_task == mli),]

    ##Create function to remove outliers
    remove_outliers <- function(x, na.rm = TRUE, ...) {
      qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
      H <- 1.5 * IQR(x, na.rm = na.rm)
      y <- x
      y[x < (qnt[1] - H)] <- NA
      y[x > (qnt[2] + H)] <- NA
      y
    }
    
    ##Remove outliers
    durations$num_actDur <- remove_outliers(durations$num_actDur)
    durations <- na.omit(durations)
    
    # Create Estimate Tables Est=T/R/F.v, Est2=T/R/F, Est3=T/R, Est4=T/F.v, 
    # Est5=T/F, Est6=T
    durations.sub <- subset(durations, select = c("id_task", "num_idTask"))
    durations.sub <- unique(durations.sub[,1:2])
    
    est <- merge(spec, durations.sub, by=c("id_task"))
    est <- unique(est[,1:7])

    # Fit Models
    durations.lm <- lm(num_actDur ~ num_idTask, 
                       data = durations)
    est["num_estimate"] <- NA
    est$num_estimate <- predict(durations.lm, newdata = est)
    durations <- subset(durations, select = c("id_project", "id_task", "num_estDur", 
                                      "num_actDur", "date_actStart", "date_actFinish"))
    durations <- unique(durations[,1:6])
    est <- subset(est, select = c("id_task", "num_estimate"))
    est <- unique(est[,1:2])
    
    error <- merge(est, durations, by=c("id_task"))
    error <- unique(error[,1:7])
    error["num_logError"] <- NA
    error$num_logError <- log(error$num_estimate)-log(error$num_actDur)
    ## -- Multivariate lm() "mlm" -----------
    utils::example("SSD", echo=FALSE)
    error["num_resSD"] <- NA
    error$num_resSD <- sigma(durations.lm) # is the same as {but more efficient than}
    error["num_oneSigma"] <- NA
    error$num_oneSigma <- error$num_estimate+error$num_resSD
    
    data <- subset(error, select = c("id_task", "num_estimate"))
    data <- unique(data[,1:2])
    
    file <- "Duration_Estimate_Template.xlsx"
    wb <- loadWorkbook(file)
    sheets <- getSheets(wb)
    sheet <- sheets[[6]]  # or another
    addDataFrame(data, sheet, col.names = FALSE, row.names = FALSE,
                 startRow = 2, startColumn = 1)
    saveWorkbook(wb, paste0(mli, "_Duration_Estimate.xlsx"))
  })
}

#####
n <- c("0010", "0017", "0024", "0026", "0069", "0082", "0086", 
       "00TM", "0100DA", "0100PM", "0100SSF", "0103", "0104", "0106", 
       "0108", "0124", "0124FLE", "0132", "0204", "0215", "0218", 
       "0219", "0225", "0227", "0234", "0235", "0244", "0301", 
       "0302", "0306", "0313", "0314", "0323", "0326", "0330", 
       "0331", "0401", "0404", "0405", "0406", "0407", "0408", 
       "0409", "0410", "0411", "0413", "0414", "0415", "0416", 
       "0417", "0418", "0420", "0421", "0422", "0423", "0424", 
       "0425", "0426", "0427", "0431", "0432", "0434", "0436", 
       "0442", "0444", "0445", "0453", "0461", "0462", "0463", 
       "0470", "0471", "0474", "0477", "0479", "0480", "0481", 
       "0482", "0484", "0485", "0492", "0497", "0501", "0503", 
       "0507", "0508", "0509", "0510", "0511", "0512", "0513", 
       "0514", "0515", "0516", "0520", "0525", "0528", "0531", 
       "0533", "0538", "0543", "0544", "0546", "0548", "0557", 
       "0559", "0564", "0566", "0570", "0572", "0574", "0575", 
       "0581", "0583", "0585", "0601", "0602", "0603", "0604", 
       "0605", "0606", "0607", "0608", "0611", "0612", "0613", 
       "0623", "0628", "0637", "0638", "0639", "0641", "0645", 
       "0701", "0702", "0703", "0705", "0706", "0710", "0712", 
       "0715", "0717", "0719", "0722", "0726", "0796", "0797", 
       "0798", "0799", "0801", "0802", "0804", "0805", "0811", 
       "0812", "0813", "0815", "0901", "0903", "0904", "0905", 
       "0906", "0907", "0908", "0909", "0910", "0911", "0914", 
       "0915", "0917", "0918", "0920", "0922", "0923", "0924", 
       "0926", "0929", "0931", "0932", "0940", "0941", "0953", 
       "0956", "0961", "0962", "0963", "0964", "0965", "0968", 
       "0969", "0972", "0973", "0974", "0976", "0979", "0980", 
       "0982", "0983", "0984", "0987", "0990", "0991", "0992", 
       "0994", "0996", "1003", "1005", "1006", "1007", "1008", 
       "1009", "1013", "1014", "1015", "1019", "1022", "1026", 
       "1027", "1028", "1035", "1044", "1047", "1069", "1070", 
       "1071", "1075", "1082", "1087", "1090", "1097", "10A2", 
       "1101", "1102", "1103", "1104", "1105", "1106", "1112", 
       "1118", "1121", "1127", "1131", "1140", "1149", "1153", 
       "1154", "1159", "1160", "1207", "1208", "1213", "1214", 
       "1215", "1223", "1226", "1229", "1230", "1233", "1239", 
       "124A", "1261", "1290", "1301", "1302", "1303", "1305", 
       "1307", "1308", "1309", "1310", "1311", "1313", "1314", 
       "1315", "1317", "1318", "1319", "1320", "1322", "1331", 
       "1332", "1333", "1334", "1401", "1402", "1403", "1409", 
       "1410", "1501", "1502", "1503", "1504", "1505", "1507", 
       "1510", "1603", "1604", "1605", "1609", "1610", "1612", 
       "1614", "1616", "1617", "1619", "1620", "1623", "1625", 
       "1634", "1636", "1643", "1645", "1658", "1659", "1716", 
       "1745", "1804", "1818", "1843", "1912", "1929", "1951", 
       "2102", "218A", "2310", "235A", "235B", "235C", "323A", 
       "323B", "4010", "4036", "4038", "4063", "4069", "4073", 
       "40SS", "4108", "557T", "609A", "706A", "706B", "9010", 
       "9018", "9019", "9031", "924A", "924B", "924D", "969A", 
       "969D", "969E", "969G", "969L", "969M", "969P", "969S", 
       "969Y", "977A", "A004", "A005", "A006", "A007", "A008", 
       "A010", "A010ABE", "A010ADE", "A010CBM", "A010PC", "A012", "A013", 
       "A014", "A015", "A016", "A018", "A019", "A020", "A022", 
       "A025", "A033", "A035", "A036", "A037", "A040", "A041", 
       "A042", "A044", "A045", "A048", "A04T", "A053", "A054", 
       "A058", "A059", "A060", "A062", "A064", "A065", "A066", 
       "A068", "A072", "A075", "A076", "A098", "A102", "A105", 
       "A108", "A122", "A124", "A125", "A138", "A139", "A140", 
       "A141", "A144", "A147", "A148", "A152", "A155", "A160", 
       "A162", "A163", "A168", "A179", "A204", "A209", "A210", 
       "A225", "A241", "A241LM", "A241PM", "A245", "A246", "A268", 
       "A279", "A286", "A341", "C002", "C050", "C086", "C093", 
       "C118", "D005", "D023", "E001", "E007", "E008", "E025", 
       "E037", "E039", "E041", "EG26", "F011", "F056", "G002", 
       "G005", "G012", "G014", "Q720", "UA10", "UA90", "UB10")


lapply(n, duration.analysis)

################################################################

options(java.parameters = "-Xmx8000m")
require(xlsx)
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)
library(caret)
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

# set the working directory
# setwd("C:/Users/502689880/Desktop/FMI_Estimates")
setwd("C:/Users/502689880/Desktop/FMI_Estimates")

# create function for effort hours analysis
# variable name = "mli"
hours.analysis <- function(mli,...){
  # initiate try/catch
  # keeps program running after an "insufficient data" error
  try({
    # create hours data frame
    requirements <- read.csv("requirements.csv", header = TRUE, 
                                stringsAsFactors = FALSE, na.strings = c(""," ","NA"))
  hours <- read.csv("hours.csv", header = TRUE, 
                    stringsAsFactors = FALSE, na.strings = c(""," ","NA"))
  hourstotal <- aggregate(num_hours ~ id_project + id_task, hours, sum)
  colnames(hourstotal)[3] <- "num_totalHours"
  
  hours <- hours[which(hours$id_task == mli),]

  hours <- merge(hours, hourstotal, by=c("id_project", "id_task"))
  hours <- merge(requirements, hours, by="id_project")
  
  hours <- unique(hours[,1:54])
  
  tasks <- read.csv("tasks2.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c(""," ","NA","#N/A"))
  
  tasks <- subset(tasks, select = c("id_task", "id_baseTask", 
                                    "id_subtask", "desc_baseTask", "desc_task"))
  tasks <- unique(tasks[,1:5])
  tasks <- tasks[which(tasks$id_task == mli),]

  req <- read.csv("req.csv", header = TRUE, 
                  stringsAsFactors = FALSE)
  frame <- read.csv("frame.csv", header = TRUE, 
                    stringsAsFactors = FALSE)
  task <- read.csv("task.csv", header = TRUE, 
                   stringsAsFactors = FALSE)
  task <- merge(task, tasks, by="id_task")
  task <- task[which(task$id_task == mli),]

  task <- subset(task, select = c("num", "id_task", "id_baseTask"))
  task <- unique(task[,1:3])
  spec <- merge(req, frame, by="num")
  spec <- unique(spec[,1:5])
  spec <- merge(spec, task, by="num")
  spec <- unique(spec[,1:7])
  
  # Convert character variables to factors
  hours$id_frame <- as.factor(hours$id_frame)
  hours$id_req <- as.factor(hours$id_req)
  hours$id_task <- as.factor(hours$id_task)
  hours$desc_frameFam <- as.factor(hours$desc_frameFam)
  
  # Create new columns for numeric representation
  hours["num_idFrame"] <- NA
  hours["num_idReq"] <- NA
  hours["num_idTask"] <- NA
  hours["num_descFrameFam"] <- NA
  
  # Convert factor variables to numeric
  hours$num_idFrame <- as.numeric(hours$id_frame)
  hours$num_idReq <- as.numeric(hours$id_req)
  hours$num_idTask <- as.numeric(hours$id_task)
  hours$num_descFrameFam <- as.numeric(hours$desc_frameFam)
  
  hours <- subset(hours, select = c("id_project", "id_req", "id_frame", 
                                    "id_task", "desc_frameFam", 
                                    "num_idReq", "num_idFrame", "num_idTask", 
                                    "num_descFrameFam", "num_totalHours"))
  hours <- unique(hours[,1:10])
  
  
  ##Create function to remove outliers
  remove_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
  }
  ##Remove outliers
  hours$num_totalHours <- remove_outliers(hours$num_totalHours)
  hours <- na.omit(hours)
  
  # Create Estimate Tables Est=T/R/F.v, Est2=T/R/F, Est3=T/R, Est4=T/F.v, 
  # Est5=T/F, Est6=T
  hours.sub <- subset(hours, select = c("id_task", "num_idTask"))
  hours.sub <- unique(hours.sub[,1:2])
  
  est <- merge(spec, hours.sub, by=c("id_task"))
  est <- unique(est[,1:8])
  
  # Fit Models
  hours.lm <- lm(num_totalHours ~ num_idTask, data = hours)
  
  est["num_estimate"] <- NA
  est$num_estimate <- predict(hours.lm, newdata = est)
  hours <- subset(hours, select = c("id_project", "id_task", "num_totalHours"))
  hours <- unique(hours[,1:3])
  est <- subset(est, select = c("id_task", "num_estimate"))
  est <- unique(est[,1:2])
  
  error <- merge(est, hours, by=c("id_task"))
  error <- subset(error, select = c("id_project", "id_task", "num_estimate", 
                                    "num_totalHours"))
  error <- unique(error[,1:4])
  error["num_logError"] <- NA
  error$num_logError <- log(error$num_estimate)-log(error$num_totalHours)
  ## -- Multivariate lm() "mlm" -----------
  utils::example("SSD", echo=FALSE)
  error["num_resSD"] <- NA
  error$num_resSD <- sigma(hours.lm) # is the same as {but more efficient than}
  error["num_oneSigma"] <- NA
  error$num_oneSigma <- error$num_estimate+error$num_resSD
  
  data <- subset(error, select = c("id_task", "num_estimate", "num_resSD"))
  data <- unique(data[,1:3])
  
  file <- "Hours_Estimate_Template.xlsx"
  wb <- loadWorkbook(file)
  sheets <- getSheets(wb)
  sheet <- sheets[[6]]  # or another
  addDataFrame(data, sheet, col.names = FALSE, row.names = FALSE,
               startRow = 2, startColumn = 1)
  saveWorkbook(wb, paste0(mli, "_Hours_Estimate.xlsx"))
  })
}

n <- c("0010", "0017", "0024", "0026", "0069", "0082", "0086", 
       "00TM", "0100DA", "0100PM", "0100SSF", "0103", "0104", "0106", 
       "0108", "0124", "0124FLE", "0132", "0204", "0215", "0218", 
       "0219", "0225", "0227", "0234", "0235", "0244", "0301", 
       "0302", "0306", "0313", "0314", "0323", "0326", "0330", 
       "0331", "0401", "0404", "0405", "0406", "0407", "0408", 
       "0409", "0410", "0411", "0413", "0414", "0415", "0416", 
       "0417", "0418", "0420", "0421", "0422", "0423", "0424", 
       "0425", "0426", "0427", "0431", "0432", "0434", "0436", 
       "0442", "0444", "0445", "0453", "0461", "0462", "0463", 
       "0470", "0471", "0474", "0477", "0479", "0480", "0481", 
       "0482", "0484", "0485", "0492", "0497", "0501", "0503", 
       "0507", "0508", "0509", "0510", "0511", "0512", "0513", 
       "0514", "0515", "0516", "0520", "0525", "0528", "0531", 
       "0533", "0538", "0543", "0544", "0546", "0548", "0557", 
       "0559", "0564", "0566", "0570", "0572", "0574", "0575", 
       "0581", "0583", "0585", "0601", "0602", "0603", "0604", 
       "0605", "0606", "0607", "0608", "0611", "0612", "0613", 
       "0623", "0628", "0637", "0638", "0639", "0641", "0645", 
       "0701", "0702", "0703", "0705", "0706", "0710", "0712", 
       "0715", "0717", "0719", "0722", "0726", "0796", "0797", 
       "0798", "0799", "0801", "0802", "0804", "0805", "0811", 
       "0812", "0813", "0815", "0901", "0903", "0904", "0905", 
       "0906", "0907", "0908", "0909", "0910", "0911", "0914", 
       "0915", "0917", "0918", "0920", "0922", "0923", "0924", 
       "0926", "0929", "0931", "0932", "0940", "0941", "0953", 
       "0956", "0961", "0962", "0963", "0964", "0965", "0968", 
       "0969", "0972", "0973", "0974", "0976", "0979", "0980", 
       "0982", "0983", "0984", "0987", "0990", "0991", "0992", 
       "0994", "0996", "1003", "1005", "1006", "1007", "1008", 
       "1009", "1013", "1014", "1015", "1019", "1022", "1026", 
       "1027", "1028", "1035", "1044", "1047", "1069", "1070", 
       "1071", "1075", "1082", "1087", "1090", "1097", "10A2", 
       "1101", "1102", "1103", "1104", "1105", "1106", "1112", 
       "1118", "1121", "1127", "1131", "1140", "1149", "1153", 
       "1154", "1159", "1160", "1207", "1208", "1213", "1214", 
       "1215", "1223", "1226", "1229", "1230", "1233", "1239", 
       "124A", "1261", "1290", "1301", "1302", "1303", "1305", 
       "1307", "1308", "1309", "1310", "1311", "1313", "1314", 
       "1315", "1317", "1318", "1319", "1320", "1322", "1331", 
       "1332", "1333", "1334", "1401", "1402", "1403", "1409", 
       "1410", "1501", "1502", "1503", "1504", "1505", "1507", 
       "1510", "1603", "1604", "1605", "1609", "1610", "1612", 
       "1614", "1616", "1617", "1619", "1620", "1623", "1625", 
       "1634", "1636", "1643", "1645", "1658", "1659", "1716", 
       "1745", "1804", "1818", "1843", "1912", "1929", "1951", 
       "2102", "218A", "2310", "235A", "235B", "235C", "323A", 
       "323B", "4010", "4036", "4038", "4063", "4069", "4073", 
       "40SS", "4108", "557T", "609A", "706A", "706B", "9010", 
       "9018", "9019", "9031", "924A", "924B", "924D", "969A", 
       "969D", "969E", "969G", "969L", "969M", "969P", "969S", 
       "969Y", "977A", "A004", "A005", "A006", "A007", "A008", 
       "A010", "A010ABE", "A010ADE", "A010CBM", "A010PC", "A012", "A013", 
       "A014", "A015", "A016", "A018", "A019", "A020", "A022", 
       "A025", "A033", "A035", "A036", "A037", "A040", "A041", 
       "A042", "A044", "A045", "A048", "A04T", "A053", "A054", 
       "A058", "A059", "A060", "A062", "A064", "A065", "A066", 
       "A068", "A072", "A075", "A076", "A098", "A102", "A105", 
       "A108", "A122", "A124", "A125", "A138", "A139", "A140", 
       "A141", "A144", "A147", "A148", "A152", "A155", "A160", 
       "A162", "A163", "A168", "A179", "A204", "A209", "A210", 
       "A225", "A241", "A241LM", "A241PM", "A245", "A246", "A268", 
       "A279", "A286", "A341", "C002", "C050", "C086", "C093", 
       "C118", "D005", "D023", "E001", "E007", "E008", "E025", 
       "E037", "E039", "E041", "EG26", "F011", "F056", "G002", 
       "G005", "G012", "G014", "Q720", "UA10", "UA90", "UB10")

lapply(n, hours.analysis)
