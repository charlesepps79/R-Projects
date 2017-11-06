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
  durations.sub <- subset(durations, select = c("id_req", "id_frame", 
                                              "id_task", "num_idReq", 
                                              "num_idFrame", "num_idTask"))
  
  est <- merge(spec, durations.sub, by=c("id_req", "id_frame", "id_task"))
  est <- unique(est[,1:9])

  durations2.sub <- subset(durations, select = c("id_req", "desc_frameFam", 
                                               "id_task", "num_idReq", 
                                               "num_descFrameFam", 
                                               "num_idTask"))
  est2 <- merge(spec, durations2.sub, by=c("id_req", "desc_frameFam", 
                                         "id_task"))
  est2 <- unique(est2[,1:9])

  durations3.sub <- subset(durations, select = c("id_req", "id_task", 
                                               "num_idReq", "num_idTask"))
  est3 <- merge(spec, durations3.sub, by=c("id_req", "id_task"))
  est3 <- unique(est3[,1:8])

  durations4.sub <- subset(durations, select = c("id_frame", "id_task", 
                                               "num_idFrame", "num_idTask"))
  est4 <- merge(spec, durations4.sub, by=c("id_frame", "id_task"))
  est4 <- unique(est4[,1:8])

  durations5.sub <- subset(durations, select = c("id_frame", "desc_frameFam", 
                                               "id_task", "num_idFrame", 
                                               "num_descFrameFam", 
                                               "num_idTask"))
  est5 <- merge(spec, durations5.sub, by=c("id_frame", "desc_frameFam", 
                                         "id_task"))
  est5 <- unique(est5[,1:9])

  # Fit Models
  durations.lm <- lm(num_actDur ~ num_idFrame + num_idReq, 
                   data = durations)
  ##
  durations2.lm <- lm(num_actDur ~ num_descFrameFam + num_idReq, 
                    data = durations)
  ##
  durations3.lm <- lm(num_actDur ~ num_idReq, data = durations)
  ##
  durations4.lm <- lm(num_actDur ~ num_idFrame, data = 
                      durations)
  ##
  durations5.lm <- lm(num_actDur ~ num_descFrameFam, data = 
                      durations)

  est["num_estimate"] <- NA
  est$num_estimate <- predict(durations.lm, newdata = est)
  error <- merge(est, durations, by=c("id_req", "id_frame", "id_task",
                                    "desc_frameFam"))
  error <- subset(error, select = c("id_project", "id_req", "id_frame", 
                                  "id_task", "desc_frameFam",
                                  "num_estimate", "num_estDur", 
                                  "num_actDur", "date_actStart", "date_actFinish"))
  error <- unique(error[,1:10])
  error["num_logError"] <- NA
  error$num_logError <- log(error$num_estimate)-log(error$num_actDur)
  ## -- Multivariate lm() "mlm" -----------
  utils::example("SSD", echo=FALSE)
  error["num_resSD"] <- NA
  error$num_resSD <- sigma(durations.lm) # is the same as {but more efficient than}
  error["num_oneSigma"] <- NA
  error$num_oneSigma <- error$num_estimate+error$num_resSD

  est2["num_estimate"] <- NA
  est2$num_estimate <- predict(durations2.lm, newdata = est2)
  error2 <- merge(est2, durations, by=c("id_req", "id_frame", "id_task",
                                      "desc_frameFam"))
  error2 <- subset(error2, select = c("id_project", "id_req", "id_frame", 
                                    "id_task", "desc_frameFam", 
                                    "num_estimate", "num_estDur", 
                                    "num_actDur", "date_actStart", "date_actFinish"))
  error2 <- unique(error2[,1:10])
  error2["num_logError"] <- NA
  error2$num_logError <- log(error2$num_estimate)-log(error2$num_actDur)
  ## -- Multivariate lm() "mlm" -----------
  utils::example("SSD", echo=FALSE)
  error2["num_resSD"] <- NA
  error2$num_resSD <- sigma(durations2.lm) # is the same as {but more efficient than}
  error2["num_oneSigma"] <- NA
  error2$num_oneSigma <- error2$num_estimate+error2$num_resSD

  est3["num_estimate"] <- NA
  est3$num_estimate <- predict(durations3.lm, newdata = est3)
  error3 <- merge(est3, durations, by=c("id_req", "id_task"))
  error3 <- subset(error3, select = c("id_project", "id_req", "id_task", 
                                    "num_estimate", 
                                    "num_estDur", "num_actDur", 
                                    "date_actStart", "date_actFinish"))
  error3 <- unique(error3[,1:8])
  error3["num_logError"] <- NA
  error3$num_logError <- log(error3$num_estimate)-log(error3$num_actDur)
  ## -- Multivariate lm() "mlm" -----------
  utils::example("SSD", echo=FALSE)
  error3["num_resSD"] <- NA
  error3$num_resSD <- sigma(durations3.lm) # is the same as {but more efficient than}
  error3["num_oneSigma"] <- NA
  error3$num_oneSigma <- error3$num_estimate+error3$num_resSD
  
  est4["num_estimate"] <- NA
  est4$num_estimate <- predict(durations4.lm, newdata = est4)
  error4 <- merge(est4, durations, by=c("id_frame", "id_task"))
  error4 <- subset(error4, select = c("id_project", "id_frame", "id_task", 
                                    "num_estimate", 
                                    "num_estDur", "num_actDur", 
                                    "date_actStart", "date_actFinish"))
  error4 <- unique(error4[,1:8])
  error4["num_logError"] <- NA
  error4$num_logError <- log(error4$num_estimate)-log(error4$num_actDur)
  ## -- Multivariate lm() "mlm" -----------
  utils::example("SSD", echo=FALSE)
  error4["num_resSD"] <- NA
  error4$num_resSD <- sigma(durations4.lm) # is the same as {but more efficient than}
  error4["num_oneSigma"] <- NA
  error4$num_oneSigma <- error4$num_estimate+error4$num_resSD

  est5["num_estimate"] <- NA
  est5$num_estimate <- predict(durations5.lm, newdata = est5)
  error5 <- merge(est5, durations, by=c("id_frame", "id_task",
                                      "desc_frameFam"))
  error5 <- subset(error5, select = c("id_project", "id_frame", "id_task", 
                                    "desc_frameFam", 
                                    "num_estimate", "num_estDur", 
                                    "num_actDur", "date_actStart", 
                                    "date_actFinish"))
  error5 <- unique(error5[,1:9])
  error5["num_logError"] <- NA
  error5$num_logError <- log(error5$num_estimate)-log(error5$num_actDur)
  ## -- Multivariate lm() "mlm" -----------
  utils::example("SSD", echo=FALSE)
  error5["num_resSD"] <- NA
  error5$num_resSD <- sigma(durations5.lm) # is the same as {but more efficient than}
  error5["num_oneSigma"] <- NA
  error5$num_oneSigma <- error5$num_estimate+error5$num_resSD
  
  data <- subset(error, select = c("id_req", "id_frame", 
                                 "id_task", "desc_frameFam",
                                 "num_estimate"))
  data <- unique(data[,1:5])
  
  data2 <- subset(error2, select = c("id_req", "id_frame", 
                                   "id_task", "desc_frameFam",
                                   "num_estimate"))
  data2 <- unique(data2[,1:5])
  
  data3 <- subset(error3, select = c("id_req", "id_task", 
                                   "num_estimate"))
  data3 <- unique(data3[,1:3])
  
  data4 <- subset(error4, select = c("id_frame", "id_task", 
                                   "num_estimate"))
  data4 <- unique(data4[,1:3])
  
  data5 <- subset(error5, select = c("desc_frameFam", "id_task", "num_estimate"))
  data5 <- unique(data5[,1:3])

  file <- "Duration_Estimate_Template.xlsx"
  wb <- loadWorkbook(file)
  sheets <- getSheets(wb)
  sheet <- sheets[[1]]  # or another
  addDataFrame(data, sheet, col.names = FALSE, row.names = FALSE,
             startRow = 2, startColumn = 1)
  sheets <- getSheets(wb)
  sheet <- sheets[[2]]  # or another
  addDataFrame(data2, sheet, col.names = FALSE, row.names = FALSE,
             startRow = 2, startColumn = 1)
  sheets <- getSheets(wb)
  sheet <- sheets[[3]]  # or another
  addDataFrame(data3, sheet, col.names = FALSE, row.names = FALSE,
             startRow = 2, startColumn = 1)
  sheets <- getSheets(wb)
  sheet <- sheets[[4]]  # or another
  addDataFrame(data4, sheet, col.names = FALSE, row.names = FALSE,
             startRow = 2, startColumn = 1)
  sheets <- getSheets(wb)
  sheet <- sheets[[5]]  # or another
  addDataFrame(data5, sheet, col.names = FALSE, row.names = FALSE,
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
###########################################################################

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
# setwd("C:/Users/502689880/Desktop/FMI_Estimates")
getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

setwd("C:/Users/502689880/Desktop/FMI_Estimates")

hours.analysis <- function(mli,...){
  
  try({requirements <- read.csv("requirements.csv", header = TRUE, 
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
  hours.sub <- subset(hours, select = c("id_req", "id_frame", 
                                      "id_task", "num_idReq", 
                                      "num_idFrame", "num_idTask"))

  est <- merge(spec, hours.sub, by=c("id_req", "id_frame", "id_task"))
  est <- unique(est[,1:10])

  hours2.sub <- subset(hours, select = c("id_req", "desc_frameFam", 
                                       "id_task", "num_idReq", 
                                       "num_descFrameFam", 
                                       "num_idTask"))
  est2 <- merge(spec, hours2.sub, by=c("id_req", "desc_frameFam", 
                                     "id_task"))
  est2 <- unique(est2[,1:10])

  hours3.sub <- subset(hours, select = c("id_req", "id_task", 
                                       "num_idReq", "num_idTask"))
  est3 <- merge(spec, hours3.sub, by=c("id_req", "id_task"))
  est3 <- unique(est3[,1:9])

  hours4.sub <- subset(hours, select = c("id_frame", "id_task", 
                                       "num_idFrame", "num_idTask"))
  est4 <- merge(spec, hours4.sub, by=c("id_frame", "id_task"))
  est4 <- unique(est4[,1:9])

  hours5.sub <- subset(hours, select = c("id_frame", "desc_frameFam", 
                                       "id_task", "num_idFrame", 
                                       "num_descFrameFam", 
                                       "num_idTask"))
  est5 <- merge(spec, hours5.sub, by=c("id_frame", "desc_frameFam", 
                                     "id_task"))
  est5 <- unique(est5[,1:10])

  # Fit Models
  hours.lm <- lm(num_totalHours ~ num_idFrame + num_idTask + num_idReq, 
               data = hours)
  hours.loglm <- lm(log(num_totalHours) ~ log(num_idFrame + num_idTask + 
                                              num_idReq), data = hours)
  
  ##
  hours2.lm <- lm(num_totalHours ~ num_descFrameFam + num_idTask + num_idReq, 
                data = hours)
  hours2.loglm <- lm(log(num_totalHours) ~ log(num_descFrameFam + num_idTask 
                                             + num_idReq), data = hours)
  
  ##
  hours3.lm <- lm(num_totalHours ~ num_idTask + num_idReq, data = hours)
  hours3.loglm <- lm(log(num_totalHours) ~ log(num_idTask + num_idReq), 
                   data = hours)

  ##
  hours4.lm <- lm(num_totalHours ~ num_idTask + num_idFrame, data = 
                  hours)
  hours4.loglm <- lm(log(num_totalHours) ~ log(num_idTask + num_idFrame), 
                   data = hours)

  ##
  hours5.lm <- lm(num_totalHours ~ num_idTask + num_descFrameFam, data = 
                  hours)
  hours5.loglm <- lm(log(num_totalHours) ~ log(num_idTask + num_descFrameFam), 
                   data = hours)

  est["num_estimate"] <- NA
  est$num_estimate <- predict(hours.lm, newdata = est)
  est["num_logEstimate"] <- NA
  est$num_logEstimate <- predict(hours.loglm, newdata = est)
  error <- merge(est, hours, by=c("id_req", "id_frame", "id_task",
                                "desc_frameFam"))
  error <- subset(error, select = c("id_project", "id_req", "id_frame", 
                                  "id_task", "desc_frameFam", 
                                  "num_estimate", 
                                  "num_totalHours"))
  error <- unique(error[,1:7])
  error["num_logError"] <- NA
  error$num_logError <- log(error$num_estimate)-log(error$num_totalHours)
  ## -- Multivariate lm() "mlm" -----------
  utils::example("SSD", echo=FALSE)
  error["num_resSD"] <- NA
  error$num_resSD <- sigma(hours.lm) # is the same as {but more efficient than}
  error["num_oneSigma"] <- NA
  error$num_oneSigma <- error$num_estimate+error$num_resSD

  est2["num_estimate"] <- NA
  est2$num_estimate <- predict(hours2.lm, newdata = est2)
  est2["num_logEstimate"] <- NA
  est2$num_logEstimate <- predict(hours2.loglm, newdata = est2)
  error2 <- merge(est2, hours, by=c("id_req", "id_frame", "id_task",
                                  "desc_frameFam"))
  error2 <- subset(error2, select = c("id_project", "id_req", "id_frame", 
                                    "id_task", "desc_frameFam", 
                                    "num_estimate", "num_totalHours"))
  error2 <- unique(error2[,1:7])
  error2["num_logError"] <- NA
  error2$num_logError <- log(error2$num_estimate)-log(error2$num_totalHours)
  ## -- Multivariate lm() "mlm" -----------
  utils::example("SSD", echo=FALSE)
  error2["num_resSD"] <- NA
  error2$num_resSD <- sigma(hours2.lm) # is the same as {but more efficient than}
  error2["num_oneSigma"] <- NA
  error2$num_oneSigma <- error2$num_estimate+error2$num_resSD

  est3["num_estimate"] <- NA
  est3$num_estimate <- predict(hours3.lm, newdata = est3)
  est3["num_logEstimate"] <- NA
  est3$num_logEstimate <- predict(hours3.loglm, newdata = est3)
  error3 <- merge(est3, hours, by=c("id_req", "id_task"))
  error3 <- subset(error3, select = c("id_project", "id_req", "id_task", "num_estimate", 
                                    "num_totalHours"))
  error3 <- unique(error3[,1:5])
  error3["num_logError"] <- NA
  error3$num_logError <- log(error3$num_estimate)-log(error3$num_totalHours)
  ## -- Multivariate lm() "mlm" -----------
  utils::example("SSD", echo=FALSE)
  error3["num_resSD"] <- NA
  error3$num_resSD <- sigma(hours3.lm) # is the same as {but more efficient than}
  error3["num_oneSigma"] <- NA
  error3$num_oneSigma <- error3$num_estimate+error3$num_resSD

  est4["num_estimate"] <- NA
  est4$num_estimate <- predict(hours4.lm, newdata = est4)
  est4["num_logEstimate"] <- NA
  est4$num_logEstimate <- predict(hours4.loglm, newdata = est4)
  error4 <- merge(est4, hours, by=c("id_frame", "id_task"))
  error4 <- subset(error4, select = c("id_project", "id_frame", "id_task", "num_estimate", 
                                    "num_totalHours"))
  error4 <- unique(error4[,1:5])
  error4["num_logError"] <- NA
  error4$num_logError <- log(error4$num_estimate)-log(error4$num_totalHours)
  ## -- Multivariate lm() "mlm" -----------
  utils::example("SSD", echo=FALSE)
  error4["num_resSD"] <- NA
  error4$num_resSD <- sigma(hours4.lm) # is the same as {but more efficient than}
  error4["num_oneSigma"] <- NA
  error4$num_oneSigma <- error4$num_estimate+error4$num_resSD

  est5["num_estimate"] <- NA
  est5$num_estimate <- predict(hours5.lm, newdata = est5)
  est5["num_logEstimate"] <- NA
  est5$num_logEstimate <- predict(hours5.loglm, newdata = est5)
  error5 <- merge(est5, hours, by=c("id_frame", "id_task",
                                  "desc_frameFam"))
  error5 <- subset(error5, select = c("id_project", "id_frame", 
                                    "id_task", "desc_frameFam", 
                                    "num_estimate", "num_totalHours"))
  error5 <- unique(error5[,1:6])
  error5["num_logError"] <- NA
  error5$num_logError <- log(error5$num_estimate)-log(error5$num_totalHours)
  ## -- Multivariate lm() "mlm" -----------
  utils::example("SSD", echo=FALSE)
  error5["num_resSD"] <- NA
  error5$num_resSD <- sigma(hours5.lm) # is the same as {but more efficient than}
  error5["num_oneSigma"] <- NA
  error5$num_oneSigma <- error5$num_estimate+error5$num_resSD

  data <- subset(error, select = c("id_req", "id_frame", 
                                 "id_task", "desc_frameFam",
                                 "num_estimate", "num_resSD"))
  data <- unique(data[,1:6])
  data2 <- subset(error2, select = c("id_req", "id_frame", 
                                   "id_task", "desc_frameFam",
                                   "num_estimate", "num_resSD"))
  data2 <- unique(data2[,1:6])
  data3 <- subset(error3, select = c("id_req", "id_task", 
                                   "num_estimate", "num_resSD"))
  data3 <- unique(data3[,1:4])
  data4 <- subset(error4, select = c("id_frame", "id_task", 
                                   "num_estimate", "num_resSD"))
  data4 <- unique(data4[,1:4])
  data5 <- subset(error5, select = c("desc_frameFam", "id_task", "num_estimate", 
                                   "num_resSD"))
  data5 <- unique(data5[,1:4])

  file <- "Hours_Estimate_Template.xlsx"
  wb <- loadWorkbook(file)
  sheets <- getSheets(wb)
  sheet <- sheets[[1]]  # or another
  addDataFrame(data, sheet, col.names = FALSE, row.names = FALSE,
             startRow = 2, startColumn = 1)
  sheets <- getSheets(wb)
  sheet <- sheets[[2]]  # or another
  addDataFrame(data2, sheet, col.names = FALSE, row.names = FALSE,
             startRow = 2, startColumn = 1)
  sheets <- getSheets(wb)
  sheet <- sheets[[3]]  # or another
  addDataFrame(data3, sheet, col.names = FALSE, row.names = FALSE,
             startRow = 2, startColumn = 1)
  sheets <- getSheets(wb)
  sheet <- sheets[[4]]  # or another
  addDataFrame(data4, sheet, col.names = FALSE, row.names = FALSE,
             startRow = 2, startColumn = 1)
  sheets <- getSheets(wb)
  sheet <- sheets[[5]]  # or another
  addDataFrame(data5, sheet, col.names = FALSE, row.names = FALSE,
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

###########################################################################
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
# setwd("C:/Users/502689880/Desktop/FMI_Estimates")
getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

setwd("C:/Users/502689880/Desktop/FMI_Estimates")

leadtime.analysis <- function(mli,...){
  try({req <- read.csv("requirements.csv", header = TRUE, 
                stringsAsFactors = FALSE, na.strings = c(""," ","NA"))
  lt <- read.csv("ltData.csv", header = TRUE, 
               stringsAsFactors = FALSE, na.strings = c(""," ","NA"))

  lt <- lt[which(lt$id_task == mli),]

  lead <- merge(req, lt, by="id_tparent")
  lead <- unique(lead[,1:36])

  spec <- read.csv("parts.csv", header = TRUE, 
                 stringsAsFactors = FALSE, na.strings = c(""," ","NA"))

  # Convert character variables to factors
  lead$id_frame <- as.factor(lead$id_frame)
  lead$id_req <- as.factor(lead$id_req)
  lead$id_task <- as.factor(lead$id_task)
  lead$desc_frameFam <- as.factor(lead$desc_frameFam)
  lead$id_part <- as.factor(lead$id_part)

  # Create new columns for numeric representation
  lead["num_idFrame"] <- NA
  lead["num_idReq"] <- NA
  lead["num_idTask"] <- NA
  lead["num_descFrameFam"] <- NA
  lead["num_idPart"] <- NA

  # Convert factor variables to numeric
  lead$num_idFrame <- as.numeric(lead$id_frame)
  lead$num_idReq <- as.numeric(lead$id_req)
  lead$num_idTask <- as.numeric(lead$id_task)
  lead$num_descFrameFam <- as.numeric(lead$desc_frameFam)
  lead$num_idPart <- as.numeric(lead$id_part)

  lead <- subset(lead, select = c("id_project", "id_req", "id_frame", "id_task", 
                                "id_part", "desc_frameFam", "num_idReq", 
                                "num_idFrame", "num_idTask", "num_idPart", 
                                "num_descFrameFam", "num_totLead"))

  lead <- unique(lead[,1:12])

  # Create Estimate Tables Est=T/R/F.v, Est2=T/R/F, Est3=T/R, Est4=T/F.v, 
  # Est5=T/F, Est6=T
  lead.sub <- subset(lead, select = c("id_req", "id_frame", "id_task", 
                                    "id_part", "num_idReq", "num_idFrame", 
                                    "num_idTask", "num_idPart"))

  est <- merge(spec, lead.sub, by=c("id_req", "id_frame", "id_task", "id_part"))
  est <- unique(est[,1:13])

  # Fit Models
  lead.lm <- lm(num_totLead ~ num_idFrame + num_idTask + num_idReq + num_idPart, 
              data = lead)

  est["num_estimate"] <- NA
  est$num_estimate <- predict(lead.lm, newdata = est)
  error <- merge(est, lead, by=c("id_req", "id_frame", "id_task",
                               "desc_frameFam", "id_part"))
  error <- subset(error, select = c("id_project", "id_req", "id_frame", 
                                  "id_task", "desc_frameFam", "id_part", 
                                  "num_estimate", 
                                  "num_totLead"))
  error <- unique(error[,1:8])
  error["num_logError"] <- NA
  error$num_logError <- log(error$num_estimate)-log(error$num_totLead)
  ## -- Multivariate lm() "mlm" -----------
  utils::example("SSD", echo=FALSE)
  error["num_resSD"] <- NA
  error$num_resSD <- sigma(lead.lm) # is the same as {but more efficient than}
  error["num_oneSigma"] <- NA
  error$num_oneSigma <- error$num_estimate+error$num_resSD

  data <- subset(error, select = c("id_req", "id_frame", 
                                 "id_task", "desc_frameFam",
                                 "id_part", "num_estimate"))
  data <- unique(data[,1:6])

  file <- "Lead_Time_Estimate_Template.xlsx"
  wb <- loadWorkbook(file)
  sheets <- getSheets(wb)
  sheet <- sheets[[1]]  # or another
  addDataFrame(data, sheet, col.names = FALSE, row.names = FALSE,
             startRow = 2, startColumn = 1)
  sheets <- getSheets(wb)
  sheet <- sheets[[2]]  # or another
  addDataFrame(data, sheet, col.names = FALSE, row.names = FALSE,
             startRow = 2, startColumn = 1)
  saveWorkbook(wb, paste0(mli, "_Lead_Time_Estimate.xlsx"))
  })
}

n <- c("0918", 	"A179", 	"1213", 	"0953", 	"0924", 	"0585", 	"1082", 	
       "A140", 	"0566", 	"0991", 	"0411", 	"0108", 	"0811", 	"1604", 	
       "0424", 	"0929", 	"0405", 	"0425", 	"0961", 	"A042", 	"0302", 	
       "0639", 	"1207", 	"0722", 	"A204", 	"0104", 	"0548", 	"0462", 	
       "A037", 	"00TM", 	"0726", 	"1402", 	"0706", 	"0965", 	"A209", 	
       "UA10", 	"UB10", 	"0470", 	"A033", 	"0968", 	"AD19", 	"235A", 	
       "1605", 	"4036", 	"A111", 	"0323", 	"1155", 	"1332", 	"1401", 	
       "C118", 	"1331", 	"0330", 	"0905", 	"0987", 	"E025", 	"1113", 	
       "1309", 	"A132", 	"0799", 	"235B", 	"0969", 	"1612", 	"0234", 	
       "1409", 	"1502", 	"0407", 	"1333", 	"C086", 	"0798", 	"4063", 	
       "A035", 	"1403", 	"969G", 	"A068", 	"0972", 	"1160", 	"0438", 	
       "1603", 	"A141", 	"1118", 	"0245", 	"0482", 	"0311", 	"1104", 	
       "0469", 	"0802", 	"0409", 	"A148", 	"0908", 	"1617", 	"0103", 	
       "0326", 	"1319", 	"0623", 	"969M", 	"A003", 	"40SS", 	"A150", 	
       "0403", 	"0964", 	"323A", 	"4073", 	"A053", 	"323B", 	"A139", 	
       "A006", 	"0111", 	"0976", 	"A04T", 	"EG26", 	"1645", 	"0801", 	
       "1007", 	"A184", 	"0922", 	"0475", 	"ACCY", 	"0205", 	"1005", 	
       "1059", 	"1652", 	"A125", 	"0227", 	"0305", 	"0419", 	"0202", 	
       "0410", 	"A192", 	"A065", 	"A151", 	"G002", 	"0408", 	"0559", 	
       "0907", 	"1154", 	"F084", 	"0546", 	"1625", 	"9010", 	"0983", 	
       "A273", 	"1026", 	"0465", 	"0979", 	"G023", 	"0201", 	"A162", 	
       "1627", 	"218A", 	"A130", 	"A152", 	"A070", 	"A268", 	"A315", 	
       "0214", 	"0238", 	"1101", 	"0406", 	"A005", 	"0557", 	"218B", 	
       "969A", 	"0329", 	"0607", 	"4035", 	"G015", 	"0963", 	"1507", 	
       "0484", 	"0601", 	"0995", 	"0244", 	"0911", 	"0435", 	"A341", 	
       "0906", 	"1071", 	"0421", 	"0510", 	"4069", 	"A138", 	"0613", 	
       "0812", 	"0910", 	"1107", 	"4007", 	"924A", 	"0427", 	"9019", 	
       "0067", 	"0207", 	"0605", 	"1087", 	"1140", 	"A164", 	"G010", 	
       "0218", 	"0904", 	"1658", 	"0926", 	"1047", 	"1233", 	"1614", 	
       "A073", 	"A102", 	"A168", 	"F056", 	"0226", 	"1311", 	"0531", 	
       "0974", 	"1310", 	"4002", 	"4038", 	"0982", 	"A225", 	"1195", 	
       "A187", 	"G012", 	"0431", 	"0485", 	"1313", 	"0199", 	"969S", 	
       "A013", 	"A076", 	"0101", 	"1019", 	"1090", 	"1215", 	"969L", 	
       "0453", 	"0992", 	"969D", 	"0122", 	"0132", 	"0110", 	"0412", 	
       "924B", 	"0211", 	"0574", 	"0914", 	"1322", 	"1501", 	"0917", 	
       "1070", 	"235C", 	"A052", 	"A60E", 	"0065", 	"0209", 	"0544", 	
       "4004", 	"A124", 	"A246", 	"0121", 	"0208", 	"1647", 	"A012", 	
       "A098", 	"0575", 	"0813", 	"1623", 	"0413", 	"A60U", 	"0941", 	
       "1044", 	"1510", 	"9018", 	"969C", 	"F066", 	"0481", 	"1001", 	
       "4045", 	"A286", 	"0087", 	"0219", 	"0815", 	"0956", 	"1003", 	
       "9031", 	"A245", 	"A287", 	"0069", 	"0088", 	"0645", 	"0916", 	
       "1106", 	"1112", 	"1410", 	"706B", 	"924D", 	"A096", 	"A60G", 	
       "0650", 	"0712", 	"1097", 	"969P", 	"0520", 	"0525", 	"1314", 	
       "1503", 	"A105", 	"0222", 	"0903", 	"1609", 	"1659", 	"A60H", 	
       "0125", 	"0603", 	"0608", 	"0932", 	"1006", 	"A116", 	"0538", 	
       "0919", 	"0981", 	"0994", 	"0996", 	"0CCC", 	"1156", 	"A090", 	
       "A60C", 	"0225", 	"0461", 	"0602", 	"0796", 	"0923", 	"0931", 	
       "1027", 	"1102", 	"969F", 	"0212", 	"0511", 	"0564", 	"0581", 	
       "0948", 	"969Y", 	"A60P", 	"G004", 	"G028", 	"0501", 	"0727", 	
       "1053", 	"1635", 	"1637", 	"706A", 	"E041", 	"0102", 	"0583", 	
       "0628", 	"1015", 	"1075", 	"1602", 	"9030", 	"918T", 	"A045", 	
       "0312", 	"0582", 	"0611", 	"0942", 	"0966", 	"0980", 	"0990", 	
       "1320", 	"9040", 	"969E", 	"A62F", 	"E031", 	"0516", 	"0533", 	
       "0627", 	"0724", 	"0902", 	"0947", 	"1010", 	"1051", 	"969J", 	
       "A044", 	"A279", 	"A62A", 	"0441", 	"0476", 	"0497", 	"0502", 	
       "0522", 	"0540", 	"0565", 	"0567", 	"1035", 	"1109", 	"1135", 	
       "1138", 	"1153", 	"1218", 	"4072", 	"A007", 	"A62P", 	"0089", 	
       "0466", 	"0468", 	"0514", 	"0606", 	"1014", 	"1023", 	"1096", 	
       "1110", 	"0570", 	"1038", 	"10A2", 	"9008", 	"A056", 	"A62C", 	
       "A62U", 	"0480", 	"0940", 	"1029", 	"1334", 	"1613", 	"A036", 	
       "A107", 	"A144", 	"A60T", 	"F011", 	"0090", 	"0307", 	"0803", 	
       "0971", 	"1030", 	"1098", 	"1230", 	"4079", 	"A153", 	"0092", 	
       "0235", 	"0479", 	"0499", 	"0549", 	"0644", 	"0938", 	"0984", 	
       "1013", 	"1158", 	"1610", 	"165J", 	"969W", 	"A025", 	"A135", 	
       "A186", 	"A197", 	"A217", 	"A218", 	"F070", 	"0091", 	"0229", 	
       "0239", 	"0423", 	"0967", 	"1016", 	"1028", 	"A031", 	"A047", 	
       "A117", 	"A118", 	"A62T", 	"E004", 	"E007", 	"E026", 	"E043", 	
       "G025", 	"0210", 	"0319", 	"0402", 	"0447", 	"0543", 	"0568", 	
       "0710", 	"0725", 	"0973", 	"1009", 	"1043", 	"1076", 	"1084", 	
       "1085", 	"1114", 	"1145", 	"1162", 	"1163", 	"1315", 	"1616", 	
       "1636", 	"4012", 	"A018", 	"A026", 	"A066", 	"A109", 	"A115", 	
       "A175", 	"E037", 	"0215", 	"0232", 	"0315", 	"0324", 	"0327", 	
       "0429", 	"0498", 	"0503", 	"0508", 	"0547", 	"0573", 	"0576", 	
       "0577", 	"0614", 	"0624", 	"0641", 	"0950", 	"0998", 	"1008", 	
       "1123", 	"1131", 	"1150", 	"1199", 	"1235", 	"1505", 	"1615", 	
       "1641", 	"165B", 	"1818", 	"4033", 	"4059", 	"4077", 	"4082", 	
       "9004", 	"9005", 	"A001", 	"A015", 	"A020", 	"A075", 	"A081", 	
       "A092", 	"A097", 	"A127", 	"A137", 	"A142", 	"A147", 	"A163", 	
       "A166", 	"A167", 	"A216", 	"A241", 	"B011", 	"C050", 	"E006", 	
       "E008", 	"E020", 	"F098", 	"G014", 	"G029", 	"G057", 	"0332", 	
       "0430", 	"0433", 	"0448", 	"0452", 	"0460", 	"0464", 	"0467", 	
       "0478", 	"0488", 	"0489", 	"0515", 	"0604", 	"0612", 	"0638", 	
       "0648", 	"0715", 	"0804", 	"1069", 	"1079", 	"1083", 	"1149", 	
       "1208", 	"1223", 	"1226", 	"1229", 	"1239", 	"124A", 	"1290", 	
       "1307", 	"1308", 	"1317", 	"1318", 	"1620", 	"1716", 	"1745", 	
       "1804", 	"1843", 	"1912", 	"1929", 	"1951", 	"2102", 	"2310", 	
       "609A", 	"696C", 	"725A", 	"725B", 	"9006", 	"977A", 	"A008", 	
       "A022", 	"A032", 	"A046", 	"A048", 	"A054", 	"A057", 	"A058", 	
       "A060", 	"A064", 	"A072", 	"A095", 	"C002", 	"C011", 	"C093", 	
       "D005", 	"D023", 	"E001", 	"E047", 	"F061", 	"G005", 	"G007", 	
       "G008", 	"Q720")

lapply(n, leadtime.analysis)
