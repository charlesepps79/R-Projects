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

leadtime.analysis <- function(mli,...){
  try({
    req <- read.csv("requirements.csv", header = TRUE, 
                       stringsAsFactors = FALSE, na.strings = c(""," ","NA"))
  lt <- read.csv("ltData.csv", header = TRUE, 
                 stringsAsFactors = FALSE, na.strings = c(""," ","NA"))
  
  lt <- lt[which(lt$id_task == mli),]
  
  lead <- merge(req, lt, by="id_tparent")
  lead$date_contractStart <- as.Date(lead$date_contractStart, "%m/%d/%Y")
  lead$date_issueFMI <- as.Date(lead$date_issueFMI, "%m/%d/%Y")
  lead <- unique(lead[,1:39])
  
  lead <- lead[which(lead$desc_projectState == "Complete"),]
  lead <- subset(lead, date_issueFMI >= "2015-10-19" & date_issueFMI <= "2017-10-19")
  
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
  
  lead <- subset(lead, select = c("id_project", "id_req", "id_frame", 
                                  "id_task", "id_part", "desc_frameFam", 
                                  "num_idReq", "num_idFrame", "num_idTask", 
                                  "num_idPart", "num_descFrameFam", 
                                  "num_totLead"))
  
  lead <- unique(lead[,1:12])
  
  # Create Estimate Tables Est=T/R/F.v, Est2=T/R/F, Est3=T/R, Est4=T/F.v, 
  # Est5=T/F, Est6=T
  lead.sub <- subset(lead, select = c("id_req", "id_frame", "id_task", 
                                      "id_part", "num_idReq", "num_idFrame", 
                                      "num_idTask", "num_idPart"))
  est <- merge(spec, lead.sub, by=c("id_req", "id_frame", "id_task", "id_part"))
  est <- unique(est[,1:13])
  
  lead2.sub <- subset(lead, select = c("id_req", "desc_frameFam", "id_task", 
                                      "id_part", "num_idReq", 
                                      "num_descFrameFam", "num_idTask", 
                                      "num_idPart"))
  est2 <- merge(spec, lead2.sub, by=c("id_req", "desc_frameFam", "id_task", 
                                    "id_part"))
  est2 <- unique(est2[,1:13])
  
  lead3.sub <- subset(lead, select = c("id_req", "id_task", "id_part", 
                                       "num_idReq", "num_idTask", 
                                       "num_idPart"))
  est3 <- merge(spec, lead3.sub, by=c("id_req", "id_task", "id_part"))
  est3 <- unique(est3[,1:12])
  
  lead4.sub <- subset(lead, select = c("id_frame", "id_task", "id_part", 
                                       "num_idFrame", "num_idTask", 
                                       "num_idPart"))
  est4 <- merge(spec, lead4.sub, by=c("id_frame", "id_task", "id_part"))
  est4 <- unique(est4[,1:12])
  
  lead5.sub <- subset(lead, select = c("id_frame", "desc_frameFam", 
                                       "id_task", "id_part", "num_idFrame", 
                                       "num_descFrameFam", "num_idTask", 
                                       "num_idPart"))
  est5 <- merge(spec, lead5.sub, by=c("id_frame", "desc_frameFam", 
                                      "id_task", "id_part"))
  est5 <- unique(est5[,1:13])
  
  # Fit Models
  lead.lm <- lm(num_totLead ~ num_idFrame + num_idReq + num_idPart, 
                data = lead)
  lead2.lm <- lm(num_totLead ~ num_descFrameFam + num_idReq + num_idPart, 
                data = lead)
  lead3.lm <- lm(num_totLead ~ num_idReq + num_idPart, 
                 data = lead)
  lead4.lm <- lm(num_totLead ~ num_idFrame, 
                 data = lead)
  lead5.lm <- lm(num_totLead ~ num_descFrameFam, 
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
  
  est2["num_estimate"] <- NA
  est2$num_estimate <- predict(lead2.lm, newdata = est2)
  error2 <- merge(est2, lead, by=c("id_req", "id_frame", "id_task",
                                 "desc_frameFam", "id_part"))
  error2 <- subset(error2, select = c("id_project", "id_req", "id_frame", 
                                    "id_task", "desc_frameFam", "id_part", 
                                    "num_estimate", "num_totLead"))
  error2 <- unique(error2[,1:8])
  error2["num_logError"] <- NA
  error2$num_logError <- log(error2$num_estimate)-log(error2$num_totLead)
  ## -- Multivariate lm() "mlm" -----------
  utils::example("SSD", echo=FALSE)
  error2["num_resSD"] <- NA
  error2$num_resSD <- sigma(lead2.lm) # is the same as {but more efficient than}
  error2["num_oneSigma"] <- NA
  error2$num_oneSigma <- error2$num_estimate+error2$num_resSD
  
  est3["num_estimate"] <- NA
  est3$num_estimate <- predict(lead3.lm, newdata = est3)
  error3 <- merge(est3, lead, by=c("id_req", "id_task", "id_part"))
  error3 <- subset(error3, select = c("id_project", "id_req", "id_task", 
                                      "id_part", "num_estimate", 
                                      "num_totLead"))
  error3 <- unique(error3[,1:6])
  error3["num_logError"] <- NA
  error3$num_logError <- log(error3$num_estimate)-log(error3$num_totLead)
  ## -- Multivariate lm() "mlm" -----------
  utils::example("SSD", echo=FALSE)
  error3["num_resSD"] <- NA
  error3$num_resSD <- sigma(lead3.lm) # is the same as {but more efficient than}
  error3["num_oneSigma"] <- NA
  error3$num_oneSigma <- error3$num_estimate+error3$num_resSD
  
  est4["num_estimate"] <- NA
  est4$num_estimate <- predict(lead4.lm, newdata = est4)
  error4 <- merge(est4, lead, by=c("id_frame", "id_task", "id_part"))
  error4 <- subset(error4, select = c("id_project", "id_frame", "id_task", 
                                      "id_part", "num_estimate", 
                                      "num_totLead"))
  error4 <- unique(error4[,1:6])
  error4["num_logError"] <- NA
  error4$num_logError <- log(error4$num_estimate)-log(error4$num_totLead)
  ## -- Multivariate lm() "mlm" -----------
  utils::example("SSD", echo=FALSE)
  error4["num_resSD"] <- NA
  error4$num_resSD <- sigma(lead4.lm) # is the same as {but more efficient than}
  error4["num_oneSigma"] <- NA
  error4$num_oneSigma <- error4$num_estimate+error4$num_resSD
  
  est5["num_estimate"] <- NA
  est5$num_estimate <- predict(lead5.lm, newdata = est5)
  error5 <- merge(est5, lead, by=c("id_frame", "id_task", "desc_frameFam", 
                                   "id_part"))
  error5 <- subset(error5, select = c("id_project", "id_frame", "id_task", 
                                      "desc_frameFam", "id_part", 
                                      "num_estimate", "num_totLead"))
  error5 <- unique(error5[,1:7])
  error5["num_logError"] <- NA
  error5$num_logError <- log(error5$num_estimate)-log(error5$num_totLead)
  ## -- Multivariate lm() "mlm" -----------
  utils::example("SSD", echo=FALSE)
  error5["num_resSD"] <- NA
  error5$num_resSD <- sigma(lead5.lm) # is the same as {but more efficient than}
  error5["num_oneSigma"] <- NA
  error5$num_oneSigma <- error5$num_estimate+error5$num_resSD
  
  data <- subset(error, select = c("id_req", "id_frame", 
                                   "id_task", "desc_frameFam",
                                   "id_part", "num_estimate"))
  data <- unique(data[,1:6])
  
  data2 <- subset(error2, select = c("id_req", "id_frame", 
                                   "id_task", "desc_frameFam",
                                   "id_part", "num_estimate"))
  data2 <- unique(data2[,1:6])
  
  data3 <- subset(error3, select = c("id_req", "id_task", 
                                     "id_part", "num_estimate"))
  data3 <- unique(data3[,1:4])
  
  data4 <- subset(error4, select = c("id_frame", "id_task", 
                                     "id_part", "num_estimate"))
  data4 <- unique(data4[,1:4])
  
  data5 <- subset(error5, select = c("desc_frameFam", "id_task", 
                                     "id_part", "num_estimate"))
  data5 <- unique(data5[,1:4])
  
  file <- "Lead_Time_Estimate_Template.xlsx"
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
  saveWorkbook(wb, paste0(mli, "_Lead_Time_Estimate.xlsx"))
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

lapply(n, leadtime.analysis)
