options(java.parameters = "-Xmx8000m")
# set the working directory
# setwd("C:/Users/502689880/Desktop/FMI_Estimates/MLI_Lists/Hours")
setwd("C:/Users/502689880/Desktop/FMI_Estimates/MLI_Lists/Lead_Times")
library(xlsx)

# Get file names
file.names = list.files(pattern="xlsx$")

# Read them into a list
df.list = lapply(file.names, read.xlsx, sheetIndex=5, header=TRUE)

df = do.call(rbind, df.list)

write.xlsx(df, "Lead_Times_5.xlsx", sheetName="data", row.names=FALSE)

