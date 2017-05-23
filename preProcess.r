# administrative tables
#
#[1] "smh.adm.nophi.csv"         "smh.er.nophi.csv"          "smh.er_cacs.nophi.csv"    
#[4] "smh.er_consults.nophi.csv" "smh.er_diag.nophi.csv"     "smh.er_int.nophi.csv"     
#[7] "smh.ip_cmg.nophi.csv"      "smh.ip_dad.nophi.csv"      "smh.ip_diag.nophi.csv"    
#[10] "smh.ip_endo.nophi.csv"     "smh.ip_hig.nophi.csv"      "smh.ip_int.nophi.csv"     
#[13] "smh.ip_scu.nophi.csv"      "smh.ip_xfer.nophi.csv"  

ss <- list()

ss[[1]] <- c('EncID.new', 'Admitting.Service',  'Language')

ss[[2]] <- c('NACRSRegistrationNumber Triage.Time',  'Triage.Level',  
  'Date.of.Physician.Initial.Assessment', 'Time.of.Physician.Initial.Assessment',  
          'Disposition.Date', 'Disposition.Time',
          'Date.Left.ER',  'Time.Left.ER')

ss[[4]] <- c('ER.Consult.Occurrence',  'ER.Consult.Service')

ss[[5]] <- c('ER.Diagnosis.Code', 'ER.Diagnosis.Type')

ss[[7]] <- c('CMG', 'Comorbidity.Level')

ss[[8]] <- c('Admit.Date', 'Admit.Time', 'Discharge.Date', 'Discharge.Time',
             'Discharge.Disposition', 'Readmission', 'Gender', 'Age',
             'MostResponsible.DocterCode', 'MostResponsible.DocterService')

ss[[9]] <- c('Diagnosis.Code',  'Diagnosis.Type')

ss[[14]] <- c('Unit', 'Date.Check.in', 'Time.Check.in')
#################################################################

##             administrative

#################################################################
fileList <- list.files()
fileList <- fileList[grep("smh", fileList)]


adminDF <- data.frame()

for (i in 1:length(fileList)){
  filename <- fileList[i]
  xx <- read.csv(filename, header=T, na.strings = c("", "NA"))

  df <- data.frame(source=filename, colname = names(xx), selected= 0)
  
  selectedVar <- which(names(xx) %in% ss[[i]])
  
  
  df$selected[selectedVar] <- 1
  
  adminDF <- rbind(adminDF, df)
}

# 'testTables1_1.csv' is to be used by testTables.r to generate json file
write.csv(adminDF, file='testTables1_1.csv', row.names = F)

#################################################################

##             clinical

#################################################################

# setwd("C:/Users/Jennifer/Desktop/fornow/MRP/datasource/data/Clinical")
fileList <- list.files()

clinicalDF <- data.frame()

for (i in 1:length(fileList)){
  filename <- fileList[i]
  xx <- read.csv(filename, header=T, na.strings = c("", "NA"))
  
  df <- data.frame(source=filename, colname = names(xx), selected= 1)
  
  clinicalDF <- rbind(clinicalDF, df)
}

# 'testTables1_1.csv' is to be used by testTables.r to generate json file
write.csv(clinicalDF, file='testTables2_1.csv', row.names = F)
