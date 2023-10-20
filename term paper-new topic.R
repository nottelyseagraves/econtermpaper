rm(list=ls(all=TRUE));gc()
setwd("C:/Users/14048/OneDrive - Middle Tennessee State University/Documents/School/ECON4620")
packages <- c("tidyverse","rvest","AER","spdep","readxl","relaimpo","psych","openxlsx","pdfetch",
              "tseries","dplyr","readxl","psych","AER","car")
packages[!unlist(lapply(packages, require, character.only = TRUE))]
install.packages('car')
install.packages('Rtools')
library(pdfetch)
library(magrittr)
library(dplyr)
library(lubridate)
#


cz<- "UNRATE"
yz<-"CORESTICKM159SFRBATL"
az<-"AAA"
#adding
bz<-"GASREGW"
dz<-"GDP"
ez<-"DFF"



dim(ww<-pdfetch_FRED(c(cz,yz,az,bz,dz,ez)))
ww<-pdfetch_FRED(c(ez))

pdfetch_FRED(c(cz,yz,az,bz,dz,ez))
ww <- as.data.frame(ww) # convert to data frame

# convert the index to a column
ww <- ww %>%
  mutate(date = row.names(.)) %>% # create a new column for date
  subset(select = c(date, UNRATE, CORESTICKM159SFRBATL, AAA, GASREGW,GDP,DFF)) %>% # reorder the columns
  as.data.frame() # convert back to a data frame

ww <- ww %>%
  mutate(date = row.names(.)) %>% # create a new column for date
  subset(select = c(DFF)) %>% # reorder the columns
  as.data.frame() # convert back to a data frame


#making year cut off
ww$month <- month(as.Date(ww$date))
ww$fyear <- year(as.Date(ww$date))
ww <- ww[ww$fyear>1995, ]
ww <- ww[ww$fyear<=2005, ]

ww<- ww[ww$month == 12, ]

ww$day <- day(as.Date(ww$date))

replace_missing_values <- function(df, var_name) {
  # Create a copy of the input data frame
  df_copy <- df
  
  # Get the index of the first row with missing values
  missing_idx <- which(is.na(df_copy[, var_name]))
  
  # Replace missing values with values from the previous row
  for (i in missing_idx) {
    df_copy[i, var_name] <- df_copy[i - 1, var_name]
  }
  
  # Return the updated data frame
  return(df_copy)
}


ww <- replace_missing_values(ww, "GASREGW")


ww <- as.data.frame(ww) 

ww$fyear <- year(as.Date(ww$date))




#finding data for last day of month
ww<- ww[ww$day == 31, ]
write.xlsx(ww, file = "fred.xlsx")




#---------DATA IS GOOD ABOVE----------------------------------------


b<-data.frame(readxl::read_xlsx(path="compustatdata2.xlsx",sheet="data"))

names(b)[3]<-"fyear"
names(b)[16]<-"rev"
names(b)[15]<-"grossprofit"

b <- subset(b, select = c("gvkey","rev","fyear","grossprofit"))

bb <- merge(b, ww, by = c( "fyear"), all = TRUE)
# Create a new data frame without observations where revenue is 0
bb <- subset(bb, rev != 0)
bb$gpmargin <-bb$grossprofit / bb$rev
bb$gpmargin<-bb$grossprofit*100
write.xlsx(bb, file = "currentdata.xlsx")



bb<- na.omit(bb)
names(bb)

dim(bb<-bb[which(bb$gpmargin>=0),])

write.xlsx(bb, file = "termpaper_data.xlsx")


summary(aa <- lm(gpmargin ~ grossprofit+rev+UNRATE+AAA+CORESTICKM159SFRBATL+GDP+GASREGW, data = bb))


names(bb)

#now getting esg data
esg<-data.frame(readxl::read_xlsx(path="KLD_MSCI_data.xlsx",sheet="Sheet1"))
esg <- subset(esg, select = c("gvkey","fyear","csr","env","gov","soc"))
data <- merge(esg, bb, by = c("gvkey", "fyear"), all = TRUE)
data<- na.omit(data)
write.xlsx(data, file = "termpaperfinaldatab.xlsx")

data<-data.frame(readxl::read_xlsx(path="termpaperdata_final.xlsx",sheet="data"))
#-----------


data$gpmargin<-data$gpmargin*100


#getting average for csr score
avg <- mean(data$csr, na.rm = TRUE)
print(avg)

# Calculate the average CSR score
avg_csr <- mean(data$csr)

# Create a new data frame with CSR scores above the average
csr_above_avg <- subset(data, csr > avg_csr)
csr_below_avg <- subset(data, csr < avg_csr)
names(csr)

#unrestricted
summary(ur <- lm(gpmargin ~ grossprofit+rev+UNRATE+AAA+CORESTICKM159SFRBATL+GDP+GASREGW, data = data))
summary(ur_above <- lm( gpmargin ~ grossprofit+rev+UNRATE+AAA+CORESTICKM159SFRBATL+GDP+GASREGW, data = csr_above_avg))
summary(ur_under <- lm(gpmargin ~ grossprofit+rev+UNRATE+AAA+CORESTICKM159SFRBATL+GDP+GASREGW, data = csr_below_avg))
nrow(ur$model)

u<-data.frame(psych::describe(data[,colnames(ur$model)])) # using the describe function from the psych package, find descriptive statistics for every variable used in your unrestricted regression
u<-u[,c("n","mean","sd","min","max")] # here we select only the columns that we want
write.csv(u,file="descripterm.csv") # this writes the object u to a csv-format file called "descrip2.csv"


reset(ur)
vif(ur)
bgtest(ur) #H0:no autocorilation of residuals
ncvTest(ur)
#H0: homoskedastic residuals, high p valuse so accept 
coeftest(ur,vcov=hccm(ur))

r2p <- c(NA,calc.relimp(ur)$lmg)
zz<-round(data.frame(summary(ur)$coefficients,VIF=c(NA,vif(ur)),r2p),4)
write.csv(zz,"ur_termpaper.csv")



#CHECK IF CAN DROP VARIBLES
linearHypothesis(ur,c("GASREGW","AAA","CORESTICKM159SFRBATL"))
#high p value so accept
#we can drop them

summary(r <- lm(gpmargin ~ grossprofit+rev+UNRATE+GDP, data = data))
summary(r_above <- lm( gpmargin ~ grossprofit+rev+UNRATE+GDP, data = csr_above_avg))
summary(r_under <- lm(gpmargin ~ grossprofit+rev+UNRATE+GDP, data = csr_below_avg))
nrow(r$model)
nrow(r_above$model)
nrow(r_under$model)

reset(r)
vif(r)
bgtest(r) #H0:no autocorilation of residuals
ncvTest(r) #HO: heteroskaticity
ncvTest(r_above) #HO: heteroskaticity
coeftest(r,vcov=hccm(r))
coeftest(r_above,vcov=hccm(r_above))
ncvTest(r_under)
coeftest(r_under,vcov=hccm(r_under))

#restricted: not broken out
r2p <- c(NA,calc.relimp(r)$lmg)
zz<-data.frame(summary(r)$coefficients,VIF=c(NA,vif(r)),r2p)
write.csv(zz,"r_termpaper.csv")

#restricted: broken out
r2p <- c(NA,calc.relimp(r_under)$lmg)
zz<-data.frame(summary(r_under)$coefficients,VIF=c(NA,vif(r_under)),r2p)
write.csv(zz,"r_undertermpaper.csv")

#restricted: below
r2p <- c(NA,calc.relimp(r_above)$lmg)
zz<-data.frame(summary(r_above)$coefficients,VIF=c(NA,vif(r_above)),r2p)
write.csv(zz,"r_abovetermpaper.csv")

x<- as.data.frame(x)
write.csv(x,"r_hccmtermpaper.csv")

x <- as.data.frame(coef(xa))
write.csv(x,"ra_hccmtermpaper.csv")

x <- as.data.frame(coef(xu))
write.csv(x,"ru_hccmtermpaper.csv")

plot(csr_above_avg$grossprofitmargin, csr_above_avg$UNRATE)
plot(csr_above$grossprofitmargin, crs_above$UNRATE)

linearHypothesis(r,"grossprofit+rev+UNRATE+GDP=1",vcov=hccm(r)) # H0: constant returns to scale

#-----------------plots-------



plot(un_above)




#-----------
summary(aa <- lm(gpmargin ~ grossprofit+rev+UNRATE+AAA+CORESTICKM159SFRBATL+GDP, data = data))
summary(aa <- lm( gpmargin ~ grossprofit+rev+UNRATE+AAA+CORESTICKM159SFRBATL+GDP, data = csr_above_avg))
summary(aa <- lm(gpmargin ~ grossprofit+rev+UNRATE+AAA+CORESTICKM159SFRBATL+GDP, data = csr_below_avg))

summary(aa <- lm(gpmargin ~ grossprofit+rev+UNRATE+CORESTICKM159SFRBATL+GDP, data = data))
summary(aa <- lm( gpmargin ~ grossprofit+rev+UNRATE+CORESTICKM159SFRBATL+GDP, data = csr_above_avg))
summary(aa <- lm(gpmargin ~ grossprofit+rev+UNRATE+CORESTICKM159SFRBATL+GDP, data = csr_below_avg))



#do some work on logging rev and gross profit
data$log_gp <- log(data$grossprofit + 1)
#have negative values, so cant use

data$rev2<-data$rev^2
summary(aa <- lm(gpmargin ~ grossprofit+rev2+UNRATE+AAA+CORESTICKM159SFRBATL+GDP+GASREGW, data = data))

data$grossprofit2<-data$grossprofit^2
summary(aa <- lm(gpmargin ~ grossprofit2+rev+UNRATE+AAA+CORESTICKM159SFRBATL+GDP+GASREGW, data = data))
reset(aa)
#BETTER WITH NO ALTERATIONS

#__________________________________________________________
#Look then at individual score broken out:

names(data)
avg_env <- mean(data$env)

#ENV
env_above_avg <- subset(data, env > avg_env)
env_below_avg <- subset(data, env < avg_env)
summary(aa <- lm(gpmargin ~ grossprofit+rev+UNRATE+AAA+CORESTICKM159SFRBATL+GDP+GASREGW, data = data))
summary(aa <- lm( gpmargin ~ grossprofit+rev+UNRATE+AAA+CORESTICKM159SFRBATL+GDP+GASREGW, data = env_above_avg))
summary(aa <- lm(gpmargin ~ grossprofit+rev+UNRATE+AAA+CORESTICKM159SFRBATL+GDP+GASREGW, data = env_below_avg))

#soc
avg_soc <- mean(data$soc)
soc_above_avg <- subset(data, soc > avg_soc)
soc_below_avg <- subset(data, soc < avg_soc)
summary(aa <- lm(gpmargin ~ grossprofit+rev+UNRATE+AAA+CORESTICKM159SFRBATL+GDP+GASREGW, data = data))
summary(aa <- lm( gpmargin ~ grossprofit+rev+UNRATE+AAA+CORESTICKM159SFRBATL+GDP+GASREGW, data = soc_above_avg))
summary(aa <- lm(gpmargin ~ grossprofit+rev+UNRATE+AAA+CORESTICKM159SFRBATL+GDP+GASREGW, data = soc_below_avg))

#GOV
avg_gov <- mean(data$gov)
gov_above_avg <- subset(data, gov > avg_gov)
gov_below_avg <- subset(data, gov < avg_gov)
summary(aa <- lm(gpmargin ~ grossprofit+rev+UNRATE+AAA+CORESTICKM159SFRBATL+GDP+GASREGW, data = data))
summary(aa <- lm( gpmargin ~ grossprofit+rev+UNRATE+AAA+CORESTICKM159SFRBATL+GDP+GASREGW, data = gov_above_avg))
summary(aa <- lm(gpmargin ~ grossprofit+rev+UNRATE+AAA+CORESTICKM159SFRBATL+GDP+GASREGW, data = gov_below_avg))



write.xlsx(data, file = "termpaperdata.xlsx")


#~~~~~~~~~~~~~~~~~~~#stopped~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~






#for citatiopn
#https://www.evidenceinvestor.com/wp-content/uploads/2021/11/Foundations-of-ESG-Investing-Part-1.pdf
#https://www.aqr.com/Insights/Research/Journal-Article/Assessing-Risk-through-Environmental-Social-and-Governance-Exposures
https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2222740
#___________________________________________________________

#CHANGE IT TO BE INLY DATES INTERSED AND USE SOME KINDA IF STATEMENT TO SAY IF FYEAR = THIS DATE.. THEN FRED VARIBLE = THIS

AND SO FORTH


c<-data.frame(readxl::read_xlsx(path="compustatdata2.xlsx",sheet="Sheet1"))
names(c)[3] <- "fyear"

data <- merge(ww, c, by = c( "fyear"), all = TRUE)
names(data)
#remove duplicate rows
data <- data[!duplicated(data), ]
data <- data[data$fyear>1995, ]
data <- data[data$fyear<=2005, ]

data <- data[data$Revenue.Total != 0, ]
data$gpmargin <-data$GrossProfit / data$Revenue.Total
data$gpmargin <-data$gpmargin 

summary(zz <- lm(gpmargin ~ UNRATE +CORESTICKM159SFRBATL+AAA+Revenue.Total, data = data))
names(bb)


bb <- subset(data, select = c("fyear","UNRATE","CORESTICKM159SFRBATL","gvkey","gpmargin","AAA"))
write.xlsx(bb, file = "fred.xlsx")

#-------------add othe varibles from compustat data



data <- data[complete.cases(data$gpmargin, data$UNRATE, data$CORESTICKM159SFRBATL, data$AAA), ]

print(sum(!is.na(data$gpmargin)))
print(sum(!is.na(data$UNRATE)))
print(sum(!is.na(data$CORESTICKM159SFRBATL)))
print(sum(!is.na(data$TWEXBGSMTH)))

# remove missing or infinite values in the variables
data <- data[is.finite(data$gpmargin) & is.finite(data$UNRATE) & is.finite(data$CORESTICKM159SFRBATL) & is.finite(data$TWEXBGSMTH), ]

# fit a linear regression model
zz <- lm(gpmargin ~ UNRATE + CORESTICKM159SFRBATL+AAA, data = data)

# print the summary of the model
summary(zz)
data <- data[is.finite(data$gpmargin) & is.finite(data$UNRATE) & is.finite(data$CORESTICKM159SFRBATL) & is.finite(data$TWEXBGSMTH), ]

summary(zz <- lm(gpmargin ~ UNRATE +CORESTICKM159SFRBATL+AAA, data = data))
data$gpmargin <- as.numeric(data$gpmargin)
data$UNRATE <- as.numeric(data$UNRATE)
data$CORESTICKM159SFRBATL <- as.numeric(data$CORESTICKM159SFRBATL)
data$TWEXBGSMTH <- as.numeric(data$TWEXBGSMTH)


names(datab)
datab$avgesg <- mean(datab$csr)
avg <- mean(datab$csr, na.rm = TRUE)



#removing any missing or infinite data
data <- na.omit(data)

#remove duplicate rows
data <- data[!duplicated(data), ]

#remove 
datab$above_avg_dummy <- as.numeric(datab$crs > avg)
