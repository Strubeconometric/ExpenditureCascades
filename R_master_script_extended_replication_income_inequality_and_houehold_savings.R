# important information ####

# please create a parent folder (name does not matter) with the following folder structure:

# data_raw <- download raw data sets and save them here
# data_tidy <- merged and cleaned data sets will be saved as a single tidy panel data set here
# output <- descriptive and result tables will be saved here

# the directory containing the three foldrs must be set as the current working directory or contain a here() or R project file

# not all econometric results are saved in the output folder
# if you are interested in robustness and statistical assumption checks please run the code from chunk to chunk and read the commented results

# change settings #####

# set error message language to english
Sys.setenv(LANG = "en")


# install packages ####
# packages have to be installed only once - remove #'s in RStudio with -> Code -> Comment/Uncomment Lines

# install.packages("tidyverse")
# install.packages("here")
# install.packages("readxl")
# install.packages("ggplot2")
# install.packages("data.table")
# install.packages("magrittr")
# install.packages("countrycode")
# install.packages("plm")
# install.packages("stargazer")
# install.packages("lmtest")
# install.packages("sandwich")
# install.packages("tseries")
# install.packages("systemfit")
# install.packages("ggeffects")

# load packages #####

# packages need to be loaded for every new session
library(tidyverse)
library(here)
library(readxl)
library(ggplot2)
library(data.table)
library(magrittr)
library(countrycode)
library(plm)
library(stargazer)
library(lmtest)
library(sandwich)
library(tseries)
library(systemfit)
library(ggeffects)

# test if here.package works getwd() = here() -> path should link to parent folders containing the three mentioned folders
here()

# import and merge data sets into a tidy panel format ####

# import WID data set

WID_data_path=here("data_raw/WID_Data.csv")
WID_data=fread(WID_data_path, colClasses = "double", na.strings = "..")

# delete and rename variables

WID_data[,(2):=NULL]

WID_data %>% setnames(., old = c(1,2,3), new = c("Year","Country","Country_Code"))

# filter for high income oecd countries

WID_data = WID_data %>% subset(., Country %in% c("Australia", "Austria", "Belgium", "Canada", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Japan",
                                                 "Korea, Rep.", "Luxembourg", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic",
                                                 "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", "United States"))

# create vector of high income oecd countries as reference for other data sets

Country_Codes_WID_Vector = WID_data[, Country_Code]
Country_Codes_WID_Vector = unique(unlist(strsplit(Country_Codes_WID_Vector, " ")))

# import oecd data sets, name and delete variables, adjust data types

OECD_current_account_balance=fread("data_raw/Current_Account_Balance_OECD.csv", colClasses = "double", na.strings = "..")

OECD_current_account_balance %>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","Current_Account_Balance"))

OECD_current_account_balance = OECD_current_account_balance %>% subset(., Country_Code %in% Country_Codes_WID_Vector)

OECD_current_account_balance=OECD_current_account_balance[, c("Country_Code", "Year", "Current_Account_Balance")]

OECD_current_account_balance = OECD_current_account_balance %>%  
  mutate(Year = as.character(Year))

# create condensed code with pipes and import the remaining data sets

gdp=fread("data_raw/gdp.csv", colClasses = "double", na.strings = "..")

gdp <- gdp[TRANSACT == "B1_GS1"]
gdp <- gdp[Measure == "Current prices"] 

gdp %<>% setnames(., old = c(1,8,15), new = c("Country_Code","Year","gdp")) %>%
  subset(Country_Code %in% Country_Codes_WID_Vector) %>%
  .[,c(2,3,4,5,6,7,9,10,11,12,13,14,16,17):=NULL] %>%
  mutate(Year = as.character(Year))

net_lending=fread("data_raw/net_lending.csv", colClasses = "double", na.strings = "..")

net_lending <- net_lending[TRANSACT == "B9S1"]
net_lending <- net_lending[Measure == "Current prices"] 

net_lending %<>% setnames(., old = c(1,8,15), new = c("Country_Code","Year","net_lending")) %>%
  subset(Country_Code %in% Country_Codes_WID_Vector) %>%
  .[,c(2,3,4,5,6,7,9,10,11,12,13,14,16,17):=NULL] %>%
  mutate(Year = as.character(Year))

OECD_fiscal_balance = merge(gdp, net_lending,  all = TRUE, by = c("Country_Code", "Year"))

OECD_fiscal_balance %<>% 
  mutate(Fiscal_Account_Balance = net_lending/gdp*100)

OECD_fiscal_balance<-OECD_fiscal_balance[,c(3,4):=NULL]


OECD_GDP_per_capita=fread("data_raw/GDP_Per_Capita_OECD.csv", colClasses = "double", na.strings = "..")

OECD_GDP_per_capita %<>% setnames(., old = c(1,7,15), new = c("Country_Code","Year","GDP_Per_Capita")) %>%
  subset(Country_Code %in% Country_Codes_WID_Vector) %>%
  .[,c(2,3,4,5,6,8,9,10,11,12,13,14,16,17):=NULL] %>%
  mutate(Year = as.character(Year))


OECD_house_prices=fread("data_raw/House_Prices_OECD.csv", colClasses = "double", na.strings = "..")

OECD_house_prices %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","House_Prices")) %>%
  subset(Country_Code %in% Country_Codes_WID_Vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.character(Year))

OECD_household_savings=fread("data_raw/Household_Savings_OECD.csv", colClasses = "double", na.strings = "..")

OECD_household_savings %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","Household_Savings")) %>%
  subset(Country_Code %in% Country_Codes_WID_Vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.character(Year))

OECD_Inflation=fread("data_raw/Inflation_OECD.csv", colClasses = "double", na.strings = "..")

OECD_Inflation %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","Inflation")) %>%
  subset(Country_Code %in% Country_Codes_WID_Vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.character(Year))

OECD_Interest_Rate=fread("data_raw/Interest_Rate_OECD.csv", colClasses = "double", na.strings = "..")

OECD_Interest_Rate %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","Interest_Rate")) %>%
  subset(Country_Code %in% Country_Codes_WID_Vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.character(Year))

OECD_National_Savings=fread("data_raw/National_Savings_OECD.csv", colClasses = "double", na.strings = "..")

OECD_National_Savings %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","National_Savings")) %>%
  subset(Country_Code %in% Country_Codes_WID_Vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.character(Year))

OECD_Disposable_Income_Per_Capita=fread("data_raw/Per_Capita_Disposable_Income_OECD.csv", colClasses = "double", na.strings = "..")

OECD_Disposable_Income_Per_Capita %<>% setnames(., old = c(1,7,15), new = c("Country_Code","Year","Disposable_Income_Per_Capita")) %>%
  subset(Country_Code %in% Country_Codes_WID_Vector) %>%
  .[,c(2,3,4,5,6,8,9,10,11,12,13,14,16,17):=NULL] %>%
  mutate(Year = as.character(Year))

OECD_Real_Disposable_Income_Per_Capita=fread("data_raw/Real_Disposable_Income_Per_Capita_OECD.csv", colClasses = "double", na.strings = "..")

OECD_Real_Disposable_Income_Per_Capita %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","Real_Disposable_Income_Per_Capita")) %>%
  subset(Country_Code %in% Country_Codes_WID_Vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.character(Year))

OECD_Share_Prices=fread("data_raw/Share_Prices_OECD.csv", colClasses = "double", na.strings = "..")

OECD_Share_Prices %<>% setnames(., old = c(1,6,7), new = c("Country_Code","Year","Share_Prices")) %>%
  subset(Country_Code %in% Country_Codes_WID_Vector) %>%
  .[,c(2,3,4,5,8):=NULL] %>%
  mutate(Year = as.character(Year))

# import and clean swiid9 data set

SWIID9_Data=fread("data_raw/swiid9_1_summary.csv", colClasses = "double", na.strings = "..")

SWIID9_Data %<>% setnames(., old = c(1,2), new = c("Country","Year")) %>%
  subset(Country %in% c("Australia", "Austria", "Belgium", "Canada", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Japan",
                        "Korea, Rep.", "Luxembourg", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic",
                        "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", "United States")) %>%
  mutate(Year = as.character(Year))

SWIID9_Data[,"Country_Code"]=countrycode(SWIID9_Data[[1]], origin = "country.name", destination = "iso3c")


# import and clean witd data set

WTID_Data=fread("data_raw/WTID_Data.csv", colClasses = "double", na.strings = "..")

WTID_Data[,"Country_Code"]=countrycode(WTID_Data[[1]], origin = "country.name", destination = "iso3c")

WTID_Data=mutate(WTID_Data, Year = as.character(Year))

# import and clean finreform data set

Finreform=fread("data_raw/Finreform.csv", colClasses = "double", na.strings = "..")

Finreform[,"Country_Code"]=countrycode(Finreform[[1]], origin = "country.name", destination = "iso3c")

Finreform <- select(Finreform, Country_Code, year, finreform, finreform_n)

Finreform <- setnames(Finreform, "year", "Year")

Finreform=mutate(Finreform, Year = as.character(Year))

# control merge for two data sets

test_verbund = merge(WID_data_tidy, OECD_current_account_balance_tidy,  all = TRUE, by = c("Country_Code", "Year"))


# merge all data sets and create tidy panel data table

Data = Reduce(function(x,y) merge(x = x, y = y, by = c("Country_Code", "Year"), all = TRUE), list(
  OECD_current_account_balance, OECD_Disposable_Income_Per_Capita,OECD_fiscal_balance, OECD_GDP_per_capita, OECD_house_prices, OECD_household_savings,
  OECD_Inflation, OECD_Interest_Rate, OECD_National_Savings, OECD_Real_Disposable_Income_Per_Capita, OECD_Share_Prices, WID_data, WTID_Data, SWIID9_Data,Finreform))

Data[,c(35):=NULL]

Data[,"Country"]=countrycode(Data[[1]], origin = "iso3c", destination = "country.name")

Data = Data %>% subset(., Country_Code %in% Country_Codes_WID_Vector)

# save data set
write.csv(Data, "data_tidy/Data_Tidy.csv", row.names = FALSE )

# clean environment 

rm(list = ls())

# import function 
Data=fread("data_tidy/Data_Tidy.csv", colClasses = "double", na.strings = "..")

# change format to data frame
Data=setDF(Data)

# set data type to numeric
Data[,3:36] = sapply(Data[,3:36],as.numeric)
Data[,37:47] = sapply(Data[,37:47],as.numeric)

# set object type to data.table
Data = as.data.table(Data)

# delete unnecessary column
Data[,c(14):=NULL]

# change variable names
Data = setnames(Data, old = c(4,6,10, 12, 13, 14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,37,38,39,40,41,42,43,44,45,46), 
                new = c("Dis_Income","GDP", "Nominal_Interest_Rate", "Disposable_Income", "Nominal_Share_Prices", "Net_Lending", "Credit_Index","Domestic_Credit_by_Financial_Sector","Private_Credit",
                        "Domestic_Credit_to_Private_Credit_by_Banks", "Monetary_Sector_Credit_to_Private_Sector", "Gini_WB",
                        "Gross_Savings", "Gross_Domestic_Savings", "Income_Share_Fourth_20", "Income_Share_Top_10", "Income_Share_Top_20",
                        "Income_Share_Lowest_10", "Income_Share_Lowest_20", "Income_Share_Second_20", "Income_Share_Third_20",
                        "Dependency_Ratio", "Inflation_CPI", "Inflation_GDP_Deflator", "Inflation_GDP_Defaltor_Linked", "Top_10", "Top_1", 
                        "Gini_SWIID9","Gini_SWIID9_se", "Gini_SWIID9_Market", "Gini_SWIID9_Market_se", "Absolute_Redistribution", "Absolute_Redistribution_se",
                        "Relative_Redistribution", "Relative_Redistribution_se", "Finreform", "Finreform_n"
                ))

# create factors / idcode / numbers for each country
Data[, idcode := .GRP, by=.(Country_Code)]

# rearrange variable order
Data = setcolorder(Data, c(47,2:46,1))

# change idcode datatype to factor
Data[,1] = lapply(Data[,1],as.factor)

# delete unneseccary column
Data[,c(36):=NULL]

# create growth rates / deltas
Data$Delta_Disposable_Income <- with(Data, ave(Disposable_Income , idcode, FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))
Data$Delta_House_Prices <- with(Data, ave(House_Prices , idcode, FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))



# create first differences
# function to create first differences 
# dat %>%  group_by(id) %>% mutate(time.difference = time - lag(time))
Data %<>% 
  group_by(idcode) %>% 
  mutate(Interest_Rate = Nominal_Interest_Rate - Inflation)
Data %<>% 
  group_by(idcode) %>% 
  mutate(Share_Prices = Nominal_Share_Prices - Inflation)
Data %<>% 
  group_by(idcode) %>% 
  mutate(Top_1Percent = Top_1*100) %>%
  mutate(Top_10Percent = Top_10*100)

# optional conversion function to pdata.frame class for plm tests
# Data = pdata.frame(Data, index=c("idcode", "Year"))


# descriptive statistics ####

# select variables
descriptive_data <- select(Data, "Year", "Household_Savings", "Gini_SWIID9", "Top_1Percent", "Top_10Percent",
                           "Dependency_Ratio", "Disposable_Income", "Interest_Rate",
                           "Fiscal_Account_Balance", "GDP", "Inflation", "Share_Prices",
                           "House_Prices", "Private_Credit", "Finreform")
# store selected variables
descriptive_data<-as.data.frame(descriptive_data)
# descriptive_data<-na.omit(descriptive_data)
descriptive_data[,1] = sapply(descriptive_data[,1],as.numeric)

# create descriptive statistics table in word format
stargazer(descriptive_data,
          digits=2,
          title="Descriptive Statistics",
          type = "html",
          out="output/descriptive.doc",
          
          
          covariate.labels = c("Idcode (Countries)", "Year", "Household Savings Rate", "Gini Coefficient", "Top 1 %", "Top 10%",
                               "Old-Age Dependency", "Disposable Income", "Interest Rate", "Fiscal Account Balance", "GDP",
                               "Inflation", "Share Prices", "House Prices", "Private Credit", "Finreform") 
)

ggplot(Data, aes(x=Household_Savings)) +
  geom_histogram()

# visualise possible functional forms of single groups
m1 <- lm(Household_Savings ~ Gini_SWIID9 + idcode, data=Data )
m2 <- lm(Household_Savings ~ Gini_SWIID9 + I(Gini_SWIID9^2) + idcode, data=Data )
m3 <- lm(Household_Savings ~ Gini_SWIID9 + I(Gini_SWIID9^2) + I(Gini_SWIID9^3) + idcode, data=Data )

p1 <- ggpredict(m1, terms=c("Gini_SWIID9", "idcode")) %>% 
  mutate(form="linear") %>% 
  rename("Gini_SWIID9" = "x", 
         "idcode" = "group", 
         "Household_Savings" = "predicted")
p2 <- ggpredict(m2, terms=c("Gini_SWIID9", "idcode")) %>% 
  mutate(form="quadratic") %>% 
  rename("Gini_SWIID9" = "x", 
         "idcode" = "group", 
         "Household_Savings" = "predicted")
p3 <- ggpredict(m3, terms=c("Gini_SWIID9", "idcode")) %>% 
  mutate(form="cubic") %>% 
  rename("Gini_SWIID9" = "x", 
         "idcode" = "group", 
         "Household_Savings" = "predicted")

ggplot() + 
  geom_line(data=p1, aes(x=Gini_SWIID9, y=Household_Savings, colour="linear")) + 
  geom_line(data=p2, aes(x=Gini_SWIID9, y=Household_Savings, colour="quadratic")) + 
  geom_line(data=p3, aes(x=Gini_SWIID9, y=Household_Savings, colour="cubic")) + 
  geom_point(data=Data, aes(x=Gini_SWIID9, y=Household_Savings)) + 
  facet_wrap(~idcode) + 
  theme_bw() + 
  labs(colour="Functional\nForm")


#sapply(descriptive_data, max, na.rm = TRUE)

# replication of "Income Distribution and Aggregate Saving: A Non‐Monotonic Relationship" by Bofinger and Scheuermeyer.####

# y = Household_Savings
# x = Gini_SWIID9+ I(Gini_SWIID9^2)
# x-controls = Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) + Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit

# fixed effects estimations

# linear fixed effects panel estimation
fe_linear <- plm(Household_Savings~Gini_SWIID9 + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                   Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "within")
# saving robust standard errors for output table
fe_linear_robust_se <- vcovHC(fe_linear,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()
# quadratic fixed effects panel estimation with robust standard erros
fe_quadratic <- plm(Household_Savings~Gini_SWIID9+ I(Gini_SWIID9^2) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                      Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "within")
# saving robust standard errors for output table
fe_quadratic_robust_se <- vcovHC(fe_quadratic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()
# cubic fixed effects panel estimation with robust standard erros
fe_cubic <- plm(Household_Savings~Gini_SWIID9+ I(Gini_SWIID9^2) + I(Gini_SWIID9^3) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                  Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "within")
# saving robust standard errors for output table
fe_cubic_robust_se <- vcovHC(fe_cubic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled ols estimations

# linear pooled ols panel estimation 
pooled_linear <- plm(Household_Savings~Gini_SWIID9 + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                       Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "pooling")
# saving robust standard errors for output table
pooled_linear_robust_se <- vcovHC(pooled_linear,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()
# quadratic pooled ols panel estimation 
pooled_quadratic <- plm(Household_Savings~Gini_SWIID9+ I(Gini_SWIID9^2) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                          Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "pooling")
# saving robust standard errors for output table
pooled_quadratic_robust_se <- vcovHC(pooled_quadratic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()
# cubic pooled ols panel estimation 
pooled_cubic <- plm(Household_Savings~Gini_SWIID9+ I(Gini_SWIID9^2) + I(Gini_SWIID9^3) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                      Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "pooling")
# saving robust standard errors for output table
pooled_cubic_robust_se <- vcovHC(pooled_cubic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# output table for paper replication in word format
stargazer(
  
  fe_linear, fe_quadratic, fe_cubic,  pooled_linear, pooled_quadratic, pooled_cubic, # choose models
  
  se = list(fe_linear_robust_se, fe_quadratic_robust_se, fe_cubic_robust_se,
            pooled_linear_robust_se, pooled_quadratic_robust_se, pooled_cubic_robust_se), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Panel Data Models Based on the Gini Coefficients from the SWIID9", # name table
  
  out="output/paper_replication.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("FE", "FE", "FE","POLS", "POLS", "POLS"), # name columns with model types
  
  covariate.labels = c("Gini", "Gini Quadratic", "Gini Cubic", "Old Age Dependency Ratio", "Growth of Disposable Income",
                       "Interest Rate", "Fiscal Account Balance", " log(GDP)", "Inflation", "Share Prices",
                       "Growth of House Prices", "Private Credit"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "Robust standard errors in parentheses: *p<0.01**p<0.001***p<0.0001", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)

# gauss-markov-/statistical assumption tests for panel and time series data ####

# function for balancing has to be defined because some panel data tests assume balanced data frames
balanced<-function(data, ID, TIME, VARS, required=c("all","shared")) {
  if(is.character(ID)) {
    ID <- match(ID, names(data))
  }
  if(is.character(TIME)) {
    TIME <- match(TIME, names(data))
  }
  if(missing(VARS)) { 
    VARS <- setdiff(1:ncol(data), c(ID,TIME))
  } else if (is.character(VARS)) {
    VARS <- match(VARS, names(data))
  }
  required <- match.arg(required)
  idf <- do.call(interaction, c(data[, ID, drop=FALSE], drop=TRUE))
  timef <- do.call(interaction, c(data[, TIME, drop=FALSE], drop=TRUE))
  complete <- complete.cases(data[, VARS])
  tbl <- table(idf[complete], timef[complete])
  if (required=="all") {
    keep <- which(rowSums(tbl==1)==ncol(tbl))
    idx <- as.numeric(idf) %in% keep
  } else if (required=="shared") {
    keep <- which(colSums(tbl==1)==nrow(tbl))
    idx <- as.numeric(timef) %in% keep
  }
  data[idx, ]
}

# tests for functional forms / prevention of model misspecification

# visualise possible functional forms

# Gini
ggplot(Data, aes(x=Gini_SWIID9, y=Household_Savings)) + 
  geom_point() + 
  geom_smooth(aes(colour="linear", fill="linear"), 
              method="lm", 
              formula=y ~ x, ) + 
  geom_smooth(aes(colour="quadratic", fill="quadratic"), 
              method="lm", 
              formula=y ~ x + I(x^2)) + 
  geom_smooth(aes(colour="cubic", fill="cubic"), 
              method="lm", 
              formula=y ~ x + I(x^2) + I(x^3)) + 
  scale_fill_brewer(palette="Set1") + 
  scale_colour_brewer(palette="Set1") + 
  theme_classic() + 
  labs(colour="Functional Form", fill="Functional Form") +
  #ggtitle("Gini Coefficient and Household Savings Rate") +
  xlab("Gini Coefficient in %") + ylab("Household Savings Rate in %")

# Top 1%
ggplot(Data, aes(x=Top_1Percent, y=Household_Savings)) + 
  geom_point() + 
  geom_smooth(aes(colour="linear", fill="linear"), 
              method="lm", 
              formula=y ~ x, ) + 
  geom_smooth(aes(colour="quadratic", fill="quadratic"), 
              method="lm", 
              formula=y ~ x + I(x^2)) + 
  geom_smooth(aes(colour="cubic", fill="cubic"), 
              method="lm", 
              formula=y ~ x + I(x^2) + I(x^3)) + 
  scale_fill_brewer(palette="Set1") + 
  scale_colour_brewer(palette="Set1") + 
  theme_classic() + 
  labs(colour="Functional Form", fill="Functional Form") +
  #ggtitle("Top 1% Income and Household Savings Rate") +
  xlab("Top 1% Income Share in %") + ylab("Household Savings Rate in %")


# Top 10%
ggplot(Data, aes(x=Top_10Percent, y=Household_Savings)) + 
  geom_point() + 
  geom_smooth(aes(colour="linear", fill="linear"), 
              method="lm", 
              formula=y ~ x, ) + 
  geom_smooth(aes(colour="quadratic", fill="quadratic"), 
              method="lm", 
              formula=y ~ x + I(x^2)) + 
  geom_smooth(aes(colour="cubic", fill="cubic"), 
              method="lm", 
              formula=y ~ x + I(x^2) + I(x^3)) + 
  scale_fill_brewer(palette="Set1") + 
  scale_colour_brewer(palette="Set1") + 
  theme_classic() + 
  labs(colour="Functional Form", fill="Functional Form") +
  #ggtitle("Top 10% Income and Household Savings Rate") +
  xlab("Top 10% Income Share in %") + ylab("Household Savings Rate in %")

# tests for deterministic/stochastic trends, weak dependency, stationary process

# Household savings 

# unbalanced test includes all years in the data set
ggplot(Data, mapping = aes(x = Year, y = Household_Savings)) +
  geom_line(aes(linetype = as.factor(idcode)))
# balanced test includes only common years for each variable which will later be used in the model
Data_without_NA<-Data[-which(is.na(Data$Household_Savings)),]
Data_without_NA<-Data_without_NA[, c("idcode","Year","Household_Savings")]
Data_without_NA_balanced<-balanced(Data_without_NA, "idcode","Year", required="shared")
Data_without_NA[,1] = sapply(Data_without_NA[,1],as.numeric)
Data_without_NA_balanced_split = data.frame(split(Data_without_NA_balanced$Household_Savings, Data_without_NA_balanced$idcode))
matplot(y = Data_without_NA_balanced_split, type = 'l', lty = 1)

# Levin-Lin-Chu Unit-Root Test
# test requires a dataset where the tested variable is stored in different lists for each group
#HO: there is unit root in variable or variable is non stationary
#H1: there is no unit root and variable is stationary 
purtest(Data_without_NA_balanced_split, pmax=2, exo = "intercept", test = "levinlin")
# stationarity can be confirmed for Household Savings

# Augmented Dickey-Fuller Test
# test requires dataset without NA's
#H0: no stationary process
#H1: stationary process
#k defines number of lags tested
adf.test(Data_without_NA$Household_Savings, k = 6)
# stationarity can be confirmed

# Gini

# unbalanced test includes all years in the data set
ggplot(Data, mapping = aes(x = Year, y = Gini_SWIID9)) +
  geom_line(aes(linetype = as.factor(idcode)))
# balanced test includes only common years in the data set
Data_without_NA<-Data[-which(is.na(Data$Gini_SWIID9)),]
Data_without_NA<-Data_without_NA[, c("idcode","Year","Gini_SWIID9")]
Data_without_NA_balanced<-balanced(Data_without_NA, "idcode","Year", required="shared")
Data_without_NA_balanced[,1] = sapply(Data_without_NA_balanced[,1],as.numeric)
Data_without_NA_balanced_split <- as.data.frame(split(Data_without_NA_balanced$Gini_SWIID9, Data_without_NA_balanced$idcode))
matplot(y = Data_without_NA_balanced_split, type = 'l', lty = 1)

# Levin-Lin-Chu Unit-Root Test for Gini
# test requires a dataset where the tested variable is stored in different lists for each group
#HO: there is unit root in variable or variable is non stationary
#H1: there is no unit root and variable is stationary 
purtest(Data_without_NA_balanced_split, pmax=2, exo = "intercept", test = "levinlin")
# stationarity can be confirmed for Gini

# Augmented Dickey-Fuller Test for Gini
# test requires dataset without NA's
#H0: no stationary
#H1: stationary
#k defines number of lags tested
adf.test(Data_without_NA$Gini_SWIID9, k = 6)
# stationarity can be confirmed

# Top 1%

# unbalanced test includes all years in the data set
ggplot(Data, mapping = aes(x = Year, y = Top_1Percent)) +
  geom_line(aes(linetype = as.factor(idcode)))
# unbalanced test includes all years in the data set
Data_without_NA<-Data[-which(is.na(Data$Top_1Percent)),]
Data_without_NA<-Data_without_NA[, c("idcode","Year","Top_1Percent")]
Data_without_NA_balanced<-balanced(Data_without_NA, "idcode","Year", required="shared")
Data_without_NA_balanced[,1] = sapply(Data_without_NA_balanced[,1],as.numeric)
Data_without_NA_balanced_split <- as.data.frame(split(Data_without_NA_balanced$Top_1Percent, Data_without_NA_balanced$idcode))
matplot(y = Data_without_NA_balanced_split, type = 'l', lty = 1)

# Levin-Lin-Chu Unit-Root Test for Top 1 %
# test requires a dataset where the tested variable is stored in different lists for each group
#HO: there is unit root in variable or variable is non stationary
#H1: there is no unit root and variable is stationary 
purtest(Data_without_NA_balanced_split, pmax=2, exo = "intercept", test = "levinlin")
# stationarity can be confirmed

# Augmented Dickey-Fuller Test for Top 1 %
# test requires dataset without NA's
#H0: no stationary
#H1: stationary
#k defines number of lags tested
adf.test(Data_without_NA$Top_1Percent, k = 6)
# stationarity can be confirmed

# Top 10%

# unbalanced test includes all years in the data set
ggplot(Data, mapping = aes(x = Year, y = Top_10Percent)) +
  geom_line(aes(linetype = as.factor(idcode)))
# unbalanced test includes all years in the data set
Data_without_NA<-Data[-which(is.na(Data$Top_10Percent)),]
Data_without_NA<-Data_without_NA[, c("idcode","Year","Top_10Percent")]
Data_without_NA_balanced<-balanced(Data_without_NA, "idcode","Year", required="shared")
Data_without_NA_balanced[,1] = sapply(Data_without_NA_balanced[,1],as.numeric)
Data_without_NA_balanced_split <- as.data.frame(split(Data_without_NA_balanced$Top_10Percent, Data_without_NA_balanced$idcode))
matplot(y = Data_without_NA_balanced_split, type = 'l', lty = 1)

# Levin-Lin-Chu Unit-Root Test for Top 10 %
# test requires a dataset where the tested variable is stored in different lists for each group
#HO: there is unit root in variable or variable is non stationary
#H1: there is no unit root and variable is stationary 
purtest(Data_without_NA_balanced_split, pmax=2, exo = "intercept", test = "levinlin")
# stationarity can be confirmed

# Augmented Dickey-Fuller Test for Top 10 %
# test requires dataset without NA's
#H0: no stationary
#H1: stationary
#k defines number of lags tested
adf.test(Data_without_NA$Top_10Percent, k = 6)
# stationarity can be confirmed

# tests for serial correlation / autocorrelation

#estimate model for test input
fe_linear <- plm(Household_Savings~Top_1Percent + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                   Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "within")

#Durbin Watson test
#H0: There is no autocorrelation / serial correlation in error term.
#H1: There is autocorrelation / serial correlation in error term.
pdwtest(Household_Savings~Top_1Percent + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
          Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "within")

#wooldridge test 
pbgtest(fe_linear)

# both tests confirm autocorrelation

# tests for homoskedasticity / heteroskedasticity

#Breusch-Pagan Test
#HO: There is homoskedasticity
#H1: There is heteroskedasticity
bptest(Household_Savings~Top_1Percent + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
         Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, studentize = F)

# test confirms heteroskedasticity

# robust standard errors are necessary

#Robust Standard Erros for heteroskedasticity and autocorrelation
#if there is heteroscedasticity
coeftest(fe_linear, vcovHC)
#if there is heteroskedascticity and autocorrelation
coeftest(fe_linear, vcovHC(fe_linear, method = "arellano"))

# tests for multicollinearity 

# variance inflation factor function needs pooled ols model as input for panel data test
pooled_linear <- plm(Household_Savings~Top_1Percent + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                       Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "pooling")
vif(pooled_linear)

# low variance inflation factor (no factor over 2), no multicollinearity induced problems expected

#Tests for cross sectional dependence in Panel data
# problem for macro panels with long time series

#Breusch-Pagan LM test for cross sectional dependence
#H0: there is no cross sectional dependence
#H1: there is cross sectional dependence

pcdtest(fe_linear, test = c("lm"))

#Pesaran CD test for cross sectional dependence
#H0: there is no cross sectional dependence
#H1: there is cross sectional dependence

# cross sectional dependence is confirmed by both tests

pcdtest(fe_linear, test = c("cd"))

# test model with cross sectional dependence robust standard errors
# PCSE if N>T
summary(fe_linear, vcov = function(x) vcovBK(x, type="HC1", cluster = c("group")))

# FGLS if T>N
test<-pggls(Household_Savings~Top_1Percent + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) + Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "within")
summary(test)

# explanatory coefficient is still statistically significant

# model comparison tests ####

# the following estimations are needed as inputs 
fe_linear <- plm(Household_Savings~Top_1Percent + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                  Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "within")
re_linear <- plm(Household_Savings~Top_1Percent + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                  Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "random")
pooled_linear <- plm(Household_Savings~Top_1Percent + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                  Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "pooling")                
#Hausman test
#H0: random effect model is consistent
#H1: fixed effect model is consistent

phtest(fe_linear,re_linear)   

#pooled ols test
#HO: the same coefficients apply to each individual (pooled ols is stable)
#h1: the same coefficients do not apply to each individual (pooled ols is unstable)

pooltest(Household_Savings~Top_1Percent, data = Data,  model = "within")


#test for individual effects and time effects
#H0: no significant individual effects and time effects
#H1: significant individual effects and time effects

plmtest(Household_Savings~Top_1Percent, data=Data,
        effect = "twoways", type="ghm")

#test on consistency between pooled ols and fixed effects model
#H0: pooled ols is consistent
#H1: fixed effect model is consistent
pFtest(fe_linear, pooled_linear)

#test for individual effects and time effects
#H0: no significant individual effects and time effects
#H1: significant individual effects and time effects

plmtest(Household_Savings~Gini_SWIID9 + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
          Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data,
        effect = "twoways", type="ghm")

#test for individual effects
#H0: no significant individual effects
#H1: significant individual effects

plmtest(Household_Savings~Gini_SWIID9 + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
          Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data,
        effect = "individual", type="honda")

#test for time effects
#H0: no significant time effects
#H1: significant time effects

plmtest(Household_Savings~Gini_SWIID9 + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
          Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data,
        effect = "time", type="honda")

# all test results point towards group fixed effects models

# term paper estimations ####

# fixed effects estimations

# linear fixed effects panel estimation
fe_linear <- plm(Household_Savings~Top_1Percent + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                   Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "within")
# saving robust standard errors for output table
fe_linear_robust_se <- vcovHC(fe_linear,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# quadratic fixed effects panel estimation with robust standard erros
fe_quadratic <- plm(Household_Savings~Top_1Percent+ I(Top_1Percent^2) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                      Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "within")
# saving robust standard errors for output table
fe_quadratic_robust_se <- vcovHC(fe_quadratic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# cubic fixed effects panel estimation with robust standard erros
fe_cubic <- plm(Household_Savings~Top_1Percent+ I(Top_1Percent^2) + I(Top_1Percent^3) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                  Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "within")
# saving robust standard errors for output table
fe_cubic_robust_se <- vcovHC(fe_cubic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# pooled ols estimations

# linear pooled ols panel estimation 
pooled_linear <- plm(Household_Savings~Top_1Percent + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                       Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "pooling")
# saving robust standard errors for output table
pooled_linear_robust_se <- vcovHC(pooled_linear,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# quadratic pooled ols panel estimation 
pooled_quadratic <- plm(Household_Savings~Top_1Percent+ I(Top_1Percent^2) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                          Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "pooling")
# saving robust standard errors for output table
pooled_quadratic_robust_se <- vcovHC(pooled_quadratic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# cubic pooled ols panel estimation 
pooled_cubic <- plm(Household_Savings~Top_1Percent+ I(Top_1Percent^2) + I(Top_1Percent^3) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                      Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "pooling")
# saving robust standard errors for output table
pooled_cubic_robust_se <- vcovHC(pooled_cubic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# random effects estimations

# linear random effects estimations 
re_linear <- plm(Household_Savings~Top_1Percent + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                   Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "random")
# saving robust standard errors for output table
re_linear_robust_se <- vcovHC(re_linear,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# quadratic random effects estimations 
re_quadratic <- plm(Household_Savings~Top_1Percent+ I(Top_1Percent^2) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                      Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "random")
# saving robust standard errors for output table
re_quadratic_robust_se <- vcovHC(re_quadratic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# cubic random effects estimations 
re_cubic <- plm(Household_Savings~Top_1Percent+ I(Top_1Percent^2) + I(Top_1Percent^3) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                  Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "random")
# saving robust standard errors for output table
re_cubic_robust_se <- vcovHC(re_cubic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# comparison of two different credit availability indicators: Amount of Privat Credit vs. Financial Liberalization Index

# credit

# linear fixed effects panel estimation
fe_linear_credit <- plm(Household_Savings~Top_1Percent + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                          Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "within")
# robust standard errors
coeftest(fe_linear_credit, vcovHC(fe_linear_credit,method = c("white1"),type = c("HC0"), cluster = c("group")))
# saving robust standard errors for output table
fe_linear_credit_robust_se <- vcovHC(fe_linear_credit,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# quadratic fixed effects panel estimation with robust standard erros
fe_quadratic_credit <- plm(Household_Savings~Top_1Percent+ I(Top_1Percent^2) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                             Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "within")
# robust standard errors
coeftest(fe_quadratic_credit, vcovHC(fe_quadratic_credit,method = c("white1"),type = c("HC0"), cluster = c("group")))
# saving robust standard errors for output table
fe_quadratic_credit_robust_se <- vcovHC(fe_quadratic_credit,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# cubic fixed effects panel estimation with robust standard erros
fe_cubic_credit <- plm(Household_Savings~Top_1Percent+ I(Top_1Percent^2) + I(Top_1Percent^3) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                         Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "within")
# robust standard errors
coeftest(fe_cubic_credit, vcovHC(fe_cubic_credit,method = c("white1"),type = c("HC0"), cluster = c("group")))
# saving robust standard errors for output table
fe_cubic_credit_robust_se <- vcovHC(fe_cubic_credit,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

#finreform

# linear fixed effects panel estimation
fe_linear_finreform <- plm(Household_Savings~Top_1Percent + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                             Inflation +  Share_Prices+ Delta_House_Prices + Finreform, data=Data, model = "within")
# robust standard errors
coeftest(fe_linear_finreform, vcovHC(fe_linear_finreform,method = c("white1"),type = c("HC0"), cluster = c("group")))
# saving robust standard errors for output table
fe_linear_finreform_robust_se <- vcovHC(fe_linear_finreform,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# quadratic fixed effects panel estimation with robust standard erros
fe_quadratic_finreform <- plm(Household_Savings~Top_1Percent+ I(Top_1Percent^2) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                                Inflation +  Share_Prices+ Delta_House_Prices + Finreform, data=Data, model = "within")
# saving robust standard errors for output table
fe_quadratic_finreform_robust_se <- vcovHC(fe_quadratic_finreform,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# cubic fixed effects panel estimation with robust standard erros
fe_cubic_finreform <- plm(Household_Savings~Top_1Percent+ I(Top_1Percent^2) + I(Top_1Percent^3) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                            Inflation +  Share_Prices+ Delta_House_Prices + Finreform, data=Data, model = "within")
# saving robust standard errors for output table
fe_cubic_finreform_robust_se <- vcovHC(fe_cubic_finreform,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()



# 1% income indicator output table for paper replication
stargazer(
  
  fe_linear_credit, fe_quadratic_credit, fe_cubic_credit, fe_linear_finreform, fe_quadratic_finreform, fe_cubic_finreform, # choose models
  
  se = list(fe_linear_credit_robust_se, fe_quadratic_credit_robust_se, fe_cubic_credit_robust_se,
            fe_linear_finreform_robust_se, fe_quadratic_finreform_robust_se, fe_cubic_finreform_robust_se), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Panel Data Models Based on the Top 1% Income Indicator from the World Inequality Database", # name table
  
  out="output/top1_results.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("FE", "FE", "FE","FE", "FE", "FE"), # name columns with model types
  
  covariate.labels = c("Top 1%", "Top 1% Quadratic", "Top 1% Cubic", "Old Age Dependency Ratio", "Growth of Disposable Income",
                       "Interest Rate", "Fiscal Account Balance", " log(GDP)", "Inflation", "Share Prices",
                       "Growth of House Prices", "Private Credit", "Financial Liberalization Index"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "robust standard errors in parentheses: *p<0.01**p<0.001***p<0.0001", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row    
)

# create same model for top 10% income indicator

# credit

# linear fixed effects panel estimation
fe_linear_credit <- plm(Household_Savings~Top_10Percent + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                          Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "within")
# saving robust standard errors for output table
fe_linear_credit_robust_se <- vcovHC(fe_linear_credit,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# quadratic fixed effects panel estimation with robust standard erros
fe_quadratic_credit <- plm(Household_Savings~Top_10Percent+ I(Top_10Percent^2) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                             Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "within")
# saving robust standard errors for output table
fe_quadratic_credit_robust_se <- vcovHC(fe_quadratic_credit,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# cubic fixed effects panel estimation with robust standard erros
fe_cubic_credit <- plm(Household_Savings~Top_10Percent+ I(Top_10Percent^2) + I(Top_10Percent^3) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                         Inflation +  Share_Prices+ Delta_House_Prices + Private_Credit, data=Data, model = "within")
# saving robust standard errors for output table
fe_cubic_credit_robust_se <- vcovHC(fe_cubic_credit,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

#finreform

# linear fixed effects panel estimation
fe_linear_finreform <- plm(Household_Savings~Top_10Percent + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                             Inflation +  Share_Prices+ Delta_House_Prices + Finreform, data=Data, model = "within")
# saving robust standard errors for output table
fe_linear_finreform_robust_se <- vcovHC(fe_linear_finreform,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# quadratic fixed effects panel estimation with robust standard erros
fe_quadratic_finreform <- plm(Household_Savings~Top_10Percent+ I(Top_10Percent^2) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                                Inflation +  Share_Prices+ Delta_House_Prices + Finreform, data=Data, model = "within")
# saving robust standard errors for output table
fe_quadratic_finreform_robust_se <- vcovHC(fe_quadratic_finreform,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# cubic fixed effects panel estimation with robust standard erros
fe_cubic_finreform <- plm(Household_Savings~Top_10Percent+ I(Top_10Percent^2) + I(Top_10Percent^3) + Dependency_Ratio + Delta_Disposable_Income + Interest_Rate + Fiscal_Account_Balance + log(GDP) +
                            Inflation +  Share_Prices+ Delta_House_Prices + Finreform, data=Data, model = "within")
# saving robust standard errors for output table
fe_cubic_finreform_robust_se <- vcovHC(fe_cubic_finreform,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()



# 10% income indiator output table for paper replication
stargazer(
  
  fe_linear_credit, fe_quadratic_credit, fe_cubic_credit, fe_linear_finreform, fe_quadratic_finreform, fe_cubic_finreform, # choose models
  
  se = list(fe_linear_credit_robust_se, fe_quadratic_credit_robust_se, fe_cubic_credit_robust_se,
            fe_linear_finreform_robust_se, fe_quadratic_finreform_robust_se, fe_cubic_finreform_robust_se), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Panel Data Models Based on the Top 10% Income Indicator from the World Inequality Database", # name table
  
  out="output/top10_results.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("FE", "FE", "FE","FE", "FE", "FE"), # name columns with model types
  
  covariate.labels = c("Top 10%", "Top 10% Quadratic", "Top 10% Cubic", "Old Age Dependency Ratio", "Growth of Disposable Income",
                       "Interest Rate", "Fiscal Account Balance", " log(GDP)", "Inflation", "Share Prices",
                       "Growth of House Prices", "Private Credit", "Financial Liberalization Index"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Household Savings Rate"), # name dependent variable 
  
  notes = "Robust Standard Errors in Parentheses: *p<0.01**p<0.001***p<0.0001", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row
)

# code end - check output folder for results ####