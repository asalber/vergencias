library(tidyverse)
library(DataExplorer)
library(GGally)
library(SmartEDA)
library(tableone)

df <- read_csv("data/datos.csv")
names(df) <- make.names(names(df))

tableOne <- CreateTableOne(vars = colnames(select(df, -"SEXO")), 
                           strata = c("SEXO"), 
                           data = df)

print(
  tableOne,
  showAllLevels = TRUE)

dExpReport(
  df,
  label=NULL,
  op_file="Report.html",
  op_dir=getwd())
