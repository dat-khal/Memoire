dsn_data<-read.delim("D:/Data/b300yno/Desktop/MEMOIRE/Donnée/DSN_SOCLE_M_AFFILIATIONBASE_202205_20220621.txt",sep="|")
dsn_data_cot<-read.delim("D:/Data/b300yno/Desktop/MEMOIRE/Donnée/DSN_SOCLE_M_AFFILIATIONCOTISATION_202205_20220621.txt",sep="|")


head(dsn_data)
head(dsn_data_cot)
dim(dsn_data_cot)

table(dsn_data_cot$COD_PIOPRAT)

