library(vegan)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(XLConnect)

# load excel workbook
excel <- loadWorkbook("C:\\Users\\user\\Google Drive\\NN_fields.xlsx") # change to match your path

# get sheet names
getSheets(excel)
sheet_names <- c("BS", "AS1_7day", "AS1_14day")
names(sheet_names) <- sheet_names

# put sheets into a list of data frames
sheet_list <- lapply(sheet_names, function(.sheet){readWorksheet(object=excel, .sheet)})

# limit sheet_list to sheets with at least 1 dimension 
#sheet_list2 <- sheet_list[sapply(sheet_list, function(x) dim(x)[1]) > 0]

#dat_BS <- readxl::read_excel(path = "C:\\Users\\user\\Google Drive\\NN_fields.xlsx", sheet = 1)
#dat_AS1_7 <- readxl::read_excel(path = "C:\\Users\\user\\Google Drive\\NN_fields.xlsx", sheet = 2)
#dat_AS1_14 <- readxl::read_excel(path = "C:\\Users\\user\\Google Drive\\NN_fields.xlsx", sheet = 3)

cleaner <- function(df){
  # cut limit rows only 174
  df <- df[1:174, ]
  # fill up NA with 0
  df[is.na(df)] <- 0
  
  df_rap1 <- df %>% filter(rep == 1)
  df_rap2 <- df %>% filter(rep == 2)
  df_rap3 <- df %>% filter(rep == 3)
  
  df_rap1$rep <- NULL
  df_rap2$rep <- NULL
  df_rap3$rep <- NULL
  
  species <- dat_rap1$species
  
  dat_rap1$species <- NULL
  dat_rap2$species <- NULL
  dat_rap3$species <- NULL
  
  # transvers data frame
  t_dat_rap1 <- as.data.frame(t(dat_rap1))
  t_dat_rap2 <- as.data.frame(t(dat_rap2))
  t_dat_rap3 <- as.data.frame(t(dat_rap3))
  
  
  #row.names(t_dat_rap1)
  #colnames(t_dat_rap1)
  colnames(t_dat_rap1) <- species
  t_dat_rap1$trt <- NULL
  t_dat_rap1$trt <- row.names(t_dat_rap1)
  
  
  row.names(t_dat_rap1) <- NULL
  
  t_dat_rap1 <- t_dat_rap1 %>% select(trt, everything())
  
  
  t_dat_rap1 <- t_dat_rap1 %>% separate(col = trt, into = c("treatment", "point", "insect.type"), sep = "_")
  
  sum_bs_rep1 <- t_dat_rap1 %>% group_by(treatment, point) %>% select(sp1:sp58)%>% summarise_each(funs(sum)) 
  
  sum_bs_rep1 <- cbind(sum_bs_rep1[1],round(sum_bs_rep1[-1], digit = 0)) 
  return(sum_bs_rep1)
}

# clean sheets and create one data frame
# data <- do.call(rbind,lapply(seq_along(sheet_list2), function(x) cleaner(sheet_list2[[x]])))
data <- do.call(rbind,lapply(names(sheet_list2), function(x) cleaner(sheet_list2[[x]])))

restr <- funtion(dat){
  # limit the 
  df <- df[1:174, ] 
  dat$rep <- as.factor(dat$rep)
  
  dat <- dat[1:174,]
  dat[is.na(dat)] <- 0
  
  dat_rap1 <- dat %>% filter(rep == 1)
  dat_rap2 <- dat %>% filter(rep == 2)
  dat_rap3 <- dat %>% filter(rep == 3)
  
  dat_rap1$rep <- NULL
  dat_rap2$rep <- NULL
  dat_rap3$rep <- NULL
  
  species <- dat_rap1$species
  
  dat_rap1$species <- NULL
  dat_rap2$species <- NULL
  dat_rap3$species <- NULL
  
  # transvers data frame
  t_dat_rap1 <- as.data.frame(t(dat_rap1))
  t_dat_rap2 <- as.data.frame(t(dat_rap2))
  t_dat_rap3 <- as.data.frame(t(dat_rap3))
  
  
  #row.names(t_dat_rap1)
  #colnames(t_dat_rap1)
  colnames(t_dat_rap1) <- species
  t_dat_rap1$trt <- NULL
  t_dat_rap1$trt <- row.names(t_dat_rap1)
  
  
  row.names(t_dat_rap1) <- NULL
  
  t_dat_rap1 <- t_dat_rap1 %>% select(trt, everything())
  
  
  t_dat_rap1 <- t_dat_rap1 %>% separate(col = trt, into = c("treatment", "point", "insect.type"), sep = "_")
  
  sum_bs_rep1 <- t_dat_rap1 %>% group_by(treatment, point) %>% select(sp1:sp58)%>% summarise_each(funs(sum)) 
  
  sum_bs_rep1 <- cbind(sum_bs_rep1[1],round(sum_bs_rep1[-1], digit = 0))
  
}
  

plots <- diversity(sum_bs_rep1[-1], index = "shannon")

fish.a<-fisher.alpha(sum_bs_rep1[-1], MARGIN = 1) #count number
fish.a	#shows you the values in the object "fish.a" that you made.




