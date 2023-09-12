
library(DBI)
library(odbc)
library(reshape2)
library(tidyr)
library(dplyr)

root <- "/media/sf_Nextcloud"
# root <- "/home/nrochat/nextcloud"

con <- dbConnect(odbc(), Driver = "SQLite3", Database = paste0(root,"/UGA/01_Projet/01_Longit/02_Data Build/databuild/Longit_DB"))


inference_data <-dbGetQuery(con, "
SELECT * FROM scale_response
  INNER JOIN scale_content ON scale_response.item_id = scale_content.item_id
  LEFT JOIN session_passation
    ON scale_response.passation_id = session_passation.passation_id
  WHERE scale = 'inference'
                            ")

inference_data<- dcast(inference_data,ID+session + date ~rank, value.var="response")

head(inference_data)


# Remove empty row participant is absent
inference_data <- inference_data[rowSums(is.na(inference_data[,paste0(1:14)]))<14, ]

# add response error
# For T4 to T6 the item 1 to 10, NA should be considred ass an error

inference_data_1 <- inference_data %>%
  filter(session %in% c("T4","T5","T6"))%>%
  dplyr::select(c("ID","session", "date",paste0(seq(1,10)))) %>%
  mutate_all( ~ ifelse(is.na(.), 0, .))

# For T7 the item 3 to 14, NA should be considred ass an error
inference_data_2 <- inference_data %>%
  filter(session %in% c("T7"))%>%
  dplyr::select(c("ID","session","date",paste0(seq(3,14)))) %>%
  mutate_all( ~ ifelse(is.na(.), 0, .))


inference_data <- plyr::rbind.fill(inference_data_1,inference_data_2)

for (names in paste0(seq(1:14))){
  if(nchar(names)==1){
    names(inference_data)[names(inference_data)==names] <- paste0("item_0",names)
  } else {
    names(inference_data)[names(inference_data)==names] <- paste0("item_",names)
  }
}


inference <- inference_data
save(inference, file="data/inference.rda" )
