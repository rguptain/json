library(RJSONIO)

uuid <- function(uppercase=FALSE) {
  
  hex_digits <- c(as.character(0:9), letters[1:6])
  hex_digits <- if (uppercase) toupper(hex_digits) else hex_digits
  
  y_digits <- hex_digits[9:12]
  
  paste(
    paste0(sample(hex_digits, 8, replace=TRUE), collapse=''),
    paste0(sample(hex_digits, 4, replace=TRUE), collapse=''),
    paste0('4', paste0(sample(hex_digits, 3, replace=TRUE), collapse=''), collapse=''),
    paste0(sample(y_digits,1), paste0(sample(hex_digits, 3, replace=TRUE), collapse=''), collapse=''),
    paste0(sample(hex_digits, 12, replace=TRUE), collapse=''),
    sep='-')
}

intent_list <- list()

params <-  "Params"
attrs <- "ConfirmSyncNoService"

k1 <- "id"
v1 <- uuid()

k2 <- "name"
v2 <- "sync_no_service"

k3 <- "auto"
v3 <- TRUE

k4 <- "fallbackIntent"
v4 <- FALSE

k5 <- "priority"
v5 <- 500000

k6 <- "webhookUsed"
v6 <- FALSE

k7 <- "webhookForSlotFilling"
v7 <- FALSE

k8 <- "events"
v8 <- vector()

vx = vector()
vx[[1]] <-  params
vx[[2]] <- attrs

k9 <- "contexts"
v9 <- vx

message_list <- list()
message_list[["type" ]] <- 0
message_list[["speech" ]] <- "We cannot perform further troubleshooting steps in automatic mode. Please, contact real agent, and escalate the sync no service issue to UARC."

message_vector = vector(mode = "list")
message_vector[[1]] <-  message_list

affected_contexts_list1 <- list()
affected_contexts_list1[["name" ]] <- params
affected_contexts_list1[["lifespan" ]] <- 0

affected_contexts_list2 <- list()
affected_contexts_list2[["name" ]] <- attrs
affected_contexts_list2[["lifespan" ]] <- 0

affected_contexts_vector = vector(mode = "list")
affected_contexts_vector[[1]] <-  affected_contexts_list1
affected_contexts_vector[[2]] <-  affected_contexts_list2

response_list <- list()
response_list[["resetContexts" ]] <- FALSE
response_list[["parameters" ]] <- vector()
response_list[["affectedContexts" ]] <- affected_contexts_vector
response_list[["messages" ]] <- message_vector

vr = vector(mode = "list")
vr[[1]] <-  response_list

k10 <- "responses"
v10 <- vr

processFile <- function(filepath) 
{
  usv = vector(mode = "list")
  con = file(filepath, "r")
  while ( TRUE ) 
  {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) 
    {
      break
    }
    user_says_data_list <- list()
    user_says_data_list[["text" ]] <- line
    
    user_says_data_vector = vector(mode = "list")
    user_says_data_vector[[1]] <-  user_says_data_list
    
    user_says_list <- list()
    user_says_list[["isTemplate" ]] <- FALSE
    user_says_list[["count" ]] <- 0
    user_says_list[["id" ]] <- uuid()
    user_says_list[["data" ]] <- user_says_data_vector
    
    usv <- c(usv, user_says_list)
  }
  
  close(con)
  return(usv)
}
user_says_vector <- vector()

pwd <- getwd()
json_data_dir <- file.path(pwd, "data", "source")
files <- list.files(json_data_dir)
for (file in files) 
{
  file_loc <- file.path(json_data_dir, file)
  user_says_vector <- processFile(file_loc)
}


k11 <- "userSays"
v11 <- user_says_vector

intent_list[[ k1 ]] <- v1
intent_list[[ k2 ]] <- v2
intent_list[[ k3 ]] <- v3
intent_list[[ k4 ]] <- v4
intent_list[[ k5 ]] <- v5
intent_list[[ k6 ]] <- v6
intent_list[[ k7 ]] <- v7
intent_list[[ k8 ]] <- v8
intent_list[[ k9 ]] <- v9
intent_list[[ k10 ]] <- v10
intent_list[[ k11 ]] <- v11

exportJson <- toJSON(intent_list, digits = 10)

write(exportJson, "test.json")