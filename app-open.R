library(shiny)
library(shinyWidgets)
library(shinythemes)
library(RPostgres)
library(httr)
library(jsonlite)
library(twilio)

source("functions.R")

cred <- fromJSON("local/cred.json")

ui <- fluidPage(theme = shinytheme("flatly"),
                # Make modal dialog a bit wider
                tags$head(tags$style(".modal-dialog {width: 700px}")),
                # Add header with logo & info link
                addHeader(),
                # Add loadbar
                addLoadbar(),
                # Set full screen background image
                setBackgroundImage(src = 'a-dog-and-a-cat-gdfb2fac9d_1920_3.jpg'),
                # Show pet
                uiOutput("pet")
                )
                
server <- function(input, output, session) {
  # ============ #
  # Parse URL ####
  # ============ #
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query)) {
      if ("pet_id" %in% names(query)) {
        pet_id <- query[["pet_id"]]
        print(pet_id)
        con <- RPostgres::dbConnect(RPostgres::Postgres(), dbname = "kindly-possum-2518.defaultdb", 
                                    host = "free-tier5.gcp-europe-west1.cockroachlabs.cloud", 
                                    port = 26257, user = "emelieh21", 
                                    password = readLines("local/pw.txt"))
        pet <- dbGetQuery(con, paste0("select pet_id, pet_name, pet_info, email, phone, image from pets where pet_id = '",pet_id,"'"))
        tracking <- dbGetQuery(con, paste0("select * from tracking where pet_id ='",pet_id,"'"))
        # Track page visit if the session token was not registered yet ####
        if (!session$token %in% tracking$session_token) {
          message("Session not tracked yet...")
          dat <- as.data.frame(session$token, stringsAsFactors = FALSE)
          names(dat) <- "session_token"
          dat$pet_id <- pet_id
          dat$time_stamp <- Sys.time()
          dbWriteTable(con, "tracking", dat, overwrite = FALSE, append = TRUE)
          # Sending email if email was entered by user ####
          if (pet$email != "") {
            message("Sending email...")
            data <- paste0('{"personalizations": [{"to": [{"email": "',pet$email,'"}]}],"from": {"email": "info.petconnect@gmail.com"},"subject": "Your pet has been scanned","content": [{"type": "text/plain", "value": "Hello, we are writing to inform you that the QR tag of your pet ',pet$pet_name,' has just been scanned. Greetings from Emelie from PetConnect."}]}')
            result <- POST("https://api.sendgrid.com/v3/mail/send",
                           body = data,
                           add_headers("Content-Type"="application/json",
                                       "Authorization"=paste0("Bearer ",cred$SENDGRID_API_KEY)))
            print(result)
          }
          # Send text message if phone number was entered by user ####
          if (pet$phone != "") {
            message("Sending SMS...")
            
            # First you need to set up your accound SID and token as environmental variables
            Sys.setenv(TWILIO_SID = cred$TWILIO_SID)
            Sys.setenv(TWILIO_TOKEN = cred$TWILIO_TOKEN)
            
            # Then we're just going to store the numbers in some variables
            phone_number <- gsub("\\+","",pet$phone)
            twilios_phone_number <- "18788798086"
            
            # Now we can send away!
            try(tw_send_message(from = twilios_phone_number, to = phone_number, 
                            body = paste0("Hello, we are writing to inform you that the QR tag of your pet ",pet$pet_name," has just been scanned. Kind regards, PetConnect")), silent=TRUE)
          }
        } else {
          message("Session already existed for this pet.")
        }
        dbDisconnect(con)
        output$pet <- renderUI({
          div(
            # Show photo ####
            div(style="display: inline-block;vertical-align:top;margin-top:20px;margin-left:10px;", 
                img(src = pet$image[1], width = 300)
            ),
            # Show info ####
            div(style="display: inline-block;vertical-align:top;margin-top:20px; margin-left:20px;", 
                h1(pet$pet_name[1]),
                tags$em(pet$pet_info[1])
            )
          )
        })
      }
    } 
  })
}


shinyApp(ui, server)
