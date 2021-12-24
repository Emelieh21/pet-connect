library(shiny)
library(shinyWidgets)
library(shinythemes)
library(auth0)
library(shinyjs)

source("functions.R")

ui <- fluidPage(theme = shinytheme("flatly"),
                # Make modal dialog a bit wider
                tags$head(tags$style(".modal-dialog {width: 700px}")),
                # Add header with logo & info link
                addHeader(),
                # Set full screen background image
                setBackgroundImage(src = 'a-dog-and-a-cat-gdfb2fac9d_1920_3.jpg'),
                # Use shinyjs
                useShinyjs(),
                # Button to add your pet
                actionButton("add_pet", "Add your pet", icon("plus"), 
                             style="color:#fff;background-color:#00a884;font-size:200%;margin:35px;"))
                
server <- function(input, output, session) {
  # =========================== #
  # Pop up window to add pet ####
  # =========================== #
  observeEvent(input$add_pet, {
    print(session$userData$auth0_info)
    showModal(modalDialog(
      title = "Add your pet",
      div(
        # Upload photo ####
        div(style="display: inline-block;vertical-align:top;margin-top:20px;margin-left:10px;", 
            div(style = "display:flexbox", id = "image-container", 
                img(src = "dog-cat-icon-3.jpg", width = 300)),
            fileInput("myFile", "Upload photo (optional)", accept = c('image/png', 'image/jpeg')),
            div(style = "margin-top: -20px"), # Reduce space
            HTML("<em style='font-size: 8px;'>* This will be publicly visible</em>")
        ),
        # Add info ####
        div(style="display: inline-block;vertical-align:top;margin-top:20px; margin-left:20px;", 
            textInput("pet_name","Name: ", value = session$userData$auth0_info$nickname),
            div(style = "margin-top: -20px"), # Reduce space
            HTML("<em style='font-size: 8px;'>* This will be publicly visible</em>"),
            textAreaInput("pet_info", "Info: "),
            div(style = "margin-top: -20px"), # Reduce space
            HTML("<em style='font-size: 8px;'>* This will be publicly visible</em>"),
            checkboxInput("mobile_notification", "Send mobile notification", value = TRUE),
            uiOutput("phone_number_output"),
            checkboxInput("email_notification", "Send email notification", value = TRUE),
            uiOutput("email_output"),
            div(id = "agree_text", style="width:300px; font-size:12px", HTML('<input type="checkbox" name="checkbox" value="check" id="agree" /> I have read and agree to the Terms and Conditions</br>and Privacy Policy'))
            )
        ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK",
                     style="color:#fff;background-color:#00a884")
      )
    ))
  })
  # React to image file selection ####
  observeEvent(input$myFile, {
    removeUI(
      selector = "#image-container > *"
    )
    inFile <- input$myFile
    if (is.null(inFile)) 
      return()
    b64 <- base64enc::dataURI(file = inFile$datapath, mime = "image/png")
    insertUI(
      selector = "#image-container",
      where = "afterBegin",
      ui = img(src = b64, width = 300)
    )
  })
  # React to phone notification checkbox ####
  observeEvent(input$mobile_notification, {
    output$phone_number_output <- renderUI({
               if (input$mobile_notification) {
                 textInput("phone_number", "Phone number:")
               } else {
                 return(NULL)
               }
    })
  })
  # React to phone notification checkbox ####
  observeEvent(input$email_notification, {
    output$email_output <- renderUI({
      if (input$email_notification) {
        textInput("email", "Email address:")
      } else {
        return(NULL)
      }
    })
  })
  # React to OK button in "Add your pet" ####
  observeEvent(input$ok, {
    # Check if terms and conditions box is checked
    if (input$agree == FALSE) {
        runjs('document.getElementById("agree_text").style.color = "red";')    
    }
  })
}


shinyAppAuth0(ui, server, config_file = 'local/_auth0.yml')
