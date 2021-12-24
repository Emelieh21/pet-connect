# Function to add an header
addHeader <- function (..., moreInfoLink = "https://github.com/Emelieh21/pet-connect", 
                       moreInfoText = "About", 
                       logo_src = "https://raw.githubusercontent.com/Emelieh21/pet-connect/main/www/logo-wide.png") { 
  div(tags$header(div(style = "background-color:#00a884;padding:15px;width:100%;margin:0;", 
                      tags$img(style = "margin-left:20px", 
                               src = logo_src), 
                      ...)), absolutePanel(top = 35, right = 25, 
                                           tags$a(style = "color:#fff;font-size:23px;font-weight:400;", 
                                                  href = moreInfoLink, target = "_blank", moreInfoText)))
}
