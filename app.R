library(DT)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinymanager)
library(shinyWidgets)

source("R/gl_viewer_module.R")

sidebar_menu_ui <- function() {
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_menu",
      menuItem(
        text = "GL Viewer",
        tabName = "gl_viewer"
      )
    )
  )
}

ui <- {
  dashboardPage(
    title = "Conestoga GL Viewer",
    header = dashboardHeader(
      title =
        img(
          src = "Conestoga-white.png",
          style = "width: 100%; height: auto;"
        )
    ),
    sidebar = sidebar_menu_ui(),
    body = dashboardBody(
      tags$script(HTML("$('body').addClass('fixed');")),
      tags$head(
        tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "vr_css_styling.css"
        ),
        tags$link(
          rel = "icon",
          type = "image/png",
          href = "cropped-conestoga-favicon-32x32.jpg"
        ),
        tags$style(HTML('.content-wrapper { overflow: auto; }'))
      ),
      useSweetAlert(),
      useShinyjs(),
      tabItems(
        tabItem(
          tabName = "gl_viewer",
          gl_viewer_ui("gl_viewer")
        )
      )
    )
  )
}

server <- function(input, output, session) {
  rv <- reactiveValues()

  rv$loaded_tabs <- list()

  gl_viewer_server("gl_viewer", session)

  observeEvent(input$sidebar_menu, {
    rv$loaded_tabs[[input$sidebar_menu]] <- T
  })

  # observeEvent(rv$loaded_tabs, {
  #   if (input$sidebar_menu == "ethanol_assessment") {
  #     ethanol_assessment_server("ethanol_assessment", session)
  #   } else if (input$sidebar_menu == "denaturant_assessment") {
  #     denaturant_assessment_server("denaturant_assessment", session)
  #   }
  # })
}

# Run the application
shinyApp(ui = ui, server = server)
