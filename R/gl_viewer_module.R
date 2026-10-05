vr_style_button <- function(id, label) {
  actionBttn(
    id,
    label,
    color = "primary",
    style = "simple",
    size = "sm"
  )
}

gl_viewer_ui <- function(id) {
  ns <- NS(id)

  div(
    box(
      width = 12,
      title = "Filter By",
      collapsible = T,
      fluidRow(
        pickerInput(
          inputId = ns("companies"),
          label = "Company",
          choices = c(
            "Arkalon" = "AETH",
            "Bonanza" = "BON",
            "Conestoga Energy Holdings" = "CEH",
            "Saffire" = "SAF",
            "Victory Renewables" = "VR",
            # "AENG",
            # "BCC",
            # "CEPC",
            # "CG",
            # "CIP",
            "CWC",
            # "DIAM",
            # "GIS",
            "HET",
            "LOGIS"
            # "MDO"
          ),
          selected = "CEH",
          multiple = T
        ) |>
          column(width = 2),
        materialSwitch(
          inputId = ns("prior_year"),
          label = div(
            shiny::tags$label(
              class = "control-label",
              "Include Prior Year"
            ),
            br()
          ),
          value = F
        ) |>
          column(width = 2),
      )
    ),
    fluidRow(
      vr_style_button(
        id = ns("update"),
        "Update Data"
      ) |>
        column(width = 2),
      downloadBttn(
        outputId = ns("download_gl"),
        color = "primary",
        style = "simple",
        size = "sm",
        icon = NULL
      ) |>
        column(width = 2)
    ),
    fluidRow(
      DTOutput(
        outputId = ns("gl_dt")
      ) |>
        column(width = 12)
    )
  )
}

get_gl_dt <- function(companies, prior_year) {
  if (length(companies) == 0) {
    tibble::tibble()
  } else {
    tibble::tibble(
      company = companies,
      gl =
        purrr::map(
          company,
          function(company) {
            ret <-
              DBI::dbConnect(
                odbc::odbc(),
                Driver = "ODBC Driver 17 for SQL Server",
                Server = "GP-DB19",
                Database = company,
                UID = "sa-apps",
                PWD = "CommonVigilantDinosaur-1"
              ) |>
              odbc::dbGetQuery(
                "select
                	TRXDATE transaction_date,
                	ORPSTDDT posting_date,
                	ORMSTRID \"entity_id\",
                	ORMSTRNM \"entity_name\",
                	ACTNUMST account_number,
                	ACTDESCR account_description,
                	ACCATDSC department,
                	ORDBTAMT debit_amount,
                	ORCRDAMT credit_amount,
                	JRNENTRY journal_entry_number,
                	DSCRIPTN \"description\",
                	OPENYEAR posting_year,
                	PERIODID posting_period_id
                from [GP-DB19].{company}.dbo.GL20000
                left join [GP-DB19].{company}.dbo.GL00100 on GL20000.ACTINDX = GL00100.ACTINDX
                left join [GP-DB19].{company}.dbo.GL00102 on GL00100.ACCATNUM = GL00102.ACCATNUM
                left join [GP-DB19].{company}.dbo.GL00105 on GL00100.ACTINDX = GL00105.ACTINDX" |>
                  glue::glue()
              )

            if (prior_year) {
              ret <-
                ret |>
                dplyr::bind_rows(
                  DBI::dbConnect(
                    odbc::odbc(),
                    Driver = "ODBC Driver 17 for SQL Server",
                    Server = "GP-DB19",
                    Database = company,
                    UID = "sa-apps",
                    PWD = "CommonVigilantDinosaur-1"
                  ) |>
                    odbc::dbGetQuery(
                      "select
                      	TRXDATE transaction_date,
                      	ORPSTDDT posting_date,
                      	ORMSTRID \"entity_id\",
                      	ORMSTRNM \"entity_name\",
                      	ACTNUMST account_number,
                      	ACTDESCR account_description,
                      	ACCATDSC department,
                      	ORDBTAMT debit_amount,
                      	ORCRDAMT credit_amount,
                      	JRNENTRY journal_entry_number,
                      	DSCRIPTN \"description\",
                      	HSTYEAR posting_year,
                      	PERIODID posting_period_id
                      from [GP-DB19].{company}.dbo.GL30000
                      left join [GP-DB19].{company}.dbo.GL00100 on GL30000.ACTINDX = GL00100.ACTINDX
                      left join [GP-DB19].{company}.dbo.GL00102 on GL00100.ACCATNUM = GL00102.ACCATNUM
                      left join [GP-DB19].{company}.dbo.GL00105 on GL00100.ACTINDX = GL00105.ACTINDX
                      where HSTYEAR = {max(ret$posting_year) - 1}" |>
                        glue::glue()
                    )
                )
            }

            ret
          }
        )
    ) |>
      tidyr::unnest(gl)
  }
}

gl_viewer_server <- function(id, global_session) {
  moduleServer(
    id,

    function(input, output, session) {
      ns <- NS(id)

      # rv <- reactiveValues(
      #   gl_dt = get_gl_dt(input$companies)
      # )

      gl_dt <-
        reactive(
          get_gl_dt(input$companies, input$prior_year)
          # rv$gl_dt
        )

      output$gl_dt <-
        renderDT({
          gl_dt() |>
            isolate() |>
            datatable(
              class = "no-wrap-dt compact",
              extensions = c("Scroller"),
              selection = "none",
              rownames = F,
              colnames = gl_dt() |> isolate() |> colnames() |> vrUtils::convert_titles_to_production_style(),
              callback = JS("$('.dataTables_scrollBody').css('border-bottom', 'none');"),
              options =
                list(
                  dom = "Bfltrpi",
                  hover = T,
                  deferRender = T,
                  scrollX = T,
                  pageLength = 20,
                  lengthMenu = c(20, 50, 100),
                  language =
                    list(
                      paginate =
                        list(
                          'first' = as.character(icon("fast-backward", lib = "glyphicon")),
                          'previous' = as.character(icon("step-backward", lib = "glyphicon")),
                          'next' = as.character(icon("step-forward", lib = "glyphicon")),
                          'last' = as.character(icon("fast-forward", lib = "glyphicon"))
                        )
                    )
                )
            ) |>
            formatCurrency(
              columns = c("debit_amount", "credit_amount"),
              currency = c("$"),
              digits = c(5)
            ) |>
            formatDate(
              columns = c("transaction_date", "posting_date"),
              method = "toLocaleDateString"
            )
        })

      gl_dt_proxy <- DT::dataTableProxy(outputId = ns("gl_dt"), session = global_session)

      observe({
        DT::replaceData(
          gl_dt_proxy,
          data = gl_dt(),
          rownames = F,
          resetPaging = F,
          clearSelection = F
        )
      })

      output$download_gl <-
        downloadHandler(
          filename = function() {
            "GL Entries.xlsx"
          },
          content = function(file) {
            openxlsx::write.xlsx(x = isolate(gl_dt()), file)
          }
        )
    }
  )
}
