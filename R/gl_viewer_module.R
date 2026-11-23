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
            "CEH" = "CEH",
            "Saffire" = "SAF",
            "Victory Renewables" = "VR",
            # "AENG",
            "Carbon Capture" = "BCC",
            # "CEPC",
            "Carbon Garden" = "CG",
            # "CIP",
            "CWC",
            # "DIAM",
            # "GIS",
            "HET",
            "Logistics" = "LOGIS"
            # "MDO"
          ),
          selected = "CEH",
          multiple = T,
          options =
            list(`actions-box` = TRUE)
        ) |>
          column(width = 2),
        pickerInput(
          inputId = ns("posting_period_filter"),
          label = "Posting Period",
          choices = 0:12,
          selected = c(),
          multiple = T,
          options =
            list(`actions-box` = TRUE)
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
        shinyWidgets::radioGroupButtons(
          inputId = ns("balance_income_toggle"),
          label = "GL Account Type",
          choices = c("Income Statement", "Balance Sheet"),
          selected = "Income Statement"
        ) |>
          column(width = 3)
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
      tabBox(
        width = 12,
        tabPanel(
          "GL",
          DTOutput(outputId = ns("gl_dt"))
        ),
        tabPanel(
          "Inventory",
          DTOutput(outputId = ns("inventory_dt"))
        )
      ) |>
        column(width = 12)
    )
  )
}

get_gl_dt <- function(companies, balance_income, prior_year, posting_period) {
  showModal(modalDialog("Loading GL Entries...", footer = NULL, easyClose = F))

  if (length(companies) == 0 | length(posting_period) == 0) {
    removeModal()
    tibble::tibble(
      company = character(0),
      transaction_date = lubridate::as_datetime(double(0)),
      posting_date = lubridate::as_datetime(double(0)),
      entity_id = character(0),
      account_number = character(0),
      account_description = character(0),
      department = character(0),
      debit_amount = double(0),
      credit_amount = double(0),
      journal_entry_number = integer(0),
      posting_year = integer(0),
      posting_period_id = integer(0),
      entry_id = integer(0)
    )
  } else {
    ret <-
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
                  # UID = "sa-apps",
                  # PWD = "CommonVigilantDinosaur-1"
                  UID = "sa",
                  PWD = "Ethanol-2"
                ) |>
                odbc::dbGetQuery(
                  "select
                  	TRXDATE transaction_date,
                  	ORPSTDDT posting_date,
                  	trim(ORMSTRID) \"entity_id\",
                  	trim(ORMSTRNM) \"entity_name\",
                  	trim(ACTNUMST) account_number,
                  	trim(ACTDESCR) account_description,
                  	trim(ACCATDSC) department,
                  	ORDBTAMT debit_amount,
                  	ORCRDAMT credit_amount,
                  	JRNENTRY journal_entry_number,
                  	trim(DSCRIPTN) \"description\",
                  	OPENYEAR posting_year,
                  	PERIODID posting_period_id,
                    GL20000.DEX_ROW_ID entry_id
                  from [GP-DB19].{company}.dbo.GL20000
                  left join [GP-DB19].{company}.dbo.GL00100 on GL20000.ACTINDX = GL00100.ACTINDX
                  left join [GP-DB19].{company}.dbo.GL00102 on GL00100.ACCATNUM = GL00102.ACCATNUM
                  left join [GP-DB19].{company}.dbo.GL00105 on GL00100.ACTINDX = GL00105.ACTINDX
                  where ACTNUMST {
                    ifelse(
                      balance_income == \"Income Statement\",
                       \">= '40000-00-000-000'\",
                       \"< '40000-00-000-000'\"
                    )
                  } and
                  PERIODID in ({posting_period |> stringr::str_c(collapse = ', ')})
                  order by TRXDATE" |>
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
                      # UID = "sa-apps",
                      # PWD = "CommonVigilantDinosaur-1"
                      UID = "sa",
                      PWD = "Ethanol-2"
                    ) |>
                      odbc::dbGetQuery(
                        "select
                        	TRXDATE transaction_date,
                          ORPSTDDT posting_date,
                        	trim(ORMSTRID) \"entity_id\",
                        	trim(ORMSTRNM) \"entity_name\",
                        	trim(ACTNUMST) account_number,
                        	trim(ACTDESCR) account_description,
                        	trim(ACCATDSC) department,
                        	ORDBTAMT debit_amount,
                        	ORCRDAMT credit_amount,
                        	JRNENTRY journal_entry_number,
                        	trim(DSCRIPTN) \"description\",
                        	HSTYEAR posting_year,
                        	PERIODID posting_period_id,
                          GL30000.DEX_ROW_ID entry_id
                        from [GP-DB19].{company}.dbo.GL30000
                        left join [GP-DB19].{company}.dbo.GL00100 on GL30000.ACTINDX = GL00100.ACTINDX
                        left join [GP-DB19].{company}.dbo.GL00102 on GL00100.ACCATNUM = GL00102.ACCATNUM
                        left join [GP-DB19].{company}.dbo.GL00105 on GL00100.ACTINDX = GL00105.ACTINDX
                        where ACTNUMST {
                          ifelse(
                            balance_income == \"Income Statement\",
                             \">= '40000-00-000-000'\",
                             \"< '40000-00-000-000'\"
                          )
                        } and
                        HSTYEAR = {max(ret$posting_year) - 1} and
                        PERIODID in ({posting_period |> stringr::str_c(collapse = ', ')})
                        order by TRXDATE" |>
                          glue::glue()
                      )
                  ) |>
                  dplyr::arrange(transaction_date)
              }

              ret |>
                tibble::tibble()
            }
          )
      ) |>
        tidyr::unnest(gl)

    removeModal()

    ret
  }
}

get_inventory_dt <- function(companies, prior_year, posting_period) {
  showModal(modalDialog("Loading Inventory Transactions...", footer = NULL, easyClose = F))

  if (length(companies) == 0 | length(posting_period) == 0) {
    removeModal()

    tibble::tibble(
      company = character(0),
      transaction_date = lubridate::as_datetime(double(0)),
      document_number = character(0),
      document_type = character(0),
      module = character(0),
      item_number = character(0),
      transaction_quantity = double(0),
      unit_cost = double(0),
      extended_cost = double(0),
      transaction_location = character(0)
    )
  } else {
    r <-
      tibble::tibble(
        company = companies,
        gl =
          purrr::map(
            company,
            function(company) {
              current_year <-
                DBI::dbConnect(
                  odbc::odbc(),
                  Driver = "ODBC Driver 17 for SQL Server",
                  Server = "GP-DB19",
                  Database = company,
                  # UID = "sa-apps",
                  # PWD = "CommonVigilantDinosaur-1"
                  UID = "sa",
                  PWD = "Ethanol-2"
                ) |>
                odbc::dbGetQuery("select max(OPENYEAR) year from GL20000") |>
                dplyr::pull(year)

              ret <-
                DBI::dbConnect(
                  odbc::odbc(),
                  Driver = "ODBC Driver 17 for SQL Server",
                  Server = "GP-DB19",
                  Database = company,
                  # UID = "sa-apps",
                  # PWD = "CommonVigilantDinosaur-1"
                  UID = "sa",
                  PWD = "Ethanol-2"
                ) |>
                odbc::dbGetQuery(
                  "select
                  	DOCDATE transaction_date,
                  	trim(DOCNUMBR) document_number,
                  	DOCTYPE document_type,
                  	trim(HSTMODUL) module,
                  	trim(ITEMNMBR) item_number,
                  	TRXQTY quantity,
                  	UNITCOST unit_cost,
                  	EXTDCOST extended_cost,
                  	trim(TRNSTLOC) transaction_location
                  from IV30300
                  where
                  	year(cast(DOCDATE as date)) = {current_year} and
                    month(cast(DOCDATE as date)) in ({posting_period |> stringr::str_c(collapse = ', ')})
                  order by DOCDATE" |>
                    glue::glue() |>
                    print()
                ) |>
                print()

              if (prior_year) {
                ret <-
                  ret |>
                  dplyr::bind_rows(
                    DBI::dbConnect(
                      odbc::odbc(),
                      Driver = "ODBC Driver 17 for SQL Server",
                      Server = "GP-DB19",
                      Database = company,
                      # UID = "sa-apps",
                      # PWD = "CommonVigilantDinosaur-1"
                      UID = "sa",
                      PWD = "Ethanol-2"
                    ) |>
                      odbc::dbGetQuery(
                        "select
                        	DOCDATE transaction_date,
                        	trim(DOCNUMBR) document_number,
                        	DOCTYPE document_type,
                        	trim(HSTMODUL) module,
                        	trim(ITEMNMBR) item_number,
                        	TRXQTY quantity,
                        	UNITCOST unit_cost,
                        	EXTDCOST extended_cost,
                        	trim(TRNSTLOC) transaction_location
                        from IV30300
                        where
                        	year(cast(DOCDATE as date)) = {current_year - 1} and
                        	month(cast(DOCDATE as date)) in ({posting_period |> stringr::str_c(collapse = ', ')})
                        order by DOCDATE" |>
                          glue::glue()
                      )
                  ) |>
                  dplyr::arrange(transaction_date)
              }

              ret |>
                tibble::tibble() |>
                dplyr::mutate(
                  document_type =
                    dplyr::case_when(
                      document_type == 1 ~ "Inventory Adjustment",
                      document_type == 2 ~ "Variance",
                      document_type == 3 ~ "Inventory Transfer",
                      document_type == 4 ~ "Purchase Receipt",
                      document_type == 5 ~ "Sales Return",
                      document_type == 6 ~ "Sales Invoice",
                      document_type == 7 ~ "Assembly",
                      document_type == 8 ~ "Inventory Cost Adj from POP Shipment",
                      document_type == 11 ~ "Inventory Cost Adj from POP Invoice",
                      T ~ as.character(document_type)
                    )
                )
            }
          )
      ) |>
      tidyr::unnest(gl)

    removeModal()

    r |>
      print()
  }
}

gl_viewer_server <- function(id, global_session) {
  moduleServer(
    id,

    function(input, output, session) {
      ns <- NS(id)

      empty_gl_dt <-
        tibble::tibble(
          company = character(0),
          transaction_date = lubridate::as_datetime(double(0)),
          posting_date = lubridate::as_datetime(double(0)),
          entity_id = character(0),
          account_number = character(0),
          account_description = character(0),
          department = character(0),
          debit_amount = double(0),
          credit_amount = double(0),
          journal_entry_number = integer(0),
          posting_year = integer(0),
          posting_period_id = integer(0),
          entry_id = integer(0)
        )

      empty_inventory_dt <-
        tibble::tibble(
          company = character(0),
          transaction_date = lubridate::as_datetime(double(0)),
          document_number = character(0),
          document_type = character(0),
          module = character(0),
          item_number = character(0),
          transaction_quantity = double(0),
          unit_cost = double(0),
          extended_cost = double(0),
          transaction_location = character(0)
        )

      observe({
        invalidateLater(14400000, session)

        prev_posting_period <- isolate(input$posting_period_filter)

        if (is.null(prev_posting_period)) {
          updatePickerInput(
            session = session,
            "posting_period_filter",
            selected =
              as.character(
                lubridate::month(lubridate::today()) - 1 + (12 * (lubridate::month(lubridate::today()) == 1))
              )
          )
        }
      })

      rv <- reactiveValues(
        gl_dt = empty_gl_dt,
        inventory_dt = empty_inventory_dt
      )

      observeEvent(
        input$companies, {
          rv$gl_dt <- empty_gl_dt
          rv$inventory_dt <- empty_inventory_dt
        }
      )

      observeEvent(
        input$balance_income_toggle, {
          rv$gl_dt <- empty_gl_dt
          rv$inventory_dt <- empty_inventory_dt
        }
      )

      observeEvent(
        input$prior_year, {
          rv$gl_dt <- empty_gl_dt
          rv$inventory_dt <- empty_inventory_dt
        }
      )

      observeEvent(
        input$posting_period_filter, {
          rv$gl_dt <- empty_gl_dt
          rv$inventory_dt <- empty_inventory_dt
        }
      )

      observeEvent(
        input$update, ignoreInit = T, {
          rv$gl_dt <-
            get_gl_dt(
              input$companies |> isolate(),
              input$balance_income_toggle |> isolate(),
              input$prior_year |> isolate(),
              as.numeric(input$posting_period_filter |> isolate())
            )
          rv$inventory_dt <-
            get_inventory_dt(
              input$companies |> isolate(),
              input$prior_year |> isolate(),
              as.numeric(input$posting_period_filter |> isolate())
            )
        }
      )

      gl_dt <-
        reactive({
          rv$gl_dt
        })

      output$gl_dt <-
        renderDT({
          # this print statement keeps the DT from filtering for some reason
          print(gl_dt())

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

      inventory_dt <-
        reactive({
          rv$inventory_dt
        })

      output$inventory_dt <-
        renderDT({
          # this print statement keeps the DT from filtering for some reason
          print(inventory_dt())

          inventory_dt() |>
            isolate() |>
            datatable(
              class = "no-wrap-dt compact",
              extensions = c("Scroller"),
              selection = "none",
              rownames = F,
              colnames = inventory_dt() |> isolate() |> colnames() |> vrUtils::convert_titles_to_production_style(),
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
              columns = c("unit_cost", "extended_cost"),
              currency = c("$"),
              digits = c(5)
            ) |>
            formatDate(
              columns = c("transaction_date"),
              method = "toLocaleDateString"
            )
        })

      inventory_dt_proxy <- DT::dataTableProxy(outputId = ns("inventory_dt"), session = global_session)

      observe({
        DT::replaceData(
          inventory_dt_proxy,
          data = inventory_dt(),
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
            openxlsx::write.xlsx(
              x =
                list(
                  "GL" = isolate(gl_dt()),
                  "Inventory" = isolate(inventory_dt())
                ),
              file
            )
          }
        )
    }
  )
}
