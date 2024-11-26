shinyServer(function(session, input, output) {
  options(shiny.maxRequestSize = 30 * 1024^2)

  # Global variables
  rv <- reactiveValues(data = list(), # List of data frames
                       sequence = list(), # List of sequences
                       activeFile = NULL, # Index of active file
                       results = list(), # List of PCA results
                       tmpData = NULL, # Temporary data
                       tmpSequence = NULL, # Temporary sequence
                       choices = NULL, # List of choices
                       drift_plot_select = 1, # Drift plot selection
                       info = vector("character"), # Vector of info
                       pca_results = list(), # List of PCA results
                       outlier_df = list(), # List of outlier data frames
                       identifier_df = list()) # List of outlier data frames

  userConfirmation <- reactiveVal(FALSE)
  disable("upload")

  rankings_merge <- data.frame(
    name = c("high", "medium", "low"),
    priority = c(1, 2, 3)
  )

  massCorrection <- read.csv("./csvfiles/adducts.csv")

  # Window/panel selection
  observeEvent(list(c(input$sequence, input$example, input$upload)), {
      windowselect("sequence")
    }, ignoreInit = T)
  observeEvent(input$explore, {
    windowselect("datatable")
  })
  observeEvent(input$export, {
    windowselect("export")
  })
  observeEvent(input$statistics_button, {
    windowselect("statistics")
  })


  ### Functions ###

  initializeVariables <- function() {
    rv$results[[length(rv$results) + 1]] <- list()
  }

  createDownloadHandler <- function(type, fileExtension, dataFunc) {
    function(x) {
      output[[paste0("dwn_", type, x)]] <- downloadHandler(
        filename = function() {
          paste0(names(rv$data[x]), "_", type, fileExtension)
        },
        content = function(file) {
          dataToWrite <- dataFunc(rv, x)  # Pass rv and x to the data function
          if(fileExtension == ".csv") {
            write.csv(dataToWrite, file, row.names = FALSE)
          } else if(fileExtension == ".xlsx") {
            write_xlsx(dataToWrite, file)
          } else if(fileExtension == ".txt") {
            writeLines(dataToWrite, file)
          }
        }
      )
    }
  }

  renderDownloadUI <- function(idPrefix, labelSuffix) {
    renderUI({
      lapply(seq_len(length(rv$choices)), function(x) {
        fluidRow(column(12, downloadLink(paste0(idPrefix, x), paste0(rv$choices[x], labelSuffix))))
      })
    })
  }

  updateDataAndSequence <- function(notificationMessage, newFileInput, suffix, additionalInfo = NULL) {
    if (is.null(rv$tmpData)) {
      showNotification(notificationMessage, type = "error")
    } else {
      if (newFileInput) {
        newIndex <- length(rv$data) + 1
        rv$data[[newIndex]] <- rv$tmpData
        rv$sequence[[newIndex]] <- rv$tmpSequence
        newName <- paste0(names(rv$data)[rv$activeFile], suffix)
        names(rv$data)[newIndex] <- newName
        if (!is.null(additionalInfo)) {
          rv$info[newIndex] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), additionalInfo, "\n")
        }
        initializeVariables()
        rv$activeFile <- newIndex
      } else {
        rv$data[[rv$activeFile]] <- rv$tmpData
        rv$sequence[[rv$activeFile]] <- rv$tmpSequence
        names(rv$data)[rv$activeFile] <- paste0(names(rv$data)[rv$activeFile], suffix)
        if (!is.null(additionalInfo)) {
          rv$info[rv$activeFile] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), additionalInfo, "\n")
        }
      }
      rv$choices <- paste(seq_along(rv$data), ": ", names(rv$data))
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL
    }
  }
  
  updateSequence <- function(seq, data) {
    # Get the column names from the data
    data_colnames <- colnames(data)
    
    # Identify missing rows in the sequence (new columns in data)
    missing_seq_rows <- setdiff(data_colnames, rownames(seq))
    
    # Exclude any sample columns that may have been renamed
    sample_cols <- seq[seq$labels == "Sample", , drop = FALSE]
    sample_colnames <- rownames(sample_cols)
    
    # Correct for any prefixes added to sample column names
    corrected_sample_colnames <- sub("^X", "", sample_colnames)
    rownames(sample_cols) <- corrected_sample_colnames
    
    # Update sequence row names
    seq[rownames(sample_cols), ] <- sample_cols
    
    # Update the list of missing sequence rows after correcting sample names
    missing_seq_rows <- setdiff(data_colnames, rownames(seq))
    
    # Proceed as before to add new identifier columns
    if (length(missing_seq_rows) > 0) {
      num_new_rows <- length(missing_seq_rows)
      
      new_seq_entries <- data.frame(
        labels = rep("Identifier", num_new_rows),
        batch = rep(NA, num_new_rows),
        order = rep(NA, num_new_rows),
        group = rep(NA, num_new_rows),
        time = rep(NA, num_new_rows),
        paired = rep(NA, num_new_rows),
        amount = rep(NA, num_new_rows),
        stringsAsFactors = FALSE,
        row.names = missing_seq_rows
      )
      
      seq <- rbind(seq, new_seq_entries)
    }
    
    # Reorder sequence to match data columns
    seq <- seq[data_colnames, , drop = FALSE]
    
    return(seq)
  }

  observeEvent(input$inputFile, { # Ensure file is uploaded before pressing Upload button
    inputFile <<- read.csv(input$inputFile$datapath, header = 1, stringsAsFactors = F, check.names = FALSE)
    enable("upload")
  })

  observeEvent(input$upload, {
    shinyCatch({
      #inputFile <- read.csv(input$inputFile$datapath, header = 1, stringsAsFactors = F, check.names = FALSE)
      if(input$fileType == "Samples in rows") {
        inputFile <- t(inputFile)
      }
    },
      blocking_level = 'message'
    )
    if(any(duplicated(names(inputFile)))) {
      sendSweetAlert(session, title = "Error", text = paste("Duplicate columns found."), type = "error")
    } else {
      labels <- identifyLabels(inputFile)
      initializeVariables()
      rv$sequence[[length(rv$sequence) + 1]] <- data.frame(labels, batch = NA,
                                                        order = NA, group = NA,
                                                        time = NA, paired = NA,
                                                        amount = NA)
      rv$data[[length(rv$data) + 1]] <- inputFile
      names(rv$data)[length(rv$data)] <- substr(input$inputFile$name, 1, nchar(input$inputFile$name) - 4)
      rv$choices <- paste(seq_along(rv$data), ": ", names(rv$data))
      rv$activeFile <- length(rv$data)
      updateTabItems(session, "tabs", selected = "Datainput")
      show("buttons")
    }
  })

  observeEvent(input$inputSequence, {
    shinyCatch({
      inputSequence <- read.csv(input$inputSequence$datapath, header = 1, stringsAsFactors = FALSE)
      colnames(inputSequence) <- tolower(colnames(inputSequence))
      inputSequence <- checkSequence(inputSequence)
    },
      blocking_level = 'message'
    )
    sequence <- rv$sequence[[rv$activeFile]]
    labeledSequence <- data.frame("sample" = row.names(sequence), sequence)
    inputSequence["sample"] <- lapply(inputSequence["sample"], as.character)
    sequence <- left_join(labeledSequence[, 1:2], inputSequence, by = "sample")
    row.names(sequence) <- sequence[, 1]
    sequence <- sequence[, -1]
    rv$sequence[[rv$activeFile]] <- sequence

    if(any(grepl("[^[:alnum:]]", sequence$group))) {
      showModal(
        modalDialog(
          title = "Invalid group names", size = "m", easyClose = TRUE,
          footer = list(actionButton("group_name_format", "Format names"), modalButton("Dismiss")),
          fluidRow(
            column(12, p("Invalid group names found. Group names must be alphanumeric and not include spaces."))
          )
        )
      )
    }
  })

  observeEvent(input$group_name_format, {
    sequence <- rv$sequence[[rv$activeFile]]
    sequence$group <- gsub("[^[:alnum:]]", "", sequence$group)
    rv$sequence[[rv$activeFile]] <- sequence
    removeModal()
  })

  observeEvent(input$reuseSequence, {
    inputSequence <- read.csv(input$inputSequence$datapath, header = 1, stringsAsFactors = FALSE)
    colnames(inputSequence) <- tolower(colnames(inputSequence))
    inputSequence <- checkSequence(inputSequence)
    sequence <- rv$sequence[[rv$activeFile]]
    labeledSequence <- data.frame("sample" = row.names(sequence), sequence)
    inputSequence["sample"] <- lapply(inputSequence["sample"], as.character)
    sequence <- left_join(labeledSequence[, 1:2], inputSequence, by = "sample")
    row.names(sequence) <- sequence[, 1]
    sequence <- sequence[, -1]
    rv$sequence[[rv$activeFile]] <- sequence
  })

  observeEvent(input$editColumns, {
    showModal(
      modalDialog(
        title = "Edit columns", size = "s", easyClose = TRUE,
        footer = list(actionButton("edit_cols_confirm", "Confirm"), modalButton("Dismiss")),
        fluidRow(
          column(width = 9, h4("Column name")),
          column(width = 3, style = "text-align: left;", h4("Keep"))
        ),
        lapply(seq(ncol(rv$data[[rv$activeFile]])), function(x) {
          fluidRow(
            column(
              width = 9,
              textInput(paste0("column_edit_name", x), NULL, value = colnames(rv$data[[rv$activeFile]])[x])
            ),
            column(
              width = 3, style = "text-align: center;",
              prettyCheckbox(paste0("columns_to_keep", x), NULL, status = "info", value = T)
            ),
          )
        })
      )
    )
  })

  observeEvent(input$edit_cols_confirm, {
    ncol <- ncol(rv$data[[rv$activeFile]])
    column_names <- character()
    column_names <- sapply(seq(ncol), function(x) {
      input[[paste0("column_edit_name", x)]]
    })
    if(!checkDuplicates(column_names)) {
      isolate(colnames(rv$data[[rv$activeFile]]) <- column_names)
      isolate(row.names(rv$sequence[[rv$activeFile]]) <- column_names)
      keep <- sapply(seq(ncol), function(x) input[[paste0("columns_to_keep", x)]])
      rv$data[[rv$activeFile]] <- rv$data[[rv$activeFile]][, keep]
      rv$sequence[[rv$activeFile]] <- rv$sequence[[rv$activeFile]][keep, ]
    }
    removeModal()
  })

  observeEvent(input$editGroups, {
    sequence <- rv$sequence[[rv$activeFile]]
    unique_groups <- unique(na.omit(sequence[, 4]))
    # Generate UI elements for each group
    group_ui_elements <- lapply(seq(unique_groups), function(x) {
      group <- unique_groups[x]
      fluidRow(
        column(3, h5(group)),
        column(9,
          textInput(paste0("edit_nickname", group), NULL, value = NULL)
        ),
      )
    })
    showModal(
      modalDialog(
        title = "Edit Group Nicknames", size = "s", easyClose = TRUE,
        footer = list(actionButton("group_edit_confirm", "Confirm"), modalButton("Dismiss")),
        fluidRow(
          column(3, h4("Group")),
          column(9, h4("Nickname"))
        ), 
        do.call(tagList, group_ui_elements)
      )
    )
  })

  observeEvent(input$group_edit_confirm, {
    sequence <- rv$sequence[[rv$activeFile]]
    groups <- sequence[, 4]
    for (x in seq_along(groups)) {
      if (!is.na(groups[x])) {
        input_x <- input[[paste0("edit_nickname", groups[x])]]
        if (nchar(input_x) != 0 & isValidName(input_x)) {
          sequence[x, 4] <- input_x
        }
      }
    }
    rv$sequence[[rv$activeFile]] <- sequence
    removeModal()
  })

  observeEvent(input$example, {
    # Load example files
    # Negative ion mode
    data <- read.csv("./example_files/Liverfetus_lipid_neg1.csv", stringsAsFactors = FALSE)
    sequence <- read.csv("./example_files/fetus seq neg.csv", stringsAsFactors = FALSE)
    row.names(sequence) <- sequence[, 1]
    sequence <- sequence[, -1]
    rv$sequence[[length(rv$sequence) + 1]] <- sequence
    rv$data[[length(rv$data) + 1]] <- data
    names(rv$data)[length(rv$data)] <- "Liverfetus_negative"
    initializeVariables()

    # Positive ion mode
    data <- read.csv("./example_files/Liverfetus_lipid_pos1.csv", stringsAsFactors = FALSE)
    sequence <- read.csv("./example_files/fetus seq pos.csv", stringsAsFactors = FALSE)
    row.names(sequence) <- sequence[, 1]
    sequence <- sequence[, -1]
    rv$sequence[[length(rv$sequence) + 1]] <- sequence
    rv$data[[length(rv$data) + 1]] <- data
    names(rv$data)[length(rv$data)] <- "Liverfetus_positive"
    initializeVariables()
    rv$choices <- paste(seq_along(rv$data), ": ", names(rv$data))

    updateTabItems(session, "tabs", selected = "Datainput")
    show("buttons")
    updateCollapse(session, "menu", close = "Data input")
    disable("example")
  })

  # Update selected data
  observeEvent(input$selectDataset, ignoreInit = TRUE, {
    rv$activeFile <- which(rv$choices %in% input$selectDataset)
    
    # Make a debugging statement for the active file
    print(paste("Active file is", rv$activeFile))
    # Make a debugging statement for the choices
    print(paste("Choices are", rv$choices))
    # Make a debugging statement for the selectDataset
    print(paste("SelectDataset is", input$selectDataset))
    # Check the names of rv$sequence
    # print("rv$sequence:")
    # print(rv$sequence[[rv$activeFile]])
    # check rownames of rv$sequence
    # print("Seq rownames:")
    # print(rownames(rv$sequence[[rv$activeFile]]))
    # print("Data colnames:")
    # print(colnames(rv$data[[rv$activeFile]]))
    # check that these are the same
    print("Are rownames and colnames identical in seq and daa?")
    print(all(rownames(rv$sequence[[rv$activeFile]]) == colnames(rv$data[[rv$activeFile]])))
    
    
    output$seq_table <- renderDT(rv$sequence[[rv$activeFile]],
                                 extensions = 'Responsive',
                                 server = F,
                                 editable = T,
                                 selection = 'none',
                                 options = list(pageLength = nrow(rv$sequence[[rv$activeFile]]),
                                                scrollX = TRUE))

    output$diboxtitle <- renderText(names(rv$data[rv$activeFile]))

    output$dttable <- renderDT(rv$data[[rv$activeFile]],
                               rownames = FALSE,
                               options = list(scrollX = TRUE,
                                              scrollY = "700px"))

    output$dt_drift_panel <- renderDT(rv$data[[rv$activeFile]][rv$sequence[[rv$activeFile]][, 1] %in% "Name"],
                                      rownames = FALSE,
                                      options = list(autoWidth = TRUE,
                                                     scrollY = "700px",
                                                     pageLength = 20))

    output$dt_boxplot_panel <- renderDT(rv$data[[rv$activeFile]][rv$sequence[[rv$activeFile]][, 1] %in% "Name"],
                                        rownames = FALSE, 
                                        options = list(autoWidth = TRUE,
                                                       scrollY = "700px",
                                                       pageLength = 20))
    
    data <- rv$data[[rv$activeFile]]
    sequence <- rv$sequence[[rv$activeFile]]
    
    output$histogram <- renderPlotly({
      samples <- data[, sequence[ , 'labels'] %in% "Sample"]
      # Debugging
      # print(length(samples))
      # print(names(samples))
      
      medians <- apply(samples, 2, median, na.rm = TRUE)
      median_data <- data.frame(
        Sample = names(medians),
        Median = medians,
        stringsAsFactors = FALSE
      )
      # Debugging
      # print(head(median_data))
      
      ggplot(median_data, aes(x = Sample, y = Median)) +
        geom_col(fill = "skyblue", color = "black", width = 0.7) +
        labs(x = "Samples", y = "Median") +
        theme_minimal() +
        theme(axis.text.x = element_blank())
    })

    output$histogram_qc <- renderUI({
      QCs <- data[, sequence[ , 'labels'] %in% "QC"]
      if(ncol(QCs) > 0) {
        medians <- apply(QCs, 2, median, na.rm = TRUE)
        median_QC <- data.frame(
          QC = names(medians),
          Median = medians
        )
        plotlyOutput("qc_distribution")
        output$qc_distribution <- renderPlotly({
          ggplot(median_QC, aes(x = QC, y = Median)) +
          geom_col(fill = "skyblue", color = "black") +
          labs(x = "Sample", y = "Median") +
          theme_minimal()
        })
      }
      else {
        textOutput("No columns labeled QC.")
      } 
    })

    if (sum(rv$sequence[[rv$activeFile]][, 1] %in% "Name") == 1) { #TODO check if this check is needed
      internalStandards <- findInternalStandards(rv$data[[rv$activeFile]][rv$sequence[[rv$activeFile]][, 1] %in% "Name"])
      updateCheckboxGroupInput(session, "isChoose", choices = internalStandards, selected = internalStandards)
      enable("normalizeIS"); enable("removeIS"); enable("saveIS")
      if(length(internalStandards) == 0) {
        disable("normalizeIS"); disable("removeIS"); disable("saveIS")
      } 
    }
  })

  # Observer for list of all the datasets 
  observeEvent(rv$choices, {
    choices <- rv$choices
    num_datasets <- length(choices)

    output$downloadSequence <- downloadHandler(
      filename <- function() {
        paste0(names(rv$data[rv$activeFile]), "_seq.csv")
      },
      content = function(file) {
        write.csv(cbind("sample" = rownames(rv$sequence[[rv$activeFile]]), rv$sequence[[rv$activeFile]]), file, row.names = FALSE) # TODO
      }
    )
    
    # Export panel
    output$export_ui <- renderDownloadUI("dwn_general", ".csv")
    output$export_metabo <- renderDownloadUI("dwn_metabo", "_metabo.csv")
    output$export_stats <- renderDownloadUI("dwn_stats", "_results.xlsx")
    output$export_settings <- renderDownloadUI("dwn_settings", ".txt")

#    lapply(seq_len(num_datasets), createDownloadHandler("general", ".csv", rv$data[[rv$activeFile]]))
#    lapply(seq_len(num_datasets), createDownloadHandler("stats", ".xlsx", rv$results[[rv$activeFile]]))
#    lapply(seq_len(num_datasets), createDownloadHandler("settings", ".txt", rv$info[[rv$activeFile]]))
#    lapply(seq_len(num_datasets), createDownloadHandler("metabo", ".csv", getMetaboData))

    lapply(1:length(rv$choices), function(x) {
      output[[paste0("dwn_stats", x)]] <- downloadHandler(
        filename = function() {
          paste0(names(rv$data[x]), "_results.xlsx")
        },
        content = function(file) {
          write_xlsx(rv$results[[x]], file)
        }
      )
    })
    lapply(1:length(rv$choices), function(x) {
      output[[paste0("dwn_general", x)]] <- downloadHandler(
        filename = function() {
          paste0(names(rv$data[x]), ".csv")
        },
        content = function(file) {
          write.csv(rv$data[[x]], file, row.names = FALSE)
        }
      )
    })
    lapply(1:length(rv$choices), function(x) {
      output[[paste0("dwn_settings", x)]] <- downloadHandler(
        filename = function() {
          paste0(names(rv$data[x]), ".txt")
        },
        content = function(file) {
          write.csv(rv$info[x], file, row.names = FALSE)
        }
      )
    })
    lapply(1:length(rv$choices), function(x) {
      dat <- rv$data[[x]]
      seq <- rv$sequence[[x]]
      seq[seq[, 1] %in% "QC", 4] <- "QC"
      group <- c("", seq[seq[, 1] %in% c("Sample", "QC"), 4])
      outdat <- data.frame(dat[seq[, 1] %in% "Name"], dat[seq[, 1] %in% c("Sample", "QC")])
      outdat <- rbind(group, outdat)
      output[[paste0("dwn_metabo", x)]] <- downloadHandler(
        filename = function() {
          paste0(names(rv$data[x]), "_metabo.csv")
        },
        content = function(file) {
          write.csv(outdat, file, row.names = FALSE)
        }
      )
    })

    updateCheckboxGroupInput(session, "export_xml_list", choices = choices, selected = NULL)
    updateCheckboxGroupInput(session, "filesToRemove", choices = names(rv$data), selected = NULL)
    updateSelectInput(session, "drift_select", choices = c("None", choices))

    inputs <- c("selectDataset", "mergeFile",
                "selectpca1", "selectpca2",
                "select_enrichment_data") # Update select inputs
    selected <- ifelse(is.null(rv$activeFile), length(choices), rv$activeFile)
    for(input in inputs) {
      updateSelectInput(session, input, choices = choices, selected = choices[selected])
    }
    
    
  })

  observeEvent(input$removeFiles, {
    if (is.null(input$filesToRemove)) {
      showNotification("No files selected.", type = "error")
    } else if(length(input$filesToRemove) == length(rv$choices)) {
      showNotification("At least one file must be kept.", type = "error")
    } else {
      keep <- !names(rv$data) %in% input$filesToRemove      
      rv$data <- rv$data[keep]
      rv$sequence <- rv$sequence[keep]
      rv$info <- rv$info[keep]
      rv$results <- rv$results[keep]
      rv$activeFile <- length(rv$data)
      rv$choices <- paste(seq_along(rv$data), ": ", names(rv$data))
      showNotification("Files removed.", type = "message")
    }
  })

  observeEvent(input$export_xml_list, {
    output$export_xml <- downloadHandler(
      filename = function() {
        paste0(names(rv$data[rv$choices %in% input$export_xml_list])[1], ".xlsx")
      },
      content = function(file) {
        write_xlsx(rv$data[rv$choices %in% input$export_xml_list], file)
      }
    )
  })

  observeEvent(input$seq_table_cell_edit, {
    sequence <- rv$sequence[[rv$activeFile]]
    info <- input$seq_table_cell_edit
    str(info)
    i <- info$row
    j <- info$col
    v <- info$value
    if(j == 1) {
      sendSweetAlert(session, title = "Warning", text = "Column 'labels' cannot be edited", type = "warning")
    } else {
      sequence[i, j] <- v
      rv$sequence[[rv$activeFile]] <- sequence
    }
  })

  observeEvent(input$updateSequence, {
    if (!is.null(rv$activeFile) && !is.null(rv$tmpSequence)) {
      rv$sequence[[rv$activeFile]] <- rv$tmpSequence
      rv$tmpSequence <- NULL
    } else {
      showNotification("No changes to update", type = "message")
    }
  })


  ## Blank filtration ##

  observeEvent(input$blankFiltrate, {
    if(is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if (!"QC" %in% rv$sequence[[rv$activeFile]][, 1]) {
      showNotification("Data must have at least 1 QC", type = "error")
    } else if (!"Blank" %in% rv$sequence[[rv$activeFile]][, 1]) {
      showNotification("Data must have at least 1 Blank", type = "error")
    } else if (sum(rv$sequence[[rv$activeFile]][, 1] %in% "Name") != 1) {
      showNotification("Data must have exactly 1 \"Name\" column", type = "error")
    } else {
      sequence <- rv$sequence[[rv$activeFile]]
      data <- rv$data[[rv$activeFile]]

      filtered <- blankFiltration(data, sequence, input$signalStrength, input$keepIS)
      
      if(input$discardBlank) {
        filtered <- filtered[!sequence[, 1] %in% "Blank"]
        sequence <- sequence[!sequence[, 1] %in% "Blank", ]
      }
      
      rv$tmpData <- filtered
      rv$tmpSequence <- sequence

      updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
      updateSelectInput(session, "select_enrichment_data", selected = "Unsaved data", 
                        choices = c("Unsaved data", rv$choices))
      output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      sendSweetAlert(session, "Success", paste0(nrow(rv$data[[rv$activeFile]]) - nrow(rv$tmpData), " features removed"), type = "success")
    }
  })

  observeEvent(input$saveBF, {
    additionalInfo <- paste("Blank filtrated with signal strength above blank =", input$signalStrength)
    updateDataAndSequence("Blank filtrate first", input$newFileBF, paste("_", input$signalStrength, "xb"), additionalInfo)
  })


  ## IS normalization ##

  observeEvent(input$normalizeIS, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if (sum(rv$sequence[[rv$activeFile]][, 1] %in% "Name") != 1) {
      showNotification("Data must have exactly 1 \"Name\" column", type = "error")
    } else if (is.null(input$isChoose)) {
      showNotification("No internal standards selected", type = "error")
    } else {
      sequence <- rv$sequence[[rv$activeFile]]
      data <- rv$data[[rv$activeFile]]
      
      normalized <- normalizationIS(data, sequence, input$isChoose, input$isMethod, input$normalizeQC)
      
      if(is.null(normalized)) {
        sendSweetAlert(session, "Error", "Internal standard normalization failed due to missing values in IS.", type = "error")
      }
      else {
        # Add isnorm column to sequence
        isColumn <- c("-", rep(NA, ncol(sequence) - 1))
        sequence <- rbind(sequence, isColumn)
        rownames(sequence)[nrow(sequence)] <- "isnorm"

        rv$tmpData <- normalized
        rv$tmpSequence <- sequence
  
        sendSweetAlert(session, title = "Success", text = paste0("Internal standards normalized with ", input$isMethod, " method"), type = "success")
        updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
        updateSelectInput(session, "select_enrichment_data", selected = "Unsaved data", 
                          choices = c("Unsaved data", rv$choices))
        output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      }
    }
  })

  observeEvent(input$saveIS, {
    additionalInfo <- paste("Internal standards normalized with", input$isMethod, "method")
    updateDataAndSequence("IS normalize first", input$newFileIS, "_is", additionalInfo)
  })

  observeEvent(input$removeIS, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else {
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]
      toRemove <- data[sequence[, 1] %in% "Name"]
      data <- data[!grepl("\\(IS\\)", toupper(toRemove[ , 1])), ]
      rv$data[[rv$activeFile]] <- data
      updateCheckboxGroupInput(session, "isChoose", choices = character(0), selected = NULL)
    }
  })


  ## Missing value filtration ##
  observeEvent(input$runFilterNA, {
    if(is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else {
      sequence <- rv$sequence[[rv$activeFile]]
      method <- input$filterNAmethod
      if(("in group" %in% method) & !any(complete.cases(sequence[, "group"]))) {
        sendSweetAlert(session, "Error!", "Group information needed.", type = "error")
      }
      else if(is.null(method)) {
        sendSweetAlert(session, "Error!", "No method selected.", type = "error")
      }
      else {
        mvf_dat <- cutoffrm(rv$data[[rv$activeFile]], sequence, input$cutoffNAs, method)
        rv$tmpData <- mvf_dat
        rv$tmpSequence <- sequence
        updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
        updateSelectInput(session, "select_enrichment_data", selected = "Unsaved data", 
                          choices = c("Unsaved data", rv$choices))
        output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
        sendSweetAlert(
          title = "Success",
          text = paste0(nrow(rv$data[[rv$activeFile]]) - nrow(rv$tmpData), " feature(s) removed"),
          type = "success"
        )
      }
    }
  })

  observeEvent(input$saveFilterNA, {
    additionalInfo <- paste(
        "Missing value filtration using",
        input$cutoffNAs,
        "% as threshold and method -",
        paste(input$filterNAmethod, collapse=", ")
    )
    updateDataAndSequence("Filtrate first", input$mvf_newsave, "_mvr", additionalInfo)
  })

  ## Imputation ##
  observeEvent(input$runImputation, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if (sum(rv$sequence[[rv$activeFile]][, 1] %in% "Sample") < 1) {
      showNotification("Data must have at least one Sample", type = "error")
    } else {
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]
      imputed <- imputation(data, sequence, input$imputationMethod, input$imputationMinX, input$imp_onlyQC, input$remainingNAs)
      
      rv$tmpData <- imputed
      rv$tmpSequence <- sequence
      
      updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
      updateSelectInput(session, "select_enrichment_data", selected = "Unsaved data", 
                        choices = c("Unsaved data", rv$choices))
      output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      sendSweetAlert(
        title = "Success",
        text = paste0(sum(is.na(rv$data[[rv$activeFile]]) | rv$data[[rv$activeFile]] == 0) - sum(is.na(rv$tmpData) | rv$tmpData == 0), " missing values were imputed."),
        type = "success"
      )
    }
  })

  observeEvent(list(input$imputationMethod, input$remainingNAs), {
    if (input$imputationMethod == "KNN") {
      hide("imp_minx_hide")
      hide("imp_remaining_hide")
    } else {
      show("imp_remaining_hide")
    }

    if (input$imputationMethod == "Min/X" || input$remainingNAs == "Min/X") {
      show("imp_minx_hide")
    } else {
      hide("imp_minx_hide")
    }
  })

  observeEvent(input$saveImputation, {
    additionalInfo <- paste("Missing values imputation with", input$imputationMethod)
    updateDataAndSequence("Impute first", input$newFileImp, "_imp", additionalInfo)
  })


  ## Drift correction ##

  # observeEvent(input$driftMethod, {
  #   if (input$driftMethod == "QC-RFSC (random forrest)") {
  #     hide("dc_qcspan_hide")
  #     hide("dc_degree_hide")
  #     show("dc_ntree_hide")
  #   } else {
  #     hide("dc_ntree_hide")
  #     show("dc_qcspan_hide")
  #     show("dc_degree_hide")
  #   }
  # })

  observeEvent(input$runDrift, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if (is.null(rv$sequence[[rv$activeFile]])) { #TODO remove? sequence is never NULL
      showNotification("No sequence file", type = "error")
    } else if (all(is.na(rv$sequence[[rv$activeFile]][, 'order']))) {
      showNotification("No order information, upload sequence", type = "error")
    } else {
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]  
      dat_qc <- data[, sequence[, 1] %in% "QC"]

      if(any(colSums(!is.na(dat_qc)) != nrow(dat_qc))) {
        sendSweetAlert(session = session, title = "Error", text = "QCs cannot have missing values.", type = "error")
      }
      else {
        corrected <- driftCorrection(data, sequence, input$driftMethod, input$driftTrees, input$driftDegree, input$driftQCspan)

        rv$tmpData <- corrected
        rv$tmpSequence <- sequence

        updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
        updateSelectInput(session, "select_enrichment_data", selected = "Unsaved data", 
                          choices = c("Unsaved data", rv$choices))
        output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      }
    }
  })

  observeEvent(input$saveDrift, {
    additionalInfo <- paste(
        "Drift correction applied using method: ", input$driftMethod,
        "with ", input$driftTrees, " trees."
    )
    updateDataAndSequence("Drift correct first", input$newFileDrift, "_dc", additionalInfo)
  })


  ## Merge datasets ##

  observeEvent(input$editRankings, {
    showModal(
      modalDialog(
        title = "Change the priority of annotations", size = "s", easyClose = TRUE,
        footer = list(actionButton("md_edit_rankings", "Save edits"), modalButton("Dismiss")),
        lapply(1:10, function(x) {
          fluidRow(
            column(
              width = 8,
              textInput(paste0("md_rankings_text", x), NULL, value = rankings_merge[x, 1], placeholder = "Empty")
            ),
            column(
              width = 4,
              numericInput(paste0("md_rankings_prio", x), NULL, value = rankings_merge[x, 2], min = 0, max = 10)
            ),
          )
        })
      )
    )
  })

  observeEvent(input$md_edit_rankings, {
    sapply(1:10, function(x) {
      rankings_merge[x, 1] <<- toupper(input[[paste0("md_rankings_text", x)]])
      rankings_merge[x, 2] <<- input[[paste0("md_rankings_prio", x)]]
    })
    removeModal()
  })
  
  observeEvent(input$mergeDatasets, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else {
      activeSequence <- rv$sequence[[rv$activeFile]]
      activeDataset <- rv$data[[rv$activeFile]]
      selected <- which(rv$choices %in% input$mergeFile)
      if(is.null(selected)) {
        showNotification("No file selected", type = "error")
      } else {
        sequenceToMerge <- rv$sequence[[selected]]
        datasetToMerge <- rv$data[[selected]]

        if(names(rv$data)[rv$activeFile] == names(rv$data)[selected]) {
          showModal(
            modalDialog(
              title = "Do you want to merge a dataset with itself?", size = "m",
              footer = list(actionButton("mergeSameFile", "Yes"), modalButton("Cancel"))
            )
          )
        } else {
          userConfirmation(TRUE)
        }
      }
    }
  })
  
  observeEvent(input$mergeSameFile, {
    userConfirmation(TRUE)
    removeModal()
  })

  observeEvent(userConfirmation(), {
    if(userConfirmation()) {
      activeSequence <- rv$sequence[[rv$activeFile]]
      activeDataset <- rv$data[[rv$activeFile]]
      selected <- which(rv$choices %in% input$mergeFile)
      sequenceToMerge <- rv$sequence[[selected]]
      datasetToMerge <- rv$data[[selected]]
      if (sum(activeSequence[, 1] %in% c("Adduct_pos", "Adduct_neg")) != 1 || sum(sequenceToMerge[, 1] %in% c("Adduct_pos", "Adduct_neg")) != 1) {
        sendSweetAlert(session = session, title = "Error", text = "Each dataset must contain exactly one adduct column labeled in the sequence file.", type = "error")
      } else if (ncol(activeDataset) != ncol(datasetToMerge)) {
        sendSweetAlert(session = session, title = "Error", text = "Datasets must have the same number of columns", type = "error")
      } else {
        mergedDatasets <<- mergeDatasets(activeDataset, activeSequence,
              datasetToMerge, sequenceToMerge, input$merge_ppm, input$merge_rt)
        clustn <- data.frame(table(mergedDatasets$mergeID))
        dub_clust <- clustn[clustn$Freq > 1, ]
        dub_dat <- mergedDatasets[mergedDatasets$mergeID %in% dub_clust[, 1], ]
        dub_qc <- dub_dat[, activeSequence[, 1] %in% "QC"]
        cov <- cv(dub_qc)
        nclust <- sapply(dub_dat$mergeID, function(x) {
          table(dub_dat$mergeID)[names(table(dub_dat$mergeID)) == x]
        })
        colnames(dub_dat)[activeSequence[, 1] %in% c("Adduct_pos", "Adduct_neg")] <- "adduct"
        out_dub <- data.frame(
          "nClust" = nclust,
          "Cluster_ID" = dub_dat$mergeID,
          "Ion_mode" = dub_dat$ionmode,
          "Adductor" = dub_dat$adduct,
          "Name" = dub_dat[, which(activeSequence[, 1] %in% "Name")],
          "RT" = dub_dat[, which(activeSequence[, 1] %in% "RT")],
          "Mass" = dub_dat[, which(activeSequence[, 1] %in% "Mass")],
          "CV" = cov
        )
        out_dub <- out_dub[order(out_dub[, 1], out_dub[, 2], decreasing = T), ]
        md_dup <<- out_dub
        cluster_ends <- which(!duplicated(out_dub[, 2]))
        output$md_modal_dt <- renderDataTable({
            datatable(out_dub,
              rownames = F,
              options = list(dom = "t", autowidth = T, paging = F),
              selection = list(selected = finddup(out_dub, rankings_merge))
            ) %>% formatStyle(1:8, `border-top` = styleRow(cluster_ends, "solid 2px"))
          },
          server = T
        )
        userConfirmation(FALSE)
        showModal(
          modalDialog(
            title = "Select features to keep", size = "l",
            p(paste0(length(unique(dub_dat$mergeID))), " duplicate clusters found, of those ", paste0(length(unique(out_dub[out_dub[, 1] > 2, ][, 2]))), " consists of more than 2 features."),
            DTOutput("md_modal_dt"),
            footer = list(actionButton("confirmMerging", "Remove duplicates"), modalButton("Dismiss"))
          )
        )
      }
    }
  })

  observeEvent(input$confirmMerging, {
    duplicates <- as.numeric(rownames(md_dup[-input$md_modal_dt_rows_selected, ]))
    merged <<- mergedDatasets[-duplicates, ]
    output$dttable <- renderDataTable(merged, rownames = F, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
    removeModal()
    confirmSweetAlert(session, inputId = "newFileMerge", title = "Merge complete", text = "Save as new file?", btn_labels = c("No", "Yes"), type = "success")
  })

  observeEvent(input$newFileMerge, {
    if (isTRUE(input$newFileMerge)) {
      rv$data[[length(rv$data) + 1]] <- merged[, seq(ncol(merged) - 2)]
      rv$sequence[[length(rv$sequence) + 1]] <- rv$sequence[[rv$activeFile]]
      names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_merged")
      initializeVariables()
    } else if (isFALSE(input$newFileMerge)) {
      rv$data[[rv$activeFile]] <- merged[, seq(ncol(merged) - 2)]
      names(rv$data)[rv$activeFile] <- paste0(names(rv$data)[rv$activeFile], "_merged")
    }
    rv$info[length(rv$data)] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), "Positive and negative mode merged: M/z tolerance ppm", input$merge_ppm, "and RT tolerance", input$merge_rt, "\n")
    rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
  })


  ## Principal Component Analysis ##
  #TODO
  observeEvent(input$run_pca1, {
    if (!is.null(rv$activeFile)) { 
      # cat("rv$activeFile is not NULL.\n")
      if (input$selectpca1 == "Unsaved data") {
        # cat("Input selectpca1 is 'Unsaved data'.\n")
        data <- rv$tmpData       # Set data to the temporary data
        seq <- rv$tmpSequence    # Set sequence to the temporary sequence
        # cat("Temporary data and sequence have been assigned.\n")
      } else { 
        # cat("Input selectpca1 is:", input$selectpca1, "\n")
        selectchoices <- paste(seq_along(rv$data), ": ", names(rv$data)) # Get the selected dataset
        # cat("Select choices are:", selectchoices, "\n")
        # Ensure rv$choices is up to date
        # cat("rv$choices are:", rv$choices, "\n")
        sd <- which(rv$choices %in% input$selectpca1) # Get the index of the selected dataset
        # cat("Selected dataset index (sd):", sd, "\n")
        data <- rv$data[[sd]]    # Set data to the selected dataset
        seq <- rv$sequence[[sd]] # Set sequence to the selected sequence
        # cat("Data and sequence have been assigned from rv$data and rv$sequence.\n")
      }
      
      if ("Sample"  %in% seq[, "labels"]) { # Check if the sequence file contains a "Sample" column
        if (any(seq[, "labels"] %in% "QC")) { # Check if the sequence file contains a "QC" column
          seq[seq[, "labels"] %in% "QC", "group"] <- "QC" # Set the "QC" column to "QC"
        } else {
          cat("No 'QC' labels found in the sequence.\n")
        }
        
        # make a print statement of the name of the dataset used
        # cat("Using dataset: ", names(rv$data)[sd], "\n")
        # make a print statement of all the sequence files 
        # cat("Sequence files: ", names(rv$sequence), "\n")
        # make a print statement of the name of the sequence used
        # cat("Using sequence: ", names(rv$sequence[[rv$activeFile]]), "\n")
        
        
        # check that data[,"Name"] exist
        name_exists <- "Name" %in% colnames(data)
        # Print the result as TRUE or FALSE
        # cat("Does the 'Name' column exist? ", name_exists, "\n")
        
        # Check if 'Name' column is unique
        is_unique <- length(unique(as.character(data[, "Name"]))) == length(as.character(data[, "Name"]))
        # Print the result as TRUE or FALSE
        # cat("Are all names unique? ", is_unique, "\n")
        
        # check if the 'Name' column is a empty string
        is_empty <- any(data[, "Name"] == "")
        # Print the result as TRUE or FALSE
        # cat("Is there an empty string in the 'Name' column? ", is_empty, "\n")
        
        # Check for duplicated names 
        is_duplicated <- any(duplicated(as.character(data[, "Name"])))
        # Print the result as TRUE or FALSE
        # cat("Are there duplicated names? ", is_duplicated, "\n")
        # Print which names are duplicated
        # cat("Duplicated names: \n")
        print(data[duplicated(as.character(data[, "Name"])), "Name"])
        
        data_subset <- data[seq[, "labels"] %in% c("Sample", "QC")] # Get the data for the samples and QC
        rownames(data_subset) <- make.unique(as.character(data[, "Name"])) # Make the rownames unique
        
        # Debugging to show the dimensions of the data_subset
        # cat("Data before PCA: \n")
        # print(class(data_subset))
        # print(str(data))
        # any(is.na(rownames(data)))
        # any(is.na(rownames(data)))
        # any(rownames(data) == "")
        # any(rownames(data) == "NA")
        # # print(str(data_subset[,1:6]))
        # # print(str(data_subset[,(ncol(data_subset)-5):ncol(data_subset)]))
        # print(dim(data_subset))
        
        seq_subset <- seq[seq[, "labels"] %in% c("Sample", "QC"), ] # Get the sequence for the samples and QC
        
        # Debugging to show the dimensions of the seq_subset
        # cat("Seq before PCA: \n")
        print(str(seq_subset))
        
        
        # Perform PCA once and save the results to pca_result
        pca_result <- pcaplot(data_subset, seq_subset, input$pca1_islog)
        
        # Generate a unique name for the PCA result based on the dataset name
        dataset_name <- names(rv$data)[sd]
        pca_name <- paste0(dataset_name, "_pca")
        pc_name <- paste0(dataset_name, "_PC")
        
        # Check if the PCA name already exists in rv$pca_results
        if (!(pca_name %in% names(rv$pca_results))) {
          # If the name does not exist, save the PCA and PC results
          rv$pca_results[[pca_name]] <- list(pca_df = pca_result$pca_df,
                                             PC_df = pca_result$PC_df)
        }
        
        output$plotpca1 <- renderPlotly({
          pca_result$pca_plotly
        })
        
        output$plotscree1 <- renderPlotly({
          pca_result$scree_plotly
        })
        
        if (sum(seq[, 1] %in% "QC") > 0) {
          qccv <- paste0("CV in QC samples: ", round(cvmean(data[seq[, 1] %in% "QC"]), 2), "</br>")
        } else {
          qccv <- "No QC in dataset </br>"
        }
        sclass <- seq[seq[, "labels"] %in% c("Sample", "QC"), ][, "group"] # Get the class of the samples and QC
        sclass <- sclass[sclass != "QC"]
        if (sum(!is.na(sclass)) > 0) {
          classcv <- sapply(sort(unique(sclass)), function(x) {
            round(cvmean(data_subset[, sclass %in% x]), 2)
          })
          classcv <- sapply(seq_along(classcv), function(x) {
            paste0("CV in group ", sort(unique(sclass))[x], ": ", classcv[x], "</br>")
          })
        } else {
          classcv <- NULL
        }
        text <- c(qccv, classcv)
        output$pca1Details <- renderUI({
          HTML(text)
        })
      }
    }
  })

  observeEvent(input$run_pca2, {
    selectchoices <- paste(seq_along(rv$data), ": ", names(rv$data))
    sd <- which(rv$choices %in% input$selectpca2)
    if ("Sample" %in% rv$sequence[[sd]][, 1]) {
      data <- rv$data[[sd]]
      seq <- rv$sequence[[sd]]
      shinyCatch(
        seq[seq[, 1] %in% "QC", ][, 4] <- "QC",
        blocking_level = 'message',
        shiny = FALSE
      )
      
      data_subset <- data[seq[, "labels"] %in% c("Sample", "QC")] # Get the data for the samples and QC
      rownames(data_subset) <- make.unique(as.character(data[, "Name"]))
      
      seq_subset <- seq[seq[, "labels"] %in% c("Sample", "QC"), ] # Get the sequence for the samples and QC
      
      # Save the PCA results to pca_results
      pca_result <- pcaplot(data_subset, seq_subset, input$pca1_islog)
      
      # Generate a unique name for the PCA result based on the dataset name
      dataset_name <- names(rv$data)[sd]
      pca_name <- paste0(dataset_name, "_pca")
      pc_name <- paste0(dataset_name, "_PC")
      
      # Check if the PCA name already exists in rv$pca_results
      if (!(pca_name %in% names(rv$pca_results))) {
        # If the name does not exist, save the PCA and PC results
        pca_result <- pcaplot(data_subset, seq_subset, input$pca1_islog)  # Perform PCA
        
        # Save the PCA and PC results as a named list for each PCA result
        rv$pca_results[[pca_name]] <- list(pca_df = pca_result$pca_df,
                                           PC_df = pca_result$PC_df)
      }
      
      # Debugging to show that rv$results is updated
      # cat("PCA results saved as:", pca_name, "\n")
      # cat("PCA results dimensions:", dim(rv$pca_results[[pca_name]]), "\n")
      # print(str(rv$pca_results[[pca_name]]))
      
      output$plotpca2 <- renderPlotly({
        pca_result$pca_plotly
      })
      
      output$plotscree2 <- renderPlotly({
        pca_result$scree_plotly
      })

      if (sum(seq$labels %in% "QC") > 0) {
        qccv <- paste0("CV in QC samples: ", round(cvmean(data[seq[, 1] %in% "QC"]), 2), "</br>")
      } else {
        qccv <- "No QC in dataset </br>"
      }
      sclass <- seq[seq[, 1] %in% c("Sample", "QC"), ][, 4]
      sclass <- sclass[sclass != "QC"]
      if (sum(!is.na(sclass)) > 0) {
        classcv <- sapply(sort(unique(sclass)), function(x) {
          round(cvmean(data_subset[sclass %in% x]), 2)
        })
        classcv <- sapply(seq_along(classcv), function(x) {
          paste0("CV in group ", sort(unique(sclass))[x], ": ", classcv[x], "</br>")
        })
      } else {
        classcv <- NULL
      }
      text <- c(qccv, classcv)
      output$pca2Details <- renderUI({
        HTML(text)
      })
    }
  })

  observeEvent(input$select_boxplot_1, { #TODO which rv choices -> function
    data <- rv$data[[which(rv$choices %in% input$select_boxplot_1)]]
    sequence <- rv$sequence[[which(rv$choices %in% input$select_boxplot_1)]]
    group <- input$select_boxplot_1_group
    data <- data[sequence[, 1] %in% "Sample" & sequence[, 4] %in% group]
    output$boxplot_1 <- renderPlot({
      boxplot(log2(data), main = input$select_boxplot_1, xlab = "Analyte", ylab = "Intensity")
    })
  }, ignoreInit = TRUE)

  observeEvent(input$drift_1, {
    rv$drift_plot_select <- 1
  })

  observeEvent(input$drift_2, {
    rv$drift_plot_select <- 2
  })

  observeEvent(input$drift_3, {
    rv$drift_plot_select <- 3
  })

  output$drift_ui <- renderUI({
    box(
      width = NULL,
      if (is.null(rv$activeFile)) {
        p("No data")
      } else if (input$drift_select != "None" && nrow(rv$data[[rv$activeFile]]) != nrow(rv$data[[which(rv$choices %in% input$drift_select)]])) {
        p("Not able to compare the selected datasets")
      } else if (input$drift_select == "None" && rv$drift_plot_select == 2) {
        p("Need dataset to compare with")
      } else if (is.null(input$dt_drift_panel_rows_selected) && rv$drift_plot_select == 1) {
        p("Select feature to plot")
      } else if (rv$drift_plot_select == 1) {
        lapply(seq_along(input$dt_drift_panel_rows_selected), function(i) {
          fluidRow(
            column(6, plotOutput(paste0("driftplotoutput", i), height = 280, width = "100%")),
            column(6, plotOutput(paste0("driftplotoutput2", i), height = 280, width = "100%"))
          )
        })
      } else if (rv$drift_plot_select == 2) {
        fluidRow(column(12, plotOutput("cvscatterplot", height = 600, width = "100%")))
      } else {
        p("Nothing here yet")
      }
    )
  })

  output$boxplot_ui <- renderUI({
    box(
      width = NULL,
      if (is.null(rv$activeFile)) {
        p("No data")
      } else if (is.null(input$dt_boxplot_panel_rows_selected)) {
        p("Select feature to plot")
      } else {
        lapply(seq_along(input$dt_boxplot_panel_rows_selected), function(i) {
          fluidRow(column(12, plotOutput(paste0("boxplotoutput", i), height = 280, width = "100%")))
        })
      }
    )
  })

  observe({
    if (length(input$dt_drift_panel_rows_selected) == 0 && rv$drift_plot_select == 1) {
      output$driftplotoutput1 <- renderPlot({
        NULL
      })
      output$driftplotoutput21 <- renderPlot({
        NULL
      })
    } else if (rv$drift_plot_select == 1) {
      for (i in 1:(length(input$dt_drift_panel_rows_selected) + 1)) {
        output[[paste0("driftplotoutput", i)]] <- renderPlot({
          NULL
        })
        output[[paste0("driftplotoutput2", i)]] <- renderPlot({
          NULL
        })
      }
      for (i in seq_along(input$dt_drift_panel_rows_selected)) {
        local({
          my_i <- i
          output[[paste0("driftplotoutput", my_i)]] <- renderPlot({
            driftplot(
              data = rv$data[[rv$activeFile]][input$dt_drift_panel_rows_selected[my_i], ],
              seq = rv$sequence[[rv$activeFile]]
            )
          })
        })
        if (input$drift_select != "None") {
          local({
            my_i <- i
            output[[paste0("driftplotoutput2", my_i)]] <- renderPlot({
              driftplot(
                data = rv$data[[which(rv$choices %in% input$drift_select)]][input$dt_drift_panel_rows_selected[my_i], ],
                seq = rv$sequence[[rv$activeFile]]
              )
            })
          })
        }
      }
    } else if (rv$drift_plot_select == 2) {
      output$cvscatterplot <- renderPlot({
        cvscatterplot(
          data = rv$data[[rv$activeFile]],
          data2 = rv$data[[which(rv$choices %in% input$drift_select)]],
          seq = rv$sequence[[rv$activeFile]],
          name1 = names(rv$data)[rv$activeFile],
          name2 = names(rv$data)[which(rv$choices %in% input$drift_select)]
        )
      })
    }
  })

  observe({
    if (length(input$dt_boxplot_panel_rows_selected > 0)) {
      for (i in seq_along(input$dt_boxplot_panel_rows_selected)) {
        local({
          my_i <- i
          output[[paste0("boxplotoutput", my_i)]] <- renderPlot({
            myboxplot(
              data = rv$data[[rv$activeFile]][input$dt_boxplot_panel_rows_selected[my_i], ],
              seq = rv$sequence[[rv$activeFile]],
              log = input$bloxplot_log,
              ylog = input$bloxplot_ylog
            )
          })
        })
      }
    }
  })
  
  # PCA results update 
  # Whenever pca_results are updated, update the selectInput choices for outlier detection
  observe({
    updateSelectInput(session, "kmeans_pca", choices = names(rv$pca_results))
  })
  
  observe({
    updateSelectInput(session, "hierarchical_pca", choices = names(rv$pca_results))
  })
  
  observe({
    updateSelectInput(session, "dbscan_pca", choices = names(rv$pca_results))
  })
  
  observe({
    updateSelectInput(session, "hdbscan_pca", choices = names(rv$pca_results))
  })
  
  observe({
    updateSelectInput(session, "optics_pca", choices = names(rv$pca_results))
  })
  
  observe({
    updateSelectInput(session, "lof_pca", choices = names(rv$pca_results))
  })
  
  # Outlier Detection 
  # kmeans
  observeEvent(input$compute_kmeans_eval, {
    # Fetch the PCA data from the reactive values
    pca_data <- rv$pca_results[[input$kmeans_pca]]  # Fetch the selected PCA result (list)
    
    # Check if PCA data is available and extract it
    if (!is.null(pca_data)) {
      pca_df <- pca_data[[1]]  # Extract the pca_df directly from the list
      
      # Get the evaluation method selected by the user
      method <- input$kmeans_eval_method
      
      # Check that the method is valid
      if (!is.null(method)) {
        # Use the selected evaluation method to create the plot
        eval_plot <- switch(method,
                            "wss" = fviz_nbclust(pca_df[, c("PC1", "PC2")], kmeans, method = "wss", k.max = nrow(pca_df) - 1) +
                              scale_x_discrete(breaks = seq(1, nrow(pca_df) - 1, by = 5)) +  # Reduce number of x-axis ticks
                              labs(title = "Optimal number of clusters (Elbow Method)") +
                              theme_bw(),
                            "silhouette" = fviz_nbclust(pca_df[, c("PC1", "PC2")], kmeans, method = "silhouette", k.max = nrow(pca_df) - 1) + 
                              scale_x_discrete(breaks = seq(1, nrow(pca_df) - 1, by = 5)) +  # Reduce number of x-axis ticks
                              labs(title = "Optimal number of clusters (Silhouette Method)") +
                              theme_bw(),
                            "gap_stat" = fviz_nbclust(pca_df[, c("PC1", "PC2")], kmeans, method = "gap_stat", k.max = nrow(pca_df) - 1) +
                              scale_x_discrete(breaks = seq(1, nrow(pca_df) - 1, by = 5)) +  # Reduce number of x-axis ticks
                              labs(title = "Optimal number of clusters (Gap Statistic Method)") +
                              theme_bw())
        
        # Render the evaluation plot in plotly
        output$kmeans_eval_plot <- renderPlotly(ggplotly(eval_plot))
      }
    }
  })
  
  observeEvent(input$run_kmeans, {
    # Fetch the selected PCA data
    pca_data <- rv$pca_results[[input$kmeans_pca]]  # Fetch the selected PCA result (list)
    
    if (!is.null(pca_data)) {
      pca_df <- pca_data[[1]]   # Extract pca_df from the list
      PC_df <- pca_data[[2]]    # Extract PC_df from the list
      k <- input$num_clusters   # Get the number of clusters (k)
      percentile_threshold <- input$percentile_threshold  # Get the percentile threshold
      
      # Check if pca_df is a valid data frame
      if (!is.null(pca_df) && is.data.frame(pca_df)) {
        # Call the kmeans_clustering function to get the results
        kmeans_results <- kmeans_clustering(pca_df, k, percentile_threshold, PC_df)
        
        # Generate a unique name for the K-means result from the selected PCA result name
        pca_result_name <- input$kmeans_pca   # This gives the selected name from the dropdown
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")  # Timestamp for uniqueness (date + time)
        kmeans_name <- paste0(pca_result_name, "_Kmeans_", timestamp)
        
        # Save the K-means result (including outlier information) into the reactive value
        rv$outlier_df[[kmeans_name]] <- list(kmeans_df = kmeans_results)
        
        # Render the K-means plot in the UI
        output$kmeans_plot <- renderPlotly({
          kmeans_results$kmeans_plotly  # Pass the interactive plotly object to renderPlotly
        })
        
        # Render the K-means outlier table in the UI
        output$kmeans_outliers <- renderDT({
          datatable(kmeans_results$kmeans_df, options = list(pageLength = 20, autoWidth = TRUE))
        })
        
      } else {
        cat("Selected PCA result is not a valid data frame.\n")
      }
    } else {
      cat("No PCA results selected or available.\n")
    }
  })
  
  # Hierarchical Clustering 
  # Observe event when the user clicks the "Run Hierarchical Clustering" button
  observeEvent(input$run_hierarchical, {
    # Fetch the selected PCA data from reactive values
    pca_data <- rv$pca_results[[input$hierarchical_pca]]  # Fetch the selected PCA result (list)
    sequence <- rv$sequence[[rv$activeFile]]  # Fetch the sequence data from the active file
    
    seq_subset <- sequence[sequence[, "labels"] %in% c("Sample", "QC"), ]  # Get the sequence for the samples and QC
    
    # Debugging to show the sequence data
    if (!"labels" %in% colnames(sequence)) {
      cat("Error: 'labels' column not found in sequence data.\n")
      return()
    }
    str(seq_subset)

    if (!is.null(pca_data)) {
      pca_df <- pca_data[[1]]   # Extract pca_df from the list
      PC_df <- pca_data[[2]]    # Extract PC_df from the list
      
      # Retrieve parameters from the UI
      method <- input$clustering_method   # Clustering method selected
      k <- input$num_clusters_hierarchical  # Number of clusters
      threshold <- input$threshold  # Dendrogram threshold
      
      # Debugging to show the parameters 
      cat("Clustering method: ", method, "\n")
      cat("Number of clusters: ", k, "\n")
      cat("Dendrogram threshold: ", threshold, "\n")
      print(head(seq_subset))
      
      # Perform the hierarchical clustering
      hierarchical_results <- perform_hierarchical_clustering(pca_df, seq_subset, method, k, threshold)
      
      # Render the hierarchical clustering plot
      output$hclust_plot <- renderPlotly({
        hierarchical_results$hclust_plot  # Pass the plotly object to renderPlotly
      })
      
      # Render the confusion matrix plot
      output$conf_matrix_plot <- renderPlotly({
        hierarchical_results$conf_matrix_plot  # Pass the plotly object to renderPlotly
      })
      
      # Render the dendrogram plot
      output$dendrogram_plot <- renderPlotly({
        hierarchical_results$dendrogram_plot  # Pass the plotly dendrogram to renderPlotly
      })
      
      # Render the outliers table
      output$hierarchical_outliers <- renderDT({
        datatable(hierarchical_results$hierarchical_outliers, options = list(pageLength = 20, autoWidth = TRUE))
      })
      
    } else {
      cat("No PCA results selected or available.\n")
    }
  })
  
  # DBSCAN Clustering
  # Compute kNN distance plot
  observeEvent(input$compute_knn, {
    req(rv$pca_results[[input$dbscan_pca]])  # Ensure PCA data is loaded
    pca_data <- rv$pca_results[[input$dbscan_pca]]  # Fetch the selected PCA result (list)
    pca_df <- pca_data[[1]]  # Extract pca_df from the list
    k <- input$knn
    
    # Debug print
    print(paste("Computing kNN distance plot with k =", k))
    
    output$knn_plot <- renderPlotly({
      perform_kNN_dist_plot(pca_df, k)
    })
  })
  
  # Run DBSCAN
  observeEvent(input$run_dbscan, {
    req(rv$pca_results[[input$dbscan_pca]])  # Ensure PCA data is loaded
    pca_data <- rv$pca_results[[input$dbscan_pca]]  # Fetch the selected PCA result (list)
    pca_df <- pca_data[[1]]  # Extract pca_df from the list
    
    eps <- input$eps
    min_pts <- input$min_pts_dbscan
    
    # Debug print
    print(paste("Running DBSCAN with eps =", eps, "and minPts =", min_pts))
    
    dbscan_res <- perform_dbscan_clustering(pca_df, eps, min_pts)

    dbscan_res$dbscan_outliers <- dbscan_res$dbscan_outliers %>%
      rename(Category = Outlier)
    
    output$dbscan_plot <- renderPlotly({
      dbscan_res$dbscan_plot
    })
    
    print(head(dbscan_res$dbscan_outliers))
    
    print(names(dbscan_res$dbscan_outliers))
    
    output$dbscan_outliers <- renderDT({
      datatable(dbscan_res$dbscan_outliers %>%
                  select(Sample, PC1, PC2, Cluster, Category), options = list(pageLength = 10))
    })
  })
  
  # HDBSCAN Clustering
  # Run HDBSCAN
  observeEvent(input$run_hdbscan, {
    # Ensure PCA data is loaded
    req(rv$pca_results[[input$hdbscan_pca]])
    
    # Fetch the selected PCA result
    pca_data <- rv$pca_results[[input$hdbscan_pca]]
    pca_df <- pca_data[[1]]  # Extract pca_df from the list
    
    min_pts <- input$min_pts_hdbscan  # Minimum number of points for HDBSCAN
    threshold <- input$threshold_hdbscan  # Outlier threshold
    
    # Debug print
    print(paste("Running HDBSCAN with minPts =", min_pts))
    
    # Perform HDBSCAN clustering
    hdbscan_res <- perform_hdbscan_clustering(pca_df, min_pts)
    
    # Process outliers from the HDBSCAN results
    hdbscan_outliers <- hdbscan_res$hdbscan_outliers
    
    # Add categorization based on OutlierScore and threshold
    if (nrow(hdbscan_outliers) > 0 && "OutlierScore" %in% names(hdbscan_outliers)) {
      hdbscan_outliers <- hdbscan_outliers %>%
        mutate(Category = ifelse(OutlierScore > threshold, "Outlier", "Inlier"))
    } else {
      showNotification("HDBSCAN results are empty or do not contain 'OutlierScore' column.", type = "error")
    }
    
    # Render HDBSCAN plot
    output$hdbscan_plot <- renderPlotly({
      hdbscan_res$hdbscan_plot
    })
    
    # Render outlier table
    output$hdbscan_outliers <- renderDT({
      datatable(hdbscan_outliers %>%
                  select(Sample, PC1, PC2, Cluster, Category), options = list(pageLength = 10))
    })
  })
  
  # OPTICS Clustering
  # Run OPTICS
  observeEvent(input$run_optics, {
    req(rv$pca_results[[input$optics_pca]])
    pca_data <- rv$pca_results[[input$optics_pca]]
    pca_df <- pca_data[[1]]  # Extract pca_df from the list
    
    # Debugging print
    
    min_pts <- input$min_pts_optics
    eps <- if (is.na(input$eps_optics)) NULL else input$eps_optics
    eps_cl <- input$eps_cl_optics
    
    print(paste("Running OPTICS with minPts =", min_pts,
                ", eps =", eps, ", and eps_cl =", eps_cl))
    
    optics_res <- perform_optics_analysis(pca_df, eps, min_pts, eps_cl)
    
    optics_outliers <- optics$optics_outliers
    
    output$optics_reachability_plot <- renderPlot({
      optics_res$reachability_plot()
    })
    
    output$reachability_plot_threshold <- renderPlot({
      optics_res$reachability_plot_threshold()
    })
    
    output$cluster_plot <- renderPlot({
      optics_res$cluster_plot()
    })
    
    output$optics_outliers <- renderTable({
      optics_res$optics_outliers
    })
  })
  
  # LOF Clustering
  # Run LOF
  observeEvent(input$run_lof, {
    req(rv$pca_results[[input$optics_pca]])
    pca_data <- rv$pca_results[[input$optics_pca]]
    pca_df <- pca_data[[1]]  # Extract pca_df from the list
    
    threshold <- input$lof_threshold
    min_pts <- input$lof_k
    
    lof_res <- calculate_and_plot_lof(pca_df, threshold = threshold, minPts = min_pts)
    
    lof_plot <- lof_res$lof_plotly
    lof_od_plot <- lof_res$lof_od_plotly
    lof_outliers <- lof_res$lof_outliers
    
    output$lof_plot <- renderPlotly({
      lof_res$lof_plotly
    })
    
    output$lof_od_plot <- renderPlotly({
      lof_res$lof_od_plotly
    })
    
    output$lof_outliers <- renderTable({
      lof_res$lof_outliers
    })
  })
  
  # Generate Heatmap
  observeEvent(input$run_heatmap, {
    # Check if there is an active file loaded
    if (!is.null(rv$activeFile)) { 
      # Select data and sequence based on user input
      if (input$selectpca1 == "Unsaved data") {
        data <- rv$tmpData
        seq <- rv$tmpSequence
        print("Using unsaved data:")
        print(head(data))
      } else {
        selected_dataset <- which(rv$choices %in% input$selectpca1)
        data <- rv$data[[selected_dataset]]
        seq <- rv$sequence[[selected_dataset]]
      }
      
      # Subset the data and sequence for samples
      data_subset <- data[seq[, "labels"] %in% c("Sample")] # Get the data for the samples and QC
      rownames(data_subset) <- make.unique(as.character(data[, "Name"])) # Make the rownames unique
      seq_subset <- seq[seq[, "labels"] %in% c("Sample"), ] # Get the sequence for the samples and QC
      
      # Debugging
      # print("Data subset preview:")
      # print(head(data_subset))
      print("Dimensions of data subset:")
      print(dim(data_subset))
      # print("Sequence subset preview:")
      # print(head(seq_subset))
      
      # Omit empty rows
      data_subset <- na.omit(data_subset)
      print("Dimensions of data subset:")
      print(dim(data_subset))
      # Omit
      
      # Retrieve user inputs for heatmap customization
      num_top_features <- input$num_top_features
      dendrogram_option <- input$dendrogram_option
      print(paste("User-selected number of top features:", num_top_features))
      print(paste("User-selected dendrogram option:", dendrogram_option))
      
      # Validate 'num_top_features' input
      if (num_top_features > nrow(data_subset)) {
        showNotification("Number of top features exceeds available features. Displaying all features.", type = "warning")
        print("Number of top features adjusted to match available features.")
        num_top_features <- nrow(data_subset)
      }
      
      # Generate and render full heatmap
      output$heatmap_plot <- renderPlotly({
        print("Generating full heatmap with all data.")
        create_heatmap(
          data_subset,
          gradient_col,
          title = "Heatmap of Full Data",
          dendrogram = dendrogram_option
        )
      })
      
      # Generate and render heatmap of top features
      output$selected_features_heatmap_plot <- renderPlotly({
        print("Selecting top features based on user input.")
        # Select top features based on user input
        selected_features <- select_top_features(data_subset, top_n = num_top_features)
        print("Top selected features:")
        print(selected_features)
        
        # Render heatmap for selected features
        print("Generating heatmap for top features.")
        create_heatmap(
          data_subset[selected_features, , drop = FALSE],
          gradient_col,
          title = "Heatmap of Selected Top Features",
          dendrogram = dendrogram_option
        )
      })
      
    } else {
      # Notification if no file is loaded
      showNotification("No data file loaded. Please load a file to generate heatmaps.", type = "error")
      print("No data file loaded. Please load a file to generate heatmaps.")
    }
  })
  
  
  # Generate Volcano Plot
  # observeEvent(input$run_volcano_plot, {
  #   req(cleaned_data(), cleaned_sequence())
  #   group1 <- input$group1
  #   group2 <- input$group2
  #   
  #   # Convert thresholds
  #   log2fc_threshold <- as.numeric(log2(input$log2fc_threshold))
  #   pval_threshold <- as.numeric(-log10(input$pval_threshold))
  #   
  #   if (group1 == group2) {
  #     showNotification("Please select two different groups for comparison.", type = "error")
  #     return(NULL)
  #   }
  #   
  #   results <- perform_statistical_testing(cleaned_data(), cleaned_sequence(), group1, group2)
  #   comparison_key <- paste(group2, "-", group1, sep = "")
  #   
  #   if (!comparison_key %in% names(results$comparison_dfs)) {
  #     showNotification("No significant results for the selected groups.", type = "warning")
  #     return(NULL)
  #   }
  #   
  #   volcano_df <- results$comparison_dfs[[comparison_key]]
  #   output$volcano_plot <- renderPlotly({ create_volcano_plot(volcano_df, group1, group2, log2fc_threshold, pval_threshold) })
  #   
  #   # Output the data table of upregulated and downregulated features
  #   output$volcano_table <- renderDT({
  #     up_down_regulated <- volcano_df %>%
  #       filter((`p adj` < pval_threshold & Log2FC > log2fc_threshold) | (`p adj` < pval_threshold & Log2FC < -log2fc_threshold)) %>%
  #       select(Metabolite, Comparison, `p adj`, Log2FC) # Select and reorder columns
  #     datatable(up_down_regulated, options = list(pageLength = 20, autoWidth = TRUE))
  #   })
  # })
  
  #### Pathway Enrichment Analysis 
  # Render the identifier column selection input based on selected dataset
  # Server code to update 'identifier_column' choices based on selected dataset
  observeEvent(input$select_enrichment_data, {
    # Ensure a dataset is selected
    req(input$select_enrichment_data)
    
    if (!is.null(rv$activeFile)) {
      if (input$select_enrichment_data == "Unsaved data") {
        data <- rv$tmpData  # Use the temporary data
      } else {
        # Get the index of the selected dataset
        sd <- which(rv$choices %in% input$select_enrichment_data)
        data <- rv$data[[sd]]  # Retrieve the selected dataset
      }
      
      # Extract column names from the selected dataset
      data_colnames <- colnames(data)
      
      # Update the 'identifier_column' select input with the new choices
      updateSelectInput(session, "identifier_column", choices = data_colnames)
    }
  })
  
  observeEvent(input$run_gather_identifiers, {
    # Ensure a dataset and column are selected
    req(input$select_enrichment_data, input$identifier_column)
    
    if (!is.null(rv$activeFile)) {
      if (input$select_enrichment_data == "Unsaved data") {
        data <- rv$tmpData       # Set data to the temporary data
        seq <- rv$tmpSequence    # Set sequence to the temporary sequence
      } else {
        sd <- which(rv$choices %in% input$select_enrichment_data) # Get the index of the selected dataset
        data <- rv$data[[sd]]    # Set data to the selected dataset
        seq <- rv$sequence[[sd]] # Set sequence to the selected sequence
      }
      
      # Get the column name for identifiers
      column_name <- input$identifier_column
      
      # Use subset_data function to subset the data
      subset <- subset_data(data, column_name)
      
      # Gather identifiers using the subsetted data
      gathered_identifiers <- gather_identifiers(subset, column_name)
      
      # Store the identifiers for later use
      rv$identifier_df <- gathered_identifiers
      
      # Merge data
      merged_data <- merge_data(data, gathered_identifiers, column_name)
      
      # Debugging
      print(intersect(names(data), names(gathered_identifiers)))
      
      # Debugging: Print the first 10 rows
      print(head(merged_data, 10))
      
      # Update the sequence
      updated_seq <- updateSequence(seq, merged_data)
      
      # Store the merged data and sequence temporarily
      rv$tmpData <- merged_data
      rv$tmpSequence <- updated_seq  # Update sequence if necessary
      # Debugging
      # print("Data columns:")
      # print(colnames(rv$tmpData))
      # print("Sequence row names:")
      # print(rownames(rv$tmpSequence))
      print("Are data column names and sequence row names identical?")
      print(identical(colnames(rv$tmpData), rownames(rv$tmpSequence)))
      
      # Call updateDataAndSequence with correct parameters
      updateDataAndSequence(
        notificationMessage = paste("Identifiers gathered from column:", column_name), # Notification message
        newFileInput = TRUE, # Change to TRUE = a new dataset or FALSE = update dataset
        suffix = "_id", # Add a suffix to the dataset name
        additionalInfo = NULL)  # Add any additional info if needed
      
      sendSweetAlert(session, "Success", "Identifiers gathered and data updated.", type = "success")
      
    }
  })
  
  observeEvent(input$run_enrichment_analysis, {
    print("Hallo world")
  })

  
  # Render the identifier count table
  output$identifier_count_table <- renderUI({
    req(nrow(rv$identifier_df) > 0)  # Ensure there are rows to display
    
    # Sum the non-NA values in each column
    identifier_counts <- sapply(rv$identifier_df, function(x) sum(!is.na(x)))
    
    # Build the HTML content with the added title
    html_content <- HTML(
      # Add the title
      paste0("<b>Total Identifiers retrieved:</b> ",
             nrow(rv$identifier_df), "<br><br>"),
      "<b>Identifiers Found:</b><br>",
      # Add the counts for each column
      paste(
        sapply(names(identifier_counts), function(col_name) { # Loop over column names and
          paste0(col_name, ": ", identifier_counts[col_name], "<br>")
        }),
        collapse = "" # Collapse the list of strings into a single string
      )
    )
    html_content
  })
  
  # Render the identifier table
  output$identifier_table <- renderDT({
    req(nrow(rv$identifier_df) > 0)  # Ensure there are rows to display
    datatable(rv$identifier_df, options = list(
      pageLength = 10,         # Number of rows per page
      autoWidth = TRUE,        # Automatically adjust column width
      scrollX = TRUE           # Enable horizontal scrolling
    ), class = "display nowrap")  # Ensures table doesn't wrap and uses full width
  })
  
  
  #### Summary of data
  observe({ 
    if (!is.null(rv$activeFile)) {
      seq <- rv$sequence[[rv$activeFile]]
      dat <- rv$data[[rv$activeFile]]
      blank_mv <- sum(is.na(dat[seq[, "labels"] %in% "Blank"])) +
                    sum(dat[seq[, "labels"] %in% "Blank"] == 0, na.rm = TRUE)
      qc_mv <- sum(is.na(dat[seq[, "labels"] %in% "QC"])) +
                    sum(dat[seq[, "labels"] %in% "QC"] == 0, na.rm = TRUE)
      sample_mv <- sum(is.na(dat[seq[, "labels"] %in% "Sample"])) +
                    sum(dat[seq[, "labels"] %in% "Sample"] == 0, na.rm = TRUE)

      data_subset <- dat[seq[, "labels"] %in% "Sample"]
      sclass <- seq[seq[, "labels"] %in% "Sample", ][, "group"]

      if (sum(seq$labels %in% "QC") > 0) {
        qccv <- paste0("CV in QC samples: ",
                      round(cvmean(dat[seq[, "labels"] %in% "QC"]), 2), "</br>")
      } else {
        qccv <- "No QC in dataset </br>"
      }

      if (sum(!is.na(sclass)) > 0) {
        classcv <- sapply(sort(unique(sclass)), function(x) {
          round(cvmean(data_subset[sclass %in% x]), 2)
        })
        classcv <- sapply(seq_along(classcv), function(x) {
          paste0("CV in group ", sort(unique(sclass))[x], ": ", classcv[x], "</br>")
        })
      } else {
        classcv <- NULL
      }
      text <- c(qccv, classcv)
      output$title <- renderText({
        HTML("<h3>", names(rv$data)[rv$activeFile], "</h3>")
      })
      output$info_ui <- renderUI({
        HTML(nrow(dat) - 1, " features.<br>",
             ncol(dat[seq[, "labels"] %in% "Sample"])," samples.<br>", 
             ncol(dat[seq[, "labels"] %in% "QC"]), " QC samples.<br>", 
             ncol(dat[seq[, "labels"] %in% "Blank"]), " Blank samples.<br>", "<br>", 
             
             sample_mv, " missing values in Samples<br>", 
             qc_mv, " missing values in QC samples<br>", 
             blank_mv, " missing values in Blank samples<br><br>")
      })
      output$cvinfo_ui <- renderUI({
        HTML(text)
      })

      # Update statistics select input options
      groups <- na.omit(seq[, 'group'])
      time <- na.omit(seq[, 'time'])

      group_inputs <- c("group1", "group2", "group1_time", "group2_time", "group1_polystest", "group2_polystest")
      for (x in group_inputs) {
        updateSelectInput(session, x, label = NULL, choices = groups)
      }

      time_inputs <- c("time1_time", "time2_time", "time1_polystest", "time2_polystest")
      for (x in time_inputs) {
        updateSelectInput(session, x, label = NULL, choices = time, selected = "")
      }
    }
  })

  ## Normalization ##

  observeEvent(input$normalize, {
     if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if(input$normMethod == "QC (PQN)" & sum(rv$sequence[[rv$activeFile]][, 1] %in% "QC") == 0) {
      sendSweetAlert(session = session, title = "Error", text = "No QC samples in dataset.", type = "error")
    } else if(input$normMethod == "Sample amount" & sum(complete.cases(rv$sequence[[rv$activeFile]][, 'amount'])) == 0) {
      sendSweetAlert(session = session, title = "Error", text = "No sample amount information in dataset.", type = "error")
    } else {
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]
      qualityControls <- data[, sequence[, 1] %in% "QC"] 

      normalizedData <- normalization(data, sequence, qualityControls, input$normMethod)

      data[, sequence[, 1] %in% c("QC", "Sample")] <- normalizedData

      rv$tmpData <- data
      rv$tmpSequence <- sequence

      updateSelectInput(session, "selectpca1", selected = "Unsaved data", 
            choices = c("Unsaved data", rv$choices))
      updateSelectInput(session, "select_enrichment_data", selected = "Unsaved data", 
                        choices = c("Unsaved data", rv$choices))
      output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = 
            list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      sendSweetAlert(title = "Success", text = paste0("Data normalized using ", input$normMethod), type = "success")
    }
  })

  observeEvent(input$saveNormalization, {
    additionalInfo <- paste("Normalized with", input$normMethod, " method")
    updateDataAndSequence("Normalize first", input$newFileNorm, "_normalized", additionalInfo)
  })

  ## Transformation ##

  observeEvent(input$transform, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if(input$logTransform == "None" & input$scaling == "None") {
      sendSweetAlert(session = session, title = "Warning", text = "No method selected.", type = "warning")
    } else {
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]

      transformed <- transformation(data, sequence, input$logTransform, input$scaling)

      data[, sequence[, 1] %in% c("QC", "Sample")] <- transformed
      rv$tmpData <- data
      rv$tmpSequence <- sequence
      
      output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      sendSweetAlert(session, title = "Success", text = "Data transformed.", type = "success")
    }
  })

  observeEvent(input$saveTransform, {
    additionalInfo <- paste(
        "Transformed with log transformation: ", input$logTransform,
        " and scaling: ", input$scaling
    )
    updateDataAndSequence("Transform first", input$newFileTransform, "_transformed", additionalInfo)
  })

  ## Statistical analysis ##

  observeEvent(input$testType, {
    sequence <- rv$sequence[[rv$activeFile]]
    enable("selectTest")
    switch(input$testType,
      GroupsUnpaired = {
        if(!any(complete.cases(sequence[, 4]))) {
          sendSweetAlert(session, "Oops!", "Invalid test. Provide information on different groups/conditions.", type = "error")
          disable("selectTest")
        }
      },
      GroupsPaired = {
        if(!any(complete.cases(sequence[, 4]))) {
          sendSweetAlert(session, "Oops!", "Invalid test. Provide information on different groups/conditions.", type = "error")
          disable("selectTest")
        }
        if(!any(complete.cases(sequence[, 6]))) {
          sendSweetAlert(session, "Oops!", "Invalid test. Indicate paired samples.", type = "error")
          disable("selectTest")
        }
      },
      GroupsTimeUnpaired = {
        if(!any(complete.cases(sequence[, 4]))) {
          sendSweetAlert(session, "Oops!", "Invalid test. Provide information on different groups/conditions.", type = "error")
          disable("selectTest")
        }
        if(!any(complete.cases(sequence[, 5]))) {
          sendSweetAlert(session, "Oops!", "Invalid test. Indicate time points.", type = "error")
          disable("selectTest")
        }
      },
      GroupsMultipleTime = {
        if(any(complete.cases(sequence[, 5])) & any(complete.cases(sequence[, 6]))) {
          sequence <- sequence[sequence[, 1] %in% "Sample" & complete.cases(sequence[, 4]), ]
          group_time <- getGroupTime(sequence)
          unique_values <- unique(group_time)
          combinations <- combn(unique_values, 2)
          valid_combinations <- combinations[, apply(combinations, 2, function(cols) is_valid_combination(cols[1], cols[2]))]
          contrasts <- generate_contrasts(as.matrix(valid_combinations)) # matrix bc if it's only 1 combination, valid_combinations is not a matrix and generate_contrasts fails
          updateCheckboxGroupInput(session, "contrasts", choices = contrasts, selected = NULL)
        } else {
          sendSweetAlert(session, "Oops!", "Invalid test. No paired samples or time points in dataset.", type = "error")
          disable("selectTest")
        }
      },
      CompareToReference = {
        if(!any(complete.cases(sequence[, 4]))) { #TODO or only one group
          sendSweetAlert(session, "Oops!", "Invalid test. Provide information on different groups/conditions.", type = "error")
          disable("selectTest")
        } else {
          updateSelectInput(session, "referenceGroup", label = NULL, choices = na.omit(sequence[, 'group']))
        }
      },
      {
         print('default')
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$selectTest, {
    data <- rv$data[[rv$activeFile]]
    sequence <- rv$sequence[[rv$activeFile]]
    switch(input$testType, 
      GroupsUnpaired = {
        if(input$group1 == input$group2) {
          sendSweetAlert(session, "Oops!", "Choose different groups to compare.", type="error")
        } else {
          results <- groupComparison(data, sequence, c(input$group1, input$group2))
          rv$results[[rv$activeFile]][[length(rv$results[[rv$activeFile]])+1]] <- results
          names(rv$results[[rv$activeFile]])[length(rv$results[[rv$activeFile]])] <- paste0(input$group1, "_vs_", input$group2)
        }
      },
      GroupsPaired = {
        if(input$group1 == input$group2) {
          sendSweetAlert(session, "Oops!", "Choose different groups to compare.", type="error")
        } else {
          results <- groupComparisonPaired(data, sequence, c(input$group1, input$group2))
          rv$results[[rv$activeFile]][[length(rv$results[[rv$activeFile]])+1]] <- results
          names(rv$results[[rv$activeFile]])[length(rv$results[[rv$activeFile]])] <- paste0(input$group1, "_vs_", input$group2)
        }
      },
      GroupsTimeUnpaired = {
          groups <- c(input$group1_time, input$group2_time)
          times <- c(input$time1_time, input$time2_time)
          groupTime <- paste(groups, times, sep = "_")
          print(groupTime)
          results <- groupComparisonTime(data, sequence, groups, times)
          rv$results[[rv$activeFile]][[length(rv$results[[rv$activeFile]])+1]] <- results
          names(rv$results[[rv$activeFile]])[length(rv$results[[rv$activeFile]])] <- paste0(input$group1, "_vs_", input$group2)
      },
      GroupsMultipleTime = { # multi-level in limma 
        data <- data[sequence[, 1] %in% c("Name", "Sample")]
        sequence <- sequence[sequence[, 1] %in% c("Sample"), ]

        group_time <- getGroupTime(sequence)
        group_time <- factor(group_time, exclude = NA)
        paired <- factor(sequence[, 'paired'],  exclude = NA)
        results <- pairedAnalysis(data, group_time, input$contrasts, paired)

        rv$results[[rv$activeFile]] <- results
      },
      CompareToReference = {
        data <- data[sequence[, 1] %in% c("Name", "Sample")]
        groups <- sequence[complete.cases(sequence[, 4]), 4]
        results <- referenceGroupComparison(data, as.numeric(input$referenceGroup), groups)
        rv$results[[rv$activeFile]][[length(rv$results[[rv$activeFile]])+1]] <- results
      },
      {
         print('default')
      }
    )
    # Render one table for each contrast
    output$results_ui <- renderUI({
      lapply(seq_along(rv$results[[rv$activeFile]]), function(i) {
        fluidRow(
          column(12, strong(names(rv$results[[rv$activeFile]])[i])),
          column(12, box(width = NULL, DTOutput(paste0("results", i))))
        )
      })
    })
    lapply(seq_along(rv$results[[rv$activeFile]]), function(i) {
      output[[paste0("results", i)]] <- renderDT(
        rv$results[[rv$activeFile]][[i]], options = list(scrollX = TRUE)
      )
    })
  })

  ## Send data to PolySTest and VSClust ##

  observeEvent(input$export_polystest, {
    sequence <- rv$sequence[[rv$activeFile]]
    tdata <- rv$data[[rv$activeFile]][, sequence[, 1] %in% c("Name",  "Sample")]
    groups <- c(input$group1_polystest, input$group2_polystest)
    time <- c(input$time1_polystest, input$time2_polystest)
    selected <- selectPolySTest(tdata, sequence, groups, time)
    PolySTestMessage <- prepareMessage2(selected$selected, selected$selected_sequence, time)
    js$send_message(url="http://computproteomics.bmb.sdu.dk:443/app_direct/PolySTest/", 
                    dat=PolySTestMessage, tool="PolySTest")
  })

  observeEvent(input$send_polystest, {
    sequence <- rv$sequence[[rv$activeFile]]
    tdata <- rv$data[[rv$activeFile]][, sequence[, 1] %in% c("Name",  "Sample")]
    tseq <- sequence[sequence[, 1] %in% c("Name",  "Sample"), ]
    time <- complete.cases(tseq[, 5])
    if(any(complete.cases(tseq[, 5]))) {
      time <- unique(tseq[complete.cases(tseq[, 5]), 5])
    } else {
      time <- c("")
    }
    PolySTestMessage <- prepareMessage2(tdata, tseq, time)
    js$send_message(url="http://computproteomics.bmb.sdu.dk:443/app_direct/PolySTest/", 
                    dat=PolySTestMessage, tool="PolySTest")
  })

  observeEvent(input$send_vsclust, {
    sequence <- rv$sequence[[rv$activeFile]]
    tdata <- rv$data[[rv$activeFile]][, sequence[, 1] %in% c("Name",  "Sample")]
    tseq <- sequence[sequence[, 1] %in% c("Name",  "Sample"), ]
    VSClustMessage <- prepareMessage2(tdata, tseq)
    js$send_message(url="http://computproteomics.bmb.sdu.dk/app_direct/VSClust/",
                    dat=VSClustMessage, tool="VSClust")
  })
  
  ###############
  # Lipid Heatmap
  ###############
  # Values is used for message display before and after data process
  values <- reactiveValues(runProcessClicked = FALSE)
  
  # When bottom clicked in interface, all the following will be processed
  observeEvent(input$run_process, {
    values$runProcessClicked <- TRUE
    
    # Required data files are loaded
    sequence <- rv$sequence[[rv$activeFile]]
    data <- rv$data[[rv$activeFile]]
    
    # Capture the number of rows before filtering, used to compare with the number of rows after filtering
    number_of_rows_before <- nrow(data)
    
    
    ###############
    # Data cleaning
    ###############
    # Removes noise, keeping only the name and length (e.g., "CAR 14:1'CAR'[M+H]+" becomes "CAR 14:1")
    data[, 1] <- sapply(data[, 1], extract_pattern)
    
    # 3. Apply the `remove_ether_lipids` function to filter out ether lipids (O- or P- prefixed lipids)
    data[, 1] <- sapply(data[, 1], remove_ether_lipids)
    
    # 2. Apply the `clean_lipid_name` function to remove content after semicolons inside parentheses
    data[, 1] <- sapply(data[, 1], clean_lipid_name)
    
    # 4. Apply the `format_strings` function to ensure the lipid names are properly formatted, adding parentheses around numbers if necessary
    data[, 1] <- sapply(data[, 1], format_strings)
    
    # Function to filter rows based on the specified pattern, meaning removes any data that are not on X(C:D) format.
    data <- filter_data_by_pattern(data)
    
    # This will make it possible to switch between original data and merged data. OG data: using _1 _2 ... _n. Merged will sum the values of the "duplicated" data. 
    if (input$selected_dataset == "original") {
      data <- unique_compound_names(data)
      
      # Merge duplicates
    }  else if (input$selected_dataset == "merged") {
      # Removes anything that are not part of the data of the samples and name. So it it possible to sum the rows. 
      data <- data[, sequence[ , 'labels'] %in% c("Name","Sample")]
      sequence <- sequence[sequence[ , 'labels'] %in% c("Name","Sample"), ]
      
      # Sums the duplicates (isoforms of the lipids)
      data <- merge_duplicates(data)
    }
    
    
    # Capture the number of rows after filtering
    number_of_rows_after <- nrow(data)
    
    # Calculate the number of rows removed
    rows_removed <- number_of_rows_before - number_of_rows_after
    output$rows_removed_text <- renderText({
      paste("Rows removed after data cleaning are:", rows_removed, ". The removal is be due to the names in the first column of the data file not being in the X(C:D) format. Keep in mind, that a merged data will also count as a removed row.")
    })
    
    
    # The following is used in the tab: 'Lipid summary'.
    #grouped_samples <- process_lipid_data(sequence, data) # not used?
    output$groups_table <- renderTable({
      
      # Extract the 'Sample' labels and corresponding 'group' from 'sequence'
      sample_rows <- sequence[sequence$labels == "Sample", ]
      unique_groups <- unique(sample_rows$group)
      
      # Create the dataframe to be displayed as a table
      lipid_df_processed_data <- data.frame(
        Group = unique_groups,
        Samples = sapply(unique_groups, function(group) {
          sample_identifiers <- rownames(sample_rows)[sample_rows$group == group]
          paste(sample_identifiers, collapse = ", ")
        })
      )
      # Return the data frame to be rendered as a table
      lipid_df_processed_data
    })
    
    
    
    # Heatmap input selection  
    observeEvent(input$run_process, {
      processed_results <- process_lipid_data(sequence, data)
      grouped_data_frames <<- create_grouped_data_frames(sequence, data)
      
      compound_names <- data[[1]]  # Extract the first column which contains compound names
      
      # Assuming that each grouped data frame has rows in the same order as "data"
      for (i in seq_along(grouped_data_frames)) {
        grouped_data_frames[[i]] <- cbind(Compound_Name = compound_names, grouped_data_frames[[i]])
      }
      
      # Extract unique group names from sequence
      unique_group_names <- unique(sequence[sequence$labels == "Sample", "group"])
      
      # Check if lengths of group names and grouped data frames match
      if (length(unique_group_names) == length(grouped_data_frames)) {
        # Apply the actual group names from the sequence file
        names(grouped_data_frames) <- unique_group_names
      } else {
        # If there's a mismatch, fallback to naming with numbers as before
        names(grouped_data_frames) <- paste("Group", seq_along(grouped_data_frames))
      }
      
      # Render the UI for group selection.
      output$select_group_ui_heatmap <- renderUI({
        column(
          title = "Select groups for Heatmap test",
          width = 12,
          
          
          tagList(
            selectInput("selected_group_for_numerator", "Select Group for numerator:",
                        choices = names(grouped_data_frames)),
            selectInput("selected_group_for_denominator", "Select Group for denominator:",
                        choices = names(grouped_data_frames)),
          )
        )
      })
      
      
      
      # Create interactive table for selected numerator group. Displayed at the starting page of the 'Lipid Heatmap'
      output$numerator_group_table <- DT::renderDataTable({
        req(input$selected_group_for_numerator) 
        # Create a copy of the data for display purposes
        display_data <- grouped_data_frames[[input$selected_group_for_numerator]]
        
        DT::datatable(
          display_data,  
          options = list(scrollX = TRUE)  # Enable horizontal scrolling
        )
      })
      
      # Create interactive table for selected denominator group
      output$denominator_group_table <- DT::renderDataTable({
        req(input$selected_group_for_denominator)  
        display_data <- grouped_data_frames[[input$selected_group_for_denominator]]
        
        DT::datatable(
          display_data,  
          options = list(scrollX = TRUE)  
        )
      })
      
      
      # Interface of selections of lipids to display
      output$select_lipid_ui <- renderUI({
        # Extract the lipid names from first column of the file 'data'
        lipid_names <<- group_lipids_by_group(data)
        
        selectizeInput("selected_lipid", "Select lipid(s) to display:",
                       choices = c("All", unique(lipid_names$group)),
                       multiple = TRUE,
                       options = list(placeholder = 'Choose lipids...',
                                      onInitialize = I('function() { this.setValue(""); }')))
      })
      
      # Reactive expression to track the number of selected lipids or the "All" selection
      selected_lipid_count <- reactive({
        # If "All" is selected, we could set this to a value that causes the default text size to be used
        if ("All" %in% input$selected_lipid) {
          return(Inf)  # 'Inf' is used here as a flag for "All"
        } else {
          return(length(input$selected_lipid))
        }
      })
      
      
      
      reactiveP_value <- reactive({
        req(input$selected_group_for_numerator, input$selected_group_for_denominator)
        
        numerator_data <- grouped_data_frames[[input$selected_group_for_numerator]]
        denominator_data <- grouped_data_frames[[input$selected_group_for_denominator]]
        
        # Ensure there is data to work with
        if (nrow(numerator_data) == 0 || nrow(denominator_data) == 0) {
          return(NULL)
        }
        
        # Initialize a vector to store the p-values
        p_values <- numeric(nrow(numerator_data))
        
        # Loop through each row to perform the t-test
        for (i in 1:nrow(numerator_data)) {
          # Extract the numerical values for numerator and denominator, excluding the first column
          num_values <- numerator_data[i, -1]
          denom_values <- denominator_data[i, -1]
          
          # Check if data is constant or contains NA values
          if (length(unique(num_values)) == 1 || length(unique(denom_values)) == 1 ||
              any(is.na(num_values)) || any(is.na(denom_values))) {
            p_values[i] <- NA  # Assign NA or another appropriate value
          } else {
            # Perform the t-test
            t_test_result <- t.test(num_values, denom_values)
            p_values[i] <- t_test_result$p.value
          }
        }
        
        # Apply Benjamini-Hochberg correction to p-values
        adjusted_p_values <- p.adjust(p_values, method = "BH")
        
        # Create a new data frame with 'Compound_Name', 'p_value', and 'padj'
        p_value_data <- data.frame(
          Compound_Name = numerator_data$Compound_Name,  
          p_value = p_values,
          padj = adjusted_p_values
        )
        
        # Filter p-values on adjusted_p_values based on ui input
        p_value_data <- p_value_data[!is.na(p_value_data$padj) & p_value_data$padj < input$p_value_adj, ]
        return(p_value_data)
      })
      
      # Reactive expression to calculate logFC
      reactiveLogFC <- reactive({
        # The required data input for the data handling. 
        req(input$selected_group_for_numerator, input$selected_group_for_denominator)
        
        # Define data input, makes it more readable 
        numerator_data <- grouped_data_frames[[input$selected_group_for_numerator]]
        denominator_data <- grouped_data_frames[[input$selected_group_for_denominator]]
        
        # Removes the fir column of the data, as it is not needed for the calculation.
        numerator_data <- numerator_data[, -1]
        denominator_data <- denominator_data[, -1]
        
        numerator_data_means <- rowMeans(numerator_data[, drop = FALSE], na.rm = TRUE) # Makes sure that the calculation is done even NA values are present. 
        denominator_data_means <- rowMeans(denominator_data[, drop = FALSE], na.rm = TRUE)
        numerator_data_means <- data.frame(numerator_data_means)
        denominator_data_means <- data.frame(denominator_data_means)
        
        # Extract the compound names, to add it to the data frame logFC, making sure they are sorted by compound names. 
        compound_names <- data[[1]]
        
        # Calculate logFC
        logFC_data <- log2((numerator_data_means + 1e-6) / (denominator_data_means + 1e-6))
        # Rename a single column
        colnames(logFC_data)[colnames(logFC_data) == "numerator_data_means"] <- "logFC"
        
        logFC <- data.frame(Compound_Name = compound_names, logFC = logFC_data)
        
        
        # Continue filtering based on lipid selection
        if (!"All" %in% input$selected_lipid) {
          logFC <- logFC[lipid_names$group %in% input$selected_lipid, ]
        }
        
        filtered_data <- logFC
        return(filtered_data) # Used for Heatmap display 
      })
      
      
      
      ##### Render UI for different thresholds within the app. 
      
      # Render UI for maximum p-value input
      output$p_value_max_ui <- renderUI({
        numericInput("p_value_max", 
                     "Maximum p-value:", 
                     value = 1, 
                     min = 0, 
                     step = 0.01)
      })
      
      # Render UI for maximum p-value input
      output$p_value_adj <- renderUI({
        numericInput("p_value_adj", 
                     "Maximum p-value_adj:", 
                     value = 1, 
                     min = 0, 
                     step = 0.01)
      })
      
      # Render UI for logFC input
      output$logFC_input_ui <- renderUI({
        tagList(
          numericInput("logFC_input", 
                       "Enter logFC threshold:", 
                       value = 0,
                       min = 0)
        )
      })
      
      # Add this in the UI section where other inputs are rendered
      output$min_lipids_per_class_ui <- renderUI({
        numericInput("min_lipids_per_class", 
                     "Minimum number of lipids per class:", 
                     value = 2, 
                     min = 1, 
                     step = 1)
      })
      
      
      
      # Output for logFC input UI
      output$logFC_scale_ui <- renderUI({
        numericInput(
          inputId = "logFC_scale_ui",
          label = "logFC scale bar input",
          value = 2.5,  # Default value
          min = 0,
          step = 0.1
        )
      })
      
      # Output UI for the logFC threshold input
      output$logFC_scale_ui_manuel <- renderUI({
        if (input$threshold_mode == 'manual') {
          numericInput(
            inputId = "logFC_scale_ui",
            label = "logFC Threshold",
            value = 2.5,  # Default value
            min = 0,
            step = 0.1
          )
        } else if (input$threshold_mode == 'dynamic') {
          value = filtre... ###### 
        }
      })
      
      
      
      
      #### Filtration within the app
      
      # Reactive expression to filter data based on p-value and logFC thresholds, plus the amount of lipids within their class.
      reactiveFilteredData <- reactive({
        logFC_data <- reactiveLogFC()  
        p_value_data <- reactiveP_value()
        
        filtered_data <- merge(logFC_data, p_value_data, by = "Compound_Name")
        
        # Apply initial filtering criteria
        filtered_data <- filtered_data %>%
          filter(!is.na(p_value) & p_value <= input$p_value_max) %>%
          filter(abs(logFC) >= input$logFC_input)
        
        # Check if filtered_data has rows after initial filtering
        if (nrow(filtered_data) == 0) {
          # No data to proceed with, return empty data frame
          return(filtered_data)
        }
        
        # Get lipid class mapping for all lipids in the original data
        names.mapping.all <- map_lipid_names(x = filtered_data[[1]])
        
        # Compute counts per class from the original data
        class_counts <- table(names.mapping.all$Class)
        class_counts_df <- as.data.frame(class_counts)
        colnames(class_counts_df) <- c("Class", "Count")
        
        # Set the threshold from user input
        min_lipids_per_class <- input$min_lipids_per_class
        
        # Get classes that meet the threshold
        classes_to_keep <- class_counts_df$Class[class_counts_df$Count >= min_lipids_per_class]
        
        # Map the class information to the filtered_data
        # Create a data frame of lipid names and class
        lipid_class_df <- data.frame(Compound_Name = filtered_data[[1]], Class = names.mapping.all$Class)
        
        # Merge the class information with filtered_data
        filtered_data <- merge(filtered_data, lipid_class_df, by = "Compound_Name", all.x = TRUE)
        
        # Now filter based on classes_to_keep
        filtered_data <- filtered_data[filtered_data$Class %in% classes_to_keep, ]
        
        # Check again if filtered_data has rows after class filtering
        if (nrow(filtered_data) == 0) {
          # No data to proceed with, return empty data frame
          return(filtered_data)
        }
        
        return(filtered_data)
      })
      
      
      # Warring message if no data meets the threshold, but does not show if 'all' lipids are selected.
      output$filteredDataWarning <- renderUI({
        filtered_data <- reactiveFilteredData()
        
        if (selected_lipid_count() == 0) {
          # No lipids selected, do not show any message
          return(NULL)
        } else if (nrow(filtered_data) == 0 && selected_lipid_count() >= 1) {
          # No data meets the filtering criteria and lipids are selected
          div(
            style = "color: red; font-weight: bold;",
            "Warning: No data meets the filtering criteria."
          )
        } else {
          # Data exists or no lipids are selected, no warning needed
          NULL
        }
      })
      
      
      # Heatmap plot 
      
      # Reactive expression for heatmap plot data and height
      heatmap_plot_data <- reactive({
        # Take the input from user in interface and change p-value and logFC
        filtered_data <- reactiveFilteredData()
        
        # Ensure the data is not NULL and has rows to plot
        req(nrow(filtered_data) > 0)
        
        # Map the lipid names (make sure it returns all necessary columns, including Lipid_Class)
        names.mapping <- map_lipid_names(x = filtered_data$Compound_Name)
        
        # Calculate the number of unique classes
        num_classes <- length(unique(names.mapping$Class))
        
        # Calculate number of rows in the facets
        ncol_facets <- 3  # Adjust based on your facet_wrap setting
        num_rows <- ceiling(num_classes / ncol_facets)
        
        # Set base height per row
        height_per_row <- 300  # Adjust as needed to match bubble plot scaling
        
        # Calculate total plot height
        total_plot_height <- num_rows * height_per_row
        
        # Ensure minimum and maximum height limits
        total_plot_height <- max(total_plot_height, 600)   # Minimum height
        total_plot_height <- min(total_plot_height, 4000)  # Maximum height
        
        # Use lipid_data in the heatmap_lipidome function
        heatmap_plot <- heatmap_lipidome(
          x = filtered_data[ , c("Compound_Name", "logFC")],
          names.mapping = names.mapping,
          class.facet = "wrap",
          x.names = "Compound_Name",
          fill.limits = c(-input$logFC_scale_ui, input$logFC_scale_ui),  # Use user input
          fill.midpoint = 0,
          melt.value.name = "logFC",
          scales = "free"
        ) +
          scale_fill_gradient2(
            low = input$low_color,
            mid = input$mid_color,
            high = input$high_color,
            midpoint = 0,
            limit = c(-input$logFC_scale_ui, input$logFC_scale_ui),  # Use user input
            space = "Lab",
            name = "logFC",
            guide = guide_colorbar(
              barwidth = 15, # Adjust width of color bar
              barheight = 1  # Adjust height of color bar
            )
          ) +
          facet_wrap(~ Class, scales = "free", ncol = ncol_facets) +
          theme(
            panel.background = element_rect(fill = input$panel_bg_color, color = "white"),
            strip.background = element_rect(fill = input$strip_bg_color, color = "white"),
            strip.text = element_text(color = input$strip_text_color, face = "bold", size = 16),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 20),
            legend.title = element_text(size = 18),
            legend.text = element_text(size = 14)
          )
        
        # Conditionally remove grid lines based on user input
        if (!input$show_grid) {
          heatmap_plot <- heatmap_plot +
            theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()
            )
        }
        
        # Return the plot and height
        list(plot = heatmap_plot, height = total_plot_height)
      })
      
      # Render the heatmap plot
      output$heatmapPlot <- renderPlot({
        heatmap_plot_data()$plot
      }, height = function() {
        heatmap_plot_data()$height
      })
      
      
      
      
      
      
      # Reactive expression for bubble plot data and height
      bubble_plot_data <- reactive({
        filtered_lipid_data <- reactiveFilteredData()
        
        # Ensure the data is not NULL and has rows to plot
        req(nrow(filtered_lipid_data) > 0)
        
        # Map the lipid names (make sure it returns all necessary columns, including Lipid_Class)
        names.mapping <- map_lipid_names(x = filtered_lipid_data$Compound_Name)
        
        # Filter out rows with any NA values
        filtered_lipid_data <- filtered_lipid_data[complete.cases(filtered_lipid_data), ]
        
        # Merge mapped names with filtered lipid data
        lipid_data <- cbind(
          filtered_lipid_data, 
          names.mapping[, c("Name.simple", "N.carbons", "N.double.bonds", "Class")]
        )
        
        # Define size bins for the p-values with appropriate labels
        lipid_data$size_category <- cut(
          lipid_data$p_value,
          breaks = c(-Inf, 0.00001, 0.0001, 0.001, 0.01, 0.05, 0.1, 0.2, Inf),
          labels = c("p ≤ 1e-5", "1e-5 < p ≤ 1e-4", "1e-4 < p ≤ 0.001", "0.001 < p ≤ 0.01",
                     "0.01 < p ≤ 0.05", "0.05 < p ≤ 0.1", "0.1 < p ≤ 0.2", "p > 0.2"),
          include.lowest = TRUE,
          right = TRUE  # Include the right end of intervals
        )
        
        # Remove any NA values in size_category
        lipid_data <- lipid_data[!is.na(lipid_data$size_category), ]
        
        # Define sizes for each category
        size_values <- c(20, 17, 15, 13, 11, 9, 8, 7)
        names(size_values) <- levels(lipid_data$size_category)
        
        # Calculate the number of unique classes
        num_classes <- length(unique(lipid_data$Class))
        
        # Calculate number of rows in the facets
        ncol_facets <- 3  # Adjust based on your facet_wrap setting
        num_rows <- ceiling(num_classes / ncol_facets)
        
        # Set base height per row
        height_per_row <- 300  # Adjust as needed to match heatmap scaling
        
        # Calculate total plot height
        total_plot_height <- num_rows * height_per_row
        
        # Ensure minimum and maximum height limits
        total_plot_height <- max(total_plot_height, 600)   # Minimum height
        total_plot_height <- min(total_plot_height, 4000)  # Maximum height
        
        
        bubble_plot <- ggplot(lipid_data, aes(x = N.carbons, y = N.double.bonds, size = size_category, 
                                              color = logFC)) +
          geom_point(alpha = 0.7) +
          scale_color_gradient2(
            low = "#4575b4",         
            mid = "white",          
            high = "#d73027",        
            midpoint = 0,            
            limit = c(-2.5, 2.5),    
            space = "Lab",           
            name = "logFC"
          ) +
          scale_size_manual(
            values = size_values,
            name = "p-value range"
          ) +
          facet_wrap(~ Class, scales = "free", ncol = ncol_facets) +
          
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
            axis.text.y = element_text(size = 10),
            strip.text = element_text(color = "black", face = "bold"),
            strip.background = element_rect(fill = "#3483d1", color = "white"),
            panel.background = element_rect(fill = "#D3D3D3", color = "white"),
            panel.grid.major = element_line(color = "grey90"),
            panel.grid.minor = element_blank(),
            legend.position = "top",
            legend.box = "vertical",
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            panel.spacing = unit(1, "lines")  # Increase spacing between facets
          )
        
        # Convert ggplot to plotly object with dynamic height
        p <- ggplotly(bubble_plot, width = 1000, height = total_plot_height)
        
        # Modify the colorbar position
        for (i in seq_along(p$x$data)) {
          if (!is.null(p$x$data[[i]]$marker$colorbar)) {
            p$x$data[[i]]$marker$colorbar$orientation <- "h"
            p$x$data[[i]]$marker$colorbar$x <- 0.5
            p$x$data[[i]]$marker$colorbar$y <- 1.05
            p$x$data[[i]]$marker$colorbar$xanchor <- "center"
            p$x$data[[i]]$marker$colorbar$yanchor <- "bottom"
            p$x$data[[i]]$marker$colorbar$lenmode <- "pixels"
            p$x$data[[i]]$marker$colorbar$len <- 250  # Adjust length
            p$x$data[[i]]$marker$colorbar$thickness <- 10
          }
        }
        
        # Adjust the legend position and layout to reduce spacing
        p <- p %>%
          layout(
            legend = list(
              orientation = "h",  # Keep the legends stacked vertically
              x = 0.5,
              y = 1.05,  # Move the legend slightly lower to reduce the top margin
              xanchor = "center",
              yanchor = "top"
            ),
            margin = list(t = 50, b = 50),  # Reduce top and bottom margins
            barmode = list(
              len = 0.5,  # Adjust the length of the color bar
              thickness = 10,
              y = 0.95,  # Move color bar closer to the legend
              xanchor = "left",
              yanchor = "top"
            )
          )
        
        # Return the plot and height
        list(plot = p, height = total_plot_height)
      })
      
      # Render the bubble plot
      output$bubble_plot <- renderPlotly({
        bubble_plot_data()$plot
      })
      
      # UI for dynamic height
      output$bubble_plot_ui <- renderUI({
        plot_data <- bubble_plot_data()
        plot_height <- plot_data$height
        
        plotlyOutput("bubble_plot", width = "100%", height = paste0(plot_height, "px"))
      })
      
      
      # IF user want to split the screen, the following will be shown. Heatmap and table side by side.
      output$visualization_ui <- renderUI({
        if (input$split_screen) {
          # Show both heatmap and table side by side
          fluidRow(
            column(width = 6,
                   div(style = "width: 100%; height: 800px; overflow-y: scroll;",  
                       withSpinner(plotOutput("heatmapPlot", width = "100%", height = "800px"))
                   )
            ),
            column(width = 6,
                   div(style = "width: 100%; height: 800px; overflow-y: scroll;",  
                       withSpinner(dataTableOutput("pValueTable_2"))
                   )
            )
          )
        } else {
          # Show only heatmap
          div(style = "width: 100%; height: 2000px; overflow-y: scroll;",  
              withSpinner(plotOutput("heatmapPlot", width = "100%", height = "2000px"))
          )
        }
      })
      
      
      
      # Reactive expression to calculate lipid group summary and total lipid count
      lipid_summary <- reactive({
        lipid_group_df <- as.data.frame(table(group_lipids_by_group(data)$group)) 
        colnames(lipid_group_df) <- c("Lipid group", "Count")
        
        # Calculate and add percentage of total lipids
        lipid_group_df$`Percentage of Total` <- round((lipid_group_df$Count / sum(lipid_group_df$Count)) * 100, 2)
        
        return(lipid_group_df)  # Return the dataframe for use in other parts
      })
      
      # Function to generate lipid group summary and percentage for the table
      output$lipid_group_count <- DT::renderDataTable({
        lipid_group_df <- lipid_summary()  # Use the reactive expression
        
        # Render the table
        DT::datatable(lipid_group_df, options = list(pageLength = 5, autoWidth = TRUE))
      })
      
      # Render total lipids as a text output
      output$lipid_total <- renderText({
        lipid_group_df <- lipid_summary()  # Use the reactive expression
        lipidsum <- sum(lipid_group_df$Count)
        paste("Total lipid count in table (After data cleaning, and are not affect by threshold):", lipidsum)
      })
      
      
      # render table with "Compound_Name", "Original.annotation", "logFC", "p_value", "padj"
      output$pValueTable <- renderDataTable({
        filtered_data <- reactiveFilteredData()
        req(data)
        
        # Ensure 'Compound_Name' exists in 'data'
        if (!"Compound_Name" %in% colnames(data)) {
          data$Compound_Name <- data[, 1]  # Assuming the first column contains compound names
        }
        
        # Ensure 'Compound_Name' exists in 'filtered_data'
        if (!"Compound_Name" %in% colnames(filtered_data)) {
          stop("'Compound_Name' column not found in 'filtered_data'")
        }
        
        # Check if 'Original annotation' exists in 'data'
        if ("Original annotation" %in% colnames(data)) {
          # Rename 'Original annotation' to 'Original.annotation' in 'data'
          colnames(data)[colnames(data) == "Original annotation"] <- "Original.annotation"
          
          # Merge 'filtered_data' with 'Original.annotation' from 'data' based on 'Compound_Name'
          merged_data <- merge(
            filtered_data,
            data[, c("Compound_Name", "Original.annotation")],
            by = "Compound_Name",
            all.x = TRUE
          )
          
          # Create dataTableToShow including 'Original.annotation'
          dataTableToShow <- merged_data[, c("Compound_Name", "Original.annotation", "logFC", "p_value", "padj")]
        } else {
          # 'Original annotation' column not found, proceed without it
          dataTableToShow <- filtered_data[, c("Compound_Name", "logFC", "p_value", "padj")]
        }
        
        # Round numeric columns
        dataTableToShow$logFC <- round(dataTableToShow$logFC, 5)
        dataTableToShow$p_value <- round(dataTableToShow$p_value, 5)
        dataTableToShow$padj <- round(dataTableToShow$padj, 5)
        
        # Render the DataTable
        datatable(dataTableToShow, options = list(pageLength = 10, scrollX = TRUE))
      })
      
      # This table is smaler and does not contain the 'Original annotation' column, is used to split the screen with table and heatmap
      output$pValueTable_2 <- renderDataTable({
        filtered_data <- reactiveFilteredData()
        
        
        dataTableToShow <- filtered_data[, c("Compound_Name", "logFC", "p_value", "padj")]
        
        # Round 'logFC' and 'p_value' to the desired number of decimal places
        dataTableToShow$logFC <- round(dataTableToShow$logFC, 5)      # 5 decimal places for logFC
        dataTableToShow$p_value <- round(dataTableToShow$p_value, 5)  # 5 decimal places for p-value
        dataTableToShow$padj <- round(dataTableToShow$padj, 5)  # 5 decimal places for p-value
        
        
        # Render the selected data in a DataTable
        datatable(dataTableToShow, options = list(pageLength = 10, scrollX = TRUE))
      })
      
      
      
      
      
      # Observe the action button to open the modal dialog
      observeEvent(input$download_heatmap_btn, {
        showModal(modalDialog(
          title = "Download Heatmap Image",
          selectInput("modal_image_format", "Select Image Format", choices = c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg")),
          numericInput("modal_image_dpi", "Image Resolution (DPI)", value = 300, min = 72, step = 72),
          footer = tagList(
            modalButton("Cancel"),
            downloadButton("modal_download_heatmap", "Download")
          )
        ))
      })
      
      
      
      
      # Reactive expression to calculate plot dimensions
      plot_dimensions <- reactive({
        # Take the input from user in interface and change p-value and logFC
        filtered_data <- reactiveFilteredData()
        
        # Ensure the data is not NULL and has rows to plot
        req(nrow(filtered_data) > 0)
        
        # Map the lipid names (make sure it returns all necessary columns, including Lipid_Class)
        names.mapping <- map_lipid_names(x = filtered_data$Compound_Name)
        
        # Calculate the number of unique classes
        num_classes <- length(unique(names.mapping$Class))
        
        # Calculate number of rows in the facets
        ncol_facets <- 3  # Adjust based on your facet_wrap setting
        num_rows <- ceiling(num_classes / ncol_facets)
        
        # Set base dimensions per row/column
        height_per_row <- 3  # in inches
        width_per_col <- 4   # in inches
        
        # Calculate total plot dimensions
        total_height <- num_rows * height_per_row
        total_width <- ncol_facets * width_per_col
        
        # Ensure minimum and maximum limits
        total_height <- max(total_height, 6)   # Minimum height in inches
        total_height <- min(total_height, 40)  # Maximum height in inches
        total_width <- max(total_width, 8)     # Minimum width in inches
        total_width <- min(total_width, 40)    # Maximum width in inches
        
        # Return the dimensions
        list(height = total_height, width = total_width)
      })
      
      
      
      # Download handler for the heatmap plot from the modal dialog
      output$modal_download_heatmap <- downloadHandler(
        filename = function() {
          paste("heatmap_plot_", Sys.Date(), ".", input$modal_image_format, sep = "")
        },
        content = function(file) {
          # Generate the plot
          heatmap_plot <- heatmap_plot_data()$plot
          
          # Get plot dimensions
          dims <- plot_dimensions()
          total_height <- dims$height  # in inches
          total_width <- dims$width    # in inches
          
          # Save the plot with user-specified format and resolution
          ggsave(
            filename = file,
            plot = heatmap_plot,
            device = input$modal_image_format,
            width = total_width,    # Width in inches
            height = total_height,  # Height in inches
            units = "in",
            dpi = input$modal_image_dpi
          )
          
          # Close the modal dialog after download
          removeModal()
        }
      )
      
      
      
      # Message shown when hovering over Original data and merged data.
      observe({
        addTooltip(session, "selected_dataset", 
                   "Choose 'Original Data' to work with the data as it was initially collected. Select 'Merged Data' for a combined and cleaned dataset.", 
                   placement = "bottem", 
                   trigger = "hover")
      })
      
      # Message shown when hovering over logFC. 
      observe({
        addTooltip(session, "logFC_input_ui", 
                   "Displays lipids where the absolute value of logFC is greater than or equal to the threshold. For example, entering '1' will include lipids with logFC ≥ 1 or logFC ≤ -1.", 
                   placement = "bottom", 
                   trigger = "hover")
      })
      
      
      # Tooltip for p-value max input
      observe({
        addTooltip(session, "p_value_max_ui", 
                   "Displays lipids where the p-value is less than or equal to the threshold.", 
                   placement = "bottom", 
                   trigger = "hover")
      })
      
      # Tooltip for p-value adjusted input
      observe({
        addTooltip(session, "p_value_adj", 
                   "Displays lipids where the adjusted p-value is less than or equal to the threshold.", 
                   placement = "bottom", 
                   trigger = "hover")
      })
      
      # Tooltip for minimum lipids per class input
      observe({
        addTooltip(session, "min_lipids_per_class_ui", 
                   "Includes lipid classes that have at least the specified minimum number of lipids.", 
                   placement = "bottom", 
                   trigger = "hover")
      })
      
      
      
      
      
    })
  }) # This finishes the first 'observeEvent' when 'Run data processing' is clicked
  
  
  # Outside of the observeEvent, based on whether runProcessClicked is TRUE or FALSE, the message display will be placed on this: 
  # For the first message, which is placed in the 'Lipid Heatmap' tab.
  output$table_message <- renderUI({
    if (!values$runProcessClicked) {
      HTML('<p>Make sure sequences file is uploaded, when uploaded: Press "Run Data Processing" to get a display of data</p>')
    }
  })
  
  # For the second message, which is placed in the 'Table' tab.
  output$table_message_2 <- renderUI({
    if (!values$runProcessClicked) {
      HTML('<p>Make sure sequences file is uploaded, when uploaded: Press "Run Data Processing" to get a display of data</p>')
    }
  })
  
  # For the third message, which is placed in the 'Bubble Plot' tab.
  output$table_message_3 <- renderUI({
    if (!values$runProcessClicked) {
      HTML('<p>Make sure sequences file is uploaded, when uploaded: Press "Run Data Processing" to get a display of data</p>')
    }
  })
  
  # Outside of the observeEvent, so the message both are shown before and after runProcessClicked is clicked. 
  observe({
    addTooltip(session, "selected_dataset", 
               "Choose 'Original Data' to work with the data as it was initially collected, all sum isoforms is within the data. Select 'Merged Data' for a combined dataset, meaning isoforms lipids are summed togehter.", 
               placement = "bottom", 
               trigger = "hover")
  })
  
  
  
  # User guide inside 'Heatmap'
  observeEvent(input$show_lipid_info, {
    showModal(modalDialog(
      title = "Lipid Summary",
      textOutput("rows_removed_text"),
      textOutput("lipid_total"),  # Display the total number of lipids
      
      
      dataTableOutput("lipid_group_count"),  # Lipid group count table
      
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  
  observeEvent(input$show_lipid_cal, {
    showModal(modalDialog(
      title = "Lipid Calculation",
      div(
        tags$b("logFC: "), 
        withMathJax("$$\\log_2\\left( \\frac{\\text{mean of group numerator} + 10^{-6}}{\\text{mean of group denominator} + 10^{-6}} \\right)$$")
      ),
      div(
        tags$b("logFC Explanation: "), 
        "+10^-6 is added to avoid division by zero. The logFC is calculated for each lipid, 
      comparing the mean of the numerator group to the mean of the denominator group, 
      ensuring all sample values are taken into account."
      ),
      div(
        tags$b("p-value: "), 
        "The p-value is calculated using Welch’s t-test, comparing the raw data between the two groups for each lipid. 
      The data used is not filtered by p-value or logFC thresholds beforehand, ensuring an unbiased comparison."
      ),
      div(
        tags$b("Adjusted p-value (p-adj): "), 
        "P-values are adjusted for multiple comparisons using the Benjamini-Hochberg (BH) method. 
      This controls the false discovery rate (FDR), reducing the likelihood of false positives when testing multiple lipids simultaneously."
      ),
      
      div(
        tags$b("Packages Used: "),
        "This analysis utilizes the 'lipidomeR' package version 0.1.2."
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  
  
  
})