##*****************************##
##  Haplofun: server function  ##
##*****************************##
## Project: haplofun
## Last modification 06.2026             
## Creation: 2023

######------  helper ------######
# uplaod limits
options(shiny.maxRequestSize = 1500*1024^2)
FILESIZE_THRESHOLD_BYTES <- 500 * 1024 

# get variant names as ID or position
get_variant_names <- function(vcf_obj) {
  ids <- getID(vcf_obj)
  is_missing <- is.na(ids) | ids == "."
  if (sum(is_missing) / length(ids) > 0.9) {
    return(paste0(getCHROM(vcf_obj), "_", getPOS(vcf_obj)))
  } else {
    return(ids)
  }
} 
# upper triangle function for LD heatmap
get_upper_tri <- function(x){
  x[lower.tri(x)] <- NA
  return(x)
}

######------  server ------###### 
server <- function(input, output, session) {

  # fade out loading once session is ready
  shinyjs::delay(400, shinyjs::hide(id = "app-loading-overlay", anim = TRUE, animType = "fade", time = 0.6))
  # initialize vcf object
  vcf_data_store <- reactiveVal(NULL)
  
  
  ####---------------------------  login module ---------------------------####
  # login modal
  login_modal <- function() {
    modalDialog(
      title = "Login required for sensitive data",
      textInput("login_user", "Username"),
      passwordInput("login_password", "Password"),
      actionButton("login_button", "Log In", class = "btn-primary"),
      hr(),
      p("No account yet?"),
      actionButton("show_register_modal", "Register"),
      actionButton("login_back_main", "Back", class = "btn-secondary"),
      footer = NULL
    )
  }
  # registration modal
  register_modal <- function() {
    modalDialog(
      title = "Registration (no email required)",
      textInput("reg_user", "Chosen username"),
      passwordInput("reg_password_1", "Choose password"),
      passwordInput("reg_password_2", "Confirm password"),
      actionButton("register_button", "Complete registration", class = "btn-success"),
      actionButton("back_to_login", "Back to login"),
      footer = NULL
    )
  }
  # access button
  observeEvent(input$access_button, {
    if (input$data_choice == "Private, sensitive human data (login/registration required)") {
      showModal(login_modal())
    } else {
      session_status$data_scope <- "public"
      session_status$logged_in <- FALSE
      updateTabsetPanel(session, "sidebarTabs", selected = "dataInput")
    }
  })
  
  # registration and login
  observeEvent(input$show_register_modal, {
    removeModal()
    showModal(register_modal())
  })
  observeEvent(input$back_to_login, {
    removeModal()
    showModal(login_modal())
  })
  observeEvent(input$login_back_main, {
    removeModal()  
    updateTabsetPanel(session, "sidebarTabs", selected = "main") 
  })
  
  # execute registration 
  observeEvent(input$register_button, {
    req(input$reg_user, input$reg_password_1, input$reg_password_2)
    
    if (input$reg_password_1 != input$reg_password_2) {
      showModal(modalDialog(title = "Error", "Passwords do not match.", footer = modalButton("OK")))
      return()
    }
    
    reg_result <- register_user(input$reg_user, input$reg_password_1)
    
    if (reg_result == "success") { 
      removeModal()
      showModal(modalDialog(title = "Successful",
                            paste0("Registration of ", input$reg_user, " successful! Please log in."),
                            footer = modalButton("OK")))
    } else if (reg_result == "existing") { 
      showModal(modalDialog(title = "Error", "Username already taken.", footer = modalButton("OK")))
    }
  })
  
  # execute login 
  observeEvent(input$login_button, {
    if (check_credentials(input$login_user, input$login_password)) {
      # login successful
      session_status$logged_in <- TRUE
      session_status$user <- input$login_user
      session_status$data_scope <- "private" 
      removeModal()
      updateTabsetPanel(session, "sidebarTabs", selected = "dataInput")
    } else {
      # login failed
      showModal(modalDialog(title = "Error", "Invalid username or password.", footer = modalButton("OK")))
    }
  })
  
  # logout
  output$logout_ui <- renderUI({
    if (session_status$logged_in) {
      actionButton("logout_button", "Logout", icon = icon("sign-out-alt"))
    }
  })
  
  # button click
  observeEvent(input$logout_button, {
    session_status$logged_in <- FALSE
    session_status$user <- NULL
    session_status$data_scope <- "public" 
    updateTabsetPanel(session, "sidebarTabs", selected = "dataInput")
  })
  
  # dynamic header 
  output$upload_header <- renderUI({
    if (session_status$data_scope == "private") {
      h3(icon("upload"), "Protected mode: Processing private human data (login successful)")
    } else {
      h3(icon("upload"), "Public mode: Processing non-sensitive data (no login required)")
    }
  })
  
  # display of session mode
  output$session_mode_display <- renderPrint({
    if (session_status$data_scope == "private") {
      cat(paste("PRIVATE/SENSITIVE DATA - Authorized user:", session_status$user))
    } else {
      cat("PUBLIC/NON-SENSITIVE DATA")
    }
  })
  
  # display of login status
  output$login_status_display <- renderPrint({
    if (session_status$logged_in) {
      cat("LOGGED IN")
    } else {
      cat("NOT LOGGED IN")
    }  
  })
  ####---------------------------------------------------------------------####
  
  ####---------------------------  read-in files ---------------------------####
  # check for presence of vcf file
  output$vcfFileUploaded <- reactive({
    !is.null(input$file) || isTRUE(input$vcfStored_1) || isTRUE(input$vcfStored_3)
  })
  outputOptions(output, "vcfFileUploaded", suspendWhenHidden = FALSE)
  isvcfDataSelected <- reactive({
    !is.null(input$file) || isTRUE(input$vcfStored_1) || isTRUE(input$vcfStored_3)
  })
  
  # read vcf file
  vcfRInput <- reactive({
    req(isvcfDataSelected())

    read_and_validate <- function(path) {
      notify_err <- function(msg) {
        showNotification(msg, type = "error", duration = 15)
        req(FALSE)
      }

      # pre-check before calling read.vcfR to avoid crashes on input
      first_line <- tryCatch(
        readLines(path, n = 1, warn = FALSE),
        error = function(e) character(0)
      )

      vcf <- tryCatch(
        withCallingHandlers(
          vcfR::read.vcfR(path, verbose = FALSE),
          warning = function(w) invokeRestart("muffleWarning")
        ),
        error = function(e) {
          showNotification(paste("Could not read VCF file:", conditionMessage(e)),
                           type = "error", duration = 15)
          NULL
        }
      )
      if (is.null(vcf)) req(FALSE)

      if (is.null(vcf@fix) || nrow(vcf@fix) == 0)
        return(notify_err("VCF file contains no variants."))
      if (is.null(vcf@gt) || ncol(vcf@gt) < 2)
        return(notify_err("VCF file has no sample genotype columns."))
      if (!any(grepl("GT", vcf@gt[seq_len(min(5, nrow(vcf@gt))), "FORMAT"])))
        return(notify_err("VCF file has no GT (genotype) field"))

      gt_sample <- gsub(":.*", "",
                        as.vector(vcf@gt[seq_len(min(20, nrow(vcf@gt))), -1, drop = FALSE]))
      is_diploid <- any(grepl("/", gt_sample, fixed = TRUE) | grepl("|", gt_sample, fixed = TRUE))
      if (is_diploid && !any(grepl("|", gt_sample, fixed = TRUE)))
        return(notify_err("VCF file contains only unphased diploid genotypes ('/'). Haplotype analysis requires phased data (alleles separated by '|')."))

      gt_all <- gsub(":.*", "", as.vector(vcf@gt[, -1, drop = FALSE]))
      n_missing <- sum(grepl("^\\./\\.|^\\.$", gt_all))
      if (n_missing > 0) {
        pct <- round(100 * n_missing / length(gt_all), 1)
        showNotification(
          paste0(n_missing, " genotype(s) (", pct, "%) are missing (./.). These will appear as gaps in the analysis."),
          type = "warning", duration = 10
        )
      }

      # check for duplicate CHROM:POS (split multiallelic rows)
      chrom_pos <- paste0(vcf@fix[, "CHROM"], ":", vcf@fix[, "POS"])
      dup_pos   <- chrom_pos[duplicated(chrom_pos)]
      if (length(dup_pos) > 0) {
        n_dup <- length(unique(dup_pos))
        return(notify_err(paste0(
          "VCF file contains ", n_dup, " position(s) with duplicate CHROM:POS entries ",
          "(e.g. ", dup_pos[1], "). This indicates multiallelic sites were split into ",
          "separate rows (e.g. with 'bcftools norm -m-'). ",
          "Haplotype analysis requires one row per position. ",
          "Please re-merge and filter with: ",
          "bcftools norm -m+ <file.vcf.gz> | bcftools view --max-alleles 2 -Oz -o output.vcf.gz"
        )))
      }

      # check for multiallelic rows (ALT contains comma)
      n_multi <- sum(grepl(",", vcf@fix[, "ALT"], fixed = TRUE))
      if (n_multi > 0) {
        pct_multi <- round(100 * n_multi / nrow(vcf@fix), 1)
        showNotification(
          paste0(
            n_multi, " variant(s) (", pct_multi, "%) have multiple ALT alleles."
          ),
          type = "warning", duration = 20
        )
      }

      vcfR::extract.indels(vcf, return.indels = FALSE)
    }

    if (!is.null(input$file)) {
      file_info    <- input$file
      file_size_mb <- round(file_info$size / 1024, 1)
      if (file_info$size > FILESIZE_THRESHOLD_BYTES) {
        showNotification(
          paste0("Warning: File is large (", file_size_mb, " MB). Reading may take a while."),
          type = "warning", duration = 20
        )
      }
      return(read_and_validate(file_info$datapath))
    }
    if (isTRUE(input$vcfStored_1))
      return(read_and_validate("https://raw.githubusercontent.com/TimHasenbein/Haplofun/main/01_data/01_IL23R_100kb_hg38_multi_rm.vcf.gz"))
    if (isTRUE(input$vcfStored_3))
      return(read_and_validate("https://raw.githubusercontent.com/TimHasenbein/Haplofun/main/01_data/linreg_nonsynonymous_ftsI_multi_rm.vcf.gz"))
    return(NULL)
  })
  
  # read meta-data
  output$isMetadataAvailable <- reactive({
    !is.null(input$fileSubInfo) || isTRUE(input$meta1000G) || isTRUE(input$metahinf)
  })
  outputOptions(output, "isMetadataAvailable", suspendWhenHidden = FALSE)
  isMetadataSelected <- reactive({
    !is.null(input$fileSubInfo) || isTRUE(input$meta1000G) || isTRUE(input$metahinf)
  })
  getSubInformationAsMatrix <- reactive({
    req(isMetadataSelected())
    req(getHaplotypes())
    req(vcfInput())
    if (!is.null(input$fileSubInfo)) {
      metafile <- read.table(input$fileSubInfo$datapath, header = F, sep="\t", fill = T)
    } else if (isTRUE(input$meta1000G)) {
      metafile <- read.table("https://raw.githubusercontent.com/TimHasenbein/Haplofun/main/01_data/050226_meta_subregions_adj.tsv", header = F, sep="\t", fill = T)
    } else if (isTRUE(input$metahinf)) {
      metafile <- read.table("https://raw.githubusercontent.com/TimHasenbein/Haplofun/main/01_data/050226_meta_hinf_linreg_resistance.tsv", header = F, sep="\t", fill = T)
    }
    trait_lookup <- setNames(as.character(metafile$V2), as.character(metafile$V1))
    vcfData <- vcfInput()
    dna_labels <- labels(vcfData)
    haplo_obj <- getHaplotypes()
    index_list <- attr(haplo_obj, "index")
    unique_traits <- sort(unique(trait_lookup))
    pie_matrix <- matrix(0, nrow = length(index_list), ncol = length(unique_traits))
    colnames(pie_matrix) <- unique_traits
    total_matches <- 0
    for (h in seq_along(index_list)) {
      current_indices <- index_list[[h]]
      full_sample_names <- dna_labels[current_indices]
      clean_sample_names <- gsub("_\\d+$", "", full_sample_names)
      matched_traits <- trait_lookup[clean_sample_names]
      matched_traits <- matched_traits[!is.na(matched_traits)]
      if (length(matched_traits) > 0) {
        total_matches <- total_matches + length(matched_traits)
        trait_counts <- table(matched_traits)
        for (trait_name in names(trait_counts)) {
          pie_matrix[h, trait_name] <- trait_counts[trait_name]
        }
      }
    }
    validate(
      need(total_matches > 0, 
           "No overlap found between VCF sample IDs and Metadata IDs. Please check if IDs match.")
    )
    pie_matrix
  })
  ####---------------------------------------------------------------------####
  
  ####--------------------------- vcf filtering ---------------------------####
  # filter setup
  observeEvent(vcfRInput(), {
    # clear stale filtered/pruned state on new load
    vcf_data_store(NULL)
    ld_pruned_vcf_store(NULL)
    filteredVcfData_Prun(NULL)
    sampled_vcf_store(NULL)
    haplonet(NULL)
    vcf <- req(vcfRInput())
    chroms <- unique(vcf@fix[, "CHROM"])
    updateSelectInput(session, "chromFilter", choices = chroms, selected = chroms[1])
    if (length(chroms) > 0) {
      pos_data <- vcf@fix[vcf@fix[, "CHROM"] == chroms[1], "POS"]
      max_pos <- max(as.integer(pos_data), na.rm = TRUE)
      updateNumericInput(session, "endPos", value = max_pos)
      updateNumericInput(session, "startPos", value = 1)
    }
  })
  
  # apply filter on button click
  observeEvent(input$applyFilters, {
    # reset pruning 
    ld_pruned_vcf_store(NULL)
    filteredVcfData_Prun(NULL)
    sampled_vcf_store(NULL)
    updateCheckboxInput(session, "enableSnpsampling", value = FALSE)
    updateCheckboxInput(session, "enableLdPruning", value = FALSE)
    # continue
    vcf <- req(vcfRInput())
    if (input$chromFilter == "") {
      showNotification("Please select a Chromosome.", type = "error")
      return()
    }
    if (input$endPos < input$startPos) {
      showNotification("End position must be greater than or equal to start position.", type = "error")
      return()
    }
    vcf_filtered_chrom <- vcf[vcf@fix[, "CHROM"] == input$chromFilter]
    pos <- as.integer(vcf_filtered_chrom@fix[, "POS"])
    vcf_final <- vcf_filtered_chrom[pos >= input$startPos & pos <= input$endPos]
    if (nrow(vcf_final@fix) <= 1) {
      showNotification("Only, one or no variants found in the selected region. Adjust filters.", type = "warning")
      return()
    }
    # MAF filter
    if (input$mafFilterInit != 0) {
      maf_threshold_value <- input$mafFilterInit
      allele_frequencies <- as.data.frame(vcfR::maf(vcf_final))
      maf_values <- apply(allele_frequencies, 1, min)
      loci_to_keep <- allele_frequencies$Frequency >= maf_threshold_value
      vcf_final <- vcf_final[loci_to_keep]
    }
    if (nrow(vcf_final@fix) <= 1) {
      showNotification("Only, one or no variants remaining after MAF filter. Adjust threshold.", type = "warning")
      return()
    }
    showNotification(
      paste0("Filtering complete: Kept ", nrow(vcf_final@fix), " out of ", nrow(vcf@fix), " SNPs."),
      type = "message",
      duration = 5
    )
    vcf_data_store(vcf_final)
  }, ignoreInit = TRUE) 
  
  # use filtered or raw data depending on user action
  filteredVcfData_pos <- reactive({
    req(vcfRInput())  
    if (is.null(vcf_data_store())) { 
      return(vcfRInput()) 
    } else {
      return(vcf_data_store()) 
    }
  })
  
  # convert into DNAbin 
  vcfPreLDPruning_vcfBin <- reactive({
    withProgress(message = 'Data conversion...', value = 0, {
      incProgress(0.5)
      vcfData <- vcfR2DNAbin(filteredVcfData_pos(), extract.indels = TRUE)
      incProgress(0.5, detail = paste(" complete!"))
      return(vcfData)
    })
  }) %>% bindCache(filteredVcfData_pos())
  
  # check for LD pruning status
  ld_pruned_vcf_store <- reactiveVal(NULL)
  output$ldPrunedVcfStoreIsNull <- reactive({
    is.null(ld_pruned_vcf_store())
  })
  outputOptions(output, "ldPrunedVcfStoreIsNull", suspendWhenHidden = FALSE)
  
  # LD r-squared matrix calculation
  r2MatrixBeforePruning <- reactive({
    tryCatch({
      R.utils::withTimeout({
        vcf <- req(vcfPreLDPruning_vcfBin()) 
        r_matrix <- pegas::LDscan(vcf, what = "r")
        r_matrix <- as.matrix(r_matrix)
        r2_matrix <- r_matrix^2
        snp_ids <- colnames(vcf)
        rownames(r2_matrix) <- snp_ids
        colnames(r2_matrix) <- snp_ids
        return(r2_matrix)
      }, timeout = 60, onTimeout = "error")
    }, error = function(e) {
      if (inherits(e, "TimeoutException")) {
        validate(need(FALSE,"High computational load detected. Please filter your data further."))
      } else {validate(need(FALSE, paste(e$message)))}
    })
  }) %>% bindCache(vcfPreLDPruning_vcfBin())
  
  # LD heatmap (BEFORE PRUNING)
  output$ldHeatmapBefore <- renderPlot({
    r2_matrix <- req(r2MatrixBeforePruning())
    upper_tri <- get_upper_tri(r2_matrix)
    LD_threshold_vis <- input$ldThreshold
    melted_r2_matrix <- reshape2::melt(upper_tri, varnames = c("Var1", "Var2"), na.rm = TRUE)
    melted_r2_matrix$is_perfect_LD <- ifelse(
      melted_r2_matrix$value >= LD_threshold_vis,
      "LD",
      "Lower LD"
    )
    ggplot(melted_r2_matrix, aes(Var2, Var1, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient(
        low = "white", high = "#d73027",
        limits = c(0, 1),
        name = expression(r^2)
      ) +
      scale_x_discrete(position = "top") + 
      scale_y_discrete(
        limits = rev(levels(factor(melted_r2_matrix$Var2))),
        position = "right"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 0),
        axis.text.y = element_text(angle = 0, vjust = 1, size = 8, hjust = 0),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 5, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_equal(expand = FALSE, clip = "off")
  })
  
  # apply LD pruning on button click
  filteredVcfData_Prun <- reactiveVal(NULL)
  observeEvent(input$applyLdPruning, {
    req(input$enableLdPruning)
    # rm bootstrapping
    sampled_vcf_store(NULL)
    updateCheckboxInput(session, "enableSnpsampling", value = FALSE)
    shinyjs::disable("applyLdPruning")
    updateActionButton(session, "applyLdPruning", label = "Pruning...", icon = icon("spinner", class = "fa-spin"))
    on.exit({
      updateActionButton(session, "applyLdPruning", label = "Apply LD Pruning", icon = icon("scissors"))
      shinyjs::enable("applyLdPruning")
    })
    vcfR <- req(filteredVcfData_pos())
    initial_count <- nrow(vcfR@fix)
    r2_matrix <- req(r2MatrixBeforePruning())
    withProgress(message = 'Applying LD pruning...', value = 0, {
      LD_threshold <- input$ldThreshold
      snps_to_remove <- character(0)
      snps_retained <- colnames(r2_matrix)
      N <- ncol(r2_matrix)
      step_size <- 1 / N
      for (i in 1:(N - 1)) {
        incProgress(step_size, detail = paste("Processing..."))
        if (colnames(r2_matrix)[i] %in% snps_to_remove) next
        for (j in (i + 1):N) {
          if (is.na(r2_matrix[i, j])) {
            next 
          }
          if (r2_matrix[i, j] >= LD_threshold) {
            snps_to_remove <- c(snps_to_remove, colnames(r2_matrix)[j])
          }
        }
      }
      incProgress(1, detail = "Finalizing data structures...")
    })
    # filter vcfR object based on retained SNPs
    snps_to_keep <- snps_retained[!(snps_retained %in% unique(snps_to_remove))]
    if (length(snps_to_keep) <= 1) {
      showNotification("Only, one or no variants remaining after Pruning. Adjust threshold.", type = "warning")
      return()
    }
    # DNAbin object
    vcf <- req(vcfPreLDPruning_vcfBin())
    vcf_pruned <- vcf[, snps_to_keep]
    final_count <- ncol(vcf_pruned)
    # vcfR object
    vcfR <- req(filteredVcfData_pos())
    vcfR@fix[,"ID"] <- get_variant_names(vcfR)
    all_vcf_ids <- vcfR@fix[, "ID"]
    rows_to_keep <- which(all_vcf_ids %in% snps_to_keep)
    vcfR_final <- vcfR[rows_to_keep,]
    showNotification(
      paste0("LD Pruning complete: Kept ", final_count, " out of ", initial_count, " SNPs (r² < ", 
             LD_threshold, ")."),
      type = "message",
      duration = 5
    )
    ld_pruned_vcf_store(vcf_pruned)
    filteredVcfData_Prun(vcfR_final)
  }, ignoreInit = TRUE)
  
  # master vcfInput (DNAbin) 
  vcfInput_1 <- reactive({
    if (!is.null(ld_pruned_vcf_store())) {
      return(ld_pruned_vcf_store()) 
    } else {
      return(vcfPreLDPruning_vcfBin())
    }
  })
  
  # master VCF data (vcfR)
  filteredVcfData_1 <- reactive({
    if (!is.null(filteredVcfData_Prun())) { 
      return(filteredVcfData_Prun())
    }
    return(filteredVcfData_pos())
  })
  
  # LD heatmap (AFTER PRUNING)
  output$ldHeatmapAfter <- renderPlot({
    vcf.filtered <- req(ld_pruned_vcf_store())
    r_matrix <- pegas::LDscan(vcf.filtered, what = "r")
    r_matrix <- as.matrix(r_matrix)
    r2_matrix <- r_matrix^2
    snp_ids <- colnames(vcf.filtered)
    rownames(r2_matrix) <- snp_ids
    colnames(r2_matrix) <- snp_ids
    upper_tri <- get_upper_tri(r2_matrix)
    melted_r2_matrix <- reshape2::melt(upper_tri, varnames = c("Var1", "Var2"), na.rm = TRUE)
    ggplot(melted_r2_matrix, aes(Var2, Var1, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient(
        low = "white", high = "#d73027",
        limits = c(0, 1),
        name = expression(r^2)
      ) +
      scale_x_discrete(position = "top") +
      scale_y_discrete(
        limits = rev(levels(factor(melted_r2_matrix$Var2))),
        position = "right"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 0),
        axis.text.y = element_text(angle = 0, vjust = 1, size = 8, hjust = 0),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 5, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_equal(expand = FALSE, clip = "off")
  })

  output$buttonLdHeatmapBefore <- downloadHandler(
    filename = function() paste0(input$textExportFileName, "_LD_heatmap_before_pruning.pdf"),
    content = function(file) {
      r2_matrix <- req(r2MatrixBeforePruning())
      upper_tri <- get_upper_tri(r2_matrix)
      LD_threshold_vis <- input$ldThreshold
      melted_r2_matrix <- reshape2::melt(upper_tri, varnames = c("Var1", "Var2"), na.rm = TRUE)
      melted_r2_matrix$is_perfect_LD <- ifelse(
        melted_r2_matrix$value >= LD_threshold_vis, "LD", "Lower LD")
      p <- ggplot(melted_r2_matrix, aes(Var2, Var1, fill = value)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "white", high = "#d73027", limits = c(0, 1), name = expression(r^2)) +
        scale_x_discrete(position = "top") +
        scale_y_discrete(limits = rev(levels(factor(melted_r2_matrix$Var2))), position = "right") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 0),
          axis.text.y = element_text(angle = 0, vjust = 1, size = 8, hjust = 0),
          panel.grid = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        ) +
        coord_equal(expand = FALSE, clip = "off")
      ggsave(file, plot = p, device = "pdf", width = 10, height = 10)
    }
  )

  output$buttonLdHeatmapAfter <- downloadHandler(
    filename = function() paste0(input$textExportFileName, "_LD_heatmap_after_pruning.pdf"),
    content = function(file) {
      vcf.filtered <- req(ld_pruned_vcf_store())
      r_matrix <- pegas::LDscan(vcf.filtered, what = "r")
      r_matrix <- as.matrix(r_matrix)
      r2_matrix <- r_matrix^2
      snp_ids <- colnames(vcf.filtered)
      rownames(r2_matrix) <- snp_ids
      colnames(r2_matrix) <- snp_ids
      upper_tri <- get_upper_tri(r2_matrix)
      melted_r2_matrix <- reshape2::melt(upper_tri, varnames = c("Var1", "Var2"), na.rm = TRUE)
      p <- ggplot(melted_r2_matrix, aes(Var2, Var1, fill = value)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "white", high = "#d73027", limits = c(0, 1), name = expression(r^2)) +
        scale_x_discrete(position = "top") +
        scale_y_discrete(limits = rev(levels(factor(melted_r2_matrix$Var2))), position = "right") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 0),
          axis.text.y = element_text(angle = 0, vjust = 1, size = 8, hjust = 0),
          panel.grid = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        ) +
        coord_equal(expand = FALSE, clip = "off")
      ggsave(file, plot = p, device = "pdf", width = 10, height = 10)
    }
  )

  # check if filtering/sampling is active
  isDataProcessed <- reactive({
    return(!is.null(vcf_data_store()) || !is.null(filteredVcfData_Prun()))
  })
  
  # initial count panel
  output$SummaryStats <- renderUI({
    file_info <- req(isvcfDataSelected())
    vcf <- req(vcfRInput())
    chrom_name <- unique(vcf@fix[, "CHROM"])[1]
    pos_data <- vcf@fix[vcf@fix[, "CHROM"] == chrom_name, "POS"]
    start <- min(as.integer(pos_data), na.rm = TRUE)
    end <- max(as.integer(pos_data), na.rm = TRUE)
    pos_range <- paste0(start, "-", end)
    n_samples <- ncol(vcf@gt) - 1
    gt_sample <- gsub(":.*", "",
      as.vector(vcf@gt[seq_len(min(5, nrow(vcf@gt))), -1, drop = FALSE]))
    ploidy <- if (any(grepl("[|/]", gt_sample))) 2L else 1L
    n_haplotypes <- n_samples * ploidy
    alert_box <- div(
      class = "alert alert-success",
      style = "padding: 10px; margin-top: 15px; margin-bottom: 15px;",
      icon("info-circle"),
      strong("VCF File Summary:"),
    )
    stats_block <- div(
      style = "padding: 10px; border: none;",
      fluidRow(
        column(6, h5("Chromosome:", style = "font-weight: bold;"), p(chrom_name)),
        column(6, h5("Position:", style = "font-weight: bold;"), p(pos_range)),
        column(6, h5("Samples:", style = "font-weight: bold;"), div(as.character(n_samples))),
        column(6, h5("Variants (Initial):", style = "font-weight: bold;"), textOutput("numInitVariants")),
        column(6, h5("Total Haplotypes:", style = "font-weight: bold;"), div(as.character(n_haplotypes))),
        column(6, h5("Total Haplotype groups:", style = "font-weight: bold;"), textOutput("numInitHG"))
      )
    )
    tagList(
      alert_box,
      stats_block
    )
  })
  
  # filter/sample SNP count panel
  output$dynamicSummaryStats <- renderUI({
    vcf <- req(vcfRInput())
    processed <- isDataProcessed()
    n_samples <- ncol(vcf@gt) - 1
    gt_sample <- gsub(":.*", "",
      as.vector(vcf@gt[seq_len(min(5, nrow(vcf@gt))), -1, drop = FALSE]))
    ploidy <- if (any(grepl("[|/]", gt_sample))) 2L else 1L
    n_haplotypes <- n_samples * ploidy
    if (processed) {
      style_css <- "padding: 10px; margin-top: 15px; border: 1px solid darkblue; background-color: #CEE5ED; border-radius: 5px; color: darkblue;"
      icon_status <- icon("filter", class = "text-success", style = "color: darkblue;")
    } else {
      style_css <- "padding: 10px; margin-top: 15px; border: none;"
      icon_status <- NULL
    }
    div(style = style_css,
        fluidRow(
          if (processed) {
            column(6, h4(icon_status, "Total Samples:"), div(as.character(n_samples)))
          } else {
            column(6, h4("Total Samples:"), div(as.character(n_samples)))
          },
          column(6, h4("Variants:"), textOutput("numVariants")),
          column(6, h4("Total Haplotypes:"), div(as.character(n_haplotypes))),
          column(6, h4("Haplotype groups:"), textOutput("numHG"))
        )
    )
  })
  
  # reset filters and pruning 
  observeEvent(input$resetFilters, {
    # reset pruning 
    ld_pruned_vcf_store(NULL)
    filteredVcfData_Prun(NULL)
    sampled_vcf_store(NULL)
    # reset filtering
    vcf_data_store(NULL)
    vcf <- req(vcfRInput()) 
    chroms <- unique(vcf@fix[, "CHROM"])
    updateSelectInput(session, "chromFilter", selected = chroms[1]) 
    if (length(chroms) > 0) {
      pos_data <- vcf@fix[vcf@fix[, "CHROM"] == chroms[1], "POS"]
      max_pos <- max(as.integer(pos_data), na.rm = TRUE)
      updateNumericInput(session, "endPos", value = max_pos)
      updateNumericInput(session, "startPos", value = 1)
    }
    updateNumericInput(session, "mafFilterInit", value = 0)
    updateCheckboxInput(session, "enableLdPruning", value = FALSE)
    updateCheckboxInput(session, "enableSnpsampling", value = FALSE)
    req(getAllHaplotypes())
    haplotypes <- getAllHaplotypes()
    rows <- nrow(haplotypes)
    new_max <- min(rows, 50)
    new_value_raw <- min(10, rows)
    new_value <- max(2, new_value_raw)
    updateSliderInput(
      session, 
      "sliderHaplotypes", 
      max = new_max, 
      value = new_value, 
      step = 1,
      min = 2 
    )
  }, ignoreInit = TRUE)
  
  # random sub-sampling
  sampled_vcf_store <- reactiveVal(NULL)
  observeEvent(input$applySnpsampling, {
    shinyjs::disable("applySnpsampling")
    updateActionButton(session, "applySnpsampling", label = "Sampling...", icon = icon("spinner", class = "fa-spin"))
    on.exit({
      updateActionButton(session, "applySnpsampling", label = "Apply random SNP sampling", icon = icon("dice"))
      shinyjs::enable("applySnpsampling")
    })
    vcf <- req(filteredVcfData_1()) 
    if (!isTRUE(input$enableSnpsampling)) {
      sampled_vcf_store(vcf)
      return()
    }
    total_snps <- nrow(vcf@fix)
    if (total_snps <= 1) {
      showNotification("Only one or no SNPs available for sampling.", type = "warning")
      return()
    }
    sampling_value <- input$snpSampleSize
    if (sampling_value <= 1) {
      showNotification("Please specify a sample size of at least 2.", type = "warning")
      return()
    }
    if (sampling_value >= total_snps) {
      vcf_sampled <- vcf
    } else {
      indices_to_keep <- sample(1:total_snps, size = sampling_value, replace = FALSE)
      vcf_sampled <- vcf[indices_to_keep, ]
      showNotification(
        paste0("Subsampling complete: Kept ", sampling_value, " out of ", total_snps, " SNPs."),
        type = "message",
        duration = 5
      )
    }
    sampled_vcf_store(vcf_sampled)
  }, ignoreInit = TRUE)
  
  # master VCF data (vcfR, FINAL)
  filteredVcfData <- reactive({
    if (isTRUE(input$enableSnpsampling) && !is.null(sampled_vcf_store())) { 
      return(sampled_vcf_store()) 
    }
    return(filteredVcfData_1()) 
  })
  
  # master vcfInput (DNAbin, FINAL)
  vcfInput <- reactive({
    if (isTRUE(input$enableSnpsampling) && !is.null(sampled_vcf_store())) {
      vcfR_final <- req(sampled_vcf_store())
      vcfData <- vcfR2DNAbin(vcfR_final, extract.indels = TRUE)
      return(vcfData)
    }
    return(vcfInput_1()) 
  })
  ####---------------------------------------------------------------------####
  
  ####--------------------------- core haplotype analysis ---------------------------####
  # get all initial haplotypes
  getAllinitHaplotypes <- reactive({
    tryCatch({
      R.utils::withTimeout({
        vcfData <- req(vcfRInput())
        vcfData <- vcfR2DNAbin(vcfData, extract.indels = TRUE)
        haplotypes <- pegas::haplotype(vcfData, locus = 1:ncol(vcfData),) 
        haplotypes <- length(base::summary(haplotypes))
      }, timeout = 20, error = function(e) {
        if (inherits(e, "TimeoutException")) {
          validate(need(FALSE,"High computational load detected. Please filter your data further."))
        } else {validate(need(FALSE, paste(e$message)))}
      })
    }) 
  }) %>% bindCache(vcfRInput())
  
  # get haplotypes
  getAllHaplotypes <- reactive({
    tryCatch({
      R.utils::withTimeout({
        vcfData <- req(vcfInput())
        haplotypes <- pegas::haplotype(vcfData, locus = 1:ncol(vcfData),) 
        haplotypes <- sort(haplotypes) 
        labels <- c(1:dim(haplotypes)[1])
        labels <- gsub(" ", "", paste("H", labels))
        rownames(haplotypes) <- labels
        haplotypes
      }, timeout = 20, error = function(e) {
        if (inherits(e, "TimeoutException")) {
          validate(need(FALSE,"getAllHaplotypes: High computational load detected. Please filter your data further."))
        } else {validate(need(FALSE, paste(e$message)))}
      })
    }) 
  }) %>% bindCache(vcfInput())
  
  # get samples per haplogroup
  getSampleswithHaplo <- reactive({
    req(getAllHaplotypes())
    idx <- attr(getAllHaplotypes(), "index")
    sample_names <- rownames(vcfInput())
    hap_samples <- lapply(idx, function(i) sample_names[i])
    names(hap_samples) <- paste0("H", seq_along(hap_samples))
    hap_samples <- data.frame(
      haplotype = names(hap_samples),
      samples = sapply(hap_samples, paste, collapse = ", ")
    )
    return(hap_samples)
  })
  
  #  get haplotype summary
  getAllHaplotypeSummary <- reactive({
    tryCatch({
      R.utils::withTimeout({
        base::summary(getAllHaplotypes())
      }, timeout = 20, error = function(e) {
        if (inherits(e, "TimeoutException")) {
          validate(need(FALSE,"High computational load detected. Please filter your data further."))
        } else {validate(need(FALSE, paste(e$message)))}
      })
    })
  })

  # select haplotype subset
  getHaplotypes <- reactive({
    tryCatch({
      R.utils::withTimeout({
        haplotypes <- getAllHaplotypes()
        oc <- oldClass(haplotypes)
        from <- attr(haplotypes, "from")
        idx <- attr(haplotypes, "index")
        total_count <- nrow(haplotypes)
        val <- input$sliderHaplotypes 
        val <- min(val, total_count)
        s <- logical(length <- nrow(haplotypes))
        s[1:val] <- TRUE
        haplotypes <- haplotypes[s, ] 
        attr(haplotypes, "index") <- idx[s]
        class(haplotypes) <- oc
        attr(haplotypes, "from") <- from
        haplotypes 
      }, timeout = 20, onTimeout = "error")
    }, error = function(e) {
      if (inherits(e, "TimeoutException")) {
        validate(need(FALSE,"getHaplotypes: High computational load detected. Please filter your data further."))
      } else {validate(need(FALSE, paste(e$message)))}
    })
  })
  
  #  get ploidy
  getPloidy <- reactive({
    numberAllSequences <- attributes(vcfInput())$dim[1]
    numberAllIndividuums <- dim(attributes(filteredVcfData())$gt)[2]-1
    numberAllSequences / numberAllIndividuums
  })
  
  #  create table of haplotypes and frequencies
  calculateHaplotypeTable <- reactive({
    tryCatch({
      R.utils::withTimeout({
        req(isvcfDataSelected())
        haplotypes <- getHaplotypes()
        haplMatrix <- toupper(as.character(haplotypes, matrix = TRUE))
        V1 <- rep(NA, nrow(haplMatrix))
        haplMatrix <- cbind(V1, haplMatrix)
        val <- input$sliderHaplotypes
        val <- min(val, nrow(haplotypes))
        haplMatrix[, 1] <- getAllHaplotypeSummary()[1:val]
        colnames(haplMatrix) <- append(list("freq"), get_variant_names(filteredVcfData()))
        haplMatrix <- as.data.frame(haplMatrix, stringsAsFactors = FALSE)
        return(haplMatrix)
      }, timeout = 20, onTimeout = "error")
    }, error = function(e) {
      if (inherits(e, "TimeoutException")) {
        validate(need(FALSE,"calculateHaplotypeTable: High computational load detected. Please filter your data further."))
      } else {validate(need(FALSE, paste(e$message)))}
    })
  }) %>% bindCache(getHaplotypes())
  
  #  extract meta-data from VCF
  calculateVcfMetadata <- reactive({
    vcfData <- filteredVcfData()
    metadata <- getFIX(vcfData)
  })
  
  # calculate hamming distance matrix
  distanceMatrixHammingReactive <- reactive({
    tryCatch({
      R.utils::withTimeout({
        haplotypes <- req(getHaplotypes())
        d <- pegas::dist.hamming(haplotypes)
        return(d)
      }, timeout = 20, onTimeout = "error") 
    }, error = function(e) {
      if (inherits(e, "TimeoutException")) {
        validate(need(FALSE,"High computational load detected. Please filter your data further."))
      } else {validate(need(FALSE, paste(e$message)))}
    })
  }) %>% bindCache(getHaplotypes())
  
  # render a table of the distance matrix
  output$plotDistanceMatrixAsMatrix <- renderTable({
    d <- distanceMatrixHammingReactive() 
    full_matrix <- as.matrix(d)
    return(full_matrix)
  }, rownames = TRUE)
  
  # calculate networks
  calculateHaplonet <- reactive({
    haplotypes <- getHaplotypes()
    num_haplotypes <- length(labels(haplotypes))
    if (num_haplotypes > 50) {
      return(NULL) 
    }
    distanceMatrixHamming <- distanceMatrixHammingReactive()
    if (input$selectNetwork == "1") {
      haplonet <- NULL
    }
    if (input$selectNetwork == "2") {
      haplonet <- haploNet(haplotypes)
    }
    if (input$selectNetwork == "3") {
      haplonet <- msn(distanceMatrixHamming)
    }
    else if (input$selectNetwork == "6") {
      haplonet <- rmst(distanceMatrixHamming,stop.criterion = 2*ceiling(sqrt(length(labels(getHaplotypes())))), quiet = TRUE)
    }
    else if (input$selectNetwork == "7") {
      haplonet <- pegas::mst(distanceMatrixHamming)
    }
    if(is.null(attr(haplonet, "alter.links"))) {
      updateSliderInput(session = session, inputId = "sliderThreshold", max = 0)
    } else {  
      updateSliderInput(session = session, inputId = "sliderThreshold", max = max(attr(haplonet, "alter.links")[, 3]))
    }
    haplonet
  }) %>% bindCache(getHaplotypes(),input$selectNetwork) 
  
  # store haplonet
  haplonet <- reactiveVal()
  observe({
    req(filteredVcfData())
    req(getHaplotypes())
    haplonet(calculateHaplonet())  
  })
  
  # summary statistics output
  output$tableNetwork <- renderTable({
    req(haplonet())
    # nucleotide diversity: measurement of genetic distance between sequences
    # haplotype diversity: measurement of genetic diversity in a population
    # branch diversity: measurement of topological diversity in haplotype network
    # haplotype network branch diversity: measurement of the complexity in a haplotype network
    network <- haplonet()
    hapFreqs <- getAllHaplotypeSummary()[labels(network)]
    nucleotideDiversity <- nuc.div(vcfInput())
    # calculate number of links of network
    numberLinks <- nrow(network)
    altLinks <- attr(network, "alter.links")
    dataFrameAltLinks <- data.frame(altLinks)
    if (!is.null(altLinks)) numberLinks <- numberLinks + nrow(altLinks)
    numberLinksShown <- nrow(network)
    if(!is.null(altLinks)) numberLinksShown <- numberLinksShown + sum(dataFrameAltLinks$step <= input$sliderThreshold)
    # haplotype diversity - Hd
    ft <- as.data.frame(hapFreqs)
    ft$total <- apply(ft,1,sum)
    sqfreq <- (ft$total/sum(ft$total))^2
    ft <- cbind(ft, sqfreq = sqfreq)
    Efh2 <- sum(ft$sqfreq)
    numberSequences <- sum(hapFreqs)
    numberIndividuals <- numberSequences / getPloidy()
    n <- numberSequences
    Hd <- (1-Efh2)*(n/(n-1)) 
    # branch diversity - Bd
    haplotype_counts <- c(network[,1],attr(network, "alter.links")[,1],network[,2],attr(network, "alter.links")[,2])
    Hbranches <- aggregate(data.frame(branches = haplotype_counts), list(haplotype = haplotype_counts), length)
    Hbranches <- cbind(Hbranches, nseq = ft$total)
    nH <- nrow(Hbranches)
    Hclasses_temp <- aggregate(Hbranches, list("HC" = Hbranches$branches), sum)
    Hclasses <- Hclasses_temp[,c(1,4)]
    names(Hclasses)[2] <- "niHc"
    nHc <- nrow(Hclasses)
    Hclasses <- cbind(Hclasses, sqfreq = (Hclasses$'niHc'/n)^2)
    EfHc2 <- sum(Hclasses$sqfreq)
    Bd <- (1-EfHc2)*(n/(n-1))
    # haplotype network branch diversity - HBd
    # HBd <- (1 - Efh2) * (1- EfHc2) * (n/(n-1))
    HBd <- Hd * Bd
    table <- matrix(c(numberLinks,numberLinksShown, Hd, Bd, HBd), nrow = 5)
    rownames(table) <- c("Total Number of calculated Links", "Number of Links currently shown", "Haplotype Diversity", "Branch Diversity", "Haplotype Network Branch Diversity")
    table
  }, colnames = F, rownames = T)
  
  # haplotype sequences output
  output$plotHaplotypeTable <- 
    tryCatch({
      R.utils::withTimeout({
        DT::renderDataTable(DT::datatable(calculateHaplotypeTable(), options = list(scrollY = "350px",scrollX = TRUE)))
      }, timeout = 20, onTimeout = "error") 
    }, error = function(e) {
      if (inherits(e, "TimeoutException")) {
        validate(need(FALSE,"High computational load detected. Please filter your data further."))
      } else {validate(need(FALSE, paste(e$message)))}
    })
  
  
  # haplotype sequences output
  output$plotSampleTable <- 
    tryCatch({
      R.utils::withTimeout({
        DT::renderDataTable(DT::datatable(getSampleswithHaplo(), options = list(scrollY = "350px",scrollX = TRUE)))
      }, timeout = 20, onTimeout = "error") 
    }, error = function(e) {
      if (inherits(e, "TimeoutException")) {
        validate(need(FALSE,"High computational load detected. Please filter your data further."))
      } else {validate(need(FALSE, paste(e$message)))}
    })
  
  
  ####---------------------------------------------------------------------####
  
  ####--------------------------- core haplotype analysis plots ---------------------------####
  #  color alleles 
  getColorByAllele <- reactive({
    req(input$snpPosition)
    req(calculateHaplotypeTable())
    allele_colors_custom <- c(
      "A" = "#95bc0e",
      "C" = "#006aa3", 
      "G" = "#fabb00", 
      "T" = "#e42032"  
    )
    snp_column_name <- input$snpPosition
    hapTab <- calculateHaplotypeTable()
    tmp <- hapTab[, snp_column_name, drop = TRUE]
    haplotype_names <- names(tmp)
    unique_alleles <- sort(unique(tmp))
    allele_to_color <- allele_colors_custom[unique_alleles]
    color_vector <- rep(NA, length(tmp))
    names(color_vector) <- haplotype_names
    color_vector[] <- allele_to_color[tmp[]]
    req(!any(is.na(color_vector)))
    return(color_vector)
  })
  
  #  highlight alleles
  getSnpPositions <- reactive({
    req(filteredVcfData())
    snp_ids <- get_variant_names(filteredVcfData())
    if (is.null(snp_ids) || length(snp_ids) == 0) {
      return(NULL)
    }
    unique_ids <- unique(snp_ids)
    return(unique_ids)
  })
  
  # observe SNP position choice
  observeEvent(getSnpPositions(), {
    req(getSnpPositions())
    all_choices <- c("None",getSnpPositions())
    updateSelectizeInput(
      session = session,
      inputId = "snpPosition",
      choices = all_choices,
      server = TRUE,
      selected = "None",
    )
  })
  
  # highlight haplotypes
  observe({
    req(getHaplotypes())
    all_choices <- c(labels(getHaplotypes()))
    updateSelectizeInput(
      session,
      "highlightHaplotypes",
      choices = all_choices,
      #server = TRUE,
      selected = NULL,
    )
  })
  
  # network plot
  haplonetPlotReactive <- reactive({
    req(input$selectNetwork != 1)
    haplonetObj <- haplonet()
    nodeLabels <- labels(haplonetObj)
    scaleRatio <- 5 / input$sliderScaleNetwork
    sizeNodes <- rep(1, length(nodeLabels))
    if (is.null(haplonetObj)) {
      num_haplotypes <- length(labels(getHaplotypes()))
      msg <- paste0("High computational load detected. Please filter your data further.")
      stop(msg) 
    }
    # scale node size
    if (input$checkboxScaleNetwork) {
      req(getHaplotypes())
      sz <- getAllHaplotypeSummary()
      labelsHap <- attr(haplonetObj, 'labels')
      sizeNodes <- sz[labelsHap]
    }
    # edge threshold
    threshold <- if (input$sliderThreshold == 0) 0 else c(1, input$sliderThreshold)
    # SNP highlight
    if (input$snpPosition != "None") {
      req(getColorByAllele())
      baseFillColors <- getColorByAllele()
    } else {
      baseFillColors <- rep(input$colorCircles, length(nodeLabels))
    }
    # node colors
    nodeColors <- rep("black", length(nodeLabels))
    # highlight haplotypes
    if (!is.null(input$highlightHaplotypes)) {
      sel <- match(input$highlightHaplotypes, nodeLabels)
      sel <- sel[!is.na(sel)]
      if (length(sel) > 0) {
        nodeColors[sel] <- "black"
        baseFillColors[sel] <- "#CCDDFF"
      }
    }
    # plot meta
    if (subInputSelected() && input$checkboxMeta) {
      pieInfo <- getSubInformationAsMatrix() 
      num <- ncol(getSubInformationAsMatrix())
      plot(
        haplonetObj,
        pie = pieInfo,
        bg = category_colors(),
        shape = if (input$selectNetwork %in% c(4, 5)) c("circles", "circles") else "circles",
        cex = input$sliderLabels,
        fast = input$checkboxFastPlotHaplonet,
        scale.ratio = scaleRatio,
        labels = TRUE,
        show.mutation = input$radioButtonEdges,
        threshold = threshold,
        size = sizeNodes
      )
      legend(
        "topright",                 
        legend = colnames(pieInfo), 
        fill = category_colors(),
        bty = "n",                 
        cex = 0.8                   
      )
    } else if (isGwasDataSelected() && length(input$traitFilter) > 0 && input$checkboxGwasNetwork && input$applyTraitFilter > 0) {
      # highlight GWAS traits
      input$applyTraitFilter
      selected_trait <- isolate(input$traitFilter)
      trait_colors <- isolate(GWAStraitColor())
      # validation of input slider numbers
      current_slider_val <- input$sliderHaplotypes
      raw_gwas_data <- isolate(calculateGwasHaplotypeHeatmapData())
      validate(
        need(nrow(as.data.table(raw_gwas_data)) > 0, "Please click 'Update GWAS annotation', to load plotting data."),
        need(length(labels(haplonetObj)) <= length(unique(as.data.table(raw_gwas_data)$haplotypes)), 
             "Increased the number of haplogroups. Please click 'Update GWAS annotation', to update the GWAS coloring of the network.")
      )
      # get status info
      status_info <- isolate(as.data.table(calculateGwasHaplotypeHeatmapData()))
      status_info <- status_info %>% tidyr::separate_rows(SNP_trait, sep = "; ")
      status_info <- status_info %>% distinct(rsID, RiskAllele,SNP_trait,trait_status,haplotypes)
      status_info <- status_info %>% filter(sub("_[^_]+$", "", SNP_trait) %in% selected_trait)
      unique_traits <- unique(status_info$SNP_trait)
      pie_matrix <- matrix(0, 
                           nrow = length(nodeLabels), 
                           ncol = length(unique_traits),
                           dimnames = list(nodeLabels, unique_traits))
      for (i in 1:nrow(status_info)) {
        h_name <- as.character(status_info$haplotypes[i])
        t_name <- as.character(status_info$SNP_trait[i])
        if (h_name %in% nodeLabels) {
          pie_matrix[h_name, t_name] <- 1
        }
      }
      no_association <- ifelse(rowSums(pie_matrix) == 0, 1, 0)
      pie_matrix <- cbind(pie_matrix, "None" = no_association)
      active_colors <- unname(GWAStraitColor()[colnames(pie_matrix)])
      active_colors <- active_colors[!is.na(active_colors)]
      plot_colors <- c(active_colors, "white")
      network_order <- labels(haplonetObj)
      haplo_counts <- table(status_info$haplotypes)
      gwas_sizes <- as.numeric(haplo_counts[network_order]) + 1
      plot(
        haplonetObj,
        pie = pie_matrix,
        bg = plot_colors,
        shape = if (input$selectNetwork %in% c(4, 5)) c("circles", "circles") else "circles",
        cex = input$sliderLabels,
        fast = input$checkboxFastPlotHaplonet,
        scale.ratio = scaleRatio,
        labels = TRUE,
        show.mutation = input$radioButtonEdges,
        threshold = threshold,
        size = if (input$checkboxScaleNetwork) sizeNodes else gwas_sizes
      )
      legend(
        "topright",
        legend = colnames(pie_matrix),
        fill = plot_colors,
        bty = "n",
        cex = 0.8
      )
    } else if (iseqtlDataSelected() && length(input$qtlFilter) > 0 && input$checkboxQtlNetwork && input$applyQTLFilter > 0) {
      # highlight QTLs
      input$applyQTLFilter
      selected_qtl <- isolate(input$qtlFilter)
      trait_colors <- isolate(QTLtargetColor())
      # validation of input slider numbers
      current_slider_val <- input$sliderHaplotypes
      raw_qtl_data <- isolate(calculateeQTLHaplotypeHeatmapData())
      validate(
        need(nrow(as.data.table(raw_qtl_data)) > 0, "Please click 'Update QTL annotation', to load plotting data."),
        need(length(labels(haplonetObj)) <= length(unique(as.data.table(raw_qtl_data)$haplotypes)),
             "Increased the number of haplogroups. Please click 'Update QTL annotation', to update the QTL coloring of the network.")
      )
      # get status info
      status_info <- isolate(as.data.table(calculateeQTLHaplotypeHeatmapData()))
      status_info <- status_info %>% tidyr::separate_rows(Target, sep = "; ")
      status_info <- status_info %>% distinct(rsID, RiskAllele,Target,haplotypes)
      status_info <- status_info %>% filter(sub("_[^_]+$", "", Target) %in% selected_qtl)
      unique_targets <- unique(status_info$Target)
      pie_matrix <- matrix(0, 
                           nrow = length(nodeLabels), 
                           ncol = length(unique_targets),
                           dimnames = list(nodeLabels, unique_targets))
      for (i in 1:nrow(status_info)) {
        h_name <- as.character(status_info$haplotypes[i])
        t_name <- as.character(status_info$Target[i])
        if (h_name %in% nodeLabels) {
          pie_matrix[h_name, t_name] <- 1
        }
      }
      no_association <- ifelse(rowSums(pie_matrix) == 0, 1, 0)
      pie_matrix <- cbind(pie_matrix, "None" = no_association)
      active_colors <- unname(QTLtargetColor()[colnames(pie_matrix)])
      active_colors <- active_colors[!is.na(active_colors)]
      plot_colors <- c(active_colors, "white")
      network_order <- labels(haplonetObj)
      haplo_counts <- table(status_info$haplotypes)
      qtl_sizes <- as.numeric(haplo_counts[network_order]) + 1
      plot(
        haplonetObj,
        pie = pie_matrix,
        bg = plot_colors,
        shape = if (input$selectNetwork %in% c(4, 5)) c("circles", "circles") else "circles",
        cex = input$sliderLabels,
        fast = input$checkboxFastPlotHaplonet,
        scale.ratio = scaleRatio,
        labels = TRUE,
        show.mutation = input$radioButtonEdges,
        threshold = threshold,
        size = if (input$checkboxScaleNetwork) sizeNodes else qtl_sizes
      )
      legend(
        "topright",
        legend = colnames(pie_matrix),
        fill = plot_colors,
        bty = "n",
        cex = 0.8
      )
    } else {
      # plot without pie coloring
      plot(
        haplonetObj,
        pie = NULL,
        col = nodeColors,
        bg = baseFillColors,
        shape = if (input$selectNetwork %in% c(4, 5)) c("circles", "circles") else "circles",
        cex = input$sliderLabels,
        fast = input$checkboxFastPlotHaplonet,
        scale.ratio = scaleRatio,
        labels = TRUE,
        show.mutation = input$radioButtonEdges,
        threshold = threshold,
        size = sizeNodes
      )
      
      if (input$snpPosition != "None") {
        allele_colors <- c("A" = "#95bc0e", "C" = "#006aa3", "G" = "#fabb00", "T" = "#e42032")
        hapTab <- calculateHaplotypeTable()
        present_alleles <- sort(unique(hapTab[, input$snpPosition, drop = TRUE]))
        legend(
          "topright", 
          legend = present_alleles,
          fill = allele_colors[present_alleles],
          title = paste(input$snpPosition),
          bty = "n",
          cex = 0.8
        )
      }
    }
  })
  
  # UI output
  output$plotNetwork <- renderPlot(
    width = function()
      input$sizeNetwork,
    height = function()
      input$sizeNetwork,
    res = 96,
    {
      tryCatch({
        R.utils::withTimeout({
          haplonetPlotReactive()
        }, timeout = 10, onTimeout = "error")
    }, error = function(e) {
      if (inherits(e, "TimeoutException")) {
        validate(need(FALSE, "High computational load detected. Try enabling 'Fast plot', or filter your data further."))
      } else {
        validate(need(FALSE, paste(e$message)))
      }
    })
    }
  )
  
  # frequency barplot
  haplotypePlotReactive <- reactive({
    req(getHaplotypes())
    tryCatch({
      R.utils::withTimeout({
        haplotype_counts <- base::summary(getHaplotypes())
        haplotype_labels <- names(haplotype_counts)
        is_stacked <- subInputSelected()
        if (is_stacked && input$checkboxMeta) {
          values_matrix <- getSubInformationAsMatrix()
          rownames(values_matrix) <- haplotype_labels
        } else {
          values_matrix <- as.data.frame(haplotype_counts) %>%
            dplyr::rename(Count = haplotype_counts) %>% 
            mutate(Subgroup = "Total", Haplotype = haplotype_labels) %>%
            dplyr::select(Haplotype, Subgroup, Count) 
        }
        if (is_stacked && input$checkboxMeta) {
          plot_data <- as.data.frame(values_matrix) %>%
            tibble::rownames_to_column(var = "Haplotype") %>% 
            tidyr::pivot_longer(
              cols = -Haplotype, 
              names_to = "Subgroup",
              values_to = "Count"
            ) %>%
            filter(Count > 0)
        } else {
          plot_data <- values_matrix
        }
        plot_data$Haplotype <- factor(plot_data$Haplotype, levels = haplotype_labels)
        p <- ggplot(plot_data, aes(x = Haplotype, y = Count)) +
          geom_col(
            aes(fill = Subgroup), 
            position = "stack", 
            color = "gray10",  
            linewidth = 0.1
          ) +
          theme_minimal(base_size = 14) +
          labs(
            title = NULL,
            x = "Haplotype ID",
            y = "Haplotype frequency",
            fill = "Subgroup"
          ) 
        if (is_stacked && input$checkboxMeta) {
          p <- p + scale_fill_manual(values = category_colors())
        } else {
          p <- p + scale_fill_manual(values = c("Total" = "gray"))
        }
        p <- p +
          theme(
            axis.line.x = element_line(colour = "gray30"),
            axis.line.y = element_line(colour = "gray30"),
            panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank(),
            axis.text.x = element_text(
              angle = 45, 
              hjust = 1, 
              size = 10 
            ),
            plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 10)),
            legend.position = if (is_stacked) "right" else "none"
          )
        return(p)
      }, timeout = 20, onTimeout = "error")
    }, error = function(e) {
      if (inherits(e, "TimeoutException")) {
        validate(need(FALSE,"High computational load detected. Please filter your data further."))
      } else {validate(need(FALSE, paste(e$message)))}
    })
  })
  
  # UI output
  output$plotHaplotypeBars <- 
    tryCatch({
      R.utils::withTimeout({
        renderPlot({
          haplotypePlotReactive() })
      }, timeout = 20, onTimeout = "error")
    }, error = function(e) {
      if (inherits(e, "TimeoutException")) {
        validate(need(FALSE,"High computational load detected. Please filter your data further."))
      } else {validate(need(FALSE, paste(e$message)))}
    })
  
  # download
  output$buttonExportHaplotypeFreqBarPlot <- downloadHandler(
    filename = function() {paste(input$textExportFileName, "_haplotypeFrequencyBarPlot.pdf", sep = "")},
    content = function(file) {
      plot_to_save <- haplotypePlotReactive()
      ggplot2::ggsave(
        file, 
        plot = plot_to_save, 
        device = "pdf", 
        width = 14, 
        height = 10, 
        units = "in"
      )
    }
  )
  
  # distance heatmap 
  output$plotDistanceMatrixAsHeatmap <- plotly::renderPlotly({
    req(getHaplotypes())
    haplotypes <- getHaplotypes()
    distanceMatrix <- distanceMatrixHammingReactive() 
    distanceMatrix <- as.matrix(distanceMatrix)
    
    cluster_method <- input$radioButtonClusterHeatmap
    use_cluster <- input$checkboxCluster
    
    df_heatmap <- data.frame(
      Haplotype_row = rep(rownames(distanceMatrix), times = ncol(distanceMatrix)),
      Haplotype_col = rep(colnames(distanceMatrix), each = nrow(distanceMatrix)),
      Distance = as.vector(distanceMatrix)
    )
    if (use_cluster) {
      dist_obj <- distanceMatrixHammingReactive() 
      hclust_obj <- hclust(dist_obj, method = cluster_method) 
      cluster_order <- hclust_obj$labels[hclust_obj$order]
      
      df_heatmap$Haplotype_row <- factor(df_heatmap$Haplotype_row, levels = cluster_order)
      df_heatmap$Haplotype_col <- factor(df_heatmap$Haplotype_col, levels = cluster_order)
    } else {
      original_order <- rownames(distanceMatrix)
      df_heatmap$Haplotype_row <- factor(df_heatmap$Haplotype_row, levels = original_order)
      df_heatmap$Haplotype_col <- factor(df_heatmap$Haplotype_col, levels = original_order)
    }
    plotly::ggplotly(ggplot(df_heatmap, aes(x = Haplotype_col, y = Haplotype_row, fill = Distance)) +
                       geom_tile(color = "white") +
                       scale_fill_gradientn(
                         colors = rev(RColorBrewer::brewer.pal(9, "YlGnBu")),
                         name = "Distance"
                       ) +
                       theme_minimal() +
                       theme(
                         axis.title = element_blank(),
                         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                         legend.position = "right" 
                       ) +
                       coord_fixed())
  })
  
  # download
  output$buttonExportHeatmapPDF <- downloadHandler(
    filename = function() {paste(input$textExportFileName, "_heatmap.pdf", sep = "")},
    content = function(file) {
      haplotypes <- getHaplotypes()
      distanceMatrix <- distanceMatrixHammingReactive() 
      distanceMatrix <- as.matrix(distanceMatrix)
      cluster_method <- input$radioButtonClusterHeatmap
      use_cluster <- input$checkboxCluster
      df_heatmap <- data.frame(
        Haplotype_row = rep(rownames(distanceMatrix), times = ncol(distanceMatrix)),
        Haplotype_col = rep(colnames(distanceMatrix), each = nrow(distanceMatrix)),
        Distance = as.vector(distanceMatrix)
      )
      if (use_cluster) {
        dist_obj <- distanceMatrixHammingReactive()
        hclust_obj <- hclust(dist_obj, method = cluster_method) 
        cluster_order <- hclust_obj$labels[hclust_obj$order]
        
        df_heatmap$Haplotype_row <- factor(df_heatmap$Haplotype_row, levels = cluster_order)
        df_heatmap$Haplotype_col <- factor(df_heatmap$Haplotype_col, levels = cluster_order)
      } else {
        original_order <- rownames(distanceMatrix)
        df_heatmap$Haplotype_row <- factor(df_heatmap$Haplotype_row, levels = original_order)
        df_heatmap$Haplotype_col <- factor(df_heatmap$Haplotype_col, levels = original_order)
      }
      plot_to_save <- ggplot(df_heatmap, aes(x = Haplotype_col, y = Haplotype_row, fill = Distance)) +
        geom_tile(color = "white") +
        scale_fill_gradientn(
          colors = rev(RColorBrewer::brewer.pal(9, "YlGnBu")),
          name = "Distance"
        ) +
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          legend.position = "right" 
        ) +
        coord_fixed()
      ggplot2::ggsave(
        file, 
        plot = plot_to_save, 
        device = "pdf", 
        width = 14, 
        height = 10, 
        units = "in"
      )
    })
  
  # PCA plot
  map_sequence_to_haplotype <- reactive({
    haplotypes_full <- req(getAllHaplotypes()) 
    hap_indices <- attr(haplotypes_full, "index") 
    total_sequences <- nrow(vcfInput()) 
    hap_map <- rep(NA_character_, total_sequences)
    for (i in seq_along(hap_indices)) {
      sequence_indices <- hap_indices[[i]]
      hap_label <- rownames(haplotypes_full)[i]
      hap_map[sequence_indices] <- hap_label
    }
    return(factor(hap_map))
  }) %>% bindCache(getAllHaplotypes())
  # map sequence index to top haplotype label 
  map_sequence_to_top_haps <- reactive({
    hap_labels <- req(map_sequence_to_haplotype())
    hap_labels <- as.integer(sub("^H", "", hap_labels))
    hap_counts <- sort(table(hap_labels), decreasing = TRUE)
    num_top_haps <- input$sliderHaplotypes 
    top_haps <- names(hap_counts[1:num_top_haps])
    top_haps <- paste0("H", top_haps)
    simplified_labels <- as.character(hap_labels)
    simplified_labels <- paste0("H", simplified_labels)
    simplified_labels[!(simplified_labels %in% top_haps)] <- "Other"
    final_levels <- c(top_haps, "Other")
    return(factor(simplified_labels, levels = final_levels))
  })
  # final overview plot: PCA calculation 
  pca_result <- reactive({
    tryCatch({
      R.utils::withTimeout({
        vcf_dna <- req(vcfInput())
        withProgress(message = 'Calculating PCA...', value = 0, {
          incProgress(0.3, detail = "Converting to genind...")
          genind_data <- adegenet::genind(
            vcf_dna, 
            ploidy = getPloidy(),
            ind.names = rownames(vcf_dna), 
            loc.names = colnames(vcf_dna)
          )
          incProgress(0.6, detail = "Running dudi.pca...")
          pca <- ade4::dudi.pca(genind_data, nf=2, scannf=FALSE)
          incProgress(1, detail = "Complete.")
          return(pca) 
        })
      }, timeout = 20, onTimeout = "error")
    }, error = function(e) {
      if (inherits(e, "TimeoutException")) {
        validate(need(FALSE,"High computational load detected. Please filter your data further."))
      } else {
        validate(need(FALSE, paste(e$message)))
      }
    })
  })
  
  # reactive plot
  finalPcaPlotReactive <- reactive({
    pca <- req(pca_result()) 
    hap_labels_simplified <- req(map_sequence_to_top_haps()) 
    percent_variance <- round(pca$eig[1:2] / sum(pca$eig) * 100, 1)
    pca_scores <- as.data.frame(pca$li)
    pca_scores$Haplotype_Group <- hap_labels_simplified 
    pca_plot_data <- pca_scores %>%
      group_by(Haplotype_Group) %>%
      summarise(
        PC1 = mean(Axis1),
        PC2 = mean(Axis2),
        Frequency = n(),
        .groups = 'drop'
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(Haplotype_Group != "Other")
    if (nrow(pca_plot_data) == 0) {
      validate("No data remaining after filtering for Top Haplotypes.")
    }
    p <- ggplot(pca_plot_data, aes(x = PC1, y = PC2, text = Haplotype_Group, size = Frequency)) + 
      geom_point(alpha = 0.7, color = "grey50") + 
      scale_size_area(
        trans = "sqrt", 
        max_size = 12, 
        name = "Haplotype Frequency",
        breaks = scales::pretty_breaks(n = 4)
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.line.x = element_line(colour = "gray30"),
        axis.line.y = element_line(colour = "gray30"),
        axis.text.x = element_text(
          angle = 45, 
          hjust = 1, 
          size = 10 
        ),
        labs(
          title = NULL,
          color = "Haplotype Group"
        ))
    return(p)
  })
  
  # UI output
  output$finalPcaPlot <- plotly::renderPlotly({
    plotly::ggplotly(finalPcaPlotReactive(),tooltip = c("PC1", "PC2", "Haplotype_Group"))
  })
  
  # download
  output$buttonPcaPlotPDF <- downloadHandler(
    filename = function() {paste(input$textExportFileName, "_pca.pdf", sep = "")},
    content = function(file) {
      plot_to_save <- finalPcaPlotReactive()
      ggplot2::ggsave(
        file, 
        plot = plot_to_save, 
        device = "pdf", 
        width = 14, 
        height = 10, 
        units = "in"
      )
    }
  )
  
  # dendrogram
  dendrogramPlotReactive <- reactive({
    tryCatch({
      R.utils::withTimeout({
        hclust <- hclust(distanceMatrixHammingReactive(), method = input$radioButtonDendrogram)
        dendData <- dendro_data(hclust, type = "rectangle")
        p <- ggplot(segment(dendData)) +
          geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
                       color = "steelblue", size = 1) +
          geom_text(data = label(dendData),
                    aes(x = x, y = y - 0.5 * max(y), label = label),
                    angle = 90, hjust = 1.5, size = 3) +
          theme_minimal() +
          labs(title = NULL, y = "Hamming-Distance", x = "") +
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank())
        return(p)
      }, timeout = 20, error = function(e) {
        if (inherits(e, "TimeoutException")) {
          validate(need(FALSE,"High computational load detected. Please filter your data further."))
        } else {validate(need(FALSE, paste(e$message)))}
      })
    }) 
  })
  
  # UI output
  output$plotDendrogram <- renderPlot({
    dendrogramPlotReactive()
  })
  
  # download
  output$buttonExportDendroPDF <- downloadHandler(
    filename = function() {paste(input$textExportFileName, "_dendrogram.pdf", sep = "")},
    content = function(file) {
      plot_to_save <- dendrogramPlotReactive()
      ggplot2::ggsave(
        file, 
        plot = plot_to_save, 
        device = "pdf", 
        width = 14, 
        height = 10, 
        units = "in"
      )
    }
  )
  
  # allele heatmap
  plotAlleleHeatmapReactive <- reactive({
    req(calculateHaplotypeTable())
    req(totalEntries())
    total_heatmap_cells <- totalEntries()
    if (total_heatmap_cells > 1000000) {
      validate(
        need(FALSE, "High computational load detected. Please filter your data further.")
      )
    } else {
      allele_map_levels <- c("A", "C", "G", "T")
      allele_colors <- c(
        "A" = "#95bc0e",  
        "C" = "#006aa3",  
        "G" = "#fabb00",  
        "T" = "#e42032"   
      )
      df_snps <- data.frame(calculateHaplotypeTable(), check.names = FALSE) %>% dplyr::select(-freq)
      df_snps$Haplotype <- rownames(df_snps)
      
      df_long <- df_snps %>%
        tidyr::pivot_longer(
          cols = -Haplotype,
          names_to = "SNP",
          values_to = "Allele"
        )
      haplo_levels <- rev(df_snps$Haplotype)
      snp_levels <- colnames(calculateHaplotypeTable()) %>% setdiff("freq")
      
      df_long$Haplotype <- factor(df_long$Haplotype, levels = rev(rownames(df_snps)))
      df_long$SNP <- factor(df_long$SNP, levels = colnames(df_snps)[colnames(df_snps) != "Haplotype"])
      
      p <- ggplot(df_long, aes(x = SNP, y = Haplotype, fill = Allele)) +
        geom_tile(color = "white", linewidth = 1) +
        scale_fill_manual(values = allele_colors) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
          axis.title = element_blank(),
          panel.grid = element_blank()
        ) +
        labs(fill = "Allele")
      return(p)
    }
  })%>% bindCache(
    calculateHaplotypeTable()
  )
  
  # UI output
  output$plotAlleleHeatmap <- plotly::renderPlotly({
    req(calculateHaplotypeTable())
    req(totalEntries())

    if (totalEntries() > 1000000) {
      validate(need(FALSE, "High computational load detected. Please filter your data further."))
    }

    hapl_mat <- calculateHaplotypeTable()
    hapl_mat <- hapl_mat[rownames(hapl_mat) != "freq",
                         setdiff(colnames(hapl_mat), "freq"), drop = FALSE]

    dt      <- as.data.table(hapl_mat, keep.rownames = "Haplotype")
    dt_long <- data.table::melt(dt, id.vars = "Haplotype", variable.name = "SNP", value.name = "Allele")
    dt_long[, Haplotype := factor(Haplotype, levels = rev(rownames(hapl_mat)))]
    dt_long[, SNP       := factor(SNP,       levels = colnames(hapl_mat))]

    allele_levels <- c("A", "C", "G", "T")
    all_colors_v  <- c("#95bc0e", "#006aa3", "#fabb00", "#e42032")

    dt_long[, allele_idx := match(Allele, allele_levels) - 1L]

    wide_z    <- dcast(dt_long, Haplotype ~ SNP, value.var = "allele_idx")
    wide_text <- dcast(dt_long, Haplotype ~ SNP, value.var = "Allele")

    z_mat    <- as.matrix(wide_z[,    -1])
    text_mat <- as.matrix(wide_text[, -1])
    haplos   <- as.character(wide_z$Haplotype)
    snps     <- colnames(z_mat)

    # discrete colorscale with sharp boundaries
    n        <- length(all_colors_v)
    zmax_val <- max(n - 1L, 1L)
    cs <- vector("list", 0)
    for (i in seq_len(n)) {
      lo <- (i - 1) / zmax_val
      cs <- c(cs, list(list(lo, all_colors_v[i])))
      if (i < n) cs <- c(cs, list(list(i / zmax_val - 1e-9, all_colors_v[i])))
    }
    cs <- c(cs, list(list(1.0, all_colors_v[n])))

    plotly::plot_ly(
      z             = z_mat,
      x             = snps,
      y             = haplos,
      text          = text_mat,
      type          = "heatmap",
      colorscale    = cs,
      zmin          = 0L,
      zmax          = zmax_val,
      showscale     = FALSE,
      xgap          = 2,
      ygap          = 2,
      hovertemplate = "SNP: %{x}<br>Haplotype: %{y}<br>Allele: %{text}<extra></extra>"
    ) |>
      plotly::layout(
        plot_bgcolor = "white",
        xaxis = list(
          tickangle = 270,
          tickmode  = "array",
          tickvals  = snps,
          ticktext  = snps,
          ticks     = "",
          title     = ""
        ),
        yaxis = list(
          title    = "",
          ticks    = "",
          tickmode = "array",
          tickvals = haplos,
          ticktext = haplos,
          tickfont = list(size = 7, color = "gray30")
        )
      )
  })
  # download
  output$buttonExportAlleleHeatmapPDF <- downloadHandler(
    filename = function() {paste(input$textExportFileName, "_allele_heatmap.pdf", sep = "")},
    content = function(file) {
      plot_to_save <- plotAlleleHeatmapReactive()
      ggplot2::ggsave(
        file, 
        plot = plot_to_save, 
        device = "pdf", 
        width = 14, 
        height = 10, 
        units = "in"
      )
    }
  )
  
  # exports haplotype table
  output$buttonExportHaplotypeTable <- downloadHandler(
    filename = function() {paste0(input$textExportFileName, "_haplotypeTable.txt")},
    content = function(file) {
      data_to_export <- calculateHaplotypeTable()
      write.table(data_to_export, file, quote = FALSE, sep = "\t")
    }
  )

  # exports distance matrix
  output$buttonExportDistanceMatrix <- downloadHandler(
    filename = function() {paste0(input$textExportFileName, "_distanceMatrix.txt")},
    content = function(file) {
      data_to_export <- as.matrix(distanceMatrixHammingReactive())
      write.table(data_to_export, file, quote = FALSE, sep = "\t")
    }
  )
  ####---------------------------------------------------------------------####
  
  ####--------------------------- gwas analysis ---------------------------####
  # get GWAS data either from user or pre-stored files
  GwasDataFile <- reactive({
    req(isGwasDataSelected())
    if (!is.null(input$GwasInfo)) {
      gwas_data <- data.table::fread(input$GwasInfo$datapath)
      validate(
        need(ncol(gwas_data) == 10, 
             paste("Invalid GWAS file: Expected 10 columns, but found", ncol(gwas_data), 
                   ". Please check the file format."))
      )
      colnames(gwas_data) <- c("chromosome","start","end","name","score","strand","trait","risk_allele","pval","effect_size")
      return(gwas_data)
    } else if (isTRUE(input$gwasStored)) {
      github_gwas_url <- "https://raw.githubusercontent.com/TimHasenbein/Haplofun/main/01_data/gwas_snps.csv"
    } else if (isTRUE(input$hinfGwasStored)) {
      github_gwas_url <- "https://raw.githubusercontent.com/TimHasenbein/Haplofun/main/01_data/Haemophilus_gwas_snps_linreg.csv"
    }
    tryCatch({
      gwas_data <- data.table::fread(github_gwas_url)
      colnames(gwas_data) <- c("chromosome","start","end","name","score","strand","trait","risk_allele","pval","effect_size")
      gwas_data <- gwas_data %>%
        dplyr::mutate(
          start = as.integer(start), 
          end = as.integer(end)
        ) %>%
        dplyr::filter(!is.na(start))
      return(gwas_data)
    }, error = function(e) {
      print(paste("Error loading GitHub file:", e$message))
      return(data.frame(Message = "Error loading stored GWAS file from GitHub."))
    })
  }) %>% bindCache(input$GwasInfo$datapath, input$gwasStored, input$hinfGwasStored)
  
  # update the GWAS trait choices
  observeEvent(
    {
      input$mainTabPanel
      getGwasDataTable()
    },
    {
      req(getGwasDataTable())
      filteredGwasData <- getGwasDataTable()
      unique_gSNPs <- unique(paste0(filteredGwasData$Name,"_",filteredGwasData$Trait))
      all_choices_gSNPs <- c(unique_gSNPs)
      updateSelectizeInput(
        session = session,
        inputId = "traitFilter",
        label = "Select SNP-trait combinations:",
        choices = all_choices_gSNPs
      )
    }
  )
  
  # check GWAS availability
  output$gwasDataSourceAvailable <- reactive({
    !is.null(input$GwasInfo) || isTRUE(input$gwasStored) || isTRUE(input$hinfGwasStored)
  })
  outputOptions(output, "gwasDataSourceAvailable", suspendWhenHidden = FALSE)
  isGwasDataSelected <- reactive({
    !is.null(input$GwasInfo) || isTRUE(input$gwasStored) || isTRUE(input$hinfGwasStored)
  })
  
  # GWAS haplotype overlap: get a gwas data table with the variants that overlap the VCF variatns
  getGwasDataTable <- reactive({
    req(GwasDataFile())
    req(filteredVcfData())

    gwasData <- as.data.table(GwasDataFile())
    setnames(gwasData, c("Chromosome","Start","End","Name","Score","Strand",
                         "Trait","Risk_allele","Pval","Effect_size"))
    gwasData[, Pval := as.numeric(Pval)]

    vcf_fix <- getFIX(filteredVcfData())
    vcf_dt <- data.table(
      CHROM = vcf_fix[, "CHROM"],
      POS   = as.numeric(vcf_fix[, "POS"]),
      ID    = get_variant_names(filteredVcfData())
    )

    if (!any(grepl("^chr", head(vcf_dt$CHROM)))) {
      gwasData[, Chromosome := gsub("^chr", "", Chromosome, ignore.case = TRUE)]
    }

    setkey(vcf_dt, CHROM, POS)
    setkey(gwasData, Chromosome, Start)
    filteredGwasData <- gwasData[vcf_dt, nomatch = 0,
                                 on = .(Chromosome = CHROM, Start = POS),
                                 .(Chrom = Chromosome, Pos = Start, Name = ID,
                                   Trait, `Risk allele` = Risk_allele,
                                   `P-val` = Pval, `Effect size` = Effect_size)]

    setorder(filteredGwasData, Name, Trait, `Risk allele`, `P-val`)
    filteredGwasData <- unique(filteredGwasData, by = c("Name", "Trait", "Risk allele"))
    setorder(filteredGwasData, Pos)

    return(filteredGwasData)
  }) |> bindCache(GwasDataFile(), filteredVcfData())
  
  # dynamic P-value slider driven by the range in the loaded GWAS data
  output$gwas_pval_slider <- renderUI({
    data <- req(getGwasDataTable())
    pvals <- data$`P-val`[!is.na(data$`P-val`) & data$`P-val` > 0]
    if (length(pvals) == 0) return(NULL)
    slider_max <- ceiling(-log10(min(pvals)))
    sliderInput(
      inputId  = "gwas_pval_filter",
      label    = "-log10(P) filter:",
      min      = 0,
      max      = slider_max,
      value    = 0,
      step     = 0.5
    )
  })

  # filter the haplotype overlap with GWAS for a specific trait
  getFilteredGwasDataTable <- eventReactive(input$applyTraitFilter, {
    data <- getGwasDataTable()
    req(data)
    data$SNP_trait <- paste0(data$Name,"_",data$Trait)
    snp_trait <- input$traitFilter
    if (is.null(input$traitFilter)) {
      filtered_data <- data
    } else {
      filtered_data <- data[data$SNP_trait %in% snp_trait, , drop = FALSE]
    }
    filtered_data <- filtered_data %>% dplyr::select(-SNP_trait)
    pval_log <- input$gwas_pval_filter
    if (!is.null(pval_log) && pval_log > 0) {
      threshold <- 10^(-pval_log)
      filtered_data <- filtered_data[
        !is.na(filtered_data$`P-val`) & filtered_data$`P-val` <= threshold, ,
        drop = FALSE
      ]
    }
    return(filtered_data)
  }, ignoreNULL = FALSE)
  
  # get the GWAS SNP indices
  getGwasSnpIndices <- eventReactive(input$applyTraitFilter, {
    req(getFilteredGwasDataTable()) 
    req(filteredVcfData())
    gwasData <- getFilteredGwasDataTable() 
    gwasRsids <- as.character(gwasData$Name)
    vcfSnpIds <- get_variant_names(filteredVcfData())
    matchingSnpIndices <- which(vcfSnpIds %in% gwasRsids)
    return(unique(matchingSnpIndices))
  },ignoreNULL = FALSE)
  
  # get the plotting data for the GWAS heatmap
  calculateGwasHaplotypeHeatmapData <- eventReactive(input$applyTraitFilter, {
    req(isGwasDataSelected())
    req(calculateHaplotypeTable())
    req(getFilteredGwasDataTable())
    selected_trait <- input$traitFilter
    gwas_dt <- as.data.table(getFilteredGwasDataTable())
    gwas_dt$SNP_trait <- paste0(gwas_dt$Name,"_",gwas_dt$Trait)
    trait_filter_expr <- if (length(selected_trait) > 0) {
      quote(SNP_trait %in% selected_trait)
    } else {
      TRUE
    }
    gwas_risk_trait <- gwas_dt[
      eval(trait_filter_expr),
      .(SNP_trait = paste(unique(paste0(SNP_trait, "_", `Risk allele`)), collapse = "; ")),
      by = .(rsID = Name, RiskAllele = `Risk allele`,Pos)
    ]
    if (nrow(gwas_risk_trait) == 0) {
      return(NULL)
    }
    haplMatrix <- calculateHaplotypeTable()
    dt_long <- as.data.table(haplMatrix, keep.rownames = "haplotypes")
    dt_long <- data.table::melt(dt_long,
                                id.vars = "haplotypes",
                                variable.name = "SNP",
                                value.name = "allele")
    setkey(dt_long, SNP, allele)
    setkey(gwas_risk_trait, rsID, RiskAllele)
    final_dt <- gwas_risk_trait[dt_long, on = .(rsID = SNP, RiskAllele = allele)]
    final_dt[, trait_status := ifelse(is.na(SNP_trait), "Absent", "Present")]
    final_dt[, `:=`(
      trait_status = factor(trait_status),
      haplotypes = factor(haplotypes, levels = rev(unique(haplotypes))),
      SNP = rsID
    )]
    final_dt <- final_dt %>% filter(SNP != "freq")
    haplo_rows  <- setdiff(rownames(haplMatrix), "freq")
    gwas_snps   <- intersect(unique(gwas_risk_trait$rsID), colnames(haplMatrix))
    if (isTRUE(input$checkboxGwasCluster) && length(gwas_snps) > 0 && length(haplo_rows) > 1) {
      allele_mat <- haplMatrix[haplo_rows, gwas_snps, drop = FALSE]
      allele_mat[is.na(allele_mat)] <- "N"
      dist_matrix <- as.dist(Reduce("+", lapply(seq_len(ncol(allele_mat)), function(j)
        outer(allele_mat[, j], allele_mat[, j], "!=") * 1L)))
      hc <- hclust(dist_matrix, method = "ward.D2")
      clustered_haplo_order <- haplo_rows[hc$order]
    } else {
      clustered_haplo_order <- haplo_rows
    }
    final_dt[, haplotypes := factor(haplotypes, levels = clustered_haplo_order)]
    return(as.data.frame(final_dt))
  })
  
  # GWAS trait color
  GWAStraitColor <- eventReactive(input$applyTraitFilter, {
    req(calculateGwasHaplotypeHeatmapData())
    traits <- as.data.table(calculateGwasHaplotypeHeatmapData())
    if (is.null(traits) || nrow(traits) == 0) {
      return(NULL) 
    }
    traits <- traits%>%dplyr::select(rsID,SNP_trait,Pos)
    traits <- traits %>% 
      mutate(pos = as.numeric(Pos)) %>%
      arrange(pos)
    traits <- unique(traits$SNP_trait)
    traits <- traits[!is.na(traits)]
    sci_palette <- c(
      "#5F9EA0", "#E6B8A2", "#2F4F4F", "#B7E4C7", "#9575CD", "#FFF1A8", "#4682B4", "#A3C1AD", 
      "#336666", "#918e8e", "#008B8B", "#FFAB91", "#7A3734", "#EDC948", "#1A5276", "#80CBC4", 
      "#ffd6b8", "#90CAF9", "#1D8348", "#BCAAA4", "#00ACC1", "#C4A484", "#7E5109", "#B39DDB", 
      "#8A9A5B", "#D7CCC8", "#4DB6AC", "#EF9A9A", "#2E86C1", "#FFCC80", "#DCE775", "#B0BEC5", 
      "#708090", "#64B5F6", "#CD5C5C", "#A5D6A7", "#34495E", "#FFF9C4", "#4FC3F7", "#5D4037", 
      "#A9DFBF", "#B2DFDB", "#6A5ACD", "#FFECB3", "#7986CB", "#556B2F", "#8EBCBB", "#F9E79F",
      "#E0F7FA", "#D98880"                                                                    
    )
    n_traits <- length(traits)
    trait_colors <- setNames(rep_len(sci_palette, n_traits), traits)
    color_list <- as.list(trait_colors)
    final_colors_list <- list()
    for (i in seq_along(color_list)) {
      split_names <- unlist(strsplit(names(color_list)[i], ";\\s*"))
      current_color <- color_list[[i]]
      for (single_name in split_names) {
        final_colors_list[[single_name]] <- current_color
      }
    }
    combined_colors <- c(unlist(final_colors_list),trait_colors)
    combined_colors <- combined_colors[!duplicated(names(combined_colors))]
    return(combined_colors)
  },ignoreNULL = FALSE)
  
  # plot the GWAS heatmap
  output$plotGwasRiskHeatmap <- plotly::renderPlotly({
    req(GWAStraitColor())
    input$applyTraitFilter
    trait_colors <- GWAStraitColor()
    plot_data <- as.data.table(calculateGwasHaplotypeHeatmapData())

    if (is.null(plot_data) || nrow(plot_data) == 0) return(NULL)

    plot_data[, SNP := factor(SNP, levels = unique(SNP))]

    # map trait labels to integers: 0 = Absent, 1..N = traits
    trait_names  <- names(trait_colors)
    all_colors_v <- c("#F2F2F2", unname(trait_colors))

    plot_data[, trait_idx      := fifelse(is.na(SNP_trait), 0L, match(SNP_trait, trait_names))]
    plot_data[, snp_trait_text := fifelse(is.na(SNP_trait), "Absent", SNP_trait)]

    wide_z    <- dcast(plot_data, haplotypes ~ SNP, value.var = "trait_idx",      fill = 0L)
    wide_text <- dcast(plot_data, haplotypes ~ SNP, value.var = "snp_trait_text", fill = "Absent")

    z_mat    <- as.matrix(wide_z[,    -1])
    text_mat <- as.matrix(wide_text[, -1])
    haplos   <- as.character(wide_z$haplotypes)
    snps     <- colnames(z_mat)

    # discrete colorscale: sharp boundary between each colour band
    n        <- length(all_colors_v)
    zmax_val <- max(n - 1L, 1L)
    cs <- vector("list", 0)
    for (i in seq_len(n)) {
      lo <- (i - 1) / zmax_val
      cs <- c(cs, list(list(lo, all_colors_v[i])))
      if (i < n) cs <- c(cs, list(list(i / zmax_val - 1e-9, all_colors_v[i])))
    }
    cs <- c(cs, list(list(1.0, all_colors_v[n])))

    plotly::plot_ly(
      z             = z_mat,
      x             = snps,
      y             = haplos,
      text          = text_mat,
      type          = "heatmap",
      colorscale    = cs,
      zmin          = 0L,
      zmax          = zmax_val,
      showscale     = FALSE,
      xgap          = 1,
      ygap          = 1,
      hovertemplate = "SNP: %{x}<br>Haplotype: %{y}<br>Trait: %{text}<extra></extra>"
    ) |>
      plotly::layout(
        plot_bgcolor = "white",
        xaxis = list(showticklabels = FALSE, ticks = "", title = ""),
        yaxis = list(
          title    = "",
          ticks    = "",
          tickmode = "array",
          tickvals = haplos,
          ticktext = haplos,
          tickfont = list(size = 7, color = "gray30")
        )
      )
  })
  
  # download the GWAS heatmap
  output$buttonGwasRiskHeatmap <- downloadHandler(
    filename = function() {paste(input$textExportFileName, "_GWAS_risk_heatmap.pdf", sep = "")},
    content = function(file) {
      req(GWAStraitColor())
      trait_colors <- GWAStraitColor()
      plot_data <- as.data.table(calculateGwasHaplotypeHeatmapData())
      plot_data$SNP_trait <- ifelse(
        nchar(plot_data$SNP_trait) > 70, 
        paste0(substr(plot_data$SNP_trait, 1, 27), "..."), 
        plot_data$SNP_trait
      )
      names(trait_colors) <- ifelse(
        nchar(names(trait_colors)) > 70, 
        paste0(substr(names(trait_colors), 1, 27), "..."), 
        names(trait_colors)
      )
      plot_data$SNP <- factor(plot_data$SNP, levels = unique(plot_data$SNP))
      p <- ggplot(plot_data, aes(x = SNP, y = haplotypes, fill = SNP_trait)) +
        geom_tile(color = "black", linewidth = 0.1) +
        scale_fill_manual(
          values = trait_colors,
          na.value = "#F2F2F2", 
          name = "Trait"
        ) +
        labs(x = "SNP", y = "Haplotype") +
        theme_classic(base_size = 14) +
        theme(
          panel.border = element_rect(color = "white", fill = NA, linewidth = 1),
          panel.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(
            face = "bold",
            size = 18,
            hjust = 0.5,
            margin = margin(b = 10)
          ),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(
            face = "bold",
            color = "gray30",
            angle = 45,
            hjust = 1,
            vjust = 1
          ),
          axis.text.y = element_text(
            face = "bold",
            color = "gray30"
          ),
          axis.title = element_text(size = 14, face = "bold"),
          legend.title = element_blank()
        ) +
        labs(
          x = NULL,
          y = NULL,
          title = NULL
        ) + 
        guides(fill="none")
      plot_legend <- p + 
        theme(legend.position = "right") + 
        guides(fill = guide_legend(ncol = 2, title = "Trait"))
      
      plot_legend <- cowplot::get_legend(plot_legend)
      pdf(file, width = 14, height = 10)
      print(p)
      grid::grid.newpage()
      grid::grid.draw(plot_legend)
      dev.off()
    }
  )
  
  #  output gwas haplotype table: table with haplotypes and information of gSNPs within the vcf
  output$gwasHaplotypeTable <- DT::renderDataTable({
    input$applyTraitFilter
    req(getGwasSnpIndices())
    req(calculateGwasHaplotypeHeatmapData())
    gwasIndices <- getGwasSnpIndices()
    if (length(gwasIndices) == 0) {
      return(DT::datatable(data.frame(Message = "No GWAS SNPs for the selected trait overlap with haplotypes."), options = list(dom = 't')))
    }
    haplotypeTable <- calculateHaplotypeTable()
    vcfSnpIds <- get_variant_names(filteredVcfData())
    gwasSnpIds <- vcfSnpIds[gwasIndices]
    displayTable <- haplotypeTable%>% dplyr::select(-freq)
    displayTable <- t(displayTable)
    displayTable <- displayTable[rownames(displayTable) %in% gwasSnpIds,, drop = FALSE]
    dt <- DT::datatable(
      displayTable,
      options = list(scrollY = "300px", scrollX = TRUE, pageLength = 10)
    )
    dt <- dt %>% DT::formatStyle(
      columns = 1,
      target = 'row',
      backgroundColor = '#FF9E99'
    )
    dt
  })
  
  # download the gwas haplotype table
  output$buttonGwasHaplotypeTable <- downloadHandler(
    filename = function() {paste0(input$textExportFileName, "_GWAS_Allele_table.txt")},
    content = function(file) {
      req(getGwasSnpIndices())
      gwasIndices <- getGwasSnpIndices()
      haplotypeTable <- calculateHaplotypeTable()
      vcfSnpIds <- get_variant_names(filteredVcfData())
      gwasSnpIds <- vcfSnpIds[gwasIndices]
      displayTable <- haplotypeTable%>% dplyr::select(-freq)
      displayTable <- t(displayTable)
      displayTable <- displayTable[rownames(displayTable) %in% gwasSnpIds,, drop = FALSE]
      write.table(displayTable, file, quote = FALSE, sep = "\t", row.names = T)
    }
  )
  
  # gwas table haplotypes: a table with the gSNP information of SNPs within the vcf
  output$gwasSnpTable <- DT::renderDataTable({
    input$applyTraitFilter
    table <- req(getFilteredGwasDataTable()) 
    table <- table %>% dplyr::select(Chrom,Pos,Name,Trait,`Risk allele`,`P-val`,`Effect size`)
    colnames(table) <- c("Chromosome","Position", "Name","Trait","Risk allele","P-value","Effect size")
    DT::datatable(
      table, 
      options = list(scrollY = "300px", scrollX = TRUE, pageLength = 10, stateSave = FALSE),
      caption = "GWAS-SNPs that overlap in haplogroups."
    )
  })
  
  # download the gwas table haplotypes
  output$buttonGwasSnpTable <- downloadHandler(
    filename = function() {paste0(input$textExportFileName, "_GWAS_SNP_table.txt")},
    content = function(file) {
      data_to_export <- getFilteredGwasDataTable()
      write.table(data_to_export, file, quote = FALSE, sep = "\t", row.names = F)
    }
  )
  
  # GWAS lollipop plot
  output$plotGwasLolli <- plotly::renderPlotly({
    input$applyTraitFilter
    req(GWAStraitColor())
    trait_colors <- GWAStraitColor()
    # update single trait colors
    traits_in_heat <- data.frame(SNP_trait = names(trait_colors), color = unname(trait_colors))
    traits_in_heat <- traits_in_heat %>% tidyr::separate_rows(SNP_trait, sep = "; ")
    trait_colors <- setNames(traits_in_heat$color, traits_in_heat$SNP_trait)
    # plot
    plot_data <- req(getFilteredGwasDataTable())
    # new 
    status_info <- calculateGwasHaplotypeHeatmapData()
    status_info <- status_info %>% distinct(rsID, RiskAllele,SNP_trait,trait_status) %>% tidyr::drop_na() %>% dplyr::select(-trait_status)
    status_info <- status_info %>% tidyr::separate_rows(SNP_trait, sep = "; ")
    colnames(status_info) <- c("Name","Risk allele","SNP_trait")
    # merge on name + risk allele; SNP_trait comes from status_info (allele-specific)
    plot_data <- plot_data %>% merge(status_info, by = c("Name", "Risk allele"))
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      error_message <- paste("No data to plot")
      return(
        plotly::ggplotly(ggplot() +
                           annotate("text", x = 0.5, y = 0.5, label = error_message, size = 6) +
                           theme_void()
        ))
    }
    plot_data <- plot_data %>% dplyr::select(Chrom,Pos,`P-val`,`Effect size`,`Risk allele`,Name,SNP_trait) %>% arrange(Pos)
    #plot_data$label <- paste0(plot_data$Name,"_",plot_data$Trait)
    plot_data$`Effect size` <- as.numeric(plot_data$`Effect size`)
    plot_data$`-log10(P-val)` <- -log10(plot_data$`P-val`)
    base_y_afc <- 0
    plotly::ggplotly(ggplot(plot_data, aes(x = Pos)) +
                       geom_segment(
                         aes(
                           x = Pos,
                           xend = Pos,
                           y = base_y_afc, 
                           yend = `Effect size`, 
                         ),
                         color = "black",
                         linewidth = 0.2
                       ) +
                       geom_point(
                         aes(
                           y = `Effect size`, 
                           size = `-log10(P-val)`,
                           color = SNP_trait
                         ),
                         stroke = 1.2, 
                         alpha = 0.7
                       ) +
                       scale_color_manual(values = trait_colors, name = "Trait") + 
                       geom_hline(yintercept = base_y_afc, linetype = "dashed", color = "gray50", linewidth = 0.5) +
                       scale_x_continuous(labels = scales::comma) + 
                       labs(
                         title = NULL,
                         x = "Genomic Position",
                         y = "Effect size" 
                       ) +
                       theme_classic() +
                       theme(
                         panel.border = element_blank(),
                         legend.position = "none",
                         axis.line.x = element_line(color = "black")
                       ))
  })
  
  # download the GWAS lollipop plot
  output$buttonGwasLolli <- downloadHandler(
    filename = function() {paste(input$textExportFileName, "_GWAS_lollipop_plot.pdf", sep = "")},
    content = function(file) {
      req(GWAStraitColor())
      trait_colors <- GWAStraitColor()
      # update single trait colors
      traits_in_heat <- data.frame(SNP_trait = names(trait_colors), color = unname(trait_colors))
      traits_in_heat <- traits_in_heat %>% tidyr::separate_rows(SNP_trait, sep = "; ")
      trait_colors <- setNames(traits_in_heat$color, traits_in_heat$SNP_trait)
      # plot
      plot_data <- req(getFilteredGwasDataTable())
      # new 
      status_info <- calculateGwasHaplotypeHeatmapData()
      status_info <- status_info %>% distinct(rsID, RiskAllele,SNP_trait,trait_status) %>% tidyr::drop_na() %>% dplyr::select(-trait_status)
      status_info <- status_info %>% tidyr::separate_rows(SNP_trait, sep = "; ")
      colnames(status_info) <- c("Name","Risk allele","SNP_trait")
      # merge on Name + Risk allele; SNP_trait comes from status_info (allele-specific)
      plot_data <- plot_data %>% merge(status_info, by = c("Name", "Risk allele"))
      if (is.null(plot_data) || nrow(plot_data) == 0) {
        error_message <- paste("No data to plot")
        return(
          plotly::ggplotly(ggplot() +
                             annotate("text", x = 0.5, y = 0.5, label = error_message, size = 6) +
                             theme_void()
          ))
      }
      plot_data <- plot_data %>% dplyr::select(Chrom,Pos,`P-val`,`Effect size`,`Risk allele`,Name,SNP_trait) %>% arrange(Pos)
      plot_data$SNP_trait <- ifelse(
        nchar(plot_data$SNP_trait) > 70, 
        paste0(substr(plot_data$Trait, 1, 27), "..."), 
        plot_data$SNP_trait
      )
      #plot_data$label <- paste0(plot_data$Name,"_",plot_data$Trait)
      plot_data$`Effect size` <- as.numeric(plot_data$`Effect size`)
      plot_data$`-log10(P-val)` <- -log10(plot_data$`P-val`)
      base_y_afc <- 0
      p <- ggplot(plot_data, aes(x = Pos)) +
        geom_segment(
          aes(
            x = Pos,
            xend = Pos,
            y = base_y_afc, 
            yend = `Effect size`, 
          ),
          color = "black",
          linewidth = 0.2
        ) +
        geom_point(
          aes(
            y = `Effect size`, 
            size = `-log10(P-val)`,
            color = SNP_trait
          ),
          stroke = 1.2, 
          alpha = 0.7
        ) +
        scale_color_manual(values = trait_colors, name = "Trait") + 
        geom_hline(yintercept = base_y_afc, linetype = "dashed", color = "gray50", linewidth = 0.5) +
        scale_x_continuous(labels = scales::comma) + 
        labs(
          title = NULL,
          x = "Genomic Position",
          y = "Effect size" 
        ) +
        theme_classic() +
        theme(
          panel.border = element_blank(),
          legend.position = "none",
          axis.line.x = element_line(color = "black")
        )
      plot_legend <- p + 
        theme(legend.position = "right") + 
        guides(fill = guide_legend(ncol = 2))
      
      plot_legend <- cowplot::get_legend(plot_legend)
      pdf(file, width = 14, height = 2)
      print(p)
      grid::grid.newpage()
      grid::grid.draw(plot_legend)
      dev.off()
    }
  )
  ####---------------------------------------------------------------------####
  
  ####--------------------------- qtl analysis ---------------------------####
  # get QTL data either from user or pre-stored files
  eqtlDataFile <- reactive({
    req(iseqtlDataSelected())
    if (!is.null(input$eQTLInfo)) {
      eqtl_data <- data.table::fread(input$eQTLInfo$datapath)
      validate(
        need(ncol(eqtl_data) == 10, 
             paste("Invalid QTL file: Expected 10 columns, but found", ncol(eqtl_data), 
                   ". Please check the file format."))
      )
      colnames(eqtl_data) <- c("chromosome","start","end","name","score","strand","tissue","pip","afc","gene_name")
      return(eqtl_data)
    }
    if (isTRUE(input$eqtlStored)) {
      github_eqtl_url <- "https://raw.githubusercontent.com/TimHasenbein/Haplofun/main/01_data/GTEx_eQTLs_v10_05.csv"
      tryCatch({
        eqtl_data <- data.table::fread(github_eqtl_url)
        colnames(eqtl_data) <- c("chromosome","start","end","name","score","strand","tissue","pip","afc","gene_name")
        return(eqtl_data)
      }, error = function(e) {
        print(paste("Error loading GitHub file:", e$message))
        return(data.frame(Message = "Error loading stored eQTL file from GitHub."))
      })
    }
    return(NULL)
  }) %>% bindCache(input$eQTLInfo$datapath, input$eqtlStored)
  
  # uptade QTL tissue choices
  observeEvent(
    {
      input$mainTabPanel
      geteQTLDataTable()
    },
    {
      req(geteQTLDataTable())
      filteredeQTLData <- geteQTLDataTable()
      unique_tissues <- unique(filteredeQTLData$tissue)
      all_choices <- c("All", unique_tissues)
      unique_qSNPs <- unique(paste0(filteredeQTLData$ID,"_",filteredeQTLData$gene_name))
      all_choices_qSNPs <- c(unique_qSNPs)
      updateSelectizeInput(
        session = session,
        inputId = "qtlFilter",
        label = "Select QTLs:",
        choices = all_choices_qSNPs
      )
    }
  )
  
  #  check QTL availability
  output$eQTLFileUploaded <- reactive({
    !is.null(input$eQTLInfo) || isTRUE(input$eqtlStored)
  })
  outputOptions(output, "eQTLFileUploaded", suspendWhenHidden = FALSE)
  iseqtlDataSelected <- reactive({
    !is.null(input$eQTLInfo) || isTRUE(input$eqtlStored)
  })
  
  # QTL haplotype overlap: get a QTL data table with the variants that overlap the VCF variants
  geteQTLDataTable <- reactive({
    req(iseqtlDataSelected()) 
    req(filteredVcfData()) 
    eQTLData <- eqtlDataFile()
    vcf_fix_df <- getFIX(filteredVcfData()) %>% 
      as.data.frame() %>%
      mutate(POS = as.numeric(POS))
    vcf_fix_df$ID <-get_variant_names(filteredVcfData())
    if (any(grepl("^chr", head(vcf_fix_df[[1]])))) {
      eQTLData <- eQTLData
    } else {
      eQTLData[[1]] <- gsub("^chr", "", eQTLData[[1]], ignore.case = TRUE)
    } 
    filtered_eQTLData <- eQTLData %>%
      inner_join(
        vcf_fix_df,
        by = c("chromosome" = "CHROM", "start" = "POS")
      ) %>%
      dplyr::select(
        chromosome, start, end, name, score, strand, tissue, pip, afc, gene_name,ID
      )
    # new keep tissue with smallest pvalue only
    filtered_eQTLData <- filtered_eQTLData %>% group_by(tissue,name,gene_name,ID) %>% 
      arrange(desc(as.numeric(pip)), .by_group = TRUE) %>%
      dplyr::slice(1) %>%
      ungroup()
    filtered_eQTLData <- filtered_eQTLData %>% arrange(start)
    return(filtered_eQTLData)
  })
  
  # filter the haplotype overlap with QTLs for a specific tissue
  getFilteredeQTLDataTable <- eventReactive(input$applyQTLFilter, {
    data <- geteQTLDataTable()
    data$QTL_target <- paste0(data$ID,"_",data$gene_name)
    req(data)
    snp_target <- input$qtlFilter
    if (is.null(input$qtlFilter)) {
      filtered_data <- data 
    } else {
      filtered_data <- data[data$QTL_target %in% snp_target, , drop = FALSE]
    }
    filtered_data <- filtered_data[filtered_data$pip >= input$pip_range, , drop = FALSE]
    return(filtered_data)
  }, ignoreNULL = FALSE)
  
  # get the plotting data for the QTL heatmap
  calculateeQTLHaplotypeHeatmapData <- eventReactive(input$applyQTLFilter, {
    req(iseqtlDataSelected())
    req(calculateHaplotypeTable())
    req(getFilteredeQTLDataTable())
    selected_snps <- input$qtlFilter
    qtl_dt <- as.data.table(getFilteredeQTLDataTable())
    qtl_dt$QTL_target <- paste0(qtl_dt$ID,"_",qtl_dt$gene_name)
    qtls_risk <- qtl_dt[, .(
      rsID = .SD[[11]],
      Target = .SD[[12]],
      RiskAllele = sub(".*/", "", .SD[[4]]),
      Pos = .SD[[2]]
    )]
    snp_filter_expr <- if (length(selected_snps) > 0) {
      quote(Target %in% selected_snps)
    } else {
      TRUE
    }
    qtl_risk_target <- qtls_risk[
      eval(snp_filter_expr),
      .(Target = paste(unique(paste0(Target, "_", RiskAllele)), collapse = "; ")),
      by = .(rsID, RiskAllele, Pos)
    ]
    if (nrow(qtl_risk_target) == 0) {
      return(NULL)
    }
    haplMatrix <- calculateHaplotypeTable()
    dt_long <- as.data.table(haplMatrix, keep.rownames = "haplotypes")
    dt_long <- data.table::melt(
      dt_long,
      id.vars = "haplotypes",
      variable.name = "SNP",
      value.name = "allele"
    )
    setkey(dt_long, SNP, allele)
    setkey(qtl_risk_target, rsID, RiskAllele)
    final_dt <- qtl_risk_target[dt_long, on = .(rsID = SNP, RiskAllele = allele)]
    final_dt[, status := ifelse(is.na(Target), "Absent", "Present")]
    final_dt[, `:=`(
      status = factor(status),
      haplotypes = factor(haplotypes, levels = rev(unique(haplotypes))),
      SNP = rsID
    )]
    final_dt <- final_dt %>% filter(SNP != "freq")
    haplo_rows <- setdiff(rownames(haplMatrix), "freq")
    qtl_snps   <- intersect(unique(qtl_risk_target$rsID), colnames(haplMatrix))
    if (isTRUE(input$checkboxQtlCluster) && length(qtl_snps) > 0 && length(haplo_rows) > 1) {
      allele_mat <- haplMatrix[haplo_rows, qtl_snps, drop = FALSE]
      allele_mat[is.na(allele_mat)] <- "N"
      dist_matrix <- as.dist(Reduce("+", lapply(seq_len(ncol(allele_mat)), function(j)
        outer(allele_mat[, j], allele_mat[, j], "!=") * 1L)))
      hc <- hclust(dist_matrix, method = "ward.D2")
      clustered_haplo_order <- haplo_rows[hc$order]
    } else {
      clustered_haplo_order <- haplo_rows
    }
    final_dt[, haplotypes := factor(haplotypes, levels = clustered_haplo_order)]
    return(as.data.frame(final_dt))
  },ignoreNULL = FALSE)
  
  # QTL tissue color
  QTLtargetColor <- eventReactive(input$applyQTLFilter, {
    req(calculateeQTLHaplotypeHeatmapData())
    target <- as.data.table(calculateeQTLHaplotypeHeatmapData())
    if (is.null(target) || nrow(target) == 0) {
      return(NULL) 
    }
    target <- target%>%dplyr::select(rsID,Target,Pos)
    target <- target %>%
      mutate(pos = as.numeric(Pos)) %>%
      arrange(pos)
    target <- unique(target$Target)
    target <- target[!is.na(target)] #
    sci_palette <- c(
      "#5F9EA0", "#E6B8A2", "#2F4F4F", "#B7E4C7", "#9575CD", "#FFF1A8", "#4682B4", "#A3C1AD", 
      "#336666", "#918e8e", "#008B8B", "#FFAB91", "#7A3734", "#EDC948", "#1A5276", "#80CBC4", 
      "#ffd6b8", "#90CAF9", "#1D8348", "#BCAAA4", "#00ACC1", "#C4A484", "#7E5109", "#B39DDB", 
      "#8A9A5B", "#D7CCC8", "#4DB6AC", "#EF9A9A", "#2E86C1", "#FFCC80", "#DCE775", "#B0BEC5", 
      "#708090", "#64B5F6", "#CD5C5C", "#A5D6A7", "#34495E", "#FFF9C4", "#4FC3F7", "#5D4037", 
      "#A9DFBF", "#B2DFDB", "#6A5ACD", "#FFECB3", "#7986CB", "#556B2F", "#8EBCBB", "#F9E79F",
      "#E0F7FA", "#D98880"                                                                    
    )
    n_targets <- length(target)
    target_colors <- setNames(rep_len(sci_palette, n_targets), target)
    color_list <- as.list(target_colors)
    final_colors_list <- list()
    for (i in seq_along(color_list)) {
      split_names <- unlist(strsplit(names(color_list)[i], ";\\s*"))
      current_color <- color_list[[i]]
      for (single_name in split_names) {
        final_colors_list[[single_name]] <- current_color
      }
    }
    combined_colors <- c(unlist(final_colors_list),target_colors)
    combined_colors <- combined_colors[!duplicated(names(combined_colors))]
    return(combined_colors)
  },ignoreNULL = FALSE)
  
  # plot the QTL heatmap
  output$ploteQTLRiskHeatmap <- plotly::renderPlotly({
    req(QTLtargetColor())
    input$applyQTLFilter
    target_colors <- QTLtargetColor()
    plot_data <- as.data.table(calculateeQTLHaplotypeHeatmapData())

    if (is.null(plot_data) || nrow(plot_data) == 0) return(NULL)

    plot_data[, SNP := factor(SNP, levels = unique(SNP))]

    # map target labels to integers: 0 = Absent, 1..N = targets
    target_names  <- names(target_colors)
    all_colors_v  <- c("#F2F2F2", unname(target_colors))

    plot_data[, target_idx  := fifelse(is.na(Target), 0L, match(Target, target_names))]
    plot_data[, target_text := fifelse(is.na(Target), "Absent", Target)]

    wide_z    <- dcast(plot_data, haplotypes ~ SNP, value.var = "target_idx",  fill = 0L)
    wide_text <- dcast(plot_data, haplotypes ~ SNP, value.var = "target_text", fill = "Absent")

    z_mat    <- as.matrix(wide_z[,    -1])
    text_mat <- as.matrix(wide_text[, -1])
    haplos   <- as.character(wide_z$haplotypes)
    snps     <- colnames(z_mat)

    # discrete colorscale with sharp boundaries
    n        <- length(all_colors_v)
    zmax_val <- max(n - 1L, 1L)
    cs <- vector("list", 0)
    for (i in seq_len(n)) {
      lo <- (i - 1) / zmax_val
      cs <- c(cs, list(list(lo, all_colors_v[i])))
      if (i < n) cs <- c(cs, list(list(i / zmax_val - 1e-9, all_colors_v[i])))
    }
    cs <- c(cs, list(list(1.0, all_colors_v[n])))

    plotly::plot_ly(
      z             = z_mat,
      x             = snps,
      y             = haplos,
      text          = text_mat,
      type          = "heatmap",
      colorscale    = cs,
      zmin          = 0L,
      zmax          = zmax_val,
      showscale     = FALSE,
      xgap          = 1,
      ygap          = 1,
      hovertemplate = "SNP: %{x}<br>Haplotype: %{y}<br>Target: %{text}<extra></extra>"
    ) |>
      plotly::layout(
        plot_bgcolor = "white",
        xaxis = list(showticklabels = FALSE, ticks = "", title = ""),
        yaxis = list(
          title    = "",
          ticks    = "",
          tickmode = "array",
          tickvals = haplos,
          ticktext = haplos,
          tickfont = list(size = 7, color = "gray30")
        )
      )
  })
  
  # download the QTL heatmap
  output$buttonploteQTLRiskHeatmap <- downloadHandler(
    filename = function() {paste(input$textExportFileName, "_QTL_heatmap.pdf", sep = "")},
    content = function(file) {
      req(QTLtargetColor())
      target_colors <- QTLtargetColor()
      plot_data <- as.data.table(calculateeQTLHaplotypeHeatmapData())
      plot_data$Target <- ifelse(
        nchar(plot_data$Target) > 70, 
        paste0(substr(plot_data$Target, 1, 27), "..."), 
        plot_data$Target
      )
      names(target_colors) <- ifelse(
        nchar(names(target_colors)) > 70, 
        paste0(substr(names(target_colors), 1, 27), "..."), 
        names(target_colors)
      )
      plot_data$SNP <- factor(plot_data$SNP, levels = unique(plot_data$SNP))
      p <- ggplot(plot_data, aes(x = SNP, y = haplotypes, fill = Target)) +
        geom_tile(color = "black", linewidth = 0.1) +
        scale_fill_manual(
          values = target_colors,
          na.value = "#F2F2F2", 
          name = "Target"
        ) +
        labs(x = "SNP", y = "Haplotype") +
        theme_classic(base_size = 14) +
        theme(
          panel.border = element_rect(color = "white", fill = NA, linewidth = 1),
          panel.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(
            face = "bold",
            size = 18,
            hjust = 0.5,
            margin = margin(b = 10)
          ),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(
            face = "bold",
            color = "gray30",
            angle = 45,
            hjust = 1,
            vjust = 1
          ),
          axis.text.y = element_text(
            face = "bold",
            color = "gray30"
          ),
          axis.title = element_text(size = 14, face = "bold"),
          legend.title = element_blank()
        ) +
        labs(
          x = NULL,
          y = NULL,
          title = NULL,
        ) + 
        guides(fill="none")
      plot_legend <- p + 
        theme(legend.position = "right") + 
        guides(fill = guide_legend(ncol = 2, title = "Targets"))
      plot_legend <- cowplot::get_legend(plot_legend)
      pdf(file, width = 14, height = 10)
      print(p)
      grid::grid.newpage()
      grid::grid.draw(plot_legend)
      dev.off()
    }
  )
  
  # output gwas haplotype table: a table with the haplotypes and information of gSNPs within the vcf
  output$eqtlHaplotypeTable <- DT::renderDataTable({
    input$applyQTLFilter
    req(getFilteredeQTLDataTable())
    req(calculateeQTLHaplotypeHeatmapData())
    filtered_eQTLData <- getFilteredeQTLDataTable()
    filtered_eQTLData <- filtered_eQTLData$ID
    if (length(filtered_eQTLData) == 0) {
      return(DT::datatable(data.frame(Message = "No QTL variants overlap with haplotypes."), options = list(dom = 't')))
    }
    haplotypeTable <- calculateHaplotypeTable()
    displayTable <- haplotypeTable%>% dplyr::select(-freq)
    displayTable <- t(displayTable)
    displayTable <- displayTable[rownames(displayTable) %in% filtered_eQTLData,, drop = FALSE]
    dt <- DT::datatable(
      displayTable,
      options = list(scrollY = "300px",scrollX = TRUE, pageLength = 10)
    )
    dt <- dt %>% DT::formatStyle(
      columns = 1,
      target = 'row',
      backgroundColor = '#D8F5DD'
    )
    dt
  })
  
  # download the qtl haplotype table
  output$buttonploteqtlHaplotypeTable <- downloadHandler(
    filename = function() {paste0(input$textExportFileName, "_QTL_Allele_table.txt")},
    content = function(file) {
      req(getFilteredeQTLDataTable())
      filtered_eQTLData <- getFilteredeQTLDataTable()
      filtered_eQTLData <- filtered_eQTLData$ID
      haplotypeTable <- calculateHaplotypeTable()
      displayTable <- haplotypeTable%>% dplyr::select(-freq)
      displayTable <- t(displayTable)
      displayTable <- displayTable[rownames(displayTable) %in% filtered_eQTLData,, drop = FALSE]
      write.table(displayTable, file, quote = FALSE, sep = "\t", row.names = T)
    }
  )
  
  # qtl table haplotypes: a table with the qSNP information of SNPs within the vcf
  output$eQTLSnpTable <- DT::renderDataTable({
    input$applyQTLFilter
    table <- req(getFilteredeQTLDataTable()) 
    table <- table %>% dplyr::select(chromosome, start, ID, gene_name, tissue, name, pip, afc)
    colnames(table) <- c("Chromosome","Position","Name","Target","Tissue","Allele","PIP","Effect size")
    DT::datatable(
      table, 
      options = list(scrollY = "300px", scrollX = TRUE, pageLength = 10),
      caption = "QTL variants that overlap haplogroups."
    )
  })
  
  # download the qtl table haplotypes
  output$buttoneQTLSnpTable <- downloadHandler(
    filename = function() {paste0(input$textExportFileName, "_QTL_SNP_table.txt")},
    content = function(file) {
      data_to_export <- getFilteredeQTLDataTable()
      write.table(data_to_export, file, quote = FALSE, sep = "\t", row.names = F)
    }
  )
  
  # QTL lollipop plot
  output$plotEqtlsLolli <- plotly::renderPlotly({
    req(QTLtargetColor())
    input$applyQTLFilter
    target_colors <- QTLtargetColor()
    # update single trait colors
    targets_in_heat <- data.frame(target = names(target_colors), color = unname(target_colors))
    targets_in_heat <- targets_in_heat %>% tidyr::separate_rows(target, sep = "; ")
    target_colors <- setNames(targets_in_heat$color, targets_in_heat$target)
    # plot
    plot_data <- req(getFilteredeQTLDataTable())
    # new 
    status_info <- calculateeQTLHaplotypeHeatmapData()
    status_info <- status_info %>% dplyr::distinct(rsID, RiskAllele,Target,status) %>% tidyr::drop_na() %>% dplyr::select(-status)
    status_info <- status_info %>% tidyr::separate_rows(Target, sep = "; ")
    colnames(status_info) <- c("ID","effect_allele","Target")
    # merge
    plot_data$effect_allele <- gsub("./(.)", "\\1", plot_data$name)
    plot_data <- plot_data %>% merge(status_info)
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      error_message <- paste("No data to plot")
      return(
        plotly::ggplotly(ggplot() +
                           annotate("text", x = 0.5, y = 0.5, label = error_message, size = 6) +
                           theme_void()
        ))
    }
    plot_data <- plot_data %>% dplyr::select(chromosome,start,pip,afc,Target,ID) %>% arrange(start)
    base_y_afc <- 0
    plotly::ggplotly(ggplot(plot_data, aes(x = start)) +
                       geom_segment(
                         aes(
                           x = start,
                           xend = start,
                           y = base_y_afc, 
                           yend = afc, 
                         ),
                         color = "black",
                         linewidth = 0.2
                       ) +
                       geom_point(
                         aes(
                           y = afc, 
                           color = Target,
                           size = pip
                         ),
                         stroke = 1.2, 
                         alpha = 0.9
                       ) +
                       geom_hline(yintercept = base_y_afc, linetype = "dashed", color = "gray50", linewidth = 0.5) +
                       scale_color_manual(values = target_colors, name = "Target") + 
                       scale_x_continuous(labels = scales::comma) + 
                       labs(
                         title = NULL,
                         x = "Genomic Position",
                         y = "Allele Fold Change" 
                       ) +
                       theme_classic() +
                       theme(
                         panel.border = element_blank(),
                         legend.position = "none",
                         axis.line.x = element_line(color = "black")
                       ))
  })
  
  # download the qtl lollipop plot
  output$buttonplotEqtlsLolli <- downloadHandler(
    filename = function() {paste(input$textExportFileName, "_QTL_lollipop_plot.pdf", sep = "")},
    content = function(file) {
      req(QTLtargetColor())
      input$applyQTLFilter
      target_colors <- QTLtargetColor()
      # update single trait colors
      targets_in_heat <- data.frame(target = names(target_colors), color = unname(target_colors))
      targets_in_heat <- targets_in_heat %>% tidyr::separate_rows(target, sep = "; ")
      target_colors <- setNames(targets_in_heat$color, targets_in_heat$target)
      # plot
      plot_data <- req(getFilteredeQTLDataTable())
      # new 
      status_info <- calculateeQTLHaplotypeHeatmapData()
      status_info <- status_info %>% dplyr::distinct(rsID, RiskAllele,Target,status) %>% tidyr::drop_na() %>% dplyr::select(-status)
      status_info <- status_info %>% tidyr::separate_rows(Target, sep = "; ")
      colnames(status_info) <- c("ID","effect_allele","Target")
      # merge
      plot_data$effect_allele <- gsub("./(.)", "\\1", plot_data$name)
      plot_data <- plot_data %>% merge(status_info)
      if (is.null(plot_data) || nrow(plot_data) == 0) {
        error_message <- paste("No data to plot")
        return(
          plotly::ggplotly(ggplot() +
                             annotate("text", x = 0.5, y = 0.5, label = error_message, size = 6) +
                             theme_void()
          ))
      }
      plot_data <- plot_data %>% dplyr::select(chromosome,start,pip,afc,Target,ID) %>% arrange(start)
      base_y_afc <- 0
      p <- ggplot(plot_data, aes(x = start)) +
        geom_segment(
          aes(
            x = start,
            xend = start,
            y = base_y_afc, 
            yend = afc, 
          ),
          color = "black",
          linewidth = 0.2
        ) +
        geom_point(
          aes(
            y = afc, 
            color = Target,
            size = pip
          ),
          stroke = 1.2, 
          alpha = 0.9
        ) +
        geom_hline(yintercept = base_y_afc, linetype = "dashed", color = "gray50", linewidth = 0.5) +
        scale_color_manual(values = target_colors, name = "Target") + 
        scale_x_continuous(labels = scales::comma) + 
        labs(
          title = NULL,
          x = "Genomic Position",
          y = "Allelic Fold Change" 
        ) +
        theme_classic() +
        theme(
          panel.border = element_blank(),
          legend.position = "none",
          axis.line.x = element_line(color = "black")
        )
      plot_legend <- p + 
        theme(legend.position = "right") + 
        guides(fill = guide_legend(ncol = 2))
      
      plot_legend <- cowplot::get_legend(plot_legend)
      pdf(file, width = 14, height = 2)
      print(p)
      grid::grid.newpage()
      grid::grid.draw(plot_legend)
      dev.off()
    }
  )
  ####---------------------------------------------------------------------####
  
  ####--------------------------- feature analysis ---------------------------####
  # get annotation data either from user or pre-stored files
  annoDataFile <- reactive({
    req(isAnnoDataSelected())
    if (!is.null(input$annoInfo)) {
      anno_data <- data.table::fread(input$annoInfo$datapath)
      validate(
        need(ncol(anno_data) == 6, 
             paste("Invalid annotation file: Expected 6 columns, but found", ncol(anno_data), 
                   ". Please check the file format."))
      )
      colnames(anno_data) <- c("chromosome","start","end","name","score","strand")
      return(anno_data)
    }
    if (isTRUE(input$annoStored1)) {
      github_anno_url <- "https://raw.githubusercontent.com/TimHasenbein/Haplofun/main/01_data/annotation_ucsc_hg38_chr1_8.csv"
      tryCatch({
        anno_data <- data.table::fread(github_anno_url)
        colnames(anno_data) <- c("chromosome","start","end","name","score","strand")
        anno_data$thickStart <- anno_data$start
        anno_data$thickEnd <- anno_data$end
        anno_data <- anno_data%>%dplyr::select("chromosome","start","end","name","score","strand","thickStart","thickEnd")
        anno_data <- anno_data %>%
          dplyr::rename(
            chrom = `chromosome`
          )
        return(anno_data)
      }, error = function(e) {
        print(paste("Error loading GitHub file:", e$message))
        return(data.frame(Message = "Error loading stored annotation file from GitHub."))
      })
    }
    if (isTRUE(input$annoStored2)) {
      github_anno_url <- "https://raw.githubusercontent.com/TimHasenbein/Haplofun/main/01_data/annotation_ucsc_hg38_chr9_23.csv"
      tryCatch({
        anno_data <- data.table::fread(github_anno_url)
        anno_data$thickStart <- anno_data$start
        anno_data$thickEnd <- anno_data$end
        anno_data <- anno_data%>%dplyr::select("chromosome","start","end","name","score","strand","thickStart","thickEnd")
        anno_data <- anno_data %>%
          dplyr::rename(
            chrom = `chromosome`
          )
        return(anno_data)
      }, error = function(e) {
        print(paste("Error loading GitHub file:", e$message))
        return(data.frame(Message = "Error loading stored annotation file from GitHub."))
      })
    }
    return(NULL)
  })
  
  # update the annotation element choices
  observeEvent(
    {
      input$mainTabPanel
      getAnnoDataTable()
    },
    {
      req(getAnnoDataTable())
      filteredAnnoData <- getAnnoDataTable()
      unique_types <- unique(filteredAnnoData$Type)
      all_choices <- c("All", unique_types)
      updateSelectInput(
        session = session,
        inputId = "annoFilter",
        label = "Select feature element:",
        choices = all_choices,
        selected = "All"
      )
    }
  )
  
  # check annotation data availability
  output$annoFileUploaded <- reactive({
    !is.null(input$annoInfo) || isTRUE(input$annoStored1) || isTRUE(input$annoStored2)
  })
  outputOptions(output, "annoFileUploaded", suspendWhenHidden = FALSE)
  isAnnoDataSelected <- reactive({
    !is.null(input$annoInfo) || isTRUE(input$annoStored1) || isTRUE(input$annoStored2)
  })
  
  # annotation haplotype overlap: get a annotation data table with the variants that overlap the VCF variants
  getAnnoDataTable <- reactive({
    req(isAnnoDataSelected()) 
    req(filteredVcfData()) 
    anno <- annoDataFile()
    filteredVcfData <- filteredVcfData()
    vcf_fix_df <- filteredVcfData@fix %>% as.data.frame()
    vcf_fix_df$ID <-get_variant_names(filteredVcfData())
    if (any(grepl("^chr", head(vcf_fix_df[[1]])))) {
      anno <- anno
    } else {
      anno[[1]] <- gsub("^chr", "", anno[[1]], ignore.case = TRUE)
    } 
    anno_gr <- GRanges(
      seqnames = anno$chrom,
      ranges = IRanges(start = anno$start, end = anno$end),
      name = anno$name,
    )
    vcf_gr <- GRanges(
      seqnames = vcf_fix_df$CHROM,
      ranges = IRanges(start = as.numeric(vcf_fix_df$POS), end = as.numeric(vcf_fix_df$POS)),
      id = vcf_fix_df$ID,
      ref = vcf_fix_df$REF,
      alt = vcf_fix_df$ALT
    )
    overlaps <- findOverlaps(
      query = vcf_gr,
      subject = anno_gr,
      type = "within" 
    )
    overlapping_variant_indices <- queryHits(overlaps)
    vcf_variants_in_ccres <- vcf_gr[unique(overlapping_variant_indices)]
    overlap_df <- data.frame(
      CHROM = as.character(seqnames(vcf_gr[queryHits(overlaps)])),
      POS = start(vcf_gr[queryHits(overlaps)]),
      ID = vcf_gr[queryHits(overlaps)]$id,
      Type = anno_gr[subjectHits(overlaps)]$name,
      Start = start(anno_gr[subjectHits(overlaps)]),
      End = end(anno_gr[subjectHits(overlaps)])
    )
    overlap_df <- overlap_df %>% arrange(Start)
    return(overlap_df)
  })
  
  # filter the haplotype overlap with selected elements
  getFilteredAnnoDataTable <- eventReactive(input$applyAnnoFilter, {
    req(getAnnoDataTable())
    data <- getAnnoDataTable()
    element_type <- input$annoFilter 
    req(element_type) 
    if (element_type == "All") {
      filtered_data <- data 
    } else {
      filtered_data <- data %>% filter(Type == element_type)
    }
    return(filtered_data)
  })
  
  # get plotting data for the annotation heatmap
  calculateAnnotationHeatmapData <- eventReactive(input$applyAnnoFilter, {
    req(isAnnoDataSelected())
    req(calculateHaplotypeTable())
    req(getFilteredAnnoDataTable())
    annoData <- getFilteredAnnoDataTable()
    annoData <- annoData %>%
      dplyr::select(
        rsID = 3,
        Type = 4,
        Pos = 2,
      )
    haplMatrix <- calculateHaplotypeTable()
    anno_risk <- annoData[annoData$rsID %in% colnames(haplMatrix), ]
    selected_type <- input$annoFilter
    if (selected_type == "All") {
      anno_risk_type <- anno_risk
    } else {
      anno_risk_type <- anno_risk[anno_risk$Type == selected_type, ]
    }
    anno_risk_type <- anno_risk_type %>%
      group_by(rsID, Pos) %>%
      summarise(
        Type = paste(Type, collapse = "; "),
        .groups = 'drop' 
      )
    anno_risk_type <- anno_risk_type %>%
      group_by(rsID, Pos, Type) %>%
      slice_head(n = 1) %>%
      ungroup()
    if (nrow(anno_risk_type) == 0) {
      return(NULL)
    }
    df <- as.data.frame(haplMatrix) %>%
      dplyr::select(-freq) %>%
      tibble::rownames_to_column("haplotypes")
    snp_cols <- names(df)[names(df) != "haplotypes"]
    df_binary <- df %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(snp_cols),
        names_to = "SNP",
        values_to = "allele"
      )
    df_binary <- df_binary %>%
      left_join(
        anno_risk_type,
        by = c("SNP" = "rsID")
      )
    haplo_rows <- setdiff(rownames(haplMatrix), "freq")
    anno_snps  <- intersect(unique(anno_risk_type$rsID), colnames(haplMatrix))
    if (isTRUE(input$checkboxAnnoCluster) && length(anno_snps) > 0 && length(haplo_rows) > 1) {
      allele_mat <- haplMatrix[haplo_rows, anno_snps, drop = FALSE]
      allele_mat[is.na(allele_mat)] <- "N"
      dist_matrix <- as.dist(Reduce("+", lapply(seq_len(ncol(allele_mat)), function(j)
        outer(allele_mat[, j], allele_mat[, j], "!=") * 1L)))
      hc <- hclust(dist_matrix, method = "ward.D2")
      clustered_haplo_order <- haplo_rows[hc$order]
    } else {
      clustered_haplo_order <- haplo_rows
    }
    df_final <- df_binary %>%
      mutate(
        Type = factor(Type),
        haplotypes = factor(haplotypes, levels = clustered_haplo_order)
      )
    return(df_final)
  })
  
  # element color
  AnnoElementColor <- eventReactive(input$applyAnnoFilter, {
    req(calculateAnnotationHeatmapData())
    elements <- as.data.table(calculateAnnotationHeatmapData())
    if (is.null(elements) || nrow(elements) == 0) {
      return(NULL) 
    }
    elements <- elements%>%dplyr::select(SNP,Type, Pos)
    elements <- elements %>% 
      mutate(pos = as.numeric(Pos)) %>%
      arrange(pos)
    elements <- unique(elements$Type)
    elements <- elements[!is.na(elements)]
    sci_palette <- c(
      "#5F9EA0", "#E6B8A2", "#2F4F4F", "#B7E4C7", "#9575CD", "#FFF1A8", "#4682B4", "#A3C1AD",
      "#336666", "#918e8e", "#008B8B", "#FFAB91", "#7A3734", "#EDC948", "#1A5276", "#80CBC4",
      "#ffd6b8", "#90CAF9", "#1D8348", "#BCAAA4", "#00ACC1", "#C4A484", "#7E5109", "#B39DDB",
      "#8A9A5B", "#D7CCC8", "#4DB6AC", "#EF9A9A", "#2E86C1", "#FFCC80", "#DCE775", "#B0BEC5",
      "#708090", "#64B5F6", "#CD5C5C", "#A5D6A7", "#34495E", "#FFF9C4", "#4FC3F7", "#5D4037",
      "#A9DFBF", "#B2DFDB", "#6A5ACD", "#FFECB3", "#7986CB", "#556B2F", "#8EBCBB", "#F9E79F",
      "#E0F7FA", "#D98880"
    )
    n_elements <- length(elements)
    elements_colors <- setNames(rep_len(sci_palette, n_elements), elements)
    color_list <- as.list(elements_colors)
    final_colors_list <- list()
    for (i in seq_along(color_list)) {
      split_names <- unlist(strsplit(names(color_list)[i], ";\\s*"))
      current_color <- color_list[[i]]
      for (single_name in split_names) {
        final_colors_list[[single_name]] <- current_color
      }
    }
    combined_colors <- c(unlist(final_colors_list),elements_colors)
    combined_colors <- combined_colors[!duplicated(names(combined_colors))]
    return(combined_colors)
  })
  
  # plot the annotation heatmap
  output$plotAnnoHeatmap <- plotly::renderPlotly({
    input$applyAnnoFilter
    req(calculateAnnotationHeatmapData())
    filteredVcfData <- req(filteredVcfData())

    vcf_fix_df <- filteredVcfData@fix %>% as.data.frame()
    vcf_fix_df$ID <- get_variant_names(filteredVcfData)
    snp_order <- unique(dplyr::arrange(vcf_fix_df, POS)$ID)

    plot_data <- as.data.table(calculateAnnotationHeatmapData())
    if (is.null(plot_data) || nrow(plot_data) == 0) return(NULL)

    plot_data[, allele := fifelse(!is.na(Type), as.character(allele), NA_character_)]
    plot_data[, SNP := factor(SNP, levels = snp_order)]

    # fixed nucleotide palette: 0=Absent, 1=A, 2=C, 3=G, 4=T
    allele_levels <- c("A", "C", "G", "T")
    all_colors_v  <- c("#F2F2F2", "#95bc0e", "#006aa3", "#fabb00", "#e42032")

    plot_data[, allele_idx := fifelse(is.na(allele), 0L, match(allele, allele_levels))]
    plot_data[, hover_text := paste0(
      fifelse(is.na(allele), "Absent", as.character(allele)),
      " | ",
      fifelse(is.na(Type), "", as.character(Type))
    )]

    wide_z    <- dcast(plot_data, haplotypes ~ SNP, value.var = "allele_idx",  fill = 0L)
    wide_text <- dcast(plot_data, haplotypes ~ SNP, value.var = "hover_text",  fill = "Absent")

    z_mat    <- as.matrix(wide_z[,    -1])
    text_mat <- as.matrix(wide_text[, -1])
    haplos   <- as.character(wide_z$haplotypes)
    snps     <- colnames(z_mat)

    # discrete colorscale with sharp boundaries
    n        <- length(all_colors_v)
    zmax_val <- max(n - 1L, 1L)
    cs <- vector("list", 0)
    for (i in seq_len(n)) {
      lo <- (i - 1) / zmax_val
      cs <- c(cs, list(list(lo, all_colors_v[i])))
      if (i < n) cs <- c(cs, list(list(i / zmax_val - 1e-9, all_colors_v[i])))
    }
    cs <- c(cs, list(list(1.0, all_colors_v[n])))

    plotly::plot_ly(
      z             = z_mat,
      x             = snps,
      y             = haplos,
      text          = text_mat,
      type          = "heatmap",
      colorscale    = cs,
      zmin          = 0L,
      zmax          = zmax_val,
      showscale     = FALSE,
      xgap          = 1,
      ygap          = 1,
      hovertemplate = "SNP: %{x}<br>Haplotype: %{y}<br>%{text}<extra></extra>"
    ) |>
      plotly::layout(
        plot_bgcolor = "white",
        xaxis = list(showticklabels = FALSE, ticks = "", title = ""),
        yaxis = list(
          title    = "",
          ticks    = "",
          tickmode = "array",
          tickvals = haplos,
          ticktext = haplos,
          tickfont = list(size = 7, color = "gray30")
        )
      )
  })
  
  # download the annotation heatmap
  output$buttonplotAnnoHeatmap <- downloadHandler(
    filename = function() {paste(input$textExportFileName, "_annotatio_heatmap.pdf", sep = "")},
    content = function(file) {
      filteredVcfData <- req(filteredVcfData())
      vcf_fix_df <- filteredVcfData@fix %>% as.data.frame()
      vcf_fix_df$ID <- get_variant_names(filteredVcfData) 
      vcf_fix_df <- vcf_fix_df %>% dplyr::select(POS,ID)
      vcf_fix_df <- vcf_fix_df %>% arrange(POS)
      plot_data <- req(calculateAnnotationHeatmapData())
      plot_data <- plot_data %>% mutate(allele = ifelse(!is.na(Type), as.character(allele),NA))
      plot_data$SNP <- factor(plot_data$SNP, levels = unique(vcf_fix_df$ID))
      p <- ggplot(plot_data, aes(x = SNP, y = haplotypes, fill = allele, text = Type)) +
        geom_tile(color = "black", linewidth = 0.1) +
        scale_fill_manual(
          values = c("A" = "#95bc0e", "C" = "#006aa3", "G" = "#fabb00", "T" = "#e42032"),
          na.value = "#F2F2F2", 
          name = "Element"
        ) +
        labs(x = "SNP", y = "Haplotype") +
        theme_classic(base_size = 14) +
        theme(
          panel.border = element_rect(color = "white", fill = NA, linewidth = 1),
          panel.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(
            face = "bold",
            size = 18,
            hjust = 0.5,
            margin = margin(b = 10)
          ),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(
            face = "bold",
            color = "gray30",
            angle = 45,
            hjust = 1,
            vjust = 1
          ),
          axis.text.y = element_text(
            face = "bold",
            color = "gray30"
          ),
          axis.title = element_text(size = 14, face = "bold"),
          legend.title = element_blank()
        ) +
        labs(
          x = NULL,
          y = NULL,
          title = NULL
        ) + 
        guides(fill="none")
      plot_legend <- p + 
        theme(legend.position = "right") + 
        guides(fill = guide_legend(ncol = 2, title = "Allele"))
      plot_legend <- cowplot::get_legend(plot_legend)
      pdf(file, width = 14, height = 10)
      print(p)
      grid::grid.newpage()
      grid::grid.draw(plot_legend)
      dev.off()
    }
  )
  
  # output the annotation haplotype table: a table with the haplotypes and information of SNPs within the vcf
  output$annoHaplotypeTable <- DT::renderDataTable({
    input$applyAnnoFilter
    req(getFilteredAnnoDataTable())
    req(calculateAnnotationHeatmapData())
    filtered_annoData <- getFilteredAnnoDataTable()
    filtered_annoData <- filtered_annoData$ID
    if (length(filtered_annoData) == 0) {
      return(DT::datatable(data.frame(Message = "No variants in haplotypes overlap."), options = list(dom = 't')))
    }
    haplotypeTable <- calculateHaplotypeTable()%>%as.data.frame()
    displayTable <- haplotypeTable %>% dplyr::select(-freq)
    displayTable <- t(displayTable)
    displayTable <- displayTable[rownames(displayTable) %in% filtered_annoData,, drop = FALSE]
    dt <- DT::datatable(
      displayTable,
      options = list(scrollY = "300px",scrollX = TRUE, pageLength = 10)
    )
    dt <- dt %>% DT::formatStyle(
      columns = 1,
      target = 'row',
      backgroundColor = '#F2D394'
    )
    dt
  })
  
  # download the annotation haplotype table
  output$buttonannoHaplotypeTable <- downloadHandler(
    filename = function() {paste0(input$textExportFileName, "_annotation_Allele_table.txt")},
    content = function(file) {
      req(getFilteredAnnoDataTable())
      filtered_annoData <- getFilteredAnnoDataTable()
      filtered_annoData <- filtered_annoData$ID
      haplotypeTable <- calculateHaplotypeTable()%>%as.data.frame()
      displayTable <- haplotypeTable %>% dplyr::select(-freq)
      displayTable <- t(displayTable)
      displayTable <- displayTable[rownames(displayTable) %in% filtered_annoData,, drop = FALSE]
      write.table(displayTable, file, quote = FALSE, sep = "\t", row.names = T)
    }
  )
  
  # annotation table filtered for haplotype
  output$annoSnpTable <- DT::renderDataTable({
    input$applyAnnoFilter
    table <- req(getFilteredAnnoDataTable()) 
    colnames(table) <- c("Chromosome","Position","Name","Type","Start","End")
    DT::datatable(
      table,
      options = list(scrollY = "300px",scrollX = TRUE, pageLength = 10),
      caption = "Haplotype variants that overlap annotation"
    )
  })
  
  # download the annotation table haplotypes
  output$buttonannoSnpTable <- downloadHandler(
    filename = function() {paste0(input$textExportFileName, "_annotation_SNP_table.txt")},
    content = function(file) {
      data_to_export <- getFilteredAnnoDataTable()
      write.table(data_to_export, file, quote = FALSE, sep = "\t", row.names = F)
    }
  )
  
  # annotation bar plot
  output$annoPlot <- plotly::renderPlotly({
    input$applyAnnoFilter
    plot_data <- getFilteredAnnoDataTable()
    if (nrow(plot_data) == 0) {
      paste("No data to plot")
    }
    element_colors <- AnnoElementColor()
    overlap_summary <- plot_data %>%
      dplyr::select(ID, Type) %>%
      dplyr::distinct() %>%
      dplyr::group_by(Type) %>%
      dplyr::summarise(
        Variant_Count = n_distinct(ID)
      ) %>%
      dplyr::arrange(desc(Variant_Count))
    return(
      plotly::ggplotly(ggplot(overlap_summary, aes(
        x = reorder(Type, -Variant_Count), 
        y = Variant_Count,
        fill = Type 
      )) + 
        scale_fill_manual(
          values = element_colors,
          na.value = "#F2F2F2", 
          name = "Element"
        ) +
        geom_bar(stat = "identity", color = "black") + 
        theme_classic() +
        theme(
          panel.border = element_blank(),
          legend.position = "none",
          axis.line.x = element_line(color = "black")
        ) + 
        xlab(label = NULL) +
        ylab(label = "Variant count"),
      tooltip = c("Variant_count","Type")
      )
    )
  })
  
  # download annotation bar plot
  output$buttonannoPlot <- downloadHandler(
    filename = function() {paste(input$textExportFileName, "_annotation_freq_plot.pdf", sep = "")},
    content = function(file) {
      element_colors <- AnnoElementColor()
      plot_data <- getFilteredAnnoDataTable()
      overlap_summary <- plot_data %>%
        dplyr::select(ID, Type) %>%
        dplyr::distinct() %>%
        dplyr::group_by(Type) %>%
        dplyr::summarise(
          Variant_Count = n_distinct(ID)
        ) %>%
        dplyr::arrange(desc(Variant_Count))
      p <- ggplot(overlap_summary, aes(
        x = reorder(Type, -Variant_Count), 
        y = Variant_Count,
        fill = Type 
      )) + 
        scale_fill_manual(
          values = element_colors,
          na.value = "#F2F2F2", 
          name = "Element"
        ) +
        geom_bar(stat = "identity", color = "black") + 
        theme_classic() +
        theme(
          panel.border = element_blank(),
          legend.position = "none",
          axis.line.x = element_line(color = "black")
        ) + 
        xlab(label = NULL) +
        ylab(label = "Variant count")
      ggsave(file, plot = p, width = 14, height = 10)
    }
  )
  
  ####---------------------------------------------------------------------####
  
  ####--------------------------- downloads ---------------------------####
  # download samples per haplogroup
  output$buttonExportSampleHaplo <- downloadHandler(
    filename = function() {paste0(input$textExportFileName, "_samplesHaplotypes.txt")},
    content = function(file) {
      data_to_export <- getSampleswithHaplo()
      write.table(data_to_export, file, quote = FALSE, sep = "\t", row.names = F)
    }
  )
  
  # download haplogroup network
  output$buttonExportHaplonetPDF <- downloadHandler(
    filename = function() {paste(input$textExportFileName, "_haplonet.pdf", sep = "")},
    content = function(file) {
      pdf(file = file, width = 14, height = 10)
      req(input$selectNetwork != 1)
      haplonetObj <- haplonet()
      nodeLabels <- labels(haplonetObj)
      scaleRatio <- 5 / input$sliderScaleNetwork
      sizeNodes <- rep(1, length(nodeLabels))
      if (is.null(haplonetObj)) {
        num_haplotypes <- length(labels(getHaplotypes()))
        msg <- paste0("High computational load detected. Please filter your data further.")
        stop(msg) 
      }
      # scale node size
      if (input$checkboxScaleNetwork) {
        req(getHaplotypes())
        sz <- getAllHaplotypeSummary()
        labelsHap <- attr(haplonetObj, 'labels')
        sizeNodes <- sz[labelsHap]
      }
      # edge threshold
      threshold <- if (input$sliderThreshold == 0) 0 else c(1, input$sliderThreshold)
      # SNP highlight
      if (input$snpPosition != "None") {
        req(getColorByAllele())
        baseFillColors <- getColorByAllele()
      } else {
        baseFillColors <- rep(input$colorCircles, length(nodeLabels))
      }
      # node colors
      nodeColors <- rep("black", length(nodeLabels))
      # highlight haplotypes
      if (!is.null(input$highlightHaplotypes)) {
        sel <- match(input$highlightHaplotypes, nodeLabels)
        sel <- sel[!is.na(sel)]
        if (length(sel) > 0) {
          nodeColors[sel] <- "black"
          baseFillColors[sel] <- "#CCDDFF"
        }
      }
      # plot meta
      if (subInputSelected() && input$checkboxMeta) {
        pieInfo <- getSubInformationAsMatrix() 
        num <- ncol(getSubInformationAsMatrix())
        plot(
          haplonetObj,
          pie = pieInfo,
          bg = category_colors(),
          shape = if (input$selectNetwork %in% c(4, 5)) c("circles", "circles") else "circles",
          cex = input$sliderLabels,
          fast = input$checkboxFastPlotHaplonet,
          scale.ratio = scaleRatio,
          labels = TRUE,
          show.mutation = input$radioButtonEdges,
          threshold = threshold,
          size = sizeNodes
        )
        legend(
          "topright",                 
          legend = colnames(pieInfo), 
          fill = category_colors(),
          bty = "n",                 
          cex = 0.8                   
        )
      } else if (isGwasDataSelected() && length(input$traitFilter) > 0 && input$checkboxGwasNetwork && input$applyTraitFilter > 0) {
        # highlight GWAS traits
        input$applyTraitFilter
        selected_trait <- isolate(input$traitFilter)
        trait_colors <- isolate(GWAStraitColor())
        # get status info
        status_info <- isolate(as.data.table(calculateGwasHaplotypeHeatmapData()))
        status_info <- status_info %>% tidyr::separate_rows(SNP_trait, sep = "; ")
        status_info <- status_info %>% distinct(rsID, RiskAllele,SNP_trait,trait_status,haplotypes)
        status_info <- status_info %>% filter(sub("_[^_]+$", "", SNP_trait) %in% selected_trait)
        unique_traits <- unique(status_info$SNP_trait)
        pie_matrix <- matrix(0, 
                             nrow = length(nodeLabels), 
                             ncol = length(unique_traits),
                             dimnames = list(nodeLabels, unique_traits))
        i = 1
        for (i in 1:nrow(status_info)) {
          h_name <- as.character(status_info$haplotypes[i])
          t_name <- as.character(status_info$SNP_trait[i])
          if (h_name %in% nodeLabels) {
            pie_matrix[h_name, t_name] <- 1
          }
        }
        no_association <- ifelse(rowSums(pie_matrix) == 0, 1, 0)
        pie_matrix <- cbind(pie_matrix, "None" = no_association)
        active_colors <- unname(GWAStraitColor()[colnames(pie_matrix)])
        active_colors <- active_colors[!is.na(active_colors)]
        plot_colors <- c(active_colors, "white")
        network_order <- labels(haplonetObj)
        haplo_counts <- table(status_info$haplotypes)
        gwas_sizes <- as.numeric(haplo_counts[network_order]) + 1
        plot(
          haplonetObj,
          pie = pie_matrix,
          bg = plot_colors,
          shape = if (input$selectNetwork %in% c(4, 5)) c("circles", "circles") else "circles",
          cex = input$sliderLabels,
          fast = input$checkboxFastPlotHaplonet,
          scale.ratio = scaleRatio,
          labels = TRUE,
          show.mutation = input$radioButtonEdges,
          threshold = threshold,
          size = if (input$checkboxScaleNetwork) sizeNodes else gwas_sizes
        )
        legend(
          "topright",
          legend = colnames(pie_matrix),
          fill = plot_colors,
          bty = "n",
          cex = 0.8
        )
      } else if (iseqtlDataSelected() && length(input$qtlFilter) > 0 && input$checkboxQtlNetwork && input$applyQTLFilter > 0) {
        # highlight QTLs
        input$applyQTLFilter
        selected_qtl <- isolate(input$qtlFilter)
        trait_colors <- isolate(QTLtargetColor())
        # validation of input slider numbers
        current_slider_val <- input$sliderHaplotypes
        raw_qtl_data <- isolate(calculateeQTLHaplotypeHeatmapData())
        validate(
          need(nrow(as.data.table(raw_qtl_data)) > 0, "Please click 'Update QTL annotation', to load plotting data."),
          need(length(labels(haplonetObj)) <= length(unique(as.data.table(raw_qtl_data)$haplotypes)), 
               "Increased the number of haplogroups. Please click 'Update QTL annotation', to update the QTL coloring of the network.")
        )
        # get status info
        status_info <- isolate(as.data.table(calculateeQTLHaplotypeHeatmapData()))
        status_info <- status_info %>% tidyr::separate_rows(Target, sep = "; ")
        status_info <- status_info %>% distinct(rsID, RiskAllele,Target,haplotypes)
        status_info <- status_info %>% filter(sub("_[^_]+$", "", Target) %in% selected_qtl)
        unique_targets <- unique(status_info$Target)
        pie_matrix <- matrix(0, 
                             nrow = length(nodeLabels), 
                             ncol = length(unique_targets),
                             dimnames = list(nodeLabels, unique_targets))
        for (i in 1:nrow(status_info)) {
          h_name <- as.character(status_info$haplotypes[i])
          t_name <- as.character(status_info$Target[i])
          if (h_name %in% nodeLabels) {
            pie_matrix[h_name, t_name] <- 1
          }
        }
        no_association <- ifelse(rowSums(pie_matrix) == 0, 1, 0)
        pie_matrix <- cbind(pie_matrix, "None" = no_association)
        active_colors <- unname(QTLtargetColor()[colnames(pie_matrix)])
        active_colors <- active_colors[!is.na(active_colors)]
        plot_colors <- c(active_colors, "white")
        network_order <- labels(haplonetObj)
        haplo_counts <- table(status_info$haplotypes)
        qtl_sizes <- as.numeric(haplo_counts[network_order]) + 1
        plot(
          haplonetObj,
          pie = pie_matrix,
          bg = plot_colors,
          shape = if (input$selectNetwork %in% c(4, 5)) c("circles", "circles") else "circles",
          cex = input$sliderLabels,
          fast = input$checkboxFastPlotHaplonet,
          scale.ratio = scaleRatio,
          labels = TRUE,
          show.mutation = input$radioButtonEdges,
          threshold = threshold,
          size = if (input$checkboxScaleNetwork) sizeNodes else qtl_sizes
        )
        legend(
          "topright",
          legend = colnames(pie_matrix),
          fill = plot_colors,
          bty = "n",
          cex = 0.8
        )
      } else {
        # plot without pie coloring
        plot(
          haplonetObj,
          pie = NULL,
          col = nodeColors,
          bg = baseFillColors,
          shape = if (input$selectNetwork %in% c(4, 5)) c("circles", "circles") else "circles",
          cex = input$sliderLabels,
          fast = input$checkboxFastPlotHaplonet,
          scale.ratio = scaleRatio,
          labels = TRUE,
          show.mutation = input$radioButtonEdges,
          threshold = threshold,
          size = sizeNodes
        )
        
        if (input$snpPosition != "None") {
          allele_colors <- c("A" = "#95bc0e", "C" = "#006aa3", "G" = "#fabb00", "T" = "#e42032")
          hapTab <- calculateHaplotypeTable()
          present_alleles <- sort(unique(hapTab[, input$snpPosition, drop = TRUE]))
          legend(
            "topright", 
            legend = present_alleles,
            fill = allele_colors[present_alleles],
            title = paste(input$snpPosition),
            bty = "n",
            cex = 0.8
          )
        }
      }
      dev.off()
    }
  )
  ####---------------------------------------------------------------------####
  
  ####--------------------------- vcf summary ---------------------------####
  # render vcf meta data 
  output$plotMetadata <- DT::renderDataTable(DT::datatable(
    calculateVcfMetadata(),
    options = list(scrollY = "350px", scrollX = TRUE),
    caption = "VCF Headerdata"
  ))
  
  # render vcf data 
  output$plotVcfData <- DT::renderDataTable(DT::datatable({
    vcf_data <- req(filteredVcfData())
    validate(
      need(
        !is.null(vcf_data) && nrow(vcf_data) > 0, 
        "Please upload or filter the data to display VCF Datalines."
      )
    )
    data_to_display <- extract.haps(vcf_data)
    t(data_to_display)
  },
  options = list(scrollY = "350px",scrollX = TRUE),
  caption = "VCF Datalines"
  ))
  
  # data summary vcf
  # no. initial variants
  output$numInitVariants <- renderText({
    req(vcfRInput())
    vcf <- vcfRInput()
    total_count <- nrow(vcf@fix)
    return(paste0(total_count))
  })
  # no. variants
  output$numVariants <- renderText({
    req(filteredVcfData())
    req(vcfRInput())
    vcf <- vcfRInput()
    total_count <- nrow(vcf@fix)
    n_variants <- nrow(filteredVcfData())
    return(paste0(n_variants, " of ", total_count, " input variants"))
  })
  # no. initial haplotype groups
  output$numInitHG <- renderText({
    req(getAllHaplotypes())
    req(getAllinitHaplotypes())
    total_hg <- getAllinitHaplotypes()
    return(paste0(total_hg))
  })
  # no. haplotype groups
  output$numHG <- renderText({
    req(getAllHaplotypes())
    req(getAllinitHaplotypes())
    total_hg <- getAllinitHaplotypes()
    n_hg <- length(base::summary(getAllHaplotypes()))
    return(paste0(n_hg, " of ", total_hg, " initial haplogroups"))
  })
  # no. total entries
  totalEntries <- reactive({
    req(isvcfDataSelected())
    use_filtered <- isDataProcessed()
    if (use_filtered) {
      vcf_obj <- filteredVcfData()
    } else {
      vcf_obj <- vcfRInput()
    }
    num_samps <- ncol(vcf_obj@gt) - 1 
    num_vars <- nrow(vcf_obj)
    return(num_vars * num_samps)
  })
  ####---------------------------------------------------------------------####
  
  ####--------------------------- IGV ---------------------------####
  # IGV
  available_genomes <- setdiff(igvShiny::get_basic_genomes(), "custom")
  observeEvent(available_genomes, {
    updateSelectInput(
      session = session,
      inputId = "igvGenome",
      choices = available_genomes,
      selected = "hg38"
    )
  }, once = TRUE)
  igv_options_reactive <- reactive({
    req(input$igvGenome)
    igvShiny::parseAndValidateGenomeSpec(genomeName = input$igvGenome)
  })
  
  output$igv_viewer <- renderIgvShiny({
    igv_options <- igv_options_reactive()
    igvShiny(igv_options)
  })
  
  output$igvUI_wrapper <- renderUI({
    igvShinyOutput("igv_viewer", height = "75vh")
  })
  
  vcfObjectReactive <- reactive({
    req(isvcfDataSelected())
    req(vcfRInput())
    req(input$igvGenome)
    selected_genome_build <- input$igvGenome
    file_path <- if (!is.null(input$file)) {
      input$file$datapath
    } else if (isTRUE(input$vcfStored_1)) {
      "https://raw.githubusercontent.com/TimHasenbein/Haplofun/main/01_data/01_IL23R_100kb_hg38_multi_rm.vcf.gz"
    } else {
      "https://raw.githubusercontent.com/TimHasenbein/Haplofun/main/01_data/linreg_nonsynonymous_ftsI_multi_rm.vcf.gz"
    }
    vcf_object <- tryCatch(
      VariantAnnotation::readVcf(file = file_path, genome = selected_genome_build),
      error = function(e) {
        showNotification(paste("IGV viewer: could not load VCF —", conditionMessage(e)),
                         type = "error", duration = 10)
        NULL
      }
    )
    req(vcf_object)
    vcf_object
  })
  
  observeEvent(list(vcfObjectReactive(),
                    igv_options_reactive()), {
                      vcf_object <- vcfObjectReactive()
                      req(vcf_object)
                      req(isvcfDataSelected()) 
                      igvShiny::loadVcfTrack(
                        id = "igv_viewer",
                        trackName = "VCF file",
                        vcfData = vcf_object, 
                        session = session
                      )
                      first_variant_location <- as.character(GenomicRanges::seqnames(vcf_object)[1])
                      igvShiny::showGenomicRegion(
                        id = "igv_viewer", 
                        region = first_variant_location, 
                        session = session
                      )
                    })
  
  observeEvent(list(GwasDataFile(), 
                    igv_options_reactive()), {
                      req(GwasDataFile())
                      igv_track_data <- GwasDataFile()
                      colnames(igv_track_data) <- c("chromosome","start","end","name","score","strand","trait","risk_allele","pval","effect_size")
                      igvShiny::loadBedTrack(
                        id = "igv_viewer",
                        tbl = igv_track_data,
                        trackName = "GWAS variants",
                        color = "red", 
                        session = session
                      )
                    })
  
  observeEvent(list(eqtlDataFile(), 
                    igv_options_reactive()), {
                      req(eqtlDataFile())
                      igv_track_data <- eqtlDataFile()
                      igvShiny::loadBedTrack(
                        id = "igv_viewer",
                        tbl = igv_track_data,
                        trackName = "eQTL variants",
                        color = "green", 
                        session = session
                      )
                    })
  
  observeEvent(list(annoDataFile(), 
                    igv_options_reactive()), {
                      req(annoDataFile())
                      igv_track_data <- annoDataFile()
                      igvShiny::loadBedTrack(
                        id = "igv_viewer",
                        tbl = igv_track_data,
                        trackName = "annotation",
                        color = "orange", 
                        session = session
                      )
                    })
  ####---------------------------------------------------------------------####
  
  ####--------------------------- pre-stored files configuration ---------------------------####
  # single checkbox clicking for pre-stored files  
  observeEvent(input$file, {
    if (!is.null(input$file)) {
      updateCheckboxInput(session = session, inputId = "vcfStored_1", value = FALSE)
      updateCheckboxInput(session = session, inputId = "vcfStored_3", value = FALSE)
      disable("vcfStored_1");disable("vcfStored_3")
    }
  })
  
  observeEvent(input$vcfStored_1, {
    if (isTRUE(input$vcfStored_1)) {
      updateCheckboxInput(session, "vcfStored_3", value = FALSE)
    }
  })
  
  observeEvent(input$vcfStored_3, {
    if (isTRUE(input$vcfStored_3)) {
      updateCheckboxInput(session, "vcfStored_1", value = FALSE)
    }
  })
  
  # enable disable pre-stored checkboxes
  observe({
    enable("metahinf"); enable("meta1000G")
    enable("hinfGwasStored"); enable("gwasStored"); enable("eqtlStored")
    enable("annoStored1"); enable("annoStored2")
    if (isTRUE(input$vcfStored_1)) {
      disable("metahinf"); disable("hinfGwasStored")
      updateCheckboxInput(session = session, inputId = "hinfGwasStored", value = FALSE)
      updateCheckboxInput(session = session, inputId = "metahinf", value = FALSE)
    } else if (isTRUE(input$vcfStored_3)) {
      disable("meta1000G"); disable("gwasStored")
      disable("eqtlStored"); disable("annoStored1"); disable("annoStored2")
      updateCheckboxInput(session = session, inputId = "meta1000G", value = FALSE)
      updateCheckboxInput(session = session, inputId = "gwasStored", value = FALSE)
      updateCheckboxInput(session = session, inputId = "eqtlStored", value = FALSE)
      updateCheckboxInput(session = session, inputId = "annoStored1", value = FALSE)
      updateCheckboxInput(session = session, inputId = "annoStored2", value = FALSE)
    }
  })
  ####---------------------------------------------------------------------####
  
  ####--------------------------- misc ---------------------------####
  # vcf main check
  output$inputSelected <- renderText({
    if (isTRUE(isvcfDataSelected())) "true" else "false"
  })
  outputOptions(output, 'inputSelected', suspendWhenHidden = FALSE)
  
  # check meta selection
  subInputSelected <- reactive({
    if (is.null(input$fileSubInfo) & isFALSE(input$meta1000G) & isFALSE(input$metahinf)) {
      return (FALSE)
    } else {
      return (TRUE)
    }
  })
  output$subInputSelectedCol <- reactive({
    if  (is.null(input$fileSubInfo) & isFALSE(input$meta1000G) & isFALSE(input$metahinf)) {
      return (FALSE)
    } else {
      return (TRUE)
    }
  })
  outputOptions(output, 'subInputSelectedCol', suspendWhenHidden = FALSE)
  
  # reset everything with new file upload
  observeEvent(isvcfDataSelected(), {
    vcf_data_store(NULL)
    ld_pruned_vcf_store(NULL)
    filteredVcfData_Prun(NULL)
    sampled_vcf_store(NULL)
    haplonet(NULL)
    updateNumericInput(session, "mafFilterInit", value = 0)
    updateCheckboxInput(session, "enableLdPruning", value = FALSE)
    updateCheckboxInput(session, "enableSnpsampling", value = FALSE)
    updateSliderInput(session, "sliderThreshold", value = 0, min = 0, max = 0)
    updateCheckboxGroupInput(session, "highlightHaplotypes", choices = NULL, selected = NULL)
    updateSelectInput(session, "snpPosition", choices = NULL, selected = NULL)
    req(getAllHaplotypes())
    haplotypes <- getAllHaplotypes()
    rows <- nrow(haplotypes)
    new_max <- min(rows, 50)
    new_value_raw <- min(10, rows)
    new_value <- max(2, new_value_raw)
    updateSliderInput(
      session, 
      "sliderHaplotypes", 
      max = new_max, 
      value = new_value, 
      step = 1,
      min = 2 
    )
    shinyjs::click("applyTraitFilter")
    updateCheckboxInput(session, "checkboxGwasNetwork", value = FALSE)
    shinyjs::click("applyQTLFilter")
    updateCheckboxInput(session, "checkboxQtlNetwork", value = FALSE)
    shinyjs::click("applyAnnoFilter")
    updateSliderInput(
      session, 
      "pip_range", 
      min = 0,
      max = 1,
      value = c(0)
    )
    haplonet(calculateHaplonet())
  })
  
  # reset GWAS with new file upload
  observeEvent(isGwasDataSelected(), {
    shinyjs::click("applyTraitFilter")
    updateCheckboxInput(session, "checkboxGwasNetwork", value = FALSE)
    updateSliderInput(session, "gwas_pval_filter", value = 0)
  })
  
  # checkbox GWAS coloring
  observeEvent(input$checkboxGwasNetwork, {
    if (isTRUE(input$checkboxGwasNetwork)) {
      updateCheckboxInput(session, "checkboxQtlNetwork", value = FALSE)
      updateCheckboxInput(session, "checkboxMeta", value = FALSE)
    }
  })
  
  # reset QTL with new file upload
  observeEvent(iseqtlDataSelected(), {
    shinyjs::click("applyQTLFilter")
    updateCheckboxInput(session, "checkboxQtlNetwork", value = FALSE)
    updateSliderInput(
      session, 
      "pip_range", 
      min = 0,
      max = 1,
      value = c(0)
    )
  })
  
  # checkbox QTL coloring
  observeEvent(input$checkboxQtlNetwork, {
    if (isTRUE(input$checkboxQtlNetwork)) {
      updateCheckboxInput(session, "checkboxGwasNetwork", value = FALSE)
      updateCheckboxInput(session, "checkboxMeta", value = FALSE)
    }
  })
  
  # checkbox meta coloring
  observeEvent(input$checkboxMeta, {
    if (isTRUE(input$checkboxMeta)) {
      updateCheckboxInput(session, "checkboxGwasNetwork", value = FALSE)
      updateCheckboxInput(session, "checkboxQtlNetwork", value = FALSE)
    }
  })
  
  # switch tabs
  observeEvent(input$goToHelpTab, {
    updateTabsetPanel(
      session = session, 
      inputId = "sidebarTabs", 
      selected = "help"
    )
  })
  
  # functional tab switch lock to ensure proper rendering
  observeEvent(input$annotation_tabs, {
    lockUI()
  })
  observeEvent(input$applyTraitFilter, {
    lockUI()
  }, ignoreInit = TRUE)
  observeEvent(input$applyQTLFilter, {
    lockUI()
  }, ignoreInit = TRUE)
  observeEvent(input$annoFilter, {
    lockUI()
  }, ignoreInit = TRUE)
  observeEvent(input$haplotypeAnalysis, {
    lockUI()
  }, ignoreInit = TRUE)
  
  # helper function to lock UI for rendering
  lockUI <- function() {
    shinyjs::disable("annotation_tabs_container")
    shinyjs::runjs("$('#annotation_tabs_container').css('opacity', '0.5').css('pointer-events', 'none');")
    session$onFlushed(function() {
      shinyjs::enable("annotation_tabs_container")
      shinyjs::runjs("$('#annotation_tabs_container').css('opacity', '1').css('pointer-events', 'auto');")
    }, once = TRUE)
  }
  
  # performance warning 
  output$performanceWarning <- renderUI({
    cell_count <- totalEntries()
    is_large <- cell_count > 350000
    if (is_large) {
      formatted_count <- prettyNum(cell_count, big.mark = ".", scientific = FALSE)
      div(
        style = "padding: 10px; margin-bottom: 15px; border: 2px solid #dc3545; background-color: #f8d7da; color: #721c24; border-radius: 5px; font-weight: bold;",
        icon("triangle-exclamation"),
        "WARNING: Dataset has ",
        tags$b(formatted_count),
        " entries. Filtering is recommended to reduce performance time."
      )
    } else {
      return(NULL)
    }
  })
  #  obsolet/else
  # 'dummy plot' to ensure `calculateHaplonetEventReactive()` is triggered.
  output$dummyPlot <- renderPlot({
    haplonet(calculateHaplonet())
    calculateHaplonetEventReactive()
  })
  # 'dummy plot' to trigger `removeEdgesEventReactive()` function
  output$dummyPlot2 <- renderPlot({
    removeEdgesEventReactive()
  })
  
  # run button heatmap  
  observeEvent(input$run, {
    req(isvcfDataSelected())
    # reset all filtered/pruned stores so summaries reflect the new VCF
    vcf_data_store(NULL)
    ld_pruned_vcf_store(NULL)
    filteredVcfData_Prun(NULL)
    sampled_vcf_store(NULL)
    haplonet(NULL)
    # reset filter UI to defaults
    updateNumericInput(session, "mafFilterInit", value = 0)
    updateCheckboxInput(session, "enableLdPruning", value = FALSE)
    updateCheckboxInput(session, "enableSnpsampling", value = FALSE)
    updateSliderInput(session, "sliderThreshold", value = 0, min = 0, max = 0)
    updateCheckboxGroupInput(session, "highlightHaplotypes", choices = NULL, selected = NULL)
    updateSelectInput(session, "snpPosition", choices = NULL, selected = NULL)
    # reset annotation filters
    shinyjs::click("applyTraitFilter")
    updateCheckboxInput(session, "checkboxGwasNetwork", value = FALSE)
    shinyjs::click("applyQTLFilter")
    updateCheckboxInput(session, "checkboxQtlNetwork", value = FALSE)
    shinyjs::click("applyAnnoFilter")
    updateSliderInput(session, "pip_range", min = 0, max = 1, value = c(0))
    # force haplotype computation now so summary counts are immediately correct
    req(getAllHaplotypes())
    haplotypes <- getAllHaplotypes()
    rows <- nrow(haplotypes)
    new_max <- min(rows, 50)
    new_value <- max(2, min(10, rows))
    updateSliderInput(session, "sliderHaplotypes", max = new_max, value = new_value, step = 1, min = 2)
    haplonet(calculateHaplonet())
    # switch to analysis tab
    updateTabsetPanel(session = session, inputId = "sidebarTabs", selected = "haplotypeAnalysis")
  })
  
  # colors 
  category_colors <- reactive({
    req(getSubInformationAsMatrix())
    all_categories <- colnames(getSubInformationAsMatrix())
    cols <- c("#246ca7ff","#fcb9cfff","#7bc3f9ff","#319533ff","#840e0eff","#DF0101", "#A4A4A4", "#222222", "#A945FF", "#77CE61", 
              "#000000", "#FF9326", "#FDF060",  "#BFF217", "#0089B2",
              "#CC1577", "#F2B950",  "#EC496F", "#326397", 
              "#B26314", "#027368", "#610B5E", "#F3C300", "#875692", "#F38400", 
              "#1D72F5", "khaki2", "darkturquoise", "#bfbfbf", "#F2F3F4", 
              "#FF9326", "#000000"
    )
    final_cols <- rep(cols, length.out = length(all_categories))
    names(final_cols) <- all_categories
    return(final_cols)
  })
  
  # percentage fraction 
  output$percentage <- renderUI({
    req(getAllHaplotypes())
    frequencyAllHaplotypes <- summary(getAllHaplotypes())
    sumFrequencyAllHaplotypes <- sum(frequencyAllHaplotypes)
    frequencySelectedHaplotypes <-
      frequencyAllHaplotypes[1:input$sliderHaplotypes]
    sumFrequencySelectedHaplotypes <-
      sum(frequencySelectedHaplotypes)
    percentage <-
      round(sumFrequencySelectedHaplotypes / sumFrequencyAllHaplotypes * 100,digits = 2)
    p("This comprises ",percentage,"% of the samples")
  })
  
  #IDLE note 
  observe({
    showNotification(
      ui = "Note: Sessions expire after 15 minutes of inactivity.",
      duration = 15,    
      type = "message",
      closeButton = TRUE
    )
  })
  ####---------------------------------------------------------------------####
  
}