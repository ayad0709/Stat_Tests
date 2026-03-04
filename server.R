# server.R
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(ggplot2)
library(readr)
library(readxl)

shinyServer(function(input, output, session) {
  
  fmt_p <- function(p) {
    if (is.null(p) || is.na(p)) return("NA")
    format.pval(p, digits = 6, eps = 1e-16)
  }
  fmt_num <- function(x, digits = 4) {
    if (is.null(x) || is.na(x)) return("NA")
    formatC(x, digits = digits, format = "f")
  }
  clamp_p <- function(p) {
    if (is.null(p) || is.na(p)) return(NA_real_)
    max(min(p, 1 - 1e-16), 1e-16)
  }
  
  sanitize_names <- function(df) {
    nm <- names(df)
    nm <- make.names(nm, unique = TRUE, allow_ = TRUE)
    names(df) <- nm
    df
  }
  
  classify_var <- function(x) {
    if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return("Date")
    if (is.logical(x)) return("Catégorielle")
    if (is.factor(x) || is.character(x)) return("Catégorielle")
    if (is.numeric(x)) return("Numérique")
    return("Autre")
  }
  
  classify_df <- function(df) {
    data.frame(
      variable = names(df),
      type = vapply(df, classify_var, character(1)),
      stringsAsFactors = FALSE
    )
  }
  
  get_selected_df <- function(df, vars) {
    df_sel <- df[, vars, drop = FALSE]
    n_initial <- nrow(df_sel)
    cc <- complete.cases(df_sel)
    df_cc <- df_sel[cc, , drop = FALSE]
    n_final <- nrow(df_cc)
    list(df = df_cc, n_initial = n_initial, n_final = n_final, exclusions = n_initial - n_final)
  }
  
  shapiro_safe <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 3 || length(x) > 5000) return(NA_real_)
    tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
  }
  
  cohen_d_ind <- function(x, g) {
    g <- droplevels(as.factor(g))
    lev <- levels(g)
    if (length(lev) != 2) return(NA_real_)
    x1 <- x[g == lev[1]]
    x2 <- x[g == lev[2]]
    n1 <- length(x1); n2 <- length(x2)
    s1 <- stats::sd(x1); s2 <- stats::sd(x2)
    sp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
    if (!is.finite(sp) || sp == 0) return(NA_real_)
    (mean(x1) - mean(x2)) / sp
  }
  
  cohen_d_paired <- function(x, y) {
    d <- x - y
    sd_d <- stats::sd(d)
    if (!is.finite(sd_d) || sd_d == 0) return(NA_real_)
    mean(d) / sd_d
  }
  
  partial_eta_sq_from_aovtab <- function(tab, effect_row) {
    if (is.null(tab) || nrow(tab) < 1) return(NA_real_)
    if (!("Sum Sq" %in% colnames(tab))) return(NA_real_)
    if (!(effect_row %in% rownames(tab))) return(NA_real_)
    ss_effect <- tab[effect_row, "Sum Sq"]
    ss_error  <- tab["Residuals", "Sum Sq"]
    denom <- ss_effect + ss_error
    if (!is.finite(denom) || denom == 0) return(NA_real_)
    ss_effect / denom
  }
  
  cramers_v <- function(chi2, n, r, c) {
    if (!is.finite(chi2) || !is.finite(n) || n <= 0) return(NA_real_)
    denom <- n * min(r - 1, c - 1)
    if (!is.finite(denom) || denom <= 0) return(NA_real_)
    sqrt(chi2 / denom)
  }
  
  alt_symbol <- function(alt) {
    if (alt == "greater") return(">")
    if (alt == "less") return("<")
    "\\neq"
  }
  
  # ---- Post-hoc helpers ----
  get_p_col_name <- function(df) {
    cand <- c("p ajustée", "p adj", "p_adj", "p", "p.value", "p-value", "p_value")
    cand[cand %in% names(df)][1]
  }
  
  posthoc_summary_lines <- function(posthoc_tbl, alpha, max_lines = 6) {
    if (is.null(posthoc_tbl) || !is.data.frame(posthoc_tbl) || nrow(posthoc_tbl) == 0) {
      return(list(
        headline = "Aucun post-hoc disponible pour ce test.",
        decision = NULL,
        lines = NULL
      ))
    }
    
    pcol <- get_p_col_name(posthoc_tbl)
    if (is.na(pcol) || is.null(pcol)) {
      return(list(
        headline = "Post-hoc calculé, mais la colonne de p-value ajustée n’a pas été détectée.",
        decision = NULL,
        lines = NULL
      ))
    }
    
    pvals <- suppressWarnings(as.numeric(posthoc_tbl[[pcol]]))
    ok <- is.finite(pvals)
    tbl <- posthoc_tbl[ok, , drop = FALSE]
    pvals <- pvals[ok]
    
    if (nrow(tbl) == 0) {
      return(list(
        headline = "Post-hoc calculé, mais aucune p-value exploitable n’a été détectée.",
        decision = NULL,
        lines = NULL
      ))
    }
    
    sig <- pvals <= alpha
    n_sig <- sum(sig, na.rm = TRUE)
    
    # Construire une étiquette de comparaison robuste
    comp <- NULL
    if ("Comparaison" %in% names(tbl)) comp <- as.character(tbl$Comparaison)
    if (is.null(comp)) {
      g1 <- if ("Groupe 1" %in% names(tbl)) as.character(tbl[["Groupe 1"]]) else rep(NA_character_, nrow(tbl))
      g2 <- if ("Groupe 2" %in% names(tbl)) as.character(tbl[["Groupe 2"]]) else rep(NA_character_, nrow(tbl))
      comp <- paste0(g1, " vs ", g2)
    }
    if ("Effet" %in% names(tbl)) {
      comp <- paste0("[", tbl$Effet, "] ", comp)
    }
    
    ord <- order(pvals, decreasing = FALSE)
    tbl <- tbl[ord, , drop = FALSE]
    pvals <- pvals[ord]
    comp <- comp[ord]
    sig <- sig[ord]
    
    lines <- lapply(seq_len(min(nrow(tbl), max_lines)), function(i) {
      dec <- if (sig[i]) "Rejet de H0 (différence significative)" else "Échec du rejet de H0 (différence non significative)"
      tags$li(
        tags$b(comp[i]), " — p ajustée = ", fmt_p(pvals[i]),
        " (α = ", fmt_num(alpha, 4), ") → ", dec, "."
      )
    })
    
    headline <- paste0(
      "Résumé post-hoc : ", n_sig, " comparaison(s) significative(s) sur ", nrow(tbl),
      " au seuil α = ", fmt_num(alpha, 4), "."
    )
    
    decision <- if (n_sig > 0) {
      "Décision post-hoc : au moins une différence entre niveaux est statistiquement significative après correction."
    } else {
      "Décision post-hoc : aucune différence deux-à-deux n’est statistiquement significative après correction."
    }
    
    list(headline = headline, decision = decision, lines = lines)
  }
  
  make_opt <- function(category, test_code, test_name, vars, vars_text, justification) {
    id <- paste(c(test_code, vars), collapse = "__")
    label <- paste0(test_name, " — ", vars_text)
    list(
      id = id,
      category = category,
      test_code = test_code,
      test_name = test_name,
      vars = vars,
      vars_text = vars_text,
      label = label,
      justification = justification
    )
  }
  

  
  
  
  
  output$sheet_ui <- renderUI({
    req(input$data_source)
    
    # Only show sheet selector for uploads
    if (!identical(input$data_source, "upload")) return(NULL)
    
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    
    if (ext %in% c("xlsx", "xls")) {
      sheets <- tryCatch(readxl::excel_sheets(input$file$datapath), error = function(e) character(0))
      if (length(sheets) == 0) {
        return(tagList(helpText("Impossible de lister les feuilles Excel : vérifiez le fichier importé.")))
      }
      tagList(
        selectInput("sheet", "Feuille Excel", choices = sheets, selected = sheets[1]),
        helpText("Sélectionnez la feuille à importer (Excel).")
      )
    } else {
      NULL
    }
  })
  
  
  
  
  
  
  
  
    
  data_raw <- reactive({
    req(input$data_source)
    
    # ---- DEMO BRANCH ----
    if (identical(input$data_source, "demo")) {
      req(input$demo_dataset)
      
      # map labels like "beaver1 (beavers)" to correct object to load
      spec <- switch(
        input$demo_dataset,
        "BJsales.lead (BJsales)"        = list(list = "BJsales",       obj = "BJsales.lead"),
        "beaver1 (beavers)"             = list(list = "beavers",       obj = "beaver1"),
        "beaver2 (beavers)"             = list(list = "beavers",       obj = "beaver2"),
        "euro.cross (euro)"             = list(list = "euro",          obj = "euro.cross"),
        "fdeaths (UKLungDeaths)"        = list(list = "UKLungDeaths",  obj = "fdeaths"),
        "ldeaths (UKLungDeaths)"        = list(list = "UKLungDeaths",  obj = "ldeaths"),
        "mdeaths (UKLungDeaths)"        = list(list = "UKLungDeaths",  obj = "mdeaths"),
        "freeny.x (freeny)"             = list(list = "freeny",        obj = "freeny.x"),
        "freeny.y (freeny)"             = list(list = "freeny",        obj = "freeny.y"),
        "stack.loss (stackloss)"        = list(list = "stackloss",     obj = "stack.loss"),
        "stack.x (stackloss)"           = list(list = "stackloss",     obj = "stack.x"),
        "state.abb (state)"             = list(list = "state",         obj = "state.abb"),
        "state.area (state)"            = list(list = "state",         obj = "state.area"),
        "state.center (state)"          = list(list = "state",         obj = "state.center"),
        "state.division (state)"        = list(list = "state",         obj = "state.division"),
        "state.name (state)"            = list(list = "state",         obj = "state.name"),
        "state.region (state)"          = list(list = "state",         obj = "state.region"),
        "state.x77 (state)"             = list(list = "state",         obj = "state.x77"),
        "sunspot.m2014 (sunspot.month)" = list(list = "sunspot.month", obj = "sunspot.m2014"),
        list(list = input$demo_dataset, obj = input$demo_dataset)
      )
      
      e <- new.env(parent = emptyenv())
      data(list = spec$list, package = "datasets", envir = e)
      obj <- get(spec$obj, envir = e, inherits = FALSE)
      
      # normalize common non-data.frame objects so your UI/tests keep working
      if (is.matrix(obj)) obj <- as.data.frame(obj)
      if (inherits(obj, "ts")) obj <- data.frame(value = as.numeric(obj))
      if (inherits(obj, "table")) obj <- as.data.frame(obj)
      
      validate(need(is.data.frame(obj), "Ce jeu de données demo n'est pas tabulaire (data.frame). Choisissez un autre."))
      
      return(sanitize_names(obj))
    }
    
    # ---- UPLOAD BRANCH (your existing code) ----
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    path <- input$file$datapath
    
    df <- NULL
    if (ext == "csv") {
      df <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
      df <- as.data.frame(df)
    } else if (ext %in% c("xlsx", "xls")) {
      sh <- input$sheet
      if (is.null(sh) || !nzchar(sh)) sh <- 1
      df <- readxl::read_excel(path, sheet = sh)
      df <- as.data.frame(df)
    } else {
      validate(need(FALSE, "Format de fichier non supporté. Importez un .csv ou un .xlsx."))
    }
    
    sanitize_names(df)
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # ---- Demo dataset selector UI (curated list you provided) ----
  demo_choices <- c(
    "AirPassengers",
    "BJsales",
    "BJsales.lead (BJsales)",
    "BOD",
    "CO2",
    "ChickWeight",
    "DNase",
    "EuStockMarkets",
    "Formaldehyde",
    "HairEyeColor",
    # "Harman23.cor",
    # "Harman74.cor",
    "Indometh",
    "InsectSprays",
    "JohnsonJohnson",
    "LakeHuron",
    "LifeCycleSavings",
    "Loblolly",
    "Nile",
    "Orange",
    "OrchardSprays",
    "PlantGrowth",
    "Puromycin",
    "Seatbelts",
    "Theoph",
    "Titanic",
    "ToothGrowth",
    "UCBAdmissions",
    "UKDriverDeaths",
    "UKgas",
    "USAccDeaths",
    "USArrests",
    "USJudgeRatings",
    "USPersonalExpenditure",
    # "UScitiesD",
    "VADeaths",
    "WWWusage",
    "WorldPhones",
    # "ability.cov",
    "airmiles",
    "airquality",
    "anscombe",
    "attenu",
    "attitude",
    "austres",
    "beaver1 (beavers)",
    "beaver2 (beavers)",
    "cars",
    "chickwts",
    "co2",
    "crimtab",
    "discoveries",
    "esoph",
    # "euro",
    "euro.cross (euro)",
    # "eurodist",
    "faithful",
    "fdeaths (UKLungDeaths)",
    "freeny",
    "freeny.x (freeny)",
    "freeny.y (freeny)",
    # "gait",
    "infert",
    "iris",
    # "iris3",
    # "islands",
    "ldeaths (UKLungDeaths)",
    "lh",
    "longley",
    "lynx",
    "mdeaths (UKLungDeaths)",
    "morley",
    "mtcars",
    "nhtemp",
    "nottem",
    "npk",
    "occupationalStatus",
    # "precip",
    "presidents",
    "pressure",
    "quakes",
    "randu",
    # "rivers",
    "rock",
    "sleep",
    # "stack.loss (stackloss)",
    "stack.x (stackloss)",
    "stackloss",
    # "state.abb (state)",
    # "state.area (state)",
    # "state.center (state)",
    # "state.division (state)",
    # "state.name (state)",
    # "state.region (state)",
    "state.x77 (state)",
    "sunspot.m2014 (sunspot.month)",
    "sunspot.month",
    "sunspot.year",
    "sunspots",
    "swiss",
    "treering",
    "trees",
    "uspop",
    "volcano",
    "warpbreaks",
    "women"
  )
  
  output$demo_dataset_ui <- renderUI({
    selectInput(
      inputId = "demo_dataset",
      label   = "Jeu de données de démonstration (package datasets)",
      choices = demo_choices,
      selected = "iris"
    )
  })
  
  load_demo_dataset <- function(label) {
    spec <- switch(
      label,
      "BJsales.lead (BJsales)"        = list(list = "BJsales",       obj = "BJsales.lead"),
      "beaver1 (beavers)"             = list(list = "beavers",       obj = "beaver1"),
      "beaver2 (beavers)"             = list(list = "beavers",       obj = "beaver2"),
      "euro.cross (euro)"             = list(list = "euro",          obj = "euro.cross"),
      "fdeaths (UKLungDeaths)"        = list(list = "UKLungDeaths",  obj = "fdeaths"),
      "ldeaths (UKLungDeaths)"        = list(list = "UKLungDeaths",  obj = "ldeaths"),
      "mdeaths (UKLungDeaths)"        = list(list = "UKLungDeaths",  obj = "mdeaths"),
      "freeny.x (freeny)"             = list(list = "freeny",        obj = "freeny.x"),
      "freeny.y (freeny)"             = list(list = "freeny",        obj = "freeny.y"),
      "stack.loss (stackloss)"        = list(list = "stackloss",     obj = "stack.loss"),
      "stack.x (stackloss)"           = list(list = "stackloss",     obj = "stack.x"),
      "state.abb (state)"             = list(list = "state",         obj = "state.abb"),
      "state.area (state)"            = list(list = "state",         obj = "state.area"),
      "state.center (state)"          = list(list = "state",         obj = "state.center"),
      "state.division (state)"        = list(list = "state",         obj = "state.division"),
      "state.name (state)"            = list(list = "state",         obj = "state.name"),
      "state.region (state)"          = list(list = "state",         obj = "state.region"),
      "state.x77 (state)"             = list(list = "state",         obj = "state.x77"),
      "sunspot.m2014 (sunspot.month)" = list(list = "sunspot.month", obj = "sunspot.m2014"),
      list(list = label, obj = label)
    )
    
    e <- new.env(parent = emptyenv())
    data(list = spec$list, package = "datasets", envir = e)
    obj <- get(spec$obj, envir = e, inherits = FALSE)
    
    # Normalize a couple common non-data.frame types
    if (is.matrix(obj)) obj <- as.data.frame(obj)
    if (inherits(obj, "ts")) obj <- data.frame(value = as.numeric(obj))
    
    obj
  }
  
  # ---- Unified data reactive: demo OR upload ----
  active_data <- reactive({
    req(input$data_source)
    
    if (identical(input$data_source, "demo")) {
      req(input$demo_dataset)
      out <- tryCatch(load_demo_dataset(input$demo_dataset),
                      error = function(e) {
                        showNotification(e$message, type = "error")
                        NULL
                      })
      req(!is.null(out))
      out
    } else {
      req(input$file)
      # KEEP your existing upload reading code here (csv/xlsx)
      # and return the resulting data.frame.
      stop("Upload branch: replace this with your current file-reading code.")
    }
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  demo_desc <- c(
    "AirPassengers" = "Monthly Airline Passenger Numbers 1949-1960",
    "BJsales" = "Sales Data with Leading Indicator",
    "BJsales.lead (BJsales)" = "Sales Data with Leading Indicator",
    "BOD" = "Biochemical Oxygen Demand",
    "CO2" = "Carbon Dioxide Uptake in Grass Plants",
    "ChickWeight" = "Weight versus age of chicks on different diets",
    "DNase" = "Elisa assay of DNase",
    "EuStockMarkets" = "Daily Closing Prices of Major European Stock Indices, 1991-1998",
    "Formaldehyde" = "Determination of Formaldehyde",
    "HairEyeColor" = "Hair and Eye Color of Statistics Students",
    "Harman23.cor" = "Harman Example 2.3",
    "Harman74.cor" = "Harman Example 7.4",
    "Indometh" = "Pharmacokinetics of Indomethacin",
    "InsectSprays" = "Effectiveness of Insect Sprays",
    "JohnsonJohnson" = "Quarterly Earnings per Johnson & Johnson Share",
    "LakeHuron" = "Level of Lake Huron 1875-1972",
    "LifeCycleSavings" = "Intercountry Life-Cycle Savings Data",
    "Loblolly" = "Growth of Loblolly Pine Trees",
    "Nile" = "Flow of the River Nile",
    "Orange" = "Growth of Orange Trees",
    "OrchardSprays" = "Potency of Orchard Sprays",
    "PlantGrowth" = "Results from an Experiment on Plant Growth",
    "Puromycin" = "Reaction Velocity of an Enzymatic Reaction",
    "Seatbelts" = "Road Casualties in Great Britain 1969-84",
    "Theoph" = "Pharmacokinetics of Theophylline",
    "Titanic" = "Survival of passengers on the Titanic",
    "ToothGrowth" = "The Effect of Vitamin C on Tooth Growth in Guinea Pigs",
    "UCBAdmissions" = "Student Admissions at UC Berkeley",
    "UKDriverDeaths" = "Road Casualties in Great Britain 1969-84",
    "UKgas" = "UK Quarterly Gas Consumption",
    "USAccDeaths" = "Accidental Deaths in the US 1973-1978",
    "USArrests" = "Violent Crime Rates by US State",
    "USJudgeRatings" = "Lawyers' Ratings of State Judges in the US Superior Court",
    "USPersonalExpenditure" = "Personal Expenditure Data",
    "UScitiesD" = "Distances Between European Cities and Between US Cities",
    "VADeaths" = "Death Rates in Virginia (1940)",
    "WWWusage" = "Internet Usage per Minute",
    "WorldPhones" = "The World's Telephones",
    "ability.cov" = "Ability and Intelligence Tests",
    "airmiles" = "Passenger Miles on Commercial US Airlines, 1937-1960",
    "airquality" = "New York Air Quality Measurements",
    "anscombe" = "Anscombe's Quartet of 'Identical' Simple Linear Regressions",
    "attenu" = "The Joyner-Boore Attenuation Data",
    "attitude" = "The Chatterjee-Price Attitude Data",
    "austres" = "Quarterly Time Series of the Number of Australian Residents",
    "beaver1 (beavers)" = "Body Temperature Series of Two Beavers",
    "beaver2 (beavers)" = "Body Temperature Series of Two Beavers",
    "cars" = "Speed and Stopping Distances of Cars",
    "chickwts" = "Chicken Weights by Feed Type",
    "co2" = "Mauna Loa Atmospheric CO2 Concentration",
    "crimtab" = "Student's 3000 Criminals Data",
    "discoveries" = "Yearly Numbers of Important Discoveries",
    "esoph" = "Smoking, Alcohol and (O)esophageal Cancer",
    "euro" = "Conversion Rates of Euro Currencies",
    "euro.cross (euro)" = "Conversion Rates of Euro Currencies",
    "eurodist" = "Distances Between European Cities and Between US Cities",
    "faithful" = "Old Faithful Geyser Data",
    "fdeaths (UKLungDeaths)" = "Monthly Deaths from Lung Diseases in the UK",
    "freeny" = "Freeny's Revenue Data",
    "freeny.x (freeny)" = "Freeny's Revenue Data",
    "freeny.y (freeny)" = "Freeny's Revenue Data",
    "gait" = "Hip and Knee Angle while Walking",
    "infert" = "Infertility after Spontaneous and Induced Abortion",
    "iris" = "Edgar Anderson's Iris Data",
    "iris3" = "Edgar Anderson's Iris Data...",
    "islands" = "Areas of the World's Major Landmasses",
    "ldeaths (UKLungDeaths)" = "Monthly Deaths from Lung Diseases in the UK",
    "lh" = "Luteinizing Hormone in Blood Samples",
    "longley" = "Longley's Economic Regression Data",
    "lynx" = "Annual Canadian Lynx trappings 1821-1934",
    "mdeaths (UKLungDeaths)" = "Monthly Deaths from Lung Diseases in the UK",
    "morley" = "Michelson Speed of Light Data",
    "mtcars" = "Motor Trend Car Road Tests",
    "nhtemp" = "Average Yearly Temperatures in New Haven",
    "nottem" = "Average Monthly Temperatures at Nottingham, 1920-1939",
    "npk" = "Classical N, P, K Factorial Experiment",
    "occupationalStatus" = "Occupational Status of Fathers and their Sons",
    "precip" = "Annual Precipitation in Selected US Cities",
    "presidents" = "Quarterly Approval Ratings of US Presidents",
    "pressure" = "Vapor Pressure of Mercury as a Function of Temperature",
    "quakes" = "Locations of Earthquakes off Fiji",
    "randu" = "Random Numbers from Congruential Generator RANDU",
    "rivers" = "Lengths of Major North American Rivers",
    "rock" = "Measurements on Petroleum Rock Samples",
    "sleep" = "Student's Sleep Data",
    "stack.loss (stackloss)" = "Brownlee's Stack Loss Plant Data",
    "stack.x (stackloss)" = "Brownlee's Stack Loss Plant Data",
    "stackloss" = "Brownlee's Stack Loss Plant Data",
    "state.abb (state)" = "US State Facts and Figures",
    "state.area (state)" = "US State Facts and Figures",
    "state.center (state)" = "US State Facts and Figures",
    "state.division (state)" = "US State Facts and Figures",
    "state.name (state)" = "US State Facts and Figures",
    "state.region (state)" = "US State Facts and Figures",
    "state.x77 (state)" = "US State Facts and Figures",
    "sunspot.m2014 (sunspot.month)" = "Monthly Sunspot Data, from 1749 to 'Present'",
    "sunspot.month" = "Monthly Sunspot Data, from 1749 to 'Present'",
    "sunspot.year" = "Yearly Sunspot Data, 1700-1988",
    "sunspots" = "Monthly Sunspot Numbers, 1749-1983",
    "swiss" = "Swiss Fertility and Socioeconomic Indicators (1888) Data",
    "treering" = "Yearly Tree-Ring Data, -6000-1979",
    "trees" = "Diameter, Height and Volume for Black Cherry Trees",
    "uspop" = "Populations Recorded by the US Census",
    "volcano" = "Topographic Information on Auckland's Maunga Whau Volcano",
    "warpbreaks" = "The Number of Breaks in Yarn during Weaving",
    "women" = "Average Heights and Weights for American Women"
  )
  
  
  
  output$demo_info_ui <- renderUI({
    req(input$data_source)
    if (!identical(input$data_source, "demo")) return(NULL)
    
    req(input$demo_dataset)
    df <- data_raw()
    
    desc <- demo_desc[[input$demo_dataset]]
    if (is.null(desc) || !nzchar(desc)) desc <- "Description indisponible."
    
    # Types
    types_df <- classify_df(df)
    n_num   <- sum(types_df$type == "Numérique")
    n_cat   <- sum(types_df$type == "Catégorielle")
    n_date  <- sum(types_df$type == "Date")
    n_other <- sum(types_df$type == "Autre")
    
    # Qualité
    miss_total <- sum(is.na(df))
    miss_by_col <- sort(colSums(is.na(df)), decreasing = TRUE)
    top_miss <- head(miss_by_col[miss_by_col > 0], 8)
    dup_rows <- sum(duplicated(df))
    mem_mb <- as.numeric(object.size(df)) / (1024^2)
    
    # Preview types (cap to 15)
    types_preview <- head(types_df, 15)
    
    # Mini numeric summary (top 8 numeric vars)
    num_vars <- names(df)[vapply(df, classify_var, character(1)) == "Numérique"]
    num_vars <- head(num_vars, 8)
    num_tbl <- NULL
    if (length(num_vars) > 0) {
      num_tbl <- do.call(rbind, lapply(num_vars, function(v) {
        x <- df[[v]]
        x <- x[is.finite(x)]
        data.frame(
          Variable = v,
          n = length(x),
          Moyenne = if (length(x)) mean(x) else NA_real_,
          Ecart_type = if (length(x)) sd(x) else NA_real_,
          Mediane = if (length(x)) stats::median(x) else NA_real_,
          Min = if (length(x)) min(x) else NA_real_,
          Max = if (length(x)) max(x) else NA_real_,
          stringsAsFactors = FALSE
        )
      }))
      num_tbl$Moyenne <- vapply(num_tbl$Moyenne, fmt_num, character(1), digits = 4)
      num_tbl$Ecart_type <- vapply(num_tbl$Ecart_type, fmt_num, character(1), digits = 4)
      num_tbl$Mediane <- vapply(num_tbl$Mediane, fmt_num, character(1), digits = 4)
      num_tbl$Min <- vapply(num_tbl$Min, fmt_num, character(1), digits = 4)
      num_tbl$Max <- vapply(num_tbl$Max, fmt_num, character(1), digits = 4)
    }
    
    # Mini categorical preview (top 5 cat vars, top 5 levels)
    cat_vars <- names(df)[vapply(df, classify_var, character(1)) == "Catégorielle"]
    cat_vars <- head(cat_vars, 5)
    
    cat_blocks <- NULL
    if (length(cat_vars) > 0) {
      cat_blocks <- lapply(cat_vars, function(v) {
        x <- as.character(df[[v]])
        x[is.na(x) | !nzchar(x)] <- "(NA/VIDE)"
        tab <- sort(table(x), decreasing = TRUE)
        tab <- head(tab, 5)
        tb <- data.frame(Modalité = names(tab), Effectif = as.integer(tab), stringsAsFactors = FALSE)
        
        tags$div(
          style = "margin-bottom:10px;",
          tags$h5(style = "margin:0 0 6px 0;", paste0("• ", v)),
          tags$div(
            style = "overflow-x:auto;",
            tags$table(
              class = "table table-striped table-bordered table-condensed",
              tags$thead(tags$tr(tags$th("Modalité (top 5)"), tags$th("Effectif"))),
              tags$tbody(lapply(seq_len(nrow(tb)), function(i) {
                tags$tr(tags$td(tb$Modalité[i]), tags$td(tb$Effectif[i]))
              }))
            )
          )
        )
      })
    }
    
    # Collapsible helper: CLOSED by default
    section <- function(title, cls, chip_text, open = FALSE, ...) {
      tags$details(
        class = paste("demo-details", cls),
        open = if (isTRUE(open)) TRUE else NULL,  # <- do NOT set open unless TRUE
        tags$summary(
          tags$span(
            class = "demo-sum-left",
            shiny::icon("angle-down"),
            title,
            tags$span(class = "demo-chip", chip_text)
          ),
          tags$span(class = "demo-chevron", shiny::icon("chevron-down"))
        ),
        tags$div(class = "demo-body", ...)
      )
    }
    
    tagList(
      # Header card
      tags$div(
        style = "padding:12px 12px; background:#fff; border:1px solid #e6e9f2; border-radius:12px; box-shadow:0 2px 10px rgba(0,0,0,.04);",
        tags$h4(style = "margin:0 0 6px 0;", shiny::icon("database"), " Données de démonstration"),
        tags$p(style = "margin:0;", tags$b("Nom : "), input$demo_dataset),
        tags$p(style = "margin:0;", tags$b("Description : "), desc),
        tags$p(class = "small-note", style = "margin:6px 0 0 0;", shiny::icon("info-circle"),
               "Cliquez sur une section ci-dessous pour l’ouvrir."),
        tags$hr(style = "margin:10px 0;"),
        tags$div(
          style = "display:grid; grid-template-columns: 1fr 1fr; gap: 8px;",
          tags$div(tags$b("Classe : "), paste(class(df), collapse = ", ")),
          tags$div(tags$b("Dimensions : "), nrow(df), " × ", ncol(df))
        )
      ),
      
      # Section 1: Data quality (CLOSED)
      section(
        title = "Qualité des données",
        cls = "demo-q",
        chip_text = paste0("NA: ", miss_total, " • Dups: ", dup_rows),
        open = FALSE,
        tags$table(
          class = "kv-table",
          tags$tr(tags$td(tags$b("Valeurs manquantes (total) :")), tags$td(miss_total)),
          tags$tr(tags$td(tags$b("Lignes dupliquées :")), tags$td(dup_rows)),
          tags$tr(tags$td(tags$b("Taille (approx.) :")), tags$td(paste0(fmt_num(mem_mb, 2), " MB")))
        ),
        if (length(top_miss) > 0) tagList(
          tags$hr(),
          tags$p(tags$b("Colonnes les plus manquantes (top 8) :")),
          tags$ul(lapply(names(top_miss), function(v) tags$li(tags$b(v), " : ", top_miss[[v]])))
        ) else tags$p(class = "small-note", "Aucune valeur manquante détectée.")
      ),
      
      # Section 2: Variable profile (CLOSED)
      section(
        title = "Profil des variables",
        cls = "demo-p",
        chip_text = paste0("Num: ", n_num, " • Cat: ", n_cat, " • Date: ", n_date),
        open = FALSE,
        
        tags$table(
          class = "kv-table",
          tags$tr(tags$td(tags$b("Numériques :")), tags$td(n_num)),
          tags$tr(tags$td(tags$b("Catégorielles :")), tags$td(n_cat)),
          tags$tr(tags$td(tags$b("Dates :")), tags$td(n_date)),
          tags$tr(tags$td(tags$b("Autres :")), tags$td(n_other))
        ),
        
        tags$hr(),
        
        fluidRow(
          column(
            6,
            textInput("var_type_filter", "Rechercher une variable", value = "", placeholder = "ex: age, mpg, income…")
          ),
          column(
            4,
            selectInput(
              "var_kind_filter",
              "Filtrer par type",
              choices = c("Tous" = "all", "Numérique" = "Numérique", "Catégorielle" = "Catégorielle", "Date" = "Date", "Autre" = "Autre"),
              selected = "all"
            )
          ),
          column(
            2,
            numericInput("var_preview_n", "N", value = 30, min = 10, max = 500, step = 10)
          )
        ),
        tags$p(class = "small-note", "Liste des variables et types (filtrable)."),
        
        tags$div(
          style = "overflow-x:auto;",
          tableOutput("vars_types_table")
        )
      ),
      
      # Section 3: Quick previews (CLOSED)
      section(
        title = "Aperçus rapides",
        cls = "demo-a",
        chip_text = paste0("Top num: ", length(num_vars), " • Top cat: ", length(cat_vars)),
        open = FALSE,
        
        if (!is.null(num_tbl) && nrow(num_tbl) > 0) tagList(
          tags$p(tags$b("Numériques (top 8) — résumé :")),
          tags$div(
            style = "overflow-x:auto;",
            tags$table(
              class = "table table-striped table-bordered table-condensed",
              tags$thead(tags$tr(
                tags$th("Variable"), tags$th("n"), tags$th("Moyenne"), tags$th("Écart-type"),
                tags$th("Médiane"), tags$th("Min"), tags$th("Max")
              )),
              tags$tbody(lapply(seq_len(nrow(num_tbl)), function(i) {
                tags$tr(
                  tags$td(tags$b(num_tbl$Variable[i])),
                  tags$td(num_tbl$n[i]),
                  tags$td(num_tbl$Moyenne[i]),
                  tags$td(num_tbl$Ecart_type[i]),
                  tags$td(num_tbl$Mediane[i]),
                  tags$td(num_tbl$Min[i]),
                  tags$td(num_tbl$Max[i])
                )
              }))
            )
          )
        ) else tags$p(class = "small-note", "Aucune variable numérique détectée."),
        
        tags$hr(),
        
        if (!is.null(cat_blocks)) tagList(
          tags$p(tags$b("Catégorielles (top 5) — modalités (top 5) :")),
          tagList(cat_blocks)
        ) else tags$p(class = "small-note", "Aucune variable catégorielle détectée.")
      )
    )
  })  
  
  output$vars_types_table <- renderTable({
    df <- data_raw()
    types_df <- classify_df(df)  # columns: variable, type
    
    # Apply type filter
    kind <- input$var_kind_filter
    if (!is.null(kind) && kind != "all") {
      types_df <- types_df[types_df$type == kind, , drop = FALSE]
    }
    
    # Apply search filter (case-insensitive substring)
    q <- input$var_type_filter
    if (!is.null(q) && nzchar(trimws(q))) {
      pat <- tolower(trimws(q))
      keep <- grepl(pat, tolower(types_df$variable), fixed = TRUE)
      types_df <- types_df[keep, , drop = FALSE]
    }
    
    # Limit rows
    n_show <- input$var_preview_n
    if (is.null(n_show) || !is.finite(n_show)) n_show <- 30
    n_show <- max(10, min(500, as.integer(n_show)))
    
    head(types_df, n_show)
  }, striped = TRUE, bordered = TRUE, spacing = "s", na = "NA")
  
  
  
  
  output$data_stats_overview_ui <- renderUI({
    df <- data_raw()
    validate(need(nrow(df) > 0, "Aucune donnée disponible."))
    
    miss_total <- sum(is.na(df))
    miss_by_col <- sort(colSums(is.na(df)), decreasing = TRUE)
    top_miss <- head(miss_by_col[miss_by_col > 0], 8)
    
    tagList(
      tags$h4("Aperçu global"),
      tags$table(
        class = "kv-table",
        tags$tr(tags$td(tags$b("Observations (n) :")), tags$td(nrow(df))),
        tags$tr(tags$td(tags$b("Variables (p) :")), tags$td(ncol(df))),
        tags$tr(tags$td(tags$b("Valeurs manquantes (total) :")), tags$td(miss_total))
      ),
      if (length(top_miss) > 0) tagList(
        tags$hr(),
        tags$h4("Variables les plus manquantes"),
        tags$ul(lapply(names(top_miss), function(v) {
          tags$li(tags$b(v), " : ", top_miss[[v]])
        }))
      )
    )
  })
  
  
  
  
  output$data_stats_ui <- renderUI({
    df <- data_raw()
    validate(need(nrow(df) > 0, "Aucune donnée disponible."))
    
    # classify columns using your existing classify_var()
    types <- vapply(df, classify_var, character(1))
    num_vars <- names(df)[types == "Numérique"]
    cat_vars <- names(df)[types == "Catégorielle"]
    
    # numeric summary table
    num_tbl <- NULL
    if (length(num_vars) > 0) {
      num_tbl <- do.call(rbind, lapply(num_vars, function(v) {
        x <- df[[v]]
        x <- x[is.finite(x)]
        data.frame(
          Variable = v,
          n = length(x),
          Moyenne = if (length(x)) mean(x) else NA_real_,
          Ecart_type = if (length(x)) sd(x) else NA_real_,
          Mediane = if (length(x)) stats::median(x) else NA_real_,
          Min = if (length(x)) min(x) else NA_real_,
          Max = if (length(x)) max(x) else NA_real_,
          stringsAsFactors = FALSE
        )
      }))
      num_tbl$Moyenne <- vapply(num_tbl$Moyenne, fmt_num, character(1), digits = 4)
      num_tbl$Ecart_type <- vapply(num_tbl$Ecart_type, fmt_num, character(1), digits = 4)
      num_tbl$Mediane <- vapply(num_tbl$Mediane, fmt_num, character(1), digits = 4)
      num_tbl$Min <- vapply(num_tbl$Min, fmt_num, character(1), digits = 4)
      num_tbl$Max <- vapply(num_tbl$Max, fmt_num, character(1), digits = 4)
    }
    
    # categorical: show top levels for each categorical variable
    cat_blocks <- NULL
    if (length(cat_vars) > 0) {
      cat_blocks <- lapply(cat_vars, function(v) {
        x <- df[[v]]
        x <- as.character(x)
        x[is.na(x) | !nzchar(x)] <- "(NA/VIDE)"
        tab <- sort(table(x), decreasing = TRUE)
        tab <- head(tab, 8)
        tb <- data.frame(Niveau = names(tab), Effectif = as.integer(tab), stringsAsFactors = FALSE)
        tagList(
          tags$h4(paste0("Catégorielle : ", v)),
          tableOutput(outputId = paste0("catfreq__", v))
        ) -> block
        
        # store table in output with a stable id
        local({
          id <- paste0("catfreq__", v)
          output[[id]] <- renderTable(tb, striped = TRUE, bordered = TRUE, spacing = "s")
        })
        
        block
      })
    }
    
    tagList(
      if (!is.null(num_tbl)) tagList(
        tags$h4("Variables numériques — résumé"),
        tableOutput("numeric_stats_table")
      ) else tags$p("Aucune variable numérique détectée."),
      
      tags$hr(),
      
      if (!is.null(cat_blocks)) tagList(
        tags$h4("Variables catégorielles — fréquences (top 8)"),
        tagList(cat_blocks)
      ) else tags$p("Aucune variable catégorielle détectée.")
    )
  })
  
  output$numeric_stats_table <- renderTable({
    df <- data_raw()
    types <- vapply(df, classify_var, character(1))
    num_vars <- names(df)[types == "Numérique"]
    validate(need(length(num_vars) > 0, "Aucune variable numérique détectée."))
    
    num_tbl <- do.call(rbind, lapply(num_vars, function(v) {
      x <- df[[v]]
      x <- x[is.finite(x)]
      data.frame(
        Variable = v,
        n = length(x),
        Moyenne = if (length(x)) mean(x) else NA_real_,
        Ecart_type = if (length(x)) sd(x) else NA_real_,
        Mediane = if (length(x)) stats::median(x) else NA_real_,
        Min = if (length(x)) min(x) else NA_real_,
        Max = if (length(x)) max(x) else NA_real_,
        stringsAsFactors = FALSE
      )
    }))
    num_tbl$Moyenne <- vapply(num_tbl$Moyenne, fmt_num, character(1), digits = 4)
    num_tbl$Ecart_type <- vapply(num_tbl$Ecart_type, fmt_num, character(1), digits = 4)
    num_tbl$Mediane <- vapply(num_tbl$Mediane, fmt_num, character(1), digits = 4)
    num_tbl$Min <- vapply(num_tbl$Min, fmt_num, character(1), digits = 4)
    num_tbl$Max <- vapply(num_tbl$Max, fmt_num, character(1), digits = 4)
    num_tbl
  }, striped = TRUE, bordered = TRUE, spacing = "s", na = "NA")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$vars_ui <- renderUI({
    req(data_raw())
    df <- data_raw()
    tagList(
      pickerInput(
        inputId = "vars",
        label = "Sélection multi-variables",
        choices = names(df),
        selected = NULL,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `selected-text-format` = "count > 2",
          `count-selected-text` = "{0} variables sélectionnées"
        )
      ),
      helpText("Sélectionnez une ou plusieurs variables. « Tests proposés » liste toutes les combinaisons. Cliquez « Exécuter » pour lancer directement l’analyse.")
    )
  })
  
  test_options <- reactive({
    df <- tryCatch(data_raw(), error = function(e) NULL)
    if (is.null(df)) return(list())
    
    vars <- input$vars
    if (is.null(vars) || length(vars) == 0) return(list())
    
    types <- vapply(vars, function(v) classify_var(df[[v]]), character(1))
    ok <- !(types %in% c("Date", "Autre"))
    vars_ok <- vars[ok]
    types_ok <- types[ok]
    if (length(vars_ok) == 0) return(list())
    
    num_vars <- vars_ok[types_ok == "Numérique"]
    cat_vars <- vars_ok[types_ok == "Catégorielle"]
    
    opts <- list()
    
    if (length(num_vars) > 0) {
      for (v in num_vars) {
        opts[[length(opts) + 1]] <- make_opt(
          "Univarié — Numérique", "shapiro", "Test de Shapiro–Wilk (normalité)",
          c(v), v,
          "Variable numérique ; teste H0 : normalité (orientation paramétrique vs non paramétrique)."
        )
        opts[[length(opts) + 1]] <- make_opt(
          "Univarié — Numérique", "qq", "QQ-plot (normalité, appui graphique)",
          c(v), v,
          "Variable numérique ; diagnostic graphique de normalité (complément de Shapiro–Wilk)."
        )
        opts[[length(opts) + 1]] <- make_opt(
          "Univarié — Numérique", "t1", "Test t à 1 échantillon",
          c(v), v,
          "Variable numérique ; compare la moyenne à une valeur de référence μ0 (bilatéral ou unilatéral)."
        )
        opts[[length(opts) + 1]] <- make_opt(
          "Univarié — Numérique", "wilcox1", "Test de Wilcoxon à 1 échantillon",
          c(v), v,
          "Variable numérique ; alternative non paramétrique vs μ0 (bilatéral ou unilatéral)."
        )
      }
    }
    
    if (length(num_vars) >= 2) {
      pairs <- combn(num_vars, 2)
      for (j in seq_len(ncol(pairs))) {
        a <- pairs[1, j]; b <- pairs[2, j]
        opts[[length(opts) + 1]] <- make_opt(
          "Bivarié — Numérique × Numérique", "pearson", "Corrélation de Pearson",
          c(a, b), paste0(a, " ~ ", b),
          "Deux variables numériques ; association linéaire (bilatéral ou unilatéral)."
        )
        opts[[length(opts) + 1]] <- make_opt(
          "Bivarié — Numérique × Numérique", "spearman", "Corrélation de Spearman",
          c(a, b), paste0(a, " ~ ", b),
          "Deux variables numériques ; association monotone via rangs (bilatéral ou unilatéral)."
        )
        opts[[length(opts) + 1]] <- make_opt(
          "Bivarié — Numérique × Numérique", "tpaired", "Test t apparié (échantillons appariés)",
          c(a, b), paste0(a, " ↔ ", b),
          "Deux mesures numériques appariées (mêmes sujets/paires) ; compare la moyenne des différences (bilatéral/unilatéral)."
        )
        opts[[length(opts) + 1]] <- make_opt(
          "Bivarié — Numérique × Numérique", "wilcoxpaired", "Wilcoxon apparié (signed-rank)",
          c(a, b), paste0(a, " ↔ ", b),
          "Deux mesures numériques appariées ; alternative non paramétrique au t apparié (bilatéral/unilatéral)."
        )
      }
    }
    
    if (length(num_vars) >= 1 && length(cat_vars) >= 1) {
      grid <- expand.grid(num_vars, cat_vars, stringsAsFactors = FALSE)
      for (i in seq_len(nrow(grid))) {
        yv <- grid[i, 1]
        gv <- grid[i, 2]
        sub <- get_selected_df(df, c(yv, gv))$df
        g <- droplevels(as.factor(sub[[gv]]))
        k <- length(levels(g))
        if (k < 2) next
        
        if (k == 2) {
          opts[[length(opts) + 1]] <- make_opt(
            "Bivarié — Numérique × Catégorielle (2 niveaux)", "tind", "Test t indépendant (Welch)",
            c(yv, gv), paste0(yv, " ~ ", gv),
            "Numérique + facteur (2 niveaux) ; comparaison de moyennes (bilatéral ou unilatéral)."
          )
          opts[[length(opts) + 1]] <- make_opt(
            "Bivarié — Numérique × Catégorielle (2 niveaux)", "wilcoxind", "Test de Wilcoxon (Mann–Whitney)",
            c(yv, gv), paste0(yv, " ~ ", gv),
            "Numérique + facteur (2 niveaux) ; alternative non paramétrique (bilatéral ou unilatéral)."
          )
        } else {
          opts[[length(opts) + 1]] <- make_opt(
            "Bivarié — Numérique × Catégorielle (≥ 3 niveaux)", "anova", "ANOVA à un facteur",
            c(yv, gv), paste0(yv, " ~ ", gv),
            "Numérique + facteur (≥3 niveaux) ; comparaison de moyennes (test global). Post-hoc : Tukey HSD (par défaut)."
          )
          opts[[length(opts) + 1]] <- make_opt(
            "Bivarié — Numérique × Catégorielle (≥ 3 niveaux)", "kruskal", "Kruskal–Wallis",
            c(yv, gv), paste0(yv, " ~ ", gv),
            "Numérique + facteur (≥3 niveaux) ; alternative non paramétrique (test global). Post-hoc pairwise disponible (p ajustée)."
          )
        }
      }
    }
    
    if (length(num_vars) >= 1 && length(cat_vars) >= 2) {
      for (y in num_vars) {
        cat_pairs <- combn(cat_vars, 2)
        for (j in seq_len(ncol(cat_pairs))) {
          a <- cat_pairs[1, j]
          b <- cat_pairs[2, j]
          opts[[length(opts) + 1]] <- make_opt(
            "Multivarié — ANOVA", "anova2", "ANOVA à deux facteurs (avec interaction)",
            c(y, a, b), paste0(y, " ~ ", a, " * ", b),
            "Numérique + 2 facteurs ; effets principaux + interaction. Post-hoc : Tukey HSD (par défaut) sur effets principaux."
          )
        }
      }
    }
    
    if (length(num_vars) >= 1 && length(cat_vars) >= 2) {
      for (y in num_vars) {
        cat_pairs <- combn(cat_vars, 2)
        for (j in seq_len(ncol(cat_pairs))) {
          idv <- cat_pairs[1, j]
          wv  <- cat_pairs[2, j]
          opts[[length(opts) + 1]] <- make_opt(
            "Multivarié — ANOVA", "rm_aov", "ANOVA à mesures répétées (1 facteur intra-sujet)",
            c(y, idv, wv), paste0(y, " ~ ", wv, " (ID = ", idv, ")"),
            "Numérique + identifiant sujet + facteur intra ; effet intra-sujet global. Post-hoc pairwise (p ajustée) disponible."
          )
        }
      }
    }
    
    if (length(cat_vars) >= 2) {
      pairs <- combn(cat_vars, 2)
      for (j in seq_len(ncol(pairs))) {
        a <- pairs[1, j]; b <- pairs[2, j]
        opts[[length(opts) + 1]] <- make_opt(
          "Bivarié — Catégorielle × Catégorielle", "chi2", "Chi-deux d’indépendance",
          c(a, b), paste0(a, " × ", b),
          "Deux variables catégorielles ; test d’indépendance (test global)."
        )
        opts[[length(opts) + 1]] <- make_opt(
          "Bivarié — Catégorielle × Catégorielle", "fisher", "Test exact de Fisher",
          c(a, b), paste0(a, " × ", b),
          "Deux variables catégorielles ; test exact (2×2 : bilatéral/unilatéral ; sinon global)."
        )
      }
    }
    
    opts
  })
  
  selected_test_id <- reactiveVal(NULL)
  
  observeEvent(input$choose_test, {
    req(input$choose_test)
    opts <- test_options()
    id <- input$choose_test
    idx <- which(vapply(opts, function(o) o$id, character(1)) == id)
    if (length(idx) == 0) return()
    
    opt <- opts[[idx[1]]]
    selected_test_id(opt$id)
    
    updatePickerInput(session, "vars", selected = opt$vars)
    updateSelectInput(session, "analysis_test", selected = opt$id)
    updateTabsetPanel(session, "top_nav", selected = "results")
    
    session$onFlushed(function() {
      shinyjs::click("run_test")
    }, once = TRUE)
  }, ignoreInit = TRUE)
  
  output$data_info_ui <- renderUI({
    df <- data_raw()
    validate(need(nrow(df) > 0, "Aucune donnée disponible."))
    
    n <- nrow(df)
    p <- ncol(df)
    miss_total <- sum(is.na(df))
    
    tagList(
      tags$table(
        class = "kv-table",
        tags$tr(tags$td(tags$b("Nombre d’observations (n) :")), tags$td(n)),
        tags$tr(tags$td(tags$b("Nombre de variables (p) :")), tags$td(p)),
        tags$tr(tags$td(tags$b("Valeurs manquantes (total) :")), tags$td(miss_total))
      ),
      tags$hr(),
      tags$h4("Types détectés (automatique)"),
      tableOutput("types_table")
    )
  })
  
  output$types_table <- renderTable({
    df <- data_raw()
    classify_df(df)
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$raw_table <- renderTable({
    df <- data_raw()
    validate(need(nrow(df) > 0, "Aucune donnée disponible."))
    head(df, 100)
  }, striped = TRUE, bordered = TRUE, spacing = "s", na = "NA")
  
  build_tests_table_ui <- function(opts_sub) {
    if (length(opts_sub) == 0) {
      return(tags$p(class = "small-note", "Aucun test applicable dans cette catégorie pour la sélection actuelle."))
    }
    tags$div(
      style = "overflow-x:auto;",
      tags$table(
        class = "table table-striped table-hover table-condensed tests-table",
        tags$thead(
          tags$tr(
            tags$th("Test"),
            tags$th("Variables"),
            tags$th("Justification"),
            tags$th(style = "width:110px;", "Action")
          )
        ),
        tags$tbody(
          lapply(opts_sub, function(o) {
            tags$tr(
              tags$td(tags$b(o$test_name)),
              tags$td(o$vars_text),
              tags$td(o$justification),
              tags$td(
                tags$button(
                  type = "button",
                  class = "btn btn-success btn-xs",
                  onclick = sprintf(
                    "Shiny.setInputValue('choose_test', '%s', {priority: 'event'});",
                    o$id
                  ),
                  icon("play"), " Exécuter"
                )
              )
            )
          })
        )
      )
    )
  }
  
  output$proposed_ui <- renderUI({
    df <- tryCatch(data_raw(), error = function(e) NULL)
    validate(need(!is.null(df), "Importez un fichier pour afficher les tests proposés."))
    
    vars <- input$vars
    validate(need(!is.null(vars) && length(vars) > 0, "Sélectionnez au moins une variable dans la barre latérale."))
    
    types <- vapply(vars, function(v) classify_var(df[[v]]), character(1))
    bad <- vars[types %in% c("Date", "Autre")]
    if (length(bad) > 0) {
      return(
        box(
          width = 12, status = "danger", solidHeader = TRUE, title = tagList(icon("exclamation-triangle"), " Variables non supportées"),
          tags$p("Variables exclues (Date/Autre) :"),
          tags$ul(lapply(bad, tags$li)),
          tags$p("Veuillez sélectionner des variables numériques et/ou catégorielles.")
        )
      )
    }
    
    opts <- test_options()
    validate(need(length(opts) > 0, "Aucun test applicable : vérifiez la sélection."))
    
    filter_val <- input$filter_tests
    if (!is.null(filter_val) && nzchar(trimws(filter_val))) {
      pat <- tolower(trimws(filter_val))
      keep <- vapply(opts, function(o) {
        any(grepl(pat, tolower(o$test_name), fixed = TRUE),
            grepl(pat, tolower(o$vars_text), fixed = TRUE),
            grepl(pat, tolower(o$category), fixed = TRUE))
      }, logical(1))
      opts <- opts[keep]
    }
    
    validate(need(length(opts) > 0, "Aucun test ne correspond au filtre ou à la sélection actuelle."))
    
    by_cat <- split(opts, vapply(opts, function(o) o$category, character(1)))
    get_cat <- function(name) if (!is.null(by_cat[[name]])) by_cat[[name]] else list()
    cat_title <- function(name) {
      n <- length(get_cat(name))
      tagList(name, tags$span(class = "badge", style = "margin-left:8px; background:#605ca8;", n))
    }
    
    tagList(
      fluidRow(
        box(
          width = 12, status = "info", solidHeader = TRUE,
          title = tagList(icon("id-card"), " Résumé"),
          # textInput("filter_tests", "Filtrer les tests (nom / variables / catégorie)", value = ""),
          # helpText("Tapez un mot-clé (ex. « corrélation », « ANOVA », un nom de variable). Le filtre agit sur la liste de tests ci-dessous."),
          # tags$hr(),
          tags$p(tags$b("Résumé de la sélection (typage automatique) :")),
          tags$ul(lapply(seq_along(vars), function(i) tags$li(tags$b(vars[i]), " — ", types[i]))),
          tags$p(class = "small-note", "Cliquez « Exécuter » sur un test : bascule vers « Analyse & résultats » et exécute.")
        )
      ),
      fluidRow(
        box(
          width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
          title = cat_title("Univarié — Numérique"),
          build_tests_table_ui(get_cat("Univarié — Numérique"))
        ),
        box(
          width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
          title = cat_title("Bivarié — Numérique × Numérique"),
          build_tests_table_ui(get_cat("Bivarié — Numérique × Numérique"))
        )
      ),
      fluidRow(
        box(
          width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
          title = cat_title("Bivarié — Numérique × Catégorielle (2 niveaux)"),
          build_tests_table_ui(get_cat("Bivarié — Numérique × Catégorielle (2 niveaux)"))
        ),
        box(
          width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
          title = cat_title("Bivarié — Numérique × Catégorielle (≥ 3 niveaux)"),
          build_tests_table_ui(get_cat("Bivarié — Numérique × Catégorielle (≥ 3 niveaux)"))
        )
      ),
      fluidRow(
        box(
          width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
          title = cat_title("Multivarié — ANOVA"),
          build_tests_table_ui(get_cat("Multivarié — ANOVA"))
        )
      ),
      fluidRow(
        box(
          width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
          title = cat_title("Bivarié — Catégorielle × Catégorielle"),
          build_tests_table_ui(get_cat("Bivarié — Catégorielle × Catégorielle"))
        )
      )
    )
  })
  
  output$analysis_test_select_ui <- renderUI({
    df <- tryCatch(data_raw(), error = function(e) NULL)
    validate(need(!is.null(df), "Importez un fichier pour choisir un test."))
    
    opts <- test_options()
    validate(need(length(opts) > 0, "Sélectionnez au moins une variable pour générer la liste des tests."))
    
    by_cat <- split(opts, vapply(opts, function(o) o$category, character(1)))
    choices <- lapply(by_cat, function(lst) {
      setNames(vapply(lst, function(o) o$id, character(1)),
               vapply(lst, function(o) o$label, character(1)))
    })
    
    sel <- input$analysis_test
    if (is.null(sel) || !nzchar(sel)) sel <- selected_test_id()
    if (is.null(sel) || !nzchar(sel)) sel <- opts[[1]]$id
    
    tagList(
      selectInput(
        inputId = "analysis_test",
        label = "Test à exécuter (organisé par catégories)",
        choices = choices,
        selected = sel
      ),
      helpText("Choisissez un test. Les paramètres et l’exécution se font à droite.")
    )
  })
  
  output$analysis_selected_info_ui <- renderUI({
    opts <- test_options()
    validate(need(length(opts) > 0, ""))
    
    id <- input$analysis_test
    if (is.null(id) || !nzchar(id)) id <- selected_test_id()
    validate(need(!is.null(id) && nzchar(id), ""))
    
    idx <- which(vapply(opts, function(o) o$id, character(1)) == id)
    validate(need(length(idx) > 0, ""))
    
    o <- opts[[idx[1]]]
    
    tagList(
      tags$p(tags$b("Test sélectionné : "), o$test_name),
      tags$p(tags$b("Variables : "), o$vars_text),
      tags$p(tags$b("Justification : "), o$justification)
    )
  })
  
  output$hypotheses_box_ui <- renderUI({
    df0 <- tryCatch(data_raw(), error = function(e) NULL)
    validate(need(!is.null(df0), ""))
    
    opts <- test_options()
    validate(need(length(opts) > 0, ""))
    
    id <- input$analysis_test
    if (is.null(id) || !nzchar(id)) id <- selected_test_id()
    validate(need(!is.null(id) && nzchar(id), ""))
    
    idx <- which(vapply(opts, function(o) o$id, character(1)) == id)
    validate(need(length(idx) > 0, ""))
    
    opt <- opts[[idx[1]]]
    
    alpha <- input$alpha_test
    if (is.null(alpha) || !is.finite(alpha)) alpha <- 0.05
    
    alt <- input$alternative
    if (is.null(alt) || !nzchar(alt)) alt <- "two.sided"
    if (!alt %in% c("two.sided", "greater", "less")) alt <- "two.sided"
    
    mu0 <- input$mu0
    if (is.null(mu0) || !is.finite(mu0)) mu0 <- 0
    
    sel <- get_selected_df(df0, opt$vars)
    df <- sel$df
    
    H0_math <- ""
    H1_math <- ""
    H0_txt  <- ""
    H1_txt  <- ""
    
    code <- opt$test_code
    
    if (code %in% c("shapiro", "qq")) {
      H0_math <- "\\(H_0: X \\sim \\mathcal{N}(\\mu,\\sigma^2)\\)"
      H1_math <- "\\(H_1: X \\not\\sim \\mathcal{N}(\\mu,\\sigma^2)\\)"
      H0_txt  <- "H0 : la distribution est compatible avec la normalité."
      H1_txt  <- "H1 : la distribution n’est pas compatible avec la normalité."
    }
    
    if (code %in% c("t1", "wilcox1")) {
      sym <- alt_symbol(alt)
      H0_math <- paste0("\\(H_0: \\mu = ", fmt_num(mu0, 4), "\\)")
      H1_math <- paste0("\\(H_1: \\mu ", sym, " ", fmt_num(mu0, 4), "\\)")
      H0_txt  <- paste0("H0 : la moyenne populationnelle est égale à ", fmt_num(mu0, 4), ".")
      H1_txt  <- paste0(
        "H1 : la moyenne populationnelle est ",
        if (alt == "two.sided") "différente de "
        else if (alt == "greater") "supérieure à "
        else "inférieure à ",
        fmt_num(mu0, 4), "."
      )
    }
    
    if (code %in% c("pearson", "spearman")) {
      sym <- alt_symbol(alt)
      H0_math <- "\\(H_0: \\rho = 0\\)"
      H1_math <- paste0("\\(H_1: \\rho ", sym, " 0\\)")
      H0_txt  <- "H0 : corrélation populationnelle nulle."
      H1_txt  <- paste0(
        "H1 : corrélation populationnelle ",
        if (alt == "two.sided") "non nulle."
        else if (alt == "greater") "positive."
        else "négative."
      )
    }
    
    if (code %in% c("tpaired", "wilcoxpaired")) {
      sym <- alt_symbol(alt)
      H0_math <- "\\(H_0: \\mu_d = 0\\)"
      H1_math <- paste0("\\(H_1: \\mu_d ", sym, " 0\\)")
      H0_txt  <- "H0 : la moyenne des différences (d = X − Y) est nulle."
      H1_txt  <- paste0(
        "H1 : la moyenne des différences est ",
        if (alt == "two.sided") "non nulle." else if (alt == "greater") "positive." else "négative."
      )
    }
    
    if (code %in% c("tind", "wilcoxind")) {
      validate(need(ncol(df) == 2, ""))
      
      gv <- opt$vars[2]
      g  <- droplevels(as.factor(df[[gv]]))
      lev <- levels(g)
      validate(need(length(lev) >= 2, ""))
      
      sym <- alt_symbol(alt)
      
      H0_math <- "\\(H_0: \\mu_1 = \\mu_2\\)"
      H1_math <- paste0("\\(H_1: \\mu_1 ", sym, " \\mu_2\\)")
      
      H0_txt <- paste0("H0 : moyennes populationnelles égales (", lev[1], " vs ", lev[2], ").")
      H1_txt <- paste0(
        "H1 : moyenne du groupe ", lev[1], " ",
        if (alt=="two.sided") "différente de" else if (alt=="greater") "supérieure à" else "inférieure à",
        " celle du groupe ", lev[2], "."
      )
    }
    
    if (code %in% c("anova", "kruskal", "anova2", "rm_aov")) {
      if (code == "rm_aov") {
        H0_math <- "\\(H_0: \\mu_{1} = \\mu_{2} = \\dots = \\mu_{k}\\) (intra-sujet)"
        H1_math <- "\\(H_1: \\exists\\ i\\neq j,\\ \\mu_i \\neq \\mu_j\\)"
        H0_txt  <- "H0 : moyennes (mesures répétées) identiques entre niveaux du facteur intra."
        H1_txt  <- "H1 : au moins une moyenne intra-sujet diffère."
      } else if (code == "anova2") {
        H0_math <- "\\(H_0: \\text{pas d'effet (facteur A), pas d'effet (facteur B), pas d'interaction}\\)"
        H1_math <- "\\(H_1: \\text{au moins un effet principal ou une interaction existe}\\)"
        H0_txt  <- "H0 : pas d'effet des facteurs et pas d'interaction."
        H1_txt  <- "H1 : au moins un effet principal ou une interaction."
      } else {
        H0_math <- "\\(H_0: \\mu_1 = \\mu_2 = \\dots = \\mu_k\\)"
        H1_math <- "\\(H_1: \\exists\\ i\\neq j,\\ \\mu_i \\neq \\mu_j\\)"
        H0_txt  <- "H0 : toutes les moyennes populationnelles sont égales."
        H1_txt  <- "H1 : au moins une moyenne diffère."
      }
    }
    
    if (code %in% c("chi2", "fisher")) {
      H0_math <- "\\(H_0: \\text{indépendance}\\)"
      H1_math <- "\\(H_1: \\text{dépendance (association)}\\)"
      H0_txt  <- "H0 : indépendance."
      H1_txt  <- "H1 : association."
    }
    
    validate(need(nzchar(H0_math) && nzchar(H1_math), ""))
    
    h0_cls <- "hypo-muted"
    h1_cls <- "hypo-muted"
    a_res <- tryCatch(analysis(), error = function(e) NULL)
    
    if (!is.null(a_res) && !is.null(a_res$opt) && !is.null(a_res$opt$id) && identical(a_res$opt$id, opt$id)) {
      if (is.character(a_res$decision) && nzchar(a_res$decision)) {
        if (grepl("^Rejet de H0$", a_res$decision)) {
          h0_cls <- "hypo-red"
          h1_cls <- "hypo-green"
        } else if (grepl("^Échec du rejet de H0$", a_res$decision)) {
          h0_cls <- "hypo-green"
          h1_cls <- "hypo-red"
        }
      }
    }
    
    tagList(
      tags$hr(),
      tags$h4("Hypothèses statistiques (Formule + texte)"),
      tags$div(
        tags$div(
          class = "hypo-block",
          tags$span(class = paste("hypo-pill", h0_cls), "H0"),
          tags$b("Formule : "),
          HTML(H0_math),
          tags$span(" — "),
          H0_txt
        ),
        tags$div(
          class = "hypo-block",
          tags$span(class = paste("hypo-pill", h1_cls), "Hₐ"),
          tags$b("Formule : "),
          HTML(H1_math),
          tags$span(" — "),
          H1_txt
        )
      )
    )
  })
  
  output$test_params_ui <- renderUI({
    df <- tryCatch(data_raw(), error = function(e) NULL)
    validate(need(!is.null(df), "Importez un fichier pour régler les paramètres."))
    
    opts <- test_options()
    validate(need(length(opts) > 0, "Sélectionnez des variables puis un test."))
    
    id <- input$analysis_test
    if (is.null(id) || !nzchar(id)) id <- selected_test_id()
    validate(need(!is.null(id) && nzchar(id), "Sélectionnez un test."))
    
    idx <- which(vapply(opts, function(o) o$id, character(1)) == id)
    validate(need(length(idx) > 0, "Sélectionnez un test valide."))
    
    code <- opts[[idx[1]]]$test_code
    
    needs_alt <- code %in% c(
      "t1","wilcox1","tind","wilcoxind","pearson","spearman","fisher",
      "tpaired","wilcoxpaired"
    )
    needs_mu0 <- code %in% c("t1","wilcox1")
    
    # Post-hoc paramétrique (ANOVA) : Tukey HSD par défaut (fixé)
    # Post-hoc non paramétrique (Kruskal / RM) : méthode d'ajustement paramétrable
    needs_posthoc <- code %in% c("anova","anova2","kruskal","rm_aov")
    
    alt_ui <- if (needs_alt) {
      tagList(
        selectInput(
          inputId = "alternative",
          label = "Hypothèse alternative (direction du test)",
          choices = c(
            "Bilatéral (≠)" = "two.sided",
            "Unilatéral (>)" = "greater",
            "Unilatéral (<)" = "less"
          ),
          selected = "two.sided"
        ),
        helpText("Bilatéral si aucune direction n’est justifiée a priori ; unilatéral uniquement si l’hypothèse est directionnelle avant l’analyse.")
      )
    } else {
      tagList(helpText("Hypothèse alternative : non applicable pour ce test (test global ou diagnostic)."))
    }
    
    mu0_ui <- if (needs_mu0) {
      tagList(
        numericInput(
          inputId = "mu0",
          label = "Valeur de référence (μ0) pour test à 1 échantillon",
          value = 0,
          step = 0.1
        ),
        helpText("μ0 est la valeur théorique/de référence : norme, seuil clinique, cible attendue, etc.")
      )
    } else NULL
    
    posthoc_ui <- NULL
    if (needs_posthoc) {
      if (code %in% c("anova","anova2")) {
        posthoc_ui <- tagList(
          selectInput(
            inputId = "posthoc_method",
            label = "Post-hoc (ANOVA) — méthode",
            choices = c("Tukey HSD (par défaut)" = "tukey"),
            selected = "tukey"
          ),
          helpText("Pour l’ANOVA, l’analyse post-hoc est réalisée via Tukey HSD (comparaisons multiples contrôlées).")
        )
      } else {
        posthoc_ui <- tagList(
          selectInput(
            inputId = "p_adjust",
            label = "Ajustement des comparaisons multiples (post-hoc)",
            choices = c("Holm" = "holm", "Bonferroni" = "bonferroni", "BH (FDR)" = "BH"),
            selected = "holm"
          ),
          helpText("Le post-hoc compare les niveaux deux à deux après un test global. L’ajustement contrôle l’inflation du risque d’erreur de type I.")
        )
      }
    }
    
    tagList(
      numericInput(
        inputId = "alpha_test",
        label = "Seuil de significativité α",
        value = 0.05,
        min = 0.0001,
        max = 0.20,
        step = 0.001
      ),
      helpText("Décision basée sur p-value ≤ α et, si applicable, sur la valeur critique (région critique) cohérente avec l’alternative."),
      alt_ui,
      mu0_ui,
      posthoc_ui
    )
  })
  
  analysis <- eventReactive(input$run_test, {
    set.seed(123)
    
    df0 <- data_raw()
    validate(need(nrow(df0) > 0, "Aucune donnée importée."))
    
    opts <- test_options()
    validate(need(length(opts) > 0, "Aucun test disponible : vérifiez la sélection de variables."))
    
    alpha <- input$alpha_test
    if (is.null(alpha) || !is.finite(alpha)) alpha <- 0.05
    validate(need(is.finite(alpha) && alpha > 0 && alpha < 1, "Veuillez saisir un α valide (0 < α < 1)."))
    
    alt <- input$alternative
    if (is.null(alt) || !nzchar(alt)) alt <- "two.sided"
    if (!alt %in% c("two.sided", "greater", "less")) alt <- "two.sided"
    
    p_adj <- input$p_adjust
    if (is.null(p_adj) || !nzchar(p_adj)) p_adj <- "holm"
    
    posthoc_method <- input$posthoc_method
    if (is.null(posthoc_method) || !nzchar(posthoc_method)) posthoc_method <- "tukey"
    
    id <- input$analysis_test
    if (is.null(id) || !nzchar(id)) id <- selected_test_id()
    validate(need(!is.null(id) && nzchar(id), "Veuillez sélectionner un test dans la liste."))
    
    idx <- which(vapply(opts, function(o) o$id, character(1)) == id)
    validate(need(length(idx) > 0, "Le test sélectionné n’est plus disponible. Veuillez re-sélectionner un test."))
    opt <- opts[[idx[1]]]
    
    sel <- get_selected_df(df0, opt$vars)
    df <- sel$df
    n_initial <- sel$n_initial
    n_final <- sel$n_final
    exclusions <- sel$exclusions
    validate(need(n_final > 1, "Après suppression des NA sur les variables du test, l’échantillon est insuffisant."))
    
    types <- vapply(opt$vars, function(v) classify_var(df[[v]]), character(1))
    validate(need(!any(types %in% c("Date", "Autre")), "Variables Date/Autre non supportées pour l’exécution."))
    
    test_name <- opt$test_name
    definition_test <- NA_character_
    cas_usage <- NA_character_
    assumptions_lines <- character(0)
    hypotheses_math <- list(H0 = NA_character_, H1 = NA_character_)
    hypotheses_text <- list(H0 = NA_character_, H1 = NA_character_)
    critere <- NA_character_
    
    stat <- NA_real_
    df_txt <- NA_character_
    crit <- NA_character_
    pval <- NA_real_
    ci_txt <- NA_character_
    es_txt <- NA_character_
    decision <- NA_character_
    justification_p <- NA_character_
    justification_crit <- NA_character_
    
    posthoc_tbl <- NULL
    posthoc_label <- NULL
    
    plot_obj <- ggplot() + theme_minimal()
    plot_obj2 <- NULL
    
    vars_display <- paste(opt$vars, collapse = " ; ")
    types_display <- paste(paste0(opt$vars, " : ", types), collapse = " | ")
    
    # ---- Univarié (shapiro/qq/t1/wilcox1) ----
    if (opt$test_code %in% c("shapiro", "qq", "t1", "wilcox1")) {
      x <- df[[opt$vars[1]]]
      p_shap <- shapiro_safe(x)
      
      assumptions_lines <- c(
        "Indépendance : supposée (dépend du plan d’échantillonnage / collecte).",
        paste0("Normalité (Shapiro–Wilk) : p = ", fmt_p(p_shap), " → ",
               ifelse(is.na(p_shap), "non évaluée (n < 3 ou n > 5000).",
                      ifelse(p_shap > alpha, "normalité plausible (échec du rejet).", "normalité douteuse (rejet).")))
      )
      
      plot_obj <- ggplot(data.frame(x = x), aes(sample = x)) +
        stat_qq() + stat_qq_line() +
        labs(
          title = "QQ-plot (diagnostic de normalité)",
          subtitle = paste0("Variable : ", opt$vars[1]),
          x = "Quantiles théoriques",
          y = "Quantiles observés"
        ) +
        theme_minimal()
      
      if (opt$test_code %in% c("shapiro", "qq")) {
        definition_test <- "Le test de Shapiro–Wilk évalue l’hypothèse de normalité. Le QQ-plot complète l’analyse par une vérification graphique."
        cas_usage <- "Variable numérique ; objectif : apprécier la plausibilité de la normalité afin de justifier l’usage de méthodes paramétriques."
        stat <- NA_real_
        df_txt <- "dl = —"
        crit <- "Non applicable (approche par p-value)"
        pval <- p_shap
        ci_txt <- "Non disponible"
        es_txt <- "Non applicable"
        decision <- if (!is.na(pval) && pval <= alpha) "Rejet de H0" else "Échec du rejet de H0"
        justification_p <- paste0("p-value = ", fmt_p(pval), " ", ifelse(!is.na(pval) && pval <= alpha, "≤", ">"), " α = ", fmt_num(alpha, 4))
        justification_crit <- "Valeur critique : non applicable (décision fondée sur la p-value)"
        
        hypotheses_math$H0 <- "\\(H_0: X \\sim \\mathcal{N}(\\mu,\\sigma^2)\\)"
        hypotheses_math$H1 <- "\\(H_1: X \\not\\sim \\mathcal{N}(\\mu,\\sigma^2)\\)"
        hypotheses_text$H0 <- "H0 : la distribution est compatible avec la normalité."
        hypotheses_text$H1 <- "H1 : la distribution n’est pas compatible avec la normalité."
        
        critere <- paste0(
          "Au seuil \\(\\alpha = ", fmt_num(alpha, 4), "\\), on rejette \\(H_0\\) si \\(p \\leq \\alpha\\). ",
          "Région critique : \\(p \\leq \\alpha\\)."
        )
      }
      
      if (opt$test_code == "t1") {
        mu0 <- input$mu0
        if (is.null(mu0) || !is.finite(mu0)) mu0 <- 0
        
        t_p <- stats::t.test(x, mu = mu0, alternative = alt, conf.level = 0.95)
        t_ci <- stats::t.test(x, mu = mu0, alternative = "two.sided", conf.level = 0.95)
        
        definition_test <- "Le test t à 1 échantillon compare la moyenne observée à une valeur de référence \\(\\mu_0\\)."
        cas_usage <- "Variable numérique (1 échantillon) ; objectif : tester si \\(\\mu\\) diffère (ou est supérieure/inférieure) à \\(\\mu_0\\)."
        
        stat <- unname(t_p$statistic)
        pval <- unname(t_p$p.value)
        dfv <- unname(t_p$parameter)
        df_txt <- paste0("dl = ", dfv)
        
        if (alt == "two.sided") {
          tcrit <- stats::qt(1 - alpha/2, df = dfv)
          crit <- paste0("t* = ±", fmt_num(tcrit, 4))
          rej <- (abs(stat) >= tcrit)
          justification_crit <- paste0("|t| = ", fmt_num(abs(stat), 4), " ", ifelse(rej, "≥", "<"), " t* = ", fmt_num(tcrit, 4))
          region <- "\\(|t| \\geq t^*\\)"
        } else if (alt == "greater") {
          tcrit <- stats::qt(1 - alpha, df = dfv)
          crit <- paste0("t* = ", fmt_num(tcrit, 4))
          rej <- (stat >= tcrit)
          justification_crit <- paste0("t = ", fmt_num(stat, 4), " ", ifelse(rej, "≥", "<"), " t* = ", fmt_num(tcrit, 4))
          region <- "\\(t \\geq t^*\\)"
        } else {
          tcrit <- stats::qt(alpha, df = dfv)
          crit <- paste0("t* = ", fmt_num(tcrit, 4))
          rej <- (stat <= tcrit)
          justification_crit <- paste0("t = ", fmt_num(stat, 4), " ", ifelse(rej, "≤", ">"), " t* = ", fmt_num(tcrit, 4))
          region <- "\\(t \\leq t^*\\)"
        }
        
        ci_txt <- paste0("[", fmt_num(t_ci$conf.int[1], 4), " ; ", fmt_num(t_ci$conf.int[2], 4), "]")
        d <- (mean(x, na.rm = TRUE) - mu0) / stats::sd(x, na.rm = TRUE)
        es_txt <- paste0("Cohen d = ", fmt_num(d, 4))
        
        decision <- if (pval <= alpha) "Rejet de H0" else "Échec du rejet de H0"
        justification_p <- paste0("p-value = ", fmt_p(pval), " ", ifelse(pval <= alpha, "≤", ">"), " α = ", fmt_num(alpha, 4))
        
        sym <- alt_symbol(alt)
        hypotheses_math$H0 <- paste0("\\(H_0: \\mu = ", fmt_num(mu0, 4), "\\)")
        hypotheses_math$H1 <- paste0("\\(H_1: \\mu ", sym, " ", fmt_num(mu0, 4), "\\)")
        hypotheses_text$H0 <- paste0("H0 : la moyenne populationnelle est égale à ", fmt_num(mu0, 4), ".")
        hypotheses_text$H1 <- paste0("H1 : la moyenne populationnelle est ", if (alt=="two.sided") "différente de " else if (alt=="greater") "supérieure à " else "inférieure à ", fmt_num(mu0, 4), ".")
        
        critere <- paste0(
          "Au seuil \\(\\alpha = ", fmt_num(alpha, 4), "\\), on rejette \\(H_0\\) si \\(p \\leq \\alpha\\) ",
          "ou si ", region, " (avec \\(t^*\\) issu de la loi t de Student). ",
          "Région critique : ", region, "."
        )
      }
      
      if (opt$test_code == "wilcox1") {
        mu0 <- input$mu0
        if (is.null(mu0) || !is.finite(mu0)) mu0 <- 0
        
        w_p <- tryCatch(
          stats::wilcox.test(x, mu = mu0, alternative = alt, conf.int = TRUE, conf.level = 0.95, exact = FALSE),
          error = function(e) stats::wilcox.test(x, mu = mu0, alternative = alt, exact = FALSE)
        )
        w_ci <- tryCatch(
          stats::wilcox.test(x, mu = mu0, alternative = "two.sided", conf.int = TRUE, conf.level = 0.95, exact = FALSE),
          error = function(e) NULL
        )
        
        definition_test <- "Le test de Wilcoxon à 1 échantillon compare une position centrale à une valeur de référence sans supposer la normalité."
        cas_usage <- "Variable numérique (1 échantillon) ; objectif : tester un décalage de localisation par rapport à \\(\\mu_0\\) (bilatéral ou unilatéral)."
        
        stat <- unname(w_p$statistic)
        pval <- unname(w_p$p.value)
        df_txt <- "dl = —"
        crit <- "Non applicable (approche par p-value)"
        ci_src <- if (!is.null(w_ci) && !is.null(w_ci$conf.int)) w_ci$conf.int else if (!is.null(w_p$conf.int)) w_p$conf.int else NULL
        ci_txt <- if (!is.null(ci_src)) paste0("[", fmt_num(ci_src[1], 4), " ; ", fmt_num(ci_src[2], 4), "]") else "Non disponible"
        
        p_use <- clamp_p(pval)
        if (!is.na(p_use)) {
          z_approx <- if (alt == "two.sided") stats::qnorm(p_use/2, lower.tail = FALSE) else stats::qnorm(p_use, lower.tail = FALSE)
          r_approx <- z_approx / sqrt(n_final)
        } else r_approx <- NA_real_
        es_txt <- paste0("r (approx.) = ", fmt_num(r_approx, 4))
        
        decision <- if (pval <= alpha) "Rejet de H0" else "Échec du rejet de H0"
        justification_p <- paste0("p-value = ", fmt_p(pval), " ", ifelse(pval <= alpha, "≤", ">"), " α = ", fmt_num(alpha, 4))
        justification_crit <- "Valeur critique : non applicable (décision fondée sur la p-value)"
        
        sym <- alt_symbol(alt)
        hypotheses_math$H0 <- paste0("\\(H_0: \\theta = ", fmt_num(mu0, 4), "\\)")
        hypotheses_math$H1 <- paste0("\\(H_1: \\theta ", sym, " ", fmt_num(mu0, 4), "\\)")
        hypotheses_text$H0 <- paste0("H0 : la position centrale est égale à ", fmt_num(mu0, 4), ".")
        hypotheses_text$H1 <- paste0("H1 : la position centrale est ", if (alt=="two.sided") "différente de " else if (alt=="greater") "supérieure à " else "inférieure à ", fmt_num(mu0, 4), ".")
        
        critere <- paste0(
          "Au seuil \\(\\alpha = ", fmt_num(alpha, 4), "\\), on rejette \\(H_0\\) si \\(p \\leq \\alpha\\). ",
          "Région critique : \\(p \\leq \\alpha\\)."
        )
      }
    }
    
    # ---- Corrélations ----
    if (opt$test_code %in% c("pearson", "spearman")) {
      x <- df[[opt$vars[1]]]
      y <- df[[opt$vars[2]]]
      
      p_shap_x <- shapiro_safe(x)
      p_shap_y <- shapiro_safe(y)
      
      assumptions_lines <- c(
        "Indépendance : supposée (dépend du plan d’étude).",
        paste0("Normalité (", opt$vars[1], ") : Shapiro–Wilk p = ", fmt_p(p_shap_x), " → ",
               ifelse(is.na(p_shap_x), "non évaluée.", ifelse(p_shap_x > alpha, "plausible.", "douteuse."))),
        paste0("Normalité (", opt$vars[2], ") : Shapiro–Wilk p = ", fmt_p(p_shap_y), " → ",
               ifelse(is.na(p_shap_y), "non évaluée.", ifelse(p_shap_y > alpha, "plausible.", "douteuse."))),
        "Linéarité/monotonie : appréciée via le nuage de points."
      )
      
      method <- if (opt$test_code == "pearson") "pearson" else "spearman"
      c_p <- stats::cor.test(x, y, method = method, alternative = alt, conf.level = 0.95)
      c_ci <- stats::cor.test(x, y, method = method, alternative = "two.sided", conf.level = 0.95)
      
      definition_test <- if (method == "pearson") {
        "La corrélation de Pearson mesure l’intensité et le sens d’une association linéaire."
      } else {
        "La corrélation de Spearman mesure l’intensité et le sens d’une association monotone (rangs)."
      }
      cas_usage <- "Deux variables numériques ; objectif : tester une association (bilatérale ou directionnelle)."
      
      stat <- unname(c_p$statistic)
      pval <- unname(c_p$p.value)
      dfv <- if (!is.null(c_p$parameter)) unname(c_p$parameter) else (n_final - 2)
      df_txt <- paste0("dl = ", dfv)
      
      if (method == "pearson") {
        if (alt == "two.sided") {
          tcrit <- stats::qt(1 - alpha/2, df = dfv)
          crit <- paste0("t* = ±", fmt_num(tcrit, 4))
          rej <- (abs(stat) >= tcrit)
          justification_crit <- paste0("|t| = ", fmt_num(abs(stat), 4), " ", ifelse(rej, "≥", "<"), " t* = ", fmt_num(tcrit, 4))
          region <- "\\(|t| \\geq t^*\\)"
        } else if (alt == "greater") {
          tcrit <- stats::qt(1 - alpha, df = dfv)
          crit <- paste0("t* = ", fmt_num(tcrit, 4))
          rej <- (stat >= tcrit)
          justification_crit <- paste0("t = ", fmt_num(stat, 4), " ", ifelse(rej, "≥", "<"), " t* = ", fmt_num(tcrit, 4))
          region <- "\\(t \\geq t^*\\)"
        } else {
          tcrit <- stats::qt(alpha, df = dfv)
          crit <- paste0("t* = ", fmt_num(tcrit, 4))
          rej <- (stat <= tcrit)
          justification_crit <- paste0("t = ", fmt_num(stat, 4), " ", ifelse(rej, "≤", ">"), " t* = ", fmt_num(tcrit, 4))
          region <- "\\(t \\leq t^*\\)"
        }
      } else {
        crit <- "Non applicable (approche par p-value)"
        justification_crit <- "Valeur critique : non applicable (décision fondée sur la p-value)"
      }
      
      ci_txt <- if (!is.null(c_ci$conf.int)) {
        paste0("[", fmt_num(c_ci$conf.int[1], 4), " ; ", fmt_num(c_ci$conf.int[2], 4), "]")
      } else "Non disponible"
      
      est <- unname(c_p$estimate)
      es_txt <- paste0(if (method == "pearson") "r = " else "ρ = ", fmt_num(est, 4))
      
      decision <- if (pval <= alpha) "Rejet de H0" else "Échec du rejet de H0"
      justification_p <- paste0("p-value = ", fmt_p(pval), " ", ifelse(pval <= alpha, "≤", ">"), " α = ", fmt_num(alpha, 4))
      
      sym <- alt_symbol(alt)
      hypotheses_math$H0 <- "\\(H_0: \\rho = 0\\)"
      hypotheses_math$H1 <- paste0("\\(H_1: \\rho ", sym, " 0\\)")
      hypotheses_text$H0 <- "H0 : corrélation populationnelle nulle."
      hypotheses_text$H1 <- paste0("H1 : corrélation populationnelle ", if (alt=="two.sided") "non nulle." else if (alt=="greater") "positive." else "négative.")
      
      critere <- paste0(
        "Au seuil \\(\\alpha = ", fmt_num(alpha, 4), "\\), on rejette \\(H_0\\) si \\(p \\leq \\alpha\\)."
      )
      
      plot_obj <- ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
        geom_point() +
        geom_smooth(method = "lm", se = TRUE) +
        labs(
          title = "Nuage de points et droite de régression",
          subtitle = paste0(opt$vars[1], " (x) vs ", opt$vars[2], " (y)"),
          x = opt$vars[1],
          y = opt$vars[2]
        ) +
        theme_minimal()
    }
    
    # ---- Tests appariés ----
    if (opt$test_code %in% c("tpaired", "wilcoxpaired")) {
      x <- df[[opt$vars[1]]]
      y <- df[[opt$vars[2]]]
      validate(need(length(x) == length(y), "Les deux variables doivent avoir la même longueur (paires)."))
      
      d <- x - y
      p_shap_d <- shapiro_safe(d)
      assumptions_lines <- c(
        "Appariement : supposé (mêmes sujets/mesures répétées).",
        paste0("Normalité des différences (Shapiro–Wilk) : p = ", fmt_p(p_shap_d), " → ",
               ifelse(is.na(p_shap_d), "non évaluée.", ifelse(p_shap_d > alpha, "plausible.", "douteuse.")))
      )
      
      plot_obj <- ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
        geom_point() +
        geom_abline() +
        labs(
          title = "Nuage de points (appariement)",
          subtitle = paste0(opt$vars[1], " vs ", opt$vars[2], " (référence y = x)"),
          x = opt$vars[1],
          y = opt$vars[2]
        ) +
        theme_minimal()
      
      if (opt$test_code == "tpaired") {
        tt <- stats::t.test(x, y, paired = TRUE, alternative = alt, conf.level = 0.95)
        tt_ci <- stats::t.test(x, y, paired = TRUE, alternative = "two.sided", conf.level = 0.95)
        
        definition_test <- "Le test t apparié compare deux mesures liées (mêmes sujets) via la moyenne des différences \\(d = X - Y\\)."
        cas_usage <- "Deux variables numériques appariées ; objectif : tester si \\(\\mu_d\\) diffère de 0 (bilatéral/unilatéral)."
        
        stat <- unname(tt$statistic)
        pval <- unname(tt$p.value)
        dfv <- unname(tt$parameter)
        df_txt <- paste0("dl = ", dfv)
        
        if (alt == "two.sided") {
          tcrit <- stats::qt(1 - alpha/2, df = dfv)
          crit <- paste0("t* = ±", fmt_num(tcrit, 4))
          rej <- (abs(stat) >= tcrit)
          justification_crit <- paste0("|t| = ", fmt_num(abs(stat), 4), " ", ifelse(rej, "≥", "<"), " t* = ", fmt_num(tcrit, 4))
          region <- "\\(|t| \\geq t^*\\)"
        } else if (alt == "greater") {
          tcrit <- stats::qt(1 - alpha, df = dfv)
          crit <- paste0("t* = ", fmt_num(tcrit, 4))
          rej <- (stat >= tcrit)
          justification_crit <- paste0("t = ", fmt_num(stat, 4), " ", ifelse(rej, "≥", "<"), " t* = ", fmt_num(tcrit, 4))
          region <- "\\(t \\geq t^*\\)"
        } else {
          tcrit <- stats::qt(alpha, df = dfv)
          crit <- paste0("t* = ", fmt_num(tcrit, 4))
          rej <- (stat <= tcrit)
          justification_crit <- paste0("t = ", fmt_num(stat, 4), " ", ifelse(rej, "≤", ">"), " t* = ", fmt_num(tcrit, 4))
          region <- "\\(t \\leq t^*\\)"
        }
        
        ci_txt <- paste0("[", fmt_num(tt_ci$conf.int[1], 4), " ; ", fmt_num(tt_ci$conf.int[2], 4), "]")
        es_txt <- paste0("Cohen d (apparié) = ", fmt_num(cohen_d_paired(x, y), 4))
        
        decision <- if (pval <= alpha) "Rejet de H0" else "Échec du rejet de H0"
        justification_p <- paste0("p-value = ", fmt_p(pval), " ", ifelse(pval <= alpha, "≤", ">"), " α = ", fmt_num(alpha, 4))
        justification_crit <- justification_crit
        
        sym <- alt_symbol(alt)
        hypotheses_math$H0 <- "\\(H_0: \\mu_d = 0\\)"
        hypotheses_math$H1 <- paste0("\\(H_1: \\mu_d ", sym, " 0\\)")
        hypotheses_text$H0 <- "H0 : la moyenne des différences est nulle."
        hypotheses_text$H1 <- paste0("H1 : la moyenne des différences est ", if (alt=="two.sided") "non nulle." else if (alt=="greater") "positive." else "négative.")
        
        critere <- paste0(
          "Au seuil \\(\\alpha = ", fmt_num(alpha, 4), "\\), on rejette \\(H_0\\) si \\(p \\leq \\alpha\\) ou si ", region, "."
        )
      }
      
      if (opt$test_code == "wilcoxpaired") {
        wt <- tryCatch(
          stats::wilcox.test(x, y, paired = TRUE, alternative = alt, conf.int = TRUE, conf.level = 0.95, exact = FALSE),
          error = function(e) stats::wilcox.test(x, y, paired = TRUE, alternative = alt, exact = FALSE)
        )
        wt_ci <- tryCatch(
          stats::wilcox.test(x, y, paired = TRUE, alternative = "two.sided", conf.int = TRUE, conf.level = 0.95, exact = FALSE),
          error = function(e) NULL
        )
        
        definition_test <- "Le test de Wilcoxon apparié compare deux mesures liées sans supposer la normalité des différences."
        cas_usage <- "Deux variables numériques appariées ; comparaison non paramétrique des différences."
        
        stat <- unname(wt$statistic)
        pval <- unname(wt$p.value)
        df_txt <- "dl = —"
        crit <- "Non applicable (approche par p-value)"
        ci_src <- if (!is.null(wt_ci) && !is.null(wt_ci$conf.int)) wt_ci$conf.int else if (!is.null(wt$conf.int)) wt$conf.int else NULL
        ci_txt <- if (!is.null(ci_src)) paste0("[", fmt_num(ci_src[1], 4), " ; ", fmt_num(ci_src[2], 4), "]") else "Non disponible"
        
        p_use <- clamp_p(pval)
        if (!is.na(p_use)) {
          z_approx <- if (alt == "two.sided") stats::qnorm(p_use/2, lower.tail = FALSE) else stats::qnorm(p_use, lower.tail = FALSE)
          r_approx <- z_approx / sqrt(n_final)
        } else r_approx <- NA_real_
        es_txt <- paste0("r (approx.) = ", fmt_num(r_approx, 4))
        
        decision <- if (pval <= alpha) "Rejet de H0" else "Échec du rejet de H0"
        justification_p <- paste0("p-value = ", fmt_p(pval), " ", ifelse(pval <= alpha, "≤", ">"), " α = ", fmt_num(alpha, 4))
        justification_crit <- "Valeur critique : non applicable (décision fondée sur la p-value)"
        
        sym <- alt_symbol(alt)
        hypotheses_math$H0 <- "\\(H_0: \\mu_d = 0\\)"
        hypotheses_math$H1 <- paste0("\\(H_1: \\mu_d ", sym, " 0\\)")
        hypotheses_text$H0 <- "H0 : la moyenne des différences est nulle."
        hypotheses_text$H1 <- paste0("H1 : la moyenne des différences est ", if (alt=="two.sided") "non nulle." else if (alt=="greater") "positive." else "négative.")
        
        critere <- paste0("Au seuil \\(\\alpha = ", fmt_num(alpha, 4), "\\), on rejette \\(H_0\\) si \\(p \\leq \\alpha\\).")
      }
    }
    
    # ---- Numérique ~ facteur (1 facteur) ----
    if (opt$test_code %in% c("tind","wilcoxind","anova","kruskal")) {
      yv <- opt$vars[1]
      gv <- opt$vars[2]
      y <- df[[yv]]
      g <- droplevels(as.factor(df[[gv]]))
      k <- length(levels(g))
      validate(need(k >= 2, "La variable catégorielle doit avoir au moins 2 niveaux après suppression des NA."))
      
      plot_obj <- ggplot(data.frame(g = g, y = y), aes(x = g, y = y)) +
        geom_boxplot() +
        labs(
          title = "Boxplot par groupe",
          subtitle = paste0("Variable numérique : ", yv, " | Groupe : ", gv),
          x = gv,
          y = yv
        ) +
        theme_minimal()
      
      group_levels <- levels(g)
      p_shap_groups <- vapply(group_levels, function(lv) shapiro_safe(y[g == lv]), numeric(1))
      p_var_f <- tryCatch(stats::var.test(y ~ g)$p.value, error = function(e) NA_real_)
      p_bart <- tryCatch(stats::bartlett.test(y ~ g)$p.value, error = function(e) NA_real_)
      
      assumptions_lines <- c(
        "Indépendance : supposée (groupes indépendants).",
        paste0("Normalité par groupe (Shapiro–Wilk) : ",
               paste0(group_levels, ": p=", vapply(p_shap_groups, fmt_p, character(1)), collapse = " ; "), "."),
        if (k == 2) paste0("Homogénéité des variances (F) : p = ", fmt_p(p_var_f), ".") else paste0("Homogénéité des variances (Bartlett) : p = ", fmt_p(p_bart), ".")
      )
      
      if (opt$test_code == "tind") {
        validate(need(k == 2, "Ce test requiert une variable catégorielle à 2 niveaux."))
        
        t_p <- stats::t.test(y ~ g, alternative = alt, conf.level = 0.95)
        t_ci <- stats::t.test(y ~ g, alternative = "two.sided", conf.level = 0.95)
        
        definition_test <- "Le test t indépendant (Welch) compare les moyennes de deux groupes indépendants."
        cas_usage <- "Numérique + facteur à 2 niveaux ; objectif : comparer \\(\\mu_1\\) et \\(\\mu_2\\) (bilatéral ou unilatéral)."
        
        stat <- unname(t_p$statistic)
        pval <- unname(t_p$p.value)
        dfv <- unname(t_p$parameter)
        df_txt <- paste0("dl = ", fmt_num(dfv, 4))
        
        if (alt == "two.sided") {
          tcrit <- stats::qt(1 - alpha/2, df = dfv)
          crit <- paste0("t* = ±", fmt_num(tcrit, 4))
          rej <- (abs(stat) >= tcrit)
          justification_crit <- paste0("|t| = ", fmt_num(abs(stat), 4), " ", ifelse(rej, "≥", "<"), " t* = ", fmt_num(tcrit, 4))
          region <- "\\(|t| \\geq t^*\\)"
        } else if (alt == "greater") {
          tcrit <- stats::qt(1 - alpha, df = dfv)
          crit <- paste0("t* = ", fmt_num(tcrit, 4))
          rej <- (stat >= tcrit)
          justification_crit <- paste0("t = ", fmt_num(stat, 4), " ", ifelse(rej, "≥", "<"), " t* = ", fmt_num(tcrit, 4))
          region <- "\\(t \\geq t^*\\)"
        } else {
          tcrit <- stats::qt(alpha, df = dfv)
          crit <- paste0("t* = ", fmt_num(tcrit, 4))
          rej <- (stat <= tcrit)
          justification_crit <- paste0("t = ", fmt_num(stat, 4), " ", ifelse(rej, "≤", ">"), " t* = ", fmt_num(tcrit, 4))
          region <- "\\(t \\leq t^*\\)"
        }
        
        ci_txt <- paste0("[", fmt_num(t_ci$conf.int[1], 4), " ; ", fmt_num(t_ci$conf.int[2], 4), "]")
        es_txt <- paste0("Cohen d = ", fmt_num(cohen_d_ind(y, g), 4))
        
        decision <- if (pval <= alpha) "Rejet de H0" else "Échec du rejet de H0"
        justification_p <- paste0("p-value = ", fmt_p(pval), " ", ifelse(pval <= alpha, "≤", ">"), " α = ", fmt_num(alpha, 4))
        
        lev <- levels(g)
        sym <- alt_symbol(alt)
        hypotheses_math$H0 <- "\\(H_0: \\mu_1 = \\mu_2\\)"
        hypotheses_math$H1 <- paste0("\\(H_1: \\mu_1 ", sym, " \\mu_2\\)")
        hypotheses_text$H0 <- paste0("H0 : moyennes populationnelles égales (", lev[1], " vs ", lev[2], ").")
        hypotheses_text$H1 <- paste0(
          "H1 : moyenne du groupe ", lev[1], " ",
          if (alt=="two.sided") "différente de" else if (alt=="greater") "supérieure à" else "inférieure à",
          " celle du groupe ", lev[2], "."
        )
        
        critere <- paste0(
          "Au seuil \\(\\alpha = ", fmt_num(alpha, 4), "\\), on rejette \\(H_0\\) si \\(p \\leq \\alpha\\) ou si ", region, "."
        )
      }
      
      if (opt$test_code == "wilcoxind") {
        validate(need(k == 2, "Ce test requiert une variable catégorielle à 2 niveaux."))
        
        w_p <- tryCatch(
          stats::wilcox.test(y ~ g, alternative = alt, conf.int = TRUE, conf.level = 0.95, exact = FALSE),
          error = function(e) stats::wilcox.test(y ~ g, alternative = alt, exact = FALSE)
        )
        w_ci <- tryCatch(
          stats::wilcox.test(y ~ g, alternative = "two.sided", conf.int = TRUE, conf.level = 0.95, exact = FALSE),
          error = function(e) NULL
        )
        
        definition_test <- "Le test de Wilcoxon (Mann–Whitney) compare deux groupes indépendants sans supposer la normalité."
        cas_usage <- "Numérique + facteur à 2 niveaux ; comparaison non paramétrique (bilatéral ou unilatéral)."
        
        stat <- unname(w_p$statistic)
        pval <- unname(w_p$p.value)
        df_txt <- "dl = —"
        crit <- "Non applicable (approche par p-value)"
        ci_src <- if (!is.null(w_ci) && !is.null(w_ci$conf.int)) w_ci$conf.int else if (!is.null(w_p$conf.int)) w_p$conf.int else NULL
        ci_txt <- if (!is.null(ci_src)) paste0("[", fmt_num(ci_src[1], 4), " ; ", fmt_num(ci_src[2], 4), "]") else "Non disponible"
        
        p_use <- clamp_p(pval)
        if (!is.na(p_use)) {
          z_approx <- if (alt == "two.sided") stats::qnorm(p_use/2, lower.tail = FALSE) else stats::qnorm(p_use, lower.tail = FALSE)
          r_approx <- z_approx / sqrt(n_final)
        } else r_approx <- NA_real_
        es_txt <- paste0("r (approx.) = ", fmt_num(r_approx, 4))
        
        decision <- if (pval <= alpha) "Rejet de H0" else "Échec du rejet de H0"
        justification_p <- paste0("p-value = ", fmt_p(pval), " ", ifelse(pval <= alpha, "≤", ">"), " α = ", fmt_num(alpha, 4))
        justification_crit <- "Valeur critique : non applicable (décision fondée sur la p-value)"
        
        lev <- levels(g)
        sym <- alt_symbol(alt)
        hypotheses_math$H0 <- "\\(H_0: \\theta_1 = \\theta_2\\)"
        hypotheses_math$H1 <- paste0("\\(H_1: \\theta_1 ", sym, " \\theta_2\\)")
        hypotheses_text$H0 <- paste0("H0 : positions centrales identiques (", lev[1], " vs ", lev[2], ").")
        hypotheses_text$H1 <- paste0(
          "H1 : position centrale du groupe ", lev[1], " ",
          if (alt=="two.sided") "différente de" else if (alt=="greater") "supérieure à" else "inférieure à",
          " celle du groupe ", lev[2], "."
        )
        
        critere <- paste0("Au seuil \\(\\alpha = ", fmt_num(alpha, 4), "\\), on rejette \\(H_0\\) si \\(p \\leq \\alpha\\).")
      }
      
      if (opt$test_code == "anova") {
        validate(need(k >= 3, "Ce test requiert une variable catégorielle avec ≥ 3 niveaux."))
        
        aov_fit <- stats::aov(y ~ g)
        s <- summary(aov_fit)[[1]]
        
        definition_test <- "L’ANOVA à un facteur compare les moyennes de plusieurs groupes et teste si au moins une moyenne diffère."
        cas_usage <- "Numérique + facteur (≥ 3 niveaux) ; objectif : tester \\(H_0: \\mu_1=\\dots=\\mu_k\\)."
        
        stat <- unname(s[["F value"]][1])
        df1 <- unname(s[["Df"]][1])
        df2 <- unname(s[["Df"]][2])
        df_txt <- paste0("dl1 = ", df1, " ; dl2 = ", df2)
        pval <- unname(s[["Pr(>F)"]][1])
        
        fcrit <- stats::qf(1 - alpha, df1 = df1, df2 = df2)
        crit <- paste0("F* = ", fmt_num(fcrit, 4))
        ci_txt <- "Non disponible (ANOVA globale)"
        eta2p <- partial_eta_sq_from_aovtab(s, rownames(s)[1])
        es_txt <- paste0("η²p (approx.) = ", fmt_num(eta2p, 4))
        
        rej <- (stat >= fcrit)
        decision <- if (pval <= alpha) "Rejet de H0" else "Échec du rejet de H0"
        justification_p <- paste0("p-value = ", fmt_p(pval), " ", ifelse(pval <= alpha, "≤", ">"), " α = ", fmt_num(alpha, 4))
        justification_crit <- paste0("F = ", fmt_num(stat, 4), " ", ifelse(rej, "≥", "<"), " F* = ", fmt_num(fcrit, 4))
        
        hypotheses_math$H0 <- "\\(H_0: \\mu_1 = \\mu_2 = \\dots = \\mu_k\\)"
        hypotheses_math$H1 <- "\\(H_1: \\exists\\ i\\neq j,\\ \\mu_i \\neq \\mu_j\\)"
        hypotheses_text$H0 <- "H0 : toutes les moyennes populationnelles sont égales."
        hypotheses_text$H1 <- "H1 : au moins une moyenne diffère."
        
        critere <- paste0(
          "Au seuil \\(\\alpha = ", fmt_num(alpha, 4), "\\), on rejette \\(H_0\\) si \\(p \\leq \\alpha\\) ",
          "ou si \\(F \\geq F^*\\). Région critique : \\(F \\geq F^*\\)."
        )
        
        # Post-hoc ANOVA : Tukey HSD (par défaut, fixé)
        tuk <- tryCatch(TukeyHSD(aov_fit), error = function(e) NULL)
        if (!is.null(tuk)) {
          th <- as.data.frame(tuk[[1]])
          th$Comparaison <- rownames(th)
          rownames(th) <- NULL
          posthoc_tbl <- th[, c("Comparaison","diff","lwr","upr","p adj")]
          names(posthoc_tbl) <- c("Comparaison","Différence","IC inf","IC sup","p ajustée")
          posthoc_label <- "🔔 Post-hoc (Tukey HSD)"
        }
      }
      
      if (opt$test_code == "kruskal") {
        validate(need(k >= 3, "Ce test requiert une variable catégorielle avec ≥ 3 niveaux."))
        
        k_res <- stats::kruskal.test(y ~ g)
        
        definition_test <- "Le test de Kruskal–Wallis compare plusieurs groupes sans supposer la normalité, à partir des rangs."
        cas_usage <- "Numérique + facteur (≥ 3 niveaux) ; objectif : comparer des distributions/positions entre groupes."
        
        stat <- unname(k_res$statistic)
        pval <- unname(k_res$p.value)
        df_txt <- paste0("dl = ", unname(k_res$parameter))
        crit <- "Non applicable (approche par p-value)"
        ci_txt <- "Non disponible"
        es_txt <- "Effet global : à interpréter via les différences de rangs (post-hoc)."
        
        decision <- if (pval <= alpha) "Rejet de H0" else "Échec du rejet de H0"
        justification_p <- paste0("p-value = ", fmt_p(pval), " ", ifelse(pval <= alpha, "≤", ">"), " α = ", fmt_num(alpha, 4))
        justification_crit <- "Valeur critique : non applicable (décision fondée sur la p-value)"
        
        hypotheses_math$H0 <- "\\(H_0: \\text{distributions identiques entre groupes}\\)"
        hypotheses_math$H1 <- "\\(H_1: \\text{au moins un groupe diffère}\\)"
        hypotheses_text$H0 <- "H0 : distributions identiques."
        hypotheses_text$H1 <- "H1 : au moins une différence entre groupes."
        
        critere <- paste0("Au seuil \\(\\alpha = ", fmt_num(alpha, 4), "\\), on rejette \\(H_0\\) si \\(p \\leq \\alpha\\).")
        
        pw <- tryCatch(stats::pairwise.wilcox.test(y, g, p.adjust.method = p_adj), error = function(e) NULL)
        if (!is.null(pw)) {
          m <- pw$p.value
          ph <- as.data.frame(as.table(m))
          ph <- ph[!is.na(ph$Freq), ]
          names(ph) <- c("Groupe 1","Groupe 2","p ajustée")
          posthoc_tbl <- ph
          posthoc_label <- paste0("Post-hoc (pairwise Wilcoxon, ajustement = ", p_adj, ")")
        }
      }
    }
    
    # ---- ANOVA à deux facteurs ----
    if (opt$test_code == "anova2") {
      y <- df[[opt$vars[1]]]
      A <- droplevels(as.factor(df[[opt$vars[2]]]))
      B <- droplevels(as.factor(df[[opt$vars[3]]]))
      validate(need(length(levels(A)) >= 2 && length(levels(B)) >= 2, "Les deux facteurs doivent avoir ≥ 2 niveaux après suppression des NA."))
      
      dat <- data.frame(y = y, A = A, B = B)
      
      aov_fit <- stats::aov(y ~ A * B, data = dat)
      s <- summary(aov_fit)[[1]]
      
      definition_test <- "L’ANOVA à deux facteurs teste les effets principaux de deux facteurs et leur interaction sur une variable numérique."
      cas_usage <- "Numérique + 2 facteurs ; objectif : tester effets A, B et interaction A×B."
      
      pA <- s["A", "Pr(>F)"]
      pB <- s["B", "Pr(>F)"]
      pI <- s["A:B", "Pr(>F)"]
      
      stat <- s["A:B", "F value"]
      pval <- pI
      df1 <- s["A:B", "Df"]
      df2 <- s["Residuals", "Df"]
      df_txt <- paste0("dl1 = ", df1, " ; dl2 = ", df2)
      
      fcrit <- stats::qf(1 - alpha, df1 = df1, df2 = df2)
      crit <- paste0("F* (interaction) = ", fmt_num(fcrit, 4))
      ci_txt <- "Non disponible (ANOVA factorielle)"
      eta2p_int <- partial_eta_sq_from_aovtab(s, "A:B")
      es_txt <- paste0("η²p (interaction, approx.) = ", fmt_num(eta2p_int, 4))
      
      decision <- if (pval <= alpha) "Rejet de H0" else "Échec du rejet de H0"
      justification_p <- paste0("p(interaction) = ", fmt_p(pval), " ", ifelse(pval <= alpha, "≤", ">"), " α = ", fmt_num(alpha, 4))
      justification_crit <- paste0("F(interaction) = ", fmt_num(stat, 4), " (réf. F*)")
      
      hypotheses_math$H0 <- "\\(H_0: \\text{pas d'effet (facteur A), pas d'effet (facteur B), pas d'interaction}\\)"
      hypotheses_math$H1 <- "\\(H_1: \\text{au moins un effet principal ou une interaction existe}\\)"
      hypotheses_text$H0 <- "H0 : pas d'effet des facteurs et pas d'interaction."
      hypotheses_text$H1 <- "H1 : au moins un effet principal ou une interaction."
      
      critere <- paste0(
        "Au seuil \\(\\alpha = ", fmt_num(alpha, 4), "\\), chaque effet (A, B, A×B) est jugé significatif si \\(p \\leq \\alpha\\)."
      )
      
      plot_obj <- ggplot(dat, aes(x = A, y = y, fill = B)) +
        geom_boxplot() +
        labs(
          title = "ANOVA à deux facteurs — Boxplots par interaction",
          subtitle = paste0("A = ", opt$vars[2], " ; B = ", opt$vars[3]),
          x = opt$vars[2],
          y = opt$vars[1],
          fill = opt$vars[3]
        ) +
        theme_minimal()
      
      plot_obj2 <- ggplot(dat, aes(x = A, y = y, group = B, color = B)) +
        stat_summary(fun = mean, geom = "line") +
        stat_summary(fun = mean, geom = "point") +
        labs(
          title = "ANOVA à deux facteurs — Moyennes (interaction)",
          subtitle = paste0("Lignes = ", opt$vars[3]),
          x = opt$vars[2],
          y = paste0("Moyenne de ", opt$vars[1]),
          color = opt$vars[3]
        ) +
        theme_minimal()
      
      tuk <- tryCatch(TukeyHSD(aov_fit), error = function(e) NULL)
      if (!is.null(tuk)) {
        out <- list()
        if ("A" %in% names(tuk)) {
          thA <- as.data.frame(tuk[["A"]]); thA$Effet <- opt$vars[2]; thA$Comparaison <- rownames(thA); rownames(thA) <- NULL
          out[[length(out)+1]] <- thA[, c("Effet","Comparaison","diff","lwr","upr","p adj")]
        }
        if ("B" %in% names(tuk)) {
          thB <- as.data.frame(tuk[["B"]]); thB$Effet <- opt$vars[3]; thB$Comparaison <- rownames(thB); rownames(thB) <- NULL
          out[[length(out)+1]] <- thB[, c("Effet","Comparaison","diff","lwr","upr","p adj")]
        }
        if (length(out) > 0) {
          posthoc_tbl <- do.call(rbind, out)
          names(posthoc_tbl) <- c("Effet","Comparaison","Différence","IC inf","IC sup","p ajustée")
          posthoc_label <- "Post-hoc (Tukey HSD sur effets principaux)"
        }
      }
      
      assumptions_lines <- c(
        paste0("Effet principal ", opt$vars[2], " : p = ", fmt_p(pA), "."),
        paste0("Effet principal ", opt$vars[3], " : p = ", fmt_p(pB), "."),
        paste0("Interaction ", opt$vars[2], "×", opt$vars[3], " : p = ", fmt_p(pI), "."),
        "Indépendance : supposée (observations indépendantes entre cellules).",
        "Normalité/homogénéité : à vérifier via diagnostics résiduels (non affichés ici)."
      )
    }
    
    # ---- ANOVA à mesures répétées ----
    if (opt$test_code == "rm_aov") {
      yv <- opt$vars[1]
      idv <- opt$vars[2]
      wv  <- opt$vars[3]
      
      y  <- df[[yv]]
      id <- droplevels(as.factor(df[[idv]]))
      w  <- droplevels(as.factor(df[[wv]]))
      validate(need(length(levels(id)) >= 2, "L'identifiant sujet doit avoir ≥ 2 niveaux."))
      validate(need(length(levels(w)) >= 2, "Le facteur intra doit avoir ≥ 2 niveaux."))
      
      dat <- data.frame(y = y, id = id, w = w)
      
      aov_fit <- stats::aov(y ~ w + Error(id/w), data = dat)
      s <- summary(aov_fit)
      
      tab_within <- NULL
      for (k in seq_along(s)) {
        if (is.list(s[[k]]) && length(s[[k]]) >= 1) {
          tab <- s[[k]][[1]]
          if (!is.null(tab) && "w" %in% rownames(tab)) {
            tab_within <- tab
            break
          }
        }
      }
      validate(need(!is.null(tab_within), "Impossible d'extraire l'effet intra-sujet (vérifiez la structure des données)."))
      
      Fw  <- tab_within["w", "F value"]
      df1 <- tab_within["w", "Df"]
      df2 <- tab_within["Residuals", "Df"]
      pw  <- tab_within["w", "Pr(>F)"]
      
      definition_test <- "L’ANOVA à mesures répétées (1 facteur intra-sujet) teste si la moyenne varie entre niveaux d’un facteur mesuré sur les mêmes sujets."
      cas_usage <- "Numérique + identifiant sujet + facteur intra ; objectif : tester un effet intra-sujet global."
      
      stat <- unname(Fw)
      pval <- unname(pw)
      df_txt <- paste0("dl1 = ", df1, " ; dl2 = ", df2)
      fcrit <- stats::qf(1 - alpha, df1 = df1, df2 = df2)
      crit <- paste0("F* = ", fmt_num(fcrit, 4))
      ci_txt <- "Non disponible (RM ANOVA globale)"
      es_txt <- "η²p : non calculée (structure Error multi-strates) ; interpréter avec prudence."
      
      decision <- if (pval <= alpha) "Rejet de H0" else "Échec du rejet de H0"
      justification_p <- paste0("p = ", fmt_p(pval), " ", ifelse(pval <= alpha, "≤", ">"), " α = ", fmt_num(alpha, 4))
      justification_crit <- paste0("F = ", fmt_num(stat, 4), " (réf. F*)")
      
      hypotheses_math$H0 <- "\\(H_0: \\mu_{1} = \\mu_{2} = \\dots = \\mu_{k}\\)"
      hypotheses_math$H1 <- "\\(H_1: \\exists\\ i\\neq j,\\ \\mu_i \\neq \\mu_j\\)"
      hypotheses_text$H0 <- "H0 : moyennes intra-sujet identiques entre niveaux."
      hypotheses_text$H1 <- "H1 : au moins une moyenne intra-sujet diffère."
      
      critere <- paste0("Au seuil \\(\\alpha = ", fmt_num(alpha, 4), "\\), on rejette \\(H_0\\) si \\(p \\leq \\alpha\\) ou si \\(F \\geq F^*\\).")
      
      plot_obj <- ggplot(dat, aes(x = w, y = y, group = id)) +
        geom_line(alpha = 0.35) +
        geom_point(alpha = 0.35) +
        stat_summary(aes(group = 1), fun = mean, geom = "line", linewidth = 1.1) +
        stat_summary(aes(group = 1), fun = mean, geom = "point", size = 2.2) +
        labs(
          title = "Mesures répétées — trajectoires individuelles + moyenne",
          subtitle = paste0("ID = ", idv, " ; Intra = ", wv),
          x = wv,
          y = yv
        ) +
        theme_minimal()
      
      # Post-hoc pairwise t apparié (p ajustée)
      wide <- reshape(dat, idvar = "id", timevar = "w", direction = "wide")
      levs <- levels(dat$w)
      ok_cols <- paste0("y.", levs)
      ok_cols <- ok_cols[ok_cols %in% names(wide)]
      if (length(ok_cols) >= 2) {
        pw_rows <- list()
        cmb <- combn(ok_cols, 2)
        for (j in seq_len(ncol(cmb))) {
          c1 <- cmb[1, j]; c2 <- cmb[2, j]
          x1 <- wide[[c1]]; x2 <- wide[[c2]]
          cc <- complete.cases(x1, x2)
          if (sum(cc) < 2) next
          tt <- stats::t.test(x1[cc], x2[cc], paired = TRUE, alternative = "two.sided")
          pw_rows[[length(pw_rows)+1]] <- data.frame(
            Comparaison = paste0(sub("^y\\.", "", c1), " vs ", sub("^y\\.", "", c2)),
            t = unname(tt$statistic),
            dl = unname(tt$parameter),
            p = unname(tt$p.value),
            stringsAsFactors = FALSE
          )
        }
        if (length(pw_rows) > 0) {
          ph <- do.call(rbind, pw_rows)
          ph$`p ajustée` <- p.adjust(ph$p, method = p_adj)
          ph$p <- NULL
          posthoc_tbl <- ph
          posthoc_label <- paste0("Post-hoc (t apparié pairwise, ajustement = ", p_adj, ")")
        }
      }
      
      assumptions_lines <- c(
        "Appariement : chaque sujet doit être mesuré dans chaque niveau (ou quasi).",
        "Sphéricité : non testée ici (à considérer si ≥ 3 niveaux).",
        "Normalité des résidus intra-sujet : à vérifier (non affichée ici)."
      )
    }
    
    # ---- Catégorielle × Catégorielle ----
    if (opt$test_code %in% c("chi2", "fisher")) {
      a <- droplevels(as.factor(df[[opt$vars[1]]]))
      b <- droplevels(as.factor(df[[opt$vars[2]]]))
      tab <- table(a, b)
      r <- nrow(tab); c <- ncol(tab)
      
      plot_obj <- ggplot(as.data.frame(tab), aes(x = a, y = Freq, fill = b)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(
          title = "Barplot groupé (effectifs)",
          subtitle = paste0(opt$vars[1], " (x) par ", opt$vars[2], " (couleur)"),
          x = opt$vars[1],
          y = "Effectifs",
          fill = opt$vars[2]
        ) +
        theme_minimal()
      
      chi <- suppressWarnings(tryCatch(stats::chisq.test(tab, correct = FALSE), error = function(e) NULL))
      min_exp <- if (!is.null(chi) && !is.null(chi$expected)) min(chi$expected) else NA_real_
      
      assumptions_lines <- c(
        "Indépendance : supposée (observations indépendantes).",
        paste0("Conditions d’effectifs (pour Chi²) : effectif attendu minimal = ", fmt_num(min_exp, 4), ".")
      )
      
      if (opt$test_code == "chi2") {
        validate(need(!is.null(chi), "Impossible de calculer le Chi² (vérifiez la table de contingence)."))
        
        definition_test <- "Le test du \\(\\chi^2\\) d’indépendance évalue si deux variables catégorielles sont indépendantes."
        cas_usage <- "Deux variables catégorielles ; objectif : tester l’association/indépendance entre modalités."
        
        stat <- unname(chi$statistic)
        pval <- unname(chi$p.value)
        df_txt <- paste0("dl = ", unname(chi$parameter))
        
        chicrit <- stats::qchisq(1 - alpha, df = unname(chi$parameter))
        crit <- paste0("χ²* = ", fmt_num(chicrit, 4))
        ci_txt <- "Non disponible"
        es_txt <- paste0("V de Cramér = ", fmt_num(cramers_v(stat, n_final, r, c), 4))
        
        rej <- (stat >= chicrit)
        decision <- if (pval <= alpha) "Rejet de H0" else "Échec du rejet de H0"
        justification_p <- paste0("p-value = ", fmt_p(pval), " ", ifelse(pval <= alpha, "≤", ">"), " α = ", fmt_num(alpha, 4))
        justification_crit <- paste0("χ² = ", fmt_num(stat, 4), " ", ifelse(rej, "≥", "<"), " χ²* = ", fmt_num(chicrit, 4))
        
        hypotheses_math$H0 <- "\\(H_0: \\text{indépendance}\\)"
        hypotheses_math$H1 <- "\\(H_1: \\text{dépendance (association)}\\)"
        hypotheses_text$H0 <- "H0 : indépendance."
        hypotheses_text$H1 <- "H1 : association."
        
        critere <- paste0(
          "Au seuil \\(\\alpha = ", fmt_num(alpha, 4), "\\), on rejette \\(H_0\\) si \\(p \\leq \\alpha\\) ",
          "ou si \\(\\chi^2 \\geq \\chi^{2*}\\). Région critique : \\(\\chi^2 \\geq \\chi^{2*}\\)."
        )
      }
      
      if (opt$test_code == "fisher") {
        is_2x2 <- (r == 2 && c == 2)
        alt_use <- if (is_2x2) alt else "two.sided"
        
        f_p <- tryCatch(
          stats::fisher.test(tab, alternative = alt_use, conf.int = TRUE),
          error = function(e) stats::fisher.test(tab)
        )
        f_ci <- if (is_2x2) {
          tryCatch(stats::fisher.test(tab, alternative = "two.sided", conf.int = TRUE), error = function(e) NULL)
        } else NULL
        
        definition_test <- "Le test exact de Fisher évalue l’indépendance entre deux variables catégorielles sans approximation asymptotique."
        cas_usage <- "Deux variables catégorielles ; objectif : tester l’association (2×2 : bilatéral/unilatéral ; sinon test global)."
        
        stat <- NA_real_
        pval <- unname(f_p$p.value)
        df_txt <- "dl = —"
        crit <- "Non applicable (approche par p-value)"
        
        ci_src <- if (!is.null(f_ci) && !is.null(f_ci$conf.int)) f_ci$conf.int else if (!is.null(f_p$conf.int)) f_p$conf.int else NULL
        ci_txt <- if (!is.null(ci_src)) paste0("[", fmt_num(ci_src[1], 4), " ; ", fmt_num(ci_src[2], 4), "]") else "Non disponible"
        es_txt <- if (!is.null(f_p$estimate)) paste0("OR = ", fmt_num(unname(f_p$estimate), 4)) else "OR = NA"
        
        decision <- if (pval <= alpha) "Rejet de H0" else "Échec du rejet de H0"
        justification_p <- paste0("p-value = ", fmt_p(pval), " ", ifelse(pval <= alpha, "≤", ">"), " α = ", fmt_num(alpha, 4))
        justification_crit <- "Valeur critique : non applicable (décision fondée sur la p-value)"
        
        hypotheses_math$H0 <- "\\(H_0: \\text{indépendance}\\)"
        hypotheses_math$H1 <- "\\(H_1: \\text{dépendance (association)}\\)"
        hypotheses_text$H0 <- "H0 : indépendance."
        hypotheses_text$H1 <- "H1 : association."
        
        critere <- paste0("Au seuil \\(\\alpha = ", fmt_num(alpha, 4), "\\), on rejette \\(H_0\\) si \\(p \\leq \\alpha\\).")
      }
    }
    
    summary_tbl <- data.frame(
      Élément = c(
        "Test",
        "Variables",
        "Types détectés",
        "n initial",
        "n final",
        "Exclusions (NA sur variables du test)",
        "Statistique",
        "Degrés de liberté",
        "Valeur critique",
        "p-value (exacte)",
        "IC 95%",
        "Taille d’effet"
      ),
      Valeur = c(
        test_name,
        vars_display,
        types_display,
        n_initial,
        n_final,
        exclusions,
        ifelse(is.na(stat), "NA", fmt_num(stat, 4)),
        df_txt,
        crit,
        fmt_p(pval),
        ci_txt,
        es_txt
      ),
      stringsAsFactors = FALSE
    )
    
    list(
      alpha = alpha,
      alt = alt,
      p_adj = p_adj,
      posthoc_method = posthoc_method,
      opt = opt,
      vars = opt$vars,
      types = types,
      n_initial = n_initial,
      n_final = n_final,
      exclusions = exclusions,
      test_name = test_name,
      definition_test = definition_test,
      cas_usage = cas_usage,
      assumptions_lines = assumptions_lines,
      hypotheses_math = hypotheses_math,
      hypotheses_text = hypotheses_text,
      critere = critere,
      summary_tbl = summary_tbl,
      posthoc_tbl = posthoc_tbl,
      posthoc_label = posthoc_label,
      decision = decision,
      just_p = justification_p,
      just_crit = justification_crit,
      pval = pval,
      plot = plot_obj,
      plot2 = plot_obj2
    )
  }, ignoreInit = TRUE)
  
  output$result_test_name_ui <- renderUI({
    a <- analysis()
    validate(need(!is.null(a), ""))
    
    code <- a$opt$test_code
    
    cls <- if (code %in% c("t1","tind","anova","anova2","rm_aov","pearson","tpaired")) {
      "result-banner-success"
    } else if (code %in% c("wilcox1","wilcoxind","kruskal","spearman","wilcoxpaired")) {
      "result-banner-warning"
    } else if (code %in% c("chi2","fisher")) {
      "result-banner-danger"
    } else {
      "result-banner-success"
    }
    
    icon_name <- if (cls == "result-banner-success") {
      "check-circle"
    } else if (cls == "result-banner-warning") {
      "exclamation-triangle"
    } else {
      "th-large"
    }
    
    tags$div(
      class = paste("result-banner", cls),
      tags$span(class = "result-banner-icon", icon(icon_name)),
      tags$div(
        tags$div("Test exécuté"),
        tags$div(class = "result-banner-sub", a$test_name)
      )
    )
  })
  
  output$result_table <- renderTable({
    a <- analysis()
    validate(need(!is.null(a), "Cliquez sur « Lancer l’analyse » pour produire les résultats."))
    a$summary_tbl
  }, striped = TRUE, bordered = TRUE, spacing = "s", na = "NA")
  
  output$posthoc_ui <- renderUI({
    a <- analysis()
    validate(need(!is.null(a), ""))
    
    validate(need(!is.null(a$posthoc_tbl), ""))
    
    title <- if (!is.null(a$posthoc_label) && nzchar(a$posthoc_label)) a$posthoc_label else "Post-hoc (comparaisons multiples)"
    tagList(
      tags$h4(title),
      tableOutput("posthoc_table")
    )
  })
  
  output$posthoc_table <- renderTable({
    a <- analysis()
    validate(need(!is.null(a), ""))
    validate(need(!is.null(a$posthoc_tbl), ""))
    
    a$posthoc_tbl
  }, striped = TRUE, bordered = TRUE, spacing = "s", na = "NA")
  
  output$result_plot <- renderPlot({
    a <- analysis()
    validate(need(!is.null(a), "Cliquez sur « Lancer l’analyse » pour afficher le graphique."))
    a$plot
  })
  
  output$result_plot2 <- renderPlot({
    a <- analysis()
    validate(need(!is.null(a), ""))
    validate(need(!is.null(a$plot2), ""))
    a$plot2
  })
  
  output$interpretation_ui <- renderUI({
    a <- analysis()
    validate(need(!is.null(a), "Cliquez sur « Lancer l’analyse » pour produire l’interprétation."))
    
    mode_modele <- isTRUE(input$mode_modele)
    
    conclusion_detaillee <- if (mode_modele) {
      tags$p(
        "Au regard des variables impliquées et après suppression ciblée des valeurs manquantes (uniquement sur les variables du test), ",
        "le test sélectionné répond à une question d’inférence adaptée au type de variables (comparaison, association ou dépendance). ",
        "La décision repose sur une double justification : (i) comparaison de la p-value au seuil \\(\\alpha\\), et (ii) lorsque applicable, comparaison de la statistique observée à une valeur critique cohérente avec l’hypothèse alternative (bilatérale ou unilatérale). ",
        "L’interprétation doit être replacée dans la problématique de recherche : sens de l’effet/association, ampleur pratique (taille d’effet), ",
        "conditions du test (indépendance, normalité, homogénéité, effectifs, linéarité/monotonie) et limites possibles (taille d’échantillon, valeurs extrêmes, biais)."
      )
    } else {
      tags$p("Décision au seuil \\(\\alpha\\) fondée sur la p-value (et la valeur critique lorsque applicable), en cohérence avec l’hypothèse alternative sélectionnée.")
    }
    
    conclusion_publi <- if (mode_modele) {
      tags$p(
        "Conclusion académique (style publication) : ",
        tags$i(a$test_name),
        " a été appliqué aux variables ", tags$b(paste(a$vars, collapse = " ; ")),
        " après exclusion des observations avec valeurs manquantes sur ces variables (n initial = ", a$n_initial, ", n final = ", a$n_final, "). ",
        "Le résultat indique ",
        ifelse(a$pval <= a$alpha, "un résultat statistiquement significatif", "l’absence d’évidence statistique suffisante"),
        " au seuil \\(\\alpha = ", fmt_num(a$alpha, 4), "\\) (p = ", fmt_p(a$pval), "). ",
        "La taille d’effet complète la significativité en documentant l’ampleur pratique, à discuter au regard de la question de recherche."
      )
    } else {
      tags$p(
        "Conclusion académique : ",
        tags$i(a$test_name),
        ", p = ", fmt_p(a$pval),
        " (\\(\\alpha = ", fmt_num(a$alpha, 4), "\\))."
      )
    }
    
    h0_cls <- "hypo-muted"
    h1_cls <- "hypo-muted"
    if (is.character(a$decision) && nzchar(a$decision)) {
      if (grepl("^Rejet de H0$", a$decision)) {
        h0_cls <- "hypo-red"; h1_cls <- "hypo-green"
      } else if (grepl("^Échec du rejet de H0$", a$decision)) {
        h0_cls <- "hypo-green"; h1_cls <- "hypo-red"
      }
    }
    
    # ---- Post-hoc conclusion détaillée ----
    ph <- posthoc_summary_lines(a$posthoc_tbl, a$alpha, max_lines = if (mode_modele) 10 else 6)
    posthoc_block <- NULL
    
    if (!is.null(a$posthoc_tbl)) {
      label <- if (!is.null(a$posthoc_label) && nzchar(a$posthoc_label)) a$posthoc_label else "Post-hoc (comparaisons multiples)"
      posthoc_block <- tagList(
        tags$hr(),
        tags$h3(label),
        tags$p(ph$headline),
        tags$p(tags$b(ph$decision)),
        if (!is.null(ph$lines)) {
          tagList(
            tags$p("Détails des comparaisons (triées par p ajustée croissante) :"),
            tags$ul(ph$lines),
            tags$p(
              "Interprétation : chaque comparaison teste \\(H_0\\) = « pas de différence entre les niveaux comparés ». ",
              "On rejette \\(H_0\\) pour une paire si la p ajustée ≤ α."
            )
          )
        } else {
          tags$p("Aucun détail exploitable n’a été détecté pour synthétiser les comparaisons.")
        }
      )
    }
    
    tagList(
      tags$h2("Règle de décision"),
      tags$h3("1. Définition du test"),
      tags$p(HTML(a$definition_test)),
      tags$h3("2. Cas d’usage"),
      tags$p(HTML(a$cas_usage)),
      tags$h3("3. Hypothèses et conditions"),
      tags$ul(lapply(a$assumptions_lines, tags$li)),
      tags$h3("4. Hypothèses statistiques (Formule + texte)"),
      tags$div(
        tags$div(
          class = "hypo-block",
          tags$span(class = paste("hypo-pill", h0_cls), "H0"),
          tags$b("Formule : "),
          HTML(a$hypotheses_math$H0),
          tags$span(" — "),
          a$hypotheses_text$H0
        ),
        tags$div(
          class = "hypo-block",
          tags$span(class = paste("hypo-pill", h1_cls), "Hₐ"),
          tags$b("Formule : "),
          HTML(a$hypotheses_math$H1),
          tags$span(" — "),
          a$hypotheses_text$H1
        )
      ),
      tags$h3("5. Critère de décision"),
      tags$p(HTML(a$critere)),
      
      tags$h2("RÉSULTAT"),
      tags$p(
        tags$b("Variables : "), paste(a$vars, collapse = " ; "), tags$br(),
        tags$b("Types : "), paste(paste0(a$vars, " (", a$types, ")"), collapse = " | "), tags$br(),
        tags$b("n initial / n final : "), a$n_initial, " / ", a$n_final, " (exclusions = ", a$exclusions, ")"
      ),
      
      tags$h2("DÉCISION"),
      tags$p(tags$b(a$decision)),
      tags$ul(
        tags$li(tags$b("Justification (p-value) : "), a$just_p),
        tags$li(tags$b("Justification (valeur critique) : "), a$just_crit)
      ),
      
      tags$h2("INTERPRÉTATION"),
      tags$h3("1️⃣ Conclusion détaillée ( un ou deux paragraphes )"),
      conclusion_detaillee,
      tags$h3("2️⃣ Conclusion académique (style publication)"),
      conclusion_publi,
      
      posthoc_block
    )
  })
  
})
