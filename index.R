library(shiny)
library(shinydashboard)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(reticulate)

# Chemins des fichiers sur le serveur
chemin_fichier_feuilles <- "seed_data.csv"
chemin_fichier_graines <- "leaf_data.csv"
chemin_fichier_combine <- "leaf_seed_data.csv"

header <- dashboardHeader(title = "Analyse de Données")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("ACP", tabName = "acp", icon = icon("chart-line")),
    menuItem("ANOVA", tabName = "anova", icon = icon("balance-scale")),
    menuItem("Étude des feuilles", tabName = "feuille", icon = icon("leaf")),
    menuItem("Étude des graines", tabName = "graine", icon = icon("seedling"))
  )
)

body <- dashboardBody(
  tabItems(
    # Première page : ACP
    tabItem(tabName = "acp",
            fluidPage(
              titlePanel("Analyse en Composantes Principales"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("choix_fichier", "Choisissez un type de fichier:",
                              choices = c("Feuilles" = chemin_fichier_feuilles, "Graines" = chemin_fichier_graines)),
                  actionButton("btn_acp", "Analyser")
                ),
                mainPanel(
                  plotOutput("plotACP"),
                  plotOutput("plotCorr")
                )
              )
            )
    ),
    # Deuxième page : ANOVA
    tabItem(tabName = "anova",
            fluidPage(
              titlePanel("Analyse de Variance (ANOVA)"),
              sidebarLayout(
                sidebarPanel(
                  uiOutput("checkbox_elements"),
                  actionButton("btn_anova", "Effectuer ANOVA")
                ),
                mainPanel(
                  tableOutput("resultAnova") # Utiliser tableOutput pour afficher les résultats sous forme de tableau
                )
              )
            )
    ),
    # Troisième page : Étude des feuilles
    tabItem(tabName = "feuille",
            fluidPage(
              titlePanel("Étude des feuilles"),
              fluidRow(
                div(style = "margin-top: 20px;",
                    plotOutput("plotVariability"),
                ),
                div(style = "margin-top: 50px;",
                    plotOutput("plotHeritability"),
                ),
                div(style = "margin-top: 50px;",
                    plotOutput("plotCorrelation"),
                )
              )
            )
    ),
    # Quatrième page : Étude des graines
    tabItem(tabName = "graine",
            fluidPage(
              titlePanel("Étude des graines"),
              fluidRow(
                div(style = "margin-top: 20px;",
                    plotOutput("plotVariabilitySeed"),
                ),
                div(style = "margin-top: 50px;",
                    plotOutput("plotHeritabilitySeed"),
                ),
                div(style = "margin-top: 50px;",
                    plotOutput("plotCorrelationSeed"),
                )
              )
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {

  # ACP
  result <- eventReactive(input$btn_acp, {
    req(input$choix_fichier)

    # Lecture des données
    data <- read.csv(input$choix_fichier)

    # Préparation des données : enlever les colonnes non numériques si nécessaire
    data <- data[, sapply(data, is.numeric)]

    # ACP avec normalisation des données
    acp_resultat <- PCA(data, scale.unit = TRUE, graph = FALSE)

    return(acp_resultat)
  })

  output$plotACP <- renderPlot({
    res <- result()
    req(res)

    fviz_pca_ind(res,
                 axes = c(1, 2), # Dimensions X et Y fixées
                 label = "none",
                 repel = TRUE,
                 addEllipses = TRUE,
                 ellipse.level = 0.95
    )
  })

  output$plotCorr <- renderPlot({
    res <- result()
    req(res)

    fviz_pca_var(res,
                 axes = c(1, 2), # Dimensions X et Y fixées
                 col.var = "contrib",
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE
    )
  })

  output$plotVariability <- renderPlot({
    source("Graphes/Leaf/Variabilite_relative_elements.R", local = TRUE)$value
    
    # Charger les données nécessaires pour le graphique
    data_for_feuille <- data.frame(
      Element = c("Li", "Mg", "P", "S", "K", "Ca", "Mn", "Fe", "Cu", "Zn", "As", "Sr", "Cd", "Co", "Se", "Rb", "Na", "Mo"),
      RSD = c(4.38, 8.35, 7.52, 9.60, 9.31, 5.86, 5.33, 2.33, 8.12, 7.01, 8.32, 6.38, 9.88, 23.90, 13.05, 10.75, 33.63, 71.29),
      RSD_Category = c(rep("Faible", 13), rep("Modéré", 3), "Fort", "Fort")
    )
    
    # Appeler la fonction generateGraph pour générer le graphique
    generateGraph(data_for_feuille)
  })
  
  output$plotHeritability <- renderPlot({
    source("Graphes/Leaf/Heritabilite_elements.R", local = TRUE)$value
    
    # Données d'héritabilité
    heritability_data <- data.frame(
      Element = c("Li", "Na", "Mg", "P", "S", "K", "Ca", "Mn", "Fe", "Co", "Cu", "Zn", "As", "Se", "Rb", "Sr", "Mo", "Cd"),
      Heritability = c(16.58, 52.89, 26.49, 28.67, 21.17, 43.54, 25.47, 5.48, 10.49, 29.38, 16.68, 4.32, 9.98, 39.87, 28.39, 28.34, 77.14, 4.95)
    )
    
    # Appeler la fonction generateGraph pour générer le graphique
    generateGraph(heritability_data)
  })
  
  output$plotCorrelation <- renderPlot({
    source("Graphes/Leaf/Correlation_rsd_heritability.R", local = TRUE)$value
    
    correlation_data <- data.frame(
      Element = c("Li", "Na", "Mg", "P", "S", "K", "Ca", "Mn", "Fe", "Co", "Cu", "Zn", "As", "Se", "Rb", "Sr", "Mo", "Cd"),
      RSD = c(4.38, 33.63, 8.35, 7.52, 9.60, 9.31, 5.86, 5.33, 2.33, 23.90, 8.12, 7.01, 8.32, 13.05, 10.75, 6.38, 71.29, 9.88),
      Heritability = c(16.58, 52.89, 26.49, 28.67, 21.17, 43.54, 25.47, 5.48, 10.49, 29.38, 16.68, 4.32, 9.98, 39.87, 28.39, 28.34, 77.14, 4.95)
    )
    
    # Appeler la fonction generateGraph pour générer le graphique
    generateGraph(correlation_data)
  })
  
  output$plotVariabilitySeed <- renderPlot({
    source("Graphes/Seed/Variabilite_relative_elements.R", local = TRUE)$value
    
    # Charger les données nécessaires pour le graphique
    data <- data.frame(
      Element = c("Li", "Mg", "P", "S", "K", "Ca", "Mn", "Fe", "Cu", "Zn", "As", "Sr", "Cd", "Co", "Se", "Rb", "Na", "Mo"),
      RSD = c(25.42, 66.74, 6.82, 3.47, 10.27, 9.79, 7.86, 22.33, 4.34, 13.97, 8.47, 13.24, 11.06, 71.84, 10.99, 17.84, 39.48, 87.28),
      RSD_Category = c(rep("Faible", 13), rep("Modéré", 3), "Fort", "Fort")
    )
    
    # Appeler la fonction generateGraph pour générer le graphique
    generateGraph(data)
  })
  
  output$plotHeritabilitySeed <- renderPlot({
    source("Graphes/Seed/Heritabilite_elements.R", local = TRUE)$value
    
    # Données d'héritabilité
    heritability_data <- data.frame(
      Element = c("Li", "Na", "Mg", "P", "S", "K", "Ca", "Mn", "Fe", "Co", "Cu", "Zn", "As", "Se", "Rb", "Sr", "Mo", "Cd"),
      Heritability = c(29.77, 18.36, 26.09, 11.33, 9.39, 26.31, 20.41, 32.92, 6.26, 3.94, 16.45, 6.42, 17.66, 36.31, 24.96, 29.94, 34.37, 33.94)
    )
    
    # Appeler la fonction generateGraph pour générer le graphique
    generateGraph(heritability_data)
  })
  
  output$plotCorrelationSeed <- renderPlot({
    source("Graphes/Seed/Correlation_rsd_heritability.R", local = TRUE)$value
    
    correlation_data <- data.frame(
      Element = c("Li", "Na", "Mg", "P", "S", "K", "Ca", "Mn", "Fe", "Co", "Cu", "Zn", "As", "Se", "Rb", "Sr", "Mo", "Cd"),
      RSD = c(25.42, 66.74, 6.82, 3.47, 10.27, 9.79, 7.86, 22.33, 4.34, 13.97, 8.47, 13.24, 11.06, 71.84, 10.99, 17.84, 39.48, 87.28),
      Heritability = c(29.77, 18.36, 26.09, 11.33, 9.39, 26.31, 20.41, 32.92, 6.26, 3.94, 16.45, 6.42, 17.66, 36.31, 24.96, 29.94, 34.37, 33.94)
    )
    
    # Appeler la fonction generateGraph pour générer le graphique
    generateGraph(correlation_data)
  })
  
  output$checkbox_elements <- renderUI({
    data <- read.csv(chemin_fichier_combine)
    elements <- setdiff(names(data), c("ID", "TYPE"))
    checkboxGroupInput("elements", "Sélectionnez les éléments pour l'ANOVA:", choices = elements)
  })

  # Créer un objet réactif pour stocker les résultats de l'ANOVA
  results <- reactiveVal(list()) # Initialiser un objet réactif vide

  # Réagir au bouton d'action pour l'ANOVA
  observeEvent(input$btn_anova, {
    req(input$elements) # S'assurer que l'utilisateur a sélectionné au moins un élément
    data_anova <- read.csv(chemin_fichier_combine)
    cat("Données ANOVA chargées:\n")
    print(head(data_anova))

    # Stocker les résultats de l'ANOVA dans l'objet réactif
    results(lapply(input$elements, function(element) {
      anova_result <- aov(reformulate("TYPE", response = element), data = data_anova)
      cat("Résultats de l'ANOVA pour", element, ":\n")
      print(summary(anova_result))
      return(list(element = element, summary = summary(anova_result)))
    }))
  })

    # Effectuer une ANOVA pour chaque élément sélectionné et renvoyer les résultats

  output$resultAnova <- renderTable({
    req(results()) # S'assurer que les résultats de l'ANOVA sont disponibles
    anova_list <- results()
    req(length(anova_list) > 0)
    do.call(rbind, lapply(anova_list, function(res) {
      anova_res <- res$summary
      # Assurez-vous que le résumé de l'ANOVA est sous forme de liste avec les éléments attendus.
      if (is.list(anova_res) && "Df" %in% names(anova_res[[1]])) {
        data.frame(
          Element = res$element,
          Df = anova_res[[1]]$Df,
          Sum_Sq = anova_res[[1]]$`Sum Sq`,
          Mean_Sq = anova_res[[1]]$`Mean Sq`,
          F_value = anova_res[[1]]$`F value`,
          Pr_greater_F = anova_res[[1]]$`Pr(>F)`
        )
      } else {
        # Si les données ne sont pas structurées comme prévu, retournez un dataframe avec des NA
        data.frame(
          Element = res$element,
          Df = NA,
          Sum_Sq = NA,
          Mean_Sq = NA,
          F_value = NA,
          Pr_greater_F = NA
        )
      }
    }))
  })
}

  shinyApp(ui = ui, server = server)