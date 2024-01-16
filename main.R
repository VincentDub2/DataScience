library(shiny)
library(FactoMineR)
library(factoextra)
library(ggplot2)

# Chemins des fichiers sur le serveur
chemin_fichier_feuilles <- "seed_data.csv"
chemin_fichier_graines <- "leaf_data.csv"

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Analyse en Composantes Principales"),
  sidebarLayout(
    sidebarPanel(
      selectInput("choix_fichier", "Choisissez un type de fichier:",
                  choices = c("Feuilles" = chemin_fichier_feuilles, "Graines" = chemin_fichier_graines)),
      actionButton("btn", "Analyser")
    ),
    mainPanel(
      plotOutput("plotACP"),
      plotOutput("plotCorr")
    )
  )
)

# Serveur
server <- function(input, output, session) {

  result <- eventReactive(input$btn, {
    req(input$choix_fichier)

    # Lecture des données
    data <- read.csv(input$choix_fichier)

    # Préparation des données : enlever les colonnes non numériques si nécessaire
    data <- data[, sapply(data, is.numeric)]

    # ACP avec normalisation des données
    acp_resultat <- PCA(data, scale.unit = TRUE, graph = FALSE)

    print(str(acp_resultat))
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
}

# Exécution de l'application
shinyApp(ui = ui, server = server)
