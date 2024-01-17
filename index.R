library(shiny)
library(shinydashboard)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(cluster)

# Chemins des fichiers sur le serveur
chemin_fichier_feuilles <- "seed_data.csv"
chemin_fichier_graines <- "leaf_data.csv"
chemin_fichier_combine <- "leaf_seed_data.csv"

header <- dashboardHeader(title = "Analyse de Données")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("ACP", tabName = "acp", icon = icon("chart-line")),
    menuItem("ANOVA", tabName = "anova", icon = icon("balance-scale")),
    menuItem("K-means Clustering", tabName = "kmeans", icon = icon("chart-pie"))
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
    # Troisième page : K-means Clustering
    tabItem(tabName = "kmeans",
            fluidPage(
              titlePanel("K-means Clustering"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("choix_fichier_kmeans", "Choisissez un type de fichier:",
                              choices = c("Feuilles" = chemin_fichier_feuilles, "Graines" = chemin_fichier_graines)),
                  numericInput("clusters", "Nombre de clusters:", min = 2, value = 3),
                  actionButton("btn", "Calculer")
                ),
                mainPanel(
                  plotOutput("plotKmeans"),
                  plotOutput("plotKmeansPCA"),
                  plotOutput("plotElbow"),
                  plotOutput("plotSilhouette"),
                  plotOutput("plotGap")
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

  # K-means Clustering
  # Fonction réactive pour lire les données sélectionnées

  # Fonction réactive pour lire les données sélectionnées
  reactiveData <- reactive({
    req(input$choix_fichier_kmeans)
    inFile <- input$choix_fichier_kmeans
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile, header = TRUE, stringsAsFactors = FALSE)
  })

  results_kmeans_pca <- eventReactive(input$btn, {
    req(reactiveData())
    donnees <- reactiveData()
    donnees_normalisees <- scale(donnees[, sapply(donnees, is.numeric)])
    set.seed(123)
    kmeans_result <- kmeans(donnees_normalisees, centers = input$clusters, nstart = 20, iter.max = 50)

    # Ajouter la colonne de cluster aux données normalisées
    donnees_normalisees <- as.data.frame(donnees_normalisees)
    donnees_normalisees$cluster <- as.factor(kmeans_result$cluster)

    # Exécuter l'analyse PCA
    donnees_pour_pca <- donnees_normalisees[, !colnames(donnees_normalisees) %in% c("cluster")]
    resultat_pca <- PCA(donnees_pour_pca, graph = FALSE)

    list(resultat_pca = resultat_pca, donnees_normalisees = donnees_normalisees, kmeans_result = kmeans_result)
  })

  observeEvent(input$btn, {
    # Assurez-vous que des données ont été chargées
    req(reactiveData())

    # Effectuer le clustering K-means
    donnees <- reactiveData()
    donnees_normalisees <- scale(donnees[, sapply(donnees, is.numeric)])
    set.seed(123)
    kmeans_result <- kmeans(donnees_normalisees, centers = input$clusters, nstart = 20, iter.max = 50)

    # Stocker les résultats pour un usage futur
    output$plotKmeans <- renderPlot({
      fviz_cluster(kmeans_result, data = donnees_normalisees)
    })

    output$plotKmeansPCA <- renderPlot({
      results <- results_kmeans_pca()
      req(results)
      fviz_pca_biplot(results$resultat_pca, label = "var",
                      col.ind = results$donnees_normalisees$cluster,
                      addEllipses = TRUE, ellipse.level = 0.95)
    })

    # Calculer et tracer la méthode du coude
    wcss <- sapply(1:10, function(k) {
      kmeans(donnees_normalisees, centers = k, nstart = 20)$tot.withinss
    })
    output$plotElbow <- renderPlot({
      plot(1:10, wcss, type = "b", xlab = "Nombre de Clusters", ylab = "WCSS")
    })

    # Calculer et tracer les scores de silhouette
    output$plotSilhouette <- renderPlot({
      req(reactiveData())
      donnees <- reactiveData()
      donnees_normalisees <- scale(donnees[, sapply(donnees, is.numeric)])
      silhouette_scores <- numeric(9)

      for (k in 2:10) {
        kmeans_result <- kmeans(donnees_normalisees, centers = k, nstart = 20)
        sil_score <- silhouette(kmeans_result$cluster, dist(donnees_normalisees))
        silhouette_scores[k - 1] <- mean(sil_score[, 3])
      }
      str(silhouette_scores)
      plot(2:10, silhouette_scores, type = "b", xlab = "Nombre de Clusters", ylab = "Score de Silhouette Moyen")
    })
  })
  # Calculer et tracer l'analyse des gaps
  output$plotGap <- renderPlot({
    req(reactiveData())
    donnees <- reactiveData()
    donnees_normalisees <- scale(donnees[, sapply(donnees, is.numeric)])
    gap_stat <- clusGap(donnees_normalisees, FUN = kmeans, nstart = 20, K.max = 10, B = 50)
    plot(gap_stat, main = "Gap Statistic")
  })


  # Fin K-means Clustering
}

shinyApp(ui = ui, server = server)