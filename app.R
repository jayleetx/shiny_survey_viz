# Jay Lee, June 2018

# you can load the data into the R object `survey` with the following code (not run)

### library(dplyr)
### source("extras.R")
### survey <- loadData() %>% filter(response == "survey")


library(shiny)
library(shinydashboard)
library(scatterplot3d)
library(dplyr)
library(vegan)
source("extras.R")

ui <- dashboardPage(
  dashboardHeader(title = "Integrative Neuroethology", titleWidth = '250'),
  
  # high-level tab definition
  dashboardSidebar(width = 250, collapsed = TRUE, disable = TRUE,
                   sidebarMenu(id = 'tabs',
                               menuItem("Introduction", tabName = "intro", icon = icon("home")),
                               menuItem("Survey", tabName = "survey", icon = icon("edit")),
                               menuItem("Visualize", tabName = "viz", icon = icon("image"))
                   )
  ),
  
  # the main app interface ----
  dashboardBody(
    useShinyjs(),
    shinyjs::inlineCSS(appCSS), # make the star for required questions red
    tabItems(
      # first major tab, intro
      tabItem(tabName = "intro",
              fluidRow(
                box(width = 12,
                    # text
                    p('The following survey was developed to quantify the integrative nature of modern neuroethology research using 3 axes of integration, taxonomic, level of biological organization, and area of question as defined by Niko Tinbergen.'),
                    br(),
                    h4("For the purpose of this survey, please consider your current primary research agenda to be the focus for your answers. This may include multiple years of work, as well as the effort of many people in a specific research group, but it should hang together as a cohesive (integrated) research agenda and omit side projects not related to the main research agenda."),
                    br(),
                    p('The survey should take 5-7 minutes to complete. The responses will be kept anonymously and reports of this study will not include individual data in a form by which you could be identified. If successful, this survey may be used in a small publication. If you have any question about this research, please contact Jay Lee (jayleetx) for information about the procedures of this study.'),
                    br(),
                    p("By continuing with the survey, you certify that you are 18 years of age or older and voluntarily consent to participate. Thank you for your participation."),
                    
                    # nav buttons
                    actionButton("to_survey", "Take Survey", icon('edit')),
                    br(),br(),
                    p("If you've already completed the survey, click below to go straight to the visualization."),
                    actionButton("to_viz", "View graph", icon('image'))
                )
              )
      ),
      
      # second major tab, survey
      tabItem(tabName = "survey",
              fluidRow(
                # 5 minor tabs here for the survey
                tabBox(id = 'surveybox', width = 12, 
                       
                       # first minor tab, demographics
                       tabPanel("Neuroethologists (1/5)", value = "surv1",
                                
                                # text
                                p("* Required", style = "color:red"),
                                p("Please complete the following demographic information. These data will not be tied to individual names or email addresses. Eventually, users will be able to explore the database of integrative neuroethologists using these terms."),
                                
                                # questions
                                selectInput('career_stage',
                                            labelMandatory("Career Stage"),
                                            c("",
                                              'Undergraduate' = 'undergrad',
                                              'Graduate student' = 'grad_stud',
                                              'Postdoctoral researcher' = 'postdoc',
                                              'Assistant professor' = 'assist_prof',
                                              'Associate professor' = 'assoc_prof',
                                              'Full professor' = 'full_prof')),
                                checkboxGroupInput('funding', 'Funding source (check all that apply)',
                                                   c("Government - basic science" = "gov_sci",
                                                     "Government - health related" = "gov_health",
                                                     "Government - military related" = "gov_military",
                                                     "Government - agriculture" = "gov_ag",
                                                     "University/research institution" = "academic_research",
                                                     "Industry" = "industry",
                                                     "Private foundation" = "private",
                                                     "Other" = "other")),
                                conditionalPanel(
                                  condition = "RegExp('other').test(input.funding)",
                                  textInput("other_funding", "Other funding sources")
                                ),
                                selectInput('institution_type', 'Type of Institution',
                                            c("",
                                              'R1 - research institution (basic research)' = 'research',
                                              'R1 - medical school campus' = 'med_school',
                                              'Liberal arts college' = 'lib_arts',
                                              'Non-teaching research institute' = 'non_teaching',
                                              'Other' = 'other')),
                                conditionalPanel(
                                  condition = "input.institution_type == 'other'",
                                  textInput("other_inst_type", "Other institution type")
                                ),
                                selectInput('institution_location', "Where is the institution that you are affiliated with located?",
                                            c("",
                                              'Africa' = 'africa',
                                              'Asia' = 'asia',
                                              'Europe' = 'europe',
                                              'North America' = 'north_america',
                                              'Oceania' = 'oceania',
                                              'South America' = 'south_america')),
                                numericInput('h_index', "Google Scholar h-index (if known)",
                                             min = 0, step = 1, value = NULL),
                                selectInput('gender', 'To which gender identity do you most identify?',
                                            c("",
                                              'Female' = 'female',
                                              'Male' = 'male',
                                              'Transgender female' = 'trans_woman',
                                              'Transgender male' = 'trans_man',
                                              'Gender non-conforming' = 'gnc',
                                              'Prefer not to answer' = 'dna',
                                              'Other' = 'other')),
                                conditionalPanel(
                                  condition = "input.gender == 'other'",
                                  textInput("other_gender", "Other gender")
                                ),
                                
                                # nav buttons
                                actionButton("to_intro", "Back to intro", icon("home")),
                                actionButton("for_tax_score", "Next", icon("arrow-right")),
                                
                                # errors
                                htmlOutput(outputId = "career_error")
                       ),
                       
                       # second minor tab, taxonomic score
                       tabPanel("Taxonomic Score of Integration (2/5)", value = "surv2",
                                
                                # text
                                p("* Required", style = "color:red"),
                                p('To calculate a "Taxonomic score of integration", count the number of species, genera, families (or orders) and phyla that you use in your primary research agenda. If one species is used, there is obviously automatically also one genus, one family and one phylum. If more than 10 taxa at that level are used, select 10.'),
                                
                                # questions
                                numericInput('n_species',
                                             labelMandatory("Number of species used in this research?"),
                                             value = NULL, min = 1, max = 10, step = 1),
                                numericInput('n_genera',
                                             labelMandatory("Number of genera used in this research?"),
                                             value = NULL, min = 1, max = 10, step = 1),
                                numericInput('n_families',
                                             labelMandatory("Number of families used in this research?"),
                                             value = NULL, min = 1, max = 10, step = 1),
                                numericInput('n_phyla',
                                             labelMandatory("Number of phyla used in this research?"),
                                             value = NULL, min = 1, max = 10, step = 1),
                                
                                # nav buttons
                                actionButton("back_neuroeth", "Back", icon("arrow-left")),
                                actionButton("for_bio_score", "Next", icon("arrow-right")),
                                
                                # errors
                                htmlOutput(outputId = "species_error"),
                                htmlOutput(outputId = "genera_error"),
                                htmlOutput(outputId = "families_error"),
                                htmlOutput(outputId = "phyla_error"),
                                htmlOutput(outputId = "tax_range_error"),
                                htmlOutput(outputId = "tax_order_error")),
                       
                       # third minor tab, biological score
                       tabPanel("Biological Level Score of Integration (3/5)", value = "surv3",
                                
                                # text
                                p("* Required", style = "color:red"),
                                p('To calculate a "Biological Level Score of Integration", estimate the proportion of importance that your research agenda gives to each of eight specified levels of potential biological organization. All levels are considered to be of equal relative importance. Perfect integration on this axis would include equal coverage of all levels (25%). Complete lack of integration on this axis would be 100% importance at one level.'),
                                strong("The sum of these importance levels must equal 100%."),
                                br(),
                                p("Indicate the relative % importance of each level of analysis in your research agenda."),
                                
                                # questions
                                numericInput('molecular_imp', labelMandatory("Molecular"),
                                             value = NULL, min = 0, max = 100),
                                numericInput('physiological_imp', labelMandatory("Physiological"),
                                             value = NULL, min = 0, max = 100),
                                numericInput('individual_imp', labelMandatory("Infividual or social behavior"), 
                                             value = NULL, min = 0, max = 100),
                                numericInput('species_imp', labelMandatory("Species interaction in full ecosystem"), 
                                             value = NULL, min = 0, max = 100),
                                
                                # nav buttons
                                actionButton("back_tax_score", "Back", icon("arrow-left")),
                                actionButton("for_tin_score", "Next", icon("arrow-right")),
                                
                                # errors
                                htmlOutput(outputId = "molecular_error"),
                                htmlOutput(outputId = "physiological_error"),
                                htmlOutput(outputId = "individual_error"),
                                htmlOutput(outputId = "species_imp_error"),
                                htmlOutput(outputId = "bio_range_error"),
                                htmlOutput(outputId = "bio_sum_error")),
                       
                       # fourth minor tab, tinbergen score
                       tabPanel("Tinbergen Score of Integration (4/5)", value = "surv4",
                                box(width = 12,
                                    p("* Required", style = "color:red"),
                                    p("To calculate a \"Tinbergen Score of Integration\", estimate the proportion of importance that your research agenda gives to each of Tinbergen’s 4 areas of inquiry. This assumes that Tinbergen was correct in his assertion that any question in biology can be fully addressed by considering mechanism (causation), ontogeny (development), adaptive value (function) and phylogeny (evolution). All areas of inquiry are considered to be of equal relative importance. Perfect integration on this axis would include equal coverage of all areas (25%). Complete lack of integration on this axis would be 100% importance in one area. See the figure below for a refresher on Tinbergen's 4 Questions."),
                                    strong("The sum of these importance levels must equal 100%.")
                                ),
                                
                                box(width = 6,
                                    p("Indicate the relative % importance of each area of inquiry in your research agenda."),
                                    
                                    # questions
                                    numericInput('mechanism_imp', labelMandatory("Mechanism"),
                                                 value = NULL, min = 0, max = 100),
                                    numericInput('ontology_imp', labelMandatory("Ontogeny"),
                                                 value = NULL, min = 0, max = 100),
                                    numericInput('adaptive_imp', labelMandatory("Adaptive value"), 
                                                 value = NULL, min = 0, max = 100),
                                    numericInput('phylogeny_imp', labelMandatory("Phylogeny"), 
                                                 value = NULL, min = 0, max = 100),
                                    
                                    # nav buttons
                                    actionButton("back_bio_score", "Back", icon("arrow-left")),
                                    actionButton("for_finish", "Next", icon("arrow-right")),
                                    
                                    # errors
                                    htmlOutput(outputId = "mechanism_error"),
                                    htmlOutput(outputId = "ontology_error"),
                                    htmlOutput(outputId = "adaptive_error"),
                                    htmlOutput(outputId = "phylogeny_error"),
                                    htmlOutput(outputId = "tin_range_error"),
                                    htmlOutput(outputId = "tin_sum_error")
                                ),
                                box(width = 6,
                                    tags$img(src = "tin_img.jpg", width = "400px", height = "500px")
                                )
                       ),
                       
                       # fifth minor tab, submit the survey
                       tabPanel("Finish and Submit (5/5)", value = "surv5",
                                
                                # text
                                h3("Thank you for taking the time to add data to our database."),
                                                    
                                # questions
                                textInput("comments", width = '100%',
                                          label = "If you know of a research program that you consider to be particularly integrative, please decribe it briefly below."),
                                
                                # nav buttons
                                actionButton("back_tin_score", "Back", icon("arrow-left")),
                                actionButton('submit_survey', "Submit and Visualize", icon('share-square')))
                )
              )
      ),
      
      # third major tab, visualization
      tabItem(tabName = "viz",
              h2("Visualize"),
              fluidRow(
                box(width = 3,
                    actionButton("rotate_cw", "Rotate clockwise", icon('sync')),
                    br(),
                    actionButton("rotate_ccw", "Rotate counter-clockwise", icon('undo')),
                    hr(),
                    selectInput("col_var", "Choose a variable to color the points by",
                                c("",
                                  "Career stage" = "career_stage",
                                  "Institution type" = "institution_type",
                                  "Institution location" = "institution_location",
                                  "H-index" = "h_index",
                                  "Gender" = "gender")),
                    hr(),
                    h3("Legend"),
                    htmlOutput(outputId = "legend")
                ),
                box(width = 9,
                    plotOutput("plot")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$tin_img <- renderImage({
    list(src = "www/tinbergen.jpg")
  })
  
  # compile the entries submitted by the user with a timestamp
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = lubridate::ymd_hms(Sys.time()),
              response = "survey")
    data <- t(data)
    data
  })
  updateData()
  
  # button click events -----
  observeEvent(input$to_survey, {
    updateTabItems(session, 'tabs', selected =  'survey')
    updateTabsetPanel(session, "surveybox", selected = 'surv1')
    showTab("surveybox", target = "surv1")
    hideTab("surveybox", target = "surv2")
    hideTab("surveybox", target = "surv3")
    hideTab("surveybox", target = "surv4")
    hideTab("surveybox", target = "surv5")
  })
  observeEvent(input$to_intro, {
    updateTabItems(session, 'tabs', selected =  'intro')
  })
  
  observeEvent(input$back_neuroeth, {
    updateTabsetPanel(session, "surveybox", selected = 'surv1')
    showTab("surveybox", target = "surv1")
    hideTab("surveybox", target = "surv2")
  })
  
  observeEvent(input$for_tax_score, {
    output$career_error <- renderUI({
      HTML(ifelse(!isTruthy(input$career_stage),
                  '<p style="color:red">* Entry required for career stage</p>',
                  no = ""))
    })
    req(input$career_stage)
    updateTabsetPanel(session, "surveybox", selected = 'surv2')
    hideTab("surveybox", target = "surv1")
    showTab("surveybox", target = "surv2")
    hideTab("surveybox", target = "surv3")
  })
  
  observeEvent(input$back_tax_score, {
    updateTabsetPanel(session, "surveybox", selected = 'surv2')
    hideTab("surveybox", target = "surv1")
    showTab("surveybox", target = "surv2")
    hideTab("surveybox", target = "surv3")
  })
  observeEvent(input$for_bio_score, {
    # require each thing exists
    output$species_error <- renderUI({
      HTML(ifelse(!isTruthy(input$n_species),
                  '<p style="color:red">* Numeric entry required for number of species</p>',
                  no = ""))
    })
    req(input$n_species)
    
    output$genera_error <- renderUI({
      HTML(ifelse(!isTruthy(input$n_genera),
                  '<p style="color:red">* Numeric entry required for number of genera</p>',
                  no = ""))
    })
    req(input$n_genera)
    
    output$families_error <- renderUI({
      HTML(ifelse(!isTruthy(input$n_families),
                  '<p style="color:red">* Numeric entry required for number of families</p>',
                  no = ""))
    })
    req(input$n_families)
    
    output$phyla_error <- renderUI({
      HTML(ifelse(!isTruthy(input$n_phyla),
                  '<p style="color:red">* Numeric entry required for number of phyla</p>',
                  no = ""))
    })
    req(input$n_phyla)
    
    # require all entries between 0 and 10
    output$tax_range_error <- renderUI({
      HTML(ifelse(!isTruthy(all(c(input$n_species, input$n_genera, input$n_families, input$n_phyla) >= 0) &
                              all(c(input$n_species, input$n_genera, input$n_families, input$n_phyla) <= 10)),
                  '<p style="color:red">* All entries must be between 0 and 10</p>',
                  no = ""))
    })
    req(all(c(input$n_species, input$n_genera, input$n_families, input$n_phyla) >= 0))
    req(all(c(input$n_species, input$n_genera, input$n_families, input$n_phyla) <= 10))
    
    # require decreasing species, genera, families, phyla
    output$tax_order_error <- renderUI({
      HTML(ifelse(!isTruthy(input$n_species >= input$n_genera &
                              input$n_genera >= input$n_families &
                              input$n_families >= input$n_phyla),
                  '<p style="color:red">* Must be more species than genera, more genera than families, and more families than phyla</p>',
                  no = ""))
    })
    req(input$n_species >= input$n_genera)
    req(input$n_genera >= input$n_families)
    req(input$n_families >= input$n_phyla)
    
    # change tabs
    updateTabsetPanel(session, "surveybox", selected = 'surv3')
    hideTab("surveybox", target = "surv2")
    showTab("surveybox", target = "surv3")
    hideTab("surveybox", target = "surv4")
  } )
  observeEvent(input$back_bio_score, {
    updateTabsetPanel(session, "surveybox", selected = 'surv3')
    hideTab("surveybox", target = "surv2")
    showTab("surveybox", target = "surv3")
    hideTab("surveybox", target = "surv4")
  })
  
  observeEvent(input$for_tin_score, {
    
    # require each thing exists
    output$molecular_error <- renderUI({
      HTML(ifelse(!isTruthy(input$molecular_imp),
                  '<p style="color:red">* Numeric entry required for molecular importance</p>',
                  no = ""))
    })
    req(input$molecular_imp)
    
    output$physiological_error <- renderUI({
      HTML(ifelse(!isTruthy(input$physiological_imp),
                  '<p style="color:red">* Numeric entry required for physiological importance</p>',
                  no = ""))
    })
    req(input$physiological_imp)
    
    output$individual_error <- renderUI({
      HTML(ifelse(!isTruthy(input$individual_imp),
                  '<p style="color:red">* Numeric entry required for individual importance</p>',
                  no = ""))
    })
    req(input$individual_imp)
    
    output$species_imp_error <- renderUI({
      HTML(ifelse(!isTruthy(input$species_imp),
                  '<p style="color:red">* Numeric entry required for species importance</p>',
                  no = ""))
    })
    req(input$species_imp)
    
    # require between 0 and 100        
    output$bio_range_error <- renderUI({
      HTML(ifelse(!isTruthy(all(c(input$molecular_imp, input$physiological_imp, input$individual_imp, input$species_imp) >= 0) &
                              all(c(input$molecular_imp, input$physiological_imp, input$individual_imp, input$species_imp) <= 100)),
                  '<p style="color:red">* All entries must be between 0 and 100</p>',
                  no = ""))
    })
    req(all(c(input$molecular_imp, input$physiological_imp, input$individual_imp, input$species_imp) >= 0))
    req(all(c(input$molecular_imp, input$physiological_imp, input$individual_imp, input$species_imp) <= 100))
    
    # require sum to 100        
    output$bio_sum_error <- renderUI({
      HTML(ifelse(!isTruthy(input$molecular_imp + input$physiological_imp + input$individual_imp + input$species_imp == 100),
                  '<p style="color:red">* Entries must add up to 100</p>',
                  no = ""))
    })
    req(input$molecular_imp + input$physiological_imp + input$individual_imp + input$species_imp == 100)
    
    # change tabs    
    updateTabsetPanel(session, "surveybox", selected = 'surv4')
    hideTab("surveybox", target = "surv3")
    showTab("surveybox", target = "surv4")
    hideTab("surveybox", target = "surv5")} )
  
  
  
  observeEvent(input$for_finish, {
    
    # require each thing exists
    output$mechanism_error <- renderUI({
      HTML(ifelse(!isTruthy(input$mechanism_imp),
                  '<p style="color:red">* Numeric entry required for mechanism importance</p>',
                  no = ""))
    })
    req(input$mechanism_imp)
    
    output$ontology_error <- renderUI({
      HTML(ifelse(!isTruthy(input$ontology_imp),
                  '<p style="color:red">* Numeric entry required for ontology importance</p>',
                  no = ""))
    })
    req(input$ontology_imp)
    
    output$adaptive_error <- renderUI({
      HTML(ifelse(!isTruthy(input$adaptive_imp),
                  '<p style="color:red">* Numeric entry required for adaptive importance</p>',
                  no = ""))
    })
    req(input$adaptive_imp)
    
    output$phylogeny_error <- renderUI({
      HTML(ifelse(!isTruthy(input$phylogeny_imp),
                  '<p style="color:red">* Numeric entry required for phylogeny importance</p>',
                  no = ""))
    })
    req(input$phylogeny_imp)
    
    # require between 0 and 100        
    output$tin_range_error <- renderUI({
      HTML(ifelse(!isTruthy(all(c(input$mechanism_imp, input$ontology_imp, input$adaptive_imp, input$phylogeny_imp) >= 0) &
                              all(c(input$mechanism_imp, input$ontology_imp, input$adaptive_imp, input$phylogeny_imp) <= 100)),
                  '<p style="color:red">* All entries must be between 0 and 100</p>',
                  no = ""))
    })
    req(all(c(input$mechanism_imp, input$ontology_imp, input$adaptive_imp, input$phylogeny_imp) >= 0))
    req(all(c(input$mechanism_imp, input$ontology_imp, input$adaptive_imp, input$phylogeny_imp) <= 100))
    
    # require sum to 100        
    output$tin_sum_error <- renderUI({
      HTML(ifelse(!isTruthy(input$mechanism_imp + input$ontology_imp + input$adaptive_imp + input$phylogeny_imp == 100),
                  '<p style="color:red">* Entries must add up to 100</p>',
                  no = ""))
    })
    req(input$mechanism_imp + input$ontology_imp + input$adaptive_imp + input$phylogeny_imp == 100)
    
    # change tabs    
    updateTabsetPanel(session, "surveybox", selected = 'surv5')
    hideTab("surveybox", target = "surv4")
    showTab("surveybox", target = "surv5")} )
  
  
  observeEvent(input$back_tin_score, {
    updateTabsetPanel(session, "surveybox", selected = 'surv4')
    hideTab("surveybox", target = "surv3")
    showTab("surveybox", target = "surv4")
    hideTab("surveybox", target = "surv5")} )
  
  # submit survey button click
  observeEvent(input$submit_survey, {
    saveData(formData()) # save the entries submitted by the user
    sapply(fieldsAll, function(x) shinyjs::reset(input[[x]])) # clear the survey
    updateData()
    updateTabItems(session, 'tabs', selected =  'viz') # go to the plot
  })
  
  
  observeEvent(input$to_viz, {
    updateTabItems(session, 'tabs', selected =  'viz')
    updateData()
  })
  
  # data calculation and plotting -----
  
  # plot points for viz
  
  col_var <- reactive({
    if (!isTruthy(input$col_var)) {
      all_data$person_color
    } else if (input$col_var == "h_index") {
      cut(all_data$h_index,
          breaks = c(-1,10,20,30,40,50,75,100,1000),
          labels = c("0-10",
                     "11-20",
                     "21-30",
                     "31-40",
                     "41-50",
                     "51-75",
                     "76-100",
                     "> 100"))
    } else {
      all_data[ ,input$col_var]
    }
  })
  colors <- reactive({
    if (input$col_var == "h_index") {
      RColorBrewer::brewer.pal(n = length(levels(col_var())), name="YlOrRd")[1:length(levels(col_var()))]
    } else if (!isTruthy(input$col_var)){
      RColorBrewer::brewer.pal(n = length(levels(col_var())), name="Set1")[1:2]
    } else {
      RColorBrewer::brewer.pal(n = length(levels(col_var())), name="Set1")
    }
  })
  col_list <- reactive({
    paste0(sprintf('<div style="background: %s;text-align: center;">%s</div>', colors(), levels(col_var())), collapse = "\n\n")
  }) 
  output$legend <- renderUI({
    HTML(col_list())
  })
  
  values <- reactiveValues(angle = 40)
  observe({
    input$rotate_ccw
    isolate(values$angle <- values$angle + 10 %% 360)
  })
  observe({
    input$rotate_cw
    isolate(values$angle <- values$angle - 10 %% 360)
  })
  
  output$plot <- renderPlot({
    scatterplot3d(for_plotting[ ,7:9], pch = 16, angle = values$angle,
                  xlab = "Taxonomic", ylab = "Biological", zlab = "Tinbergen",
                  color = colors()[col_var()], box = FALSE, cex.symbols = 1.8)
  })
  
  
}

shinyApp(ui, server)
