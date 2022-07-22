library(shiny)
library(VennDiagram)
library(RColorBrewer)

url <-
    'https://www.ncbi.nlm.nih.gov/genomes/VirusVariation/vvsearch_dev/?report=download_lineages'
# lineages <- read.csv('lineages.csv')
lineages <- tryCatch({
    lineages <- read.csv(url)
},
error = function(cond) {
    message(paste("URL not working:", url))
    message(paste("Loading local DB..."))
    lineages <- read.csv('lineages.csv')
    return(lineages)
})

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
    titlePanel("Covid Venn"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            selectizeInput(
                'selected_lineages',
                'Lineages',
                lineages$lineage,
                options = list(maxItems = 4),
                selected = c("BA.5", "BA.1", "BA.2.75"),
                multiple = T
            ),

            checkboxGroupInput(
                'selected_types',
                'Types of mutations',
                selected = c('aa_definition', 'VinTEBS', 'VinOEBS'),
                choiceNames = c('Defining', 'VinTEBS (therapeutic)', 'VinOEBS (other)'),
                choiceValues = c('aa_definition', 'VinTEBS', 'VinOEBS')
            ),
            radioButtons('show_mutations', 'Show mutations', c('yes', 'no'),
                         'yes'),
            HTML(
                'Created using data from the <a href="https://www.ncbi.nlm.nih.gov/activ">NCBI SARS-CoV-2 Variants Overview</a>. See <a href="https://github.com/MDU-PHL/covid-venn">GitHub</a> for more details.'
                ),
        ),
        # Show Venn
        mainPanel(fluidRow(align = "center",
                           plotOutput("venn")))
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    get_mutations <- function(df, lineage, types) {
        df[df$lineage == lineage, names(df) %in% append(types, 'lineages')]
    }


    output$venn <- renderPlot({
        mutations <-
            lapply(input$selected_lineages, function (x) {
                paste0(get_mutations(lineages, x, input$selected_types),
                       collapse = '')
            })
        number_of_groups <- length(input$selected_lineages)
        if (input$show_mutations == 'yes') {
            label_size <- 0.7
            # force venn into predictable shape so we can edit the labels
            if (number_of_groups == 3) {
                groups <- list(c('a', 'c'), c('b', 'c'), c('c', 'd'))
            } else if (number_of_groups == 2) {
                groups <- list(c('a', 'c'), c('b', 'c'))
            } else {
                groups <- lapply(mutations, function(x) {
                    unlist(strsplit(x, ","))
                })
                label_size <- 1.5
            }

        } else {
            groups <- lapply(mutations, function(x) {
                unlist(strsplit(x, ","))
            })
            label_size <- 1.5
        }
        if (length(mutations) == 0) {
            return('No Mutations')
        }
        v <- venn.diagram(
            groups,
            category.names = input$selected_lineages,
            filename = NULL,
            height = 480 ,
            width = 480 ,
            resolution = 300,
            euler.d = T,

            # Circles
            lwd = 2,
            lty = 'blank',
            fill = brewer.pal(4, "Pastel2")[0:length(input$selected_lineages)],

            # Numbers
            cex = label_size,
            fontface = "bold",
            fontfamily = "sans",

            cat.cex = 1.5,
            cat.fontfamily = "sans",

            # Logging
            disable.logging = T
        )

        split_to_lines <- function(x, n = 2) {
            return (lapply(split(
                x,
                rep(1:(length(x) %/% n), length.out = length(x))
            ),
            function(x) {
                paste(x, collapse = " ")
            }))
        }

        make_lable <- function(set, labels_per_line = 3) {
            paste(length(set),
                  paste(
                      split_to_lines(set, n = labels_per_line),
                      collapse = "\n"
                  ),
                  sep = '\n')
        }

        if (input$show_mutations == 'yes') {
            if (number_of_groups == 1) {
                group1 <- unlist(strsplit(mutations[[1]], ","))
                v[[3]]$label <- make_lable(group1)
            }
            if (number_of_groups == 2) {
                group1 <- unlist(strsplit(mutations[[1]], ","))
                group2 <- unlist(strsplit(mutations[[2]], ","))
                g1 <- setdiff(group1, group2)
                g2 <- setdiff(group2, group1)
                g12 <- intersect(group1, group2)
                v[[5]]$label <- make_lable(g1)
                v[[6]]$label <- make_lable(g2)
                v[[7]]$label <- make_lable(g12)

            }
            if (number_of_groups == 3) {
                group1 <- unlist(strsplit(mutations[[1]], ","))
                group2 <- unlist(strsplit(mutations[[2]], ","))
                group3 <- unlist(strsplit(mutations[[3]], ","))
                g1 <- setdiff(group1, union(group2, group3))
                g2 <- setdiff(group2, union(group1, group3))
                g3 <- setdiff(group3, union(group1, group2))
                g12 <- setdiff(intersect(group1, group2), group3)
                g13 <- setdiff(intersect(group1, group3), group2)
                g23 <- setdiff(intersect(group2, group3), group1)
                g123 <- intersect(intersect(group2, group1), group3)
                v[[7]]$label <- make_lable(g1)
                v[[8]]$label <- make_lable(g12)
                v[[9]]$label <- make_lable(g2)
                v[[10]]$label <- make_lable(g13)
                v[[11]]$label <- make_lable(g123)
                v[[12]]$label <- make_lable(g23)
                v[[13]]$label <- make_lable(g3)
            }

        }
        grid.newpage()
        grid.draw(v)
    }, height = 600, width = 600)
}

# Run the application
shinyApp(ui = ui, server = server)
