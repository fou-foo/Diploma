library(ggvis)

# For dropdown menu
actionLink <- function(inputId, ...) {
	tags$a(href='javascript:void',
		   id=inputId,
		   class='action-button',
		   ...)
}

fluidPage(
	titlePanel("Parametrización regla PR001"),
	fluidRow(
		column(3,
			   wellPanel(
			   	checkboxGroupInput("p_69", label = h3("Causales del art. 69"), 
			   					   choices = list("Con créditos fiscales firmes" = 1, "Con créditos fiscales determinados" = 2, 
			   					   			   "Contribuyente no localizado" = 3, "Contribuyente con sentencia condenatoria" = 4,
			   					   			   "Con crédito fiscal afectado por el art. 146-A" = 5, "Con créditos condonados" = 6), selected = 1),
			   	checkboxGroupInput("Otros", label = h3("Otros"), choices = list("69b" = 1, "PCC" = 2, 
			   					   			   "Empresa fachada" = 3, "PCAR" = 4),  selected = 1),
			   	sliderInput("p_ianes", h4("IANES por proveedores (porcentaje)"), min = 0, max = 1, value = 0, step = .01)
			   	, fluidRow(column(3, verbatimTextOutput("value")))
			   )
		),
		column(9, ggvisOutput("tramites"), ggvisOutput("dinero") )
		
	),
		
	titlePanel("Parametrización regla B13 "),
	fluidRow(
		column(3,
			   wellPanel(
			   	h4("Regla PR001 (proporción de proveedores)"),
			   	sliderInput("Anos", "Años desde última auditoría", min = 0, max = 12 ,value = 0, step = 1))),
		column(9, ggvisOutput("b13_1"),  ggvisOutput("b13_2"))
	)
	
	
	
	
	
	
	#titlePanel("Parametrización 2 "),
	#fluidRow(
	#	column(3,
	#		   wellPanel(
	#		   	h4("Regla PR001 (proporción de proveedores)"),
	#		   	sliderInput("reviews", "Minimum number of reviews on Rotten Tomatoes",
	#		   				10, 300, 80, step = 10),
	#		   	sliderInput("year", "Year released", 1940, 2014, value = c(1970, 2014)),
	#		   	sliderInput("oscars", "Minimum number of Oscar wins (all categories)",
	#		   				0, 4, 0, step = 1),
	#		   	sliderInput("boxoffice", "Dollars at Box Office (millions)",
	#		   				0, 800, c(0, 800), step = 1),
	#		   	selectInput("genre", "Genre (a movie can have multiple genres)",
	#		   				c("All", "Action", "Adventure", "Animation", "Biography", "Comedy",
	#		   				  "Crime", "Documentary", "Drama", "Family", "Fantasy", "History",
	#		   				  "Horror", "Music", "Musical", "Mystery", "Romance", "Sci-Fi",
	#		   				  "Short", "Sport", "Thriller", "War", "Western")
	#		   	),
	#		   	textInput("director", "Director name contains (e.g., Miyazaki)"),
	#		   	textInput("cast", "Cast names contains (e.g. Tom Hanks)")
	#		   ),
	#		   wellPanel(
	#		   	selectInput("xvar", "X-axis variable", axis_vars, selected = "Meter"),
	#		   	selectInput("yvar", "Y-axis variable", axis_vars, selected = "Reviews"),
	#		   	tags$small(paste0(
	#		   		"Note: The Tomato Meter is the proportion of positive reviews",
	#		   		" (as judged by the Rotten Tomatoes staff), and the Numeric rating is",
	#		   		" a normalized 1-10 score of those reviews which have star ratings",
	#		   		" (for example, 3 out of 4 stars)."
	#		   	))
	#		   )
	#	),
	#	column(9, ggvisOutput("plot1"), wellPanel( span("Number of movies selected:", textOutput("n_movies"))))
	#	)
)