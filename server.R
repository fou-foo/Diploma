#library(ggvis)
#library(dplyr)
#library(RSQLite)


#mio
library(dplyr)
library(ggvis)
library(lubridate)
#library(googleVis)

data <- read.csv("/home/fou/Desktop/SAT/chamba/Param app /GARA_STATS2.csv")
data <- data %>% mutate(BsumaFirmes = as.numeric(sumaFirmes > 0), BsumaNo_Localizados =  as.numeric(sumaNo_Localizados > 0),
						BsumaCondonados = as.numeric(sumaCondonados > 0), BsumaExigibles = as.numeric(sumaExigibles > 0),
						BsumaCondonados_ri = as.numeric(sumaCondonados_ri > 0), Bsumasentencias = as.numeric(sumasentencias >0),
						Bsumaflg69b = as.numeric(sumaflg69b > 0 ),
						BsumaPCC = as.numeric(sumaPCC > 0), 
						BsumaEF = as.numeric(sumaEF >0 ),
						BsumaPCAR = as.numeric(sumaPCAR > 0) 
)

datos <- data %>% filter(BIN_PR001 == 1 | BIN_B13 == 1)
#datos <- datos %>% mutate(p_69 = (sumapub69/numero_de_proveedores),
#						  p_69b = (sumaflg69b/numero_de_proveedores),
#						  p_ef = (sumaEF/numero_de_proveedores),
#						  p_pcar = (sumaPCAR/ numero_de_proveedores),
#						  p_ianes = (sumaMalo_IANES/ sumaIVA))

#z <-filter(datos, p_69>.99)

#table(datos$p_69)

art_69 <- list('sumaFirmes'= 1, 'sumaNo_Localizados' = 3, 'sumaCondonados' = 6,
			   'sumaExigibles' = 2, 'sumaCondonados_ri'= 5, 'sumasentencias' = 4 )
otros <- list( "69b" = 1, "PCC" = 2, "EF" = 3, "PCAR" = 4 )
da <- datos %>% filter(BIN_B13 == 1)
da <- da %>% select( sir_nocontrol, ejercicio, periodo, max_f_termino, monto_sol_dev )
da <- da %>%  mutate(tiempoInicial = ymd(paste(ejercicio, periodo, '01', sep = '-' )))
da <- da %>% mutate(tiempoTermino = ymd(paste(max_f_termino, '01', '01', sep = '-')))
da$tiempoTermino[is.na(da$max_f_termino)] <- NA
# Set up handles to database tables on app start
##db <- src_sqlite("movies.db")
##omdb <- tbl(db, "omdb")
##tomatoes <- tbl(db, "tomatoes")

# Join tables, filtering out those with <10 reviews, and select specified columns
##all_movies <- inner_join(omdb, tomatoes, by = "ID") %>%
##	filter(Reviews >= 10) %>%
##	select(ID, imdbID, Title, Year, Rating_m = Rating.x, Runtime, Genre, Released,
##		   Director, Writer, imdbRating, imdbVotes, Language, Country, Oscars,
##		   Rating = Rating.y, Meter, Reviews, Fresh, Rotten, userMeter, userRating, userReviews,
##		   BoxOffice, Production)


function(input, output, session)
{
	
	foo1 <- reactive({
		
		#mio
		#input <- list(p_69 = c(1,2,3,4,5,6), otros = c(1,2,3,4))
		eleccionUno <- names(art_69)[match(input$p_69,art_69)] #parser de los nombres de numero a string
		eleccionDos <- names(otros)[match(input$Otros, otros)]
		causales_69 <- as.numeric(names(art_69) %in% eleccionUno)   # banderas para filtrar cada origen del 69
		causalesOtros <- as.numeric(names(otros) %in% eleccionDos)
		ianes <- input$p_ianes 
		#output$value <- renderPrint({ ianes })
		
		dataPr001 <- datos %>% select(sir_nocontrol,periodo ,BIN_PR001, BsumaFirmes, BsumaNo_Localizados, BsumaCondonados,
									  BsumaExigibles, BsumaCondonados_ri, Bsumasentencias,
									  Bsumaflg69b, BsumaPCC, BsumaEF, BsumaPCAR,
									  #ianes
									  sumaMalo_IANES, sumaIVA,
									  monto_sol_dev
		) %>% filter(BIN_PR001 == 1)  #periodo de febrero mete ruido
		
		#sumilación de los riesgos 1 si inporta y 0 si no 
		if(!causales_69[1])
		{
			index <- which(dataPr001$BsumaFirmes == 1)
			dataPr001$BsumaFirmes[index] <- 0
		}
		if(!causales_69[2])
		{
			index <- which(dataPr001$BsumaNo_Localizados == 1)
			dataPr001$BsumaNo_Localizados[index] <- 0
		}
		if(!causales_69[3])
		{
			index <- which(dataPr001$BsumaCondonados == 1)
			dataPr001$BsumaCondonados[index] <- 0
		}
		if(	!causales_69[4])
		{
			index <- which(dataPr001$BsumaExigibles == 1)
			dataPr001$BsumaExigibles[index] <- 0
		}
		if(!causales_69[5])
		{
			index <- which(dataPr001$BsumaCondonados_ri == 1)
			dataPr001$BsumaCondonados_ri[index] <- 0
		}
		if(!causales_69[6])
		{
			index <- which(dataPr001$Bsumasentencias == 1)
			dataPr001$Bsumasentencias <- 0
		}
		if(!causalesOtros[1])
		{
			index <- which(dataPr001$Bsumaflg69b == 1) 
			dataPr001$Bsumaflg69b <- 0
		}
		if(!causalesOtros[2])
		{
			index <- which(dataPr001$BsumaPCC == 1)
			dataPr001$BsumaPCC[index] <- 0
		}
		if(!causalesOtros[3])
		{
			index <- which( dataPr001$BsumaEF == 1)
			dataPr001$BsumaEF[index] <- 0
		}
		if(!causalesOtros[4])
		{
			index <- which(dataPr001$BsumaPCAR == 1)
			dataPr001$BsumaPCAR[index] <- 0
		}
		
		statsPR001 <- dataPr001  %>% group_by(sir_nocontrol,periodo, monto_sol_dev) %>%
			transmute(RiesgoPRO = sum(BsumaFirmes, BsumaNo_Localizados, BsumaCondonados,
									  BsumaExigibles, BsumaCondonados_ri, Bsumasentencias, Bsumaflg69b,
									  BsumaPCC, BsumaEF, BsumaPCAR, na.rm = TRUE),
					  r_ianes = ((sumaMalo_IANES / sumaIVA) > ianes) ) 
		statsPR001 <- statsPR001 %>% mutate(Riesgo = sum(RiesgoPRO, r_ianes, na.rm = TRUE))
		
		statsPR001$Riesgo <- statsPR001$Riesgo ==  0  #mejor marcar a los de no riesgo por la diferencia de los NA 
		# que son "no hay información" y que estan mezclados con os riesgos en la tabla que me paos Martha
		statsPR001$Riesgo <- factor(statsPR001$Riesgo , labels  = c( 'Riesgo', 'No_Riesgo')) #hay que poner atención a la definicion de estos "labels"
		a <- statsPR001 %>% group_by(Riesgo) %>% summarise(c = n(), dinero = sum(as.numeric(monto_sol_dev), na.rm = TRUE))
		as.data.frame(a)
	})
	
	
	
	
	
	vis_names <- function(x)
	{
		if(is.null(x)) return(NULL)
		paste0(x[1,1]," :",x[1,4], collapse="<br />")		#http://stackoverflow.com/questions/27992078/displaying-information-using-ggvis
		
	}
	
	vis_tramites <- reactive({
		
		foo2 <- foo1 %>% ggvis(~Riesgo,~c, fill = ~ Riesgo  )%>% layer_bars() %>%  scale_nominal("fill", range = c("red", "green")) %>%
			add_axis("x", title = "") %>%
			add_axis("y", title = "") %>%
			add_axis("x", orient = "top", ticks = 0, title = "PR001 tramites", #title
					 properties = axis_props(
					 	axis = list(stroke = "white"),
					 	labels = list(fontSize = 0))) %>% hide_legend("fill") %>% add_tooltip(vis_names, "hover") %>%
			set_options(width = 300, height = 300)
		
	})
	
	vis_dinero <- reactive({
		
		foo2 <- foo1 %>% ggvis(~Riesgo, ~dinero, fill = ~ Riesgo  )%>% layer_bars() %>%  scale_nominal("fill", range = c("red", "green")) %>%
			add_axis("x", title = "") %>%
			add_axis("y", title = "") %>%
			add_axis("x", orient = "top", ticks = 0, title = "PR001 dinero", #title
					 properties = axis_props(
					 	axis = list(stroke = "white"),
					 	labels = list(fontSize = 0))) %>% hide_legend("fill") %>% add_tooltip(vis_names, "hover") %>%
			set_options(width = 300, height = 300)
		
	})
	
	
	vis_tramites %>%  bind_shiny("tramites")
	vis_dinero %>%  bind_shiny("dinero")
	
	# Filter the movies, returning a data frame
	##movies <- reactive({
	#	# Due to dplyr issue #318, we need temp variables for input values
	##	reviews <- input$reviews
	##	oscars <- input$oscars
	##	minyear <- input$year[1]
	##	maxyear <- input$year[2]
	##	minboxoffice <- input$boxoffice[1] * 1e6
	##	maxboxoffice <- input$boxoffice[2] * 1e6
	#		#statsPR001 <-  
	#		# Apply filters
	##		m <- all_movies %>%
	##			filter(
	##				Reviews >= reviews,
	##				Oscars >= oscars,
	##				Year >= minyear,
	##				Year <= maxyear,
	##				BoxOffice >= minboxoffice,
	##				BoxOffice <= maxboxoffice
	##			) %>%
	##			arrange(Oscars)
	#		# Optional: filter by genre
	#		#if (input$genre != "All") {
	#		#	genre <- paste0("%", input$genre, "%")
	##			m <- m %>% filter(Genre %like% genre)
	#		#}
	#		# Optional: filter by director
	##		if (!is.null(input$director) && input$director != "") {
	##			director <- paste0("%", input$director, "%")
	##			m <- m %>% filter(Director %like% director)
	##		}
	#		# Optional: filter by cast member
	##		if (!is.null(input$cast) && input$cast != "") {
	##			cast <- paste0("%", input$cast, "%")
	##			m <- m %>% filter(Cast %like% cast)
	##		}
	##		m <- as.data.frame(m)
	#		# Add column which says whether the movie won any Oscars
	#		# Be a little careful in case we have a zero-row data frame
	##		m$has_oscar <- character(nrow(m))
	##		m$has_oscar[m$Oscars == 0] <- "No"
	##		m$has_oscar[m$Oscars >= 1] <- "Yes"
	##		m
	##	})
	
	
	# Function for generating tooltip text
	##	movie_tooltip <- function(x) {
	##		if (is.null(x)) return(NULL)
	##		if (is.null(x$ID)) return(NULL)
	#		# Pick out the movie with this ID
	##		all_movies <- isolate(movies())
	##		movie <- all_movies[all_movies$ID == x$ID, ]
	##		paste0("<b>", movie$Title, "</b><br>",
	##			   movie$Year, "<br>",
	##			   "$", format(movie$BoxOffice, big.mark = ",", scientific = FALSE)
	##		)
	##	}
	
	# A reactive expression with the ggvis plot
	##	vis <- reactive({
	# Lables for axes
	##		xvar_name <- names(axis_vars)[axis_vars == input$xvar]
	##		yvar_name <- names(axis_vars)[axis_vars == input$yvar]
	# Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
	# but since the inputs are strings, we need to do a little more work.
	##		xvar <- prop("x", as.symbol(input$xvar))
	##		yvar <- prop("y", as.symbol(input$yvar))
	##		movies %>%
	##			ggvis(x = xvar, y = yvar) %>%
	##			layer_points(size := 50, size.hover := 200,
	##						 fillOpacity := 0.2, fillOpacity.hover := 0.5,
	##						 stroke = ~has_oscar, key := ~ID) %>%
	##			add_tooltip(movie_tooltip, "hover") %>%
	##			add_axis("x", title = xvar_name) %>%
	##			add_axis("y", title = yvar_name) %>%
	##			add_legend("stroke", title = "Won Oscar", values = c("Yes", "No")) %>%
	##			scale_nominal("stroke", domain = c("Yes", "No"),
	##						  range = c("orange", "#aaa")) %>%
	##			set_options(width = 500, height = 500)
	##	})
	##	vis %>% bind_shiny("plot1")
	
	##	output$n_movies <- renderText({ nrow(movies()) })
	
	b13 <- reactive({
		limite <- input$Anos
		da <- da %>% mutate(Riesgo = (tiempoTermino + dyears(limite)) <= tiempoInicial )
		da$Riesgo <- factor(da$Riesgo, labels = c("Riesgo", "No_Riesgo"))
		table(da$Riesgo)
		p <- da %>% group_by(Riesgo) %>% summarise(c = n(), dinero = sum(as.numeric(monto_sol_dev), na.rm = TRUE))
		p
	})
	
	vis_tramites2 <- reactive({
		
		r <- b13 %>% ggvis(~Riesgo,~c, fill = ~ Riesgo  ) %>% layer_bars() %>%  scale_nominal("fill", range = c("red", "green")) %>%
			add_axis("x", title = "") %>%
			add_axis("y", title = "") %>%
			add_axis("x", orient = "top", ticks = 0, title = "B13 tramites", #title
					 properties = axis_props(
					 	axis = list(stroke = "white"),
					 	labels = list(fontSize = 0))) %>% hide_legend("fill") %>% add_tooltip(vis_names, "hover") %>%
			set_options(width = 300, height = 300)
		
	})
	
	vis_dinero2 <- reactive({
		
		r2 <- b13 %>% ggvis(~Riesgo, ~dinero, fill = ~ Riesgo  )%>% layer_bars() %>% scale_nominal("fill", range = c("red", "green")) %>%
			add_axis("x", title = "") %>%
			add_axis("y", title = "") %>%
			add_axis("x", orient = "top", ticks = 0, title = "B13 dinero", #title
					 properties = axis_props(
					 	axis = list(stroke = "white"),
					 	labels = list(fontSize = 0))) %>% hide_legend("fill") %>%
			add_tooltip(vis_names, "hover") %>%
			set_options(width = 300, height = 300)
		
	})
	
	
	vis_tramites2 %>%  bind_shiny("b13_1")
	vis_dinero2 %>%  bind_shiny("b13_2")
	
	
}