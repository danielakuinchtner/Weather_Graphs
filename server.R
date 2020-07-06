library(shiny)
library(classInt)
library(leaflet)
library(RColorBrewer)

shinyServer(function(input, output) {
  
  load("temperature.Rda") 
  load("radiation.Rda")
  load("rain.Rda") 
  load("wind.Rda")
  load("humidity.Rda") 

	temp <- temperatura
	rad <- radiacao
	rain <- chuva
	wind <- vento
	umid <- umidade
	
	#Here you can inform latitude and longitude from anywhere.
	
	#temp <- subset(temp, -7 <= LATITUDE & LATITUDE <= -9 & -34 <= LONGITUDE & LONGITUDE <= -41) # PE
	#temp <- subset(temp, -34 <= LATITUDE & LATITUDE <= -23 & -58 <= LONGITUDE & LONGITUDE <= -48)	# SUL
	temp <- subset(temp, -34.021077 <= LATITUDE & LATITUDE <= -26.806236 & -57.895448 <= LONGITUDE & LONGITUDE <= -49.571597)	# RS
	rad <- subset(rad, -34.021077 <= LATITUDE & LATITUDE <= -26.806236 & -57.895448 <= LONGITUDE & LONGITUDE <= -49.571597)
	rain <- subset(rain, -34.021077 <= LATITUDE & LATITUDE <= -26.806236 & -57.895448 <= LONGITUDE & LONGITUDE <= -49.571597)
	wind <- subset(wind, -34.021077 <= LATITUDE & LATITUDE <= -26.806236 & -57.895448 <= LONGITUDE & LONGITUDE <= -49.571597)
	umid <- subset(umid, -34.021077 <= LATITUDE & LATITUDE <= -26.806236 & -57.895448 <= LONGITUDE & LONGITUDE <= -49.571597)
	#temp <- subset(temp, -23.43 <= LATITUDE & LATITUDE <= -20.80 & -44.96 <= LONGITUDE & LONGITUDE <= -40.87)	# RJ
	#rad <- subset(rad, -23.43 <= LATITUDE & LATITUDE <= -20.80 & -44.96 <= LONGITUDE & LONGITUDE <= -40.87)	# RJ
	#rain <- subset(rain, -23.43 <= LATITUDE & LATITUDE <= -20.80 & -44.96 <= LONGITUDE & LONGITUDE <= -40.87)	# RJ
	#wind <-subset(wind, -23.43 <= LATITUDE & LATITUDE <= -20.80 & -44.96 <= LONGITUDE & LONGITUDE <= -40.87)	# RJ
	#umid <- subset(umid, -23.43 <= LATITUDE & LATITUDE <= -20.80 & -44.96 <= LONGITUDE & LONGITUDE <= -40.87)	# RJ

	
	output$map <- renderLeaflet({

		lat = as.numeric(as.character(temp[,2]))
		lon = as.numeric(as.character(temp[,1]))

		colName <- paste(format(as.Date(input$day, format = '%d/%m/%Y'), "%Y%m%d"),   # day
		                 formatC(input$hour, width=2, flag="0"), sep="")              # hour
		
		variavel <- as.numeric(as.character(temp[,colName]))
		
		#RdYlBu 11
		colors <- rev(brewer.pal(9, "YlOrRd")) #set breaks for the 11 colors 
		brks<-classIntervals(variavel, n=11, style="quantile")
		brks<- brks$brks #plot the map
		
		mapa = leaflet(temp) %>% addTiles()%>% # mapa basico
		  #addProviderTiles(providers$Esri.NatGeoWorldMap)
		  addProviderTiles(providers$CartoDB.Positron)
		  #addProviderTiles(providers$Stamen.Toner)
		  
		  # addProviderTiles(providers$MtbMap) %>%
		  # addProviderTiles(providers$Stamen.TonerLines,
		  #                  options = providerTileOptions(opacity = 0.10)) %>%
		  # addProviderTiles(providers$Stamen.TonerLabels)
		
		mapa %>% addMarkers(
		  clusterOptions = markerClusterOptions(tileOptions(maxZoom=10))
		) 
		
		mapa %>% addRectangles(
		lng1=lon-0.075, lat1=lat-0.075,
		lng2=lon+0.075, lat2=lat+0.075,
		color=colors[findInterval(variavel, brks,all.inside=TRUE)], weight = 0.1)

	})
	
	
	# Function difftime calculates a difference of two date/time objects and returns an object of class "difftime" with an attribute indicating the units
	# grafico GRAFICO TEMPERATURA
	output$plot <- renderPlot({

		day <- as.integer(difftime( 
					as.Date(input$day, format = '%d/%m/%Y'),	# day
					as.Date('20/03/2017', format = '%d/%m/%Y'), # Base
					units = "days"))
		
		day <- (day-1)*24+3
		
		variavel <- temp[,seq(day,day+23)]
		plot(0:23, colMeans(variavel) ,type="l", col = "pink",
			main  = paste("Date: ", format(input$day, '%d/%m/%Y')),
			xlab = 'Measurement (every hour)',
			ylab = 'Temperature',
			xlim = c(00,23),
			ylim = c(00,40))
		
		# browser()
		axis(side=1, 0:23, labels=TRUE)
		points(0:23, colMeans(temp[,seq(day,day+23)]), pch=21, bg="yellow")
		
	})
	
	
	# grafico RADIACAO
	output$plot2 <- renderPlot({
	  
	  lat = as.numeric(as.character(rad[,2]))
	  lon = as.numeric(as.character(rad[,1]))
	  
	  colName <- paste(format(as.Date(input$day, format = '%d/%m/%Y'), "%Y%m%d"),   # day
	                   formatC(input$hour, width=2, flag="0"), sep="")              # hour
	  
	  variavel <- as.numeric(as.character(rad[,colName]))
	  
	  day <- as.integer(difftime( 
	    as.Date(input$day, format = '%d/%m/%Y'),	# day
	    as.Date('20/03/2017', format = '%d/%m/%Y'), # Base
	    units = "days"))
	  
	  day <- (day-1)*24+3
	  
	  variavel <- rad[,seq(day,day+23)]
	  plot(0:23, colMeans(variavel) ,type="l", col = "red",
	       main  = paste("Date: ", format(input$day, '%d/%m/%Y')),
	       xlab = 'Measurement (every hour)',
	       ylab = 'Radiation',
	       xlim = c(00,23),
	       ylim = c(00,1000))
	  
	  # browser()
	  axis(side=1, 0:23, labels=TRUE)
	  points(0:23, colMeans(rad[,seq(day,day+23)]), pch=21, bg="yellow")
	  
	})
	
	
	# grafico VENTO
	output$plot3 <- renderPlot({
	  
	  lat = as.numeric(as.character(wind[,2]))
	  lon = as.numeric(as.character(wind[,1]))
	  
	  colName <- paste(format(as.Date(input$day, format = '%d/%m/%Y'), "%Y%m%d"),   # day
	                   formatC(input$hour, width=2, flag="0"), sep="")              # hour
	  
	  variavel <- as.numeric(as.character(wind[,colName]))
	  
	  day <- as.integer(difftime( 
	    as.Date(input$day, format = '%d/%m/%Y'),	# day
	    as.Date('20/03/2017', format = '%d/%m/%Y'), # Base
	    units = "days"))
	  
	  day <- (day-1)*24+3
	  
	  variavel <- wind[,seq(day,day+23)]
	  plot(0:23, colMeans(variavel) ,type="l", col = "blue",
	       main  = paste("Date: ", format(input$day, '%d/%m/%Y')),
	       xlab = 'Measurement (every hour)',
	       ylab = 'Wind',
	       xlim = c(00,23),
	       ylim = c(00,10))
	  
	  # browser()
	  axis(side=1, 0:23, labels=TRUE)
	  points(0:23, colMeans(wind[,seq(day,day+23)]), pch=21, bg="green")
	  
	})
	
	
	# grafico UMIDADE
	output$plot4 <- renderPlot({
	  
	  lat = as.numeric(as.character(umid[,2]))
	  lon = as.numeric(as.character(umid[,1]))
	  
	  colName <- paste(format(as.Date(input$day, format = '%d/%m/%Y'), "%Y%m%d"),   # day
	                   formatC(input$hour, width=2, flag="0"), sep="")              # hour
	  
	  variavel <- as.numeric(as.character(umid[,colName]))
	  
	  day <- as.integer(difftime( 
	    as.Date(input$day, format = '%d/%m/%Y'),	# day
	    as.Date('20/03/2017', format = '%d/%m/%Y'), # Base
	    units = "days"))
	  
	  day <- (day-1)*24+3
	  
	  variavel <- umid[,seq(day,day+23)]
	  plot(0:23, colMeans(variavel) ,type="l", col = "black",
	       main  = paste("Date: ", format(input$day, '%d/%m/%Y')),
	       xlab = 'Measurement (every hour)',
	       ylab = 'Humidity',
	       xlim = c(00,23),
	       ylim = c(00,10))
	  
	  # browser()
	  axis(side=1, 0:23, labels=TRUE)
	  points(0:23, colMeans(wind[,seq(day,day+23)]), pch=21, bg="orange")
	  
	})
	
	
	# grafico CHUVA
	output$plot5 <- renderPlot({
	
	  lat = as.numeric(as.character(rain[,2]))
	  lon = as.numeric(as.character(rain[,1]))
	  
	  colName <- paste(format(as.Date(input$day, format = '%d/%m/%Y'), "%Y%m%d"),   # day
	                   formatC(input$hour, width=2, flag="0"), sep="")              # hour
	  
	  variavel <- as.numeric(as.character(rain[,colName]))
	  
	  day <- as.integer(difftime( 
	    as.Date(input$day, format = '%d/%m/%Y'),	# day
	    as.Date('20/03/2017', format = '%d/%m/%Y'), # Base
	    units = "days"))
	  
	  day <- (day-1)*24+3
	  
	  variavel <- rain[,seq(day,day+23)]
	  plot(0:23, colMeans(variavel) ,type="l", col = "cyan",
	       main  = paste("Date: ", format(input$day, '%d/%m/%Y')),
	       xlab = 'Measurement (every hour)',
	       ylab = 'Rain (mm)',
	       xlim = c(00,23),
	       ylim = c(00,5))
	  
	  # browser()
	  axis(side=1, 0:23, labels=TRUE)
	  points(0:23, colMeans(rain[,seq(day,day+23)]), pch=21, bg="blue")
	  
	})

	
	# dados TEMPERATURA
	tempDados <- reactive({
	  
	  day <- as.integer(difftime( 
	    as.Date(input$day, format = '%d/%m/%Y'),	# day
	    as.Date('20/03/2017', format = '%d/%m/%Y'), # Base
	    units = "days"))
	  
	  day <- (day-1)*24+3
	  
	  variavel <- temp[,seq(day,day+23)]
	  
	  data.frame(
	    Information = c("Maximum temperature",
	                   "Minimum temperature",
	                   "Average temperature",
	                   "Median Temperature"),
	    
	    Value= c( 
	      round(max(colMeans(variavel)), 2),
	      round(min(colMeans(variavel)), 2),
	      round(mean(colMeans(variavel)), 2),
	      round(median(colMeans(variavel)), 2)
	      ))
	})
	
	output$dadostemp <- renderTable({
	  tempDados()
	})
	
	
	# dados RADIACAO
	radDados <- reactive({
	  
	  day <- as.integer(difftime( 
	    as.Date(input$day, format = '%d/%m/%Y'),	# day
	    as.Date('20/03/2017', format = '%d/%m/%Y'), # Base
	    units = "days"))
	  
	  day <- (day-1)*24+3
	  
	  variavel <- rad[,seq(day,day+23)]
	  
	  data.frame(
	    Information = c("Maximum Radiation",
	                   "Minimum Radiation",
	                   "Average Radiation",
	                   "Median Radiation"),
	    Value= c( 
	      round(max(colMeans(variavel)), 2),
	      round(min(colMeans(variavel)), 2),
	      round(mean(colMeans(variavel)), 2),
	      round(median(colMeans(variavel)), 2)))
  })
	
	output$dadosrad <- renderTable({
	  radDados()
	  
	})
	
	
	# dados VENTO
	ventoDados <- reactive({
	  
	  day <- as.integer(difftime( 
	    as.Date(input$day, format = '%d/%m/%Y'),	# day
	    as.Date('20/03/2017', format = '%d/%m/%Y'), # Base
	    units = "days"))
	  
	  day <- (day-1)*24+3
	  
	  variavel <- wind[,seq(day,day+23)]
	  
	  data.frame(
	    Information = c("Maximum Wind",
	                   "Minimum Wind",
	                   "Average Wind",
	                   "Median Wind"),
	    Value= c( 
	      round(max(colMeans(variavel)), 2),
	      round(min(colMeans(variavel)), 2),
	      round(mean(colMeans(variavel)), 2),
	      round(median(colMeans(variavel)), 2)))
	})
	
	output$dadosvento <- renderTable({
	  ventoDados()
	  
	})
	
	
	# dados UMIDADE
	umidadeDados <- reactive({
	  
	  day <- as.integer(difftime( 
	    as.Date(input$day, format = '%d/%m/%Y'),	# day
	    as.Date('20/03/2017', format = '%d/%m/%Y'), # Base
	    units = "days"))
	  
	  day <- (day-1)*24+3
	  
	  variavel <- umid[,seq(day,day+23)]
	  
	  data.frame(
	    Information = c("Maximum Humidity",
	                   "Minimum Humidity",
	                   "Average Humidity",
	                   "Median Humidity"),
	    Value= c( 
	      round(max(colMeans(variavel)), 2),
	      round(min(colMeans(variavel)), 2),
	      round(mean(colMeans(variavel)), 2),
	      round(median(colMeans(variavel)), 2)))
	  
	})
	
	output$dadosumidade <- renderTable({
	  umidadeDados()
	  
	})
	
	
	# dados CHUVA
	chuvaDados <- reactive({
	  
	  day <- as.integer(difftime( 
	    as.Date(input$day, format = '%d/%m/%Y'),	# day
	    as.Date('20/03/2017', format = '%d/%m/%Y'), # Base
	    units = "days"))
	  
	  day <- (day-1)*24+3
	  
	  variavel <- rain[,seq(day,day+23)]
	  
	  data.frame(
	    Information = c("Maximum Rain (mm)",
	                   "Minimum Rain (mm)",
	                   "Average Rain (mm)",
	                   "Median Rain (mm)"),
	    Value= c( 
	      round(max(colMeans(variavel)), 2),
	      round(min(colMeans(variavel)), 2),
	      round(mean(colMeans(variavel)), 2),
	      round(median(colMeans(variavel)), 2)))
	})
	
	output$dadoschuva <- renderTable({
	  chuvaDados()
	  
	})

})