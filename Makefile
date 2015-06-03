all: app 

app: 
	R -e 'library(shiny);runApp("ptt-app", port=8000)'

deploy:
	cd ptt-app && R -e 'library(shinyapps);deployApp(appName="ptt-app")'

terminate:
	R -e 'library(shinyapps);terminateApp(appName="ptt-app")'

xxlarge:
	R -e 'library(shinyapps);configureApp("ptt-app", size="xxlarge")'
