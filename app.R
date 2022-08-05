
# Statistical Computing Final Project
# Free Diving Records App

inst_pack_func <- function(list.of.packages){
    new.packages <- list.of.packages[!(list.of.packages %in%
                                           installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    lapply(list.of.packages,function(x){library(x,character.only=TRUE)})
}
list.of.packages <- c("shiny","tidyverse","magrittr",
                      "dplyr","shinyWidgets","leaflet","ggplot2","DT","shinythemes")
inst_pack_func(list.of.packages)

freediving <- read.csv("freediving.csv")

ui <- fluidPage(
    titlePanel("Group Latte - Final Project 292"),
    theme = shinytheme("flatly"),
    setBackgroundImage(
        src = "https://image.freepik.com/free-vector/abstract-minimal-white-background_23-2148887988.jpg"),
    sidebarLayout(
        sidebarPanel(
            h2("Free Diving Records App", align="center"),
            h5("Created by Latte", align = "center"),
            sliderInput(inputId = "Record", 
                        label = "Minimum Record Time", 
                        value = 30, min = 0, max = 300),
            selectInput(inputId = "Disciplines", label = "Disciplines",
                        choices = c( "Dynamic With Fins (DYN)" = "DYN", 
                                     "Dynamic Without Fins (DNF)" = "DNF",
                                     "Dynamic With Bifins (DYNB)"= "DYNB",
                                     "No Limit (NLT)" = "NLT",
                                     "Variable Weight (VWT)" ="VWT",
                                     "Constant Weight (CWT)" = "CWT",
                                     "Constant Weight Without Fins (CNF)" = "CNF",
                                     "Free Immersion (FIM)" = "FIM",
                                     "Constant Weight With Bifins (CWTB)"= "CWTB")),
            radioButtons(inputId = "Sex", label = "Sex",
                         choices = c("Women","Men","Women & Men"), selected ="Women"),
            checkboxInput(
                inputId = "themeToggle",
                label = icon("sun")
            ),
            helpText("To open dark mode please click to light icon :)"),
            p("Made with", a("Shiny",
                             href = "http://shiny.rstudio.com"
            ), "."),
            
            img(
                src = "https://www.kindpng.com/picc/m/706-7068650_r-shiny-logo-png-transparent-png.png",
                width = "100px", height = "90px"
            ),
            h5("Group Members:"),
            h6("Aybuke Altuntas "),
            h6("Gozde Akkaya  "),
            h6("Gulnaz Cavusoglu "),
            h6("Hatice Kubra Gulcan ")
            
        ),
        
        mainPanel(
            tabsetPanel( type = "tab",
                         tabPanel("Welcome Page",
                                  p("Hello everyone!"),
                                  p("Welcome to our Free Diving Records App. We are glad to see you here :)."),
                                  p("Aim of this app is to show, clearly and directly relevant information about free diving records.To achieve this, we examined the subjects in terms of data visualization and statistical significance."),
                                  p("The user can learn about different types of disciplines via videos, see distributions of different disciplines in the world map, and compare records of athletes in terms of gender, also see how many athletes joined from different countries via the bar graph. Besides that, several athletes and results from information can be found and explored in this app."),
                                  img(src = "https://upload.wikimedia.org/wikipedia/commons/b/b3/Ben_Noble_%282483696721%29.jpg", height = 400, width = 800) ),
                         tabPanel("Data Summary",
                                  p(),
                                  p("The data contains 260 observations of  13 variables, and it is in the data frame format. The thirteen (13) of them are factors, and the two(2) are numeric. The variables are discipline, athlete, country, record, date, event, sex, wikipedia, profile, video_name, video_link, latitude, and longitude. When we first find the data, it doesn't contain latitude and longitude variables; we add these variables for each country to visualize via maps. The discipline variable shows different types of disciplines in freediving; the athlete variable shows the name of athletes who joined the competitions, the country variable shows from which country the athlete, the record variable shows the diving time of athletes in minutes, the date variable shows the time of diving the event variable shows the name of competitions or event, the sex variable shows the gender of the person, the wikipedia variable shows the wikipedia link that gives general pieces of information about athletes the profile variable contains the link which is directing to International Association for the Development of Apnea website, the  video_name and video_link variables shows diving videos of athletes if there is any. Latitude and longitude variables show the countries location."),
                                  p("You can select how many rows of data you want to see if you want."),
                                  DTOutput(outputId = "table")),
                         tabPanel("Examples of Disciplines and Their Averages",
                                  htmlOutput("Disciplines"),
                                  p(),
                                  textOutput("average"),
                                  p(),
                                  p("To see different types of disciplines,"),
                                  p("Please choose a discipline from side panel :)")),
                         tabPanel("Map of Records Based on Disciplines",
                                  leafletOutput("map"),
                                  textOutput("sta"),
                                  p(),
                                  p("To see athletes' countries based on different disciplines, and minimum record time please use the side panel :)",
                                    "This map will show you the record in terms of discipline and the region of the people who make that record by flags.",
                                    "Feel free to choose any discipline which you are interested and also you can choose the minimum record time.",
                                    "This minimum record time means when you choose a number the map displays the record that you choose with higher records.",
                                    "To add, when there is no record time in the choosen discipline, the map is saying that there are no records for that discipline.")),
                         tabPanel("Records Based on Sex",
                                  plotOutput("boxplot"),
                                  textOutput("boxcomment")),
                         tabPanel("Barplot of Country",
                                  plotOutput("barplot"),
                                  p(),
                                  p("The graph represents the number of athletes from different countries. Russia has the most athletes 
                                    interested in freediving, although Australia, Belgium, and Finland have the smallest number of athletes. ")),
                         tabPanel("References", 
                                  p(),
                                  p("Here is a", a("LINK",  href="https://www.kaggle.com/igalbronshtein/freediving-aida-world-records-dataset"), 
                                    "that you can find our data taken"),
                                  p("Here is a", a("LINK",  href= "https://commons.wikimedia.org/wiki/File:Ben_Noble_(2483696721).jpg"), 
                                    "that you can find our welcome page image taken"))
                         
            ),
            
        )
    ),
    tags$script(
        "
        // define css theme filepaths
        const themes = {
            dark: 'shinythemes/css/darkly.min.css',
            light: 'shinythemes/css/flatly.min.css'
        }

        // function that creates a new link element
        function newLink(theme) {
            let el = document.createElement('link');
            el.setAttribute('rel', 'stylesheet');
            el.setAttribute('text', 'text/css');
            el.setAttribute('href', theme);
            return el;
        }

        // function that remove <link> of current theme by href
        function removeLink(theme) {
            let el = document.querySelector(`link[href='${theme}']`)
            return el.parentNode.removeChild(el);
        }

        // define vars
        const darkTheme = newLink(themes.dark);
        const lightTheme = newLink(themes.light);
        const head = document.getElementsByTagName('head')[0];
        const toggle = document.getElementById('themeToggle');

        // define extra css and add as default
        const extraDarkThemeCSS = '.dataTables_length label, .dataTables_filter label, .dataTables_info {       color: white!important;} .paginate_button { background: white!important;} thead { color: white;}'
        const extraDarkThemeElement = document.createElement('style');
        extraDarkThemeElement.appendChild(document.createTextNode(extraDarkThemeCSS));
        head.appendChild(extraDarkThemeElement);


        // define event - checked === 'light'
        toggle.addEventListener('input', function(event) {
            // if checked, switch to light theme
            if (toggle.checked) {
                removeLink(themes.dark);
                head.removeChild(extraDarkThemeElement);
                head.appendChild(lightTheme);
            }  else {
                // else add darktheme
                removeLink(themes.light);
                head.appendChild(extraDarkThemeElement)
                head.appendChild(darkTheme);
            }
        })
        "
    ),
)


server <- function(input, output) {
    output$str <- renderPrint({
        str(freediving)
    })
    output$summary <- renderPrint({
        summary(freediving)
    })
    output$Disciplines <- renderUI({
        if(input$Disciplines == "DYN"){
            HTML('<iframe width="700" height="400" src="https://www.youtube.com/embed/V1nzf4DpLN0" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
        }else if(input$Disciplines == "DNF"){
            HTML('<iframe width="700" height="400" src="https://www.youtube.com/embed/eRoTBCIgW7c" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
        }else if(input$Disciplines == "DYNB"){
            HTML('<iframe width="700" height="400" src="https://www.youtube.com/embed/bvdMIX-0CLE" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
        }else if(input$Disciplines ==  "NLT"){
            HTML('<iframe width="700" height="400" src="https://www.youtube.com/embed/ierAsmNk4pI" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
        }else if(input$Disciplines ==   "VWT"){
            HTML('<iframe width="700" height="400" src="https://www.youtube.com/embed/8vcyWm5AqUs" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
        }else if(input$Disciplines ==   "CWT"){
            HTML('<iframe width="700" height="400" src="https://www.youtube.com/embed/90KowxrMqJM" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
        }else if(input$Disciplines ==   "CNF"){
            HTML('<iframe width="700" height="400" src="https://www.youtube.com/embed/e_sc7j0XHEk" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
        }else if(input$Disciplines ==    "FIM"){
            HTML('<iframe width="700" height="400" src="https://www.youtube.com/embed/VjCxBNltkIk" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
        }else if(input$Disciplines ==    "CWTB"){
            HTML('<iframe width="700" height="400" src="https://www.youtube.com/embed/OVDJ2_TvUhQ" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
        }
    })
    output$average <- renderText({
        average <- filter(freediving, freediving$discipline == input$Disciplines)
        paste("The average record times of",input$Disciplines, "is", mean(average$record),"minutes.")
    })
    output$map <- renderLeaflet({
        freediving1 <- filter(freediving, input$Disciplines  == freediving$discipline, input$Record <= freediving$record)
        suppressWarnings(if(input$Record <= max(freediving1$record)){
            flag_icons <- makeIcon(
                iconUrl = case_when(
                    freediving1$country == "AT" ~ "country_flags/AT.png",
                    freediving1$country == "AU" ~ "country_flags/AU.png",
                    freediving1$country == "BE" ~ "country_flags/BE.png",
                    freediving1$country == "BR" ~ "country_flags/BR.png",
                    freediving1$country == "CA" ~ "country_flags/CA.png",
                    freediving1$country == "CN" ~ "country_flags/CN.png",
                    freediving1$country == "CO" ~ "country_flags/CO.png",
                    freediving1$country == "CU" ~ "country_flags/CU.png",
                    freediving1$country == "CZ" ~ "country_flags/CZ.png",
                    freediving1$country == "DE" ~ "country_flags/DE.png",
                    freediving1$country == "DK" ~ "country_flags/DK.png",
                    freediving1$country == "FI" ~ "country_flags/FI.png",
                    freediving1$country == "FR" ~ "country_flags/FR.png",
                    freediving1$country == "GB" ~ "country_flags/GB.png",
                    freediving1$country == "GR" ~ "country_flags/GR.png",
                    freediving1$country == "HR" ~ "country_flags/HR.png",
                    freediving1$country == "IT" ~ "country_flags/IT.png",
                    freediving1$country == "JP" ~ "country_flags/JP.png",
                    freediving1$country == "MC" ~ "country_flags/MC.png",
                    freediving1$country == "NL" ~ "country_flags/NL.png",
                    freediving1$country == "NZ" ~ "country_flags/NZ.png",
                    freediving1$country == "PL" ~ "country_flags/PL.png",
                    freediving1$country == "RU" ~ "country_flags/RU.png",
                    freediving1$country == "SE" ~ "country_flags/SE.png",
                    freediving1$country == "SI" ~ "country_flags/SI.png",
                    freediving1$country == "US" ~ "country_flags/US.png",
                    freediving1$country == "VE" ~ "country_flags/VE.png",
                ),
                iconWidth = 25, iconHeight = 25,
                shadowWidth = 10, shadowHeight = 10
            )
            leaflet(freediving1) %>%
                addProviderTiles(providers$Esri) %>% 
                addMarkers(~longtidute, ~latitude,
                           popup = ~country , label = ~country, icon = flag_icons)
        }else{
            leaflet() %>%
                addProviderTiles(providers$Esri) %>% 
                setView(lng = 0, lat =  0 , zoom=1)
            
        })
    })
    output$sta <- renderText({
        freediving1 <- filter(freediving, input$Disciplines  == freediving$discipline, input$Record <= freediving$record)
        suppressWarnings(if(input$Record >= max(freediving1$record)){
            paste("You have selected", input$Disciplines, "and it doesn't contain higher than", input$Record ,"minutes record value")
        })
    })
    output$boxplot <- renderPlot({
        if(input$Sex == "Women"){
            women <- filter(freediving, freediving$sex == "Women")
            ggplot(women,aes(x = sex, y = record)) +
                geom_boxplot(fill = "purple")
        }else if(input$Sex == "Men"){
            men <- filter(freediving, freediving$sex == "Men")
            ggplot(men,aes(x = sex, y = record)) +
                geom_boxplot(fill = "sky blue" )
        }else{
            ggplot(freediving, aes(x = sex, y = record, col = sex)) + 
                geom_boxplot()
        }
    })
    output$boxcomment <- renderText({
        if(input$Sex == "Women"){
            paste("The graph respresents recording time of", input$Sex ,".",
                  "The womans median record time is 100 and the shape of graph is right skewed.","To see different types of sex, please use side panel.")
        }else if(input$Sex == "Men"){
            men <- filter(freediving, freediving$sex == "Men")
            paste("The graph respresents recording time of",input$Sex, ".",
                  "The mens median record time is 120 and the shape of graph is right skewed.",
                  "Also, we can see from the boxplot there are some outliers, that means some mens' record time is too high than other mens.","To see different types of sex, please use side panel.")
        }else{
            paste("The graph respresents recording time of ",input$Sex," and both of them are right skewed.",
                  "These graphs show that Mens recording time bigger than Womens, and there are some outliers in Mens graph.","To see different types of sex, please use side panel.")
        }
    })
    
    output$barplot <- renderPlot({
        ggplot(freediving) +
            geom_bar(mapping = aes(x = country, fill = country)) +
            labs(title = "Bar Plot of Country")
    })
    output$table <- renderDT(freediving)
    
}

shinyApp(ui = ui, server = server)

