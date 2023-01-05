library(ggplot2)
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(dplyr)
library(tidyr)
library(shiny)
library(readxl)
library(gganimate)
library(gifski)
library(summarytools)
library(plotly)
library(tidyverse)
library(janitor)
library(reshape2)
library(gridExtra)
#install.packages('rsconnect')
library(rsconnect)

#connect to account
rsconnect::setAccountInfo(name='merilikonik', 
                          token='ABF27B9721D05E29332CA714AD4D09F8', 
                          secret='Kf3wvzPSVQRNJ3I+NBGnke5UDHq3CxvF0NnM5xsx')



# Load data
#uni_data <- read_excel("Documents/KOOL/Andmetevisualiseerimine/uni_data.xlsx")
#uni_data<-subset(uni_data,select=-c(`Geo Shape`))
#uni<-uni_data
#uni_data<-uni%>%select(-c(PCP,Alumni,Award,PUB,'N&S',"Hi Ci", "National rank", "Geo Point 2D" , "ISO2 CODE"  ,   "ISO3 CODE"))

# Define UI ----
ui <- fluidPage(
  titlePanel("Top 500 maailma ülikooli edetabel 2005-2018"),
  helpText("Aastate 2005-2018 maailma 500 parimate ülikoolide edetabelite visualiseerimine"),
  helpText("Õppejõud: Olga Dunajeva"),
  helpText("Autor: Merili Konik"),
  
  tabsetPanel(
    
    tabPanel("Projekti ja Rakenduse kirjeldus",
             sidebarPanel( img(src = "https://www.inf.elte.hu/media/df/a2/6f4d6b2751caaccacac7a84df0aa17407c379fbeb2434f7d2965ded4515a/arwu-shanghairankings-600x300px-thumb-thumb.jpg", 
                               height = 200, width = 300),
                           br(),
                           br(),
                          p("Andmed on pärit leheküljelt ",
                              a("Open data soft.", 
                                href = "https://public.opendatasoft.com/explore/dataset/shanghai-world-university-ranking/information/?sort=world_rank")),
                          p("Kaardirakenduse jaoks vajaminev andmestik on saadud R paketist", em("rnaturalearth."), 
                            a("Paketi into siin.", 
                              href = "https://cran.r-project.org/web/packages/rnaturalearth/index.html")),
                          width = 4
                          ),
                           
             mainPanel(
               
               h2("Projekti kirjeldus"),
               p("Igal aastal avalikustatakse maailma parimad ülikoolid. Antud projekt visualiseerib", em("Academic Ranking of World Universities,"), 
                 "samuti tuntud kui", em("Shanghai Ranking"), "koostatud edetabelite andmeid, st maailma parimad ülikoole aastatel 2005-2018."),
                       h3("Projekti eesmärgid:"),
p("1. Anda visuaalide ja tabeli kujul ülevaade kasutatavatest andmetest."),
p("2. Visualiseerida, millistest riikidest on top 500 ülikoolid aastatel 2005-2018."), 
p("3. Visualiseerida interaktiivse graafikuga, millised on kümme parimat ülikooli aastatel 2005-2018."),
                       br(),
               h2("Rakenduse kirjeldus"),
               p("Rakenduse annab ülevaate top 500 ülikooli kohta aastatel 2005-2018."),
h3("Rakenduse ülevaade:"),
p("1. Andmed – tunnuste kirjeldused ja andmetabel."),
p("2. Tunnuste ülevaade – graafiline ülevaade valitud tunnuse kohta."),
p("3. Andmed kaardil – Top 500 ülikooli jagunevus riikide vahel. Andmed on esitatud geograafilisel kaardil."),
p("4. Top 10 ülikooli – liikuv tulpdiagramm näitamaks top 10 ülikooli läbi aastate.")

             )
             ),
#esimene paneel
tabPanel("Andmed", 
         sidebarLayout(
           sidebarPanel(h4("Maailma ülikoolide top 500 aastatel 2005-2018"), 
                        br(),
                        br(),
                        p("Algandmestikus on 7105 vaatlus ja 16 tunnust. Kasutatavasse andmestikku on valitud 6 tunnust, vaatluste arv on sama. "),
                        br(),
                        p("Kasutatavad tunnused on:"),
                        p(strong("University"), " - ülikooli täispikk nimi."),
                        p(strong("Total_score"), " - ülikooli saadud skoor. Selle tunnuse puhul on andmetes 5702 puuduvat väärtust."),
                        p(strong("Uni_rank"), " - ülikooli numbriline või vahemikuline koht edetabelis. Peale 100. kohta on edetabelis vahemikud. Vahemikud on 50 või 100 koha kaupa."),
                        p(strong("Year"), " - vastav aasta, millal ülikool antud koha saavutas."),
                        p(strong("W_uni_rank"), " - ülikooli numbriline koht edetablis."),
                        p(strong("Country"), " - ülikooli asukoha riik."),
                        width = 3),
           mainPanel(br(),dataTableOutput("tabel")
                     ) )),  

#tegelt teine paneel
tabPanel("Tunnuste ülevaade", 
         sidebarLayout(
           sidebarPanel(h4("Ülevaade valitud tunnuse kohta"),
                        helpText("Vali tunnus, mille kohta on soov rohkem infot saada."),
                        selectInput("tunnus", "Vali tunnus:",
                                    c(
                                      "Riigid" = "Country",
                                      "Aasta" = "Year",
                                      "Punktiskoor"= "total_score"))
                        ,width = 3),
           mainPanel(
             plotOutput("statistika" , width = "100%")
           ))), 
         
 #Teine paneel   
    tabPanel("Andmed kaardil",
  sidebarLayout(
    sidebarPanel(h4("Top 500 ülikooli asukohta järgi"),
                 helpText("Andmed on geograafilisel kaardil. Valida saab aastat ja piirkonda."),
      sliderInput("aasta_kaardil","Vali aasta", value = 2018,min=2005,max=2018,sep = ""),
      selectInput("piirkonnad", "Vali piirkond:",
                  c("Kõik piirkonnad",
                    "Aasia ja Venemaa" = "Asia",
                    "Euroopa" = "Europe",
                    "Aafrika" = "Africa",
                    "Okeaania"="Oceania",
                    "Põhja- ja Lõuna-Ameerika"= "Americas")), width = 3
    ),
    mainPanel(
      h3(textOutput("selected_var"),align="center"),
      plotOutput("ggPlot" , width = "100%")
    ))),
  # Kolmas paneel
  tabPanel("Top 10 ülikooli",
           sidebarLayout(
             sidebarPanel(h4("Maailma ülikoolide top 10 aastatel 2005-2018"),
                          helpText("Graafiku ilmumine võtab aega kuni 10 sekundit. Palun oota!"),width = 3),
           mainPanel(imageOutput("liikuvplot" , width = "150%")
             )))

#lõpp
))




# Define server logic ----
server <- function(input, output) {

  #Esimene paneel  
  output$tabel <- renderDataTable(uni_data, options =
                                    list(searching = FALSE,ordering=F,
                                         lengthMenu = c(5, 10, 20),
                                         pageLength = 5,scrollX = TRUE))
  
  
  
  
  
  output$selected_var <- renderText({
    paste("Kaadril on näidatud valitud piirkond aastal", input$aasta_kaardil)})
  
  #tegelt teine paneel
    output$statistika <- renderPlot({
    if(input$tunnus =="Country"){
      
      riigid<-uni_data%>%group_by(Country)%>%
        summarise(total_count=n(),
                  .groups = 'drop')
      
      ggplot(data=riigid, aes(x=reorder(Country,total_count), y=total_count)) +
        geom_bar(stat="identity",color="black",fill="light grey", width = 0.7)+
        geom_text(aes(label = total_count), hjust=-0.2)+
        labs(title = "Edetabelid olevate ülikoolide asukohad aastatel 2005-2018 ", x = "Riiki", y = "Mainimiste arv edetabelites kokku") +
        theme(text=element_text(size = 17))+
        coord_flip()
      
    }else if(input$tunnus =="Year"){
      
      aastad<-uni_data%>%group_by(Year)%>%
        summarise(total_count=n(),
                  .groups = 'drop')
      
      ggplot(data=aastad, aes(x=Year, y=total_count)) +
        geom_bar(stat="identity",color="black",fill="light grey")+
        geom_text(aes(label = total_count), hjust = -0.3,
                  position = position_dodge(0.9))+
        labs(title = "Ülikoolide arv aastate kaupa", x = "Aasta", y = "Ülikoole kokku") +
        theme(text=element_text(size = 17))+
        coord_flip()+
        scale_y_continuous( limits=c(0,600))
        
        
    }else if(input$tunnus =="total_score"){
      
      #Andmetöötlus
      colnames(uni_data)[colnames(uni_data) == "Total score"] ="total_score"
      
      ggplot(uni_data, aes(total_score)) + geom_density() +
        labs(title = "Saadud punktiskoorid aastatel 2005-2018",
             subtitle= "Andmetes on 5702 puuduvat väärtust",x = "Skoor", y = "Tihedus")+
        theme(text=element_text(size = 17))+
        scale_x_continuous(n.breaks=10)
      
     }
    }, height=750)
    

  #Teine paneel
  output$ggPlot <- renderPlot({
    #andmetöötlus
    world <- ne_countries(scale = "medium",returnclass = "sf")
    theme_set(theme_bw())
    colnames(world)[colnames(world) == "name_sort"] ="Country"
    
    #Venemaa Aasia alla
    world[world$Country=="Russian Federation",]<-world%>%filter(Country=="Russian Federation")%>%
      mutate(region_un = recode(region_un, "Europe" = 'Asia' ),
             continent=recode(continent, "Europe" = 'Asia' ))
    
    uni_count<-uni_data %>% group_by(Country, Year) %>% 
      summarise(uni_count=n(),.groups = 'drop') %>% 
      as.data.frame()
    
    
    uni_count_a<-uni_count%>%filter(Year==input$aasta_kaardil )
    kaardi_df<-merge(x = world, y = uni_count_a, by = "Country", all = TRUE)
    
    kaart<-kaardi_df%>%filter(region_un==input$piirkonnad)
    
    
    if(input$piirkonnad=="Asia"){
      ymin= -20
      ymax=NA
      xmin=0
      xmax=NA
    }else if(input$piirkonnad=="Europe"){
      ymin= 30
      ymax=NA
      xmin=-40
      xmax=NA
    }else if(input$piirkonnad=="Americas"){
      ymin= -55
      ymax=NA
      xmin=NA
      xmax=0
    }else if(input$piirkonnad=="Africa"){
      ymin= -40
      ymax=NA
      xmin=NA
      xmax=69
    }else if(input$piirkonnad=="Oceania"){
      ymin= -55
      ymax=10
      xmin=100
      xmax=NA
    }
    
    if(input$piirkonnad=="Kõik piirkonnad"){
      ggplot(kaardi_df) +
        geom_sf(aes(fill = uni_count)) +
        scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
        labs(fill = "Ülikoolide arv")+
        theme(
          legend.position = "bottom",
          legend.key.height = unit(0.5, "cm"),
          legend.key.width = unit(2, "cm"),
          text=element_text(size = 15)
        ) +
        guides(fill = guide_colorbar(title.position = "top"))
    } else{
      ggplot(kaart) +
        geom_sf(aes(fill = uni_count)) +
        scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
        labs(fill = "Ülikoolide arv")+
      theme(
          legend.position = "bottom",
          legend.key.height = unit(0.5, "cm"),
          legend.key.width = unit(2, "cm"),
          text=element_text(size = 15)
        ) +
        guides(fill = guide_colorbar(title.position = "top"))+
        ylim(ymin,ymax)+
        xlim(xmin,xmax)
    }
    
    
   # ggplotly(p,tooltip = c("kokku",input$piirkonnad,"nimi"))
  })
  
  
  
  
  
  # Kolmas paneel
  output$liikuvplot <- renderImage(
    {
      #Andmetöötlus
      colnames(uni_data)[colnames(uni_data) == "World rank"] ="uni_rank"
      colnames(uni_data)[colnames(uni_data) == "World rank integer"] ="w_uni_rank"
      colnames(uni_data)[colnames(uni_data) == "Total score"] ="total_score"
      
      uni_formatted <- uni_data %>%
        group_by(Year) %>% # The * 1 makes it possible to have non-integer ranks while sliding
        group_by(University) %>%
        filter(w_uni_rank <=10) %>%
        ungroup()
      
      uni_formatted$uni_rank<-as.numeric(uni_formatted$uni_rank)
      uni_formatted$Year<-as.numeric(uni_formatted$Year)
      uni_formatted$total_score<-round(uni_formatted$total_score,0)
      
      #Liikuv plot
      staticplot = ggplot(uni_formatted, 
                          aes(uni_rank, group = University,
                              fill = as.factor(University), 
                              color = as.factor(University))) +
        geom_tile(aes(y = total_score/2,
                      height = total_score,
                      width = 0.9), alpha = 0.8, color = NA) +
        geom_text(aes(y = 0, label = paste(University, " ")), vjust = 0.2, hjust = 1) +
        geom_text(aes(y=total_score,label=" ", hjust=0)) +
        coord_flip(clip = "off", expand = FALSE) +
        scale_y_continuous(labels = scales::comma) +
        scale_x_reverse() +
        guides(color = FALSE, fill = FALSE) +
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=13),
              legend.position="none",
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.grid.major.x = element_line( size=.1, color="grey" ),
              panel.grid.minor.x = element_line( size=.1, color="grey" ),
              plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=1),
              plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
              plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
              plot.background=element_blank(),
              plot.margin = margin(2,2, 2, 8, "cm"))
      
      
      p = staticplot + transition_states(Year, transition_length = 1000, state_length = 30) +
        view_follow(fixed_x = TRUE)  +
        labs(title = 'Year : {closest_state}',
             subtitle  =  "Top 10 Universities")
      
      
      anim_save("outfile.gif", animate(p)) # New
      
      # Return a list containing the filename
      list(src = "outfile.gif",
           contentType = 'image/gif',
           width = 3000,
            height = 1500
           # alt = "This is alternate text"
      )}, deleteFile = TRUE)
  
  
  
  }


# Run the app ----
shinyApp(ui = ui, server = server)
