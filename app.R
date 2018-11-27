library(shiny)
library(shinythemes)
#http://docs.rstudio.com/shinyapps.io/getting-started.html#deploying-applications
#reset working directory to 
#https://www.youtube.com/watch?v=Y9XZQO1n_7c video for git version controll

#upload databases
table_men <- read.csv('table_men.csv', header = T)
table_women <- read.csv('table_women.csv', header = T)
table_youth <- read.csv('table_youth.csv', header = T)
data <- read.csv('snowboards.csv', header = T)

#create snowboard length search function
snowboard_length <- function(height, weight, gender) {
  if (gender == 'women') table <- table_women
  if (gender == 'men') table <- table_men
  if (gender == 'youth') table <- table_youth
  
  h<-which(table$height == height)
  w<-which(table[h,]$weight == weight)
  size <-table[(h[1]+w),]$length
  if (!is.null(size) && length(size) > 0)
    sizes <- paste(size-2,'-', size+2, " cm.", sep = '')
  else 
    sizes <-'Enter valid height and weight combination'
  return(sizes)
  
}

#create snowboard length text function
snowboard_length_text <- function(height, weight, gender, sizes){
  text = NULL
  if(sizes == 'Enter valid height and weight combination')
    text <- ''
  else 
    text <- paste('Because you are', as.character(height), 'and', as.character(weight),
                     'pounds in the', as.character(gender), 
                     'category, your snowboard length should be around')
  return(text)
  
}


#create snowboard shape search function
snowboard_shape <- function(direction){
  shape = NULL
  if (direction == 'I ride in one direction')
    shape <- c('directional', 'directional twin')
  if (direction == 'I sometimes ride switch') 
    shape <- c('twin', 'directional twin')
  if (direction == 'I often ride switch') 
    shape <- c('twin', 'asymetrical twin')
  
  return(shape)
  
}

#snowboard shape text function
snowboard_shape_text <- function(direction, shape){
  if(direction == 'I ride in one direction')
    direction_text <- 'in one direction most of the time'
  if(direction == 'I sometimes ride switch')
    direction_text <- 'switch some of the time'
  if(direction == 'I often ride switch')
    direction_text <- 'switch most of the time'
  
  shape_text <- paste("Because you board", direction_text, 'a board that is a',
                      '<B>', toupper(shape[1]),'</B>', 'or a','<B>',
                      toupper(shape[2]),'</B>', ' shape would be best for you.')
  
  return(shape_text)
  
}


#create snowboard profile search function
snowboard_profile <- function(riding_style, speed){
  profile = NULL
  if (riding_style == 1)
    profile <- paste('rocker','or', 'flat', sep = ' ') #removed hybrid-flat
  if (riding_style == 2) 
    profile <- paste('rocker','or', 'flat', sep = ' ') #removed hybrid-flat
  if (riding_style == 3) 
    profile <- paste('flat','or', 'hybrid-camber', sep = ' ')
  if (riding_style == 3 && speed == 'The lift up takes longer than my way down') 
    profile <- paste('flat,', 'hybrid-camber,','or', 'camber', sep = ' ')
  if (riding_style == 4) 
    profile <- paste('rocker','or', 'hybrid-rocker', sep = ' ')
  if (riding_style == 5) 
    profile <- paste('hybrid-rocker,', 'flat,','or', 'hybrid-camber', sep = ' ')
  if (riding_style == 6) 
    profile <- paste('hybrid-camber','or', 'camber', sep = ' ')
  if (riding_style == 7) 
    profile <- paste('hybrid-rocker','or', 'hybrid-camber', sep = ' ') #removed s-rocker
  if (riding_style == 8) 
    profile <- paste('hybrid-camber','or', 'hybrid-rocker', sep = ' ')
  
  return(profile)
}  

snowboard_profile_text <- function(style, profile){
  text = NULL
  if(style == 1|2)
    text <- c('Because you are still perfecting your turns', 
              'profile(s) would be best for you because you seek to get up and turning with ease.
              Let\'s face it, you\'re putting in a lot of hard work now so might as well make easier for you to get cruising.
              And don\'t worry... the selfies still look great while sitting on your butt if you\'re strapped in to a snowboard.')
  if(style == 3)
    text <- c('Because you mostly cruise the groomers', 'profile(s) would be best for you because you seek
              fluidity in every steezy turn you make while surfing the mountains corduroy. Yeah brah!')
  if(style == 4)
    text <- c('Because you mostly ride in the begginer terrain park', 'profile(s) would be best for you because
              you need to spin, press, stomp at moments notice while reducing edge catches... assuring more fist pumps, 
              high fives, cheers from the chairlift above.')
  if(style == 5)
    text <- c('Because you mostly ride the advanced terrain park', 'profile(s) would be best for you because you need 
              greater pop, more stability and more durability with all the torture you put that board through!')
  if(style == 6)
    text <- c('Because you mostly carve up the steep groomed terrain', 'profile(s) would be best for you because you 
              need stability at high speeds in order to power though those extreme eurocarve edge angles. Das ist gut!')
  if(style == 7)
    text <- c('Because you mostly ride in the magical forests', 'profile(s) would be best for you because you need
              easier edge transitions throug the trees like sasquatch, yet to float over chop and powder like a fairy.')
  if(style == 8)
    text <- c('Because you\'re a baller and you do it all', 'profile(s) would be best for because you need versatility. 
              Youll get the stability, easier edge transitions, and float over choppy snow and powder. It\'s wizardry 
              the way you make it all look good!')
  
  
  profile_text <- paste(text[1], '<B>', toupper(profile[1:length(profile)-2]), toupper(profile[length(profile)]), 
                        '</B>', text[-1], sep = ' ')
  
  return(profile_text)
  
}

#snowboard_flex function
snowboard_flex <- function(speed, style, days){
  if (speed == 'I stop once or more during a run')
    flex <- paste('Soft flex')
  if (speed == 'I don\'t need to rest during a run') 
    flex <- paste('Soft flex')
  
  if (speed == 'I never get stuck on flat runs' && style == '3') 
    flex <- paste('Medium flex')
  if (speed == 'I never get stuck on flat runs' && style == '4') 
    flex <- paste('Soft-medium flex')
  if (speed == 'I never get stuck on flat runs'&& style == '5') 
    flex <- paste('Medium flex')
  if (speed == 'I never get stuck on flat runs'&& style == '5' && days == 'more than 15 days') 
    flex <- paste('Medium-stiff flex')
  if (speed == 'I never get stuck on flat runs' && style == '6') 
    flex <- paste('Medium-stiff flex')
  if (speed == 'I never get stuck on flat runs' && style == '7') 
    flex <- paste('Soft-medium flex')
  if (style == '7' && days == 'more than 15 days') 
    flex <- paste('Medium-stiff flex')
  if (speed == 'I never get stuck on flat runs' && style == '8') 
    flex <- paste('Medium flex')
  if (speed == 'I never get stuck on flat runs' && style == '8' && days == 'more than 15 days') 
    flex <- paste('Medium-stiff flex')
  
  if (speed == 'The lift up takes longer than my way down' && style == 1|2) 
    flex <- paste('Soft flex')
  if (speed == 'The lift up takes longer than my way down' && style == '3') 
    flex <- paste('Medium-stiff flex')
  if (speed == 'The lift up takes longer than my way down' && style == '4') 
    flex <- paste('Soft-medium flex')
  if (speed == 'The lift up takes longer than my way down' && style == '4' && days == 'more than 15 days') 
    flex <- paste('Medium flex')
  if (speed == 'The lift up takes longer than my way down'&& style == '5') 
    flex <- paste('Medium-stiff flex')
  if (speed == 'The lift up takes longer than my way down' && style == '6') 
    flex <- paste('Medium-stiff flex')
  if (speed == 'The lift up takes longer than my way down' && style == '6' && days == 'more than 15 days') 
    flex <- paste('Stiff flex')
  if (speed == 'The lift up takes longer than my way down' && style == '7') 
    flex <- paste('Medium-stiff flex')
  if (speed == 'The lift up takes longer than my way down' && style == '8') 
    flex <- paste('Medium-stiff flex')
  
  return(flex)
  
}

#snowboard flex text function
snowboard_flex_text <- function(style, flex){
  text = NULL
  if(style == 1|2)
    text <- c('Because you are still perfecting your turns', 
                  'is best for in order to make turning easier and more fluid.')
  if(style == 3)
    text <- paste('Because you mostly cruise the groomers', 'is best 
                  for you to in order to make your turns more graceful and fluid.')
  if(style == 4)
    text <- c('Because you mostly ride in the begginer terrain park', 
                  'is best for you in order to butter, slide, and reduce 
                  edge catches.')
  if(style == 5)
    text <- c('Because you mostly ride the advanced terrain park', 'is 
                  best for you in order to maintain stability on high speed 
                  landings, and to get more pop on jumps and rails.')
  if(style == 6)
    text <- c('Because you mostly carve up the steep groomed terrain', 'is 
                  best for you in order to maintain stability at high speed 
                  and hold extreme edge angles on your eurocarves.')
  if(style == 7)
    text <- c('Because you mostly ride in the magical forests', 'is best
                  for you in order to flex around bumps and trees yet maintain
                  stability on natural kickers and at higher speeds.')
  if(style == 8)
    text <- c('Because you\'re a baller and you do it all', 'is best for you in order to maintain stability at higher speeds, yet keep some flex for easier turns and more pop.')
  
  
  flex_text <- paste(text[1], '<B>', toupper(flex), '</B>', text[2], sep = ' ')
  
  return(flex_text)
  
}



#sidecut function
snowboard_sidecut <- function(style, direction){
  text = NULL
  sidecut = NULL
  
  if (style == 1 | style == 2){
    sidecut <- paste('short-radius sidecut')
  }
  
  if (style == 7){
    sidecut <- paste('progressive sidecut', 'or a', 'short-radius sidecut', sep = ' ')
  }
  
  if (style == 6){
    sidecut <- paste('large-radius sidecut', 'magne-traction', 'or a', 'progressive sidecut', sep = ', ')
  }
  
  if (style == 6 && direction == 'I often ride switch'){
    sidecut <- paste('asymetrical sidecut', 'magne-traction', 'or a', 'progressive sidecut',  sep = ', ')
  }
  
  if (style == 3){
    sidecut <- paste('short-radius sidecut', 'magne-traction', 'or a', 'progressive sidecut', sep = ', ')
   }
  
  if (style == 4 | style == 5){
    sidecut <- paste('short-radius sidecut', 'magne-traction', 'or a', 'progressive sidecut', sep = ', ')
   }
  
  if (style == 8){
    sidecut <- paste('magne-traction', 'or a', 'progressive sidecut', sep = ', ')
     }
  
  return(sidecut)
  
  }


#sidecut function
snowboard_sidecut_text <- function(style, direction){
  text = NULL
  sidecut = NULL
  sidecut <- snowboard_sidecut(style,direction)
  
  if (style == 1 | style == 2){
    text <- 'would be the best sidecut for you because need a board that helps for smooth transitions between turns.'
  }
  if (style == 7){
    text <- 'would be best for you because you need a board that turns on a dime.'
  }
  
  if (style == 6){
    text <- 'bould be best for you because you need a board that performs at higher speeds.'
  }
  
  if (style == 6 && direction == 'I often ride switch'){
    text <- 'would be best for you becuase you need a board that performs at high speeds 
    while riding switch.'
  }
  
  if (style == 3){
    text <- 'would be the best sidecut for you because, as you learn to ride mindlessly, you need a board
    that can make smaller turns, yet maintain stability at higher speeds.'
  }
  
  if (style == 4 | style == 5){
    text <- 'would be the best sidecut for you because you need a board that maintains stability 
    in landings and pipe, yet butters and spins smoothly.'
  }
  
  if (style == 8){
    text <- 'would be the best sidecut for you because you need a board that maintains stability in speed, yet turns effortlessly.'
  }
  
  sidecut_text <- paste('A', '<B>', toupper(sidecut), '</B>', text)
  
  
  return(sidecut_text)
  
}

#function to subset snowboard PROFILE
  #profile categories: "hybrid-camber or hybrid-rocker", "hybrid-rocker or hybrid-camber", 
  #"hybrid-camber or camber", "hybrid-rocker, flat, or hybrid-camber", "rocker or hybrid-rocker"
  #"flat, hybrid-camber, or camber", "flat or hybrid-camber", "rocker or flat"

strip_profile <- function(profile){
  profile_stri <- gsub(' or', '', profile)
  profile_strip <- gsub(',', '', profile_stri)
  profile_stripp <- strsplit(profile_strip, ' ')
  return(profile_stripp)
}

subset_profile <- function(profile, data){
  profile_tidy <- strip_profile(profile)
  subset <- data[data[,6] %in% c(profile_tidy[[1]]),]
  return(subset)
}


# function to subset snowboard FLEX
# flex categories: 'Soft flex', 'Medium flex', 'Soft-medium flex', 'Medium-stiff flex', 'Stiff flex'
strip_flex <- function(flex){
  flex_stri <- flex_stripped <- gsub(' flex', '', flex) # remove word 'flex' from string
  flex_stripped <- flex_stripped <- gsub('-', ', ', flex_stri) #replace '-' with ', ' in string
  return(tolower(flex_stripped))
}

subset_flex <- function(flex, data) {
  flex_stripped <- strip_flex(flex)
  subset <- data[grepl(flex_stripped, data[,7]),]
  return(subset)
}


#function to subset snowboard DIRECTION
subset_shape <- function(shape, data) {
  data[data[,5] %in% c(shape),]
}


#subset all flex, shape, profile
subset_all <- function(profile, shape, flex, data){
  subset_1 <- subset_profile(profile, data)
  subset_2 <- subset_shape(shape, subset_1)
  subset_3 <- subset_flex(flex, subset_2)
  NA_rows <- apply(subset_3, 1, function(x) all(is.na(x)))
  if (sum(!NA_rows) == 0){ 
    recommendation <- 'Product recommendation coming soon'
  } else { recommendation <- subset_3[!NA_rows,] }
  return(subset_2)
}

# Define UI 
ui <- fluidPage(
  
  fluidRow(
    column(width=12, tags$img(src = "default2.jpeg")),
    column(width = 12, 
           h1(strong("SNOWBOARD FINDER APP")),h4('find the snowboard that is best for you'),
           offset = 0
           )
    ),
  
  # sidebar layout with a input and output def ----
  sidebarLayout(
    
    sidebarPanel(
      selectInput('gender', h4(strong('Sizing for:')),
                  choices = c('men', 'women', 'youth')
                  ),
      
      h4(strong('Weight')),
      inputPanel(
      sliderInput('weight', 'lbs', value = NA, min = 40, max = 290)
      ),
      h4(strong('Height')),
      inputPanel(
        sliderInput('height1', 'feet', value = NA, min = 3, max = 6),
        sliderInput('height2', 'inches', value = NA, min = 0, max = 11, step = 0.5)
      ),
      selectInput('level', h4(strong('Your Level')), 
                     choices = c('begginer' = 'rocker.png',
                                 'intermediate' = 'camber.jpg',
                                 'advanced' = 'hybrids.gif')),
      
      h4(strong('Riding Style')),
      selectInput('style', h5('I am mostly:'),
                   choices = c('perfecting turns on greens'= '1',
                               'perfecting turns on blues'= '2',
                               'on any groomed run' = '3',
                               'in the begginer terrain park' = '4',
                               'in the advanced terrain park' = '5',
                               'carving fast down blue runs' = '6',
                               'in the trees and sidecountry' = '7',
                               'in the park, groomers, and trees equally' = '8')
                  ),
      
      radioButtons('direction', h4(strong('Direction')),
                   choices = c('I ride in one direction'= 'I ride in one direction',
                               'I sometimes ride switch'= 'I sometimes ride switch',
                               'I often ride switch'= 'I often ride switch'),
                   inline = F),
      
      selectInput('speed', h4(strong('Speed')),
                   choices = c('I stop once or more during a run',
                               'I don\'t need to rest during a run',
                               'I never get stuck on flat runs',
                               'The lift up takes longer than my way down')
                   ),
      
      radioButtons('days', h4(strong('Days I board in a Season')),
                   choices = c('less than 15 days',
                               'more than 15 days'),
                   inline = F)
      
      ),

      mainPanel(
        tags$style("body {background-color: lightblue; }"),
        
        wellPanel(
          h2(strong('RESULTS'), align = 'center'),
                  
          h4('Any of these combinations may work for you:', align = 'center'),
          tags$head(HTML("<!-- Global site tag (gtag.js) - Google Analytics -->
<script async src=\"https://www.googletagmanager.com/gtag/js?id=UA-107415974-2\"></script>
                         <script>
                         window.dataLayer = window.dataLayer || [];
                         function gtag(){dataLayer.push(arguments);}
                         gtag('js', new Date());
                         
                         gtag('config', 'UA-107415974-2');
                         </script>"))

          ),
        
        wellPanel(
          h2('Length'),
        
          htmlOutput('length_text'),
          
          tags$img(src = 'length.png')
        ),
        
        wellPanel(
          h2('Profile'),
          
          htmlOutput('profile_text'),
          
          fluidRow(
            column(4, HTML("<div style='height: 141px;'>"),imageOutput('profile_img1'), 
                   HTML("</div>")
                   ),
            
            column(7, HTML("<div align='right', style='height: 141px;'>"), imageOutput('profile_img2'),
                   HTML("</div>")
                   )
            )
          
          ),
        
        wellPanel(
          h2('Shape/Direction'),
        
          htmlOutput('shape_text'),
          
          fluidRow(
            column(4, HTML("<div style='height: 141px;'>"),imageOutput('shape_img1'), 
                   HTML("</div>")
            ),
            
            column(7, HTML("<div align='right', style='height: 141px;'>"),imageOutput('shape_img2'), 
                   HTML("</div>")
            )
          )
          
          ),
        
        wellPanel(
          h2('Flex'),
        
          htmlOutput("flex_text"),
          
          tags$img(src = 'flex.jpg')
          ),
        
        wellPanel(
          h2('Sidecut'),
        
          htmlOutput('sidecut_text'),
          
          fluidRow(
            column(4, HTML("<div style='height: 141px;'>"),tags$img(src = 'short-radius sidecut.png'), 
                   HTML("</div>")),
            
            column(7, HTML("<div align='right', style='height: 141px;'>"),tags$img(src = 'long-radius sidecut.png'), 
                   HTML("</div>"))
                   ),
          fluidRow(
            column(4, HTML("<div style='height: 141px;'>"),tags$img(src = 'progressive sidecut.png'), 
                   HTML("</div>")),
            column(7, HTML("<div align='right', style='height: 141px;'>"),tags$img(src = 'magne-traction.png'), 
                   HTML("</div>"))
                   )
          
          ),
        
        wellPanel(
          h2('Snowboard Recommendations'),
          
          tableOutput('recommendation')
        ),
        
        h4(strong('Notice:')),
        h4('These are recommendations. Consult your local snowboard shop for a board that is right for you.')
        )
  
)
)


#Define server logic
server <- function(input, output) {
  
  #profile <- snowboard_profile(input$style, input$speed)
  #shape <- snowboard_shape(input$direction)
  #flex <- reactive({snowboard_flex(input$speed, input$style, input$days)})
  
  output$profile_img1 <- renderImage({
    profile <- snowboard_profile(input$style, input$speed)
    text <- unlist(strsplit(profile, ' '))
    
    if (length(text) == 3)
      image_text <- paste(text[1], '.png', sep ='')
    if (length(text) == 4)
      text <- gsub(',', '', text)
    image_text <- paste(text[1], '.png', sep ='')
    
    filename <- normalizePath(file.path('./www', image_text))
    
    list(src = filename,
         width = 351,
         height = 141)
    
    }, deleteFile = FALSE)
  
  output$profile_img2 <- renderImage({
    profile <- snowboard_profile(input$style, input$speed)
    text <- unlist(strsplit(profile, ' '))
    text <- gsub(',', '', text)
    
    if (length(text) == 3)
      image_text <- paste(text[3], '.png', sep ='')
    if (length(text) == 4)
      image_text <- paste(text[2], '.png', sep ='')
    
    filename <- normalizePath(file.path('./www', image_text))
    
    list(src = filename, width = 351, height = 141)
    
  }, deleteFile = FALSE)
  
  output$length_text <- renderText({
    length <- snowboard_length(paste(input$height1, round(input$height2),
                                     sep = ','), round(input$weight),
                               input$gender)
    
    text <- snowboard_length_text(paste(input$height1,'feet', round(input$height2),
                                        'inches'),
                                  input$weight, input$gender, length)
    
    length_text <- paste(text, '<B>',length, '</B>')
    
    return(length_text)
    
  })
  
   output$profile_text <- renderText({
     
    profile <- snowboard_profile(input$style, input$speed)
      
    profile_text <- snowboard_profile_text(input$style, profile)
    
    return(profile_text)
    
   }) 
   
   output$shape_text <- renderText({
     
     shape <- snowboard_shape(input$direction)
     
     shape_text <- snowboard_shape_text(input$direction, shape)
     
     return(shape_text)
    
    })
   
   output$shape_img1 <- renderImage({
     shape <- snowboard_shape(input$direction)
     
     text <- unlist(strsplit(shape, ' ')) #split on space
     
     image_text <- paste(text[1], '.png', sep ='')
     
     filename <- normalizePath(file.path('./www', image_text), mustWork = TRUE)
     
     list(src = filename,
          width = 351,
          height = 141)
     
   }, deleteFile = FALSE)
     
   output$shape_img2 <- renderImage({
     shape <- snowboard_shape(input$direction)
     text <- unlist(strsplit(shape, ' ')) #split on space
     
     image_text <- paste(text[2], '.png', sep ='')
     
     filename <- normalizePath(file.path('./www', image_text), mustWork = TRUE)
     
     list(src = filename,
          width = 351,
          height = 141)
     
   }, deleteFile = FALSE)
   
   output$flex_text <- renderText({
     
      flex <- snowboard_flex(input$speed, input$style, input$days)
         
      flex_text <- snowboard_flex_text(input$style, flex)
         
      return(flex_text)
     })
     
     
   output$sidecut_text <- renderText({
      sidecut_text <- snowboard_sidecut_text(input$style, input$direction)
         
      print(sidecut_text)
     })
  
   output$sidecut_img1 <- renderImage({
     sidecut <- snowboard_sidecut(input$style, input$direction)
     text <- unlist(strsplit(sidecut, ',')) #split on space
     if (length(text) == 1)
       text <- unlist(strsplit(sidecut, ' or a'))
     
     image_text <- paste(text[1], '.png', sep ='')
     
     filename <- normalizePath(file.path('./www', image_text), mustWork = TRUE)
     
     list(src = filename,
          width = 351,
          height = 141)
     
   }, deleteFile = FALSE)
   
   output$sidecut_img2 <- renderImage({
     sidecut <- snowboard_sidecut(input$style, input$direction)
     text <- unlist(strsplit(sidecut, ',')) #split on space
     if (length(text) == 1)
       text <- unlist(strsplit(sidecut, ' or a'))
     
     image_text <- paste(text[2], '.png', sep ='')
     
     filename <- normalizePath(file.path('./www', image_text), mustWork = TRUE)
     
     list(src = filename,
          width = 351,
          height = 141)
     
   }, deleteFile = FALSE)
   output$recommendation <- renderTable({
     
     shape <- snowboard_shape(input$direction)
     
     profile <- snowboard_profile(input$style, input$speed)
     
     flex <- snowboard_flex(input$speed, input$style, input$days)
     
     subset_1 <- subset_profile(profile, data)
     
     subset_2 <- subset_shape(shape, subset_1)
     
     subset_3 <- subset_flex(flex, subset_2)
     
     NA_rows <- apply(subset_3, 1, function(x) all(is.na(x)))
     if (sum(!NA_rows) == 0){ 
       recommendation <- 'Product recommendation coming soon'
     
     } else {
         subset_final <- subset_3[!NA_rows,] 
         recommendation <- subset_final[,2:3]
      }
     
     #recommendation <- subset_all(profile, flex, shape, data)
       #subset_profile(profile, data)
       #subset_shape(shape, data)
       #subset_flex(flex, data)
       #subset_all(profile, flex, shape, data)
     
     return(recommendation)
     })
}
    

shinyApp(ui, server)