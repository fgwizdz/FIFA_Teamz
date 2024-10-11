
library(shiny)
library(DT)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(leaflet)
library(maps)
library(rnaturalearth)
library(leaflet.extras)
library(shinyjs)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(reactable)
library(tidyr)
library(formattable)
library(DiagrammeR)
library(png)
library(jpeg)

#Definicja interfejsu użytkownika
ui <- fluidPage(
  tags$head(
    
    tags$style(HTML("
    
  .flag-box {
        display: flex;
        flex-direction: column;
        justify-content: flex-end;
        text-align: center;
        height: 100px; 
  }
      
.nav-tabs {
  background-color: #00D7FF; 
  color: #FFFFFF; 
  text-transform: uppercase; 
}
  /* Nagłówki h1 */
  h1 {
    margin-bottom: 85px; 
    font-size: 36px; 
    font-weight: bold;
    color: #225964; 
    text-align: center;
  }


  body { 
  background-image: url('tło.jpg'); 
    background-color: #E6E6FA; 
    background-size: cover;
    background-position: center center; 
    padding-bottom: 350px;
    text-align: center; 
    font-family: Arial, sans-serif;
  }

 
  .main-text {
    color: #333333;
  }


  .tytul {
    color: #003F4A;  
    padding: 20px;  
    text-align: center;  
    font-size: 65px;  
    font-weight: bold;  
    font-style: italic;
  }
  


  h2 {
    margin-top: 85px;
    font-size: 38px;
    font-weight: bold;
    color: #333;
    text-align: center;
    font-family: 'Abyssinica SIL';
  }

  /* Nagłówki h3 */
  h3 {
    margin-top: 65px;
    font-size: 24px; 
    font-weight: bold;
    color: #333;
    text-align: center;
  }


  #mapa-container {
    border: 26px solid #48D1CC; 
    margin: 1px;
    padding: 5px;
  }


  .custom-table-container2 {
  text-align: center;
    display: flex;
    justify-content: center;
    border: 1px solid #ddd; 
    margin: 10px;
    width: 98%;
    overflow-x: auto;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1); 
    border-radius: 5px; 
  }


  .custom-table-container2 .table {
    width: 100%;
    border-collapse: collapse;
  }


  .custom-table-container2 .table th {
    background-color: #4CAF50; 
    color: white; 
    padding: 12px 8px;
  }


  .custom-table-container2 .table td {
    border: 1px solid #ddd; 
    padding: 8px;
    text-align: left;
  }


  .custom-table-container2 .table tr:hover {
    background-color: #ddd;
  }


  .custom-table-container2 .table tr:nth-child(even) {
    background-color: #f2f2f2;
  }


  .center-div {
    display: flex;
    justify-content: center;
    
  }


  .reactable {
    font-size: 18px;
    color: blue;
    margin: 0 auto; 
    width: 80%;
  }
   .centered-sidebar {
      display: flex;
      flex-direction: column;
      align-items: center;
    }

 
  .reactable-header {
    background-color: #4CAF50;
    color: white;
    text-align: center;
  }

 
  .reactable-cell {
    text-align: center;
    padding: 10px;
  }

 
  .table-container {
    display: flex;
    justify-content: center;
    align-items: center;
    height: 100%;
  }

 
  .reactable {
    background-color: transparent;  
    color: #000;                   
  }
  
  .reactable .rt-tr-group {
    background-color: transparent;  
  }
  
  .reactable .rt-th,
  .reactable .rt-td {
    border-color: #ddd;            
  }
  
  
  .stadion-box {
      padding: 10px;
      border: 2px solid #00D7FF;
      border-radius: 20px;
      background-color: #f9f9f9;
      margin-bottom: 10px;  
      box-shadow: 2px 2px 5px rgba(0,0,0,0.1);
      text-align: center;
      transition: all 0.3s ease-in-out;
    }
    .stadion-box:hover {
      background-color: #00D7FF;
      color: white;
      cursor: pointer;
    }
    

    .btn-primary {
      background-color: #007BFF;
      color: white;
      border-radius: 20px;
      padding: 10px 20px;
      border: none;
      transition: background-color 0.3s ease-in-out;
    }
    .btn-primary:hover {
      background-color: #0056b3;
    }
    

    .center-div, .centered-content {
      display: flex;
      justify-content: center;
      flex-direction: column;
      align-items: center;
    }
    

    .stadium-details {
      text-align: center;
      font-size: 18px;
      color: #333;
      margin-top: 20px;
      line-height: 1.6;
    }
    

    .styled-table {
      margin: 20px auto;
      border-collapse: collapse;
      width: 80%;
      background-color: #f4f4f4;
      text-align: center;
      border: 1px solid #ddd;
      border-radius: 10px;
      padding: 10px;
      box-shadow: 2px 2px 5px rgba(0,0,0,0.1);
    }
    .styled-table th, .styled-table td {
      padding: 12px;
      border-bottom: 1px solid #ddd;
    }
    .styled-table th {
      font-weight: bold;
      background-color: #00D7FF;
      color: white;
      border-radius: 10px 10px 0 0;
    }
    .styled-table tr:last-child td {
      border-bottom: none;
      border-radius: 0 0 10px 10px;
    }
    

    .title-text {
      font-family: 'Arial', sans-serif;
      font-size: 24px;
      font-weight: bold;
      color: #225964;
      text-align: center;
      margin-bottom: 20px;
    }
    
  .map-description {
    padding: 15px;
    border: 2px solid #00BFFF;  
    border-radius: 10px; 
    background-color: #ffffff; 
    box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);  
    margin: auto;  
    width: 80%;  
    text-align: center;  
    margin-bottom: 20px; 
  }

  .map-title {
    color: #225964;  
    text-align: center;  
    margin-bottom: 10px;  
  }

  .map-container {
    display: flex;
    justify-content: center;
  }

  .leaflet-container {
    border: 2px solid #00BFFF;  
    border-radius: 10px; 
  }

  .stadium-details {
    text-align: center;
  }

  .title-text {
    color: #225964;
    text-align: center;
  }
    
    
    
    
"))
  ),
  
  
  

  
  # Panel tytułowy i opis aplikacji
  div(class = "container-fluid",
      div(class = "tytul",
          "FIFA Teamz"
      )),
  
  # Zawartość strony głównej
  tabsetPanel(
    id = "tabs",
    tabPanel("Strona Główna",
             column(12,
                    div(class = "container",
                        titlePanel("FIFA Teamz - Twoje narzędzie do analizy Euro 2024"),
                        br(), br(),
                        
                        
                        p("FIFA Teamz to zaawansowana aplikacja webowa stworzona w RStudio przy użyciu frameworku Shiny, która pozwala na analizę danych związanych z Euro 2024."), 
                          p("Dzięki tej aplikacji możesz śledzić wyniki, rankingi FIFA, zobaczyć, którzy trenerzy prowadzą poszczególne reprezentacje narodowe, a także stworzyć swoją wymarzoną drużynę."),
                        
                        
                        tags$br(),
                        h2("Dlaczego warto korzystać z FIFA Teamz?"),
                        p("Euro 2024 to jedno z najważniejszych wydarzeń piłkarskich, a dane odgrywają kluczową rolę w zrozumieniu wyników i analizie wydarzeń. "),
                          p("FIFA Teamz dostarcza kibicom narzędzi do śledzenia rozgrywek, analizy wyników, przeglądania rankingów FIFA oraz poznania aktualnych trenerów prowadzących poszczególne drużyny narodowe."),
                        
                        
                        tags$br(),
                        h2("Co oferuje FIFA Teamz?"),
                        tags$ul(
                          style = "list-style-type: none;",
                          tags$li(tags$span("★ "), "Śledzenie Euro 2024: Zobacz, jak przebiegały eliminacje i turniej główny."),
                          tags$li(tags$span("★ "), "Rankingi FIFA: Przeglądaj aktualne rankingi FIFA i porównuj drużyny."),
                          tags$li(tags$span("★ "), "Analizy Trenerskie: Sprawdź, którzy trenerzy prowadzą dane reprezentacje i ich wpływ na wyniki drużyn."),
                          tags$li(tags$span("★ "), "Tworzenie Kart Zawodników: Wygeneruj swoją kartę zawodnika, wybierając statystyki i zdjęcia."),
                          tags$li(tags$span("★ "), "Zespół Marzeń: Stwórz swoją drużynę marzeń, wybierając najlepszych zawodników na każdą pozycję."),
                          tags$li(tags$span("★ "), "Geograficzne Wizualizacje: Zobacz, jak różne drużyny rozlokowane są na mapie Europy."),
                          tags$li(tags$span("★ "), "Generowanie Raportów: Twórz i eksportuj raporty z wynikami w formatach takich jak PDF i Excel.")
                        ),
                        
                        
                       
                        tags$br(),
                        h2("Jak zacząć?"),
                        p("Korzystanie z FIFA Teamz jest łatwe i intuicyjne. Wybierz interesującą Cię kategorię z zakładek powyżej i rozpocznij eksplorację!"),
                        p("Możesz tworzyć własne drużyny, analizować wyniki oraz tworzyć raporty."),
                        p("Rozpocznij swoją podróż od wyboru interesującej Cię kategorii, skorzystaj z interaktywnych narzędzi, aby przeglądać dane i odkrywać fascynujące informacje o Euro 2024."),
                        
                        tags$br(),
                        h2("Odkryj świat danych piłkarskich!"),
                        p("Zanurz się w szczegółowej analizie, odkrywaj nowe spostrzeżenia i pozostań na bieżąco z najnowszymi trendami piłkarskimi dzięki FIFA Teamz."),
                        h2("Po przejrzeniu całej aplikacji webowej, zostaw opinie na temat strony!" ),
                        tags$div(style = "display: flex; justify-content: center; align-items: center; flex-direction: column; height: 200px;",
                                 textInput("user_comment", "Wpisz swój komentarz", width = "50%"),
                                 actionButton("submit_comment", "Wyślij komentarz")
                        ),
                        h2("Komentarze użytkowników"),
                        tableOutput("comments_table")
                        
                    )
             )
    ),
    
  

    
    
                    #druga tabela
                    tabPanel("Reprezentacje i Trenerzy",
                             titlePanel("Flagi Reprezentacji"),
                             fluidRow(
                               column(12,
                                      h3("Kliknij na ikonkę flagi, aby zobaczyć informacje o reprezentacji"),
                                      br(),
                                      uiOutput("flagGrid")
                               )
                             ),
                    h2(strong("Reprezentacje oraz Trenerzy")),
                    DTOutput("tabelaDruzyny")
                    ),
                    
    
    
    
    
    
    
    
    
             # 3 tabela Dodaj elementy drugiej strony
  
    
  tabPanel("Zawodnicy",
           titlePanel("Zawodnicy"),
           br(),
           p("Wybierz reprezentację z rozwijanego menu poniżej, aby zobaczyć szczegółowe informacje o zawodnikach danej drużyny."),
           p("Znajdziesz tu dane takie jak imię i nazwisko, data urodzenia, wiek, pozycja oraz klub, w którym aktualnie grają."),
           tags$br(),
           tags$br(),
           tags$br(),
           tags$br(),
           fluidRow(
             column(width = 12, align = "center",  
                    selectInput("wybranaDruzyna", "Wybierz drużynę:",
                                choices = unique(dane_polaczone$nazwa_druzyny))
             )
           ),
           fluidRow(
             column(width = 12,
                    tableOutput("tabelaZawodnikow"), class = "custom-table-container2"
             )
           ),
           h3(strong("Kluby Zawodników")),
           p("Wybierz klub piłkarski, aby zobaczyć, którzy zawodnicy z reprezentacji narodowych występują w jego barwach."),
           tags$br(),
           fluidRow(
             column(width = 12, align = "center",  
                    selectInput("wybrany_klub", "Wybierz Klub:", choices = unique(dane_polaczone$klub))
             )
           ),
           tags$br(),
           fluidRow(
             column(width = 12,
                    tableOutput("tabelakluby"), class = "custom-table-container2"
             )
           ),
           textOutput("iloscPiłkarzy"),
           tags$br(),tags$br(),tags$br(), 
  ),
    
    
    
    
    #4 MAPA KRAJÓW
    
    
                   tabPanel("Mapa krajów",
                            titlePanel("Mapa krajów"),
                            p("Mapę możesz przybliżać oraz oddalać za pomocą interaktywnych przycisków znajdujących się po lewej stronie mapy, bądź za pośrednictwem kursora."),
                            tags$br(),
                            p("Możesz również wybrać Reprezentacje z listy, aby automatycznie przybliżyło na kraj."),
                            tags$br(),
                            p("Proszę o kliknięcie markera, aby zapoznać się z danymi."),
                            tags$br(),
                            
                            fluidRow(
                              column(5, offset = 5, 
                                     selectInput("wybor_druzyny", "Wybierz drużynę:", choices = dane_druzyny$nazwa_druzyny)
                              ),
                              tags$br(),
                            ),
                            
                            div(leafletOutput("mapa", width = "100%", height = "700px"), id = "mapa-container"),
                            textOutput("kraj_info")
                   ),
    
    
  
    #piłkarze poprzez wiek

    
    tabPanel("Piłkarze poprzez Wiek",
             h2(strong("Analiza wieku piłkarzy")),
             p("Na tej stronie możesz analizować piłkarzy na dwa różne sposoby:"),
             
             h3("1. Filtrowanie według wieku:"),
             p("Użyj poniższych suwaków, aby ustawić minimalny i maksymalny wiek piłkarzy. 
            Po ustawieniu przedziału wiekowego wyświetli się tabela zawierająca szczegółowe informacje 
            o piłkarzach, którzy mieszczą się w wybranym zakresie wiekowym."),
             
           
             actionButton("reset_filters", "Resetuj filtry", class = "btn"),
             br(), br(),
             
             div(class = "center-div",
                 sliderInput("min_wiek", "Minimalny wiek:", 
                             min = min(dane_zawodnicy$wiek), 
                             max = max(dane_zawodnicy$wiek), 
                             value = min(dane_zawodnicy$wiek))),
             div(class = "center-div",
                 sliderInput("max_wiek", "Maksymalny wiek:", 
                             min = min(dane_zawodnicy$wiek), 
                             max = max(dane_zawodnicy$wiek), 
                             value = max(dane_zawodnicy$wiek))),
             
             h2("Tabela z danymi piłkarzy"),
             dataTableOutput("tabela_wiek"),
             
             h3("2. Analiza rozkładu wiekowego:"),
             p("W aplikacji dostępne są dwa rodzaje histogramów, które pomagają wizualizować dane dotyczące piłkarzy w różnych klubach piłkarskich:"),
             
             p("1. Histogram według wieku – Ten histogram przedstawia liczbę piłkarzy w podziale na różne przedziały wiekowe. Dzięki temu możesz zobaczyć, ile zawodników zarejestrowano w danym wieku. Klikając na wybrany słupek histogramu, możesz wyświetlić szczegóły, takie jak liczba piłkarzy w danym wieku i ich nazwiska."),
             
             p("2. Histogram według klubów – Ten histogram pokazuje liczbę piłkarzy przypisanych do poszczególnych klubów. Pozwala on łatwo ocenić, które kluby mają największą liczbę zawodników. Kliknięcie na nazwę klubu w histogramie wyświetli szczegóły dotyczące zawodników w tym klubie."),
             
             p("Możesz przełączać się między tymi dwoma histogramami za pomocą rozwijanego menu, aby lepiej poznać statystyki dotyczące wieku zawodników oraz ich przynależności klubowej."),
             
             h2("Histogramy"),
             fluidRow(
             column(12, align = "center",
             selectInput("wybor_histogramu", "Wybierz histogram", choices = c("Wiek", "Kluby")))),
             plotlyOutput("histogramPlot"),
             
             h3("Szczegóły wybranego wieku"),
             verbatimTextOutput("details"),
             
             uiOutput("lista_pilkarzy"),
             br(),br(),br(),br(),br(),br(),br(),br()
             
    ),
    

    
    
    
    
    
    
                
    tabPanel("Analiza Rankingów FIFA",
             
             ui <- dashboardPage(
               dashboardHeader(title = "Interaktywna Analiza Rankingów FIFA"),
               dashboardSidebar(
                 sidebarMenu(
                   menuItem("Wykres", tabName = "wykres", icon = icon("bar-chart")),
                   menuItem("Opis", tabName = "opis", icon = icon("info-circle"))
                 )
               ),
               dashboardBody(
                 tabItems(
                   tabItem(tabName = "wykres",
                           fluidRow(
                             box(
                               title = "Wykres Rankingów FIFA", 
                               status = "primary", 
                               solidHeader = TRUE, 
                               plotlyOutput("wykresInteraktywny", height = "500px"), 
                               width = 12
                             ),
                             box(
                               title = "Informacje o drużynach",
                               status = "info",
                               solidHeader = TRUE,
                               DT::dataTableOutput("tabelaDruzyn"),
                               width = 12
                             )
                           )
                   ),
                   tabItem(tabName = "opis",
                           fluidRow(
                             box(
                               title = "Opis Analizy",
                               status = "primary",
                               solidHeader = TRUE,
                               width = 12,
                               p("Opis Wykresu Rankingów FIFA"),
                               p("Prezentowany wykres interaktywny przedstawia zależność między 
              oficjalnym rankingiem FIFA a przyznanymi punktami dla drużyn piłkarskich. 
              Ranking FIFA jest międzynarodowym systemem rankingowym stworzonym przez Fédération Internationale de Football Association (FIFA), 
              który klasyfikuje drużyny piłkarskie na świecie."),
                               p("Na osi poziomej wykresu znajduje się ranking FIFA, 
              gdzie numer 1 reprezentuje drużynę z najwyższą oceną. 
              Oś pionowa reprezentuje punkty FIFA, które są przyznawane na podstawie wyników meczów międzynarodowych.
              Im więcej punktów, tym wyższa pozycja drużyny w rankingach międzynarodowych."),
                               p("Interaktywność wykresu pozwala użytkownikowi na uzyskanie szczegółowych informacji 
              o każdej drużynie poprzez najechanie kursorem na reprezentujący ją punkt. 
              Dzięki temu można szybko zidentyfikować nazwę drużyny, jej aktualny ranking oraz liczbę punktów FIFA."),
                               p("Wykres jest pomocnym narzędziem do analizy tendencji i porównywania pozycji drużyn,
              a także do obserwacji zmian w czasie. Może być wykorzystany przez analityków sportowych,
              komentatorów, a także przez fanów piłki nożnej zainteresowanych statystykami i rankingami."),
                               br()
                             )
                           )
                   )
                 )
               )
             )),
  
    
    tabPanel(
      "Euro 2024",
      titlePanel("Wielki Finał i Historia Euro 2024"),
      p(
        "Mistrzostwa Europy w piłce nożnej EURO 2024, które odbyły się w Niemczech, zakończyły się niezapomnianym finałem, 
      gdzie w emocjonującym meczu zwyciężyła drużyna ",
      strong("Hiszpanii"), 
      ".",
 br(),
      img(src = "Flag_of_Spain.gif", height = "90px"),
 
      ),
      p(
        "Łącznie 24 drużyny z całej Europy walczyły o tytuł najlepszej drużyny kontynentu, a turniej obfitował w niesamowite 
      momenty i zaciętą rywalizację."
      ),
      br(),
      p("Kliknij poniżej, aby zobaczyć pełną listę drużyn, które brały udział w turnieju."),
      br(),
      actionButton("show", "Pokaż drużyny"),
      tags$br(), tags$br(), tags$br(), tags$br(),
      div(
      class = "table-container",
      reactableOutput("euroTable")  
  ),
  
  h2("Tabela z informacjami o Eliminacjach"),
  br(),
  p("Eliminacje do Euro 2024 trwały od marca 2023 do marca 2024 roku, z udziałem 53 reprezentacji walczących o 20 miejsc w turnieju."),
  p("Niemcy, jako gospodarz, mieli zagwarantowany udział, a dodatkowe 3 miejsca przyznano najlepszym drużynom, które nie zakwalifikowały się bezpośrednio,
    z każdej dywizji Ligi Narodów UEFA."),
    p("Losowanie grup eliminacyjnych odbyło się 9 października 2022 roku."),
  br(),
  formattableOutput("table3"),
  
  h2("Tabela z informacjami jakie drużyny się zakwalifikowały"),
  p("Niemcy jako gospodarze zostali automatycznie przydzieleni do Mistrzostw Europy, natomiast pozostałe zespoły musiały Zwycięzyć swoją grupę."),
  dataTableOutput("table"), 
  
  
  

  
  
  
    h2("Podział na Grupy po zakwalifikowaniu się"),
  
  p("21 drużyn zakwalifikowanych do Euro 2024 oraz 3 drużyny, które wyłoniły się z marcowych barażów, zostały podzielone na 4 grupy, po 6 drużyn w każdym."),
   p(" Niemcy, jako gospodarz, automatycznie trafiły do Grupy A na pozycji A1."), 
    p("Pozostałe drużyny z Grupy pierwszej rozlosowano do Grup B-F, gdzie zajęły pierwsze pozycje (B1, C1 itd.). "),
    p("Drużyny z pozostałych Grup przydzielono do nich w kolejności alfabetycznej, a ich pozycje w grupach ustalono poprzez losowanie."),
    p("Po zakończeniu barażów wszystkie drużyny zostały przypisane do swoich grup, tworząc kompletny skład turnieju Euro 2024."),
   br(), br(), br(),
  div(
    class = "table-container",
    style = "width: 50%; margin: 0 auto; background-color: transparent;",  
    reactableOutput("groupTable")
  ),
    
    br(),
    ),

 tabPanel("Więcej Euro 2024",
          h2("Tabela z przebiegiem meczów pucharowych"),
          p("Poniżej znajduje się szczegółowa tabela przedstawiająca przebieg wszystkich meczów pucharowych rozgrywanych podczas turnieju Euro 2024. Dane te obejmują zarówno wyniki, jak i statystyki związane z frekwencją na stadionach oraz lokalizacją każdego meczu."),
          br(),
          reactableOutput("tabela_mecze"),
          br(),
          h2("Diagram przedstawiający mecze pucharowe od 1/8 finału"),
          p("Poniższy diagram przedstawia ścieżkę, jaką przebyły drużyny od 1/8 finału aż do wielkiego finału. Każdy z meczów na tym etapie miał kluczowe znaczenie dla ostatecznych rozstrzygnięć turnieju."),
          br(),
          p(strong("Wyróżniające się mecze:"), " W turnieju szczególną uwagę zwrócił mecz ćwierćfinałowy między ", strong("Hiszpanią a Niemcami"), ", który zakończył się wynikiem 2-1 dla Hiszpanii, oraz zacięty półfinał, w którym Anglia pokonała Holandię 2-1."),
          p("Zwycięska drużyna, ", strong("Hiszpania"), ", po wygraniu dramatycznego finału z Anglią 2-1, zdobyła tytuł mistrza Europy."),
          br(),
          p(strong("Historia turnieju:"), " Euro 2024 przyniosło wiele emocji, z niespodziewanymi wynikami i wzlotami oraz upadkami drużyn."),
          p("Turniej zgromadził najlepsze drużyny z całej Europy, które walczyły o najwyższe trofeum w europejskim futbolu."),
            p("Drabinka turniejowa odzwierciedla zmagania od fazy pucharowej, ukazując drogę do finału."),
          br(),
          DiagrammeROutput("bracket")
         
 ),
 tabPanel(
   "Stadiony Euro 2024",
   sidebarLayout(
     sidebarPanel(
       h3("Wybierz stadion:", class = "title-text"),
       uiOutput("stadionList"),  
       tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
       tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
       tags$hr(style = "margin: 20px 0; border: none;"),
       
       div(
         class = "map-description",  
      
         h4("Mapa Stadionów Euro 2024", class = "map-title"),
         p("Mapa po prawej stronie przedstawia lokalizacje wszystkich stadionów, na których będą rozgrywane mecze Euro 2024. Możesz kliknąć na wybrany stadion na mapie lub wybrać go z listy powyżej, aby zobaczyć szczegóły na temat jego pojemności, miasta oraz klubu."),
         p("Dzięki tej mapie, masz możliwość zapoznania się z geograficznym rozmieszczeniem stadionów w Niemczech, gdzie odbędą się najważniejsze mecze turnieju.")
       ),
       style = "text-align: center;"  
     ),
     mainPanel(
       h3("Szczegóły stadionu", class = "title-text"),
       uiOutput("stadionDetails"), 
       tags$br(),
       
       div(class = "centered-content", 
           tableOutput("matchesTable")), 
       tags$br(),
       h2("Mapa Stadionów"),
       div(class = "map-container",  
           leafletOutput("mapaStadionow", height = "600px", width = "600px")  
       )
     )
   )
 ),
 
 tabPanel(
   "Klasyfikacja Euro 2024", 
   titlePanel("Tabela Klasyfikacji"),
   br(),
   reactableOutput("tabelaKlasyfikacji"),
   h2("Analiza Goli", align = "center"), 
   br(),
     fluidRow(
     column(12, align = "center",
            selectInput("selectedCategory", "Wybierz kategorię goli:", 
                        choices = unique(df_gole$Kategoria), width = "50%"),  
            plotlyOutput("barPlot", width = "80%"),  
            br(),
            DTOutput("goalsTable", width = "80%")  
     )
   )
   
 ),
  

 
 tabPanel("Tworzenie karty zawodnika",
          h2("Stwórz swoją kartę!"),
          br(),
          p("Czy kiedykolwiek marzyłeś o tym, by mieć własną kartę piłkarską, tak jak najwięksi zawodnicy świata?"),
          p("Teraz to możliwe!"),
          p("Wprowadź swoje imię, wybierz pozycję na boisku i dostosuj swoje kluczowe atrybuty, takie jak szybkość, strzały, podania, czy drybling."),
          p("Na końcu wygeneruj profesjonalną kartę ze swoim zdjęciem i ulubionymi statystykami."),
          p("Pochwal się swoimi wynikami przed znajomymi i sprawdź, kto z Was zasługuje na miano mistrza! Gotowy na stworzenie własnej karty? Do dzieła!"),
          br(), br(),
          sidebarLayout(
            sidebarPanel(
              textInput("name", "Imię i nazwisko", ""),
              selectInput("position", "Pozycja", choices = list("Napastnik", "Pomocnik", "Obrońca")),
              sliderInput("pace", "Sprint (BIE)", min = 1, max = 99, value = 50),
              sliderInput("shooting", "Strzały (STR)", min = 1, max = 99, value = 50),
              sliderInput("passing", "Podania (POD)", min = 1, max = 99, value = 50),
              sliderInput("defense", "Defensywa (OBR)", min = 1, max = 99, value = 50),
              sliderInput("dribbling", "Drybling (DRY)", min = 1, max = 99, value = 50),
              sliderInput("physical", "Fizyczność (FIZ)", min = 1, max = 99, value = 50),
              fileInput("user_image", "Wybierz zdjęcie", accept = c("image/png", "image/jpeg")),
              actionButton("generate_card", "Stwórz Kartę"),
              downloadButton("save_card", "Zapisz Kartę"),
              br(),
              br(),
              actionButton("reset_card", "Resetuj Kartę")
            ),
            fluidRow(
              column(5, align = "center", 
                     plotOutput("playerCard", height = "600px", width = "400px")
              )
            )
          )),
 
 
 tabPanel("Zespół Marzeń",
          titlePanel("Stwórz Drużynę Marzeń!"),
          value = "team_tab",
          p("Ten program to interaktywna aplikacja w R Shiny, która pozwala użytkownikom na stworzenie własnej drużyny marzeń w piłce nożnej."),
          p("Użytkownik wybiera zawodników na poszczególne pozycje (bramkarz, obrońcy, pomocnicy, napastnicy) z dostępnych list, a następnie wizualizuje swój skład na boisku."),
          p("Aplikacja umożliwia zapisanie wygenerowanej drużyny jako plik graficzny, dzięki czemu można łatwo podzielić się nią z innymi."),
          sidebarLayout(
            sidebarPanel(
              selectInput("bramkarz", "Wybierz bramkarza", choices = zawodnicy[["bramkarz"]]),
              selectInput("lewy_obronca", "Wybierz Lewego Obrońcę", choices = zawodnicy[["Lewy Obrońca"]]),
              selectInput("srodkowy_obronca", "Wybierz Środkowego Obrońcę", choices = zawodnicy[["Środkowy Obrońca"]]),
              selectInput("prawy_obronca", "Wybierz Prawego Obrońcę", choices = zawodnicy[["Prawy Obrońca"]]),
              selectInput("cofniety_pomocnik", "Wybierz Defensywnego Pomocnika", choices = zawodnicy[["Cofnięty Pomocnik"]]),
              selectInput("srodkowy_pomocnik", "Wybierz Środkowego Pomocnika", choices = zawodnicy[["Środkowy Pomocnik"]]),
              selectInput("srodkowy_pomocnik2", "Wybierz Środkowego Prawego Pomocnika", choices = zawodnicy[["Środkowy Pomocnik"]]),
              selectInput("srodkowy_pomocnik3", "Wybierz Środkowego Lewego Pomocnika", choices = zawodnicy[["Środkowy Pomocnik"]]),
              selectInput("lewy_skrzydlo", "Wybierz Lewe Skrzydło", choices = zawodnicy[["Lewe Skrzydło"]]),
              selectInput("prawy_napastnik", "Wybierz Prawe Skrzydło", choices = zawodnicy[["Prawy Napastnik"]]),
              selectInput("srodkowy_napastnik", "Wybierz Środkowego Napastnika", choices = zawodnicy[["Srodkowy Napastnik"]]),
              actionButton("generate_team", "Stwórz Drużynę"),
              downloadButton("save_team", "Zapisz Drużynę"),
              br(), br(),
              actionButton("reset_team", "Resetuj Drużynę")
            ),
            mainPanel(
              plotOutput("boisko", height = "1000px", width = "1000px"),
              div(
                style = "position: absolute; top: 10px; left: 30%; transform: translateX(-50%); font-size: 36px; font-weight: bold; color: black;", 
                textOutput("srednia_ocena")
              ),
            )
          )
 )


))
                





server <- function(input, output, session) {
 
  

  dane_druzyny <- read_excel("RStudio/dane.xlsx", sheet = "druzyny")
  dane_zawodnicy <- read_excel("RStudio/dane.xlsx", sheet = "zawodnicy")
  dane_zawodnicy$data_urodzenia <- as.Date(dane_zawodnicy$data_urodzenia)
  dane_polaczone <- left_join(dane_zawodnicy, dane_druzyny, by = "id_druzyny")

  
  dane_kwalifikacje <- read_excel("RStudio/Zeszyt1.xlsx")
  kwalifikacje <- dane_kwalifikacje
  Grupy <- read_excel("RStudio/Grupy.xlsx")
  Grupy_clean <- Grupy
  Grupy_clean[is.na(Grupy_clean)] <- ""
  tabela_klasyfikacji <- read_excel("RStudio/tabela_klasyfikacji.xlsx")
  
  
  euro <- data.frame(
    "Drużyna" = c("Niemcy", "Francja", "Hiszpania", "Turcja", "Austria", "Anglia", 
                  "Węgry", "Słowacja", "Albania", "Dania", "Holandia", "Rumunia", 
                  "Szwajcaria", "Serbia", "Czechy", "Włochy", "Słowenia", "Chorwacja", 
                  "Belgia", "Portugalia", "Szkocja", "Polska", "Gruzja", "Ukraina"),
    "Miejsce końcowe" = c(5, 3, 1, 7, 9, 2, 18, 12, 21, 16, 
                          3, 13, 6, 19, 21, 14, 10, 20, 
                          10, 8, 24, 23, 15, 17),
    
    "Grupa" = c("Grupa 1", "Grupa 1", "Grupa 1", "Grupa 2", "Grupa 2", "Grupa 1", 
                "Grupa 2", "Grupa 3", "Grupa 2", "Grupa 2", "Grupa 3", "Grupa 2", 
                "Grupa 4", "Grupa 4", "Grupa 3", "Grupa 4", "Grupa 3", "Grupa 3", 
                "Grupa 1", "Grupa 1", "Grupa 3", "Grupa 4", "Grupa 4", "Grupa 4") 
  )
  euro <- euro[order(euro$Grupa), ]
  
  mecze <- data.frame(
    Grupa = c(
      rep("Grupa A", 6), rep("Grupa B", 6), rep("Grupa C", 6), rep("Grupa D", 6), 
      rep("Grupa E", 6), rep("Grupa F", 6), 
      rep("1/8 finału", 8), rep("Ćwierćfinały", 4), rep("Półfinały", 2), rep("Finał", 1)
    ),
    dane <- data.frame(
      Grupa = c(
        rep("Grupa A", 6), rep("Grupa B", 6), rep("Grupa C", 6), rep("Grupa D", 6), 
        rep("Grupa E", 6), rep("Grupa F", 6), 
        rep("1/8 finału", 8), rep("Ćwierćfinały", 4), rep("Półfinały", 2), rep("Finał", 1)
      ),
      Stadion = c(
        # Grupa A
        "Munich Football Arena", "Cologne Stadium", "Stuttgart Arena", 
        "Cologne Stadium", "Frankfurt Arena", "Stuttgart Arena",
        # Grupa B
        "Olympiastadion Berlin", "BVB Stadion Dortmund", "Volksparkstadion Hamburg",
        "Arena AufSchalke", "Düsseldorf Arena", "Leipzig Stadium",
        # Grupa C
        "Stuttgart Arena", "Arena AufSchalke", "Munich Football Arena", 
        "Frankfurt Arena", "Cologne Stadium", "Munich Football Arena",
        # Grupa D
        "Volksparkstadion Hamburg", "Düsseldorf Arena", "Olympiastadion Berlin",
        "Leipzig Stadium", "Olympiastadion Berlin", "BVB Stadion Dortmund",
        # Grupa E
        "Munich Football Arena", "Frankfurt Arena", "Düsseldorf Arena",
        "Cologne Stadium", "Frankfurt Arena", "Stuttgart Arena",
        # Grupa F
        "BVB Stadion Dortmund", "Leipzig Stadium", "Volksparkstadion Hamburg",
        "BVB Stadion Dortmund", "Arena AufSchalke", "Volksparkstadion Hamburg",
        # 1/8 finału
        "Olympiastadion Berlin", "BVB Stadion Dortmund", "Arena AufSchalke", 
        "Cologne Stadium", "Düsseldorf Arena", "Frankfurt Arena", 
        "Munich Football Arena", "Leipzig Stadium",
        # Ćwierćfinały
        "Stuttgart Arena", "Volksparkstadion Hamburg", "Düsseldorf Arena", 
        "Olympiastadion Berlin",
        # Półfinały
        "Munich Football Arena", "BVB Stadion Dortmund",
        # Finał
        "Olympiastadion Berlin"
      ),
      Mecz = c(
        # Grupa A
        "Niemcy 5–1 Szkocja", "Węgry 1–3 Szwajcaria", "Niemcy 2–0 Węgry", 
        "Szkocja 1–1 Szwajcaria", "Szwajcaria 1–1 Niemcy", "Szkocja 0–1 Węgry",
        # Grupa B
        "Hiszpania 3–0 Chorwacja", "Włochy 2–1 Albania", "Chorwacja 2–2 Albania", 
        "Hiszpania 1–0 Włochy", "Albania 0–1 Hiszpania", "Chorwacja 1–1 Włochy",
        # Grupa C
        "Słowenia 1–1 Dania", "Serbia 0–1 Anglia", "Słowenia 1–1 Serbia",
        "Dania 1–1 Anglia", "Anglia 0–0 Słowenia", "Dania 0–0 Serbia",
        # Grupa D
        "Polska 1–2 Holandia", "Austria 0–1 Francja", "Polska 1–3 Austria", 
        "Polska 2–0 Francja", "Holandia 2–3 Austria", "Francja 1–1 Polska",
        # Grupa E
        "Rumunia 3–0 Ukraina", "Belgia 0–1 Słowacja", "Słowacja 1–2 Ukraina",
        "Belgia 2–0 Rumunia", "Słowacja 1–1 Rumunia", "Ukraina 0–0 Belgia",
        # Grupa F
        "Turcja 3–1 Gruzja", "Portugalia 2–1 Czechy", "Gruzja 1–1 Czechy",
        "Turcja 0–3 Portugalia", "Gruzja 0–1 Portugalia", "Czechy 1–2 Turcja",
        # 1/8 finału
        "Szwajcaria 2–0 Włochy", "Niemcy 2–0 Dania", "Anglia 2–1 Słowacja", 
        "Hiszpania 4–1 Gruzja", "Francja 1–0 Belgia", "Portugalia 0–0 (3–0) Słowenia",
        "Rumunia 0–3 Holandia", "Austria 1–2 Turcja",
        # Ćwierćfinały
        "Hiszpania 2–1 Niemcy", "Portugalia 0–0 (3–5) Francja", 
        "Anglia 1–1 (k. 5–3) Szwajcaria", "Holandia 2–1 Turcja",
        # Półfinały
        "Hiszpania 2–1 Francja", "Holandia 1–2 Anglia",
        # Finał
        "Hiszpania 2–1 Anglia"
      ),
      Frekwencja = c(
        # Grupa A
        65052, 41676, 54700, 42711, 46685, 54000,
        # Grupa B
        68844, 60512, 46784, 49328, 46586, 38322,
        # Grupa C
        54000, 48935, 63023, 48638, 41536, 64288,
        # Grupa D
        48117, 46255, 69455, 38763, 86938, 59278,
        # Grupa E
        61591, 45138, 42535, 42535, 45138, 54000,
        # Grupa F
        59127, 38412, 46784, 61442, 45986, 47683,
        # 1/8 finału
        68817, 61212, 47889, 42233, 47228, 46007, 62612, 38305,
        # Ćwierćfinały
        54000, 47789, 46907, 70091,
        # Półfinały
        62042, 60926,
        # Finał
        65600
      ),
      Data = c(
        # Grupa A
        "14.06.2024 21:00", "15.06.2024 18:00", "15.06.2024 21:00",
        "19.06.2024 18:00", "19.06.2024 21:00", "23.06.2024 21:00",
        # Grupa B
        "15.06.2024 18:00", "15.06.2024 21:00", "16.06.2024 18:00",
        "16.06.2024 21:00", "21.06.2024 18:00", "21.06.2024 21:00",
        # Grupa C
        "16.06.2024 18:00", "16.06.2024 21:00", "20.06.2024 18:00",
        "20.06.2024 21:00", "25.06.2024 18:00", "25.06.2024 21:00",
        # Grupa D
        "16.06.2024 15:00", "16.06.2024 18:00", "19.06.2024 18:00",
        "19.06.2024 21:00", "23.06.2024 21:00", "25.06.2024 21:00",
        # Grupa E
        "17.06.2024 15:00", "17.06.2024 18:00", "20.06.2024 18:00",
        "20.06.2024 21:00", "23.06.2024 18:00", "26.06.2024 18:00",
        # Grupa F
        "18.06.2024 18:00", "19.06.2024 18:00", "19.06.2024 21:00",
        "23.06.2024 18:00", "23.06.2024 21:00", "26.06.2024 21:00",
        # 1/8 finału
        "29.06.2024 18:00", "29.06.2024 21:00", "30.06.2024 18:00", 
        "30.06.2024 21:00", "01.07.2024 18:00", "01.07.2024 21:00",
        "02.07.2024 18:00", "02.07.2024 21:00",
        # Ćwierćfinały
        "05.07.2024 18:00", "05.07.2024 21:00", "06.07.2024 18:00",
        "06.07.2024 21:00",
        # Półfinały
        "09.07.2024 21:00", "10.07.2024 21:00",
        # Finał
        "14.07.2024 21:00"
      )
    )
    
    
    
  )
  
  
  stadion_data <- data.frame(
    Stadion = c("Red Bull Arena", "Deutsche Bank Park", "Düsseldorf Arena", 
                "Cologne Stadium", "Volksparkstadion", "Arena AufSchalke", 
                "Stuttgart Arena", "BVB Stadion Dortmund", "Munich Football Arena", 
                "Olympiastadion Berlin"),
    Pojemność = c(42000, 46000, 47000, 47000, 50000, 50000, 54000, 66000, 67000, 70000),
    Miasto = c("Lipsk", "Frankfurt", "Düsseldorf", "Kolonia", "Hamburg", 
               "Gelsenkirchen", "Stuttgart", "Dortmund", "Monachium", "Berlin"),
    Klub = c("RB Lipsk", "Eintracht Frankfurt", "Fortuna Düsseldorf", "FC Koeln", 
             "Hamburger SV", "FC Schalke 04", "VfB Stuttgart", "BVB Borussia Dortmund", 
             "FC Bayern Monachium", "Hertha BSC Berlin"),
    Mecze = I(list(
      c("Chorwacja – Włochy", "Holandia – Francja", "Portugalia – Czechy", "Austria - Turcja"),
      c("Szwajcaria – Niemcy", "Dania – Anglia", "Belgia – Słowacja", "Słowacja – Rumunia", "Portugalia - Słowenia"),
      c("Albania – Hiszpania", "Austria – Francja", "Słowacja – Ukraina", "Rumunia - Holandia", "Francja - Beliga"),
      c("Węgry – Szwajcaria", "Szkocja – Szwajcaria", "Anglia – Słowenia", "Belgia – Rumunia", "Anglia - Słowacja"),
      c("Chorwacja – Albania", "Polska – Holandia", "Gruzja – Czechy", "Czechy – Turcja", "Portugalia - Francja"),
      c("Hiszpania – Włochy", "Serbia – Anglia", "Gruzja – Portugalia", "Anglia - Słowacja"),
      c("Niemcy – Węgry", "Szkocja – Węgry", "Słowenia – Dania", "Ukraina – Belgia", "Hiszpania - Niemcy"),
      c("Włochy – Albania", "Francja –  Polska", "Turcja – Gruzja", "Turcja – Portugalia", "Niemcy - Dania", "Holandia - Anglia"),
      c("Niemcy – Szkocja (mecz otwarcia)", "Słowenia – Serbia", "Dania – Serbia", "Rumunia – Ukraina", "Rumunia - Holandia", "Hiszpania - Francja"),
      c("Hiszpania – Chorwacja", "Polska – Austria", "Holandia – Austria", "Szwajcaria - Włochy", "Holandia - Turcja", "Hiszpania - Anglia")
    )),
    Zdjęcie = c("red_bull_arena.jpg", "deutsche_bank_park.jpg", "merkur_spiel_arena.jpg", 
                "rheinenergie_stadion.jpg", "volksparkstadion.jpg", "arena_aufschalke.jpg", 
                "mercedes_benz_arena.jpg", "westfalenstadion.jpg", "allianz_arena.jpg", 
                "olympiastadion_berlin.jpg") )
  
  
  
  
  
  euro<- euro[order(euro$`Miejsce.końcowe`), ]
  euro_sorted <- euro[order(euro$Grupa), ]
  
  granice_europy <- ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(continent == "Europe")

  

  wspolrzedne <- data.frame(
    nazwa_druzyny = c("Niemcy", "Albania", "Anglia", "Austria", "Belgia", "Czechy", "Dania", 
      "Francja", "Hiszpania", "Holandia", "Portugalia", "Rumunia", 
      "Serbia", "Słowacja", "Słowenia", "Szkocja", "Szwajcaria", "Turcja", 
      "Węgry", "Włochy", "Polska", "Andora", "Armenia", "Azerbejdżan", "Białoruś", "Bośnia i Hercegowina",
      "Bułgaria", "Chorwacja", "Cypr", "Estonia", "Finlandia", "Grecja", 
      "Gruzja", "Irlandia", "Islandia", "Izrael", "Kazachstan", "Kosowo", 
      "Litwa", "Łotwa", "Luksemburg", "Macedonia Północna", "Malta", 
      "Mołdawia", "Czarnogóra", "Norwegia", "Rosja", "San Marino", 
      "Szwecja", "Ukraina", "Walia", "Gibraltar"
    ),
    longitude = c(
      13.4050, 20.1683, -1.6174, 14.5501, 4.4699, 15.4749, 9.5018, 
      2.2137, -3.7038, 5.2913, -8.2245, 24.9668, 20.7970,  
      17.1077, 14.5058, -3.1883, 7.4474, 32.8597, 19.1451, 12.5674, 19.1451, 
      1.5732033, 44.6736646, 47.7872508, 27.6971358, 17.5961467,
      25.4856617, 15.6575209, 33.1451285, 25.3319078, 25.9209164, 21.9877132,
      44.0287382, -7.9794599, -18.1059013, 34.8594762, 66.7780818, 20.9021231,
     23.7499997, 24.7537645, 6.1296751, 21.7168387,
      14.4476911, 28.5670941, 19.5180992, 8.7876653, 97.7453061,
      12.458306, 14.5208584, 31.2718321, -3.73893,  -5.3474761
    ),
    latitude = c(
      52.5200, 41.1533, 51.5099, 47.5162, 50.8503, 49.8038, 56.2639, 
      46.6031, 40.4168, 52.3676, 39.3999, 45.9432, 44.0165,  
      48.1486, 46.0569, 55.9533, 46.9480, 39.9334, 47.1625, 41.9028, 51.9194,
      42.5407167, 40.7696272, 40.3936294, 53.4250605, 44.3053476,
      42.6073975, 45.3658443, 34.9823018, 58.7523778, 63.2467777,  38.9953683,
      41.6809707, 52.865196,  64.9841821, 30.8124247, 48.1012954, 42.5869578,
      55.3500003, 56.8406494, 49.8158683, 41.6171214,
      35.8885993, 47.2879608, 42.9868853, 61.1529386, 64.6863136,
      43.9458623, 59.6749712, 49.4871968, 52.2928116, 36.1285933
    ),
    flag_path = c("flags/niemcy.png", "flags/albania.png", "flags/anglia.png", 
                  "flags/austria.png", "flags/belgia.png", "flags/czechy.png", 
                  "flags/dania.png", "flags/francja.png", "flags/hiszpania.png", 
                  "flags/holandia.png", "flags/portugalia.png", "flags/rumunia.png", 
                  "flags/serbia.png", "flags/słowacja.png", "flags/słowenia.png", 
                  "flags/szkocja.png", "flags/szwajcaria.png", "flags/turcja.png", 
                  "flags/węgry.png", "flags/włochy.png", "flags/polska.png", 
                  "flags/andora.png", "flags/armenia.png", "flags/azerbejdżan.png", 
                  "flags/białoruś.png", "flags/bosnia_i_hercegowina.png", 
                  "flags/bułgaria.png", "flags/chorwacja.png", "flags/cypr.png", 
                  "flags/estonia.png", "flags/finlandia.png", "flags/grecja.png", 
                  "flags/gruzja.png", "flags/irlandia.png", "flags/islandia.png", 
                  "flags/izrael.png", "flags/kazachstan.png", "flags/kosowo.png", 
                  "flags/litwa.png", "flags/łotwa.png", "flags/luksemburg.png", 
                  "flags/macedonia_polnocna.png", "flags/malta.png", "flags/mołdawia.png", 
                  "flags/czarnogóra.png", "flags/norwegia.png", "flags/rosja.png", 
                  "flags/san_marino.png", "flags/szwecja.png", "flags/ukraina.png", 
                  "flags/walia.png", "flags/gibraltar.png"),
    populacja = c(83000000, 2900000, 56000000, 9000000, 11600000, 10700000, 5900000, 
                  67800000, 47500000, 17400000, 10200000, 19100000, 6700000, 5400000, 
                  2100000, 5500000, 8700000, 85000000, 9600000, 59200000, 38000000, 
                  77000, 3000000, 10200000, 9300000, 3300000, 6800000, 3900000, 
                  1200000, 1300000, 5500000, 10300000, 3700000, 5100000, 376000, 
                  9300000, 19300000, 1800000, 2800000, 1900000, 645000, 2000000, 
                  514000, 2600000, 620000, 5400000, 144000000, 34000, 10500000, 
                  36000000, 3100000, 34000),
    capital = c("Berlin", "Tirana", "Londyn", "Wiedeń", "Bruksela", "Praga", "Kopenhaga", 
                "Paryż", "Madryt", "Amsterdam", "Lizbona", "Bukareszt", "Belgrad", 
                "Bratysława", "Lublana", "Edynburg", "Berno", "Ankara", "Budapeszt", 
                "Rzym", "Warszawa", "Andorra la Vella", "Erywań", "Baku", "Mińsk", 
                "Sarajewo", "Sofia", "Zagrzeb", "Nikozja", "Tallinn", "Helsinki", 
                "Ateny", "Tbilisi", "Dublin", "Reykjavik", "Jerozolima", "Astana", 
                "Prisztina", "Wilno", "Ryga", "Luksemburg (miasto)", "Skopje", 
                "Valletta", "Kiszyniów", "Podgorica", "Oslo", "Moskwa", "San Marino (miasto)", 
                "Sztokholm", "Kijów", "Cardiff", "Gibraltar")
  )
  
  stadiony <- data.frame(
    name = c("Red Bull Arena", "Deutsche Bank Park", "Düsseldorf Arena", 
             "Cologne Stadium", "Volksparkstadion", "Arena AufSchalke", 
             "Stuttgart Arena", "BVB Stadion Dortmund", "Munich Football Arena", 
             "Olympiastadion Berlin"),
    lat = c(51.3455, 50.0684, 51.2612, 50.9330, 53.5875, 51.5542, 
            48.7922, 51.4926, 48.2188, 52.5146),
    lng = c(12.3731, 8.6457, 6.7332, 6.8751, 9.9756, 7.0677, 
            9.2311, 7.4519, 11.6247, 13.2399),
    miasto = c("Lipsk", "Frankfurt", "Düsseldorf", "Kolonia", 
               "Hamburg", "Gelsenkirchen", "Stuttgart", 
               "Dortmund", "Monachium", "Berlin"),
    Pojemność = c(42000, 46000, 47000, 47000, 50000, 50000, 54000, 66000, 67000, 70000)
  )
  
  dane_gole <- list(
    "3 gole" = c("Harry Kane (Anglia)", "Georges Mikautadze (Gruzja)", "Dani Olmo (Hiszpania)", 
                 "Cody Gakpo (Holandia)", "Jamal Musiala (Niemcy)", "Ivan Schranz (Słowacja)"),
    "2 gole" = c("Jude Bellingham (Anglia)", "Fabián Ruiz (Hiszpania)", "Nico Williams (Hiszpania)", 
                 "Donyell Malen (Holandia)", "Niclas Füllkrug (Niemcy)", "Kai Havertz (Niemcy)", 
                 "Florian Wirtz (Niemcy)", "Răzvan Marin (Rumunia)", "Breel Embolo (Szwajcaria)", "Merih Demiral (Turcja)"),
    "1 gol" = c("Nedim Bajrami (Albania)", "Klaus Gjasula (Albania)", "Qazim Laci (Albania)", 
                "Cole Palmer (Anglia)", "Bukayo Saka (Anglia)", "Ollie Watkins (Anglia)", 
                "Marko Arnautović (Austria)", "Christoph Baumgartner (Austria)", 
                "Michael Gregoritsch (Austria)", "Marcel Sabitzer (Austria)", "Roman Schmid (Austria)", 
                "Gernot Trauner (Austria)", "Kevin De Bruyne (Belgia)", "Youri Tielemans (Belgia)", 
                "Andrej Kramarić (Chorwacja)", "Luka Modrić (Chorwacja)", "Lukas Provod (Czechy)", 
                "Patrik Schick (Czechy)", "Tomáš Souček (Czechy)", "Christian Eriksen (Dania)", 
                "Morten Hjulmand (Dania)", "Randal Kolo Muani (Francja)", "Kylian Mbappé (Francja)", 
                "Dani Carvajal (Hiszpania)", "Mikel Merino (Hiszpania)", "Álvaro Morata (Hiszpania)", 
                "Mikel Oyarzabal (Hiszpania)", "Rodri (Hiszpania)", "Ferran Torres (Hiszpania)", 
                "Lamine Yamal (Hiszpania)", "Memphis Depay (Holandia)", "Stefan de Vrij (Holandia)", 
                "Xavi Simons (Holandia)", "Wout Weghorst (Holandia)", "Emre Can (Niemcy)", 
                "İlkay Gündoğan (Niemcy)", "Adam Buksa (Polska)", "Robert Lewandowski (Polska)", 
                "Krzysztof Piątek (Polska)", "Francisco Conceição (Portugalia)", "Bruno Fernandes (Portugalia)", 
                "Bernardo Silva (Portugalia)", "Denis Drăguș (Rumunia)", "Nicolae Stanciu (Rumunia)", 
                "Luka Jović (Serbia)", "Ondrej Duda (Słowacja)", "Erik Janža (Słowenia)", 
                "Žan Karničnik (Słowenia)", "Scott McTominay (Szkocja)", "Michel Aebischer (Szwajcaria)", 
                "Kwadwo Duah (Szwajcaria)", "Remo Freuler (Szwajcaria)", "Dan Ndoye (Szwajcaria)", 
                "Xherdan Shaqiri (Szwajcaria)", "Ruben Vargas (Szwajcaria)", "Samet Akaydin (Turcja)", 
                "Kerem Aktürkoğlu (Turcja)", "Hakan Çalhanoğlu (Turcja)", "Arda Güler (Turcja)", 
                "Mert Müldür (Turcja)", "Cenk Tosun (Turcja)", "Roman Jaremczuk (Ukraina)", 
                "Mykoła Szaparenko (Ukraina)", "Kevin Csobot (Węgry)", "Barnabás Varga (Węgry)", 
                "Nicolò Barella (Włochy)", "Alessandro Bastoni (Włochy)", "Mattia Zaccagni (Włochy)"),
    "Gole samobójcze" = c("Klaus Gjasula (dla Chorwacji)", "Maximilian Wöber (dla Francji)", 
                          "Jan Vertonghen (dla Francji)", "Robin Hranać (dla Portugalii)", 
                          "Robin Le Normand (dla Gruzji)", "Donyell Malen (dla Austrii)", 
                          "Antonio Rüdiger (dla Szkocji)", "Samet Akaydin (dla Portugalii)", 
                          "Mert Müldür (dla Holandii)", "Riccardo Calafiori (dla Hiszpanii)")
  )
  
  zawodnicy <- list(
    "Lewy Obrońca" = c("Andrew Robertson", "Alphonso Davies", "Theo Hernandez", "Jordi Alba", "Ferland Mendy",
                       "Raphael Guerreiro", "Jose Gaya", "Alex Sandro", "Benjamin Mendy", "Marc Cucurella",
                       "Kieran Tierney", "Luke Shaw", "Renan Lodi", "Nico Schulz", "Sergio Reguilon",
                       "Marcos Alonso", "Robin Gosens", "Matthias Ginter", "Leonardo Spinazzola", "Ben Chilwell"),
    
    "Środkowy Obrońca" = c("Virgil van Dijk", "Sergio Ramos", "Kalidou Koulibaly", "Raphael Varane", "Matthijs de Ligt",
                           "Aymeric Laporte", "Marquinhos", "Giorgio Chiellini", "Antonio Rudiger", "Jules Kounde",
                           "Harry Maguire", "Stefan de Vrij", "Presnel Kimpembe", "Thiago Silva", "Gerard Pique",
                           "David Alaba", "Clement Lenglet", "Diego Godin", "Leonardo Bonucci", "John Stones"),
    
    "Prawy Obrońca" = c("Trent Alexander", "Kyle Walker", "Joao Cancelo", "Dani Carvajal", "Achraf Hakimi",
                        "Kieran Trippier", "Aaron Wan-Bissaka", "Ricardo Pereira", "Reece James", "Benjamin Pavard",
                        "Nelson Semedo", "Hector Bellerin", "Timothy Castagne", "Sergino Dest", "Alvaro Odriozola",
                        "Lukas Klostermann", "Kevin Mbabu", "Thomas Meunier", "Davide Calabria", "Juan Cuadrado"),
    
    "Środkowy Pomocnik" = c("Kevin De Bruyne", "Luka Modric", "Joshua Kimmich", "Frenkie de Jong", "Paul Pogba",
                            "Bruno Fernandes", "Toni Kroos", "Marco Verratti", "N'Golo Kante", "Fabinho",
                            "Casemiro", "Leon Goretzka", "Rodri", "Saul Niguez", "Sergio Busquets",
                            "Thiago Alcantara", "Nicolo Barella", "Ilkay Gundogan", "Hakim Ziyech", "Christian Eriksen"),
    
    "Lewe Skrzydło" = c("Neymar", "Sadio Mane", "Raheem Sterling", "Son Heung-Min", "Leroy Sane",
                        "Eden Hazard", "Kingsley Coman", "Ousmane Dembele", "Mikel Oyarzabal", "Serge Gnabry",
                        "Marcus Rashford", "Jack Grealish", "Christian Pulisic", "Ansu Fati", "Lorenzo Insigne",
                        "Allan Saint-Maximin", "Phil Foden", "Harvey Barnes", "Jadon Sancho", "Ryan Sessegnon"),
    
    "Prawy Napastnik" = c("Lionel Messi", "Mohamed Salah", "Cristiano Ronaldo", "Kylian Mbappe", "Gareth Bale",
                          "Riyad Mahrez", "Angel Di Maria", "Bernardo Silva", "Federico Chiesa", "Jadon Sancho",
                          "Pedro Neto", "Giovanni Reyna", "Lucas Moura", "Federico Valverde", "Hakim Ziyech",
                          "Raphinha", "Bukayo Saka", "Adama Traore", "Domenico Berardi", "Ismaila Sarr"),
    "Srodkowy Napastnik" = c("Erling Haaland", "Robert Lewandowski", "Harry Kane", "Karim Benzema",
                             "Victor Osimhen", "Romelu Lukaku", "Lautaro Martínez", "Christopher Nkunku", "Dusan Vlahović",
                             "Marcus Thuram", "Tammy Abraham", "Darwin Núñez", "Alexander Isak", "Antoine Griezmann", "Jonathan David", 
                             "Arkadiusz Milik", "Gonçalo Ramos", "Olivier Giroud", "Richarlison", "João Félix"),
    "bramkarz" = c("Thibaut Courtois", "Jan Oblak", "Marc ter Stegen", "Alisson Becker", "Ederson", "Mike Maignan", 
                   "Manuel Neuer", "Gian Donnarumma", "David de Gea", "Keylor Navas", "Emil Martínez", "Wojciech Szczęsny",
                   "Aaron Ramsdale", "Yassine Bounou", "Nick Pope", "Jordan Pickford", "Hugo Lloris", "Unai Simón", "André Onana", "Samir Handanović"),
    "Cofnięty Pomocnik" = c("N'Golo Kanté", "Casemiro", "Joshua Kimmich", "Rodri", 
                            "Sergio Busquets", "Fabinho", "Wilfred Ndidi", "Declan Rice", 
                            "Thomas Partey", "Marcelo Brozović")
    
    
  )
  
  
  oceny_zawodnikow <- list(
  # Lewy Obrońca
  "Andrew Robertson" = 87, "Alphonso Davies" = 86, "Theo Hernandez" = 85, "Jordi Alba" = 84, "Ferland Mendy" = 83,
  "Raphael Guerreiro" = 82, "Jose Gaya" = 81, "Alex Sandro" = 80, "Benjamin Mendy" = 79, "Marc Cucurella" = 78,
  "Kieran Tierney" = 77, "Luke Shaw" = 85, "Renan Lodi" = 84, "Nico Schulz" = 76, "Sergio Reguilon" = 79,
  "Marcos Alonso" = 78, "Robin Gosens" = 82, "Matthias Ginter" = 84, "Leonardo Spinazzola" = 81, "Ben Chilwell" = 82,
  
  # Środkowy Obrońca
  "Virgil van Dijk" = 91, "Sergio Ramos" = 89, "Kalidou Koulibaly" = 88, "Raphael Varane" = 87, "Matthijs de Ligt" = 86,
  "Aymeric Laporte" = 87, "Marquinhos" = 88, "Giorgio Chiellini" = 87, "Antonio Rudiger" = 83, "Jules Kounde" = 82,
  "Harry Maguire" = 82, "Stefan de Vrij" = 85, "Presnel Kimpembe" = 84, "Thiago Silva" = 86, "Gerard Pique" = 82,
  "David Alaba" = 85, "Clement Lenglet" = 81, "Diego Godin" = 83, "Leonardo Bonucci" = 84, "John Stones" = 83,
  
  # Prawy Obrońca
  "Trent Alexander" = 87, "Kyle Walker" = 85, "Joao Cancelo" = 86, "Dani Carvajal" = 85, "Achraf Hakimi" = 84,
  "Kieran Trippier" = 83, "Aaron Wan-Bissaka" = 81, "Ricardo Pereira" = 82, "Reece James" = 83, "Benjamin Pavard" = 80,
  "Nelson Semedo" = 80, "Hector Bellerin" = 79, "Timothy Castagne" = 78, "Sergino Dest" = 76, "Alvaro Odriozola" = 75,
  "Lukas Klostermann" = 82, "Kevin Mbabu" = 81, "Thomas Meunier" = 79, "Davide Calabria" = 80, "Juan Cuadrado" = 83,
  
  # Środkowy Pomocnik
  "Kevin De Bruyne" = 91, "Luka Modric" = 87, "Joshua Kimmich" = 89, "Frenkie de Jong" = 87, "Paul Pogba" = 85,
  "Bruno Fernandes" = 88, "Toni Kroos" = 88, "Marco Verratti" = 87, "N'Golo Kante" = 89, "Fabinho" = 87,
  "Casemiro" = 89, "Leon Goretzka" = 86, "Rodri" = 85, "Saul Niguez" = 83, "Sergio Busquets" = 86,
  "Thiago Alcantara" = 86, "Nicolo Barella" = 84, "Ilkay Gundogan" = 85, "Hakim Ziyech" = 84, "Christian Eriksen" = 85,
  
  # Lewe Skrzydło
  "Neymar" = 91, "Sadio Mane" = 89, "Raheem Sterling" = 88, "Son Heung-Min" = 89, "Leroy Sane" = 85,
  "Eden Hazard" = 88, "Kingsley Coman" = 86, "Ousmane Dembele" = 84, "Mikel Oyarzabal" = 85, "Serge Gnabry" = 85,
  "Marcus Rashford" = 85, "Jack Grealish" = 84, "Christian Pulisic" = 82, "Ansu Fati" = 79, "Lorenzo Insigne" = 85,
  "Allan Saint-Maximin" = 82, "Phil Foden" = 86, "Harvey Barnes" = 82, "Jadon Sancho" = 85, "Ryan Sessegnon" = 78,
  
  # Prawy Napastnik
  "Lionel Messi" = 93, "Mohamed Salah" = 91, "Cristiano Ronaldo" = 91, "Kylian Mbappe" = 91, "Gareth Bale" = 81,
  "Riyad Mahrez" = 85, "Angel Di Maria" = 87, "Bernardo Silva" = 86, "Federico Chiesa" = 85, "Jadon Sancho" = 85,
  "Pedro Neto" = 82, "Giovanni Reyna" = 80, "Lucas Moura" = 81, "Federico Valverde" = 84, "Hakim Ziyech" = 84,
  "Raphinha" = 83, "Bukayo Saka" = 81, "Adama Traore" = 79, "Domenico Berardi" = 82, "Ismaila Sarr" = 82,
  
  # Srodkowy Napastnik
  "Erling Haaland" = 91, "Robert Lewandowski" = 92, "Harry Kane" = 89, "Karim Benzema" = 91, 
  "Victor Osimhen" = 85, "Romelu Lukaku" = 87, "Lautaro Martínez" = 86, "Christopher Nkunku" = 86, "Dusan Vlahović" = 85,
  "Marcus Thuram" = 82, "Tammy Abraham" = 81, "Darwin Núñez" = 84, "Alexander Isak" = 82, "Antoine Griezmann" = 85, 
  "Jonathan David" = 81, "Arkadiusz Milik" = 80, "Gonçalo Ramos" = 83, "Olivier Giroud" = 81, "Richarlison" = 81, "João Félix" = 84,
  
  # Bramkarz
  "Thibaut Courtois" = 90, "Jan Oblak" = 91, "Marc ter Stegen" = 89, "Alisson Becker" = 89, "Ederson" = 89, 
  "Mike Maignan" = 86, "Manuel Neuer" = 90, "Gian Donnarumma" = 88, "David de Gea" = 87, "Keylor Navas" = 88,
  "Emil Martínez" = 84, "Wojciech Szczęsny" = 86, "Aaron Ramsdale" = 84, "Yassine Bounou" = 85, "Nick Pope" = 83,
  "Jordan Pickford" = 82, "Hugo Lloris" = 87, "Unai Simón" = 83, "André Onana" = 82, "Samir Handanović" = 84,
  
  # Cofnięty Pomocnik
  "N'Golo Kanté" = 89, "Casemiro" = 89, "Joshua Kimmich" = 90, "Rodri" = 87, 
  "Sergio Busquets" = 86, "Fabinho" = 87, "Wilfred Ndidi" = 85, "Declan Rice" = 84, 
  "Thomas Partey" = 83, "Marcelo Brozović" = 86
)

  

  
df_gole <- data.frame(
  Kategoria = rep(names(dane_gole), times = sapply(dane_gole, length)),
  Zawodnik_Narodowosc = unlist(dane_gole),
  stringsAsFactors = FALSE
)


df_gole <- df_gole %>%
  tidyr::separate(Zawodnik_Narodowosc, into = c("Zawodnik", "Narodowosc"), sep = " \\(", extra = "merge") %>%
  dplyr::mutate(Narodowosc = sub("\\)$", "", Narodowosc))  

  
  
  dane_druzyny <- merge(dane_druzyny, wspolrzedne, by = "nazwa_druzyny", all.x = TRUE)
  
  
  
  
  draw_player_card <- function(name, position, pace, shooting, passing, defense, dribbling, physical, user_image = NULL) {
    overall_rating <- round(mean(c(pace, shooting, passing, defense, dribbling, physical)))
    
    img <- readPNG("www/card_template.png")  
    
    p <- ggplot() +
      
      annotation_raster(img, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
      
     
      annotate("text", x = 0.15, y = 0.8, label = overall_rating, size = 12, fontface = "bold", color = "black") +
      annotate("text", x = 0.5, y = 0.9, label = position, size = 10, fontface = "bold", color = "black") +
      annotate("text", x = 0.57, y = 0.47, label = name, size = 8, fontface = "bold", color = "black") +
      
     
      annotate("text", x = 0.3, y = 0.35, label = paste0(pace, " BIE"), size = 6, color = "black", hjust = 0) +
      annotate("text", x = 0.3, y = 0.3, label = paste0(shooting, " STR"), size = 6, color = "black", hjust = 0) +
      annotate("text", x = 0.3, y = 0.25, label = paste0(passing, " POD"), size = 6, color = "black", hjust = 0) +
      annotate("text", x = 0.85, y = 0.35, label = paste0(dribbling, " DRY"), size = 6, color = "black", hjust = 1) +
      annotate("text", x = 0.85, y = 0.3, label = paste0(defense, " OBR"), size = 6, color = "black", hjust = 1) +
      annotate("text", x = 0.83, y = 0.25, label = paste0(physical, " FIZ"), size = 6, color = "black", hjust = 1) +
      
      
      xlim(0, 1) +
      ylim(0, 1) +
      theme_void()  
    
    
    if (!is.null(user_image)) {
     
      ext <- tools::file_ext(user_image)
      user_img <- if (ext == "png") {
        readPNG(user_image)
      } else if (ext == "jpeg" || ext == "jpg") {
        readJPEG(user_image)
      }
      p <- p + annotation_raster(user_img, xmin = 0.35, xmax = 0.65, ymin = 0.55, ymax = 0.75)
    }
    
    return(p)
  }
  
  
  draw_team <- function(bramkarz, lewy_obronca, srodkowy_obronca, cofniety_pomocnik, prawy_obronca, srodkowy_pomocnik, srodkowy_pomocnik2, srodkowy_pomocnik3, lewy_skrzydlo, prawy_napastnik, srodkowy_napastnik) {
    img <- readPNG("www/boisko.png")
    

    
    ggplot() +
      
      annotation_raster(img, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
      
      
      annotate("text", x = 0.5, y = 0.95, label = bramkarz, size = 6, color = "white") +
      annotate("text", x = 0.25, y = 0.85, label = lewy_obronca, size = 6, color = "white") +
      annotate("text", x = 0.5, y = 0.75, label = srodkowy_obronca, size = 6, color = "white") +
      annotate("text", x = 0.75, y = 0.85, label = prawy_obronca, size = 6, color = "white") +
      annotate("text", x = 0.5, y = 0.85, label = cofniety_pomocnik, size = 6, color = "white") +
      annotate("text", x = 0.5, y = 0.65, label = srodkowy_pomocnik, size = 6, color = "white") +
      annotate("text", x = 0.7, y = 0.65, label = srodkowy_pomocnik2, size = 6, color = "white") +
      annotate("text", x = 0.25, y = 0.65, label = srodkowy_pomocnik3, size = 6, color = "white") +
      annotate("text", x = 0.25, y = 0.4, label = lewy_skrzydlo, size = 6, color = "white") +
      annotate("text", x = 0.75, y = 0.4, label = prawy_napastnik, size = 6, color = "white") +
      annotate("text", x = 0.5, y = 0.3, label = srodkowy_napastnik, size = 6, color = "white") +
      
      
      xlim(0, 1) +
      ylim(0, 1) +
      
     
      theme_void() +
      theme(
        plot.margin = margin(0, 0, 0, 0, "cm"),
        panel.background = element_rect(fill = "transparent", color = NA), 
        plot.background = element_rect(fill = "transparent", color = NA)
      ) +
      
      
      coord_fixed(ratio = 1)
  }
  file_path <- "comments.txt"
  
  
  if (!file.exists(file_path)) {
    file.create(file_path)
  } else {
   
    fileConn <- file(file_path)
    writeLines("", fileConn)
    close(fileConn)
  }
  
  
  comments <- reactiveVal(data.frame(Komentarz = character(), stringsAsFactors = FALSE))
  
  
  observeEvent(input$submit_comment, {
    if (input$user_comment != "") {
    
      new_comment <- data.frame(Komentarz = input$user_comment, stringsAsFactors = FALSE)
      updated_comments <- rbind(comments(), new_comment)
      comments(updated_comments)
      
      
      write.table(updated_comments, file_path, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\n", append = TRUE)
      
      
      updateTextInput(session, "user_comment", value = "")
    }
  })
  
  
  output$comments_table <- renderTable({
    comments()
  })
  
  output$flagGrid <- renderUI({
    fluidRow(
      lapply(1:length(wspolrzedne$nazwa_druzyny), function(i) {
        if (wspolrzedne$nazwa_druzyny[i] != "Bośnia i Hercegowina" && wspolrzedne$nazwa_druzyny[i] != "Macedonia Północna") {
          column(
            width = 2,  
            div(
              class = "flag-box",
              actionLink(
                inputId = paste0("flag_", i),
                label = tags$img(
                  src = paste0("flags/", wspolrzedne$nazwa_druzyny[i], ".png"), 
                  alt = wspolrzedne$nazwa_druzyny[i],
                  style = "width: 100px; height: auto;"  
                )
              )
            )
          )
        }
      })
    )
  })
  
  
  lapply(1:length(wspolrzedne$nazwa_druzyny), function(i) {
    observeEvent(input[[paste0("flag_", i)]], {
      selectedCountry <- wspolrzedne$nazwa_druzyny[i]
      population <- wspolrzedne$populacja[i]
      capital <- wspolrzedne$capital[i]
      showModal(modalDialog(
        title = paste("", selectedCountry),
        tagList(
          p("Kraj:", selectedCountry),
          p(paste("Stolica:", capital)),
          p(paste("Populacja:", prettyNum(population, big.mark = ",", scientific = FALSE))),
          
          
        ),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
 
  
  
  output$tabelaDruzyny <- renderDT({
    datatable(dane_druzyny[, c("nazwa_druzyny", "Trener")],
              options = list(autoWidth = TRUE),
              colnames = c("Nazwa Drużyny", "Trener"),
              rownames = FALSE)
  })
  
  
  observe({
    updateSelectInput(session, "wybranaDruzyna", choices = dane_druzyny$nazwa_druzyny)
  })
  
  #DRUGA TABELA // działa
  output$tabelaZawodnikow <- renderTable({
    wybrana_druzyna <- input$wybranaDruzyna
    if (is.null(wybrana_druzyna)) {
      return()
    }
    

    
  #3 Zawodnicy
    dane_filtrowane <- dane_polaczone %>%
      filter(nazwa_druzyny == wybrana_druzyna) %>%
      mutate(data_urodzenia = as.Date(data_urodzenia),
             wiek = as.integer(wiek)) %>%
      mutate(data_urodzenia = format(data_urodzenia, "%Y-%m-%d")) %>%
      select("Nazwa Zawodnika" = imie_nazwisko_zawodnika, "Data urodzenia" = data_urodzenia, "Wiek" = wiek, "Pozycja" = pozycja, "Klub" = klub)
    
    dane_filtrowane
  })
  
  
  
  #4 DZIAŁJĄCA MAPA, KTÓRA WYŚWIETLA MAPE WRAZ Z JEJ NARODOWOŚCIAMI

  output$mapa <- renderLeaflet({
    leaflet(data = dane_druzyny) %>%
      addTiles() %>% 
      addPolygons(data = granice_europy, fillOpacity = 0, color = "grey") %>%
      addMarkers(
        lng = ~longitude,  
        lat = ~latitude, 
        popup = ~paste("Kraj: ", nazwa_druzyny, "<br>Trener: ", Trener, "<br>Ranking FIFA:", punkty_fifa),
        options = markerOptions(
          riseOnHover = TRUE
        )
      )
    

  })

  observeEvent(input$wybor_druzyny, {
    wybrana_druzyna <- dane_druzyny[dane_druzyny$nazwa_druzyny == input$wybor_druzyny, ]
    if (nrow(wybrana_druzyna) > 0) {
      leafletProxy("mapa", session) %>%
        setView(lng = wybrana_druzyna$longitude, lat = wybrana_druzyna$latitude, zoom = 7)
    }
  })
  


  dane_filtr <- reactive({
    dane_polaczone %>%
      filter(wiek >= input$min_wiek, wiek <= input$max_wiek) %>%
      select("Nazwa Zawodnika" = imie_nazwisko_zawodnika,"Data urodzenia" = data_urodzenia, "Wiek" =wiek, "Klub
      " = klub, "Reprezentacja" = nazwa_druzyny)
  })
 
  observeEvent(input$reset_filters, {
    updateSliderInput(session, "min_wiek", value = min(dane_zawodnicy$wiek))
    updateSliderInput(session, "max_wiek", value = max(dane_zawodnicy$wiek))
  })
  
  
  output$histogramPlot <- renderPlotly({
    if (input$wybor_histogramu == "Wiek") {
      plot_ly(dane_polaczone, x = ~wiek, type = 'histogram',
              marker = list(
                color = '#4F60FF', 
                line = list(
                  color = 'black',
                  width = 1        
                ))) %>%
        layout(
          xaxis = list(title = 'Wiek'),
          yaxis = list(title = 'Liczba Piłkarzy')
        )
    } else if (input$wybor_histogramu == "Kluby") {
      klub_counts <- dane_polaczone %>%
        group_by(klub) %>%
        summarise(Liczba_Zawodnikow = n()) %>%
        filter(Liczba_Zawodnikow >= 3) %>%
        arrange(desc(Liczba_Zawodnikow))
      
      plot_ly(klub_counts, x = ~klub, y = ~Liczba_Zawodnikow, type = 'bar',
              marker = list(
                color = '#FFA07A',  
                line = list(
                  color = 'black',  
                  width = 1         
                ))) %>%
        layout(
          xaxis = list(title = 'Klub', tickangle = -45),  
          yaxis = list(title = 'Liczba Zawodników'),
          title = 'Histogram liczby zawodników w klubach'
        )
    }
  })
  
  
  output$details <- renderPrint({
    if (input$wybor_histogramu == "Wiek") {
      event_data <- event_data("plotly_click")
      if (!is.null(event_data)) {
        wiek <- event_data$x
        liczba_pilkarzy <- sum(dane_polaczone$wiek == wiek)
        cat("Wybrany wiek:", wiek, "\nLiczba piłkarzy w tym wieku:", liczba_pilkarzy)
      }
    } else if (input$wybor_histogramu == "Kluby") {
      event_data <- event_data("plotly_click")
      if (!is.null(event_data)) {
        klub <- event_data$x
        
       
        zawodnicy <- dane_polaczone %>%
          filter(klub == !!klub) %>%
          pull(imie_nazwisko_zawodnika)
        
        if (length(zawodnicy) > 0) {
          cat("Zawodnicy grający w klubie:", klub, ":\n")
          cat(zawodnicy, sep = "\n")
        } else {
          cat("Brak danych o zawodnikach dla tego klubu.")
        }
      }
    }
  })
  output$lista_pilkarzy <- renderUI({
    event_data <- event_data("plotly_click")
    
    if (!is.null(event_data)) {
      if (input$wybor_histogramu == "Wiek") {
        wybrany_wiek <- event_data$x
        
        
        piłkarze <- dane_polaczone %>% 
          filter(wiek == wybrany_wiek) %>% 
          pull(imie_nazwisko_zawodnika) %>% 
          paste(collapse = ", ")
        
        if (piłkarze == "") piłkarze <- "Brak piłkarzy w tym wieku."
        HTML(paste("<strong>Piłkarze w wieku", wybrany_wiek, "lat:</strong>", piłkarze))
        
      } else if (input$wybor_histogramu == "Kluby") {
       
        return(NULL)
      }
    }
  })
  

  
  output$tabela_wiek <- renderDataTable({
    dane_filtr()
  }, options = list(lengthMenu = c(20, 50, 60), pageLength = 20)) 

  
  
  
  output$wykresInteraktywny <- renderPlotly({
    plot_ly(dane_druzyny, x = ~ranking_fifa, y = ~punkty_fifa, type = 'scatter', mode = 'markers',
            text = ~paste("Nazwa drużyny:", nazwa_druzyny, "<br>Ranking FIFA:", ranking_fifa, "<br>Punkty FIFA:", punkty_fifa),
            hoverinfo = 'text', marker = list(size = 10)) %>%
      layout(
        xaxis = list(title = 'Ranking FIFA'),
        yaxis = list(title = 'Punkty FIFA', type = 'log'),  
        hovermode = 'closest'
      )
  })
  
  
  output$tabelaDruzyn <- DT::renderDataTable({
    selected_data <- event_data("plotly_selected")
    
    
    if (is.null(selected_data)) {
      return(NULL)
    } else {
      cat("Wybrane dane:\n")
      print(selected_data) 
      
     
      wybrane_druzyny <- dane_druzyny %>%
        filter(ranking_fifa %in% selected_data$x) %>%
        select(
          Kraj = nazwa_druzyny, 
          Trener = Trener, 
          `Stadion narodowy` = Stadion_Narodowy, 
          `Punkty FIFA` = punkty_fifa, 
          Stolica = capital
        )
      
      
      if (nrow(wybrane_druzyny) > 0) {
        DT::datatable(wybrane_druzyny, options = list(pageLength = 10))
      } else {
        return(NULL)
      }
    }
  })
  
  

 
  wybrani_zawodnicy <- reactive({
    data <- dane_polaczone[dane_polaczone$klub == input$wybrany_klub, c("imie_nazwisko_zawodnika", "nazwa_druzyny", "klub")]
    
 
    colnames(data) <- c("Piłkarz", "Narodowość", "Klub")
      data
  })
  
  
  output$tabelakluby <- renderTable({
    wybrani_zawodnicy()
  })
  
 
  
  output$iloscPiłkarzy <- renderText({
    ilosc_pilkarzy <- nrow(wybrani_zawodnicy())
    paste("Ilość piłkarzy grających w tym samym klubie:", ilosc_pilkarzy)
  })

  #EURO
  
 
    observeEvent(input$show, {
    output$euroTable <- renderReactable({
      reactable(
        euro,
        defaultPageSize = 12, 
        searchable = TRUE,
        showPageInfo = FALSE,
        columns = list(
          `Miejsce.końcowe` = colDef(name = "Miejsce końcowe", cell = function(value, index) {
            if (value == 1) {
              
              div(style = "background-color: #d2b82e; padding: 8px; text-decoration: underline; font-weight: bold;", value)
            } else if (value == 2) {
              
              div(style = "background-color: silver; padding: 8px; font-weight: bold;", value)
            } else if (value == 3) {
              
              div(style = "background-color: #cd7f32; padding: 8px; font-weight: bold;", value)
              
            } else {
              value
            }
          }),
          `Drużyna` = colDef(name = "Drużyna", cell = function(value, index) {
            if (index == 1) {
              
              div(style = "background-color: #d2b82e; padding: 8px;", value)
            } else if (index == 2) {
              
              div(style = "background-color: silver; padding: 8px;", value)
            } else if (index == 3) {
              
              div(style = "background-color: #cd7f32; padding: 8px;", value)
            } else if (index == 4) {
              
              div(style = "background-color: #cd7f32; padding: 8px;", value)
            } else {
              value
            }
          }),
          `Grupa` = colDef(show = FALSE)
        )
      )
    })
    })
    
    output$table3 <- renderFormattable({
      formattable(Grupy_clean, list(
        Grupa = formatter("span"),      
        Drużyna1 = formatter("span"),   
        Drużyna2 = formatter("span")    
      ))
    })
    
    output$table <- renderDataTable({
      datatable(
        kwalifikacje,
        options = list(
          pageLength = 10,    
          autoWidth = TRUE,  
          dom = 'Bfrtip',     
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')  
        ),
        extensions = 'Buttons',  
        rownames = FALSE  
      )
    })
  
 
    
    
    output$groupTable <- renderReactable({
      euro <- euro[order(euro$Grupa), ] 
      
      reactable(
        euro,
        bordered = TRUE,    
        striped = TRUE,     
        highlight = TRUE,   
        columns = list(
          `Grupa` = colDef(
            name = "Grupa",
            headerStyle = list(
              background = "#f2f2f2",  
              fontWeight = "bold"      
            )
          ),
          `Drużyna` = colDef(
            name = "Drużyna",
            headerStyle = list(
              background = "#f2f2f2",  
              fontWeight = "bold"      
            )
          ),
          `Miejsce.końcowe` = colDef(show = FALSE)
        ),
        groupBy = "Grupa",  
        style = list(
          fontFamily = "Arial, sans-serif",  
          fontSize = "14px",                 
          backgroundColor = "white"          
        )
      )
    })

output$tabela_mecze <- renderReactable({
  reactable(
    dane,
    searchable = TRUE,  
    filterable = TRUE,  
    columns = list(
      Grupa = colDef(
        name = "Grupa", 
        sortable = FALSE,
        filterInput = TRUE,  
        cell = function(value) {
          
          color <- if (grepl("Grupa A", value)) {
            "#f8d7da"
          } else if (grepl("Grupa B", value)) {
            "#d4edda"
          } else if (grepl("Grupa C", value)) {
            "#d1ecf1"
          } else if (grepl("Grupa D", value)) {
            "#fff3cd"
          } else if (grepl("Grupa E", value)) {
            "#e2e3e5"
          } else if (grepl("Grupa F", value)) {
            "#c3e6cb"
          } else if (grepl("1/8 finału", value)) {
            "#b8daff"
          } else if (grepl("Ćwierćfinały", value)) {
            "#f5c6cb"
          } else if (grepl("Półfinały", value)) {
            "#bee5eb"
          } else if (grepl("Finał", value)) {
            "#ffeeba"
          } else {
            "white"
          }
          div(style = paste("background-color:", color, "; padding: 8px; text-align: center;"), value)
        }
      ),
      Stadion = colDef(
        name = "Stadion", 
        sortable = FALSE,
        filterable = TRUE,  
        align = "center"    
      ),
      Mecz = colDef(
        name = "Mecz", 
        sortable = FALSE,
        filterable = TRUE,  
        align = "center"   
      ),
      Frekwencja = colDef(
        name = "Frekwencja", 
        sortable = TRUE, 
        format = colFormat(separators = TRUE),
        filterable = TRUE,  
        align = "center",   
        footer = function(values) {
          
          sprintf("Suma: %s", format(sum(values, na.rm = TRUE), big.mark = ","))
        }
      ),
      Data = colDef(
        name = "Data", 
        sortable = TRUE,
        filterable = TRUE,  
        align = "center"   
      )
    ),
    groupBy = "Grupa",  
    bordered = TRUE,
    striped = TRUE,
    highlight = TRUE,
    defaultPageSize = 10,  
    theme = reactableTheme(
      headerStyle = list(
        backgroundColor = "#343a40",
        color = "white",
        fontWeight = "bold",
        textAlign = "center" 
      ),
      borderColor = "#6c757d"
    )
  )
  })

output$bracket <- renderDiagrammeR({
  drabinka <- "
    graph TD;
        A1[Hiszpania 4-1 Gruzja] -->|Ćwierćfinał| Q1[Hiszpania 2-1 Niemcy];
        A2[Niemcy 2-0 Dania] -->|Ćwierćfinał| Q1;
        A3[Portugalia 3-1 Słowenia] -->|Ćwierćfinał| Q2[Francja 5-3 Portugalia];
        A4[Francja 5-0 Belgia] -->|Ćwierćfinał| Q2;
        Q1 -->|Półfinał| S1[Hiszpania 2-1 Francja];
        Q2 -->|Półfinał| S1;
        
        A5[Holandia 2-1 Turcja] -->|Ćwierćfinał| Q3[Holandia 2-1 Anglia];
        A6[Anglia 1-0 Szwajcaria] -->|Ćwierćfinał| Q3;
        A7[Austria 1-2 Turcja] -->|Ćwierćfinał| Q4[Holandia 2-1 Turcja];
        A8[Anglia 5-3 Szwajcaria] -->|Ćwierćfinał| Q4;
        Q3 -->|Półfinał| S2[Holandia 1-2 Anglia];
        Q4 -->|Półfinał| S2;
        
        S1 -->|Finał| F[Hiszpania 2-1 Anglia];
        S2 -->|Finał| F;

        style A1 fill:#C5E1A5,stroke:#9CCC65,stroke-width:2px;
        style A2 fill:#C5E1A5,stroke:#9CCC65,stroke-width:2px;
        style A3 fill:#C5E1A5,stroke:#9CCC65,stroke-width:2px;
        style A4 fill:#C5E1A5,stroke:#9CCC65,stroke-width:2px;
        style A5 fill:#C5E1A5,stroke:#9CCC65,stroke-width:2px;
        style A6 fill:#C5E1A5,stroke:#9CCC65,stroke-width:2px;
        style A7 fill:#C5E1A5,stroke:#9CCC65,stroke-width:2px;
        style A8 fill:#C5E1A5,stroke:#9CCC65,stroke-width:2px;

        style Q1 fill:#90CAF9,stroke:#42A5F5,stroke-width:3px;
        style Q2 fill:#90CAF9,stroke:#42A5F5,stroke-width:3px;
        style Q3 fill:#90CAF9,stroke:#42A5F5,stroke-width:3px;
        style Q4 fill:#90CAF9,stroke:#42A5F5,stroke-width:3px;

        style S1 fill:#FFCC80,stroke:#FFA726,stroke-width:4px, font-weight:bold;
        style S2 fill:#FFCC80,stroke:#FFA726,stroke-width:4px, font-weight:bold;

        style F fill:#EF9A9A,stroke:#E57373,stroke-width:5px, font-weight:bold;
    "
  grViz(drabinka)
})

 output$stadionList <- renderUI({
    lapply(1:nrow(stadion_data), function(i) {
      div(
        class = "stadion-box",  
        actionLink(inputId = stadion_data$Stadion[i], label = stadion_data$Stadion[i])
      )
    })
  })
  
  
  selectedStadion <- stadion_data[1, ]
  
  output$stadionDetails <- renderUI({
    tagList(
      h4(selectedStadion$Stadion, class = "title-text"),
      div(class = "stadium-details",
          p(paste("Pojemność –", selectedStadion$Pojemność)),
          p(paste("Miasto –", selectedStadion$Miasto)),
          p(paste("Klub –", selectedStadion$Klub)),
          img(src = selectedStadion$Zdjęcie, height = "300px", style = "display: block; margin-left: auto; margin-right: auto;")
      )
    )
  })
  
  output$matchesTable <- renderTable({
    data.frame("Mecze" = unlist(selectedStadion$Mecze))
  }, striped = TRUE, bordered = TRUE, class = 'styled-table')
  
  
  lapply(1:nrow(stadion_data), function(i) {
    observeEvent(input[[stadion_data$Stadion[i]]], {
      selectedStadion <- stadion_data[i, ]
      
      output$stadionDetails <- renderUI({
        tagList(
          h4(selectedStadion$Stadion, class = "title-text"),
          div(class = "stadium-details",
              p(paste("Pojemność –", selectedStadion$Pojemność)),
              p(paste("Miasto –", selectedStadion$Miasto)),
              p(paste("Klub –", selectedStadion$Klub)),
              img(src = selectedStadion$Zdjęcie, height = "300px", style = "display: block; margin-left: auto; margin-right: auto;")
          )
        )
      })
      
      output$matchesTable <- renderTable({
        data.frame("Mecze" = unlist(selectedStadion$Mecze))
      }, striped = TRUE, bordered = TRUE, class = 'styled-table')
    })
  })



  
  output$mapaStadionow <- renderLeaflet({
    
    customIcon <- makeIcon(
      iconUrl = "marker2.png",  
      iconWidth = 25, iconHeight = 20
    )
    
    leaflet(data = stadiony) %>%
      addTiles() %>%
      setView(lng = 10.4515, lat = 51.1657, zoom = 6) %>%
      
      addMarkers(
        lng = ~lng, 
        lat = ~lat, 
        icon = customIcon,  
        popup = ~paste(
          "<b>", name, "</b><br>",
          "Miasto: ", miasto, "<br>",
          "Pojemność: ", Pojemność, " miejsc"
        ),
        label = ~paste(name),  
        labelOptions = labelOptions(
          noHide = FALSE,
          direction = "auto",
          style = list(
            "color" = "blue", 
            "font-weight" = "bold",
            "background-color" = "white",
            "border-radius" = "4px",
            "border" = "1px solid #ccc"
          )
        )
      ) %>%
      
      addCircleMarkers(
        lng = ~lng, 
        lat = ~lat, 
        radius = 5, 
        color = "blue", 
        stroke = FALSE, 
        fillOpacity = 0.5,
        group = "stadiony"
      ) %>%
      addResetMapButton()  
  })

  output$tabelaKlasyfikacji <- renderReactable({
    reactable(
      tabela_klasyfikacji,
      striped = TRUE,  
      bordered = TRUE,  
      highlight = TRUE, 
      defaultPageSize = 12,  
      style = list(fontSize = "12px"), 
      theme = reactableTheme(
        headerStyle = list(
          backgroundColor = "#f7f7f7", 
          color = "#333", 
          fontWeight = "bold",
          textAlign = "center" 
        ),
        cellStyle = list(
          textAlign = "center" 
        ),
        rowStyle = list(
          backgroundColor = "#fafafa" 
        ),
        borderColor = "#ccc" 
      ),
      columns = list(
        Miejsce = colDef(align = "center", style = list(fontWeight = "bold", color = "#4CAF50")),
        Punkty = colDef(align = "center", style = list(color = "#FF5733")), 
        Grupa = colDef(align = "center", style = list(color = "#0066CC")),
        Mecze = colDef(align = "center"),
        Reprezentacja = colDef(align = "center"),
        Zwycięstwa = colDef(align = "center"),
        Remisy = colDef(align = "center"),
        Porażki = colDef(align = "center"),
        'Bramki Strzelone' = colDef(align = "center"),
        'Bramki Stracone' = colDef(align = "center"),
        'Bilans Bramek' = colDef(align = "center")

      
      )
    )
  })
  

  filteredData <- reactive({
    subset(df_gole, Kategoria == input$selectedCategory)
  })
  

  output$barPlot <- renderPlotly({
    plot_ly(
      df_gole,
      labels = ~Kategoria,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "text",
      text = ~paste("Kategoria:", Kategoria, "<br>Liczba goli:", length(Zawodnik)),
      marker = list(colors = RColorBrewer::brewer.pal(n = 4, name = "Set3"))
    ) %>%
      layout(title = "Proporcje goli w różnych kategoriach")
  })
  

  output$goalsTable <- renderDT({
    datatable(
      filteredData(),
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  observeEvent(input$reset_card, {
    updateTextInput(session, "name", value = "")
    updateSelectInput(session, "position", selected = "Napastnik")
    updateSliderInput(session, "pace", value = 50)
    updateSliderInput(session, "shooting", value = 50)
    updateSliderInput(session, "passing", value = 50)
    updateSliderInput(session, "defense", value = 50)
    updateSliderInput(session, "dribbling", value = 50)
    updateSliderInput(session, "physical", value = 50)
    output$playerCard <- NULL  
  })
  
  card_plot <- reactive({
    draw_player_card(input$name, input$position, input$pace, input$shooting, input$passing, input$defense, input$dribbling, input$physical, input$user_image$datapath)
  })
  
  observeEvent(input$generate_card, {
    output$playerCard <- renderPlot({
      card_plot()
    })
  })
  
  output$save_card <- downloadHandler(
    filename = function() {
      paste0("karta_zawodnika_", gsub(" ", "_", input$name), ".png")
    },
    content = function(file) {
      ggsave(file, plot = card_plot(), device = "png", width = 4, height = 6)
    }
  )
  
  
  
  


    observeEvent(input$generate_team, {
      output$boisko <- renderPlot({
        draw_team(input$bramkarz, input$lewy_obronca, input$cofniety_pomocnik, input$srodkowy_obronca, input$prawy_obronca, input$srodkowy_pomocnik, input$srodkowy_pomocnik2, input$srodkowy_pomocnik3, input$lewy_skrzydlo, input$prawy_napastnik, input$srodkowy_napastnik)
      })
      
      # Obliczanie średniej oceny
      srednia_ocena <- oblicz_srednia_ocena(input$bramkarz, input$lewy_obronca, input$srodkowy_obronca, input$cofniety_pomocnik, input$prawy_obronca, input$srodkowy_pomocnik, input$srodkowy_pomocnik2, input$srodkowy_pomocnik3, input$lewy_skrzydlo, input$prawy_napastnik, input$srodkowy_napastnik, oceny_zawodnikow)
      
      # Wyświetlanie średniej oceny w interfejsie użytkownika
      output$srednia_ocena <- renderText({
        paste("Średnia ocena składu:", round(srednia_ocena, 0))
      })
    })
  
  output$save_team <- downloadHandler(
    filename = function() {
      paste("druzyna_marzen", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = draw_team(input$bramkarz, input$lewy_obronca, input$cofniety_pomocnik, input$srodkowy_obronca, input$prawy_obronca, input$srodkowy_pomocnik, input$srodkowy_pomocnik2, input$srodkowy_pomocnik3, input$lewy_skrzydlo, input$prawy_napastnik, input$srodkowy_napastnik), width = 10, height = 10, units = "in", dpi = 300)
    }
  )
  
  observe({
    if (input$tabs == "team_tab") {
      reset_team_selection()
    }
  })
  
  reset_team_selection <- function() {
    updateSelectInput(session, "bramkarz", selected = "")
    updateSelectInput(session, "lewy_obronca", selected = "")
    updateSelectInput(session, "srodkowy_obronca", selected = "")
    updateSelectInput(session, "prawy_obronca", selected = "")
    updateSelectInput(session, "cofniety_pomocnik", selected = "")
    updateSelectInput(session, "srodkowy_pomocnik", selected = "")
    updateSelectInput(session, "srodkowy_pomocnik2", selected = "")
    updateSelectInput(session, "srodkowy_pomocnik3", selected = "")
    updateSelectInput(session, "lewy_skrzydlo", selected = "")
    updateSelectInput(session, "prawy_napastnik", selected = "")
    updateSelectInput(session, "srodkowy_napastnik", selected = "")
    
   
    output$boisko <- renderPlot({
      ggplot() + theme_void() 
    })
  }
  
  observe({
 
    all_players <- zawodnicy[["Środkowy Pomocnik"]]
    
    
    selected_players <- c(input$srodkowy_pomocnik, input$srodkowy_pomocnik2, input$srodkowy_pomocnik3)
    
   
    updateSelectInput(session, "srodkowy_pomocnik", choices = setdiff(all_players, selected_players[selected_players != input$srodkowy_pomocnik]), selected = input$srodkowy_pomocnik)
    updateSelectInput(session, "srodkowy_pomocnik2", choices = setdiff(all_players, selected_players[selected_players != input$srodkowy_pomocnik2]), selected = input$srodkowy_pomocnik2)
    updateSelectInput(session, "srodkowy_pomocnik3", choices = setdiff(all_players, selected_players[selected_players != input$srodkowy_pomocnik3]), selected = input$srodkowy_pomocnik3)
  })

  
  
  observeEvent(input$reset_team, {
    updateSelectInput(session, "bramkarz", selected = "")
    updateSelectInput(session, "lewy_obronca", selected = "")
    updateSelectInput(session, "srodkowy_obronca", selected = "")
    updateSelectInput(session, "prawy_obronca", selected = "")
    updateSelectInput(session, "cofniety_pomocnik", selected = "")
    updateSelectInput(session, "srodkowy_pomocnik", selected = "")
    updateSelectInput(session, "srodkowy_pomocnik2", selected = "")
    updateSelectInput(session, "srodkowy_pomocnik3", selected = "")
    updateSelectInput(session, "lewy_skrzydlo", selected = "")
    updateSelectInput(session, "prawy_napastnik", selected = "")
    updateSelectInput(session, "srodkowy_napastnik", selected = "")
    
    
    output$boisko <- renderPlot({
      ggplot() + theme_void() # Czyści pole z wizualizacją
    })
  })

  oblicz_srednia_ocena <- function(bramkarz, lewy_obronca, srodkowy_obronca, cofniety_pomocnik, prawy_obronca, srodkowy_pomocnik, srodkowy_pomocnik2, srodkowy_pomocnik3, lewy_skrzydlo, prawy_napastnik, srodkowy_napastnik, oceny_zawodnikow) {
    
    wybrane_oceny <- c(
      oceny_zawodnikow[[bramkarz]],
      oceny_zawodnikow[[lewy_obronca]],
      oceny_zawodnikow[[srodkowy_obronca]],
      oceny_zawodnikow[[cofniety_pomocnik]],
      oceny_zawodnikow[[prawy_obronca]],
      oceny_zawodnikow[[srodkowy_pomocnik]],
      oceny_zawodnikow[[srodkowy_pomocnik2]],
      oceny_zawodnikow[[srodkowy_pomocnik3]],
      oceny_zawodnikow[[lewy_skrzydlo]],
      oceny_zawodnikow[[prawy_napastnik]],
      oceny_zawodnikow[[srodkowy_napastnik]]
    )
    
    
    srednia_ocena <- mean(wybrane_oceny)
    return(srednia_ocena)
  }
  
  

}

shinyApp(ui = ui, server = server)
