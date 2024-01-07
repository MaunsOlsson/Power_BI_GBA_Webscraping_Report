library(rvest)
library(stringr)
library(tidyverse)

# Metacritic -----------------------------------------

## Läsa in varje sida ---------------------------------
# Man börjar att läsa in startsidan.
page <- "https://www.metacritic.com/browse/game/gba/all/all-time/metascore/?releaseYearMin=1958&releaseYearMax=2023&platform=gba&page=1"
# Om man kollar på startsidan så ser man att det finns flera olika spel (oftast 24) spel på varje sida och att varje sida startas med
# html 'https://www.metacritic.com/browse/game/gba/all/all-time/metascore/?releaseYearMin=1958&releaseYearMax=2023&platform=gba&page=[NUMMER]'

# Man kan därmed göra en for-loop, för att få fram html för alla sidor där spel är bevarade 
# Först skapas en vektor där for-loopen lagrar värdena i

pages <- c()
# Där en for-loopen går igenom varje värde genom
for(i in 1:19){
  pages[i] <- paste0("https://www.metacritic.com/browse/game/gba/all/all-time/metascore/?releaseYearMin=1958&releaseYearMax=2023&platform=gba&page=", i)
}

# Nu finns alla länkar som string, de behöver dock läsas som html i R. Dessa lagras i en lista.
pages_html <- list()
for(i in 1:19){
  # Läser in sidan
  pages_html[[i]] <- read_html(pages[i])
  # Lämnar ett meddelande till användaren att sidan har lästs in
  message(paste0("Page ", i, " has been successfully scraped!"))
  
  # Detta är om man skulle läsa in andra platformer med fler spel. Cooldown så att man inte blir utesluten!
  if(i %% 20 == 0){Sys.sleep(10)}
}

# När man har läst in varje sida, behöver man lista upp alla spel på varje individuell sida.
links <- list()
game_images <- list()

for(i in 1:19){
  # För att hämta listor VIKTIGT för att gå vidare i webscaping
  links[[i]] <- pages_html[[i]] %>% # För alla spel som är listade på varje sida länkar till spelets egna metacritic sida
    html_elements('div .c-finderProductCard.c-finderProductCard-game a')  %>% # därmed så behövs alla spel för varje sida
    html_attr('href') # läsas in för att få fram deras länkar. Spelets länkar förvaras i en lista med varje metacritic sida.
  
  # Den här koden skäms jag över, men det funkar. Detta lägger till metacritic's homepage, vilket annars inte sparas ner.
  links[[i]] <- paste0("https://www.metacritic.com", links[[i]])

  ### Samla bilder (kan tas bort vid behov) -----------
  # För att hämta game_images, inte lika viktig och kan kommenteras bort om detta inte fungerar
  game_images[[i]] <- pages_html[[i]] %>%
    html_elements('div .c-finderProductCard_img.g-height-100.g-width-100 picture img')  %>%
    html_attr('src')
}

## Metacritic scraper function ------------------------

# En funktion skapas vilket kan läsa in mått från varje spel's metacritic sida. Användaren får två parametrar. Website =
# hemsidan som hämtas ner och wait = intervallet mellan varje hemsida som skrapas. Wait är ett sätt att inte oavsiktligt DDOSA hemsidan och bli
# avstängd från den
MetaCritic_PageScarpe <- function(website, wait = 0){
  page <- read_html(website)
  message("Scraping URL: ", website)
  
# Hämtar ner namn och utsläpps datum. Dessa är lagrade i ett element som hela tiden har samma klass.
  name <- page %>%
    html_element("div.c-productHero_title.g-inner-spacing-bottom-medium.g-outer-spacing-top-medium") %>%
    html_text2()
  
  release <- page %>%
    html_element("div.g-text-xsmall span.u-text-uppercase") %>%
    html_text2()
  release <- lubridate::mdy(release)
  
  # Criticscore är svårare att få fram då det är lagrat i ett element som har olika klass beroende om spelet är kvalité, därmed behöver man använda
  # sig av Xpath, vilket är ett annat sätt att få fram rätt element
  criticscore <- page %>%
    html_element(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[4]/div/div[2]/div/div[1]/div[2]/div/div[1]/div/div/a/div/div') %>%
    html_text2() %>% as.numeric()
  
  # Hämtar ut alla N critics
  {
    # amount of reviews
    Ncritic <- page %>%
      html_elements(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[4]/div/div[2]/div/div[1]/div[2]/div/div[1]/div/div/div/div/span[2]/a/span') %>%
      html_text2()
    Ncritic <- gsub("[^0-9.-]", "", Ncritic) %>% as.numeric() # Detta tar bort alla tecken förutom de numeriska värdena och lagrar den som numerisk
    
    # amount of positive
    Ncritic_Pos <- page %>%
      html_elements(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[4]/div/div[2]/div/div[1]/div[2]/div/div[2]/div[2]/div[1]/span[2]') %>%
      html_text2()
    Ncritic_Pos <- gsub("[^0-9.-]", "", Ncritic_Pos) %>% as.numeric()
    
    # amount of mixed
    Ncritic_Mixed <- page %>%
      html_elements(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[4]/div/div[2]/div/div[1]/div[2]/div/div[2]/div[2]/div[2]/span[2]') %>%
      html_text2()
    Ncritic_Mixed <- gsub("[^0-9.-]", "", Ncritic_Mixed) %>% as.numeric()
    
    # amount of negative
    Ncritic_Neg <- page %>%
      html_elements(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[4]/div/div[2]/div/div[1]/div[2]/div/div[2]/div[2]/div[3]/span[2]') %>%
      html_text2()
    Ncritic_Neg <- gsub("[^0-9.-]", "", Ncritic_Neg) %>% as.numeric()
    
    # Om spelet saknar recension, så får man numeric(0), alltså en variabel som lagrar tomma värden.
    # Detta är inte samma som saknade värden (NA), vilket egentligen är av intresse, och gör så att df senare har 0 rader. Detta är fel och behöver åtgärdas.
    {
      if(length(Ncritic) == 0) Ncritic <- NA
      if(length(Ncritic_Pos) == 0) Ncritic_Pos <- NA
      if(length(Ncritic_Neg) == 0) Ncritic_Neg <- NA
      if(length(Ncritic_Mixed) == 0) Ncritic_Mixed <- NA
    }
    }
  
  userscore <- page %>%
    html_element(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[4]/div/div[4]/div/div[1]/div[2]/div/div[1]/div/div/a/div/div') %>%
    html_text2() %>% as.numeric()
  
  # Hämtar ut alla N user, se kommentar för N critic, då det är samma koncept, men på et annat element
  {
    # amount of reviews
    Nuser <- page %>%
      html_elements(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[4]/div/div[4]/div/div[1]/div[2]/div/div[1]/div/div/div/div/span[2]/a/span') %>%
      html_text2()
    Nuser <- gsub("[^0-9.-]", "", Nuser) %>% as.numeric()
    
    # amount of positive
    Nuser_Pos <- page %>%
      html_elements(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[4]/div/div[4]/div/div[1]/div[2]/div/div[2]/div[2]/div[1]/span[2]') %>%
      html_text2()
    Nuser_Pos <- gsub("[^0-9.-]", "", Nuser_Pos) %>% as.numeric()
    
    # amount of mixed
    Nuser_Mixed <- page %>%
      html_elements(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[4]/div/div[4]/div/div[1]/div[2]/div/div[2]/div[2]/div[2]/span[2]') %>%
      html_text2()
    Nuser_Mixed <- gsub("[^0-9.-]", "", Nuser_Mixed) %>% as.numeric()
    
    # amount of negative
    Nuser_Neg <- page %>%
      html_elements(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[4]/div/div[4]/div/div[1]/div[2]/div/div[2]/div[2]/div[3]/span[2]') %>%
      html_text2()
    Nuser_Neg <- gsub("[^0-9.-]", "", Nuser_Neg) %>% as.numeric()
    
    Nuser_var <- c(Nuser, Nuser_Mixed, Nuser_Neg, Nuser_Pos)
    
    # Condition if empty
    {
      if(length(Nuser) == 0) Nuser <- NA
      if(length(Nuser_Pos) == 0) Nuser_Pos <- NA
      if(length(Nuser_Neg) == 0) Nuser_Neg <- NA
      if(length(Nuser_Mixed) == 0) Nuser_Mixed <- NA
    }
    }
  
  # Hämtar ut utvecklare, utgivare mm.
  Dev <- page %>%
    html_element(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[6]/div/div/div[2]/div[2]/div[2]/div[1]/ul/li') %>%
    html_text2()
  
  Publisher <- page %>%
    html_element(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[6]/div/div/div[2]/div[2]/div[2]/div[2]/span[2]') %>%
    html_text2()
  
  Genre <- page %>%
    html_element(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[6]/div/div/div[2]/div[2]/div[3]/ul/li/div/a/span') %>%
    html_text2()
  
  Rate <- page %>%
    html_element(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[6]/div/div/div[2]/div[1]/div/div/div[2]/span[1]') %>%
    html_text2()
  
  url <- website
  
  # Stannar upp funktionen. Detta används så att man inte gör en DDOS på hemsidan
  Sys.sleep(wait)
  
  tibble(Name = name,
         Release = release,
         Developer = Dev,
         Publisher = Publisher,
         Rating = Rate,
         Genre = Genre,
         Critics_Score = criticscore,
         Number_Critics = Ncritic,
         Number_Critics_Pos = Ncritic_Pos,
         Number_Critics_Mixed = Ncritic_Mixed,
         Number_Critics_Negative = Ncritic_Neg,
         User_score = userscore,
         Number_User = Nuser,
         Number_User_Pos = Nuser_Pos,
         Number_User_Mixed = Nuser_Mixed,
         Number_User_Negative = Nuser_Neg,
         url = url)
}

## Faktisk implementering av funktion -----------------

# Skapar en lista som alla värden skapas i
results <- list()

# Faktiska implementeringen
for(i in 1:length(links)){ #Går igenom alla sidor där alla spel förvaras i
  results_page <- list() # Skapar en lista som alla spels mått förvaras i
  for(j in 1:length(links[[i]])){ # Går igenom spels mått för varje sida
    waittime <- sample(x = seq(from = 0.5, to = 5, by = 0.25), size = 1) # cooldowner, oregelbundet intervall med hjälp av sample
    results_page[[j]] <- MetaCritic_PageScarpe(website = links[[i]][[j]], wait = waittime) # Gör funktionen på varje spel
    message("Link sucessfully scraped!")
  }
  results[[i]] <- results_page # Spara varje sida i den skapade listan
  message(paste0("\nAll games from page ", i, " has been successfully scraped! Now waiting for next page...")) # Ge ett meddelande till användaren att en sida är läst
  Sys.sleep(15) # Sov några sekunder för att inte göra en DDOs på hemsidan och kunna fortsätta läsa in data
  message(paste0(round(x = i/length(links), digits = 4) * 100, "% of the given pages have been scraped \n"))
}

DF <- bind_rows(results, .id = 'url')
# Klar!

rm(results)
## Bearbetning av datamaterialet ----------------------

# Om links finns kvar. Av någon anledning sparas inte url i DF
DF$url <- unlist(links)
DF$image <- unlist(game_images)

# Snittpoäng mellan kritiker och användare
{
  DF$Mean_score <- (DF$Critics_Score/10 + DF$User_score)/2
  DF$Mean_score[is.na(DF$User_score)] <- DF$Critics_Score[is.na(DF$User_score)]/10
  DF$Mean_score[is.na(DF$User_score) & is.na(DF$Critics_Score)] <- NA
  DF$Mean_score[is.na(DF$Critics_Score)] <- DF$User_score[is.na(DF$Critics_Score)]
}

# Om kategorisering av variabler. Det är för många genrer som är lika. De måste kodas om.
{
  RPG <- c('Trainer RPG', 'JRPG', 'Action RPG', 'RPG', 'Roguelike')
  DF$Genre[DF$Genre %in% RPG] <- 'RPG'
  
  Puzzle <- c('Puzzle', 'Matching Puzzle', 'Action Puzzle', 'Logic Puzzle', 'Stacking Puzzle')
  DF$Genre[DF$Genre %in% Puzzle] <- 'Puzzle'
  
  Strategy <- c('Turn-Based Strategy', 'Turn-Based Tactics', 'Real-Time Strategy', 'Strategy')
  DF$Genre[DF$Genre %in% Strategy] <- 'Strategy'
  
  Shooter <- c('FPS', 'Third Person Shooter', "Shoot-'Em-Up", "Horizontal Shoot-'Em-Up",
               "Top-Down Shoot-'Em-Up", "Vertical Shoot-'Em-Up", "Rail Shooter")
  DF$Genre[DF$Genre %in% Shooter] <- 'Shooter'
  
  Sports <- c("Athletics",  "Baseball", "Baseball Sim", "Basketball", "Biking",
              "Dancing", "Fishing", "Football", "Football Sim", "Golf Sim", "Skating",
              "Skiing", "Soccer Sim", "Surfing", "Team Sports", "Tennis", "Rhythm")
  DF$Genre[DF$Genre %in% Sports] <- 'Sports'
  
  Racing <- c("Arcade Racing", "Auto Racing", "Auto Racing Sim", "Future Racing")
  DF$Genre[DF$Genre %in% Racing] <- 'Racing'
  
  Board <- c("Arcade", "Board", "Card Battle", "Compilation", "Gambling", "Party", "Pinball", "Trivia")
  DF$Genre[DF$Genre %in% Board] <- 'Board and arcade'
  
  Sim <- c("Aircraft Combat Sim", "Management", "Space Combat Sim", "Tycoon", "Vehicle Combat Sim", "Virtual Life", "Virtual Pet")
  DF$Genre[DF$Genre %in% Sim] <- 'Simulation'
  
  Fighting <- c("2D Beat-'Em-Up", "2D Fighting", "3D Beat-'Em-Up", "3D Fighting", "Combat Sport", "Wrestling")
  DF$Genre[DF$Genre %in% Fighting] <- 'Fighting'
  
  Platformer <- c("2D Platformer", "3D Platformer", "Metroidvania")
  DF$Genre[DF$Genre %in% Platformer] <- 'Platformer'
  
  Action <- c("Action", "Action Adventure", "Linear Action Adventure", "Open-World Action", "Point-and-Click", "Survival")
  DF$Genre[DF$Genre %in% Action] <- 'Action/Adventure'
  
  rm(RPG, Puzzle, Shooter, Sports, Racing, Board, Sim, Fighting, Platformer, Action)
  table(DF$Genre)
}

# Samma sak gäller tyvärr utgivare som konstant byter namn på utgivaren.
{
  # Utgivaren Buena Vista Games och Buena Vista Interactive benämns som olika utgivare när detta inte är fallet.
  # Dessutom är de också samma som Disney Interactive. Källa: https://en.wikipedia.org/wiki/Disney_Interactive_Studios
  Disney <- c('Buena Vista Games', 'Buena Vista Interactive', 'Disney Interactive')
  DF$Publisher[DF$Publisher %in% Disney] <- 'Disney Interactive'
  
  EA <- c("EA Games", "Electronic Arts", "EA Sports", "EA Sports Big")
  DF$Publisher[DF$Publisher %in% EA] <- "Electronic Arts"
  
  # Rockstar köptes av 
  # T2 <- c("Rockstar Games", "Take-Two Interactive")
  # DF$Publisher[DF$Publisher %in% EA] <- "Take-Two Interactive"
}

# Sen finns det dublikationer i datamaterialet
test <- data.frame(table(DF$Name))
test <- test[test$Freq > 1, ]
test
# Need for Speed: Underground 2 har två element på varsin sida av anledningar som är okända. Båda leder till samma sida. Denna försvann vid updatering ???
# Shonen Jump's Shaman King: Legacy of the Spirits, Soaring Hawk är egentligen inte en kopia utan är ett spel som heter
# Shonen Jump's Shaman King: Legacy of the Spirits, Sprinting Wolf, men som länkas till Shonen Jump's Shaman King: Legacy of the Spirits, Soaring Hawk
# Hemsidan visar olika betyg mellan de (på en recensent poäng högre, de båda har lågt)
# Dessa behöver tas bort
DF <- DF[-c(333),]


## Nedsparning och nedladdning : ----------------------

write.csv(x = DF, file = 'Location\\data.csv', row.names = FALSE)

DF <- read.csv(file = 'Location\\data.csv')
