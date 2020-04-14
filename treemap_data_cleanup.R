library(tidyverse)
library(magrittr)

#read data
kk_trees_all <- read_csv('../data/trae_basis.csv')

#keep relevant columns and rename some variables and column names
kk_trees_all %<>% 
  rename(geometry = wkb_geometry, fredet = fredet_beskyttet_trae) %>%
  select(traeart,dansk_navn, slaegtsnavn, planteaar, fredet, geometry) %>%
  mutate(fredet = ifelse(fredet == "Ikke registreret", "", fredet))

#seperate geometry column into lon and lat columns
kk_trees_all$geometry <- gsub("\\(|\\)", "", kk_trees_all$geometry)

kk_trees_all %<>% 
  separate(geometry, c(NA, "lon", "lat"), sep = " ") %>%
  mutate(lon = as.double(lon), lat = as.double(lat))

#create species_genus dataframe to get an overview of genus and species names
species_genus <- kk_trees_all %>% group_by(traeart, dansk_navn, slaegtsnavn) %>% summarize(antal = n())
species_genus %<>% separate(traeart, into = "genus", sep = " ", remove = FALSE)

#create look-up table with corrected genus names based on existing names as they appear in the KK data
genus_names <- c("Abies" = "Ædelgran (Abies)",
                 "Acer" = "Løn (Acer)",
                 "Ácer" = "Løn (Acer)",
                 "Aesculus" = "Hestekastanje (Aesculus)",
                 "Ailanthus" = "Skyrækker (Ailanthus)",
                 "Albizia" = "Albizia",
                 "Almus" = "El (Alnus)",
                 "Alnus" = "El (Alnus)",
                 "Álnus" = "El (Alnus)",
                 "Amelachier" = "Bærmispel (Amelanchier)",
                 "Amelanchier" = "Bærmispel (Amelanchier)",
                 "Amelánchier" = "Bærmispel (Amelanchier)",
                 "Aralia" = "Aralie (Aralia)",
                 "Araucaria" = "Abeskræk (Araucaria)",
                 "Betula" = "Birk (Betula)",
                 "Bétula" = "Birk (Betula)",
                 "Buxus" = "Buksbom (Buxus)",        
                 "Carpinus" = "Avnbøg (Carpinus)",        
                 "Castanea" = "Kastanje (Castanea)",
                 "Catálpa" = "Trompetkrone (Catalpa)",
                 "Cedrús"  = "Ceder (Cedrus)",
                 "Cedrus" = "Ceder (Cedrus)",
                 "Cephalotaxus" = "Blommetaks (Cephalotaxus)",
                 "Cercidiphýllum" = "Hjertetræ (Cercidiphyllum)",
                 "Cercis" = "Judastræ (Cercis)",
                 "Chamaecyparis" ="Dværgcypres/Ædelcypres (Chamaecyparis)",
                 "Cornus" = "Kornel (Cornus)",
                 "Corylus" = "Hassel (Corylus)",
                 "Crataegus" = "Tjørn (Crataegus)",
                 "Cydónia" = "Kvæde (Cydonia)",
                 "Davídia" = "Duetræ (Davidia)",
                 "Eleagnus" = "Sølvblad (Elaeagnus)",
                 "Euódia" = "Tetradium/Euodia",
                 "Euonymus" = "Benved (Euonymus)",
                 "Fagus" = "Bøg (Fagus)",
                 "Fágus" = "Bøg (Fagus)",
                 "Fraxinus" = "Ask (Fraxinus)",
                 "Fráxinus" = "Ask (Fraxinus)",
                 "Ginkgo" = "Tempeltræ (Ginkgo)",
                 "Gleditsia" = "Tretorn (Gleditsia)",
                 "Gymnocladus" = "Stennød (Gymnocladus)",
                 "Hibiscus" = "Hibiscus",
                 "Ilex" = "Kristtorn (Ilex)",
                 "Júglans" = "Valnød (Juglans)",
                 "Juglans" = "Valnød (Juglans)",
                 "Juniperus" = "Ene (Juniperus)",
                 "Koelreuteria" = "Kinesertræ (Koelreutheria)",
                 "Laburnum" = "Guldregn (Laburnum)",
                 "Larix" = "Lærk (Larix)",
                 "Liquidambar" = "Ambratræ (Liquidambar)",
                 "Liquidámbar" = "Ambratræ (Liquidambar)",
                 "Liriodéndron" = "Tulipantræ (Liriodendron)",
                 "Macluara" = "Maclura",
                 "Magnolia" = "Magnolie (Magnolia)",
                 "Magnólia" = "Magnolie (Magnolia)",
                 "Malus" = "Æble (Malus)",
                 "Méspilus" = "Mispel (Mespilus)",
                 "Metasequoia" = "Vandgran (Metasequoia)",
                 "Mórus" = "Morbær (Morus)",
                 "Nothofágus" = "Sydbøg (Nothofagus)",
                 "Nyssa" = "Tupelotræ (Nyssa)",
                 "Ostrya" = "Humlebøg (Ostrya)",
                 "Parrotia" = "Papegøjebusk (Parrotia)",
                 "Paulównia" = "Kejsertræ (Paulownia)",
                 "Picea" = "Gran (Picea)",
                 "Pinus" = "Fyr (Pinus)",
                 "Pínus" = "Fyr (Pinus)",
                 "Platanus" = "Platan (Platanus)",
                 "Populus" = "Poppel (Populus)",
                 "Prunus" = "Kirsebær/Blomme/Stenfrugt (Prunus)",
                 "Prúnus" = "Kirsebær/Blomme/Stenfrugt (Prunus)",
                 "Pseudotsuga" = "Douglasgran (Pseudotsuga)",
                 "Pterocárya" = "Vingevalnød (Pterocarya)",
                 "Pyrus" = "Pære (Pyrus)",
                 "Quercus" = "Eg (Quercus)",
                 "Quércus" = "Eg (Quercus)",
                 "Rhus" = "Sumak (Rhus)",
                 "Robinia" = "Robinie (Robinia)",
                 "Robínia" = "Robinie (Robinia)", 
                 "Salix" = "Pil (Salicaceae)",
                 "Sambuscus" = "Hyld (Sambucus)",
                 "Sequoiadéndron" = "Mammuttræ (Sequoiadendron)",
                 "Sorbus" = "Røn (Sorbus)",
                 "Sórbus" = "Røn (Sorbus)",
                 "Styphnolobium" = "Pagodetræ (Styphnolobium)",
                 "Styrax" = "Styrax",
                 "Syringa" = "Syren (Syringa)",
                 "Taxodium" = "Sumpcypres (Taxodium)",
                 "Táxus" = "Taks (Taxus)",
                 "Taxus" = "Taks (Taxus)",
                 "Thuja" = "Thuja (Thuja)",
                 "Thujopsis" = "Hønsebenstræ (Thujopsis)",
                 "Tilia" = "Lind (Tilia)",
                 "Torreya" = "Nøddetaks (Torreya)",
                 "Tsuga" = "Hemlock (Tsuga)",
                 "Ulmus" = "Elm (Ulmus)",
                 "Viburnum" = "Kvalkved (Viburnum)",
                 "Zelkóva" = "Zelkova (Zelkova)")          
                                     
                  
genus_lookup <- tibble::enframe(genus_names)
colnames(genus_lookup) <- c("genus", "slaegtsnavn_rettet")

#create lookup table for entries with missing species names
genus_names_2 <- c("Ægte kastanie" = "Kastanje (Castanea)",
                   "Ask" = "Ask (Fraxinus)",
                   "Birk" = "Birk (Betula)",
                   "Bøg" = "Bøg (Fagus)",
                   "Ceder" = "Ceder (Cedrus)",
                   "Eg" = "Eg (Quercus)",
                   "El" = "El (Alnus)",
                   "Fyr" = "Fyr (Pinus)",
                   "Hestekastanie" = "Hestekastanje (Aesculus)",
                   "Kastanie" = "Kastanje (Castanea)",
                   "Kirsebær/Blomme" = "Kirsebær/Blomme/Stenfrugt (Prunus)",
                   "Lærk" = "Lærk (Larix)",
                   "Lind" = "Lind (Tilia)",
                   "Løn" = "Løn (Acer)",
                   "Pil" = "Pil (Salicaceae)",
                   "Platan" = "Platan (Platanus)",
                   "Poppel" = "Poppel (Populus)",
                   "Robinie" = "Robinie (Robinia)",
                   "Røn" = "Røn (Sorbus)",
                   "Taks" = "Taks (Taxus)",
                   "Thuja" = "Thuja (Thuja)",
                   "Tjørn" = "Tjørn (Crataegus)",
                   "Valnød" = "Valnød (Juglans)")

genus_lookup_2 <- tibble::enframe(genus_names_2)
colnames(genus_lookup_2) <- c("slaegtsnavn", "slaegtsnavn_rettet")
genus_lookup_2 %<>% mutate(traeart = "Ikke registreret")


#add corrected genus names to the full data table                                             
kk_trees_all %<>% separate(traeart, into = "genus", sep = " ", remove = FALSE)
kk_trees_all %<>% full_join(genus_lookup, by = "genus")
kk_trees_all %<>% full_join(genus_lookup_2, by = c("slaegtsnavn", "traeart")) %>%
  mutate(slaegtsnavn_rettet = coalesce(slaegtsnavn_rettet.x, slaegtsnavn_rettet.y)) %>% 
  select(-slaegtsnavn_rettet.x, -slaegtsnavn_rettet.y)

#check results are ok
species_genus_check <- kk_trees_all %>% group_by(traeart, dansk_navn, slaegtsnavn, slaegtsnavn_rettet) %>% summarize(antal = n())

#add species numbers
antal <- kk_trees_all %>% count(dansk_navn)

kk_trees_all %<>%
  inner_join(antal, by = "dansk_navn") 

#save tidy dataset
saveRDS(kk_trees_all, file = 'treemap/kk_trees_all.RDS')

