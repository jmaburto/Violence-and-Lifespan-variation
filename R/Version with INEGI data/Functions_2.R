
# useful labels
CoD.name.vec <- c('Infectious and respiratory', 'Cancers', 'Circulatory',
                  'Birth conditions', 'Diabetes', 'Other AMS', 'IHD', 'HIV', 
                  'Suicide', 'Lung Cancer', 'Cirrhosis', 'Homicide',
                  'RTA', 'Other HD', 'Rest')

CoD.code.vec <- paste0('Cause',c(1:14,16))

CoD.name.vec2 <- c('Amenable','Diabetes','IHD', 'HIV', 
                   'Suicide', 'Lung Cancer', 'Cirrhosis', 'Homicide',
                   'Road traffic accidents', 'Other')

CoD.code.vec2 <- 1:10

state.name.vec <- c("National","Aguascalientes","Baja California","Baja California Sur","Campeche",
                    "Coahuila","Colima","Chiapas","Chihuahua","Mexico City","Durango",
                    "Guanajuato","Guerrero","Hidalgo","Jalisco","Mexico State","Michoacan",
                    "Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla","Queretaro",
                    "Quintana Roo","San Luis Potosi","Sinaloa","Sonora","Tabasco","Tamaulipas",
                    "Tlaxcala","Veracruz","Yucatan","Zacatecas")

state.code.vec <- 0:32

names(state.name.vec) <- state.code.vec

region.recvec            <- c(0,2,3,3,1,3,2,1,3,2,3,2,1,2,2,2,
                              2,1,2,3,1,1,2,1,3,3,3,1,3,2,1,1,3)

names(region.recvec)     <- 0:32

#### sex=1 <- males
#### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 
#### 4. Birth, 5. Diabetes, 6. Other Medical Care AM
#### 7. IHD, 8. HIV, 9. Suicide, 10. Lung Cancer,
#### 11. Cirrhosis, 12. Homicide, 13. Road traffic accidents, 
#### 14. other heart diseases, 15. Ill-defined causes, 16. All other Non-AM
#### Note: these data do not contain Not Specified categories
