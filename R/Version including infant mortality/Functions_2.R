
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
