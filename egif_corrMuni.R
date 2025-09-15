library(sf)
library(tidyverse)

setwd("C:/Users/pjgel/OneDrive - Generalitat de Catalunya/EGIF")  # Set the working directory

## Reading municipality table
Muni <- readxl::read_xlsx("./AUX_TABLES/MUNICIPIOS.xlsx") %>%
  drop_na(X,Y) %>%  # Remove rows with NA in X or Y
  st_as_sf(coords=c("X","Y")) %>%  # Convert to spatial dataframe
  mutate(X=st_coordinates(.)[,1],  # Extract X coordinate
         Y=st_coordinates(.)[,2]) %>%  # Extract Y coordinate
  st_set_crs(32630)  # Set the coordinate reference system


# Reading the PIF1 table where the location of fires is and filtering those without coordinates
# or outside the latitude of the Spanish territory
PIF1 <- read_csv2("./AUX_TABLES/PIF1.csv") %>% 
  drop_na(X) %>%  # Remove rows with NA in X
  drop_na(Y) %>%  # Remove rows with NA in Y
  filter(Y<4862438,  # Filter by Y coordinate range
         Y>3980000)

# Empty list to fill with corrected points
list_pt_def <- list()

# Loop to iterate and find the correct location of points
for(i in 1:nrow(PIF1)){
  print(i)
  # Target ignition point (PI)
  test_pt <- PIF1[i,]%>% st_as_sf(coords = c("X","Y")) %>%  # Convert to spatial dataframe
    mutate(X_or=st_coordinates(.)[,1],  # Original X coordinate
           Y_or=st_coordinates(.)[,2])  # Original Y coordinate
  
  test_pt <- st_set_crs(test_pt,as.numeric(paste0(326,test_pt %>% as_tibble() %>%  
                                                    select(HUSO) %>% pull())))  # Set CRS based on PI HUSO
  # Municipality of the target PI
  Muni_test <- Muni %>% filter(IDPROVINCIA==test_pt$IDPROVINCIA,  # Filter by province
                               IDMUNICIPIO==test_pt$IDMUNICIPIO)  # and municipality
  
  # If the PI has no associated municipality, end the loop with the UTM zone it has been 
  # in the table
  if(nrow(Muni_test)==0){
    list_pt_def[[i]] <- test_pt %>% st_set_crs(as.numeric(paste0(326,test_pt %>% as_tibble() %>%  select(HUSO) %>% pull()))) %>% 
      mutate(HUSO_rev=test_pt %>% as_tibble() %>%  select(HUSO) %>% pull(),  # Revised HUSO
             dist_XY_Muni=NA) %>%  # Distance to municipality NA
      st_transform(4326) %>%  # Transform to WGS84
      mutate(lon=st_coordinates(.)[,1],  # Longitude
             lat=st_coordinates(.)[,2])  # Latitude
    next
  }
  # Distances between the municipality in each UTM zone
  d1 <- st_distance(test_pt %>% st_set_crs(32629), Muni_test %>% st_transform(32629))
  d2 <- st_distance(test_pt %>% st_set_crs(32630), Muni_test %>% st_transform(32630))
  d3 <- st_distance(test_pt %>% st_set_crs(32631), Muni_test %>% st_transform(32631))
  
  # Summary table
  df_dist <- tibble(HUSO=c(29,30,31),dist=c(d1,d2,d3))
  
  # Setting the UTM ZONE based on minimum distance
  EPSG <- as.numeric(paste0(326,df_dist %>% filter(dist==min(dist)) %>% select(HUSO) %>% pull()))
  
  # Save the point in the list
  list_pt_def[[i]] <- test_pt %>% st_set_crs(EPSG) %>% 
    mutate(HUSO_rev=df_dist %>% filter(dist==min(dist)) %>% select(HUSO) %>% pull(),  # Revised HUSO
           dist_XY_Muni=df_dist %>% filter(dist==min(dist)) %>% select(dist) %>% pull()) %>%  # Distance to municipality
    st_transform(4326) %>%  # Transform to WGS84
    mutate(lon=st_coordinates(.)[,1],  # Longitude
           lat=st_coordinates(.)[,2])  # Latitude
  
  
}


PIF1 <- bind_rows(list_pt_def) # Merge all points in one object
# st_write(PIF1,"./PIF1_pt_Step1.gpkg") # save as GEOPackage
PIF1 <- st_read("./PIF1_pt_Step1.gpkg")


# Points that fall outside the limits of the Spanish territory are removed. Process to be done in ARCgis



# Check if the points are within their province or in a 3km buffer. Otherwise, relocate it using the municipality's centroid

Prov_shp <- st_read("./Provincias/Limites_provinciales.shp") %>% st_transform(st_crs(4326))


list_prov <- list()

for(i in unique(Prov_shp$ID_EGIF)[1:48]){
  
  print(i)
  PROV_test <- Prov_shp %>% filter(ID_EGIF==i) %>% st_buffer(3000)  # Buffer of 3km from Province Limit
  
  pt_sel <- PIF1 %>% filter(IDPROVINCIA==unique(PROV_test$ID_EGIF)) %>% 
    mutate(Intersects=lengths(st_intersects(.,PROV_test))>0) %>%  # Check intersection
    left_join(Muni %>% as_tibble() %>%  
                select(IDPROVINCIA, 
                       IDMUNICIPIO, 
                       NOMBRE,X,Y),
              by=c("IDMUNICIPIO","IDPROVINCIA")) %>% 
    rename(X_Muni=X,
           Y_Muni=Y,
           X=X_or,
           Y=Y_or)
  
  
  pt_sel_ok <- pt_sel %>% filter(Intersects==T)%>% 
    select(-HUSO) %>% 
    rename(HUSO=HUSO_rev) %>% 
    st_drop_geometry()
  colnames(pt_sel_ok)
  
  if(nrow(pt_sel %>% 
          filter(Intersects==F)%>%
          drop_na(dist_XY_Muni))==0){
    message(paste0("N. of incorrect points:",0))
    list_prov[[length(list_prov)+1]] <- bind_rows(pt_sel_ok) %>% 
      select(-PARAJE,
             -IDCOI,
             -starts_with("dist"))
    next
  }else{
    message(paste0("N. of incorrect points:",nrow(pt_sel %>% filter(Intersects==F))))
    pt_sel_no <- pt_sel %>% filter(Intersects==F) %>% 
      as_tibble() %>% 
      select(-starts_with("geom"), lon, lat) %>% 
      drop_na(dist_XY_Muni) %>%
      st_as_sf(coords=c("X_Muni","Y_Muni")) %>% 
      st_set_crs(32630)%>% 
      mutate(X_Muni=st_coordinates(.)[,1],
             Y_Muni=st_coordinates(.)[,2]) %>% 
      st_transform(4326) %>% 
      mutate(lon=st_coordinates(.)[,1],
             lat=st_coordinates(.)[,2],
             X=X_Muni,
             Y=Y_Muni,
             HUSO=30) %>% 
      select(-HUSO_rev) %>% 
      st_drop_geometry()
    
    list_prov[[length(list_prov)+1]] <- bind_rows(pt_sel_no,
                                                  pt_sel_ok) %>% 
      select(-PARAJE,
             -IDCOI,
             -starts_with("dist"))
  }
  
  
  
}

PIF1_ok <- bind_rows(list_prov)%>%
  st_as_sf(coords=c("lon","lat")) %>% 
  st_set_crs(4326)%>% 
  mutate(lat=st_coordinates(.)[,1],
         lon=st_coordinates(.)[,2])


# Auxilitmap# Auxiliary information

# PIF2 temporal information
PIF2 <- read_csv2("./AUX_TABLES/PIF2.csv") %>% 
  mutate(IDPIF=as.integer(IDPIF)) %>% 
  select(IDPIF, ends_with("CION"),ends_with("OE")) %>% 
  mutate(DETECCION= as.Date(DETECCION, format="%d/%m/%Y"),
         INCENDIOE= as.Date(INCENDIOE, format="%d/%m/%Y"),
         DETECCION=as.POSIXct(paste(DETECCION, HDETECCION), format="%Y-%m-%d %H:%M:%S"),
         EXTINCION=as.POSIXct(paste(INCENDIOE, HINCENDIOE), format="%Y-%m-%d %H:%M:%S")) %>% 
  select(IDPIF,DETECCION,EXTINCION)

# PIF4 Cause information
PIF4 <- read_csv2("./AUX_TABLES/PIF4.csv") %>% 
  select(IDPIF, ends_with("AUSA"),IDCAUSAS,IDCLASEDIA) %>% 
  mutate(IDPIF=as.integer(IDPIF)) %>% 
  mutate_if(is.numeric, as.integer)

# PIF9 Burned surface information
PIF9 <- read_csv2("./AUX_TABLES/PIF9.csv") %>% 
  mutate(IDPIF=as.integer(IDPIF)) %>% 
  select(IDPIF, starts_with("TOT")) %>% 
  mutate(TOTAL_SUP=TOTALAR+TOTALNAR)

# Relational tables with names of provinces, CCAA, CAUSES, and Day Class
PROV_names <- readxl::read_xlsx("./AUX_TABLES//PROVINCIAS.xlsx") %>% select(starts_with("IDP"),NOMBRE) %>% 
  rename(NAMEPROV=NOMBRE)
CCAA_names <- readxl::read_xlsx("./AUX_TABLES/COMUNIDADES.xlsx") %>% select(starts_with("ID"),NOMBRE) %>% 
  rename(NAMECCAA=NOMBRE)

grcausas <- readxl::read_xlsx("./AUX_TABLES/GRUPOCAUSAS.xlsx") %>% filter(IDIDIOMA==0) %>% select(IDGRUPOCAUSA,DESCRIPCION) %>% 
  rename(IDCAUSA=1,
         CAUSADESC=2)

clase_dia <- readxl::read_xlsx("./AUX_TABLES/CLASEDIA.xlsx") %>% filter(IDIDIOMA==0) %>% select(-IDIDIOMA) %>% 
  rename(CLASEDIADESC=2)

PIF_def <- PIF1_ok %>% select(-IDENTIDADMENOR) %>%
  left_join(PIF2, by="IDPIF") %>% 
  left_join(PIF4, by="IDPIF") %>% 
  left_join(PIF9, by="IDPIF") %>% 
  left_join(CCAA_names, by="IDCOMUNIDAD") %>% 
  left_join(PROV_names, by="IDPROVINCIA") %>% 
  left_join(grcausas, by="IDCAUSA") %>% 
  left_join(clase_dia, by="IDCLASEDIA") %>% 
  rename(IDCCAA=IDCOMUNIDAD,
         IDPROV=IDPROVINCIA,
         IDMUNI=IDMUNICIPIO,
         NAMEMUNI=NOMBRE,
         X_MUNI=X_Muni,
         Y_MUNI=Y_Muni) %>% 
  mutate(AÑO=year(DETECCION)) %>%  # Year of detection
  select(IDPIF,DETECCION,EXTINCION,
         AÑO,IDCCAA,
         NAMECCAA,IDPROV,NAMEPROV,
         IDMUNI,
         NAMEMUNI,
         X_MUNI,
         Y_MUNI,
         X,
         Y,
         HUSO,
         lon,
         lat,
         IDCAUSA,
         CAUSADESC,
         IDCLASEDIA,
         CLASEDIADESC,
         TOTALAR,
         TOTALNAR,
         TOTAL_SUP,
         Intersects)

st_write(PIF_def, "./EGIF_spBBDD.gpkg")
