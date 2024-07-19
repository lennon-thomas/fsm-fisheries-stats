##########################
## Paths to directories ##
##########################

### SET GRIT SERVER AND OS ###
sys_path <- ifelse(Sys.info()["nodename"] == "quebracho" | Sys.info()["nodename"] == "sequoia",
                   "/home",
                   # Otherwise, set the directory for local machines based on the OS
                   # If using Mac OS, the directory will be automatically set as follows
                   ifelse(Sys.info()["sysname"]=="Darwin",
                          "/Users/Shared/nextcloud",
                          # If using Windows, the directory will be automatically set as follows
                          ifelse(Sys.info()["sysname"]=="Windows",
                                 "G:/Shared\ drives/nextcloud",
                                 # If using Linux, will need to manually modify the following directory path based on their user name
                                 # Replace your_username with your local machine user name
                                 "/home/your_username/Nextcloud")))

### HOME DIRECTORIES ###
# Path to emlab's data folder
data_path <- file.path(sys_path, "emLab", "data")
# Path to Blue Prosperity Coalition's top folder
bpc_path <- file.path(sys_path, "emlab-waitt", "blue-prosperity-coalition")

# FSM CRS
fsm_crs <- 3832
# PNA EEZ borders
pna <- sf::st_read(file.path(bpc_path, "site-based-work/fsm/fsm-vds/climate-change-expand", "data", "processed", "pna_eezs_updated_moll.gpkg")) %>% 
  sf::st_transform(crs = fsm_crs)
# FSM EEZ border
fsm <- dplyr::filter(pna, iso_sov1 == "FSM")