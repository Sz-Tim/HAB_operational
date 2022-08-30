## -----------------------------------------------------------------------------
## HAB forecasts
## Initial setup
## Tim Szewczyk
## -----------------------------------------------------------------------------


# set up ------------------------------------------------------------------
library(tidyverse)
library(glue)
library(jsonlite)
library(lubridate)
# library(WeStCOMS) # devtools::install_github("Sz-Tim/WeStCOMS")
library(ncdf4)
library(sf)
source("code/00_fn.R")

cores <- 24

# WeStCOMS dates
dates.c <- list(v1_start="2014-10-28",
                v1_end="2019-04-01",
                v2_end="2022-06-01")
dates.n <- map(dates.c, ~str_remove_all(.x, "-"))

# directories
dirs <- switch(get_os(), 
               osx=list(proj=getwd(), 
                        part="~/Documents/SAMS/HAB_Operational/ptrack/",
                        wc="~/Documents/SAMS/WeStCOMS/",
                        mesh="~/Documents/SAMS/WeStCOMS/data/",
                        cprn="~/Documents/SAMS/00_gis/copernicus/"),
               linux=list(proj=getwd(),
                          part="/home/sa04ts/HAB_operational/ptrack/",
                          wc="/media/archiver/common/sa01da-work/",
                          mesh="/home/sa04ts/FVCOM_meshes/",
                          cprn="/home/sa04ts/gis/copernicus/"),
               # TODO: set up directories for testing on windows (maybe)
               windows=list(proj=getwd(),
                            part="ptrack/",
                            wc="/media/archiver/common/sa01da-work/",
                            mesh="../WeStCOMS/data/",
                            cprn="../00_gis/copernicus/"))

# site locations
fsa.id <- read_delim(paste0(dirs$part, "fsa_sites_v2.dat"), 
                     delim="\t", col_names=c("site.id", "x", "y"))# %>%
  # filter(!site.id %in% c(70, 74, 75, 80, 88))



# particle tracking -------------------------------------------------------
setwd(dirs$part)
setwd("past")
file.copy("../runPTrack_smn.sh", "runPTrack_smn.sh")

# WeStCOMS v1
wc1.prop <- setPartTrackProperties(parallelThreads=cores, 
                                   destinationDirectory="init_v1/",
                                   datadir=paste0(dirs$wc, "minch2/Archive/"),
                                   mesh1=paste0(dirs$mesh, "WeStCOMS_mesh.nc"),
                                   location="minch",
                                   minchVersion="2",
                                   sitefile="../../large_elem_centroids_v1.dat",
                                   sitefileEnd="../../fsa_sites_v2.dat",
                                   start_ymd=dates.n$v1_start,
                                   end_ymd=dates.n$v1_end)
cat(wc1.prop, "\n", file="HAB_WeStCOMS1.properties")
system2("bash", c("runPTrack_smn.sh", "HAB_WeStCOMS1.properties"))

# WeStCOMS v2
wc2.prop <- setPartTrackProperties(parallelThreads=cores,
                                   destinationDirectory="init_v2/",
                                   restartParticles="../init_v1/locationsEnd.dat",
                                   sitefile="../../large_elem_centroids_v2.dat",
                                   sitefileEnd="../../fsa_sites_v2.dat",
                                   start_ymd=dates.n$v1_end,
                                   end_ymd=dates.n$v2_end)
cat(wc2.prop, "\n", file="HAB_WeStCOMS2.properties")
system2("bash", c("../runPTrack_smn.sh", "HAB_WeStCOMS2.properties"))

# compile output
connect.f <- dir("init_v1/connectivity")
connect.v1 <- connect.f %>%
  map_dfr(~read_delim(glue("init_v1/connectivity/{.x}"), 
                      delim=" ", col_names=F, show_col_types=F) %>% 
            mutate(date=str_sub(.x, 14, 21)) %>%
            group_by(date) %>% 
            summarise(across(everything(), sum)) %>% 
            ungroup) %>%
  pivot_longer(-1, names_to="site.id", values_to="influx") %>%
  mutate(site.id=rep(fsa.id$site.id, length(connect.f)),
         date=ymd(date))
connect.f <- dir("init_v2/connectivity")
connect.v2 <- connect.f %>%
  map_dfr(~read_delim(glue("init_v1/connectivity/{.x}"), 
                      delim=" ", col_names=F, show_col_types=F) %>% 
            mutate(date=str_sub(.x, 14, 21)) %>%
            group_by(date) %>% 
            summarise(across(everything(), sum)) %>% 
            ungroup) %>%
  pivot_longer(-1, names_to="site.id", values_to="influx") %>%
  mutate(site.id=rep(fsa_v2$site.id, length(connect.f)),
         date=ymd(date))

bind_rows(connect.v1 %>% filter(! date %in% connect.v2$date), 
          connect.v2) %>%
  write_csv(paste0(dirs$proj, glue("/data/toDate/influx_{dates.n$v2_end}.csv")))

setwd(dirs$proj)


# 
# 
# # WeStCOMS ----------------------------------------------------------------
# nLags <- 7
# buffer_radius <- 5e3
# westcoms.dir <- paste0(dirs$wc,
#                        c("minch2/Archive/", "WeStCOMS2/Archive/"))
# mesh.f <- paste0(dirs$mesh,
#                  c("WeStCOMS_mesh.gpkg", "WeStCOMS2_mesh.gpkg"))
# 
# # load files
# mesh.sf <- map(mesh.f, loadMesh) 
# fsa.df <- glue("data/toDate/fsa_{dates.n$v2_end}.txt") %>% 
#   fromJSON() %>% clean_fsa(dates.c$v1_end)
# # match fsa site locations to grid elements
# hydro.i_L <- fsa.df %>%
#   group_by(grid) %>%
#   group_split() %>%
#   map2(.x=., .y=mesh.sf, 
#        ~st_as_sf(.x, coords=c("lon", "lat"), remove=F, crs=27700) %>% 
#          st_join(., .y %>% 
#                    select(-area) %>% 
#                    rename(depth.elem=depth,
#                           site.elem=i)) %>% 
#          filter(!is.na(site.elem)) %>%
#          st_drop_geometry) %>%
#   bind_rows %>%
#   mutate(depth=pmin(10, depth.elem),
#          obs.id=row_number())
# # match fsa site buffers to grid elements
# hydro.i_R <- hydro.i_L %>%
#   select(-site.elem, -depth.elem, -starts_with("trinode")) %>%
#   group_by(grid) %>%
#   group_split() %>%
#   map2(.x=., .y=mesh.sf, 
#        ~st_as_sf(.x, coords=c("lon", "lat"), remove=F, crs=27700) %>% 
#          st_buffer(dist=buffer_radius) %>%
#          st_join(., .y %>% 
#                    select(-area) %>% 
#                    rename(depth.elem=depth,
#                           site.elem=i)) %>% 
#          filter(!is.na(site.elem)) %>%
#          st_drop_geometry) %>%
#   bind_rows %>%
#   mutate(depth=pmin(10, depth.elem))
# write_csv(hydro.i_L, glue("data/toDate/hydro_i_L_{dates.n$v2_end}.csv"))
# write_csv(hydro.i_R, glue("data/toDate/hydro_i_R_{dates.n$v2_end}.csv"))
# 
# # set summary functions
# var.df <- tibble(var=c("temp", "salinity", "short_wave",
#                        "u", "v", "ww", "km",
#                        "uwind_speed", "vwind_speed", "precip"),
#                  dayFn=c(mean, mean, integrateShortWave,
#                          q90, q90, q90, q90,
#                          q90, q90, sum),
#                  depthFn=c(mean, mean, NA,
#                            mean, mean, mean, mean,
#                            NA, NA, NA))
# # extract by lagged dates
# hydro_L <- hydro_R <- vector("list", nLags)
# for(i in 0:nLags) {
#   cat("Starting lag", i, "\n")
#   hydro_L[[i+1]] <- hydro.i_L %>% 
#     select(obs.id, site.id, date, depth, grid, site.elem, starts_with("trinode")) %>%
#     mutate(date=str_remove_all(as.character(date-i), "-")) %>%
#     extractHydroVars(., westcoms.dir, var.df$var, var.df$dayFn, var.df$depthFn,
#                      cores=cores, progress=T) %>%
#     rename_with(~paste0(.x, "_L", i), .cols=any_of(var.df$var))
#   hydro_R[[i+1]] <- hydro.i_R %>% 
#     select(obs.id, site.id, date, depth, grid, site.elem, starts_with("trinode")) %>%
#     mutate(date=str_remove_all(as.character(date-i), "-")) %>%
#     extractHydroVars(., westcoms.dir, var.df$var, var.df$dayFn, var.df$depthFn,
#                      regional=T, cores=cores, progress=T) %>%
#     rename_with(~paste0(.x, "_R", i), .cols=any_of(var.df$var))
# }
# # summarise lags to weekly averages
# hydro.df <- c(hydro_L, hydro_R) %>%
#   reduce(., full_join) %>% 
#   pivot_longer(c(contains("_L"), contains("_R")), names_to="param", values_to="value") %>%
#   mutate(parType=str_sub(param, 1, -4),
#          res=str_sub(param, -2, -2),
#          lag=str_sub(param, -1, -1)) %>%
#   select(-param) %>%
#   pivot_wider(names_from="parType", values_from="value") %>%
#   mutate(water=sqrt(u^2 + v^2 + ww^2),
#          wind=sqrt(uwind_speed^2 + vwind_speed^2),
#          waterDir=atan2(v, u),
#          windDir=atan2(vwind_speed, uwind_speed),
#          timespan=if_else(lag=="0", "0", "wk")) %>% 
#   select(-u, -v, -ww, -vwind_speed, -uwind_speed) %>%
#   group_by(obs.id, res, timespan) %>%
#   summarise(across(where(is.numeric), ~mean(.x, na.rm=T))) %>%
#   ungroup %>%
#   pivot_wider(names_from=c(res, timespan), values_from=4:ncol(.),
#               names_glue="{.value}_{res}_{timespan}")
# write_csv(hydro.df, glue("data/toDate/hydro_{dates.n$v2_end}.csv"))
# 
# 
# 
# # Copernicus --------------------------------------------------------------
# 
# nLags <- 7
# cprn.df <- load_copernicus(dir(dirs$cprn, "nc$", full.names=T), dates.c) %>% 
#   filter(!is.na(chl)) %>%
#   group_by(date) %>% mutate(id=row_number()) %>% ungroup
# cprn.sf <- cprn.df %>% filter(date==first(date)) %>%
#   st_as_sf(., coords=c("lon", "lat"), crs=4326)
# 
# # match fsa site locations
# fsa.id.sf <- st_as_sf(fsa.id, coords=c("x", "y"), crs=27700)
# fsa.id.cprn <- fsa.id %>%
#   mutate(cprn_id=st_nearest_feature(fsa.id.sf %>% st_transform(4326), cprn.sf))
# cprn.df <- left_join(cprn.df, 
#                     select(fsa.id.cprn, -x, -y), 
#                     by=c("id"="cprn_id"))
# 
# # calculate lags
# cprn.out <- cprn.df %>% 
#   filter(!is.na(site.id)) %>%
#   select(date, lat, lon, grid, id, site.id,
#          chl, attn, no3, o2, diato, dino, ph, po4, nppv, spco2) %>%
#   group_by(id) %>%
#   multijetlag(chl, attn, no3, o2, diato, dino, ph, po4, nppv, spco2, n=nLags) %>%
#   pivot_longer(-(1:7), names_to="var", values_to="val") %>%
#   mutate(timespan=if_else(str_split_fixed(var, "_", 2)[,2]=="", "0", "wk"),
#          var=str_split_fixed(var, "_", 2)[,1]) %>%
#   group_by(date, id, site.id, var, timespan) %>%
#   summarise(across(where(is.numeric), ~mean(.x, na.rm=T))) %>%
#   ungroup %>%
#   pivot_wider(names_from=c(var, timespan), values_from=val,
#               names_glue="{var}_{timespan}")
#   
# write_csv(cprn.out, glue("data/toDate/cprn_{dates.n$v2_end}.csv"))
# 
# 
# 
# # algal densities ---------------------------------------------------------
# 
# species <- c("alexandrium_sp", "dinophysis_sp", "karenia_mikimotoi",
#              "prorocentrum_lima", "pseudo_nitzschia_sp")
# 
# sampling.df <- read_csv(glue("data/toDate/hydro_i_L_{dates.n$v2_end}.csv"))
# 
# thresh.df <- read_csv(glue("data/hab_tf_thresholds.csv")) %>%
#   filter(!is.na(tl)) %>%
#   group_by(hab_parameter, tl) %>%
#   slice_head(n=1) %>%
#   ungroup %>%
#   mutate(bloom=as.numeric(!is.na(alert)))
# 
# # TODO: This is copied and largely unedited
# for(sp in 1:length(species)) {
#   target <- species[sp]
#   target.tf <- thresh.df %>% filter(hab_parameter==target)
#   
#   target.df <- sampling.df %>%
#     filter(grid==1) %>% 
#     rename(N=!!target) %>%
#     select(obs.id, site.id, date, hour, grid, lon, lat, fetch, bearing, N) %>%
#     mutate(yday=yday(date),
#            ydayCos=cos(2*pi*yday/365),
#            ydaySin=sin(2*pi*yday/365),
#            year=year(date),
#            bearing=bearing*pi/180,
#            N=round(N),
#            N.ln=log1p(N),
#            N.PA=as.numeric(N>0)) %>%
#     rowwise() %>%
#     mutate(N.cat=target.tf$tl[max(which(N >= target.tf$min_ge))]) %>%
#     ungroup %>%
#     mutate(N.catF=factor(N.cat, levels=unique(target.tf$tl), ordered=T),
#            N.catNum=as.numeric(N.catF),
#            N.bloom=target.tf$bloom[match(N.cat, target.tf$tl)]) %>%
#     arrange(site.id, date) %>%
#     group_by(site.id) %>%
#     multijetlag(N.ln, N.PA, N.cat, N.catF, N.bloom, date, n=2) %>%
#     ungroup %>%
#     mutate(across(starts_with("date_"), ~as.numeric(date-.x)),
#            # I don't love this since small if N.ln_x is small OR date_x is large
#            N.lnWt_1=N.ln_1/date_1,
#            N.lnWt_2=N.ln_2/date_2) 
#   
#   write_csv(target.df, glue("out/toDate/dataset_{target}.csv"))
# }
# 
# 
# 
# 
# 
# # toxin concentrations ----------------------------------------------------
# 
# 
# 
# 
# 
# 
# 
# 
# # compile -----------------------------------------------------------------
# 
# # TODO: copied from 02_HB_initFit.R
# target.df %>%
#   full_join(hydro.df) %>%
#   full_join(connect.df) %>%
#   mutate(across(contains("Dir_"), ~cos(.x-bearing))) %>%
#   mutate(across(one_of(grep("Dir", covars, invert=T, value=T)), CenterScale)) %>%
#   arrange(site.id, date) %>%
#   filter(complete.cases(.)) %>%
#   select(site.id, lon, lat, date, year, obs.id, fetch, bearing,
#          starts_with("N"), starts_with("date_"), starts_with("yday"),
#          one_of(covars))
