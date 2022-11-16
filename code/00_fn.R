## -----------------------------------------------------------------------------
## HAB forecasts
## Functions
## Tim Szewczyk
## -----------------------------------------------------------------------------




get_os <- function() {
  # https://www.r-bloggers.com/2015/06/identifying-the-os-from-r/
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}


setPartTrackDirs <- function(base.dir="./", 
                             java.dir="/home/sa04ts/particle_tracking/") {
  
}



setPartTrackProperties <- function(
  destinationDirectory="out/",
  datadir="/media/archiver/common/sa01da-work/WeStCOMS2/Archive/",
  datadirPrefix="netcdf_",
  datadirSuffix="",
  datadir2="",
  datadir2Prefix="",
  datadir2Suffix="",
  mesh1="/home/sa04ts/FVCOM_meshes/WeStCOMS2_mesh.nc",
  mesh1Type="FVCOM",
  mesh2="",
  coordRef="OSGB1936",
  location="minch",
  minchVersion=2,
  habitat="",
  suffix="",
  sitefile="../../large_elem_centroids_v2.dat",
  sitefileEnd="../../fsa_sites_v2.dat",
  verboseSetUp="true",
  start_ymd=20190401,
  end_ymd=20220601,
  numberOfDays=0,
  backwards="false",
  checkOpenBoundaries="false",
  readHydroVelocityOnly="false",
  duplicateLastDay="true",
  recordsPerFile1=25,
  dt=3600,
  verticalDynamics="true",
  fixDepth="false",
  maxDepth=10000,
  parallelThreads=4,
  restartParticles="",
  restartParticlesCutoffDays=21,
  releaseScenario=1,
  nparts=1,
  setStartDepth="true",
  startDepth=1,
  seasonalDensityPath="",
  thresh=500,
  endOnArrival="false",
  rk4="true",
  stepsPerStep=30,
  diffusion="true",
  variableDiffusion="true",
  D_h=0.1,
  D_hVert=0.001,
  salinityThreshold=0,
  mortalityRate=0,
  salinityMort="false",
  swimLightLevel="false",
  vertSwimSpeedMean=0,
  vertSwimSpeedStd=0,
  sinkingRateMean=0,
  sinkingRateStd=0,
  viabletime=1,
  maxParticleAge=168,
  viableDegreeDays=-1,
  maxDegreeDays=-1,
  recordPsteps="false",
  splitPsteps="true",
  pstepsInterval=168,
  recordMovement="true",
  recordElemActivity="false",
  recordConnectivity="true",
  connectivityInterval=24,
  recordLocations="true",
  recordArrivals="true"
) {
  params <- c(
    destinationDirectory=destinationDirectory,
    datadir=datadir,
    datadirPrefix=datadirPrefix,
    datadirSuffix=datadirSuffix,
    datadir2=datadir2,
    datadir2Prefix=datadir2Prefix,
    datadir2Suffix=datadir2Suffix,
    mesh1=mesh1,
    mesh1Type=mesh1Type,
    mesh2=mesh2,
    coordRef=coordRef,
    location=location,
    minchVersion=minchVersion,
    habitat=habitat,
    suffix=suffix,
    sitefile=sitefile,
    sitefileEnd=sitefileEnd,
    verboseSetUp=verboseSetUp,
    start_ymd=start_ymd,
    end_ymd=end_ymd,
    numberOfDays=numberOfDays,
    backwards=backwards,
    checkOpenBoundaries=checkOpenBoundaries,
    readHydroVelocityOnly=readHydroVelocityOnly,
    duplicateLastDay=duplicateLastDay,
    recordsPerFile1=recordsPerFile1,
    dt=dt,
    verticalDynamics=verticalDynamics,
    fixDepth=fixDepth,
    maxDepth=maxDepth,
    parallelThreads=parallelThreads,
    restartParticles=restartParticles,
    restartParticlesCutoffDays=restartParticlesCutoffDays,
    releaseScenario=releaseScenario,
    nparts=nparts,
    setStartDepth=setStartDepth,
    startDepth=startDepth,
    seasonalDensityPath=seasonalDensityPath,
    thresh=thresh,
    endOnArrival=endOnArrival,
    rk4=rk4,
    stepsPerStep=stepsPerStep,
    diffusion=diffusion,
    variableDiffusion=variableDiffusion,
    D_h=D_h,
    D_hVert=D_hVert,
    salinityThreshold=salinityThreshold,
    mortalityRate=mortalityRate,
    salinityMort=salinityMort,
    swimLightLevel=swimLightLevel,
    vertSwimSpeedMean=vertSwimSpeedMean,
    vertSwimSpeedStd=vertSwimSpeedStd,
    sinkingRateMean=sinkingRateMean,
    sinkingRateStd=sinkingRateStd,
    viabletime=viabletime,
    maxParticleAge=maxParticleAge,
    viableDegreeDays=viableDegreeDays,
    maxDegreeDays=maxDegreeDays,
    recordPsteps=recordPsteps,
    splitPsteps=splitPsteps,
    pstepsInterval=pstepsInterval,
    recordMovement=recordMovement,
    recordElemActivity=recordElemActivity,
    recordConnectivity=recordConnectivity,
    connectivityInterval=connectivityInterval,
    recordLocations=recordLocations,
    recordArrivals=recordArrivals
  )
  return(paste(names(params), params, sep="=", collapse="\n"))
}



clean_fsa <- function(x, v1_end) {
  x %>%
    as_tibble %>% 
    filter(!is.na(date_collected)) %>%
    filter(karenia_mikimotoi >= 0) %>% # -99 in karenia...?
    mutate(datetime_collected=as_datetime(date_collected)) %>%
    filter(datetime_collected >= "2013-07-20") %>% 
    mutate(date=date(datetime_collected),
           hour=pmin(20, pmax(5, hour(datetime_collected))),
           grid=if_else(date_collected < v1_end, 1, 2),
           site.id=as.numeric(factor(paste(sin, area, site)))) %>%
    rename(obs.id=oid) %>%
    group_by(sin, area, site) %>% 
    mutate(lon=median(easting), lat=median(northing)) %>%
    ungroup %>%
    mutate(depth_recorded=depth %>% 
             str_to_lower() %>% 
             str_remove_all("m| |\\r|\\n|j|<|>|\\+") %>%
             str_replace("su?.face", "0") %>%
             str_remove("\\(0\\)") %>%
             str_replace("bucket", "NA") %>%
             str_replace("30c", ".3") %>%
             str_replace("55", "5.5") %>%
             str_remove("[0-9]-") %>%
             str_replace("-", "NA") %>%
             as.numeric) %>%
    select(-geom, -easting, -northing, -tide, -datetime_collected, -depth)
}




load_copernicus <- function(f, grd.dates,
                            xmin=-Inf, xmax=Inf, ymin=54, ymax=Inf) {
  library(ncdf4); library(tidyverse)
  cop.dims <- nc_open(f[1])$dim
  i <- list(lon=which(cop.dims$longitude$vals >= xmin &
                        cop.dims$longitude$vals <= xmax),
            lat=which(cop.dims$latitude$vals >= ymin &
                        cop.dims$latitude$vals <= ymax))
  cop.names <- map_chr(f, ~names(nc_open(.x)$var))
  
  dim.df <- expand_grid(date=cop.dims$time$vals,
              lat=cop.dims$latitude$vals[i$lat],
              lon=cop.dims$longitude$vals[i$lon]) %>%
    mutate(date=date(as.POSIXct(date, origin=lubridate::origin)),
           grid=case_when(date < grd.dates$v1_start ~ 0,
                          date >= grd.dates$v1_start & date <= grd.dates$v1_end ~ 1,
                          date > grd.dates$v1_end & date <= grd.dates$v2_end ~ 2,
                          date > grd.dates$v2_end ~ 3))
  val.df <- map(f, ~c(ncvar_get(nc_open(.x))[i$lon,i$lat,])) %>% setNames(cop.names) 
  bind_cols(dim.df, val.df) %>% filter(grid != 0)
}
