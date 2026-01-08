repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")

# getOrUpdatePkg(c("Require", "SpaDES.project"), c("1.0.1.9003", "0.1.1.9037")) # only install/update if required
# getOrUpdatePkg(c("reproducible", "SpaDES.core"), c("21.2.99999", "2.1.8.9999"))
# generic absolute path for anybody; but individual can change
projectDir <- getwd()

ecozoneName <- "Montane Cordillera" #for studyAreaPSP
ecodistrictName <- "Fraser_Basin" #for studyArea
ecodistrictNumber <- 986
climateVarsForGMCS <- c("a_MAT" = "MAT", "a_MAP" = "MAP", #"a_MSP" = "MSP", #"a_FFP" = "FFP"
                        "a_DD5" = "DD5", "aDD_0" = "DD_0", "a_CMI" = "CMI", "a_AHM" = "AHM")

inSim <- SpaDES.project::setupProject(
  paths = list(inputPath = "inputs", 
               outputPath = "outputs", 
               modulePath = "modules", 
               projectPath = getwd(), 
               cachePath = "cache"),
  packages = c(
    "PredictiveEcology/reproducible@AI (>= 2.1.2.9072)", 
    "PredictiveEcology/SpaDES.core@box (>= 2.1.8.9002)", 
    "terra" # "leaflet", "tidyterra",
  ), # for StudyArea visualization below
  require = c("usethis"),
  useGit = "ianmseddy",
  times = list(start = 2020, end = 2040),
  options = list(# gargle_oauth_email = "predictiveecology@gmail.com",
    "~/googledriveAuthentication.R", # has the above lines; each user can create their own file
    # spades.allowInitDuringSimInit = FALSE, 
    repos = unique(c("predictiveecology.r-universe.dev", 'https://dmlc.r-universe.dev', getOption("repos"))),
    spades.moduleCodeChecks = FALSE,
    Require.cloneFrom = Sys.getenv("R_LIBS_USER"),
    terra.memfrac = 0.1,
    reproducible.inputPaths = "~/data"
  ),
  functions = "R/makeClimateVariablesForModule.R",
  modules = c("ianmseddy/gmcsDataPrep@xgboost", 
              "PredictiveEcology/Biomass_borealDataPrep@development", 
              "PredictiveEcology/Biomass_core@development", 
              "PredictiveEcology/canClimateData@development",
              "PredictiveEcology/climateYear@main"),
  studyArea = {
    sa <- reproducible::prepInputs(url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip", 
                                   destinationPath = paths$inputPath, 
                                   fun = "terra::vect")
    targetCRS <- terra::crs("EPSG:3348")
    # EPSG:3348 (NAD83(CSRS) / Statistics Canada Lambert) are commonly used for large areas of Canada. 
    sa <- sa[sa$ECODISTRIC == ecodistrictNumber,]  |>
      terra::project(targetCRS) 
    sa <- terra::buffer(sa, 5000)
    return(sa)
  },  
  rasterToMatch = {
    rtm <- terra::rast(sa, res = c(250, 250), vals = 1) |>
      reproducible::postProcess(maskTo = sa)
  },
  studyAreaPSP = {
    ecozones <- reproducible::prepInputs(url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                                         destinationPath = paths$inputPath)
    ecozones <- ecozones[ecozones$ZONE_NAME == ecozoneName,]
  },
  sppEquiv = {
    species <- LandR::speciesInStudyArea(studyArea = studyArea, dPath = paths$inputPath, sppEquivCol = "LandR")
    # spp <- grep("_Spp", species$speciesList, invert = TRUE, value = TRUE)
    # column <- LandR::equivalentNameColumn(spp, LandR::sppEquivalencies_CA)
    sppEquiv <- LandR::sppEquivalencies_CA[LandR %in% species$speciesList,]
    sppEquiv[LANDIS_traits != "",] #Popu bal and popu tre are just "aspen"
  },
  climateVariables = c(makeClimateVariablesForModule(unname(climateVarsForGMCS), type= "projected", years = 2020:2050),
                       makeClimateVariablesForModule(unname(climateVarsForGMCS), type = "historical", 
                                                     years = "1951_1980", yearType = "historical_period")
                       # below was to confirm if things were working for Alex?
                       # , makeClimateVariablesForModule(unname(climateVarsForGMCS), type = "hindcast", years = 2021:2023)
                       ), 
  cceArgs = list(quote(projectedClimateRasters), 
                 quote(gcsModel),
                 quote(mcsModel), 
                 quote(climateYear)),
  params = list(
    .globals = list(
      dataYear = 2020,
      .plots = "png",
      sppEquivCol = "LandR", # will get a warning if this is not here
      .useCache = c(".inputObjects"),
      .studyAreaName = paste0(ecodistrictName,ecodistrictNumber, "_2020")
      ), 
    gmcsDataPrep = list(
      minTrees = 15,
      minMeasures = 2,
      minDBH = 9, #7 for BC, 9 for many places. min DBH is all over the place, sometimes 0, sometimes 9
      # depending on jurisdiction, place, and year. 9 is defensible as a cut-off but we lose info about young plots. 
      PSPperiod = c(1950, 2020), 
      climateVariables = climateVarsForGMCS
    ), 
    Biomass_core = list(
      growthAndMortalityDrivers = "LandR.CS"
    )
  )
)

pkgload::load_all("modules/gmcsDataPrep/pkgs/caret/pkg/caret") 
pkgload::load_all("../LandR.CS")
stopifnot(packageVersion("caret") == "7.0.2.9001")

out <- SpaDES.core::simInitAndSpades2(inSim)

#
#TODO:
# once biomass is estimated, we actually stop caring about `newSpeciesName`. In fact we should only use Species at that point. 
# Species will be the new sppEquivCol. This is how it must be. 


# elev <- reproducible::prepInputs(url = 'https://drive.google.com/file/d/14puAtns8oTZDtvWzpQ6_FgK4MbozGZFK/', 
#                    to = inSim$rasterToMatch, destinationPath = "inputs", overwrite = TRUE,
#                    writeTo = paste0(ecodistrictNumber, "_DEM.tif"))
# 
# bb <- climr::get_bb(xyz = elev)
# climr::pre_cache(bbox = bb)
# normalData <- climr::downscale(xyz = elev, db_option = "local", 
#                                vars = unname(climateVarsForGMCS), nthread = 6, 
#                                return_refperiod = TRUE, ensemble_mean = TRUE)
# Sys.time(projectedClimate <- climr::downscale(xyz = elev, db_option = "local", 
#                                      vars = unname(climateVarsForGMCS), 
#                                      nthread = 6,
#                                      gcms = climr::list_gmcs()[4], #the recommended ensemble: [1,4:7, 10:12]
#                                      ssps = "ssp370", max_run = 0, return_refperiod = TRUE, 
# gcm_spp_years = 2020:2050))
Sys.time()#started around 420 pm
