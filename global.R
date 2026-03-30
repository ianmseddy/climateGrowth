repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")

# getOrUpdatePkg(c("Require", "SpaDES.project"), c("1.0.1.9003", "0.1.1.9037")) # only install/update if required
# getOrUpdatePkg(c("reproducible", "SpaDES.core"), c("21.2.99999", "2.1.8.9999"))
# generic absolute path for anybody; but individual can change
projectDir <- getwd()

ecozoneName <- "Montane Cordillera" #for studyAreaPSP
ecodistrictName <- "Bowron_Valley" #for studyArea
ecodistrictNumber <- 986
ecoprovinceNumber <- "14.1"
ecoprovinceName <- "Columbia_Montane"
climateNormal <- 1981:2010 
# climateNormal should correspond to the age of the data used for simulation
ecoprovinceUrl <- "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/province/ecoprovince_shp.zip"
ecodistrictUrl <- "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/ecodistrct/ecodistrct_shp.zip"


#TODO: make function that switches between ecoprovince and ecodistrict - this is annoying
ecoName <- ecoprovinceName
ecoNumber <- ecoprovinceNumber
ecoField <- "ECOPROVINC"
ecoUrl <- ecoprovinceUrl
#these are kept in by LandR::sppInStudyArea but are inconsequential in the Columbia
sppToDrop <- c("Abie_ama", "Tsug_mer", "Alnu_rub", "Lari_lar", "Pice_sit")

#TODO: pass normal 
inSim <- SpaDES.project::setupProject(
  paths = list(inputPath = "inputs", 
               outputPath = "outputs", 
               modulePath = "modules", 
               projectPath = getwd(), 
               cachePath = "cache"),
  packages = c(
    "PredictiveEcology/reproducible", 
    "PredictiveEcology/SpaDES.core", 
    "terra" # "leaflet", "tidyterra",
  ), # for StudyArea visualization below
  require = c("usethis"),
  useGit = "ianmseddy",
  times = list(start = 2020, end = 2050),
  options = list(
    # gargle_oauth_email = "predictiveecology@gmail.com",
    "~/googledriveAuthentication.R", # has the above lines; each user can create their own file
    # spades.allowInitDuringSimInit = FALSE, 
    LandR.CS.debug = TRUE, 
    LandR.CS.logPath = file.path("outputs/LandR.CS", paste(ecoName, ecoNumber, sep = "_")),
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
              "PredictiveEcology/climateYear@development"),
  studyArea = {
    sa <- reproducible::prepInputs(url = ecoUrl, 
                                   destinationPath = paths$inputPath, 
                                   fun = "terra::vect")
    targetCRS <- terra::crs("EPSG:3348")
    # EPSG:3348 (NAD83(CSRS) / Statistics Canada Lambert) are commonly used for large areas of Canada. 
    sa <- sa[sa[[ecoField]][,1] %in% ecoNumber,]  |>
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
    sppEquiv <- sppEquiv[!LandR %in% sppToDrop]
  },
  climateVariablesForGMCS = c("a_MAT" = "MAT", "a_CMI" = "CMI", #strong support for inclusion - e.g Luo and Chen
                              "aMAP = MAP", #weak correlation with other variables, so include (only 0.66 with summer precip.)
                              # "a_MSP" = "MSP", #"a_FFP" = "FFP" #summer precip, frost-free period,
                              # "a_AHM" = "AHM", 
                              "aPPT_sm" = "PPT_sm", #the second precip is redudant
                              "a_DD5" = "DD5", "aDD_0" = "DD_0", #
                              "a_SHM" = "SHM"),
  #dropped summer precip, frost-free period, summer precip (may-sep), and summer moisture index
  #I believe we want to include summer heat moisture
  climateVariables = c(makeClimateVariablesForModule(unname(climateVariablesForGMCS), type= "projected", years = 2020:2100),
                       makeClimateVariablesForModule(unname(climateVariablesForGMCS), type = "historical", 
                                                     years = "1991_2020", yearType = "historical_period")
                       # below was to confirm if things were working for Alex?
                       # , makeClimateVariablesForModule(unname(climateVarsForGMCS), type = "hindcast", years = 2021:2023)
  ), 
  debugGMCS = TRUE,
  cceArgs = list(quote(historicalClimateRasters),#need for normals 
                 quote(gcsModel),
                 quote(mcsModel), 
                 quote(climateVariablesForGMCS),
                 quote(currentClimateRasters)),
  params = list(
    .globals = list(
      dataYear = 2020,
      .plots = "png",
      sppEquivCol = "LandR", # will get a warning if this is not here
      .useCache = c(".inputObjects", "Init"),
      .studyAreaName = paste0(ecoName,ecoNumber, "_2020")
    ), 
    gmcsDataPrep = list(
      minTrees = 15,
      minMeasures = 2,
      QCflag = c(1,2), #J's new param
      PSPdataTypes = c("BC", "AB"), #don't touch the NFI
      minDBH = 9, #7 for BC, 9 for many places. min DBH is all over the place, sometimes 0, sometimes 9
      # depending on jurisdiction, place, and year. 9 is defensible as a cut-off but we lose info about young plots. 
      PSPperiod = c(1950, 2020)
    ), 
    Biomass_core = list(
      growthAndMortalityDrivers = "LandR.CS", 
      gmcsGrowthLimits = c(33,300)
      
    )
  )
)

# pkgload::load_all("../climateData") #alternatively require normalfix branch
pkgload::load_all("pkgs/caret/pkg/caret")
out <- SpaDES.core::simInitAndSpades2(inSim)
