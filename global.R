repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
getOrUpdatePkg(c("Require", "SpaDES.project"), c("1.0.1.9003", "0.1.1.9037")) # only install/update if required

Require::Require("usethis")
# generic absolute path for anybody; but individual can change
projectDir <- getwd()
dir.create(projectDir, recursive = TRUE, showWarnings = FALSE)
setwd(projectDir)

inSim <- SpaDES.project::setupProject(
  modules = c("ianmseddy/gmcsDataPrep@development"),
  packages = c(
    "PredictiveEcology/reproducible@AI (>= 2.1.2.9072)", 
    "PredictiveEcology/SpaDES.core@box (>= 2.1.8.9002)", 
    "terra" # "leaflet", "tidyterra",
  ), # for StudyArea visualization below
  require = c("reproducible", "usethis"),
  useGit = "ianmseddy",
  times = list(start = 2011, end = 2012),
  options = list(# gargle_oauth_email = "predictiveecology@gmail.com",
    "~/googledriveAuthentication.R", # has the above lines; each user can create their own file
    # spades.allowInitDuringSimInit = FALSE, 
    repos = unique(c("predictiveecology.r-universe.dev", 'https://dmlc.r-universe.dev', getOption("repos"))),
    spades.moduleCodeChecks = FALSE,
    Require.cloneFrom = Sys.getenv("R_LIBS_USER"),
    reproducible.inputPaths = "~/data"
  ),
  studyArea = LandR::randomStudyArea(size = 1e8), #I just want the CRS
  studyAreaPSP = {
    saPSP <- reproducible::prepInputs(url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip", 
                                      destinationPath = paths$inputPath, 
                                      fun = "terra::vect", 
                                      projectTo = studyArea)
    saPSP <- saPSP[saPSP$ZONE_NAME == "Montane cordillera",] #6.2 Quebec and Ontario
  },
  sppEquiv = {
    species <- LandR::speciesInStudyArea(studyArea = studyAreaPSP, dPath = paths$inputPath)
    spp <- grep("_Spp", species$speciesList, invert = TRUE, value = TRUE)
    column <- LandR::equivalentNameColumn(spp, LandR::sppEquivalencies_CA)
    sppEquiv <- LandR::sppEquivalencies_CA[which(LandR::sppEquivalencies_CA[[column]] %in% spp),]
    sppEquiv[LANDIS_traits != "",]
  },
  params = list(
    .globals = list(
      dataYear = 2020,
      .plots = "png",
      sppEquivCol = "LandR", # will get a warning if this is not here
      .useCache = c(".inputObjects"),
      minCoverThreshold = 0), 
    gmcsDataPrep = list(
      minTrees = 12,
      minMeasures = 2,
      minDBH = 7, #7 for BC
      PSPperiod = c(1920, 2020), 
      climateVariables = c("ATA" = "MAT", "ACMI" = "CMI", "ACMI_sm" = "CMI_sm", "CMI_sp", 
                           "AMAP" = "MAP", "PPT_sm", "PPT_sp", "ADD_0" = "DD_0", "ADD1040" = "DD1040")
      #named vector indicates "anomaly" - both are added to gpboost
    )
  )
)
# 
# Require::Require("gpboost", repos="https://cran.r-project.org")  # needed for the functions 

out <- SpaDES.core::simInitAndSpades2(inSim)


# compare with glmmPQL
# compare 

