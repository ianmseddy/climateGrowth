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
    "PredictiveEcology/reproducible@AI (>= 2.1.2.9063)", 
    "PredictiveEcology/SpaDES.core@box (>= 2.1.8.9002)", 
    "terra" # "leaflet", "tidyterra",
  ), # for StudyArea visualization below
  require = c("reproducible", "usethis"),
  useGit = "ianmseddy@gmail.com",
  times = list(start = 2011, end = 2012),
  options = list(# gargle_oauth_email = "predictiveecology@gmail.com",
    # gargle_oauth_cache = ".secret",
    # gargle_oauth_client_type = "web", # for command line
    "~/googledriveAuthentication.R", # has the above lines; each user can create their own file
    spades.allowInitDuringSimInit = FALSE, # TRUE
    # reproducible.gdalwarp = TRUE,
    repos = unique(c("predictiveecology.r-universe.dev", 'https://dmlc.r-universe.dev', getOption("repos"))),
    spades.moduleCodeChecks = FALSE,
    Require.cloneFrom = Sys.getenv("R_LIBS_USER"),
    reproducible.inputPaths = "~/data"
  ),
  studyAreaPSP = {
    saPSP <- reproducible::prepInputs(url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/province/ecoprovince_shp.zip", 
                                      destinationPath = paths$inputPath, 
                                      fun = "terra::vect")
    saPSP <- saPSP[saPSP$ECOPROVINC == "6.2",]
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
      minTrees = 15,
      minDBH = 5,
      PSPperiod = c(1920, 2020), 
      climateVariables = c("ATA" = "MAT", "ACMI" = "CMI", "CMI_sm", "CMI_sp", "AMAP" = "MAP", "PPT_sm", "PPT_sp", "Tave_sp", "Tave_sm")
    )
  )
)

Require::Require("gpboost", repos="https://cran.r-project.org")  # needed for the functions 

out <- SpaDES.core::simInitAndSpades2(inSim)


