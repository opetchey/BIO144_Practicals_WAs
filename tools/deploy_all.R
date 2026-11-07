## Deploy all BIO144 Unit tutorials (homework, practical, weekly quiz) to RStudio Connect.
## Run from the root of BIO144_Practicals_WAs (open uzhbio144.Rproj first), then:
##   source("tools/deploy_all.R")
## Each Unit is deployed on its own, so one failure does not stop the others.
## A summary is printed at the end.
##
## Why each Unit is copied to its own temporary folder (fix of 2026-11-07):
## for Shiny documents (runtime: shiny_prerendered), rsconnect::deployDoc()
## uploads the whole folder, i.e. all thirteen UnitN.Rmd files. On 6 Nov 2026
## every Unit then showed Unit 0 (the first .Rmd in alphabetical order).
## Each Unit .Rmd needs no other local files (datasets and scripts are read
## from GitHub), so each one is now uploaded alone from a folder of its own.
##
## test_only: TRUE deploys only Unit 1. Check that its page title on Connect
## says "Unit 1", then set it to FALSE and run the script again for all Units.
test_only <- TRUE

library(rsconnect)

## 0. Check for the fs / libuv problem -------------------------------------------
## fs >= 2.0 must be built from source on Connect and needs libuv, which the
## UZH Connect server does not have. Until the admins install libuv1-dev, deploy
## with fs < 2.0 installed locally.
if (packageVersion("fs") >= "2.0.0") {
  stop("Your fs package is version ", packageVersion("fs"),
       ", which fails to build on Connect.\n",
       "Install an older version first, e.g. remotes::install_version(\"fs\", \"1.6.6\"),",
       " restart R, and run this script again.")
}

server  <- "rstudio.mnf.uzh.ch:3939"
account <- "opetch"

## 1. The Units and their existing content on Connect -----------------------------
## guid = the content GUID on Connect (from the URLs on the course information
## Unit pages). Using it updates the existing content, keeping the same URL,
## even where there is no local deployment record (Units 0, 10, 11, 12).
units <- data.frame(
  file = paste0("Unit", 0:12, ".Rmd"),
  name = paste0("unit", 0:12),
  guid = c(
    "6902b39f-65cc-4952-9a24-17cca0fe3ae2",  # Unit 0
    "1913b078-b142-457f-8f08-8d66cc562490",  # Unit 1
    "415e9279-c26a-4756-8570-fb1f9daab65c",  # Unit 2
    "b448ba8b-4d13-4e69-83ed-4250a8bada0c",  # Unit 3
    "d6c3f6eb-c513-44c8-af74-d3db7f4d00a7",  # Unit 4
    "550e6fb0-423a-4087-9bb2-49e9be0a86c7",  # Unit 5
    "63692bfe-bf6e-442f-a216-b676b85bb663",  # Unit 6
    "bfebafca-fa87-4852-9df8-c715bf8439fe",  # Unit 7
    "6c82eb18-dd0f-4fab-a464-b37b710c4c65",  # Unit 8
    "0385940d-cbc4-4c60-9bb7-fb54316459b4",  # Unit 9
    "131990d2-3b5d-40cb-b673-b13e0b6f1a26",  # Unit 10
    "f916093b-9fa2-4dde-977a-3eec00e5c9cf",  # Unit 11
    "fa0a83c4-fec9-4557-9f34-d0f36a41cf62"   # Unit 12
  )
)

## To deploy only some Units, subset here, e.g.:
## units <- units[units$file %in% c("Unit1.Rmd", "Unit8.Rmd"), ]
if (test_only) units <- units[units$file == "Unit1.Rmd", ]

## 2. Deploy -----------------------------------------------------------------------
deploy_unit <- function(file, name, guid) {
  message("\n==================== Deploying ", file, " ====================")
  tryCatch({
    if (!file.exists(file)) stop("file not found: ", file)
    ## A fresh folder containing only this Unit's .Rmd
    bundle_dir <- file.path(tempdir(), paste0("deploy_", name))
    unlink(bundle_dir, recursive = TRUE)
    dir.create(bundle_dir)
    file.copy(file, bundle_dir)
    rsconnect::deployApp(
      appDir = bundle_dir,
      appFiles = file,
      appPrimaryDoc = file,
      appName = name,
      appId = guid,
      server = server,
      account = account,
      forceUpdate = TRUE,
      launch.browser = FALSE
    )
    "OK"
  }, error = function(e) paste("FAILED:", conditionMessage(e)))
}

results <- mapply(deploy_unit, units$file, units$name, units$guid)

## 3. Summary ----------------------------------------------------------------------
message("\n==================== Summary ====================")
print(data.frame(unit = units$file, result = unname(results)), row.names = FALSE)
message("\nNow open each Unit's page on Connect (signed in) and check that its title ",
        "names the right Unit, e.g. \"Unit 1 BIO144 Homework, Practical, and Quiz\".")
if (test_only) message("test_only is TRUE: only Unit 1 was deployed. If it is right, ",
                       "set test_only <- FALSE and run the script again.")
