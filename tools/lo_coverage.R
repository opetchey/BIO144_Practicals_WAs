# Map the BIO144 learning objectives to the practicals and weekly quizzes.
#
# Tags in the Unit*.Rmd files (invisible to students):
#   - first line inside a quiz chunk:      # LO: LO4.1.9, LO4.1.8
#   - line after a practical heading:      <!-- LO: LO5.1.3, LO2.1.5 -->
#   "(video only)" in a heading tag marks objectives covered only by a video;
#   "none (...)" marks an item that serves no learning objective.
#
# Run from the root of BIO144_Practicals_WAs (e.g. open uzhbio144.Rproj, then
# source("tools/lo_coverage.R")). Expects the course information repository next to this one.
# Writes tools/lo_coverage.csv (one row per objective) and tools/lo_tags.csv (one row per tag).

lo_dir <- "../BIO144_Course_Information/learning_objectives"
read_utf8 <- function(f) { con <- file(f, encoding = "UTF-8"); on.exit(close(con)); readLines(con, warn = FALSE) }
unit_files <- list.files(".", pattern = "^Unit[0-9]+\\.Rmd$")

## --- objectives -----------------------------------------------------------
yml <- setdiff(list.files(lo_dir, pattern = "\\.ya?ml$"), "course.yml")
los <- do.call(rbind, lapply(yml, function(f) {
  d <- yaml::yaml.load(paste(read_utf8(file.path(lo_dir, f)), collapse = "\n"))
  data.frame(id = vapply(d$objectives, `[[`, "", "id"),
             chapter = sub("-.*$", "", f),
             type = vapply(d$objectives, function(o) if (is.null(o$type)) NA_character_ else o$type, ""),
             text = vapply(d$objectives, `[[`, "", "text"))
}))
ord <- order(sapply(strsplit(sub("^LO", "", los$id), ".", fixed = TRUE),
                    function(x) sum(as.numeric(x) * c(1e6, 1e3, 1))))
los <- los[ord, ]

## --- tags -----------------------------------------------------------------
tags <- do.call(rbind, lapply(unit_files, function(f) {
  l <- read_utf8(f)
  out <- list()
  for (i in seq_along(l)) {
    if (grepl("^# LO:", l[i]) && i > 1 && grepl("^```\\{r", l[i - 1])) {
      label <- trimws(sub(",.*$", "", sub("^```\\{r\\s*", "", sub("\\}\\s*$", "", l[i - 1]))))
      out[[length(out) + 1]] <- data.frame(unit = sub("\\.Rmd$", "", f), line = i, kind = "quiz",
                                           item = label, tag = sub("^# LO:\\s*", "", l[i]))
    }
    if (grepl("^<!-- LO:", l[i]) && i > 1) {
      tag <- sub("\\s*-->\\s*$", "", sub("^<!-- LO:\\s*", "", l[i]))
      kind <- if (grepl("video only", tag)) "video" else "practical"
      out[[length(out) + 1]] <- data.frame(unit = sub("\\.Rmd$", "", f), line = i, kind = kind,
                                           item = sub("^#+\\s*", "", l[i - 1]), tag = tag)
    }
  }
  do.call(rbind, out)
}))
ids_in <- function(x) regmatches(x, gregexpr("LO[0-9]+\\.[0-9]+\\.[0-9]+", x))[[1]]
tag_long <- do.call(rbind, lapply(seq_len(nrow(tags)), function(i) {
  ids <- ids_in(tags$tag[i])
  if (!length(ids)) return(NULL)
  data.frame(id = ids, unit = tags$unit[i], kind = tags$kind[i], item = tags$item[i])
}))

unknown <- setdiff(tag_long$id, los$id)
if (length(unknown)) warning("Tags refer to objectives that do not exist: ", paste(unknown, collapse = ", "))

## --- coverage table -------------------------------------------------------
count_kind <- function(id, k) sum(tag_long$id == id & tag_long$kind == k)
los$n_practical <- vapply(los$id, count_kind, 0L, k = "practical")
los$n_quiz      <- vapply(los$id, count_kind, 0L, k = "quiz")
los$n_video     <- vapply(los$id, count_kind, 0L, k = "video")
los$where <- vapply(los$id, function(id) {
  x <- tag_long[tag_long$id == id, ]
  paste(unique(paste0(x$unit, ":", x$item)), collapse = " | ")
}, "")
los$status <- ifelse(los$n_practical > 0, "practical",
               ifelse(los$n_quiz > 0, "quiz only",
               ifelse(los$n_video > 0, "video only", "NOT COVERED")))

write.csv(los, "tools/lo_coverage.csv", row.names = FALSE)
write.csv(tags, "tools/lo_tags.csv", row.names = FALSE)

## --- summary --------------------------------------------------------------
cat("Tagged items:", nrow(tags), " (", sum(tags$kind == "quiz"), "quiz,",
    sum(tags$kind == "practical"), "practical,", sum(tags$kind == "video"), "video;",
    sum(grepl("^none", tags$tag)), "marked 'none')\n\n")
print(table(chapter = factor(los$chapter, unique(los$chapter)), los$status))
cat("\nObjectives not covered at all:\n")
print(los[los$status == "NOT COVERED", c("id", "type", "text")], row.names = FALSE, right = FALSE)
cat("\n'analysis' objectives without a practical:\n")
print(los[los$type %in% "analysis" & los$n_practical == 0, c("id", "status")], row.names = FALSE)
