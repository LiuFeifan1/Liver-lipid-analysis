# Data: liverlipidome, lipidomeR package
# Source: https://doi.org/10.21228/M8MW26

data("liverlipidome", package = "lipidomeR")
liver <- liverlipidome

# Inspect the dataset
print(dim(liver))
print(names(liver))
print(head(liver))

# Count samples rather than lipid measurements
samples <- unique(liver[c("ID", "Diagnosis")])
stopifnot(!anyDuplicated(samples$ID))
print(table(samples$Diagnosis, useNA = "ifany"))

# Select the first lipid alphabetically for this exercise
lipids <- sort(unique(as.character(liver$Lipid_Name)))
selected_lipid <- lipids[1]
print(selected_lipid)

one_lipid <- liver[
  liver$Lipid_Name == selected_lipid,
  c("ID", "Diagnosis", "Lipid_Level")
]

# Check the measurements
stopifnot(!anyDuplicated(one_lipid$ID))
stopifnot(is.numeric(one_lipid$Lipid_Level))
print(colSums(is.na(one_lipid)))

# Summarise each diagnosis group
groups <- split(one_lipid$Lipid_Level, one_lipid$Diagnosis)

summary_table <- do.call(rbind, lapply(names(groups), function(g) {
  x <- groups[[g]]
  observed <- x[!is.na(x)]
  
  data.frame(
    diagnosis = g,
    lipid = selected_lipid,
    n_total = length(x),
    n_measured = length(observed),
    n_missing = sum(is.na(x)),
    median = if (length(observed)) median(observed) else NA_real_,
    iqr = if (length(observed)) IQR(observed) else NA_real_
  )
}))

print(summary_table)

write.csv(
  summary_table,
  "lipid_summary.csv",
  row.names = FALSE
)

# Save a graph
png("lipid_by_diagnosis.png", width = 1400, height = 1000, res = 150)

boxplot(
  Lipid_Level ~ Diagnosis,
  data = one_lipid,
  main = selected_lipid,
  xlab = "Diagnosis",
  ylab = "Lipid level (dataset scale)",
  col = "lightblue"
)

stripchart(
  Lipid_Level ~ Diagnosis,
  data = one_lipid,
  vertical = TRUE,
  method = "stack",
  add = TRUE,
  pch = 16,
  col = "darkblue"
)

dev.off()

writeLines(
  capture.output(sessionInfo()),
  "session_info.txt"
)