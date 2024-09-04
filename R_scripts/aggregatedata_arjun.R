aggregatedata_arjun <- function (path = NA, bins = 9, f0_bins = 1, n_formants = NA,
                                  method = "median", encoding = "UTF-8",
                                  write = FALSE) {
  
  # Autofill parameters
  if (is.na(path)) path = getwd()
  
  if (f0_bins == "same") f0_bins = bins
  
  files = list.files(paste0(path, "/csvs"), full.names = TRUE)
  s_files = tools::file_path_sans_ext(list.files(paste0(path, "/csvs")))
  file_names = list.files(paste0(path, "/csvs"))
  file_names = substr(file_names, 1, nchar(file_names) - 4)
  
  n_files = length(files)
  start = Sys.time()
  csvs = lapply(1:n_files, function(i) {
    tmp = utils::read.csv(files[i], na.strings = c("", "NA", "0"))
    tmp$file = file_names[i]
    
    # Check for missing required columns
    required_cols <- c("time", "f1", "f2", "f3", "intensity")
    missing_cols <- setdiff(required_cols, colnames(tmp))
    if (length(missing_cols) > 0) {
      warning(paste("File", files[i], "is missing columns:", paste(missing_cols, collapse = ", ")))
      return(NULL) # Skip this file
    }
    
    # Handle missing values (simple imputation or other strategies)
    tmp <- tmp %>% 
      mutate(across(c("f1", "f2", "f3", "intensity"), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
    
    # Remove rows with any remaining NA values
    tmp <- na.omit(tmp)
    
    # If the file ends up empty after removing NAs, skip it
    if (nrow(tmp) == 0) {
      warning(paste("File", files[i], "has no valid data after processing. Skipping."))
      return(NULL)
    }
    
    tmp
  })
  
  # Filter out NULL elements (files that were skipped)
  csvs <- csvs[!sapply(csvs, is.null)]
  
  # Combine all files into one data frame
  csvs = do.call(rbind, csvs)
  
  # Ensure intensity column is present
  if (!("intensity" %in% colnames(csvs))) {
    stop("The 'intensity' column is missing in the data.")
  }
  
  # How many formants to process?
  if (is.na(n_formants)) {
    n_formants = 4
    if (length(which(colnames(csvs) == "f4")) == 0) n_formants = 3
  }
  
  # What method should be used?
  if (method == "mean") method = mean
  
  # Split csvs and get filenames
  tmp_csvs = split(csvs, csvs$file)
  files = paste0(names(tmp_csvs), ".wav")
  
  f0present = FALSE
  if ("f0" %in% colnames(csvs)) f0present = TRUE
  
  # Internal function to quickly calculate duration and bins
  tmp_csvs = lapply(tmp_csvs, function(x) {
    tmp_time = x$time
    tmp_time = tmp_time - min(tmp_time)
    x$dur = max(tmp_time)
    tmp_time = tmp_time / max(tmp_time)
    tmp_time = 1 + floor(tmp_time * (bins - .001))
    x$bin = tmp_time
    x
  })
  # Rejoining data
  tmp_csvs = do.call(rbind, tmp_csvs)
  
  if (n_formants == 3) {
    tmp_agg = stats::aggregate(cbind(f1, f2, f3) ~ bin + file, tmp_csvs, method)
    aggregated = c(t(tmp_agg[, 3:5]))
  }
  if (n_formants == 4) {
    tmp_agg = stats::aggregate(cbind(f1, f2, f3, f4) ~ bin + file, tmp_csvs, method)
    aggregated = c(t(tmp_agg[, 3:6]))
  }
  
  # Debugging: Print lengths and expected dimensions
  print(length(aggregated))
  print(length(files))
  print(bins)
  print(n_formants)
  
  # Check if the length matches the expected size
  expected_length = length(files) * bins * n_formants
  if (length(aggregated) != expected_length) {
    warning("Length of aggregated data does not match the expected length. Adjusting by adding NAs.")
    missing_length = expected_length - length(aggregated)
    aggregated = c(aggregated, rep(NA, missing_length))
  }
  
  # Aggregated bins into matrix for output
  aggregated = data.frame(matrix(aggregated, length(files), bins * n_formants, byrow = TRUE))
  colnames(aggregated) = paste0("f", rep(1:n_formants, bins), if (bins > 1) rep(1:bins, each = n_formants))
  
  # Find duration
  duration = tapply(tmp_csvs$dur, tmp_csvs$file, max)
  
  # Put parts together
  aggregated = cbind(file = files, duration, aggregated)
  
  # Calculation of f0 and intensity (intensity is mandatory now)
  if ((f0_bins) == 1 & f0present) {
    f0 = stats::aggregate(f0 ~ file, tmp_csvs, FUN = method, na.rm = TRUE, na.action = stats::na.pass)
    intensity = stats::aggregate(intensity ~ file, tmp_csvs, FUN = method, na.rm = TRUE, na.action = stats::na.pass)
    aggregated = cbind(aggregated, f0 = f0$f0, intensity = intensity$intensity)
  }
  
  if ((f0_bins) > 1 & f0present) {
    
    # Split csvs and get filenames
    tmp_csvs = split(csvs, csvs$file)
    files = names(tmp_csvs)
    
    # Internal function to quickly calculate duration and bins
    tmp_csvs = lapply(tmp_csvs, function(x) {
      tmp_time = x$time
      tmp_time = tmp_time - min(tmp_time)
      x$dur = max(tmp_time)
      tmp_time = tmp_time / max(tmp_time)
      tmp_time = 1 + floor(tmp_time * (f0_bins - .001))
      x$bin = tmp_time
      x
    })
    # Rejoining data
    tmp_csvs = do.call(rbind, tmp_csvs)
    
    f0 = stats::aggregate(f0 ~ bin + file, tmp_csvs, FUN = method, na.rm = TRUE, na.action = stats::na.pass)
    f0 = data.frame(matrix(f0$f0, length(files), f0_bins, byrow = TRUE))
    colnames(f0) = paste0("f0", if (f0_bins > 1) 1:f0_bins) # Only add the number if >1 bins
    
    intensity = stats::aggregate(intensity ~ bin + file, tmp_csvs, FUN = method, na.rm = TRUE, na.action = stats::na.pass)
    intensity = data.frame(matrix(intensity$intensity, length(files), f0_bins, byrow = TRUE))
    colnames(intensity) = paste0("dB", if (f0_bins > 1) 1:f0_bins) # Only add the number if >1 bins
    
    aggregated = cbind(aggregated, f0, intensity)
  }
  
  rownames(aggregated) = 1:nrow(aggregated)
  
  if (write) {
    dir.create(paste0(path, "/processed_data"), showWarnings = FALSE)
    utils::write.csv(aggregated, paste0(path, "/processed_data/aggregated_data_TEST.csv"))
  }
  
  aggregated
}

