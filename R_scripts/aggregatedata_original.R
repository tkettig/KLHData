

aggregatedata_original <- function (path=NA, bins = 9, f0_bins = 1, n_formants = NA,
                           method = "median", encoding = "UTF-8",
                           write = FALSE){

  # Autofill parameters
  if (is.na(path)) path = getwd()

  if (f0_bins == "same") f0_bins = bins

  files = list.files (paste0(path,"/csvs"),full.names=TRUE)
  s_files = tools::file_path_sans_ext (list.files (paste0(path,"/csvs")))
  file_names = list.files (paste0(path,"/csvs"))
  file_names = substr (file_names,1, nchar(file_names)-4)

  n_files = length(files)
  start = Sys.time()
  csvs = lapply (1:n_files, function(i){
    tmp = utils::read.csv (files[i], na.strings = "0")
    tmp$file = file_names[i]
    tmp
  })
  csvs = do.call (rbind, csvs)

  # How many formants to process?
  if (is.na (n_formants)) {
    n_formants=4
    if (length (which(colnames(csvs)=="f4")) == 0) n_formants = 3
  }

  # What method should be used?
  if (method=="mean") method = mean

  # Split csvs and get filenames
  #tmp_csvs = csvs
  tmp_csvs = split (csvs, csvs$file)
  files = paste0 (names (tmp_csvs), ".wav")

  f0present = FALSE
  if (sum(colnames(csvs)=="f0") > 0) f0present = TRUE
  
  intensitypresent = FALSE
  if (sum(colnames(csvs)=="intensity") > 0) intensitypresent = TRUE

  # internal function to quickly calculate duration and bins
  tmp_csvs = lapply (tmp_csvs, function (x){
    tmp_time = x$time
    tmp_time = tmp_time - min(tmp_time)
    x$dur = max (tmp_time)
    tmp_time = tmp_time / max (tmp_time)
    tmp_time = 1+floor(tmp_time*(bins-.001))
    x$bin = tmp_time
    x
  })
  # rejoining data
  tmp_csvs = do.call (rbind, tmp_csvs)

  if (n_formants==3){
    tmp_agg = stats::aggregate (cbind (f1,f2,f3) ~ bin+file, tmp_csvs, method)
    aggregated = c(t(tmp_agg[,3:5]))
  }
  if (n_formants==4){
    tmp_agg = stats::aggregate (cbind (f1,f2,f3,f4) ~ bin+file, tmp_csvs, method)
    aggregated = c(t(tmp_agg[,3:6]))
  }

  # aggregated bins into matrix for output
  aggregated =  data.frame(matrix (aggregated,length (files),bins*n_formants, byrow = TRUE))
  colnames (aggregated) = paste0 ("f",rep(1:n_formants,bins),if(bins>1)rep(1:bins,each=n_formants))

  # find duration
  duration = tapply (tmp_csvs$dur, tmp_csvs$file, max)

  # put parts together
  aggregated = cbind (file = files, duration, aggregated)

  # calculation of f0 if it applies
  if ((f0_bins) == 1 & f0present & intensitypresent){
    f0 = stats::aggregate (f0 ~ file, tmp_csvs, FUN = method, na.rm = TRUE, na.action = stats::na.pass)
    intensity = stats::aggregate (intensity ~ file, tmp_csvs, FUN = method, na.rm = TRUE, na.action = stats::na.pass)
    aggregated = cbind (aggregated, f0 = f0$f0, intensity = intensity$intensity)
  }
  # calculation of f0 if it applies
  if ((f0_bins) > 1 & f0present & intensitypresent){

    # Split csvs and get filenames
    tmp_csvs = split (csvs, csvs$file)
    files = names (tmp_csvs)

    # internal function to quickly calculate duration and bins
    tmp_csvs = lapply (tmp_csvs, function (x){
      tmp_time = x$time
      tmp_time = tmp_time - min(tmp_time)
      x$dur = max (tmp_time)
      tmp_time = tmp_time / max (tmp_time)
      tmp_time = 1+floor(tmp_time*(f0_bins-.001))
      x$bin = tmp_time
      x
    })
    # rejoining data
    tmp_csvs = do.call (rbind, tmp_csvs)

    f0 = stats::aggregate (f0 ~ bin+file, tmp_csvs, FUN = method, na.rm = TRUE, na.action = stats::na.pass)
    f0 = data.frame (matrix (f0$f0, length(files), f0_bins, byrow = TRUE))
    colnames (f0) = paste0 ("f0",if(f0_bins>1)1:f0_bins) # only add the number if >1 bins

    intensity = stats::aggregate (intensity ~ bin+file, tmp_csvs, FUN = method, na.rm = TRUE, na.action = stats::na.pass)
    intensity = data.frame (matrix (intensity$intensity, length(files), f0_bins, byrow = TRUE))
    colnames (intensity) = paste0 ("dB",if(f0_bins>1)1:f0_bins) # only add the number if >1 bins
    
    aggregated = cbind (aggregated, f0, intensity)
  }

  rownames (aggregated) = 1:nrow(aggregated)

  if (write){
    dir.create(path %+% "/processed_data", showWarnings = FALSE)
    utils::write.csv (aggregated, path %+% "/processed_data/aggregated_data_TEST.csv")
  }

  aggregated
}





