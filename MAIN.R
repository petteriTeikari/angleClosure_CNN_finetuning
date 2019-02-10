MAIN = function(data_folder = NA) {
  
  # INITIALIZE --------------------------------------------------------------
  
    if (!require("rstudioapi")) install.packages("rstudioapi"); library("rstudioapi")
    if (!require("readxl")) install.packages("readxl"); library("readxl")
    
    # use the default if no a
    if (is.na(data_folder) | !exists("data_folder")) {
      
      full_path = rstudioapi::getActiveDocumentContext()$path
      source(full_path)
      script.dir = strsplit(full_path, split = .Platform$file.sep, fixed=TRUE)[[1]]
      just_the_file = tail(script.dir,1)
      script.dir = gsub(just_the_file, '', full_path)
      # remove the last separator
      if (substr(script.dir, nchar(script.dir), nchar(script.dir)) == '/') {
        script.dir = substr(script.dir, 1, nchar(script.dir)-1)
      } else if (substr(script.dir, nchar(script.dir), nchar(script.dir)) == '/') {
        script.dir = substr(script.dir, 1, nchar(script.dir)-1)
      }
      # i.e. one dir backwards and then in that directory to
      # -> DATA -> RETCAM ASOCT
      data_folder = file.path(script.dir, '..', 'Data_ASOCT', 'RETCAM ASOCT', 
                              fsep = .Platform$file.sep)
    }
    
    data_out_paths = list()
    data_out_paths[['main']] = file.path(script.dir, '..', 'Data_OUT')
    data_out_paths[['SL']] = file.path(script.dir, '..', 'Data_OUT', 'SL', fsep = .Platform$file.sep)
    
    # Create the folder for the DATA_OUT
    if (dir.exists(data_out_paths[['main']]) == FALSE) {
      dir.create(data_out_paths[['main']], showWarnings = TRUE, recursive = FALSE, mode = "0777")
    }
    
    # Create the folder for the DATA_OUT -> SL
    if (dir.exists(data_out_paths[['SL']]) == FALSE) {
      dir.create(data_out_paths[['SL']], showWarnings = TRUE, recursive = FALSE, mode = "0777")
    }
  

  # PROCESS FILES -----------------------------------------------------------

    # Get the directory listing in this folder
    dir_data = get.directory.listings(data_folder, data_out_paths)
    
    # Define labels
    data_key = 'RETCAM'
    labels_list = define.labels.for.machine.learning(xls_data = dir_data[['XLSData']][[data_key]],
                                       subjects = dir_data[['SubjectsFound']][[data_key]],
                                       label_method = 'ASOCT', data_key = data_key)
  
    # Delete the unwanted images and move the files to subfolders
    folder_key = 'SL'
    filter.images.on.disk(labels_list, image_folder = data_out_paths[[folder_key]])
  
}

filter.images.on.disk = function(labels_list, image_folder,
                                 folder_names = c('Open', 'Closed')) {
  
  filenames = list.files(image_folder, pattern = '*.jpg', recursive = FALSE, full.names = FALSE)
  filepaths = list.files(image_folder, pattern = '*.jpg', recursive = FALSE, full.names = TRUE)
  
  # delete these files that did not have determined labels
  not_kept_codes = labels_list$subjects_not_usable
  idx = list()
  for (f in 1 : length(not_kept_codes)) {
    idx = grep(not_kept_codes[f], filenames)
    cat(' ... Removing the file = ', filenames[idx], '\n')
    file.remove(filepaths[idx])
  }
  
  # Double file listing, TODO!
  filenames = list.files(image_folder, pattern = '*.jpg', recursive = FALSE, full.names = FALSE)
  filepaths = list.files(image_folder, pattern = '*.jpg', recursive = FALSE, full.names = TRUE)
  
  # move the file
  labels = c(FALSE, TRUE)
  for (label in 1 : length(folder_names)) {
    
    cat(' ... ... Moving image for the label "', folder_names[label], '"\n')
    files_to_move = filepaths[labels_list$binary_label == labels[label]]
    filenames_to_move = filenames[labels_list$binary_label == labels[label]]
    path_out = file.path(image_folder, folder_names[label])
    
    if (dir.exists(path_out) == FALSE) {
      dir.create(path_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
    }
    
    for (f in 1 : length(files_to_move)) {
      from = files_to_move[f]
      to = file.path(path_out, filenames_to_move[f])
      success = file.rename(from, to)
    }
    
  }
  
  
  
}

define.labels.for.machine.learning = function(xls_data, subjects, label_method, data_key,
                                              expected_values = c('open', 'closed')) {
  
  col_names = colnames(xls_data)
  if (identical(label_method, 'ASOCT')) {
    
    # find matching column indices
    indices = grep(label_method, col_names)
    var_matrix = xls_data[,indices]
  
    # Convert to boolean (open / closed), and throw away other values
    number_of_vectors = dim(var_matrix)[2]
    in_vectors_to_lowercase = list()
    unique_values = list()
    valid_indices = matrix(, nrow = dim(var_matrix)[1], ncol = dim(var_matrix)[2])
    
    for (v in 1 : number_of_vectors) {
      
      valid_indices_vals = list()
      in_vectors_to_lowercase[[v]] = unname(tolower(unlist(var_matrix[,v])))
      unique_values = unlist(c(unique_values, unique(in_to_lowercase)))
      
      # check if the values are expected
      expected_map = c(0, 1)
      approved_idxs = list()
      for (val in 1 : length(expected_values)) {
        found_ind = in_vectors_to_lowercase[[v]] %in% expected_values[val]
        in_vectors_to_lowercase[[v]][found_ind] = as.logical(expected_map[val])
        if (val == 1) {
          valid_indices_vals = found_ind
        } else {
          valid_indices_vals = found_ind | valid_indices_vals
        }
      }
      valid_indices[,v] = valid_indices_vals
      
    }
  }
  
  # Final unique values in dataset
  uniques_final = unique(unique_values)
  cat('Final Unique values = "', uniques_final,'"\n')
  
  # Get rid of the images with indeter labels
  valid_subject_indices = rowSums(valid_indices) == number_of_vectors
  
  # Keep the subject codes
  subjects_kept = subjects[valid_subject_indices]
  subjects_not_usable = subjects[!valid_subject_indices]
  cat('The followinbg subjects had non-expected values in their "data_key", such as "indeter": \n')
  cat(' ... ', subjects_not_usable, '\n')
  
  # And the actual Excel data
  key_vectors_kept = matrix(, nrow = length(subjects_kept), ncol = dim(var_matrix)[2])
  for (v in 1 : number_of_vectors) {
    key_vectors_kept[,v] = as.logical(in_vectors_to_lowercase[[v]][valid_subject_indices])
    
  }
  
  # Up to you, what rule you want to use, e.g. 88 images with 4x4 classes does not make sense
  # Now if half or more of the quadrants are closed, then the eye is defined as closed angle glaucoma
  binary_label = rowSums(key_vectors_kept) >= 2
  
  cat('Number of ALL labels =', length(binary_label), '\n')
  cat('Number of FALSE labels =', length(which(binary_label == FALSE)), '\n')
  cat('Number of TRUE labels =', length(which(binary_label == TRUE)), '\n')
  cat('Problematic dataset, so little images, and this is already an imbalanced dataset, see e.g.', '\n')
  cat('https://machinelearningmastery.com/tactics-to-combat-imbalanced-classes-in-your-machine-learning-dataset/', '\n')
  
  list_out = list(binary_label, valid_subject_indices, subjects_kept, subjects_not_usable)
  names(list_out) = c('binary_label', 'valid_subject_indices', 'subjects_kept', 'subjects_not_usable')
  
  return(list_out)
  
}

get.directory.listings = function(data_folder, data_out_paths,
                                  excel_file = 'R641_14022011_dataclean.xls') {
  
  # get the metadata excel file
  metadata_file_fullpath = file.path(data_folder, excel_file, fsep = .Platform$file.sep)
  xls_data = read_excel(metadata_file_fullpath)
  
  
  # find the subfolders
  dir_names = list.dirs(data_folder, recursive = FALSE, full.names = FALSE)
  dir_paths = list.dirs(data_folder, recursive = FALSE, full.names = TRUE)
  
  # go through the subfolders (e.g. RETCAM and VISANTE)
  subjects_found = list()
  xls_indices = list()
  xls_data_out = list()
  
  for (i in 1 : length(dir_names)) {
    
    subjects_found[[dir_names[i]]] = list.the.subfolder(dir_path = dir_paths[i], 
                                         dir_name = dir_names[i],
                                         data_out_paths,
                                         RETCAM_string_wildcard = 'SL',
                                         RETCAM_integer_wilcard = '1',
                                         VISANTE_string_wildcard = 'SL',
                                         VISANTE_integer_wilcard = '1')    
    
    # get matching data from the excel files
    xls_indices[[dir_names[i]]]  = find.xls.sheet.indices(xls_subjects = xls_data$ID,
                                                          found_subjects = subjects_found[[dir_names[i]]])
    
    # keep only the valid rows
    indices = xls_indices[[dir_names[i]]][['FoundExcelIndices']]
    xls_data_out[[dir_names[i]]] = xls_data[indices,]
  }
  
  list_out = list(subjects_found, xls_data_out)
  names(list_out) = c('SubjectsFound', 'XLSData')
  
  return(list_out)    
  
}

list.the.subfolder = function(dir_path, dir_name = '', data_out_paths,
                              RETCAM_string_wildcard, RETCAM_integer_wilcard,
                              VISANTE_string_wildcard, VISANTE_integer_wilcard) {
  
  cat(paste0('\nGoing through the subjects in folder "', dir_name, '"\n'))
  dir_names_in_subfolder = list.dirs(dir_path, recursive = FALSE, full.names = FALSE)
  dir_paths_in_subfolder = list.dirs(dir_path, recursive = FALSE, full.names = TRUE)
  cat(paste0('  ... number of folders (subjects) found = ', length(dir_names_in_subfolder), '\n'))
  
  if (identical(dir_name, 'RETCAM')) {
    subj_found = lapply(dir_paths_in_subfolder, function(filepath){
      process.RETCAM.folder(filepath, data_out_paths,
                            string_wildcard = RETCAM_string_wildcard,
                            integer_wilcard = RETCAM_integer_wilcard)})
    
  } else {
    subj_found = lapply(dir_paths_in_subfolder, function(filepath){
      process.VISANTE.folder(filepath, data_out_paths,
                             string_wildcard = VISANTE_string_wildcard,
                             integer_wilcard = VISANTE_integer_wilcard)
    })
  }
  
  subj_found = unlist(subj_found)
  non_na_indices = !is.na(subj_found)
  subj_found = subj_found[non_na_indices] # get rid of NAs
  
  return(subj_found)
  
}

process.VISANTE.folder = function(filepath, data_out_paths,
                                   string_wildcard = 'SL',
                                   integer_wilcard = '1') {
  
  # filepath = dir_paths_in_subfolder[1]
  subject_code_raw = tail(strsplit(filepath, split = .Platform$file.sep)[[1]],1) # e.g. ER001
  subject_code = substr(subject_code_raw, 1, 5) # assume that your first 5 characters are all that you need
  
  # List the images in this folder
  filenames = list.files(filepath, recursive = FALSE, full.names = FALSE)
  filepaths = list.files(filepath, recursive = FALSE, full.names = TRUE)
  
  # TODO!
  # Placeholder
  cat(' ... Nothing done at the moment for individual VISANTE subject\n')
  
}

process.RETCAM.folder = function(filepath, data_out_paths,
                                       string_wildcard = 'SL',
                                       integer_wilcard = '1') {
  
  # filepath = dir_paths_in_subfolder[1]
  subject_code_raw = tail(strsplit(filepath, split = .Platform$file.sep)[[1]],1) # e.g. ER001
  subject_code = substr(subject_code_raw, 1, 5) # assume that your first 5 characters are all that you need
  
  # List the images in this folder
  filenames = list.files(filepath, recursive = FALSE, full.names = FALSE)
  filepaths = list.files(filepath, recursive = FALSE, full.names = TRUE)
  
  # Find the file that you want 
  str_indices = grep(string_wildcard, filenames)
  filenames_left = filenames[str_indices]  
  filepaths_left = filepaths[str_indices]  
  
  # Pick the first pick now
  # TODO! if you want to also check how things change with 2nd best guess
  integer_index = grep(paste0(string_wildcard, '_', integer_wilcard), filenames_left)
  index_from_all_files = str_indices[integer_index]
  
  # the desired filename, and the full path to this file
  filename_final = filenames[index_from_all_files]
  filepath_final = filepaths[index_from_all_files]
  
  if (length(filename_final) == 0) {
    cat('  ... no', string_wildcard, 'file found for subject =',  subject_code, '\n')
    output = NA
  } else {
    cat('  ... found = ', filename_final, '\n')  
    output = subject_code
  }
  
  # copy the matching file to the folder to be used for CNN finetuning experiment
  path_out = data_out_paths[[string_wildcard]]
  from = filepath_final
  to = file.path(path_out, filename_final)
  copy_was_succesful = file.copy(from, to)
  
  return(output)
  
}

find.xls.sheet.indices = function(xls_subjects, found_subjects) {
  
  cat('XLS sheet had', length(xls_subjects), 'subjects', '\n')
  cat(' In the folders, we found', length(found_subjects), 'subjects', '\n')
  
  if (length(found_subjects) == 0) {
   
    list_out = list(NA, NA)
     
  } else {
  
    indices_found = list()
    not_found_from_excel = list()
    
    for (i in 1 : length(found_subjects)) {
      indx = grep(found_subjects[i], xls_subjects)
      if (length(indx) == 0) {
        not_found_from_excel[i] = found_subjects
      }
      # cat(indx, '\n')
      indices_found[i] = indx
    }
    
    list_out = list(unlist(indices_found), unlist(not_found_from_excel))
    names(list_out) = c('FoundExcelIndices', 'NotFoundSubjects')
    
  }
    
  return(list_out)
  
}