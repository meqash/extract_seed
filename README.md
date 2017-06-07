#extract_seed

#this code extracts miniseed to SAC format data

#any comments or modification are really appreciated

#feel free to contact Jun Xie through junxie01@gmail.com

#executable file:"extract_seed_short" in directory "extract/src/"

#source file    :"extract_seed_short.f90", comple with make. This code can be used to extract both seed and mseed file.

#With a little modifcation to extract earthquake data (e.g., extract_eq.f90)

#how to run     :a parameter file is needed with the form

#"station.list

#year_begin day_begin year_end day_end
#                              seed_type(0 for mseed, 1 for seed) length_of_SAC_file_in_seconds overlaping_in_percent number_of_component component
#                              corner_frequency_f1 f2 remove_response_or_not(1/0) do_decimate_or_not(1/0)
#                              Ouput_directory"
#                              The station.list is the same as previous one
#                              See the example: for_extract
