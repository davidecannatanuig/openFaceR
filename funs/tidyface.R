## tidyface (consolidation)

# I am thinking about to remake the rest of the functions so that they provide not the final result (e.g. frequency), 
# but the intermediate one, and add to the faces object. In this way preprocessing can be done as it follows:
# a. function is applied to all the elements of the df
# b. a select_faces is used for selecting which columns to process
# c. statistics and frequencies are authomatically chosen for summarisation after 


#Example:

#read_faces(dir) %>%
#mutate_face(velocity = velocity(), mei = mei()) %>%
#select_face(AU_12, gaze_y, velocity, mei) %>%
#tidy_face(mean = T, sd = T, median = F)


