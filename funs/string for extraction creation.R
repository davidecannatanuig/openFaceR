#  Creation of string for the control panel


# TODO command to  preparation to do https://blog.rsquaredacademy.com/command-line-basics-for-r-users/

#' get command
#' provides two strings to consecutively use n the control pane to get videos features extracted into csv format
#' @param: of_dir: the folder where openface executable is stored
#' @param: input_dir: the folder where to save csv files
#' @param filename: file name to provide if the goal is to only analyse one video
#' @output: two strings to copy paste into the command line

 get_commands <- function(of_dir, input_dir, output_dir, filename = NULL){
   
   print("Please, open yor command prompt and input, in order, the following strings:")
   print("1:")
   
   string1 <- paste("cd ", of_dir, "\n", sep = "") 
   cat(string1)
  
   print("2:")
   
   if(is.null(filename)){
   
     string2 <- paste("for /F %i in ('dir /b \"", input_dir, "\"\') do FeatureExtraction.exe  -inroot \"", input_dir, 
                    "\" -f %i  -3Dfp -pose -aus -gaze -out_dir \"", output_dir, "\"", sep = "")
     
   } else {
     string2 <- paste("FeatureExtraction.exe -inroot \"", input_dir, "\" -f \"", filename, 
                      "\" -3Dfp -pose -aus -gaze -out_dir \"", output_dir, "\"", sep = "")
   }
   cat(string2)
   
 }
 
 ### ex: get_commands("C/programs/openface/openface.exe", "myvideos/videoex", "myoutput")
##TODO would be better to integrate with the command prompt and do extraction directly.
