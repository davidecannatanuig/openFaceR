#' Get commands for OpenFace
#' @description Provides two strings to consecutively use in the control pane to get videos features
#' extracted into csv format
#' @param of_dir The folder where openface executable is stored.
#' @param input_dir The folder where to save csv files.
#' @param filename File name to provide if the goal is to only analyse one video.
#' @return Two strings to copy paste into the command line.
#' @examples
#' get_commands("C/programs/openface/openface.exe", "myvideos/videoex", "myoutput")
#'
#' @export

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

##TODO would be better to integrate with the command prompt and do extraction directly.
