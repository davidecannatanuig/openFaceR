### transform video into string


## preparation to do https://blog.rsquaredacademy.com/command-line-basics-for-r-users/

### this is made out of two parts:
# 1. Creation of string

#TODO add more options

 get_commands <- function(openface, foldername, outputfolder, filename = NULL){
   
   print("Please, open yor command prompt and input, in order, the following strings:")
   print("1:")
   
   string1 <- paste("cd ", openface, "\n", sep = "") 
   cat(string1)
  
   print("2:")
   
   if(is.null(filename)){
   
     string2 <- paste("for /F %i in ('dir /b \"", foldername, "\"\') do FeatureExtraction.exe  -inroot \"", foldername, 
                    "\" -f %i  -3Dfp -pose -aus -gaze -out_dir \"", outputfolder, "\"", sep = "")
     
   } else {
     string2 <- paste("FeatureExtraction.exe -inroot \"", foldername, "\" -f \"", filename, 
                      "\" -3Dfp -pose -aus -gaze -out_dir \"", outputfolder, "\"", sep = "")
   }
   cat(string2)
   
 }
 
 
 ##TODO would be better to integrate with the command prompt and do extraction directly.

get_commands("alpha", "bravo", "charlie", "delta")
