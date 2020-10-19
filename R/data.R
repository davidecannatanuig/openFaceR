### Data Description ###

#' test faces
#' @description openfacer provides 4 test OpenFace datasets: John, Teddy, Lara and Paula.
#' Together, they are collected in the "faces" object "test_faces"
#'
#' @format Each of the four example face datasets in test_faces is composed by 538 variables.
#' Lara has 2203 rows, teddy 3281, paula and john 5367. Each row in each of the examples contains information
#' relative to one single frame, There are 30 frames per second.
#' for more information visit: https://github.com/TadasBaltrusaitis/OpenFace/wiki/Output-Format
#' \describe{
#'   \item{frame}{the order of the frame analysed}
#'   \item{face_id}{The identifier number of the face, as there is one face per video it is always 0}
#'   \item{timestamp}{the time of the frame in the video, in seconds}
#'   \item{confidence}{The confidence of OpenFace of providing high quality results (from 0 to 1)}
#'   \item{success}{1 if the confidence is high enough for results being reliable, 0 if it is not}
#'   \item{gaze_0_x:gaze_0_z}{The coordinates of the gaze of the left eye}
#'   \item{gaze_1_x:gaze_1_z}{The coordinates of the gaze of the right eye}
#'   \item{gaze_angle_x, gaze_angle_y}{eye gaze direction in radians}
#'   \item{eye_lmk_x_0:eye_lmk_y_55}{two dimensional coordinates of the eyes in pixels}
#'   \item{eye_lmk_X_0:eye_lmk_Z_55}{three dimensional coordinates of the eyes in millimeters}
#'   \item{pose_Tx:pose_Tz}{The location of the head in respect of the camera in millimeters}
#'   \item{pose_Rx, pose_Ry, pose_Rz}{pitch, yaw and roll of the head in radiants}
#'   \item{X_0:Z_67}{coordinates of the 67 3D face landmarks in millimeters}
#'   \item{AU01_r:AU45_r}{action units intensity, as a number from 0 (inactive) to 1 (completely active)}
#'   \item{AU01_r:AU45_r}{action units presence (1 is present, 0 is not present)}
#'
#' @source the four example are made of data collected on mechanical turk in a prestudy made by the research group.
#' The names have been randomly assigned by the researchers and do not refer to real people.
#' @name test_faces

NULL

#' @rdname test_faces
"john"

#' @rdname test_faces
"paula"

#' @rdname test_faces
"lara"

#' @rdname test_faces
"teddy"

#' @rdname test_faces
"test_faces"
