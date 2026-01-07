#' Lichen data from the Current Vegetation Survey
#'
#' @name lichen
#' @keywords lichen
#' @docType data
#' @description
#' Data were collected between 1993 and 1999 as part of the Lichen Air
#'   Quality surveys on public lands in Oregon and southern Washington.
#'   Observations were obtained from 1-acre (0.4 ha) plots at Current
#'   Vegetation Survey (CVS) sites. Indicator variables denote the presences
#'   and absences of 7 lichen species. Data for each sampled plot include
#'   the topographic variables elevation, aspect, and slope; bioclimatic
#'   predictors including maximum, minimum, daily, and average temperatures,
#'   relative humidity precipitation, evapotranspiration, and vapor
#'   pressure; and vegetation variables including the average age of
#'   the dominant conifer and percent conifer cover. The data in lichenTest
#'   were collected from half-acre plots at CVS sites in the same
#'   geographical region and contains many of the same variables,
#'   including presences and absences for the 7 lichen species. As such,
#'   it is a good test dataset for predictive methods applied to the Lichen
#'   Air Quality data.
#' @usage lichen
#' @format
#' A data frame with 840 observations and 40 variables. One variable is
#'   a location identifier, 7 (coded as 0 and 1) identify the presence or
#'   absence of a type of lichen species, and 32 are characteristics of
#'   the survey site where the data were collected.
#'
#'   There were 12 monthly values in the original data for each of the
#'   bioclimatic predictors. Principal components analyses suggested
#'   that for each of these predictors 2 principal components explained
#'   the vast majority (95.0\%-99.5\%) of the total variability. Based on
#'   these analyses, indices were created for each set of bioclimatic
#'   predictors. The variables with the suffix Ave in the variable name
#'   are the average of 12 monthly variables. The variables with the
#'   suffix Diff are contrasts between the sum of the April-September
#'   monthly values and the sum of the October-December and January-March
#'   monthly values, divided by 12. Roughly speaking, these are
#'   summer-to-winter contrasts.
#'
#'   The variables are summarized as follows:
#'
#'   \describe{
#'   \item{LobaOreg}{Lobaria oregana (Absent = 0, Present = 1)}
#'   \item{EvapoTransAve}{Average monthly potential evapotranspiration in mm}
#'   \item{EvapoTransDiff}{Summer-to-winter difference in monthly potential
#'   evapotranspiration in mm}
#'   \item{MoistIndexAve}{Average monthly moisture index in cm}
#'   \item{MoistIndexDiff}{Summer-to-winter difference in monthly monthly
#'   moisture index in cm}
#'   \item{PrecipAve}{Average monthly precipitation in cm}
#'   \item{PrecipDiff}{Summer-to-winter difference in monthly precipitation
#'   in cm}
#'   \item{RelHumidAve}{Average monthly relative humidity in percent}
#'   \item{RelHumidDiff}{Summer-to-winter difference in monthly relative
#'   humidity in percent}
#'   \item{PotGlobRadAve}{Average monthly potential global radiation in kJ}
#'   \item{PotGlobRadDiff}{Summer-to-winter difference in monthly potential
#'   global radiation in kJ}
#'   \item{AveTempAve}{Average monthly average temperature in degrees Celsius}
#'   \item{AveTempDiff}{Summer-to-winter difference in monthly average
#'   temperature in degrees Celsius}
#'   \item{MaxTempAve}{Average monthly maximum temperature in degrees Celsius}
#'   \item{MaxTempDiff}{Summer-to-winter difference in monthly maximum
#'   temperature in degrees Celsius}
#'   \item{MinTempAve}{Average monthly minimum temperature in degrees Celsius}
#'   \item{MinTempDiff}{Summer-to-winter difference in monthly minimum
#'   temperature in degrees Celsius}
#'   \item{DayTempAve}{Mean average daytime temperature in degrees Celsius}
#'   \item{DayTempDiff}{Summer-to-winter difference in average daytime
#'   temperature in degrees Celsius}
#'   \item{AmbVapPressAve}{Average monthly average ambient vapor pressure in Pa}
#'   \item{AmbVapPressDiff}{Summer-to-winter difference in monthly average
#'   ambient vapor pressure in Pa}
#'   \item{SatVapPressAve}{Average monthly average saturated vapor pressure
#'   in Pa}
#'   \item{SatVapPressDiff}{Summer-to-winter difference in monthly average
#'   saturated vapor pressure in Pa}
#'   \item{Aspect}{Aspect in degrees}
#'   \item{TransAspect}{Transformed Aspect: TransAspect=(1-cos(Aspect))/2}
#'   \item{Elevation}{Elevation in meters}
#'   \item{Slope}{Percent slope}
#'   \item{ReserveStatus}{Reserve Status (Reserve, Matrix)}
#'   \item{StandAgeClass}{Stand Age Class (< 80 years, 80+ years)}
#'   \item{ACONIF}{Average age of the dominant conifer in years}
#'   \item{PctVegCov}{Percent vegetation cover}
#'   \item{PctConifCov}{Percent conifer cover}
#'   \item{PctBroadLeafCov}{Percent broadleaf cover}
#'   \item{TreeBiomass}{Live tree (> 1inch DBH) biomass, above ground,
#'   dry weight}
#'   }
#' @source
#' Cutler, D. Richard., Thomas C. Edwards Jr., Karen H. Beard, Adele Cutler,
#'   Kyle T. Hess, Jacob Gibson, and Joshua J. Lawler. 2007. Random Forests
#'   for Classification in Ecology. Ecology 88(11): 2783-2792.
#'
#'   https://CRAN.R-project.org/package=EZtune/
"lichen"

#' Concrete Compressive Strength Data Set
#'
#' @name concrete
#' @keywords concrete
#' @docType data
#' @description
#' Concrete strength is very important in civil engineering and is a highly
#'   nonlinear function of age and ingredients. This dataset contains 1030
#'   instances and there are 8 features relevant to the response: concrete
#'   strength. The description of the varaibles are given below.
#'
#'   Name – Data Type – Measurement
#' @usage concrete
#' @format
#' A data frame with 1030 rows, 8 covariate variables, and 1 response variable.
#' @details
#' Cement – quantitative – kg in a m3 mixture
#'
#'   Slag (Blast Furnace Slag) – quantitative – kg in a m3 mixture
#'
#'   FlyAsh – quantitative – kg in a m3 mixture
#'
#'   Water – quantitative – kg in a m3 mixture
#'
#'   Superplast (Superplasticizer) – quantitative – kg in a m3 mixture
#'
#'   CoarseAgg (Coarse Aggregate) – quantitative – kg in a m3 mixture
#'
#'   FineAgg (Fine Aggregate) – quantitative – kg in a m3 mixture
#'
#'   Age – quantitative – Day (1~365)
#'
#'   Strength (Concrete compressive strength) – quantitative – MPa
#' @source
#' \url{https://archive.ics.uci.edu/ml/datasets/Concrete+Compressive+Strength}
#' @references
#' -Cheng Yeh, "Modeling of strength of high performance concrete using
#'   artificial neural networks," Cement and Concrete Research, Vol. 28,
#'   No. 12, pp. 1797-1808 (1998).
"concrete"

#' The MNIST Digit Dataset
#'
#' @name mnist
#' @keywords mnist digit
#' @docType data
#' @description
#' This dataset is made up of 1797 8x8 images. Each image is of a
#'   hand-written digit. In order to utilize an 8x8 figure like this,
#'   we have transformed it into a feature vector with length 64.
#' @usage mnist
#' @format
#' A data frame with 1797 rows and 65 variables.
#' @source
#' \url{https://scikit-learn.org/stable/auto_examples/datasets/plot_digits_last_image.html}
#' \url{https://archive.ics.uci.edu/dataset/81/pen+based+recognition+of+handwritten+digits}
"mnist"

#' The MNIST Digit Dataset pivoted longer
#'
#' @name long_mnist
#' @keywords mnist digit
#' @docType data
#' @description
#' This dataset comes from 1797 8x8 images. Each image is of a
#'   hand-written digit. In order to utilize an 8x8 figure like this,
#'   we first transformed it into a feature vector with length 64. We then
#'   pivoted the data into 5 columns: the digit label (y), pixel
#'   identity (name), pivel value (value), pixel horizontal location (x1),
#'   pixel vertical location (x2).
#' @usage long_mnist
#' @format
#' A data frame with 115008 rows and 5 variables.
#' @source
#' \url{https://scikit-learn.org/stable/auto_examples/datasets/plot_digits_last_image.html}
#' \url{https://archive.ics.uci.edu/dataset/81/pen+based+recognition+of+handwritten+digits}
"long_mnist"
