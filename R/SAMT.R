#' Taxa list
#'
#' This function provides the marine macroinvertebrates list from soft sediments in South Australia, available in SAMT database.
#' @keywords taxa
#' @param Taxa.list Returns a dataframe with a list of taxa names.
#' @description Taxa_list provides a list of names of marine macroinvertebrates taxa contained in SAMTdatabase.
#' @return A data frame with the Taxa list.
#' @export
#' @examples
#' Taxa.list()
 
Taxa.list<- function (){
		return(data.frame(Taxa.list=colnames(SAMTdatabase)))
		}
utils::globalVariables(c("SAMTdatabase", "Traits"))

#' Traits list
#'
#' This function provides the list of traits available in SAMT database.
#' @keywords traits
#' @param Traits.list Returns a data frame with names of the traits used in the SAMTdatabase.
#' @param Traits A full list of traits and traits modalities.
#' @description List of traits and trait-modalities used for classifying marine macroinvertebrates taxa of soft sediments in South Australia.
#' @return A data frame with the traits list.
#' @export
#' @examples
#' Traits.list()

Traits.list<- function (){
		return(data.frame(Traits.list=colnames(Traits)))
		}

#' SAMTdb
#'
#' This function provides the fuzzy coding (functional trait calssification) allocated to each marine macroinvertebrate taxa of South Australia.
#' @keywords fuzzy
#' @param SAMTdb A database with information on macrobenthic taxa and their functional trait classification.
#' @description SAMTdb is a databse designed for marine macroinvertebrates taxa in South Australia (values range from 0 to 1).
#' @return A data frame with the taxa list with fuzzy coding or functional trait classification.
#' @export
#' @examples
#' SAMTdb()

SAMTdb<- function (){
		return(data.frame(SAMTdatabase))
		}

#'Traits values by taxa
#'
#' This function multiply the trait values per each taxa by the abundance per each taxa.
#' @keywords TVT
#' @param TVT Name of the function.
#' @param a A database or datamatrix that contains taxa abundace per site/replicate. In all cases, taxa labels are requiered to match with the traits database.
#' @param b SAMTdatabse (recommended), but it could be another database or datamatrix that contains the same number of functional traits by taxa that SAMTdatabase. In all cases, taxa labels are requiered to match with the abundance database.
#' @description TVT calculate trait values at taxa level taking in consideration the taxa abundance (Funtional clasiffication by Taxa abudance).
#' @return A data frame with the Trait Values by each Taxa (TVT).
#' @export
#' @examples
#' TVT(dummy_data,SAMTdatabase)

TVT<-function(a,b){ 
	x<-data.frame(rowSums(a))
	y<-data.frame(b)
	{
	TA<-data.frame((x[,1])*(y[,2:54]))
	print(TA)
	}}

#' Traits Values per Site/Replicate
#'
#' This function calcualte the traits values for specific site / replicate / region / time.
#' @keywords TVSR
#' @param TVSR Name of the function.
#' @param a A database or datamatrix that contains taxa abundace per site/replicate. In all cases, taxa labels are requiered to match with the traits database.
#' @param b SAMTdatabse (recommended), but it could be another database or datamatrix that contains the same number of functional traits by taxa that SAMTdatabase. In all cases, taxa labels are requiered to match with the abundance database.
#' @description TVSR calculate the trait value by site/replicate level (Functional classification * Taxa Abudance(specific location/time)).
#' @return A data frame with the Trait Values for each Site/Replicate//location/time (TVSR).
#' @export
#' @examples
#' TVSR(dummy_data,SAMTdatabase)

TVSR<-function(a,b){  
	x<-data.frame(rowSums(a))
	y<-data.frame(b,row.names=1)	
	{
	f<-data.frame((x[,1])*(y[,2:54]))
	TAS<-data.frame(t(t(as.matrix(f)) %*% as.matrix(y)))
	print(TAS)
	}}
