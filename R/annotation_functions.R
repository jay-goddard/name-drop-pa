
#' Misc. annotation files for converting gene names and numbers
#'
#' @description
#' A small set of functions I frquently use to convert between gene names,
#' PAO1 numbers and PA14 numbers
#'
#'

# load in annotation files
# (all orginally sources from www.pseudomonas.com)

PAO1_orth <- read.csv('./data/PAO1-PA14_genes-orthologs/Pseudomonas_aeruginosa_PAO1_107_orthologs.csv', stringsAsFactors = FALSE)

PA14_orth <- read.csv('./data/PAO1-PA14_genes-orthologs/Pseudomonas_aeruginosa_UCBPP-PA14_109_orthologs.csv', stringsAsFactors = FALSE)

PA14_ann <- read.csv('./data/PAO1-PA14_genes-orthologs/Pseudomonas_aeruginosa_UCBPP-PA14_109.csv', stringsAsFactors = FALSE, skip = 2)

PAO1_ann <- read.csv('./data/PAO1-PA14_genes-orthologs/Pseudomonas_aeruginosa_PAO1_107.csv', stringsAsFactors = FALSE, skip = 2)

# functions

#' Convert from 3 or 4 letter P. aeruginosa gene name to PAO1 number syntax
#'
#' @param x A 3 or 4 letter gene name
#' @return The corresponding PAO1 number
#' @examples
#' name_to_PAO1('dnaA')
#' @export
name_to_PAO1 <- function(x){
  if(x==''){
    return(x)
  }
  out_num <- PAO1_ann$Locus.Tag[PAO1_ann$Name == x]
  if(identical(out_num, character(0))){
    return(x)
  }
  return(substring(out_num[1],1,6))
}


#' Convert from P. aeruginosa gene PAO1 number to 3 or 4 letter gene name
#'
#' @param x A  PAO1 number
#' @return The corresponding 3 or 4 letter gene name
#' @examples
#' PAO1_to_name('PA0001')
#' @export
PAO1_to_name <- function(x){
  if(x==''){
    return(x)
  }
  out_num <- PAO1_ann$Name[substring(PAO1_ann$Locus.Tag,1,6) == x]
  if(identical(out_num, character(0))){
    return(x)
  }
  if(out_num[1] == ''){
    return(x)
  }
  return(out_num[1])
}

#' Convert from 3 or 4 letter P. aeruginosa gene name to PA14 number syntax
#'
#' @param x A 3 or 4 letter gene name
#' @return The corresponding PA14 number
#' @examples
#' name_to_PA14('dnaA')
#' @export
name_to_PA14 <- function(x){
  if(x==''){
    return(x)
  }
  out_num <- PA14_ann$Locus.Tag[PA14_ann$Name == x]
  if(identical(out_num, character(0))){
    return(x)
  }
  return(substring(out_num[1],1,10))
}

#' Convert from P. aeruginosa gene PA14 number to 3 or 4 letter gene name
#'
#' @param x A PA14 number
#' @return The corresponding 3 or 4 letter gene name
#' @examples
#' PA14_to_name('PA14_00001')
#' PAO1_to_name(PA14_to_PAO1(PA14_to_name('PA14_0001')))
#' @export
PA14_to_name <- function(x){
  if(x==''){
    return(x)
  }
  out_num <- PA14_ann$Name[substring(PA14_ann$Locus.Tag,1,10) == x]
  if(identical(out_num, character(0))){
    return(x)
  }
  if(out_num == ''){
    return(x)
  }
  return(out_num)
}

#' Convert from P. aeruginosa gene PA14 number to PAO1 number
#'
#' @param x A PA14 number
#' @return The homologous PAO1 number
#' @examples
#' PA14_to_PAO1('PA14_00001')
#' @export
PA14_to_PAO1 <- function(x){
  if(x==''){
    return(x)
  }
  out_num <- PA14_orth$Locus.Tag..Hit.[PA14_orth$Locus.Tag..Query. == x & PA14_orth$Strain..Hit. == 'Pseudomonas aeruginosa PAO1 (Reference)']
  if(identical(out_num, character(0))){
    return(x)
  }
  return(out_num[1])
}

#' Convert from P. aeruginosa gene PAO1 number to PA14 number
#'
#' @param x A PAO1 number
#' @return The homologous PA14 number
#' @examples
#' PAO1_to_PA14('PA0001')
#' @export
PAO1_to_PA14 <- function(x){
  if(x==''){
    return(x)
  }
  out_num <- PAO1_orth$Locus.Tag..Hit.[PAO1_orth$Locus.Tag..Query. == x & PAO1_orth$Strain..Hit. == 'Pseudomonas aeruginosa UCBPP-PA14']
  if(identical(out_num, character(0))){
    return(x)
  }
  return(out_num[1])
}
