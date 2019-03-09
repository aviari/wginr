# -------------------------------------------------
# $Id: xlx.r 321 2017-08-18 11:10:19Z viari $
#
#' @name xlx
#' @docType package
#' @title eXtended LX library
#' @author Alain Viari
#' @details
#' \tabular{ll}{
#' Package: \tab xlx\cr
#' Type: \tab Package\cr
#' Version: \tab 1.0\cr
#' Date: \tab 2013-12-12\cr
#' License: \tab GPL\cr
#' }
#' @description
#' Extensions to the LX base library.\cr
#' 
#' These utilities are currently subdivised in different subpackages:\cr
#' \subsection{General programming utilities}{
#' tbd
#' }
#'
#' \subsection{Basta format}{
#' tbd
#' }
#'
#' \subsection{Bim format}{
#' tbd
#' }
#'
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("+ attaching ", pkgname)
  lx.register.file.handler(.bgzf.file.handler())
}

.onLoad <- function(libname, pkgname) {
}

