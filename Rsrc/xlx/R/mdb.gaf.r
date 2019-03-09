# -------------------------------------------------
# $Id: mdb.gaf.r 321 2017-08-18 11:10:19Z viari $
#
# gene association file format
#

# -------------------------------------------------
#' read Gene Association (GAF) file
#' @param pathname pathname of GAF file to read
#' @details
#' GAF 1.0 or 2.0 specifications\cr
#' \tabular{llllll}{
#' \bold{Column} \tab \bold{Content} \tab \bold{Required?} \tab \bold{Cardinality}
#' \tab \bold{Example} \tab \bold{Dataframe_Colname}\cr
#' 1 \tab DB \tab required \tab 1 \tab UniProtKB \tab DB\cr
#' 2 \tab DB Object ID \tab required \tab 1 \tab P12345 \tab DB.Object.ID\cr
#' 3 \tab DB Object Symbol \tab required \tab 1 \tab PHO3 \tab DB.Object.Symbol\cr
#' 4 \tab Qualifier \tab optional \tab 0 or greater \tab NOT \tab Qualifier\cr
#' 5 \tab GO ID \tab required \tab 1 \tab GO:0003993 \tab GO.ID\cr
#' 6 \tab DB:Reference (|DB:Reference) \tab required \tab 1 or greater \tab PMID:2676709 \tab DB.Reference\cr
#' 7 \tab Evidence Code \tab required \tab 1 \tab IMP \tab Evidence.Code\cr
#' 8 \tab With (or) From \tab optional \tab 0 or greater \tab GO:0000346 \tab With.or.From\cr
#' 9 \tab Aspect \tab required \tab 1 \tab F \tab Aspect\cr
#' 10 \tab DB Object Name \tab optional \tab 0 or 1 \tab Toll-like receptor 4 \tab DB.Object.Name\cr
#' 11 \tab DB Object Synonym (|Synonym) \tab optional \tab 0 or greater \tab hToll|Tollbooth \tab DB.Object.Synonym\cr
#' 12 \tab DB Object Type \tab required \tab 1 \tab protein \tab DB.Object.Type\cr
#' 13 \tab Taxon(|taxon) \tab required \tab 1 or 2 \tab taxon:9606 \tab Taxon\cr
#' 14 \tab Date \tab required \tab 1 \tab 20090118 \tab Date\cr
#' 15 \tab Assigned By \tab required \tab 1 \tab SGD \tab Assigned.By\cr
#' 16 \tab Annotation Extension \tab optional \tab 0 or greater \tab part_of(CL:0000576) \tab Annotation.Extension\cr
#' 17 \tab Gene Product Form ID \tab optional \tab 0 or 1 \tab UniProtKB:P12345-2 \tab Gene.Product.Form.ID\cr
#' }
#' @return a dataframe with columns corresponding to
#' GAF 1.0 or 2.0 specifications (see details)
#' @note
#' the GAF file may be provided in plain text or gzipped format. this is checked
#' automaticaly, there is no need to add the .gz extension.
#' @seealso \link{mdb.gaf.filter}
#' @examples
#' tab <- mdb.gaf.read(lx.system.file("samples/test_gaf.dat", "xlx"))
#' tab <- mdb.gaf.filter(tab)
#' tab[tab$gid=="VPS4A",]
#' 
#' tac <- mdb.gaf.read(lx.system.file("samples/test_gaf_compressed.dat", "xlx"))
#' identical(tab, tac)
#
mdb.gaf.read <- function(pathname) {

  # try gzipped version if it exists
  
  if (file.exists(paste0(pathname, ".gz")))
    pathname <- paste0(pathname, ".gz")
  
  # read table and setup colnames according to gaf 1.0/2.0 specs.
  
  gaf <- read.table(pathname, header=F, stringsAsFactors=F,
                    comment.char="!", sep="\t", quote="")
  gaf.cols <- c("DB", "DB.Object.ID", "DB.Object.Symbol", "Qualifier", "GO.ID",
                "DB.Reference", "Evidence.Code", "With.or.From", "Aspect",
                "DB.Object.Name", "DB.Object.Synonym", "DB.Object.Type", "Taxon",
                "Date", "Assigned.By", "Annotation.Extension", "Gene.Product.Form.ID")
  colnames(gaf) <- head(gaf.cols, ncol(gaf))
  
  gaf
}

# -------------------------------------------------
#' filter gaf table and keep only geneID <-> termID associations
#' @param gaf gaf dataframe (from \link{mdb.gaf.read})
#' @param DB source DBs to keep (keep all if NULL)
#' @param GN gene IDs to keep (keep all if NULL)
#' @param gid.col column index of geneID
#' @param tid.col column index of termID
#' @param no.qual if TRUE (dft) remove entries with (non empty) qualifiers
#'        else keep all entries, including the repugnant \bold{NOT} qualifier,
#'        (this may therefore lead to plain wrong associations)
#' @return a dataframe with two columns: 'gid', 'tid' specifying the
#'         association
#' @seealso \link{mdb.gaf.read}
#' @examples
#' tab <- mdb.gaf.read(lx.system.file("samples/test_gaf.dat", "xlx"))
#' tab <- mdb.gaf.filter(tab)
#' tab[tab$gid=="VPS4A",]
#

mdb.gaf.filter <- function(gaf, DB=c("User", "UniProtKB"), GN=NULL,
                           gid.col="DB.Object.Symbol", tid.col="GO.ID",
                           no.qual=TRUE) {
  
  # remove entries with nasty qualifiers (NOT is plain non-sense)
  if (no.qual)
    gaf <- gaf[gaf$Qualifier == "",]
  
  # filter source
  if (! is.null(DB))
    gaf <- gaf[gaf$DB %in% DB,]
  
  # keep only gid tid association
  gaf <- gaf[,c(gid.col, tid.col)]
  colnames(gaf) <- c("gid", "tid")
  
  # remove invalid chars in gid/tid
  gaf$gid <- gsub("[^A-Za-z0-9\\.]", ".", toupper(gaf$gid))
  gaf$tid <- gsub("[^A-Za-z0-9\\.]", ".", toupper(gaf$tid))
  
  # filter Genes
  if (! is.null(GN)) {
    gaf <- gaf[gaf$gid %in% toupper(GN),]
  }
  
  # remove duplicates
  keys <- paste0(gaf$gid, "#", gaf$tid)
  gaf <- gaf[! duplicated(keys),]
  
  gaf
}

