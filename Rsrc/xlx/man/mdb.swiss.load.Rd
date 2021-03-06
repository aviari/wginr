% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mdb.swiss.r
\name{mdb.swiss.load}
\alias{mdb.swiss.load}
\title{quick load swissprot db}
\usage{
mdb.swiss.load(dbname, extra = "ALL", force = FALSE, local = TRUE)
}
\arguments{
\item{dbname}{filename (without extension) of uniprot db}

\item{extra}{string comma-separated list of additional lines to parse (e.g "DE,OS,OC,KW")\cr
if empty only the default ID, AC, and '  ' (sequence) lines are parsed\cr
if "ALL" then all keys are parsed}

\item{force}{force read and serialize even if serialized file already exists}

\item{local}{if TRUE (default), serialized DB is saved and/or loaded in
current directory else in \code{dirname(dbname)}.}
}
\value{
a list indexed by records primary AC\cr
        each element is a list indexed by the record key (except sequence that is indexed by 'seq')\cr
        each recordkey element is either the raw line(s) or the resutl of a specific parser\cr
        current parsers are provided for : AC, OC, KW, DR\cr
}
\description{
this is a quicker version of \link{mdb.swiss.read}\cr
\code{mdb.swiss.load} try to recover a previously loaded and serialized 
file called : \code{dbname.<sort_extra>.rds}
(where <sort_extra> is a '_' separated string of sorted extra, see below)\cr
if it does not exist then it reads the flat file called : \code{dbname.dat}
and further serialize the result into \code{dbname.<sort_extra>.rds}\cr
you may force to ignore the serialized version by using force=TRUE
}
\note{
you may add your own function \code{.mdb.swiss.parse.<key>(rec)}
to parse other keys than (AC, OC, KW, DR and seq).
}
\examples{

db <- mdb.swiss.load(lx.system.file("samples/test_swiss", "xlx"))

# reload serialized version
db <- mdb.swiss.load(lx.system.file("samples/test_swiss", "xlx"))

# get entry names
names(db)

# get info about specific entry :
db$P04395
db$P04395$OC
db$P04395$DR$PROSITE

# search for entries matching pattern :
mdb.find(db, "KW", "gluconate", ignore.case=TRUE)

# remove local serialized DB
unlink("test_swiss.ALL.rds")

}
\seealso{
\link{mdb.swiss.read}
}
