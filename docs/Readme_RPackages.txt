# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ! Only read this in case of trouble with installation of R packages !
# ! This is intended for administrators only                          !
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#

The R part of WGInR need the following external R packages :

xtable          (used by lx)
bit             (used by xlx)

intervals       (used by xlx)
digest          (used by xlx)

nlme            (used by asdog)
mgcv            (used by asdog)
bcp             (used by asdog)
data.table      (used by asdog)
RColorBrewer    (used by asdog)
viridisLite     (used by asdog)

and the following optional external R package(s) :

testthat        (for testing during development)

It will build the following internal packages : 

lx      : basic utilities
xlx     : extended utilities
rbgzf   : RBGZF file format handler
thmm    : Discrete time HMM
asdog   : CNV analysis

If these packages are not installed on your machine, then 'make install'
will try to install them automatically by trying (in this order):

-1- to download them from CRAN
-2- to install them from a local source copy, located in Rsrc/packages/source
-3- to install them from a local binary copy, located in Rsrc/packages/binary/<portname>

Most of the time, this should make the process as transparent as possible.
However, in case of trouble you need to install the faulty package(s) by yourself
using the R install.packages() function manually.

Hopefully, since all these external packages are quite common they should
not give you any sweat.

The only trouble may come with 'bcp' (actually RcppArmadillo) 
which requires gcc >= 4.6

