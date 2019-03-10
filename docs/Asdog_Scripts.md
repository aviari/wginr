#
# Readme
#

# ------------------------------
# -0- Installation
# ------------------------------
#

# the best is to reload everything from Inria's svn gforge

svn co svn+ssh://USER@scm.gforge.inria.fr/svn/wginr/trunk WGInR

# then

cd WGInR

make all install

# ------------------------------
# -1- generation of baf and basta files
# ------------------------------

# baf files are produced from bam files
# by the command'bam2baf' (which is in WGInR/ports/<portname>/bin)
#
# Sept 2018 : now version 1.2 
# indel bug corrected
# added -q and -Q options
# disk write speedup
#
# Jan 2019 : now bam2baf will multithread compression
# this is a 30% time gain
#

------------------------------------------
 bam2baf Version 1.4
------------------------------------------
synopsis:
  convert bam to baf format
use: bam2baf[options] in.bam out.baf
------------------------------------------
options:

-h : this[H]elp

-p : generate[P]seudo baf (header only)

-q INT : min map[q]uality (dft=0)

-q INT : min base[Q]uality (dft=13)

-v : set[V]erbose mode

-z : compress outfile with bg[Z]f

-@ INT : number of threads for compressing
         0=max available (dft=0)

------------------------------------------

#
# I advise to put the option z which will produce a compressed baf
# <file>.baf.bgz, it takes a little longer but the size of the file
# is really smaller (factor 10 to 100).
#

#
# in the same way, the basta file of the genome is created from
# of the file fasta by the command "fasta2basta
#

------------------------------------------
 fasta2basta Version 1.1
------------------------------------------
synopsis:
  convert fasta to basta format
use: fasta2basta[-@ n -v -h -z] in.fst out.bst
------------------------------------------
options:

-h : this[H]elp

-v : set[V]erbose mode

-z : compress outfile with bg[Z]f

-@ INT : number of threads for compressing
         0:max available (dft=0)

# same remark on the option -z to create a compressed version

#
# ------------------------------
# -2- scripts asdog
# ------------------------------
#

# all scripts are now called asdog.xxx
#
# the sequence order is:
#
# asdog.gcmodel.r : builds the GC model
# asdog.gccorrect.r : calculates the GC correction from the previous model
# asdog.plmodel.r : builds the ploidy model
# asdog.segment.segment.r : calculates the segmentation
#
# the use of scripts is given by running the script without arguments
# (short version) or with the --help argument (version with the list of options)
#
# scripts produce:
# - .rds files
# - (optionally) pdf and txt files
#

#
# additional scripts:
#
# asdog.report.r : forces the reporting of the results of one of the previous scripts
# asdog.matchpair.r : checks for tumor/normal matching
# asdog.popmap.r : calculates the most likely ethnic population
# 

# -------------------
# asdog.gcmodel.r

use: asdog.gcmodel.r --base=<base> --ref=<ref>[--outdir=<dir>][--<option>=<value>]
       <base> := path to baf file (without extension)
       <ref> : = path to ref genome basta file (without extension)
       <dir> := output directory (dft='.'')
use --help to get a list of available options

# -------------------
# asdog.gccorrect.r

use: asdog.gccorrect.r --gcmodel=<gcmodel>[--outdir=<dir>][--<option>=<value>]
       <gcmodel> := path to GC model (.rds) file
       <dir> := output directory (dft=.)
use --help to get a list of available options

# -------------------
# asdog.plmodel.r

use: asdog.plmodel.r --normal=<normal> --gccorrect=<gccorrect>[--outdir=<dir>][--<option>=<value>]
 or asdog.plmodel.r --plmodel=<plmodel>[--outdir=<dir>][--<option>=<value>]
       <normal> := path to normal baf file (without extension)
       <gccorrect> := path to gccorrect (.rds) file
       <plmodel> := path to previous plmodel (.rds) file
       <dir> := output directory (dft='.'')
use --help to get a list of available options

# -------------------
# asdog.segment.r 

use: asdog.segment.r --gccorrect=<gccorrect>[-- --plmodel=<plmodel>][--outdir=<dir>][--<option>=<value>]
       <gccorrect> := path to gccorrect (.rds) file
       <plmodel> := path to ploidy model (.rds) file
       <dir> := output directory (dft=.)
use --help to get a list of available options


Translated with www.DeepL.com/Translator