#
# Readme
#

# ------------------------------
# -0- Installation
# ------------------------------
#

# le mieux est de tout reloader depuis le svn gforge d'Inria

svn co svn+ssh://USER@scm.gforge.inria.fr/svn/wginr/trunk WGInR

# puis

cd WGInR

make all install

# ------------------------------
# -1- generation des fichiers baf et basta
# ------------------------------

# les fichiers baf sont produits à partir des fichiers bam
# par la commande 'bam2baf' (qui se trouve dans WGInR/ports/<portname>/bin)
#
# Sept 2018 : now version 1.2 
#                   indel bug corrected
#                   added -q and -Q options
#                   disk write speedup
#
# Jan 2019 : now bam2baf will multithread compression
#            this is a 30% time gain
#

------------------------------------------
 bam2baf Version 1.4
------------------------------------------
synopsis :
  convert bam to baf format
usage: bam2baf [options] in.bam out.baf
------------------------------------------
options:

-h : this [H]elp

-p : generate [P]seudo baf (header only)

-q INT : min map [q]uality (dft=0)

-q INT : min base [Q]uality (dft=13)

-v : set [V]erbose mode

-z : compress outfile with bg[Z]f

-@ INT : number of threads for compressing
         0=max available (dft=0)

------------------------------------------

#
# je conseile de mettre l'option z qui va produire un baf compressé
# <file>.baf.bgz, ca prend un peu plus de temps mais la taille du fichier
# est vraiment plus petite (facteur 10 à 100).
#

#
# de la meme façon, le fichier basta du génome est créé à partir
# du fichier fasta par la commande 'fasta2basta'
#

------------------------------------------
 fasta2basta Version 1.1
------------------------------------------
synopsis :
  convert fasta to basta format
usage: fasta2basta [-@ n -v -h -z] in.fst out.bst
------------------------------------------
options:

-h : this [H]elp

-v : set [V]erbose mode

-z : compress outfile with bg[Z]f

-@ INT : number of threads for compressing
         0:max available (dft=0)

# meme remarque sur l'option -z pour créer une version compressée

#
# ------------------------------
# -2- scripts asdog
# ------------------------------
#

# tous les scripts s'appellent désormais asdog.xxx
#
# l'ordre d'enchainement est :
#
# asdog.gcmodel.r   : construit le modèle GC
# asdog.gccorrect.r : calcule la correction GC d'après le modèle précédent
# asdog.plmodel.r   : construit le modèle de ploidy
# asdog.segment.r   : calcule la segmentation
#
# l'usage des scripts est donné en lancant le script sans argument
# (version courte) ou avec l'argument --help (version avec la liste des options)
#
# les scripts produisent :
# - des fichiers .rds
# - (optionellement) des fichiers pdf et txt
#

#
# scripts additionnels:
#
# asdog.report.r    : force le reporting des résultats d'un des scripts précédents
# asdog.matchpair.r : verifie le matching tumeur/normal
# asdog.popmap.r    : calcule la population ethnique la plus probable
# 

# -------------------
# asdog.gcmodel.r

usage: asdog.gcmodel.r --base=<base> --ref=<ref> [--outdir=<dir>] [--<option>=<value>]
       <base>   := path to baf file (without extension)
       <ref>    := path to ref genome basta file (without extension)
       <dir>    := output directory (dft='.')
use --help to get a list of available options

# -------------------
# asdog.gccorrect.r

usage: asdog.gccorrect.r --gcmodel=<gcmodel> [--outdir=<dir>] [--<option>=<value>]
       <gcmodel> := path to GC model (.rds) file
       <dir>     := output directory (dft=.)
use --help to get a list of available options

# -------------------
# asdog.plmodel.r

usage: asdog.plmodel.r --normal=<normal> --gccorrect=<gccorrect> [--outdir=<dir>] [--<option>=<value>]
 or    asdog.plmodel.r --plmodel=<plmodel> [--outdir=<dir>] [--<option>=<value>]
       <normal>    := path to normal baf file (without extension)
       <gccorrect> := path to gccorrect (.rds) file
       <plmodel>   := path to previous plmodel (.rds) file
       <dir>       := output directory (dft='.')
use --help to get a list of available options

# -------------------
# asdog.segment.r 

usage: asdog.segment.r --gccorrect=<gccorrect> [--plmodel=<plmodel>] [--outdir=<dir>] [--<option>=<value>]
       <gccorrect> := path to gccorrect (.rds) file
       <plmodel>   := path to ploidy model (.rds) file
       <dir>       := output directory (dft=.)
use --help to get a list of available options














