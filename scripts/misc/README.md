
# Miscellaneous scripts

These scripts are provided 'as is' and have not been
thoroughly tested yet.

>Important: These scripts should be copied (or linked) in the
>directory containing the wginr scripts, i.e. in ```wginr/scripts```.

# asdog

This script will chain the different asdog modules in order to fullfil
a complete CNV (asdog) analysis from tumor and paired normal WGS.

### installation
```
cd scripts
ln -s misc/asdog .
```

### description

Except for some cases that will be described hereafter, the script 
will produce all output files in the current directory unless the
--outdir=\<dirname\> option is provided in which case output will be
produced in the specified directory (called \<outdir\>).

The scripts basically take three arguments (see usage below):  
```asdog <ref> <tumor> <normal> ```  
and will chain the following operations:

1. if \<ref\>.bst[.bgz] is not found, it will convert \<ref\>.fst (fasta) to 
   \<ref\>.bst.bgz (basta). The basta file will be placed next to the fasta file,
   unless the directory is not writable, in which case it will be put in \<outdir\>.
2. if \<tumor\>.baf[.bgz] (resp. \<normal\>.baf[.bgz]) is not found, it will convert
   \<tumor\>.bam (sorted bam) to \<tumor\>.baf.bgz (baf). The baf file will be placed
   next to the bam file, unless the directory is not writable, in which case
   it will be put in \<outdir\>.
3. run the asdog.gcmodel.r, asdog.gccorrect.r, asdog.plmodel.r and asdog.segment.r
   scripts modules in this order. If the output (rds file) of a script module
   already exists then the module execution will be skipped, unless the --force
   option has been given. Note that whenever a new output is produced, then the
   subsequent modules in the chain will be forced to execute.
   The same is true if a new basta or baf file is generated in steps 1 or 2.
   However the script does not keep tracks of the options, therefore it will
   not reexecute a module event if options have changed (to do this use --force).
   
### usage
```
asdog [-f|--force] ref tumor normal [--<option>=<value>]*

  -f|--force : force removal of previous computations if any
         ref : reference file (without extension) Basta|Fasta format
       tumor : tumor file (without extension) Baf|Bam format
      normal : paired normal file (without extension) Baf|Bam format
     options : use asdog --help <module> to get a list of options
     
or

asdog --help gcmodel|gccorrect|plmodel|segment
  get list of module options
```

### examples

```sh
cd samples

./run_cleanup # cleanup previous computations

# In the following call, only the first line is mandatory.
# The other lines will just change the default parameters due to the artificial
# nature of this demo sample (very small size)

../scripts/asdog ref tumor normal \
   --chrs=1 --gcmodel.large.winsize=0  \
   --gcmodel.sample.gcbins=0 \
   --gcmodel.regions.minreg=1000 \
   --gcmodel.regions.binsize=1000 \
   --gcmodel.sample.size=1000
   
../scripts/asdog ref tumor normal  # will not rerun
```





