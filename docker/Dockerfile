#
# Docker recipe for WgInR
#

FROM r-base

MAINTAINER alain.viari@inria.fr

RUN apt-get update && apt-get upgrade -y && apt-get install -y \
    csh git texlive texinfo

RUN cd home && git clone --progress https://github.com/aviari/wginr.git

# change to your favorite CRAN repository
ENV CRAN_REPOS http://cran.irsn.fr

RUN cd home/wginr && make all install clean

ENV PATH /home/wginr/scripts:/home/wginr/ports/x386-linux/bin:$PATH

CMD ["/bin/bash"]

