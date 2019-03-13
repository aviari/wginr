#
# $Id: Makefile 474 2019-01-10 23:38:37Z viari $
#

SHELL = /bin/sh

.PHONY: all banner check compile option install test clean distclean

all: compile

banner:
	@echo ' __          __      _____                        _____  '
	@echo ' \ \        / /     / ____|         ()           |  __ \ '
	@echo '  \ \  /\  / /     | |  __          _   _ __     | |__) |'
	@echo '   \ \/  \/ /      | | |_ |        | | |  _ \    |  _  / '
	@echo '    \  /\  /       | |__| |        | | | | | |   | | \ \ '
	@echo '     \/  \/  hole   \_____| enomes |_| |_| |_|   |_|  \_\'
	@echo '...Lite Distribution...'
	@echo ''
	
	@echo ' $(TARGET) Ok'
	@echo ''

check:
	scripts/checkconfig
	@make banner TARGET=$@

compile:
	cd Csrc && make all
	cd Rsrc && make all
	@make banner TARGET=$@

install:
	cd Csrc && make install
	cd Rsrc && make install
	@make banner TARGET=$@

test:
	cd Csrc && make test
	cd Rsrc && make test
#	cd samples && run_test
	@make banner TARGET=$@

clean:
	cd Csrc && make clean
	cd Rsrc && make clean
	@make banner TARGET=$@

distclean:
	cd Csrc && make distclean
	cd Rsrc && make distclean
	@make banner TARGET=$@

#
# targets for dev only
#

# dev_archive:
#	cd Archives && make install

# dev-status:
#	svn status --no-ignore
