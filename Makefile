CC = /usr/bin/mzc

all: delegation2spass delegation2spass_dist

delegation2spass: delegation2spass.scm
	$(CC) --exe delegation2spass delegation2spass.scm

delegation2spass_dist: delegation2spass
	$(CC) --exe-dir delegation2spass_dist delegation2spass

clean:
	rm -r delegation2spass delegation2spass_dist
