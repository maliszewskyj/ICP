#Toplevel makefile

all: icp mods imotors fndlam

mods: cd mods; $(MAKE)

icp:
	cd src; $(MAKE) icp

imotors: icp
	cd src; $(MAKE) imotors; $(MAKE) imot

fndlam: icp
	cd src; $(MAKE) fndlam
