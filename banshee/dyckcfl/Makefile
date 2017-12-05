# Copyright (c) 2000-2004
#      The Regents of the University of California.  All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. Neither the name of the University nor the names of its contributors
#    may be used to endorse or promote products derived from this software
#    without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.

#OS:=$(shell uname)

#ifeq ($(OS), Darwin)
#	jni_dir = /System/Library/Frameworks/JavaVM.framework/Headers 
#else 
#	jni_dir = /usr/local/java/include/
#endif

#ifndef RLIB
region_dir = ../libcompat
#else
#region_dir = ../rlib
#endif

engine_dir = ../engine
banshee_bindir = ../bin
banshee_exe = ../bin/banshee.exe

DYCKCFL = dyckcfl

TARGETS =  $(DYCKCFL).o mr_$(DYCKCFL).o $(DYCKCFL)_terms.h $(DYCKCFL)_spec.o $(DYCKCFL)_terms.c $(DYCKCFL)_terms.o dyckcfl-replay.o dyckcfl-replay

TAGFILES = $(wildcard *.c) $(wildcard *.h)

#CC = gcc

ifdef DEBUG
CFLAGS = -I$(engine_dir) -I$(region_dir) -Ddeletes= -Dtraditional= \
         -Dsameregion= -Dparentptr= -Wall -Werror -g -pg \
	 -DREGION_PROFILE
#
else
CFLAGS = -I$(engine_dir) -I$(region_dir) -Ddeletes= -Dtraditional= \
         -Dsameregion= -Dparentptr= -Wall -Werror \
	 -DREGION_PROFILE
#
endif

.PHONY: clean 

all: $(TARGETS) 

mr_$(DYCKCFL).o: mr_$(DYCKCFL).c mr_$(DYCKCFL).h
	$(CC) -c $(CFLAGS) -DNONSPEC mr_$(DYCKCFL).c

$(DYCKCFL).o: $(DYCKCFL).c $(DYCKCFL).h
	$(CC) -c $(CFLAGS) -DNONSPEC $(DYCKCFL).c

$(DYCKCFL)_terms.h: $(DYCKCFL)_terms.bsp
	$(banshee_exe) $(DYCKCFL)_terms.bsp

$(DYCKCFL)_terms.c: $(DYCKCFL)_terms.bsp
	$(banshee_exe) $(DYCKCFL)_terms.bsp

TAGS: $(TAGFILES)
	etags $(TAGFILES)

#detect:
#	@echo "OS is $(OS)"

#jdyckcfl:
#	javac JDyckCFL.java
#	javah -jni JDyckCFL
#	$(CC) -c -I$(engine_dir) -I$(region_dir) -I$(jni_dir) -Ddeletes= \
#	-Dtraditional= -Dsameregion= -Dparentptr= -DNONSPEC -Wall JDyckCFLImp.c
#	$(CC) -dynamiclib -o libjdyckcfl.jnilib dyckcfl.o JDyckCFLImp.o $(engine_dir)/libnsengine.a $(region_dir)/libregions.a -framework JavaVM

dyckcfl-replay: dyckcfl-replay.o $(DYCKCFL).o
	$(CC) $(CFLAGS) dyckcfl-replay.o $(DYCKCFL).o -o dyckcfl-replay -L$(region_dir) -L$(engine_dir) -lnsengine -lregions

clean:
	rm -f $(TARGETS) *~ JDyckCFL.class JDyckCFLImp.o libjdyckcfl.jnilib 
