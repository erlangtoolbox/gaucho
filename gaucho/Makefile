.PHONY: all clean Emakefile spec

all: ebin/gaucho.app Emakefile
	erl -noinput -eval 'up_to_date=make:all(),halt()'

ebin/gaucho.app: src/gaucho.app
	mkdir --parents -- ebin
	cp --force -- $< $@

ifdef DEBUG
EXTRA_OPTS := $(EXTRA_OPTS) debug_info,
endif

Emakefile: Emakefile.in
	sed "s/{{EXTRA_OPTS}}/$(EXTRA_OPTS)/" $< > $@

clean:
	rm --force -- ebin/*.beam Emakefile ebin/*.app *.spec
	rm -rf out

VERSION=`cat version`
RELEASE=`cat release`
REVISION=`git --no-pager log --max-count=1 --format=format:%h`

spec: opensuse.spec.in
	cat $< | \
		sed "s/{{VERSION}}/$(VERSION)/" | \
		sed "s/{{RELEASE}}/$(RELEASE)/" | \
		sed "s/{{REVISION}}/$(REVISION)/" \
		> strikead-erlang-gaucho.spec
