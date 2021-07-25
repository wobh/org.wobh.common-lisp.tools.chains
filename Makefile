.PHONY: install installdirs

abbrev_name = chains
system_type = tools
system_name = org.wobh.common-lisp.$(system_type).$(abbrev_name)

files = $(system_name).asd \
	$(abbrev_name).lisp \
	$(abbrev_name)-test.lisp

installdir = $(XDG_DATA_HOME)/common-lisp/source/$(system_name)/

installdirs :
	mkdir -p $(installdir)

install :
	install $(files) $(installdir)

