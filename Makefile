LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION=0.3.2

all:
	mkdir -p ebin/
	(cd src;$(MAKE))

test: all
	prove -v t/*.t

clean:
	(cd src;$(MAKE) clean)
	rm -rf erl_crash.dump *.beam *.hrl erlang_facebook-$(VERSION).tgz

package: clean
	@mkdir erlang_facebook-$(VERSION)/ && cp -rf src support Makefile README.markdown erlang_facebook-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf erlang_facebook-$(VERSION).tgz erlang_facebook-$(VERSION)
	@rm -rf erlang_facebook-$(VERSION)/

install:
	mkdir -p $(prefix)/$(LIBDIR)/erlang_facebook-$(VERSION)/ebin
	for i in ebin/*.beam; do install $$i $(prefix)/$(LIBDIR)/erlang_facebook-$(VERSION)/$$i ; done
