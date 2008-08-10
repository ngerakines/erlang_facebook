all: code

code: clean
	erl -make

clean:
	rm -rfv *.beam *.rel *.script *.boot erl_crash.dump erlang_facebook/ *.deb

package-debian: code
	mkdir -p erlang_facebook/usr/lib/erlang/lib/erlang_facebook-0.3/ebin/ && cp erlang_facebook.beam erlang_facebook/usr/lib/erlang/lib/erlang_facebook-0.3/ebin/erlang_facebook.beam
	mkdir -p erlang_facebook/DEBIAN/ && cp control erlang_facebook/DEBIAN/control
	dpkg -b erlang_facebook erlang_facebook.deb

install-debian: package-debian
	dpkg -i erlang_facebook.deb
