all: code

code: clean
	erl -make

clean:
	rm -rfv *.beam erl_crash.dump

dist-src: clean
	tar zcvf erlang_facebook-0.3.tgz Makefile README.markdown src/

