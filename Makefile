all: code

code: clean
	erlc src/erlang_facebook.erl

clean:
	rm -rfv *.beam erl_crash.dump

dist-src: clean
	tar zcvf erlang_facebook-0.3.2.tgz Makefile README.markdown src/
