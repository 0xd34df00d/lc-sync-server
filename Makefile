
MAKE_RELEASE_SCRIPT='\
	up_to_date=make:all(),\
	RelName="syncserver-0.1.0",\
	Opt=[{path,["./ebin/","./build/"]},{outdir,"./build/"}],\
	ok=systools:make_script(RelName,Opt),\
	ok=systools:make_tar(RelName,Opt),\
	io:format("SyncServer was built~n").'

INSTALL_RELEASE_SCRIPT='\
	{ok,Vsn}=release_handler:unpack_release("syncserver-0.1.0"),\
	io:format("SyncServer was unpacked~n").'

UNINSTALL_RELEASE_SCRIPT='\
	release_handler:remove_release("0.1.0"),\
	io:format("SyncServer was uninstalled~n").'

RELDIR=/usr/lib/erlang/releases

build:	clean
	erl -make
	mkdir build
	erl -eval $(MAKE_RELEASE_SCRIPT) -s erlang halt

clean:
	rm -f ebin/*.beam
	rm -rf build/

install:	build
	cp build/*.tar.gz $(RELDIR)
	erl -boot start_sasl -eval $(INSTALL_RELEASE_SCRIPT) -s erlang halt

uninstall:
	erl -boot start_sasl -eval $(UNINSTALL_RELEASE_SCRIPT) -s erlang halt
	rm -f $(RELDIR)/syncserver-0.1.0.tar.gz
	rm -f $(RELDIR)/syncserver-0.1.0.rel

