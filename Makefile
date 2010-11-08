
BEAMS=ebin

build:	clean
	erl -make

clean:
	rm -f ebin/*.beam

install:
	# Not implemented yet
	false

uninstall:
	# Not implemented yet
	false

run:
	./syncserver start

stop:
	./syncserver stop

db:	build
	./make-db

cookie:
	cat /dev/urandom|head -c32|md5sum|head -c32 > cookie
	chmod 400 cookie

