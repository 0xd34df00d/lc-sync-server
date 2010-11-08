
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

