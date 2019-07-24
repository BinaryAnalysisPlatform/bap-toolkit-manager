

install: build
	mv bap_report.native bap-report
	cp bap-report /home/oleg/bin

build:
	bapbuild bap_report.native

clean:
	bapbuild -clean
