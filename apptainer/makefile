
all:
	ninja -C build
	extensions/cando/scando-zeus-install

amber.sif: ./apptainer/amber.def
	apptainer build --fakeroot ./amber.sif apptainer/amber.def

cando.sif: amber.sif apptainer/cando-debian.def
	./apptainer/build-cando-debian

app:
	make cando.sif

send:
	scp cando.sif owlsnest:work/bin/
#	time ssh owlsnest-update

app-send:
	make app
	make send
