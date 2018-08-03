.PHONY: all clean 

BUILD_EXAMPLE=\
	dune build sactor.exe
	

CLEAN= dune clean
INSTALL=dune install

all:
		${BUILD_EXAMPLE}
		#cp _build/default/test.exe ./test


clean:
	${CLEAN}
	#rm ./test
