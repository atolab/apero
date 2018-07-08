.PHONY: all clean test doc

BUILD_LIB=jbuilder build --dev
BUILD_EXAMPLE=\
	jbuilder build example/echo/echo.exe 

CLEAN= jbuilder clean
TEST=jbuilder runtest -j1 --no-buffer --dev
DOC=jbuilder build --dev @doc
INSTALL=jbuilder install

all:
		${BUILD_LIB}
		${BUILD_EXAMPLE}
		
test:
		${TEST}

doc:
	${DOC}

install:
		${INSTALL}

clean:
	${CLEAN}
