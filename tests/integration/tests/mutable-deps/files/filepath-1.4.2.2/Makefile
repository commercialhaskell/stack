all: cpp gen

cpp:
	cpphs --noline -DIS_WINDOWS=False -DMODULE_NAME=Posix   -OSystem/FilePath/Posix.hs   System/FilePath/Internal.hs
	cpphs --noline -DIS_WINDOWS=True  -DMODULE_NAME=Windows -OSystem/FilePath/Windows.hs System/FilePath/Internal.hs

gen:
	runhaskell Generate.hs

.PHONY: all cpp gen
