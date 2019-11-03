default: run

profile:
	-@clear
	-@stack build --profile
	-@stack exec --profile -- rg-fa-exe +RTS -p -hc -i0.01
	-@stack exec -- hp2ps -c rg-fa-exe.hp && ps2pdf rg-fa-exe.ps
	-@mv rg-fa-exe.prof profiling-data/prof/`date -Iminutes`.prof
	-@mv rg-fa-exe.hp profiling-data/heap-prof/`date -Iminutes`.hp
	-@mv rg-fa-exe.pdf profiling-data/heap-vis/`date -Iminutes`.pdf
	-@profiteur profiling-data/prof/`date -Iminutes`.prof
	-@mv profiling-data/prof/`date -Iminutes`.prof.html profiling-data/prof-vis/`date -Iminutes`.prof.html
	-@rm rg-fa-exe.aux
	-@rm rg-fa-exe.ps

clean-profile:
	-@rm profiling-data/heap-prof/*
	-@rm profiling-data/heap-vis/*
	-@rm profiling-data/prof/*
	-@rm profiling-data/prof-vis/*

run:
	-@stack run rg-fa-exe

swagger:
	-@stack run swagger-spec-exe

trace:
	-@stack test --trace --coverage

test:
	-@stack test --coverage

doc:
	-@stack haddock

clean:
	-@stack clean
	-@clear

.PHONY: default test run clean bench profile doc run-spec swagger ghcid ghcid-exe
