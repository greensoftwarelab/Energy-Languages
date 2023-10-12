ifndef GO
GO := go
endif

compile:
	$(GO) build -o regexredux_run regexredux.go

run:
	env GOGC=off ./regexredux_run 0 < ../../../regexredux-input5000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../../scripts/RAPL/build/rapl "$(JSON)" env GOGC=off ./regexredux_run 0 < ../../../regexredux-input5000000.txt
