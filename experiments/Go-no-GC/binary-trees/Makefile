ifndef GO
GO := go
endif

compile:
	$(GO) build -o binarytrees_run binarytrees.go

run:
	./binarytrees_run 21

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" ./binarytrees_run 21
