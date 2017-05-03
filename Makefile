all:
	jbuilder build @install

clean:
	@rm -r _build

examples:
	jbuilder build examples/main.exe examples/i3_workspaces.exe examples/with_subcommands.exe

.PHONY: all examples clean
