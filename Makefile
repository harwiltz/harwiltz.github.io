BUILDDIR=build
EMACS=emacs --batch

all: init
	$(EMACS) -l publish.el --eval "(publish-site \"$(BUILDDIR)\" t)"

init:
	mkdir -p $(BUILDDIR)

clean:
	rm -rf $(BUILDDIR)
