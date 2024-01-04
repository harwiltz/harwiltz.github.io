BUILDDIR=build
EMACS=emacs --batch

all: init
	$(EMACS) -l publish.el --eval "(publish-site \"$(BUILDDIR)\" t)"

serve:
	python -m http.server -d $(BUILDDIR)

init:
	mkdir -p $(BUILDDIR)

clean:
	rm -rf $(BUILDDIR)
