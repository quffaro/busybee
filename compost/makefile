SHELL = /bin/bash

### Type 'make' at the prompt to see a list of available tasks.

# --- Variables used by rules ---

core-files := pollen.rkt \
              index.ptree \
			  pollen-local/polytag.rkt \
			  pollen-local/common-helpers.rkt \

posts-sourcefiles := $(wildcard posts/*.poly.pm)

posts-html := $(patsubst %.poly.pm,%.html,$(posts-sourcefiles))
posts-tree := $(patsubst %.poly.pm,%.tree,$(posts-sourcefiles))

# I want to show off my Pollen source files, so I name them .pollen.html
posts-sourcelistings := $(patsubst %.poly.pm,%.pollen.html,$(posts-sourcefiles))

other-sourcefiles := books.html.pm about.html.pm
other-html := $(patsubst %.html.pm,%.html,$(other-sourcefiles))
other-sourcelistings := $(patsubst %.html.pm,%.pollen.html,$(other-sourcefiles))

# --- Rules ---

all: last_html.rebuild $(posts-html) $(posts-sourcelistings) $(other-html) $(other-sourcelistings) index.html feed.xml topics.html
all: ## Update all web content (not trees)

# Certain files affect all HTML output files. If these change, I want to do a complete rebuild
# of all posts. Rendering the whole ptree is somewhat faster than rendering each post separately.
#
# To ensure Pollen doesn't rely on its cache for these rebuilds, we need to touch pollen.rkt.
# But we save its timestamp first and restore it afterwards; otherwise we'd have the side effect
# of triggering the rule for last_tree.rebuild also, even if pollen.rkt hadn't actualy been changed.
last_html.rebuild: $(core-files) template.html.p pollen-local/tags-html.rkt
	touch -r pollen.rkt _save_timestamp
	touch pollen.rkt
	raco pollen setup -p posts/
	raco pollen render -p -t html posts/*.poly.pm 
	tidy -quiet -modify -indent --wrap 0 --tidy-mark no --drop-empty-elements no posts/*.html || true
	touch -r _save_timestamp pollen.rkt; rm _save_timestamp
	touch last_html.rebuild

# If the above rule was triggered, all the posts-html files will already have been re-rendered.
# (That rule comes before this one in the list of dependencies for "all")
# But if not, any individual files that have been edited will get re-rendered.
$(posts-html): %.html: %.poly.pm
	raco pollen render -t html $<
	tidy -quiet -modify -indent --wrap 0 --tidy-mark no --drop-empty-elements no $@ || true

$(posts-sourcelistings): util/make-html-source.sh
$(posts-sourcelistings): %.pollen.html: %.poly.pm
	util/make-html-source.sh $< > $@
	tidy -quiet -modify -indent --wrap 0 --tidy-mark no --drop-empty-elements no $@ || true

trees: ## Update tree files
trees: last_tree.rebuild $(posts-tree)

# Similar to HTML, first re-render all the trees if necessary...
last_tree.rebuild: $(core-files) template.tree.p pollen-local/tags-tree.rkt
	touch -r pollen.rkt _save_timestamp
	touch pollen.rkt
	raco pollen setup -p posts/
	raco pollen render -p -t tree posts/*.poly.pm
	touch -r _save_timestamp pollen.rkt; rm _save_timestamp
	touch last_tree.rebuild

# ...then, if a complete re-render wasn't necessary, render any individual trees that need it.
$(posts-tree): %.tree: %.poly.pm
	raco pollen render -t tree $<

feed.xml: $(core-files) $(posts-sourcefiles) feed.xml.pp pollen-local/tags-html.rkt
	touch feed.xml.pp
	raco pollen render feed.xml.pp

index.html: $(core-files) $(posts-sourcefiles) 
index.html: index.html.pp pollen-local/tags-html.rkt
	touch index.html.pp
	raco pollen render index.html
	tidy -quiet -modify -indent --wrap 0 --tidy-mark no --drop-empty-elements no index.html || true

$(other-html): %.html: %.html.pm
	raco pollen render $@
	tidy -quiet -modify -indent --wrap 0 --tidy-mark no --drop-empty-elements no $@ || true

topics.html: topics.html.pp $(core-fils) $(posts-sourcefiles) pollen-local/tags-html.rkt
	touch topics.html.pp
	raco pollen render topics.html.pp
	tidy -quiet -modify -indent --wrap 0 --tidy-mark no --drop-empty-elements no topics.html || true
