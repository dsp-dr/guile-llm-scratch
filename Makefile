# Makefile for Guile3 Book Implementation
# Uses GNU Make (gmake on FreeBSD)

# Configuration
PREFIX ?= /usr/local
GUILE ?= guile3
GUILD ?= guild3
EMACS ?= emacs

# Directories
SRC_DIR = src
TEST_DIR = tests
DOC_DIR = docs
BUILD_DIR = build
DIST_DIR = dist

# Source files
SCM_SOURCES := $(shell find $(SRC_DIR) -name '*.scm' 2>/dev/null)
TEST_SOURCES := $(shell find $(TEST_DIR) -name '*-test.scm' 2>/dev/null)
ORG_SOURCES := $(shell find $(DOC_DIR) -name '*.org' 2>/dev/null)

# Compiled files
GO_FILES := $(SCM_SOURCES:$(SRC_DIR)/%.scm=$(BUILD_DIR)/%.go)

# Flags
GUILE_FLAGS = -L $(SRC_DIR) -C $(BUILD_DIR)
GUILD_FLAGS = -L $(SRC_DIR) -O2

# Default target
.PHONY: all
all: compile

# Create build directory
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# Compile Scheme files
$(BUILD_DIR)/%.go: $(SRC_DIR)/%.scm | $(BUILD_DIR)
	@mkdir -p $(dir $@)
	$(GUILD) compile $(GUILD_FLAGS) -o $@ $<

# Main targets
.PHONY: compile
compile: $(GO_FILES)
	@echo "Compilation complete."

.PHONY: tangle
tangle: $(ORG_SOURCES)
	@echo "Tangling org files..."
	@for file in $(ORG_SOURCES); do \
		$(EMACS) --batch \
			--eval "(require 'org)" \
			--eval "(org-babel-do-load-languages 'org-babel-load-languages '((scheme . t)))" \
			--eval "(setq org-confirm-babel-evaluate nil)" \
			--eval "(org-babel-tangle-file \"$$file\")" \
			2>/dev/null || echo "Warning: Could not tangle $$file"; \
	done
	@echo "Tangling complete."

.PHONY: test
test: compile
	@echo "Running tests..."
	@if [ -f "$(TEST_DIR)/test-runner.scm" ]; then \
		$(GUILE) $(GUILE_FLAGS) $(TEST_DIR)/test-runner.scm; \
	else \
		for test in $(TEST_SOURCES); do \
			echo "Running $$test..."; \
			$(GUILE) $(GUILE_FLAGS) $$test; \
		done; \
	fi

.PHONY: repl
repl:
	$(GUILE) $(GUILE_FLAGS) --listen

.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)
	rm -rf $(DIST_DIR)
	find . -name "*~" -delete
	find . -name "*.go" -delete

.PHONY: distclean
distclean: clean
	rm -rf tmp/*.pdf
	rm -rf tmp/*.html

# Documentation targets
.PHONY: docs
docs: docs-html docs-info

.PHONY: docs-html
docs-html: $(ORG_SOURCES)
	@echo "Generating HTML documentation..."
	@mkdir -p $(DIST_DIR)/html
	@for file in $(ORG_SOURCES); do \
		base=$$(basename $$file .org); \
		$(EMACS) --batch $$file \
			--eval "(require 'ox-html)" \
			--eval "(setq org-export-with-toc t)" \
			--eval "(setq org-html-validation-link nil)" \
			-f org-html-export-to-html \
			2>/dev/null || echo "Warning: Could not export $$file"; \
		mv $(DOC_DIR)/$$base.html $(DIST_DIR)/html/ 2>/dev/null || true; \
	done

.PHONY: docs-pdf
docs-pdf: $(ORG_SOURCES)
	@echo "Generating PDF documentation..."
	@mkdir -p $(DIST_DIR)/pdf
	@for file in $(ORG_SOURCES); do \
		base=$$(basename $$file .org); \
		$(EMACS) --batch $$file \
			--eval "(require 'ox-latex)" \
			--eval "(setq org-latex-pdf-process '(\"pdflatex -interaction nonstopmode %f\" \"pdflatex -interaction nonstopmode %f\"))" \
			-f org-latex-export-to-pdf \
			2>/dev/null || echo "Warning: Could not export $$file to PDF"; \
		mv $(DOC_DIR)/$$base.pdf $(DIST_DIR)/pdf/ 2>/dev/null || true; \
	done

.PHONY: docs-info
docs-info: $(ORG_SOURCES)
	@echo "Generating Info documentation..."
	@mkdir -p $(DIST_DIR)/info
	@for file in $(ORG_SOURCES); do \
		base=$$(basename $$file .org); \
		$(EMACS) --batch $$file \
			--eval "(require 'ox-texinfo)" \
			-f org-texinfo-export-to-info \
			2>/dev/null || echo "Warning: Could not export $$file to Info"; \
		mv $(DOC_DIR)/$$base.info $(DIST_DIR)/info/ 2>/dev/null || true; \
	done

# Installation targets
.PHONY: install
install: compile
	@echo "Installing to $(PREFIX)..."
	@mkdir -p $(PREFIX)/share/guile/site/3.0
	@mkdir -p $(PREFIX)/lib/guile/3.0/site-ccache
	@cp -r $(SRC_DIR)/* $(PREFIX)/share/guile/site/3.0/
	@cp -r $(BUILD_DIR)/* $(PREFIX)/lib/guile/3.0/site-ccache/

.PHONY: uninstall
uninstall:
	@echo "Uninstalling from $(PREFIX)..."
	@rm -rf $(PREFIX)/share/guile/site/3.0/core
	@rm -rf $(PREFIX)/share/guile/site/3.0/utils
	@rm -rf $(PREFIX)/lib/guile/3.0/site-ccache/core
	@rm -rf $(PREFIX)/lib/guile/3.0/site-ccache/utils

# Development helpers
.PHONY: check
check: test

.PHONY: lint
lint:
	@echo "Running linter..."
	@for file in $(SCM_SOURCES); do \
		$(GUILE) -c "(use-modules (system base compile)) \
			(compile-file \"$$file\" \
			#:opts '(#:warnings (unbound-variable arity-mismatch format)))" \
			2>&1 | grep -v "wrote" || true; \
	done

.PHONY: format
format:
	@echo "Formatting Scheme files..."
	@command -v guile-fmt >/dev/null 2>&1 && \
		find $(SRC_DIR) $(TEST_DIR) -name '*.scm' -exec guile-fmt -i {} \; || \
		echo "guile-fmt not found. Install it for auto-formatting support."

.PHONY: watch
watch:
	@echo "Watching for changes..."
	@while true; do \
		$(MAKE) -q compile || $(MAKE) compile; \
		sleep 2; \
	done

# Help target
.PHONY: help
help:
	@echo "Guile3 Book Implementation Makefile"
	@echo ""
	@echo "Usage: gmake [target]"
	@echo ""
	@echo "Main targets:"
	@echo "  all       - Compile all source files (default)"
	@echo "  compile   - Compile Scheme source files"
	@echo "  tangle    - Extract code from org files"
	@echo "  test      - Run test suite"
	@echo "  repl      - Start REPL with project loaded"
	@echo "  clean     - Remove compiled files"
	@echo "  distclean - Remove all generated files"
	@echo ""
	@echo "Documentation targets:"
	@echo "  docs      - Generate all documentation"
	@echo "  docs-html - Generate HTML documentation"
	@echo "  docs-pdf  - Generate PDF documentation"
	@echo "  docs-info - Generate Info documentation"
	@echo ""
	@echo "Installation targets:"
	@echo "  install   - Install to PREFIX (default: /usr/local)"
	@echo "  uninstall - Remove installation"
	@echo ""
	@echo "Development targets:"
	@echo "  check     - Alias for test"
	@echo "  lint      - Check for warnings"
	@echo "  format    - Format source files (requires guile-fmt)"
	@echo "  watch     - Auto-compile on changes"
	@echo ""
	@echo "Variables:"
	@echo "  PREFIX    - Installation prefix (default: /usr/local)"
	@echo "  GUILE     - Guile interpreter (default: guile3)"
	@echo "  GUILD     - Guild compiler (default: guild3)"