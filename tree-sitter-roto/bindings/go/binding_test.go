package tree_sitter_roto_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_roto "github.com/nlnetlabs/roto/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_roto.Language())
	if language == nil {
		t.Errorf("Error loading Roto grammar")
	}
}
