#include <tree_sitter/parser.h>
#include <string>

enum {
  COMMENT
};

void skip(TSLexer *lexer) {
  lexer->advance(lexer, true);
}

void advance(TSLexer *lexer) {
  lexer->advance(lexer, false);
}

bool scan(TSLexer *lexer) {
  while(true) {
    switch(lexer->lookahead) {
      case '(':
        advance(lexer);
        if (lexer->lookahead == '*') scan(lexer);
        break;
      case '*':
        advance(lexer);
        if (lexer->lookahead == ')') {
          advance(lexer);
          return true;
        }
        break;
      case '\0':
        if (lexer->eof(lexer)) { return false; }
        advance(lexer);
        break;
      default:
        advance(lexer);
    }
  }
}

extern "C" {
  void *tree_sitter_alt_ergo_external_scanner_create() {
    return NULL;
  }

  void tree_sitter_alt_ergo_external_scanner_destroy(void *payload) {
  }

  unsigned tree_sitter_alt_ergo_external_scanner_serialize(void *payload, char *buffer) {
  }

  void tree_sitter_alt_ergo_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
  }

  bool tree_sitter_alt_ergo_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
    while (iswspace(lexer->lookahead)) skip(lexer);
    if (lexer->lookahead == '(') {
      advance(lexer);
      lexer->result_symbol = COMMENT;
      return scan(lexer);
    }
    return false;
  }
}
