import std/[strformat, strutils, unicode, streams, tables, macros]
import utils, types

type
  TokenKind* = enum
    tkEmpty = "empty"
    tkNull = "null"
    tkStar = "star"
    tkPlus = "plus"
    tkBool = "bool"
    tkTilde = "tilde"
    tkComma = "comma"
    tkCaret = "caret"
    tkDollar = "dollar"
    tkSemicolon = "semicolon"
    tkGreater = "greater_than"
    tkSlashDash = "slash_dash"
    tkDoublePipe = "double_pipe"
    tkLineCont = "line_continuation"
    tkEqual = "equal"
    tkIdent = "identifier"
    tkString = "string"
    tkRawString = "raw_string"
    tkWhitespace = "whitespace"
    tkNewLine = "new_line"
    tkOpenPar = "open_parenthesis"
    tkClosePar = "close_parenthesis" # Type tagation
    tkOpenBra = "open_bracket"
    tkCloseBra = "close_bracket" # Children block
    tkOpenSqu = "open_square_bracket"
    tkCloseSqu = "close_square_bracket"
    tkNumFloat = "float_number"
    tkNumInt = "integer_number"
    tkNumHex = "hexadecimal_number"
    tkNumBin = "binary_number"
    tkNumOct = "octagonal_number"

  Token* = object
    lexeme*: string
    start*: int
    kind*: TokenKind

  Lexer* = object
    case isStream*: bool
    of true:
      stream*: Stream
    else:
      source*: string
      current*: int
    multilineStringsNewLines*: seq[tuple[idx, length: int]]
      # Indexes and length of new lines in multiline strings that have to be converted to a single \n
    stack*: seq[Token]

const
  # KDL 2.0 spec: characters that cannot appear in bare identifiers
  nonIdenChars = {'\\', '/', '(', ')', '{', '}', ';', '[', ']', '=', '"', '#'}
  nonInitialChars = Digits + nonIdenChars
  equals = [0x003D, 0xFE66, 0xFF1D, 0x1F7F0]
  litMatches = {
    # KDL 2.0 keywords
    "#null": tkNull,
    "#true": tkBool,
    "#false": tkBool,
    "#inf": tkNumFloat,
    "#-inf": tkNumFloat,
    "#nan": tkNumFloat,
    # Structural tokens
    ";": tkSemicolon,
    "/-": tkSlashDash,
    "(": tkOpenPar,
    ")": tkClosePar,
    "{": tkOpenBra,
    "}": tkCloseBra,
    "[": tkOpenSqu,
    "]": tkCloseSqu,
    # Note: +, -, *, ~, ^, $, >, < can be bare identifiers in KDL 2.0
    # Only match them if they're actually special in context
  }

proc `$`*(lexer: Lexer): string =
  result =
    if lexer.isStream:
      &"{(if lexer.stream.atEnd: \"SUCCESS\" else: \"FAIL\")}\n  "
    else:
      &"{(if lexer.current == lexer.source.len: \"SUCCESS\" else: \"FAIL\")} {lexer.current}/{lexer.source.len}\n  "

  for token in lexer.stack:
    result.add &"({token.kind})"
    result.addQuoted token.lexeme
    result.add " "

proc getPos*(lexer: Lexer): int =
  if lexer.isStream:
    lexer.stream.getPosition()
  else:
    lexer.current

proc setPos(lexer: var Lexer, x: int) =
  if lexer.isStream:
    lexer.stream.setPosition(x)
  else:
    lexer.current = x

proc inc(lexer: var Lexer, x = 1) =
  lexer.setPos(lexer.getPos() + x)

proc dec(lexer: var Lexer, x = 1) =
  lexer.setPos(lexer.getPos() - x)

macro lexing(token: TokenKind, body: untyped) =
  ## Converts a procedure definition like:
  ## ```nim
  ## proc foo() {.lexing: tkEmpty.} =
  ##   echo "hi"
  ## ```
  ## Into
  ## ```nim
  ## proc foo(lexer: var Lexer, consume: bool = true, addToStack: bool = true): bool {.discardable.} =
  ##   let before = getPos(lexer)
  ##   echo "hi"
  ##   result = before != getPos(lexer)
  ##   if not consume:
  ##     setPos(lexer, before)
  ##   if result and addToStack: # Only when token is not tkEmpty
  ##     lexer.add(token, before)
  ## ```

  body.expectKind(nnkProcDef)

  body.params[0] = ident"bool" # Return type
  body.params.add(newIdentDefs(ident"lexer", newNimNode(nnkVarTy).add(ident"Lexer")))
  body.params.add(newIdentDefs(ident"consume", ident"bool", newLit(true)))
  body.params.add(newIdentDefs(ident"addToStack", ident"bool", newLit(true)))

  body.addPragma(ident"discardable")

  # Modify the procedure statements list (body)

  body[^1].insert(
    0,
    quote do:
      let before {.inject.} = getPos(lexer),
  )
  body[^1].add(
    quote do:
      result = before != getPos(lexer)
  )
  body[^1].add(
    quote do:
      if not consume:
        setPos(lexer, before)
  )

  if token != bindSym"tkEmpty":
    body[^1].add(
      quote do:
        if result and addToStack:
          lexer.add(`token`, before)
    )

  result = body

proc eof(lexer: var Lexer, extra = 0): bool =
  let before = lexer.getPos
  inc lexer, extra

  result =
    if lexer.isStream:
      lexer.stream.atEnd
    else:
      lexer.current >= lexer.source.len

  lexer.setPos before

proc peek(lexer: var Lexer, next = 0): char =
  if not lexer.eof(next):
    let before = lexer.getPos
    inc lexer, next

    result =
      if lexer.isStream:
        lexer.stream.peekChar()
      else:
        lexer.source[lexer.current]

    lexer.setPos before

proc peekStr(lexer: var Lexer, until: int): string =
  if lexer.eof(until - 1):
    return

  if lexer.isStream:
    lexer.stream.peekStr(until)
  else:
    lexer.source[lexer.current ..< lexer.current + until]

proc peek(lexer: var Lexer, x: string): bool =
  lexer.peekStr(x.len) == x

proc peekRune(lexer: var Lexer): Rune =
  if lexer.eof():
    return

  if lexer.isStream:
    lexer.stream.peekRune()
  else:
    lexer.source.runeAt(lexer.current)

proc add(lexer: var Lexer, kind: TokenKind, start: int) =
  let before = lexer.getPos()
  lexer.setPos start
  lexer.stack.add(
    Token(kind: kind, lexeme: lexer.peekStr(before - start), start: start)
  )
  lexer.setPos before

proc error(lexer: Lexer, msg: string) =
  let coord =
    if lexer.isStream:
      lexer.stream.getCoord(lexer.getPos)
    else:
      lexer.source.getCoord(lexer.getPos)

  let errorMsg =
    if lexer.isStream:
      lexer.stream.errorAt(coord)
    else:
      lexer.source.errorAt(coord)

  raise newException(
    KdlLexerError, &"{msg} at {coord.line + 1}:{coord.col + 1}\n{errorMsg.indent(2)}\n"
  )

proc literal(lexer: var Lexer, lit: string, consume = true): bool {.discardable.} =
  result = lexer.peek(lit)
  if result and consume:
    lexer.inc lit.len

proc skipWhile(lexer: var Lexer, x: set[char]): int {.discardable.} =
  while not lexer.eof() and lexer.peek() in x:
    inc result
    inc lexer

proc disallowedRunes() {.lexing: tkEmpty.} =
  if lexer.eof():
    return
  let r = lexer.peekRune.int32
  if r == 0xFEFFi32:
    if lexer.getPos() == 0:
      lexer.inc 0xFEFF.Rune.size
    else:
      lexer.error &"The code point U+{r.toHex(4)} is only allowed at the start of a KDL document. Not"
  elif isDisallowedRune(r):
    lexer.error &"The code point U+{r.toHex(4)} isn't allowed on a KDL document"

proc tokenMultiLineComment*() {.lexing: tkEmpty.} =
  if not lexer.peek("/*"):
    return

  lexer.inc 2

  var nested = 1

  while not lexer.eof() and nested > 0:
    if lexer.peek("*/"):
      dec nested
      lexer.inc 2
    elif lexer.peek("/*"):
      inc nested
      lexer.inc 2
    else:
      inc lexer

  if nested > 0:
    lexer.error "Expected end of multi-line comment"

proc tokenNewLine*() {.lexing: tkNewLine.} =
  for nl in newLines:
    if lexer.peek(nl):
      lexer.inc nl.len
      break

proc tokenWhitespace*() {.lexing: tkWhitespace.} =
  ## This treats multline comments as whitespaces
  if not lexer.eof() and (let rune = lexer.peekRune(); rune.int in whitespaces):
    lexer.inc rune.size
  else:
    lexer.tokenMultiLineComment()

proc skipWhitespaceOrNewline*() {.lexing: tkEmpty.} =
  if not lexer.eof():
    if (let rune = lexer.peekRune(); rune.int in whitespaces):
      lexer.inc rune.size
    else:
      lexer.tokenNewLine(addToStack = false)

proc skipWhitespaces*() {.lexing: tkEmpty.} =
  while lexer.tokenWhitespace(addToStack = addToStack, consume = consume):
    discard

proc tokenNumWhole() {.lexing: tkEmpty.} =
  if lexer.peek() in {'-', '+'}:
    inc lexer

  if lexer.peek() notin Digits:
    lexer.setPos before
    return

  inc lexer

  lexer.skipWhile(Digits + {'_'})

proc tokenNumExp() {.lexing: tkEmpty.} =
  if lexer.peek().toLowerAscii() != 'e':
    return

  inc lexer

  if lexer.peek() in {'-', '+'}:
    inc lexer

  if lexer.peek() notin Digits:
    lexer.error "Expected one or more digits"

  lexer.skipWhile(Digits + {'_'})

proc tokenNumFloat() {.lexing: tkNumFloat.} =
  if not lexer.tokenNumWhole():
    lexer.setPos before
    return

  if lexer.peek() == '.':
    inc lexer

    if lexer.peek() notin Digits:
      lexer.error "Expected one or more digits"

    lexer.skipWhile(Digits + {'_'})

    if lexer.peek().toLowerAscii() == 'e':
      lexer.tokenNumExp()
  elif lexer.peek().toLowerAscii() == 'e':
    lexer.tokenNumExp()
  else:
    lexer.setPos before
    return

proc tokenNumInt*() {.lexing: tkNumInt.} =
  lexer.tokenNumWhole()

proc tokenNumBin*() {.lexing: tkNumBin.} =
  if lexer.peek("0b"):
    lexer.inc 2
    if lexer.peek() notin {'0', '1'}:
      lexer.error "Expected one or more binary digits"

    lexer.skipWhile({'0', '1', '_'})

proc tokenNumHex*() {.lexing: tkNumHex.} =
  if lexer.peek("0x"):
    lexer.inc 2
    if lexer.peek() notin HexDigits:
      lexer.error "Expected one or more octal digits"

    lexer.skipWhile(HexDigits + {'_'})

proc tokenNumOct*() {.lexing: tkNumOct.} =
  if lexer.peek("0o"):
    lexer.inc 2
    if lexer.peek() notin {'0' .. '7'}:
      lexer.error "Expected one or more octal digits"

    lexer.skipWhile({'0' .. '7', '_'})

proc getCoord(lexer: Lexer, pos = lexer.getPos()): Coord =
  if lexer.isStream:
    lexer.stream.getCoord(pos)
  else:
    lexer.source.getCoord(pos)

proc tokenEqual() {.lexing: tkEqual.} =
  if (let r = lexer.peekRune(); r.int32 in equals):
    lexer.inc r.size

proc tokenStringBody(lexer: var Lexer, raw = false) =
  let before = lexer.getPos()

  # In KDL 2.0, raw strings use #"..."# format (no 'r' prefix)
  let hashes = lexer.skipWhile({'#'})

  # Raw strings require at least one # before the quote
  if raw and hashes == 0:
    lexer.setPos before
    return

  # Regular strings should not start with #
  if not raw and hashes > 0:
    lexer.setPos before
    return

  if lexer.peek() != '"':
    lexer.setPos before
    return

  # Check for multiline strings (""" or #"""#)
  var isMultiline = false
  if lexer.peek() == '"' and lexer.peek(1) == '"' and lexer.peek(2) == '"':
    isMultiline = true
    inc lexer, 3  # Consume the three quotes
  else:
    inc lexer  # Single quote

  var terminated = false
  let newlineCountBefore = lexer.multilineStringsNewLines.len

  while not lexer.eof():
    lexer.disallowedRunes()

    let before = lexer.getPos()
    if lexer.tokenNewLine(addToStack = false):
      # Single-quote strings (both raw and regular) cannot contain newlines
      if not isMultiline:
        if raw:
          lexer.error "Raw strings with single quote cannot span multiple lines. Use #\"\"\"...\"\"\"# for multiline raw strings"
        else:
          lexer.error "Strings with single quote cannot span multiple lines. Use \"\"\"...\"\"\" for multiline strings"
      lexer.multilineStringsNewLines.add((before, lexer.getPos() - before))
      continue

    let r = lexer.peekRune()
    case r
    of '\\'.Rune:
      if raw:
        inc lexer
        continue

      lexer.inc

      if lexer.skipWhitespaceOrNewline():
        while lexer.skipWhitespaceOrNewline():
          discard
        continue

      let next = lexer.peek()
      if next notin escapeTable and next != 'u':
        lexer.error &"Invalid escape '{next}'"

      lexer.inc

      if next == 'u':
        let u_before = lexer.getPos()
        inc lexer # consume 'u'
        if lexer.peek() != '{':
          # Not a valid unicode escape. Backtrack and then just let the 'u' be processed as a normal character.
          lexer.setPos(u_before)
          continue # Skip the rest of the unicode processing for this character.
        
        # If it *is* a unicode escape, proceed with the original logic:
        inc lexer # consume '{'

        let digits = lexer.skipWhile(HexDigits)
        if digits notin 1 .. 6:
          lexer.error &"Expected 1-6 hexadecimal digits but found {digits}"

        if lexer.peek() != '}':
          lexer.error "Expected closing bracket '}'"
        inc lexer # consume '}'
        # The actual Rune conversion and adding to result happens in parser.nim's escapeString,
        # so here in lexer.nim, we just need to consume the token correctly.

    of '"'.Rune:
      # Check for closing delimiter
      if isMultiline:
        # Multiline strings need """ or #"""#
        if lexer.peek() == '"' and lexer.peek(1) == '"' and lexer.peek(2) == '"':
          inc lexer, 3
          let endHashes = lexer.skipWhile({'#'})
          if not raw or hashes == 0 or endHashes == hashes:
            terminated = true
            break
          elif endHashes > hashes:
            lexer.error &"Expected {hashes} hashes but found {endHashes}"
        else:
          # Single " inside multiline string, just consume it
          inc lexer
      else:
        # Regular string, single " ends it
        inc lexer
        let endHashes = lexer.skipWhile({'#'})
        if not raw or hashes == 0 or endHashes == hashes:
          terminated = true
          break
        elif endHashes > hashes:
          lexer.error &"Expected {hashes} hashes but found {endHashes}"
    else:
      lexer.disallowedRunes()
      inc lexer

  if not terminated:
    lexer.error "Unterminated string"

  # Multiline strings must actually span multiple lines
  if isMultiline:
    let newlineCount = lexer.multilineStringsNewLines.len - newlineCountBefore
    if newlineCount == 0:
      lexer.error "Multiline strings must contain at least one newline"

proc tokenString*() {.lexing: tkString.} =
  lexer.tokenStringBody()

proc tokenRawString*() {.lexing: tkRawString.} =
  lexer.tokenStringBody(raw = true)

proc tokenIdent*() {.lexing: tkIdent.} =
  if lexer.eof() or lexer.peek() in nonInitialChars:
    return

  # Check for disallowed bare identifiers: inf, -inf, nan, true, false, null (must use # prefix in KDL 2.0)
  if lexer.literal("inf") or lexer.literal("-inf") or lexer.literal("nan"):
    if (
      lexer.eof() or lexer.tokenWhitespace(addToStack = false) or
      lexer.tokenNewLine(addToStack = false) or lexer.peek() in nonIdenChars
    ):
      lexer.error "Bare 'inf', '-inf', or 'nan' are not allowed. Use #inf, #-inf, or #nan instead"

  # Check for disallowed bare keywords: true, false, null
  if lexer.literal("true") or lexer.literal("false") or lexer.literal("null"):
    if (
      lexer.eof() or lexer.tokenWhitespace(addToStack = false) or
      lexer.tokenNewLine(addToStack = false) or lexer.peek() in nonIdenChars
    ):
      lexer.error "Bare 'true', 'false', or 'null' are not allowed. Use #true, #false, or #null instead"

  # KDL 2.0: Reject identifiers starting with patterns that look like numbers
  let firstChar = lexer.peek()

  # Reject .digit pattern
  if firstChar == '.':
    # Check if there's a next character and if it's a digit
    let currentPos = lexer.getPos()
    if not lexer.eof(1):  # There is a next character
      let nextChar = lexer.peek(1)
      lexer.setPos(currentPos)  # Reset position after peeking
      if nextChar in Digits:
        lexer.error "Identifiers cannot start with '.' followed by a digit"
    else:
      lexer.setPos(currentPos)  # Reset position

  # Reject +digit and -digit patterns (return to let number tokenizers handle them)
  if firstChar in {'+', '-'}:
    let currentPos = lexer.getPos()
    if not lexer.eof(1):  # There is a next character
      let nextChar = lexer.peek(1)
      lexer.setPos(currentPos)  # Reset position after peeking
      if nextChar in Digits:
        # This looks like a number, let number tokenizers handle it
        return
    else:
      lexer.setPos(currentPos)  # Reset position

  # Check if the identifier is similar to a number, and if it is it should follow the EOF, a whitespace, a new line or any non-ident char in order to be discarded.
  if (
    lexer.tokenNumHex(addToStack = false) or lexer.tokenNumBin(addToStack = false) or
    lexer.tokenNumOct(addToStack = false) or lexer.tokenNumFloat(addToStack = false) or
    lexer.tokenNumInt(addToStack = false)
  ):
    if (
      lexer.eof() or lexer.tokenWhitespace(addToStack = false) or
      lexer.tokenNewLine(addToStack = false) or lexer.peek() in nonIdenChars
    ):
      lexer.setPos before
      return

  block outer:
    while not lexer.eof():
      lexer.disallowedRunes()
      let rune = lexer.peekRune()

      # Check if rune is whitespace or newline (including Unicode whitespace like U+3000)
      if rune.int in whitespaces:
        break
      # Check for newline characters
      for nl in newLines:
        if lexer.peek(nl):
          break outer

      if rune.int <= 0x20 or rune.int > 0x10FFFF:
        break

      for c in nonIdenChars:
        if rune == Rune(c):
          break outer

      lexer.inc rune.size

proc tokenSingleLineComment*() {.lexing: tkEmpty.} =
  if not lexer.peek("//"):
    return

  lexer.inc 2

  while not lexer.eof(): # Consume until a new line or EOF
    if lexer.tokenNewLine(addToStack = addToStack):
      break

    inc lexer

proc tokenLineCont*() {.lexing: tkLineCont.} =
  if lexer.peek() != '\\':
    return

  inc lexer

  lexer.skipwhitespaces()
  # After escline, we expect: a newline, a comment, or EOF
  if not lexer.tokenSingleLineComment(addToStack = false) and
      not lexer.tokenNewLine(addToStack = false) and
      not lexer.eof():
    lexer.error "Expected a new line"

proc tokenLitMatches() {.lexing: tkEmpty.} =
  ## Tries to match any of the litMatches literals.
  for (lit, kind) in litMatches:
    if lexer.literal(lit):
      lexer.add(kind, before)
      break

proc validToken*(
    source: sink string,
    token: proc(lexer: var Lexer, consume = true, addToStack = true): bool,
): bool =
  var lexer = Lexer(isStream: true, stream: newStringStream(source))

  try:
    result = lexer.token() and lexer.eof()
  except KdlLexerError:
    return

proc scanKdl*(lexer: var Lexer) =
  const choices = [
    disallowedRunes, tokenWhitespace, tokenNewLine, tokenLineCont,
    tokenSingleLineComment, tokenEqual, tokenRawString, tokenString, tokenLitMatches,
    tokenIdent, tokenNumHex, tokenNumBin, tokenNumOct, tokenNumFloat, tokenNumInt,
  ]

  while not lexer.eof():
    var anyMatch = false

    for choice in choices:
      if lexer.choice():
        anyMatch = true
        break

    if not anyMatch:
      lexer.error &"Could not match any pattern for {quoted($lexer.peekRune)}"

proc scanKdl*(source: string, start = 0): Lexer =
  result = Lexer(isStream: false, source: source, current: start)
  result.scanKdl()

proc scanKdlFile*(path: string): Lexer =
  scanKdl(readFile(path))

proc scanKdl*(stream: sink Stream): Lexer =
  result = Lexer(isStream: true, stream: stream)
  defer:
    result.stream.close()
  result.scanKdl()

proc scanKdlStream*(source: sink string): Lexer =
  scanKdl(newStringStream(source))

proc scanKdlFileStream*(path: string): Lexer =
  scanKdl(openFileStream(path))
