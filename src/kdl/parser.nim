import std/[parseutils, strformat, strutils, unicode, options, streams, tables, macros]
import bigints
import lexer, nodes, types, utils
# TODO: add parseKdlVal
type
  None = object

  Parser* = object
    case isStream*: bool
    of true:
      stream*: Stream
    else:
      source*: string

    # Indexes and length of new lines in multiline strings that have to be converted to a single \n
    multilineStringsNewLines*: seq[tuple[idx, length: int]]
    stack*: seq[Token]
    current*: int

  Match[T] = tuple[ok, ignore: bool, val: T]

const
  integers = {tkNumInt, tkNumHex, tkNumBin, tkNumOct}
  numbers = integers + {tkNumFloat}
  strings = {tkString, tkRawString}

macro parsing(x: typedesc, body: untyped): untyped =
  ## Converts a procedure definition like:
  ## ```nim
  ## proc foo() {.parsing[T].} =
  ##   echo "hi"
  ## ```
  ## Into
  ## ```nim
  ## proc foo(parser: var Parser, required: bool = true): Match[T] {.discardable.} =
  ##   let before = getPos(parser)
  ##   echo "hi"
  ## ```

  body.expectKind(nnkProcDef)

  result = body.copyNimTree()

  result.params[0] = nnkBracketExpr.newTree(ident"Match", x) # Return type
  result.params.insert(
    1, newIdentDefs(ident"parser", newNimNode(nnkVarTy).add(ident"Parser"))
  )
  result.params.add(newIdentDefs(ident"required", ident"bool", newLit(true)))

  result.addPragma(ident"discardable")

  if result[^1].kind == nnkStmtList:
    result[^1].insert(
      0,
      quote do:
        let before {.inject.} = parser.current,
    )

proc eof(parser: Parser, extra = 0): bool =
  parser.current + extra >= parser.stack.len

proc peek(parser: Parser, next = 0): Token =
  if not parser.eof(next):
    result = parser.stack[parser.current + next]
  else:
    let token = parser.stack[parser.current - 1]
    result = Token(start: token.start + token.lexeme.len)

proc error(parser: Parser, msg: string) =
  let coord =
    if parser.isStream:
      parser.stream.getCoord(parser.peek().start)
    else:
      parser.source.getCoord(parser.peek().start)

  let errorMsg =
    if parser.isStream:
      parser.stream.errorAt(coord)
    else:
      parser.source.errorAt(coord)

  raise newException(
    KdlParserError, &"{msg} at {coord.line + 1}:{coord.col + 1}\n{errorMsg.indent(2)}\n"
  )

proc consume(parser: var Parser, amount = 1) =
  parser.current += amount

template invalid[T](x: Match[T]) =
  ## Returns if x is ok
  let val = x

  result.ok = val.ok

  if val.ok:
    return

template valid[T](x: Match[T]): T =
  ## Returns if x is not ok and gives x.val back
  let val = x

  result.ok = val.ok

  if not val.ok:
    result.ignore = false
    parser.current = before
    return

  val.val

template hasValue[T](match: Match[T]): bool =
  let (ok, ignore, val {.inject.}) = match
  ok and not ignore

template setValue[T](x: untyped, match: Match[T]) =
  if hasValue match:
    x = val

proc match(x: TokenKind | set[TokenKind]) {.parsing: Token.} =
  let token = parser.peek()
  let matches =
    when x is TokenKind:
      token.kind == x
    else:
      token.kind in x

  if matches:
    result.ok = true
    result.val = token
    parser.consume()
  elif required:
    when x is TokenKind:
      parser.error &"Expected {x} but found {token.kind}"
    else:
      parser.error &"Expected one of {x} but found {token.kind}"

proc skipWhile(parser: var Parser, kinds: set[TokenKind]): int {.discardable.} =
  while not parser.eof():
    if parser.peek().kind in kinds:
      parser.consume()
      inc result
    else:
      break

proc more(kind: TokenKind) {.parsing: None.} =
  ## Matches one or more tokens of `kind`
  discard valid parser.match(kind, required)
  discard parser.skipWhile({kind})

proc parseNumber(token: Token, tag: Option[string]): KdlVal =
  assert token.kind in numbers

  if token.kind in integers:
    # Handle hex numbers specially - they default to unsigned
    if token.kind == tkNumHex:
      let hexDigits = token.lexeme[2..^1]  # Skip "0x" prefix

      # Check if value is too large for uint64 (more than 16 hex digits)
      if hexDigits.len > 16:
        # Use BigInt for large hex values
        let bigVal = initBigInt(hexDigits, 16)
        result = KdlVal(kind: KBigInt, bigint: bigVal, tag: tag)
      else:
        var hexVal: BiggestUInt
        discard parseHex(token.lexeme, hexVal, 2)  # Start after "0x"

        case tag.get(string.default)
        of "i8":
          result = initKVal(cast[int64](hexVal).int8, tag)
        of "i16":
          result = initKVal(cast[int64](hexVal).int16, tag)
        of "i32":
          result = initKVal(cast[int64](hexVal).int32, tag)
        of "i64":
          result = initKVal(cast[int64](hexVal), tag)
        of "isize":
          result = initKVal(cast[int64](hexVal).int, tag)
        of "u8":
          result = initKVal(hexVal.uint8, tag)
        of "u16":
          result = initKVal(hexVal.uint16, tag)
        of "u32":
          result = initKVal(hexVal.uint32, tag)
        of "u64":
          result = initKVal(hexVal.uint64, tag)
        of "usize":
          result = initKVal(hexVal.uint, tag)
        else:
          # Default: store hex as unsigned
          result = initKVal(hexVal.uint64, tag)
    else:
      # Non-hex integers: decimal, binary, octal
      # Try to parse as regular int, fall back to BigInt if too large
      try:
        let numVal = case token.kind
                     of tkNumInt:
                       token.lexeme.parseBiggestInt()
                     of tkNumBin:
                       token.lexeme.parseBinInt()
                     of tkNumOct:
                       token.lexeme.parseOctInt()
                     else:
                       0

        case tag.get(string.default)
        of "i8":
          result = initKVal(numVal.int8, tag)
        of "i16":
          result = initKVal(numVal.int16, tag)
        of "i32":
          result = initKVal(numVal.int32, tag)
        of "i64":
          result = initKVal(numVal.int64, tag)
        of "isize":
          result = initKVal(numVal.int, tag)
        of "u8":
          result = initKVal(numVal.uint8, tag)
        of "u16":
          result = initKVal(numVal.uint16, tag)
        of "u32":
          result = initKVal(numVal.uint32, tag)
        of "u64":
          result = initKVal(numVal.uint64, tag)
        of "usize":
          result = initKVal(numVal.uint, tag)
        else:
          # For unsupported types like i128, u128, store as int64 with tag preserved
          # Applications can handle these specially based on the tag
          result = initKVal(numVal, tag)
      except ValueError:
        # Number too large for int64, use BigInt
        let base = case token.kind
                   of tkNumInt: 10
                   of tkNumBin: 2
                   of tkNumOct: 8
                   else: 10
        let digits = if token.kind == tkNumBin: token.lexeme[2..^1]
                     elif token.kind == tkNumOct: token.lexeme[2..^1]
                     else: token.lexeme
        let bigVal = initBigInt(digits, base)
        result = KdlVal(kind: KBigInt, bigint: bigVal, tag: tag)
  else:
    # Handle special float keywords: #inf, #-inf, #nan
    let fnumVal = case token.lexeme
      of "#inf": Inf
      of "#-inf": -Inf
      of "#nan": NaN
      else: token.lexeme.parseFloat()

    case tag.get(string.default)
    of "f32":
      result = initKVal(fnumVal.float32, tag)
    of "f64":
      result = initKVal(fnumVal.float64, tag)
    else:
      # For unsupported types like decimal64, decimal128, store as float64 with tag preserved
      # Applications can handle these specially based on the tag
      result = initKVal(fnumVal, tag)

proc continuesWithNewLine(s: string, at: var int, consume = true): bool =
  ## Checks if there's a new line in s at at and increments at by the lenght
  ## of the new line if consume is true
  for nl in newLines:
    if s.continuesWith(nl, at):
      if consume:
        at.inc nl.len
      return true

proc continuesWithWhitespace(s: string, at: var int, consume = true): bool =
  ## Checks if there's a whitespace in s at at and increments at by the lenght
  ## of the whitespace if consume is true
  for w in whitespaces:
    if s.continuesWith($Rune(w), at):
      if consume:
        at.inc w.Rune.size
      return true

proc resolveWhitespaceEscapes(str: string): string =
  ## Phase 1: Resolve ONLY whitespace escapes (\ followed by whitespace/newline)
  ## According to KDL 2.0 spec, this MUST be done BEFORE dedentation
  var i = 0
  while i < str.len:
    if str[i] == '\\' and i + 1 < str.len:
      let nextChar = str[i + 1]
      # Special case: \\ is an escaped backslash, keep both characters
      if nextChar == '\\':
        result.add str[i]
        result.add str[i + 1]
        i += 2
      # Check if this is escaped whitespace/newline
      elif nextChar in {' ', '\t', '\r', '\n'}:
        # Consume the backslash
        inc i
        # Consume all following whitespace/newlines
        while i < str.len and str[i] in {' ', '\t', '\r', '\n'}:
          inc i
        # Don't add anything - the escaped whitespace is consumed
      else:
        # Not whitespace escape, keep the backslash for later processing
        result.add str[i]
        inc i
    else:
      result.add str[i]
      inc i

proc escapeString(str: string, x = 0 .. str.high): string =
  ## Phase 2: Process non-whitespace escape sequences
  ## (whitespace escapes should already be resolved via resolveWhitespaceEscapes)
  var i = x.a
  while i <= x.b:
    if str[i] == '\\':
      inc i # Consume backslash

      if i > x.b: # Dangling backslash
        result.add '\\'
        break

      case str[i]
      of '\\': result.add '\\'
      of 'n': result.add '\n'
      of 'r': result.add '\r'
      of 't': result.add '\t'
      of 'b': result.add '\b'
      of 'f': result.add '\f'
      of 's': result.add ' '  # KDL 2.0: \s is space
      of '"': result.add '"'
      of 'u':
        inc i
        if i > x.b or str[i] != '{':
          # Invalid unicode escape, treat as literal 'u'
          result.add "u"
          dec i # Backtrack to not consume the character after 'u'
        else:
          inc i
          var hex: string
          let start = i
          while i <= x.b and str[i] in HexDigits:
            inc i
          hex = str[start ..< i]

          if i <= x.b and str[i] == '}':
            # Validate hex digit count (1-6 digits per KDL spec)
            if hex.len < 1 or hex.len > 6:
              raise newException(KdlParserError, "Unicode escape \\u{" & hex & "} must have 1-6 hexadecimal digits, found " & $hex.len)

            let codepoint = parseHexInt(hex)
            # Validate Unicode codepoint
            if codepoint >= 0xD800 and codepoint <= 0xDFFF:
              raise newException(KdlParserError, "Unicode escape \\u{" & hex & "} is a surrogate codepoint (U+D800 to U+DFFF), which is invalid")
            elif codepoint > 0x10FFFF:
              raise newException(KdlParserError, "Unicode escape \\u{" & hex & "} exceeds maximum Unicode codepoint (U+10FFFF)")
            result.add Rune(codepoint)
          else:
            # Invalid unicode escape, treat as literal
            result.add "\\u{" & hex
            if i <= x.b:
              result.add str[i]
      else:
        # Check for escaped whitespace (\ followed by whitespace/newline)
        # This handles regular strings; multiline strings pre-process with resolveWhitespaceEscapes
        if str[i] in {' ', '\t', '\r', '\n'}:
          # Consume all following whitespace and newlines
          while i <= x.b and str[i] in {' ', '\t', '\r', '\n'}:
            inc i
          dec i  # Back up one since we'll inc at the end of the loop
        else:
          # Any other escaped character is treated as the character itself
          result.add str[i]
      inc i
    else:
      result.add str[i]
      inc i

proc parseString(token: Token, multilineStringsNewLines: seq[(int, int)]): KdlVal =
  assert token.kind in strings

  result = initKString()

  var varToken = token
  varToken.lexeme = newStringOfCap(token.lexeme.len)

  var i = 0
  while i < token.lexeme.len:
    let before = i
    for (idx, length) in multilineStringsNewLines:
      if i + token.start == idx:
        varToken.lexeme.add '\n'
        i += length

    if i == before:
      varToken.lexeme.add token.lexeme[i]
      inc i

  if token.kind == tkString:
    # Check if it's a multiline string (starts with """)
    if varToken.lexeme.len >= 6 and varToken.lexeme[0..2] == "\"\"\"":
      # Multiline string: skip first 3 and last 3 quotes
      var content = varToken.lexeme[3 ..< varToken.lexeme.len - 3]

      # Strip first newline if present
      if content.len > 0 and content[0] == '\n':
        content = content[1..^1]

      # Validate: line continuation cannot escape past content boundary
      # The '\' escapes whitespace/newlines. Invalid cases:
      # 1. "bar\" (nothing after \) - would escape the delimiter itself
      # 2. "bar\\n  " (\ escapes newline + indent of closing line) - ambiguous dedent
      # Valid: "bar\   " (\ escapes trailing spaces within content) - OK
      var i = content.len - 1

      # Case 1: Content ends with '\' as last character (truly nothing after)
      if i >= 0 and content[i] == '\\':
        raise newException(KdlParserError, "Line continuation (\\) cannot be the last character before closing delimiter")

      # Case 2: '\' followed by newline then whitespace (escapes closing line indent)
      # Skip trailing spaces/tabs to find newline
      while i >= 0 and content[i] in {' ', '\t'}:
        dec i
      # If we have trailing whitespace and it's preceded by newline and that line ends with '\'
      if i >= 0 and content[i] == '\n':
        let hadTrailingWhitespace = content[content.len - 1] in {' ', '\t'}
        if hadTrailingWhitespace:
          # Check if line before this newline ends with '\'
          dec i
          if i >= 0 and content[i] == '\r':  # Handle \r\n
            dec i
          if i >= 0 and content[i] == '\\':
            # The '\' would escape the newline and closing line's whitespace
            raise newException(KdlParserError, "Line continuation (\\) cannot escape the closing line's indentation")

      # STEP 1: Resolve whitespace escapes BEFORE dedentation (KDL 2.0 spec requirement)
      content = resolveWhitespaceEscapes(content)

      # STEP 2: Find indentation of closing line
      var lastNewlinePos = -1
      for i in countdown(content.len - 1, 0):
        if content[i] == '\n':
          lastNewlinePos = i
          break

      # Calculate base indentation from closing line
      var baseIndent = ""
      if lastNewlinePos >= 0:
        # Extract whitespace after last newline (the closing line's indent)
        for i in (lastNewlinePos + 1) ..< content.len:
          if content[i] in {' ', '\t'}:
            baseIndent.add content[i]
          else:
            break

        # Remove the closing line (it's just indentation)
        content = content[0..lastNewlinePos]
        # Remove trailing newline
        if content.len > 0 and content[^1] == '\n':
          content = content[0..^2]
      else:
        # No newline means the entire content is the closing line indentation
        baseIndent = content
        content = ""

      # STEP 3: Apply dedentation
      if baseIndent.len > 0 and content.len > 0:
        var dedented = ""
        var i = 0
        while i < content.len:
          if content[i] == '\n' or i == 0:
            # Start of a line
            if i > 0:
              dedented.add '\n'
              inc i

            # Try to match and remove base indentation
            var matched = 0
            while matched < baseIndent.len and i + matched < content.len and
                  content[i + matched] == baseIndent[matched]:
              inc matched

            # Validate: if we couldn't match all base indentation, check if line is not empty
            if matched < baseIndent.len and i + matched < content.len and content[i + matched] != '\n':
              # Line has content but doesn't have the full base indentation - error
              raise newException(KdlParserError,
                "Multiline string indentation error: content line does not start with the closing line's indentation")

            # Skip the matched indentation
            i += matched
          else:
            dedented.add content[i]
            inc i

        content = dedented

      # STEP 4: Resolve other escape sequences (after dedentation)
      result.str = escapeString(content)
    else:
      # Regular string: skip first and last quote
      result.str = escapeString(varToken.lexeme, 1 ..< varToken.lexeme.high)
  else: # Raw string
    var hashes: string
    discard
      varToken.lexeme.parseUntil(hashes, '"', start = 0) # Count the number of hashes

    # Check if it's a multiline raw string (has """ after hashes)
    let quoteStart = hashes.len
    if varToken.lexeme.len >= quoteStart + 6 and
       varToken.lexeme[quoteStart..quoteStart+2] == "\"\"\"":
      # Multiline raw string: exclude hashes + """ and """ + hashes, apply dedentation
      var content = varToken.lexeme[quoteStart + 3 .. varToken.lexeme.high - hashes.len - 3]

      # Strip first newline if present
      if content.len > 0 and content[0] == '\n':
        content = content[1..^1]

      # Find indentation of closing line
      var lastNewlinePos = -1
      for i in countdown(content.len - 1, 0):
        if content[i] == '\n':
          lastNewlinePos = i
          break

      # Calculate base indentation from closing line
      var baseIndent = ""
      if lastNewlinePos >= 0:
        for i in (lastNewlinePos + 1) ..< content.len:
          if content[i] in {' ', '\t'}:
            baseIndent.add content[i]
          else:
            break

        # Remove the closing line
        content = content[0..lastNewlinePos]
        if content.len > 0 and content[^1] == '\n':
          content = content[0..^2]
      else:
        # No newline means the entire content is the closing line indentation
        baseIndent = content
        content = ""

      # Apply dedentation (raw strings don't process escapes, so no escapeString call)
      if baseIndent.len > 0:
        var dedented = ""
        var i = 0
        while i < content.len:
          if content[i] == '\n' or i == 0:
            if i > 0:
              dedented.add '\n'
              inc i

            # Try to match and remove base indentation
            var matched = 0
            while matched < baseIndent.len and i + matched < content.len and
                  content[i + matched] == baseIndent[matched]:
              inc matched

            # Validate: if we couldn't match all base indentation, check if line is not empty
            if matched < baseIndent.len and i + matched < content.len and content[i + matched] != '\n':
              # Line has content but doesn't have the full base indentation - error
              raise newException(KdlParserError,
                "Multiline string indentation error: content line does not start with the closing line's indentation")

            i += matched
          else:
            dedented.add content[i]
            inc i

        result.str = dedented
      else:
        result.str = content
    else:
      # Regular raw string
      result.str =
        varToken.lexeme[1 + hashes.len .. varToken.lexeme.high - hashes.len - 1]

proc parseValue(token: Token, multilineStringsNewLines: seq[(int, int)], tag: Option[string]): KdlVal =
  case token.kind
  of numbers:
    token.parseNumber(tag)
  of strings:
    let strVal = token.parseString(multilineStringsNewLines).getString()
    case tag.get(string.default)
    of "date":
      initKVal(strVal, KDate, tag)
    of "time":
      initKVal(strVal, KTime, tag)
    of "date-time", "datetime": # KDL 2.0 uses date-time, but allow datetime for flexibility
      initKVal(strVal, KDateTime, tag)
    of "duration":
      initKVal(strVal, KDuration, tag)
    else:
      initKVal(strVal, tag)
  of tkIdent:
    # Bare identifiers are treated as string values in KDL 2.0
    initKVal(token.lexeme, tag)
  of tkBool:
    # KDL 2.0: booleans are #true and #false
    initKVal(token.lexeme == "#true", tag)
  of tkNull:
    # KDL 2.0: null is #null
    initKVal(nil, tag)
  else:
    initKVal(nil, tag)
    

    
proc parseIdent(
    token: Token, multilineStringsNewLines: seq[(int, int)]
): Option[string] =
  case token.kind
  of strings:
    token.parseString(multilineStringsNewLines).getString().some
  of tkIdent:
    token.lexeme.some
  else:
    string.none

proc skipLineSpaces(parser: var Parser) =
  parser.skipWhile({tkNewLine, tkWhitespace, tkLineCont})

proc matchLineCont() {.parsing: None.} =
  parser.skipWhile({tkWhitespace})
  discard valid parser.match(tkLineCont, required)
  discard parser.skipWhile({tkWhitespace})

proc matchNodeSpace() {.parsing: None.} =
  invalid parser.matchLineCont(required = false)
  discard valid parser.more(tkWhitespace, required)

proc matchSlashDash() {.parsing: None.} =
  discard valid parser.match(tkSlashDash, required)
  while parser.matchNodeSpace(required = false).ok:
    discard

proc matchIdent() {.parsing: Option[string].} =
  result.val = valid(parser.match({tkIdent} + strings, required)).parseIdent(
      parser.multilineStringsNewLines
    )

proc matchTag() {.parsing: Option[string].} =
  discard valid parser.match(tkOpenPar, required)
  parser.skipWhile({tkWhitespace})  # Skip whitespace/comments after (
  result.val = valid parser.matchIdent(required = true)
  parser.skipWhile({tkWhitespace})  # Skip whitespace/comments before )
  discard parser.match(tkClosePar, true)

proc matchValue(slashdash = false) {.parsing: KdlVal.} =
  if slashdash:
    result.ignore = parser.matchSlashDash(required = false).ok

  let (_, _, tag) = parser.matchTag(required = false)
  parser.skipWhile({tkWhitespace})  # Skip whitespace/comments after type annotation

  # In KDL 2.0, bare identifiers (tkIdent) are valid as string values
  result.val = valid(parser.match({tkBool, tkNull, tkIdent} + strings + numbers, required))
    .parseValue(parser.multilineStringsNewLines, tag)
  result.val.tag = tag

proc matchProp(slashdash = true) {.parsing: KdlProp.} =
  if slashdash:
    result.ignore = parser.matchSlashDash(required = false).ok

  let ident = valid parser.matchIdent(required = false)

  # KDL 2.0 allows whitespace before and after '=' in properties
  parser.skipWhile({tkWhitespace})
  discard valid parser.match(tkEqual, required)
  parser.skipWhile({tkWhitespace})

  let value = valid parser.matchValue(required = true)

  result.val = (ident.get, value)

proc matchNodeEnd() {.parsing: None.} =
  result.ok = parser.eof()

  if not result.ok:
    let token = parser.peek()
    discard valid parser.match({tkNewLine, tkSemicolon, tkCloseBra}, required)

    if token.kind == tkCloseBra: # Unconsume
      dec parser.current

proc matchNode(slashdash = true) {.parsing: KdlNode.}

proc matchNodes() {.parsing: KdlDoc.} =
  parser.skipLineSpaces()

  while not parser.eof():
    if hasValue parser.matchNode(required = required):
      result.ok = true
      result.val.add val
    elif not required:
      break

    parser.skipLineSpaces()

proc matchChildren(slashdash = true) {.parsing: KdlDoc.} =
  if slashdash:
    result.ignore = parser.matchSlashDash(required = false).ok
    # After slashdash, skip line spaces before the children block
    if result.ignore:
      parser.skipLineSpaces()

  discard valid parser.match(tkOpenBra, required)
  result.val = parser.matchNodes(required = false).val
  discard valid parser.match(tkCloseBra, true)

proc matchNode(slashdash = true) {.parsing: KdlNode.} =
  if slashdash:
    result.ignore = parser.matchSlashDash(required = false).ok
    # After slashdash at document level, skip line spaces before the commented node
    if result.ignore:
      parser.skipLineSpaces()

  let tag = parser.matchTag(required = false).val
  parser.skipWhile({tkWhitespace})  # Skip whitespace/comments after node type annotation
  let ident = valid parser.matchIdent(required)

  result.val = initKNode(ident.get, tag = tag)

  invalid parser.matchNodeEnd(required = false)

  # Check if children block immediately follows (e.g., node{})
  if parser.peek().kind == tkOpenBra:
    setValue result.val.children, parser.matchChildren(required = false)
    invalid parser.matchNodeEnd(required = true)
    return

  # Node has arguments/properties - parse them
  # Check for spacing at the START of each iteration to prevent infinite loops
  while true: # Match arguments and properties
    # Check for spacing BEFORE attempting to parse entry
    let hasSpace = parser.matchNodeSpace(required = false).ok

    if not hasSpace:
      # No space found - check for special cases
      let nextToken = parser.peek().kind
      if nextToken == tkOpenBra:
        # Children block immediately follows
        dec parser.current # Unconsume to match it later
        break
      elif nextToken != tkSlashDash:
        # No space and not slashdash - node must end here
        break
      # else: nextToken == tkSlashDash, zero-space slashdash is valid, continue parsing

    # Check for slashdash before entry
    # But first peek ahead to see if it's for the children block
    let isSlashdashed =
      if parser.peek().kind == tkSlashDash:
        # Look ahead past slashdash and any whitespace
        let savedPos = parser.current
        discard parser.matchSlashDash(required = false)
        parser.skipLineSpaces()
        let nextIsChildren = parser.peek().kind == tkOpenBra
        parser.current = savedPos  # Reset position

        if nextIsChildren:
          # Slashdash is for children block, not for an entry
          false
        else:
          # Slashdash is for this entry, consume it
          discard parser.matchSlashDash(required = false)
          parser.skipLineSpaces()
          true
      else:
        false

    # Try to parse property (key=value) or argument (value)
    let propMatch = parser.matchProp(required = false, slashdash = false)

    if hasValue propMatch:
      if not isSlashdashed:
        result.val.props[val.key] = val.val
    else:
      # Not a property, try to parse as argument value
      let valMatch = parser.matchValue(required = false, slashdash = false)
      if hasValue valMatch:
        if not isSlashdashed:
          result.val.args.add val
      else:
        # Neither property nor value matched
        if isSlashdashed:
          # Slashdash without a following entry is an error
          parser.error("Slashdash (/-) must be followed by a node entry")
        # Stop parsing entries
        break

  # Match all children blocks (slashdashed or not)
  # KDL 2.0 allows multiple children blocks like: node {} /-{} {} /-{}
  while true:
    # Skip whitespace and line continuations between children blocks
    parser.skipWhile({tkWhitespace})
    while parser.peek().kind == tkLineCont:
      discard parser.match(tkLineCont, false)
      parser.skipLineSpaces()
      parser.skipWhile({tkWhitespace})

    # Check for children block (slashdashed or not)
    let nextKind = parser.peek().kind
    if nextKind == tkSlashDash or nextKind == tkOpenBra:
      # For slashdashed children BETWEEN blocks (not first), validate immediately followed by {
      # At this point, we've already consumed whitespace on line 768
      # If we see slashdash now and we already have children, this is between blocks
      if nextKind == tkSlashDash and result.val.children.len > 0:
        let savedPos = parser.current
        discard parser.match(tkSlashDash, false)
        # At this context (between blocks), only newline/escline are allowed before {
        # Whitespace tokens mean same-line space which is invalid
        let afterSlashdash = parser.peek().kind
        if afterSlashdash == tkWhitespace:
          parser.current = savedPos
          parser.error("Slashdash between children blocks must not have space before '{': use '/-{' or '/-\\n{'")
        parser.current = savedPos

      let childrenMatch = parser.matchChildren(required = false)
      if hasValue childrenMatch:
        # Only use the first non-slashdashed children block
        if result.val.children.len == 0:
          result.val.children = val
    else:
      break

  invalid parser.matchNodeEnd(required = true)

proc parseKdl*(lexer: sink Lexer): KdlDoc =
  if lexer.isStream:
    var parser = Parser(
      isStream: true,
      multilineStringsNewLines: lexer.multilineStringsNewLines,
      stack: lexer.stack,
      stream: lexer.stream,
    )
    defer:
      parser.stream.close()
    result = parser.matchNodes().val
  else:
    var parser = Parser(
      isStream: false,
      multilineStringsNewLines: lexer.multilineStringsNewLines,
      stack: lexer.stack,
      source: lexer.source,
    )
    result = parser.matchNodes().val

proc parseKdl*(source: string, start = 0): KdlDoc =
  var lexer = Lexer(isStream: false, source: source, current: start)
  lexer.scanKdl()
  result = lexer.parseKdl()

proc parseKdlFile*(path: string): KdlDoc =
  parseKdl(readFile(path))

proc parseKdl*(stream: sink Stream): KdlDoc =
  var lexer = Lexer(isStream: true, stream: stream)
  lexer.scanKdl()
  result = lexer.parseKdl()

proc parseKdlStream*(source: sink string): KdlDoc =
  parseKdl(newStringStream(source))

proc parseKdlFileStream*(path: string): KdlDoc =
  parseKdl(newFileStream(path))
