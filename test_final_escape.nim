import src/kdl/parser

try:
  let doc = parseKdl("node \"\"\"\n  foo\n  bar\\\n  \"\"\"\n")
  echo "Parsed successfully - WRONG, should fail"
  echo "Nodes: ", doc.len
  if doc.len > 0 and doc[0].args.len > 0:
    echo "Arg value: ", doc[0].args[0]
except:
  echo "Correctly failed: ", getCurrentExceptionMsg()
