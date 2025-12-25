import src/kdl/parser

try:
  let doc = parseKdl("node /-{\n    child\n} foo {\n    bar\n}\n")
  echo "Parsed successfully - WRONG, should fail"
  echo "Nodes: ", doc.len
except:
  echo "Correctly failed: ", getCurrentExceptionMsg()
