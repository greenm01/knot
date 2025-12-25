import src/kdl/parser

try:
  let doc = parseKdl(readFile("tests/test_cases/examples/nuget.kdl"))
  echo "Parsed successfully!"
  echo "Nodes: ", doc.len
except:
  echo "Failed: ", getCurrentExceptionMsg()
