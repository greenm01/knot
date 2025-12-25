import src/kdl/parser

try:
  let doc = parseKdl("node true")
  echo "Parsed successfully"
  echo "Nodes: ", doc.len
  if doc.len > 0:
    echo "Args: ", doc[0].args.len
    if doc[0].args.len > 0:
      echo "Arg value: ", doc[0].args[0]
except:
  echo "Failed: ", getCurrentExceptionMsg()
