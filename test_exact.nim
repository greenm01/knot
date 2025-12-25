import src/kdl/parser
import options

let input = """node /-{
    child
} foo {
    bar
}
"""

try:
  let doc = parseKdl(input)
  echo "Parsed successfully - WRONG, should fail"
  echo "Nodes: ", doc.len
  if doc.len > 0:
    echo "Node name: ", doc[0].name
    echo "Args: ", doc[0].args.len
    if doc[0].children.len > 0:
      echo "Children: ", doc[0].children.len
except:
  echo "Correctly failed: ", getCurrentExceptionMsg()
