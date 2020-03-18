echo "test: GETting /tx\n"

curl -X GET \
  http://localhost:8008/tx \
  -H 'Cache-Control: no-cache'

################################################################################

echo "\n\ntest: POSTing root tx\n"

curl -X POST \
  http://localhost:8008/tx \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json' \
  -d '{	"tx": "0a0022200a1e47756172616e746565642d456e7472616e63652d546f6b656e2e74657374" }'

sleep 1

echo "\n\ntest: POSTing wiring tx\n"

curl -X POST \
  http://localhost:8008/tx \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json' \
  -d '{	"tx": "0a20dce4021c8f117e89c479665f6d61ff650b150af375d6498b593da6afa8d2ca9f1afa010add010a0a70726976696c656467651001100010021000100210001006100010011000100310001003100010011000100210001004100010031000100510001004100010051000100110001005100010021000100510001006100010021000100610001003100010061000100510001000100310001a036275791a07666f7253616c651a05626c6f636b1a07756e626c6f636b1a047363616e1a086e6f74536f6c64321a0873686f774f7665721a076e6f74536f6c641a066e6f53686f771a04627579271a076275794261636b1a096e6f745265736f6c641a0663726561746512160a046d61696e10011801220a70726976696c656467651800" }'

sleep 1

echo "\n\ntest: POSTing 0th firing tx (the 'execution')\n"

curl -X POST \
  http://localhost:8008/tx \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json' \
  -d '{	"tx": "0a20dce4021cacc5f351d54402799977d7e4f7b86805359aec724805c80ec0b4d546120710001a03aa0003" }'

sleep 1

echo "\n\ntest: POSTing 1st firing tx\n"

curl -X POST \
  http://localhost:8008/tx \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json' \
  -d '{	"tx": "0a20dce4021c1447c0b50a5ce982dd4e78650ca3cc642004b4408eac0264da1ca5b812240a20dce4021c1447c0b50a5ce982dd4e78650ca3cc642004b4408eac0264da1ca5b81004" }'

sleep 1

echo "\n\ntest: POSTing 2nd firing tx\n"

curl -X POST \
  http://localhost:8008/tx \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json' \
  -d '{	"tx": "0a20dce4021cd649d3a9d1f69832f26739c1d81c873ca5f343ec2dd92d335adfc805122a0a20dce4021c1447c0b50a5ce982dd4e78650ca3cc642004b4408eac0264da1ca5b810011a04111aaa11" }'

sleep 1

echo "\n\ntest: POSTing 3rd firing tx\n"

curl -X POST \
  http://localhost:8008/tx \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json' \
  -d '{	"tx": "0a20dce4021c15fda2dfd9ec2ef4413b9e5a4ac5cbd8def33c0ca2c071f75a71464b122b0a20dce4021c1447c0b50a5ce982dd4e78650ca3cc642004b4408eac0264da1ca5b810051a05222bbbb222" }'

sleep 1

################################################################################

echo "\n\ntest: GETting /tx\n"
curl -X GET \
  http://localhost:8008/tx \
  -H 'Cache-Control: no-cache'

echo "\n\n--------------------------------------------------------------------------------"

sleep 1

echo "\n\ntest: posting invalid nonsense tx\n"

curl -X POST \
  http://localhost:8008/tx \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json' \
  -d '{ "quux": "THIS SHOULD BE INVALID" }'

sleep 1

echo "\n\ntest: POSTing invalid root tx\n"

curl -X POST \
  http://localhost:8008/tx \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json' \
  -d '{ "tx": "this is an ill-formed hexadecimal number" }'

sleep 1
