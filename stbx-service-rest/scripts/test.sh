echo "test: GETting /tx\n"

curl -X GET \
  http://localhost:8080/tx \
  -H 'Cache-Control: no-cache'

echo "\n\ntest: POSTing root tx\n"

curl -X POST \
  http://localhost:8080/tx \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json' \
  -d '{	"tx": "0a04deadbeef1a2c0a150a01611000100110001001100010001a01781a0179120f0a017a10011801180222017322017418001800" }'

sleep 1

echo "\n\ntest: GETting /tx\n"
curl -X GET \
  http://localhost:8080/tx \
  -H 'Cache-Control: no-cache'

echo "\n\n--------------------------------------------------------------------------------"

sleep 1

echo "\n\ntest: posting invalid nonsense tx\n"

curl -X POST \
  http://localhost:8080/tx \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json' \
  -d '{ "quux": "THIS SHOULD BE INVALID" }'

sleep 1

echo "\n\ntest: POSTing invalid root tx\n"

curl -X POST \
  http://localhost:8080/tx \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json' \
  -d '{ "tx": "this is an ill-formed hexadecimal number" }'

sleep 1
