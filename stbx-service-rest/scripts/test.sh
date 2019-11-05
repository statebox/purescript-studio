echo
echo
echo "test: posting invalid nonsense tx"
curl -X POST \
  http://localhost:8080/tx \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json' \
  -d '{ "quux": "THIS SHOULD BE INVALID" }'

sleep 1

echo
echo
echo "test: POSTing invalid root tx"
echo
curl -X POST \
  http://localhost:8080/tx \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json' \
  -d '{ "root": "THIS SHOULD BE ALSO BE INVALID" }'

sleep 1

echo
echo
echo "test: POSTing root tx"
echo
curl -X POST \
  http://localhost:8080/tx \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json' \
  -d '{
	"root": {
		"message": "my root tx 1"
	},
	"previous": ""
}'

sleep 1

echo
echo
echo "test: GETting /tx"
echo
curl -X GET \
  http://localhost:8080/tx \
  -H 'Cache-Control: no-cache'
