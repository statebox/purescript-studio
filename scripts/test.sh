# exit on error
set -e

cd vec                   && npm test && cd ..
cd stbx-core             && npm test && cd ..
cd stbx-service-rest     && npm test && cd ..
cd stbx-rest-integration && npm test && cd ..
cd stbx-lang             && npm test && cd ..
