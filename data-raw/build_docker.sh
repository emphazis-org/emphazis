# to build
docker build . -f inst/Dockerfile -t luciorq/emphazis-app:latest

# to run
docker run -d \
  --restart always \
  -p 8989:3838 \
  --name emphazis-app \
  luciorq/emphazis-app:latest
