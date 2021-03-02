# to build
docker build inst -t luciorq/emphazis-app:latest

# to run
docker run -d \
  --restart always \
  -p 80:3838 \
  --name emphazis-app \
  luciorq/emphazis-app:latest
