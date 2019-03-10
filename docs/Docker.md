# Docker recipe for WGInrR

This recipe [Dockerfile](../docker/Dockerfile) will produce
a Docker image for WGInR based on Linux debian.

Final image is about 1.4Gb. Be patient during the build.

```
cd docker
docker build -t wginr .
docker run -it wginr
```
