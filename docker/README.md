# Docker recipe for WGInrR

This recipe [Dockerfile](../docker/Dockerfile) will produce
a Docker image for WGInR based on Linux debian.

Final image is about 1.4Gb. Be patient during the build.

## building image

```
cd docker
docker build -t wginr .
```

## running image

```
docker run -it -v ${HOME}:/home/$USER wginr
```

then play (you home dir is at /home/\<user\>)

for instance:

```
cd /home/wginr/samples && ./run_test
```

