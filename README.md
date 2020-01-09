# Instructions

```
./build.sh                    # builds inside Docker
stack install wai-app-static  # for warp web server
warp -v -d asterius-bin
```

Then visit: http://localhost:3000/awgl.html

# Re-compilation in interactive Docker image

```
./docker-interactive.sh     # start interactive Docker session
./inside-docker-prepare.sh  # prepare the container
./inside-docker-build.sh    # build
```
