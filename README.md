# Instructions

```
./build.sh                    # builds inside Docker
stack install wai-app-static  # for warp web server
warp -v -d asterius-bin
```

Then visit: http://localhost:3000/awgl.html

# Re-compilation in interactive Docker image

```
./docker-interactive.sh        # start interactive Docker session
bash inside-docker-prepare.sh  # prepare the container
bash inside-docker-build.sh    # build
```
