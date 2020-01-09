# Instructions

```
./build.sh                    # builds inside Docker
stack install wai-app-static  # for warp web server
warp -v -d asterius-bin
```

Then visit: http://localhost:3000/awgl.html

# Re-compilation in Docker

Prepare container:

```
./docker-interactive.sh   # start interactive Docker session
ahc-cabal new-update
mkdir -p asterius-bin
```

Compile:

```
cd asterius-bin
ahc-cabal new-install . -j1 --symlink-bindir . && ahc-dist --browser --input-exe awgl
```
