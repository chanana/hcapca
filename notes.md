# Notes on running docker image

1. Command used to run docker image
```bash
docker run \
--rm \
-v $(pwd):/usr/local/src/hcapca \
hcapca_with_libs \
/usr/local/src/hcapca/src/HC-PCA.R config_file.yaml
```

2. Evan Rees's command to run his docker image
```bash
docker run --rm -it biocontainers/clustal-omega:v1.2.1_cv5
```
3. ER's more nuanced command
```bash
echo "Pre-alignment"
ls example/data/processed
docker run \
    --rm \
    -v $(pwd)/example/data:/output \
    biocontainers/clustal-omega:v1.2.1_cv5 \
        clustalo -i /output/processed/orgs_yenB.faa \
        --force \
        --auto -o /output/processed/orgs_yenB.aln
echo "Post-alignment"
ls example/data/processed
```
4. Docker `build` command to make image
```bash
docker build --force-rm --rm -t schanana/hcapca:1.0 .
```

5. Docker run command
```bash
docker run -i --rm -v $(pwd):/hcapca schanana/hcapca:1.2 src/hcapca.R
```
