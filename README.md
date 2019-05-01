# hcapca -- Hierarchical Clustering Analysis and Principal Component Analysis

**Tested on:**
* macOS 10.14.4
* Ubuntu 18.04.2 LTS
* _Windows 7_
* _Windows 10_

---

# Instructions

## 1. Install Docker

## 2. Get example data and config file
Example data and config can be found at [this link](https://uwmadison.box.com/s/ky874zpel8kby3yvwzsb1kthqbic9age). Please download and unzip to one place such that your directory structure looks like:  
```
hcapca-docker
    |--config_file.yaml
    |--data
        |--Analyses.dat
        |--Variables_m.dat
        |--Variables_t.dat
        |--Table.dat
```
Note: I will refer to `hcapca-docker` as the `root` directory.

## 3a. Unix systems (macOS and Linux):
 ### 1. Fire up a terminal and `cd` to the `root` directory. 
 ### 2. Download the docker image using command
  ```bash
  docker pull schanana/hcapca:1.6
  ```
 ### 3. Run the following command to generate output
  ```bash
  docker run \
    --interactive \
    --rm \
    --tty \
    --name hcapca \
    --volume $(pwd):/hp \
    --workdir /hp \
    schanana/hcapca:1.6 \
    hcapca.R
  ```

  ### 4. Open `output` folder to see a directory structure as follows:
  ```
  output
    |--report.html
    |--hca
    |   |--<lots_of>.pdf
    |
    |--pca
        |--<names_with_underscores>.html
        |--<folders_with_same_names>
  ```

  ### 5. Example output for b0