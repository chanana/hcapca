# hcapca

**hcapca** is an R script for performing Hierarchical Clustering Analysis (HCA) and Principal Component Analysis (PCA) on LC-MS data. It was tested on the following operating systems:
* macOS 10.14.4
* Ubuntu 18.04.2 LTS
* Windows 7
* Windows 10

# Instructions

## 1. Install Docker
Install [Docker Community Edition (CE)](https://docs.docker.com/install) for your operating system. For older Mac and Windows systems, you will need to install [Docker Toolbox](https://docs.docker.com/toolbox/overview) instead.

## 2. Get example data and config file
Example data and `config_file.yaml` can be found at [this link](https://uwmadison.box.com/s/ky874zpel8kby3yvwzsb1kthqbic9age). Please download and unzip to one place such that your directory structure looks like:  

```
root
  |--config_file.yaml
  |--data
      |--Analyses.dat
      |--Variables_m.dat
      |--Variables_t.dat
      |--Table.dat
```

**_Note: The <span style="color:blue">`root`</span> directory is where you unzip the `data` folder and `config_file.yaml`._**

## 3.  Housekeeping:
* You must have administrator access to install Docker or Docker Toolbox.
* You should increase the memory limits to allow the script to run. I recommend 4 GB of RAM and 2 cores. 
	* For **Windows 7**, open VirtualBox (installed as part of Docker Toolbox) as admin and stop the virtual machine **default** that is running. In settings for the default virtual machine, change the RAM and processor allocation.
    * For **Linux**, you don't need to do this since Docker has access to the entire system.
    * For **Windows 10** and **macOS**, open the preferences from the Docker app and increase resources as needed.
* For enabling shared folders:
	* In **Windows 10**, you need to enable shared folders in preferences. Right click on the Docker icon in the system tray > settings > shared drives > check appropriate drives > Apply
	* In **Windows 7**, as before, access the VirutalBox as admin > stop the default virtual machine > go to settings for the virtual machine > Shared Folders > Add as needed

## 4. Run the script
 #### 4.1 For macOS and Linux, from the `root` directory, run:
  ```bash
  docker run --interactive --tty --rm \
    --volume $(pwd):/hp \
    --workdir /hp \
    schanana/hcapca:1.6 hcapca.R
  ```
 #### 4.2 For Windows 
 On Windows 7, use the `Docker Quickstart Terminal` while on Windows 10, use the `Powershell` **(not x86 or ISE, just Powershell)** and type:
 ```bash
 docker run --interactive --tty --rm \
    --volume //c/Users/username/path/to/root/directory:/hp
    --workdir /hp \
    schanana/hcapca:1.6 hcapca.R
 ```
Regardless of OS, a folder called `output` should be created within the `root` directory with the following structure:
```
output
    |--report.html
    |--hca
    |   |--lots_of.pdf
    |
    |--pca
        |--names_with_underscores.html
        |--directories_with_the_same_names
```
  
  # Example outputs
  ##### 1. HCA
  `b0-15.pdf` HCA of node b0 with 15 samples  
  ![b0-15.pdf](./example_outputs/b0-15.jpg)
  ##### 2. PCA  
  `b0_PC1-2_S.html` PCA Scores plot of b0 for PC1 and PC2
  ![b0_PC1-2_S.html](./example_outputs/b0_PC1-2_S.html.png)
  ##### 3. Report  
  `report.html` Sample of report with links
  ![Sample Report](./example_outputs/report.png)

# Troubleshooting:
* In some flavors of **Windows**, there may be an error in mounting a shared drive. If that happens, try the following:
   * Make sure the path is specified as `//<drive_letter>/<path>`
   * Make sure the shared drives are enabled and that particular path is shared
   * Make sure the terminal is running as administrator

* Docker abruptly stops running the script
  * Make sure to allocate sufficient RAM and processing power to Docker. Usually, if the virual OS cannot get more memory, it experiences an Out Of Memory (OOM) error and kills the offending process thereby exiting the container.
  
# Wiki
[Here](https://github.com/chanana/hcapca/wiki) is a link to wiki page which is still under construction.

# License
This project is licensed under the GNU General Public License v3.0 - please see the [LICENSE.md](LICENSE.md) file for details

# Acknowledgments
* A huge thank you to Chris Thomas for helping bring this idea to fruition. He does not have a github account so [here is a link](https://www.ncbi.nlm.nih.gov/pubmed/?term=Thomas%5BAuthor%5D%20AND%20Bugni%5BAuthor%5D&cmd=DetailsSearch) to a pubmed search with his publications.
