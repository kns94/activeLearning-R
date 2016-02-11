Outlier Detection Using Active Learning:


Requirements

    - R Version 3.2.3

        (UBUNTU)

        Uninstall old R:

            sudo apt-get remove r-base-core

        Then:

            sudo gedit /etc/apt/sources.list

        Add the following to the file:

            deb http://cran.rstudio.com/bin/linux/ubuntu precise/

        and exit gedit.

        Then copy/paste these commands into the command line:

            sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

            sudo add-apt-repository ppa:marutter/rdev

            sudo apt-get update

            sudo apt-get upgrade

            sudo apt-get install r-base


        (MACOS)

        https://cran.r-project.org/bin/macosx/

    - R Studio 

        Download the relevant version from https://www.rstudio.com/products/rstudio/download/


Usage:

1) Open R studio and run the file installPackages.R 
2) Open ui.R and server.R in R studio
3) Click on runapp button on the top! 

Input Files:

1) The relevant Oracle could be found in /data/oracle
2) The relevant Outlier file could be found in /data/outliers

Output Files:

1) The relevant output files would be stored in /www/

Note: Incase the outlier file is not detected, you can run the file detectOutliers.R and enter relevant input parameters.