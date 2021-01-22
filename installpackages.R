# Based on the following tutorial:

#https://www.r-bloggers.com/2017/07/quick-way-of-installing-all-your-old-r-libraries-on-a-new-device/

# All of my currently installed packages:

installed <- as.data.frame(installed.packages())
write.csv(installed, 'installed_previously.csv')

# To install on a new device:

## create a list of libraries from your old list that 
## were not already installed when you freshly download
## R from your new/different device

installedPreviously <- read.csv('installed_previously.csv')
bareR <- as.data.frame(installed.packages())
toInstall <- setdiff(installedPreviously, baseR)

## now there is a list of libraries that you don't have on
## your new device that were there on your previous one

# Now, to downlad this list of libraries:

<p>install.packages(toInstall)</p>