# DATA_608_Knowledge_and_Visual_Analytics


### Installing Dash for Udemy Course...
https://gist.github.com/luiscape/19d2d73a8c7b59411a2fb73a697f5ed4

```
#
#  Original solution via StackOverflow:
#    http://stackoverflow.com/questions/35802939/install-only-available-packages-using-conda-install-yes-file-requirements-t
#

#
#  Install via `conda` directly.
#  This will fail to install all
#  dependencies. If one fails,
#  all dependencies will fail to install.
#
conda install --yes --file requirements.txt

#
#  To go around issue above, one can
#  iterate over all lines in the
#  requirements.txt file.
#
while read requirement; do conda install --yes $requirement; done < requirements.txt

# One suggested improvement was to catch the errors:
while read requirement; do conda install --yes $requirement; done < requirements.txt 2>error.log
```
https://stackoverflow.com/questions/48493505/packagesnotfounderror-the-following-packages-are-not-available-from-current-cha

```
conda install -c conda-forge <package>
```
