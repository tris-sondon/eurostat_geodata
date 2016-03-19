rmarkdown::render("analysis.R", encoding = "UTF-8")

system("rsync -arv --exclude=rawdata --exclude=zip --exclude=run.R --exclude=local_data --exclude=run.R --exclude='eurostat_geodata.Rproj' --exclude=.Rhistory --exclude=.git --exclude=.gitignore --exclude=.Rproj.user ~/btsync/mk/workspace/ropengov/eurostat_geodata/ muuankarski@kapsi.fi:public_html/ropengov/eurostat_geodata/")
