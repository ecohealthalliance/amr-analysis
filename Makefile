all:
	Rscript -e "source('_drake.R'); drake::make(config = config)"
