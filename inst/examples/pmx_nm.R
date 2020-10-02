## using only runnumber 
# ctr <- pmx_nm(
#  directory=model_dir,
#  runno = "001" 
#)

## using a model file (e.g. run001.lst)
#ctr <- pmx_nm(
#  directory=model_dir,
#  file = "run001.lst" 
#)

## if simulation was performed post-hoc, an additional simulation file can be loaded for VPC
#ctr <- pmx_nm(
#  directory=model_dir,
#  file = "run001.lst", 
#  simfile = "simulation.ctl"
#)

## loading with individual table(s)-names
#ctr <- pmx_nm(directory = model_dir,
#              runno = 3, 
#              table_names = "xptab")
