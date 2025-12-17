
import subprocess

subprocess.run(
    ["C:/Program Files/R/R-4.5.1/bin/Rscript.exe", "DT_dynamics_estimator.R"],
    cwd="Path/To/BlueBird-multizone-modelling/code"     # ← R script path
)

subprocess.run(
    ["C:/Program Files/R/R-4.5.1/bin/Rscript.exe", "DT_predict.R"],
    cwd="Path/To/BlueBird-multizone-modelling/code"     # ← R script path
)
