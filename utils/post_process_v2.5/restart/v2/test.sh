#!/bin/bash

exec_path="/lfs/h2/emc/ptmp/santha.akella/rtofs-tools_28Sep2026/archv2mom6res/build/"
exec_name="${exec_path}rtofs2mom6"

inputs_dir="/lfs/h2/emc/ptmp/santha.akella/TMP2/"
#<archive.a> <archive.b> <regional.depth.a> <Temp.nc> <Salt.nc> <h.nc> <u.nc> <v.nc> <dayout>

${exec_name} ${inputs_dir}/rtofs_glo.t00z.n00.archv.a ${inputs_dir}/rtofs_glo.t00z.n00.archv.b ${inputs_dir}/rtofs_glo.navy_0.08.regional.depth.a ${inputs_dir}/MOM.res.nc ${inputs_dir}/MOM.res_1.nc ${inputs_dir}/MOM.res_2.nc ${inputs_dir}/MOM.res_3.nc ${inputs_dir}/MOM.res_4.nc 45926.0
