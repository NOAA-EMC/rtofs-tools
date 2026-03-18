# Steps:
1. build using: `ncoda_graph/build.sh`
2. Make sure the `bin` directory is populated as shown below.
```
mkdir -p ../../../bin
cp /scratch4/NCEPDEV/marine/Santha.Akella/bin/* ../../../bin/
```
3. To generate plots: `cd ush-plots; ./month_stats.sh 2026031000 10 |& tee test.log`

# Details of above step 2 follow:
- gs: cp /scratch4/NCEPDEV/marine/Zulema.Garraffo/bin/gs
- pstopng2.sh: cp /scratch4/NCEPDEV/marine/Zulema.Garraffo/bin/pstopng2.sh
- gmeta2png.sh cp /scratch4/NCEPDEV/marine/Zulema.Garraffo/rtofs.prod.v2.5/ush_plot/gmeta2png.sh
- rtofs_dtg: builds from https://github.com/NOAA-EMC/NCODA/tree/develop/dtg/

# The expected location of the above:
```
[Santha.Akella@ufe01 ncoda_graph]$ pwd -P
/scratch4/NCEPDEV/marine/Santha.Akella/rtofs.prod.v2.5/rtofs-tools/ncoda_graph
[Santha.Akella@ufe01 ncoda_graph]$ 
[Santha.Akella@ufe01 ncoda_graph]$ ls ../../../bin/
README.md  gmeta2png.sh  gs  pstopng2.sh  rtofs_dtg  setup_env.sh
```
